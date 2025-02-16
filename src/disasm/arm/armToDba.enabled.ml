(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

include Cli.Options (struct
  let name = "arm"
  let shortname = name
end)

type supported_modes = Both | Thumb | Arm

module SupportedModes = Builder.Variant_choice (struct
  type t = supported_modes

  let name = "supported-modes"
  let default = Arm

  let doc =
    "Can be used to only decode thumb instructions, arm instructions or both \
     (default: arm)."

  let to_string = function Both -> "both" | Thumb -> "thumb" | Arm -> "arm"

  let of_string = function
    | "both" -> Both
    | "thumb" -> Thumb
    | "arm" -> Arm
    | x ->
        raise
          (Invalid_argument
             (x
            ^ " is not a valid arm decoding mode. Expected one of both, thumb \
               or arm."))

  let choices = [ "both"; "thumb"; "arm" ]
end)

module Parser = Unisim_helper.Make (Logger)

let decode_arm addr bytes =
  Arm32dba.decode ~thumb:false ~addr bytes |> Parser.parse_message

let decode_from_reader_arm addr reader =
  if addr mod 4 <> 0 then
    ( Unisim_helper.empty_instruction,
      Unisim_helper.die,
      Some Unisim_helper.Undefined )
  else
    match Lreader.Peek.i32 reader with
    | exception _ ->
        ( Unisim_helper.empty_instruction,
          Unisim_helper.die,
          Some Unisim_helper.Failure )
    | bytes -> decode_arm (Int32.of_int addr) bytes

let merge_itblock addr (instrs : Instruction.Generic.t array) itblock =
  let n = Array.length itblock in
  let sizes = Array.make (n + 1) 0 in
  for i = 1 to n do
    sizes.(i) <- sizes.(i - 1) + Dhunk.length itblock.(i - 1)
  done;
  let inner j i = i + sizes.(j) in
  let outer =
    let rec iter i addr map =
      if i = n then map
      else
        iter (i + 1)
          ( addr + Size.Byte.to_int instrs.(i).size |> fun x ->
            Logger.info "%#08x" x;
            x )
          (Dba_types.Caddress.Map.add
             (Dba_types.Caddress.block_start_of_int addr)
             (Dba.Jump_target.inner sizes.(i))
             map)
    in
    let map =
      iter 1
        (addr + Size.Byte.to_int instrs.(0).size)
        Dba_types.Caddress.Map.empty
    in
    fun j -> function
      | Dba.JInner goto -> Dba.Jump_target.inner (inner j goto)
      | Dba.JOuter caddr as jo -> (
          try Dba_types.Caddress.Map.find caddr map with Not_found -> jo)
  in
  let open Dhunk in
  let j = ref 0 in
  init sizes.(n) (fun i ->
      if i >= sizes.(!j + 1) then incr j;
      Dba_types.Instruction.reloc ~outer:(outer !j) ~inner:(inner !j)
        (Utils.unsafe_get_opt (inst itblock.(!j) (i - sizes.(!j)))))

let pp_opcode ppf bv =
  for i = 0 to (Bitvector.size_of bv / 8) - 1 do
    Format.fprintf ppf "%02x"
      (Z.to_int
         Bitvector.(
           value_of (extract bv { Interval.lo = 8 * i; hi = (8 * (i + 1)) - 1 })))
  done

let itstate w i =
  if i = 0 then 0 else w land 0xe0 lor ((w lsl (i - 1)) land 0x1f)

let isitw w =
  let w = w land 0xffff in
  0xbf00 < w && w <= 0xbfff

let decode_thumb itstate addr bytes =
  Arm32dba.decode ~thumb:true ~itstate ~addr bytes |> Parser.parse_message

let decode_from_reader_thumb addr reader =
  let addr =
    if addr mod 2 = 1 then (
      Lreader.rewind reader 1;
      addr - 1)
    else addr
  in
  match Lreader.Peek.u16 reader with
  | exception _ ->
      ( Unisim_helper.empty_instruction,
        Unisim_helper.die,
        Some Unisim_helper.Failure )
  | bytes when not @@ isitw bytes ->
      let bytes = try Lreader.Peek.i32 reader with _ -> Int32.of_int bytes in
      decode_thumb 0 (Int32.of_int addr) bytes
  | word -> (
      (* it block *)
      let n =
        1
        +
        if word land 0x01 <> 0 then 4
        else if word land 0x02 <> 0 then 3
        else if word land 0x04 <> 0 then 2
        else if word land 0x08 <> 0 then 1
        else assert false
      in
      let itblock = Array.make n Unisim_helper.empty_instruction
      and ithunks = Array.make n Unisim_helper.die in
      let rec init i offset =
        if i = n then offset
        else
          match
            try Lreader.Peek.i32 reader
            with _ -> Int32.of_int (Lreader.Peek.u16 reader)
          with
          | exception _ -> raise @@ Failure ""
          | bytes -> (
              match
                decode_thumb (itstate word i)
                  (Int32.of_int (addr + offset))
                  bytes
              with
              | _, _, Some _ -> raise_notrace Not_found
              | instr, dhunk, None ->
                  itblock.(i) <- instr;
                  ithunks.(i) <- dhunk;
                  let size = Size.Byte.to_int instr.size in
                  Lreader.advance reader size;
                  init (i + 1) (offset + size))
      in
      try
        let size = init 0 0 in
        Lreader.rewind reader size;
        let bytes = Lreader.Peek.read reader size in
        let opcode = Format.asprintf "%a" pp_opcode bytes in
        let mnemonic =
          Mnemonic.supported itblock (fun ppf it ->
              Array.iteri
                (fun i g ->
                  if i <> 0 then Format.pp_print_string ppf "; ";
                  Instruction.Generic.pp_mnemonic ppf g)
                it)
        in
        ( Instruction.Generic.create size opcode mnemonic,
          merge_itblock addr itblock ithunks,
          None )
      with Not_found ->
        ( Unisim_helper.empty_instruction,
          Unisim_helper.die,
          Some Unisim_helper.Failure ))

let thumb_mode = Dba.(Expr.var ~tag:Var.Tag.Flag "t" 1)

let assert_mode mode dhunk =
  let open Dhunk in
  init
    (length dhunk + 1)
    (function
      | 0 -> Dba.Instr._assert mode 1
      | i ->
          Dba_types.Instruction.reloc
            ~inner:(fun x -> x + 1)
            (Utils.unsafe_get_opt (inst dhunk (i - 1))))

let merge_dhunk arm thumb =
  let open Dhunk in
  let larm = length arm and lthumb = length thumb in
  init
    (1 + larm + lthumb)
    (function
      | 0 -> Dba.Instr.ite thumb_mode (Dba.Jump_target.inner (1 + larm)) 1
      | i when i <= larm ->
          Dba_types.Instruction.reloc
            ~inner:(fun x -> x + 1)
            (Utils.unsafe_get_opt (inst arm (i - 1)))
      | i ->
          Dba_types.Instruction.reloc
            ~inner:(fun x -> x + larm + 1)
            (Utils.unsafe_get_opt (inst thumb (i - 1 - larm))))

let merge_ginstr arm thumb =
  let open Instruction.Generic in
  let size, opcode =
    if arm.size > thumb.size then (arm.size, arm.opcode)
    else (thumb.size, thumb.opcode)
  in
  let mnemonic =
    match (arm.mnemonic, thumb.mnemonic) with
    | Mnemonic.Supported s, Mnemonic.Supported s' ->
        Mnemonic.supported () (fun ppf _ ->
            Format.fprintf ppf "[arm] %s | [thumb] %s" s s')
    | _, _ -> assert false
  in
  create (Size.Byte.to_int size) opcode mnemonic

let merge_result arm thumb =
  match (arm, thumb) with
  | (_, _, Some _), (_, _, Some _) ->
      ( Unisim_helper.empty_instruction,
        Unisim_helper.die,
        Some Unisim_helper.Failure )
  | (_, _, Some _), (ins, body, None) -> (ins, assert_mode thumb_mode body, None)
  | (ins, body, None), (_, _, Some _) ->
      (ins, assert_mode (Dba.Expr.lognot thumb_mode) body, None)
  | (ins, body, None), (ins', body', None) ->
      (merge_ginstr ins ins', merge_dhunk body body', None)

let decode_from_reader addr reader =
  let ins, body, status =
    match SupportedModes.get () with
    | Both ->
        merge_result
          (decode_from_reader_arm addr reader)
          (decode_from_reader_thumb addr reader)
    | Arm -> decode_from_reader_arm addr reader
    | Thumb -> decode_from_reader_thumb addr reader
  in
  (match status with
  | None ->
      Lreader.advance reader (Size.Byte.to_int ins.size);
      Parser.incr_success ins.opcode
  | Some err ->
      Lreader.advance reader (Size.Byte.to_int ins.size);
      Parser.incr_error err ins.opcode);
  (ins, body)

let decode reader (addr : Virtual_address.t) =
  let res = decode_from_reader (addr :> int) reader in
  Logger.debug ~level:5 "@[%a@]" Dhunk.pp (snd res);
  Logger.debug ~level:3 "@[%a@]" Parser.pp_statistics ();
  res

let cached_decode reader =
  let h = Virtual_address.Htbl.create 7 in
  fun (addr : Virtual_address.t) ->
    match Virtual_address.Htbl.find h addr with
    | res -> res
    | exception Not_found ->
        let res = decode reader addr in
        Virtual_address.Htbl.add h addr res;
        res

let () =
  Disasm_core.register_decoder
    (Machine.armv7 Machine.LittleEndian)
    decode Fun.id
