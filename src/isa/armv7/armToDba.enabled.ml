(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

open Basic_types.Integers

module Logger = Isa_logger.Sub (struct
  let name = "arm"
end)

module Parser = Unisim_helper.Make (Logger)

let decode_arm addr bytes =
  Arm32dba.decode ~thumb:false ~addr bytes |> Parser.parse_message

let decode_from_reader_arm addr reader =
  if Virtual_address.modi addr 4 <> 0 then
    ( Unisim_helper.empty_instruction,
      Unisim_helper.die,
      Some Unisim_helper.Undefined )
  else
    match Reader.Peek.i32 reader with
    | exception _ ->
        ( Unisim_helper.empty_instruction,
          Unisim_helper.die,
          Some Unisim_helper.Failure )
    | bytes -> decode_arm (Virtual_address.to_int32 addr) bytes

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
          (Virtual_address.add_int (Size.Byte.to_int instrs.(i).size) addr)
          (Dba_types.Caddress.Map.add
             (Dba_types.Caddress.block_start addr)
             (Dba.Jump_target.inner sizes.(i))
             map)
    in
    let map =
      iter 1
        (Virtual_address.add_int (Size.Byte.to_int instrs.(0).size) addr)
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
        (Option.get (inst itblock.(!j) (i - sizes.(!j)))))

let pp_opcode ppf bv =
  for i = 0 to (Bitvector.size_of bv / 8) - 1 do
    Format.fprintf ppf "%02x"
      (Z.to_int
         Bitvector.(value_of (extract ~hi:((8 * (i + 1)) - 1) ~lo:(8 * i) bv)))
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
    if Virtual_address.modi addr 2 = 1 then (
      Reader.rewind reader 1;
      Virtual_address.pred addr)
    else addr
  in
  match Uint16.to_int (Reader.Peek.u16 reader) with
  | exception _ ->
      ( Unisim_helper.empty_instruction,
        Unisim_helper.die,
        Some Unisim_helper.Failure )
  | bytes when not @@ isitw bytes ->
      let bytes = try Reader.Peek.i32 reader with _ -> Int32.of_int bytes in
      decode_thumb 0 (Virtual_address.to_int32 addr) bytes
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
            try Reader.Peek.i32 reader
            with _ -> Uint16.to_int32 (Reader.Peek.u16 reader)
          with
          | exception _ -> raise @@ Failure ""
          | bytes -> (
              match
                decode_thumb (itstate word i)
                  (Virtual_address.to_int32
                     (Virtual_address.add_int offset addr))
                  bytes
              with
              | _, _, Some _ -> raise_notrace Not_found
              | instr, dhunk, None ->
                  itblock.(i) <- instr;
                  ithunks.(i) <- dhunk;
                  let size = Size.Byte.to_int instr.size in
                  Reader.advance reader size;
                  init (i + 1) (offset + size))
      in
      try
        let size = init 0 0 in
        Reader.rewind reader size;
        let bytes = Reader.Peek.read reader size in
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
            (Option.get (inst dhunk (i - 1))))

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
            (Option.get (inst arm (i - 1)))
      | i ->
          Dba_types.Instruction.reloc
            ~inner:(fun x -> x + larm + 1)
            (Option.get (inst thumb (i - 1 - larm))))

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

let decode_from_reader :
    thumb:Machine.thumb_mode ->
    Virtual_address.t ->
    int Reader.t ->
    Instruction.Generic.t * Dhunk.t =
 fun ~thumb addr reader ->
  let ins, body, status =
    match thumb with
    | Unknown ->
        merge_result
          (decode_from_reader_arm addr reader)
          (decode_from_reader_thumb addr reader)
    | False -> decode_from_reader_arm addr reader
    | True -> decode_from_reader_thumb addr reader
  in
  (match status with
  | None ->
      Reader.advance reader (Size.Byte.to_int ins.size);
      Parser.incr_success ins.opcode
  | Some err ->
      Reader.advance reader (Size.Byte.to_int ins.size);
      Parser.incr_error err ins.opcode);
  (ins, body)

let decode :
    ?thumb:Machine.thumb_mode ->
    int Reader.t ->
    Virtual_address.t ->
    Instruction.Generic.t * Dhunk.t =
 fun ?(thumb = Unknown) reader (addr : Virtual_address.t) ->
  let res = decode_from_reader ~thumb addr reader in
  Logger.debug ~level:5 "@[%a@]" Dhunk.pp (snd res);
  Logger.debug ~level:3 "@[%a@]" Parser.pp_statistics ();
  res

let () =
  Decoder.register
    (Machine.armv7 Machine.LittleEndian ~thumb:False)
    (decode ~thumb:False);
  Decoder.register
    (Machine.armv7 Machine.LittleEndian ~thumb:True)
    (decode ~thumb:True);
  Decoder.register
    (Machine.armv7 Machine.LittleEndian ~thumb:Unknown)
    (decode ~thumb:Unknown)
