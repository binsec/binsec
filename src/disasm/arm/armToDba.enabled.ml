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

module Statistics = struct
  open! Basic_types

  type opcode_tbl = (Instruction.Generic.t, unit) Hashtbl.t

  type t = {
    decoded : opcode_tbl;
    mutable n_instr : int;
    parse_failed : int ref;
    invalid_size : int ref;
    other_errors : unit String.Htbl.t;
    not_implemented : unit String.Htbl.t;
  }

  let empty =
    {
      decoded = Hashtbl.create 17;
      n_instr = 0;
      parse_failed = ref 0;
      invalid_size = ref 0;
      other_errors = String.Htbl.create 7;
      not_implemented = String.Htbl.create 7;
    }

  let add_bytes bytes h = String.Htbl.add h bytes ()
  let size h = String.Htbl.length h

  let size_unique h =
    let s = String.Set.empty in
    String.Htbl.fold (fun k _ s -> String.Set.add k s) h s
    |> String.Set.cardinal

  let pp_h ppf h =
    Format.fprintf ppf "@[<v 0>%d (%d)@ @[<hov 0>%a@]@]" (size h)
      (size_unique h)
      (fun ppf -> String.Htbl.iter (fun k _ -> Format.fprintf ppf "%s;@ " k))
      h

  let incr_decoded (i, _) t =
    t.n_instr <- t.n_instr + 1;
    Hashtbl.replace t.decoded i ()

  let incr_invalid_size t = incr t.invalid_size
  let incr_parse_failed t = incr t.parse_failed
  let incr_errors opcode t = add_bytes opcode t.other_errors
  let incr_not_implemented opcode t = add_bytes opcode t.not_implemented

  let pp ppf t =
    Format.fprintf ppf
      "@[<v 0>ARM decoding statistics@ ----@ Decoded (unique): %d (%d)@ Failed \
       parsing: %d@ Invalid size: %d@ Not implemented (unique): %a @ Misc \
       errors (unique): %a@ @]"
      t.n_instr (Hashtbl.length t.decoded) !(t.parse_failed) !(t.invalid_size)
      pp_h t.not_implemented pp_h t.other_errors
end

let stats = Statistics.empty
let show_stats ppf () = Statistics.pp ppf stats

let find key kvs =
  try List.assoc key kvs
  with Not_found ->
    Arm_options.Logger.debug "Decoder message has no %s field. Aborting." key;
    raise Not_found

(* Some conversion functions from parsed categorized value to the expected types
   in Instruction.Generic.create *)
let to_hex_opcode = function
  | Parse_helpers.Message.Value.Int h -> Z.format "%02x" h
  | _ -> assert false

let to_mnemonic = function
  | Parse_helpers.Message.Value.Str s ->
      Mnemonic.supported s Format.pp_print_string
  | _ -> assert false

let just_integer = function
  | Parse_helpers.Message.Value.Int n -> Z.to_int n
  | _ -> assert false

let compare_labeled_instruction (caddr1, _i1) (caddr2, _i2) =
  Dba_types.Caddress.compare caddr1 caddr2

let to_block addr_instr_list =
  (* Blocks returned by Unisimi's ARM decoded are not necessarily ordered.
     We need to do it here. The specific comparison functions explicits
     assumptions about what is expected (same virtual addresses and differences
     of identifiers).
  *)
  List.sort compare_labeled_instruction addr_instr_list
  |> List.map snd |> Dhunk.of_list

let mk_instruction (kvs, instructions) =
  let opcode = find "opcode" kvs |> to_hex_opcode in
  let mnemonic = find "mnemonic" kvs |> to_mnemonic in
  let size = find "size" kvs |> just_integer in
  let block = to_block instructions in
  let ginstr = Instruction.Generic.create size opcode mnemonic in
  (ginstr, block)

(* Create a dummy instruction.
   This is used for "unfailing" mode where something is always returned, even in
   cases of Parser.Error.
*)
let dummy_instruction kvs =
  let block = Dba.Instr.stop (Some Dba.KO) |> Dhunk.singleton in
  let opcode = find "opcode" kvs |> to_hex_opcode in
  let mnemonic = find "mnemonic" kvs |> to_mnemonic in
  let size = 4 in
  let ginstr = Instruction.Generic.create size opcode mnemonic in
  (ginstr, block)

let empty_instruction =
  let block = Dba.Instr.stop (Some Dba.KO) |> Dhunk.singleton in
  let opcode = "" in
  let mnemonic = Mnemonic.unsupported () in
  let size = 4 in
  let ginstr = Instruction.Generic.create size opcode mnemonic in
  (ginstr, block)

let dummy_parse s =
  let open Lexing in
  let lexbuf = from_string s in
  try
    let kvs = Parser.decoder_base Lexer.token lexbuf in
    let opcode = to_hex_opcode (find "opcode" kvs) in
    match dummy_instruction kvs with
    | i ->
        Statistics.incr_errors opcode stats;
        Error i
    | exception Failure _ ->
        Statistics.incr_not_implemented opcode stats;
        Error empty_instruction
  with
  | Failure _ | Parsing.Parse_error | Parser.Error ->
      Statistics.incr_parse_failed stats;
      let pos = lexeme_start_p lexbuf in
      Arm_options.Logger.error
        "@[<v 0>Probable parse error at line %d, column %d@ Lexeme was: %s@ \
         Entry was: %s@ Getting basic infos only ... @]"
        pos.pos_lnum pos.pos_cnum (Lexing.lexeme lexbuf) s;
      Error empty_instruction
  | Not_found -> Error empty_instruction

let parse_result s =
  Arm_options.Logger.debug ~level:1 "@[<v 0>Parsing %s@]" s;
  let open Lexing in
  let lexbuf = from_string s in
  try Ok (mk_instruction (Parser.decoder_msg Lexer.token lexbuf))
  with _ -> dummy_parse s

let decode_arm addr bytes =
  Arm32dba.decode ~thumb:false ~addr bytes |> parse_result

let decode_from_reader_arm addr reader =
  if addr mod 4 <> 0 then Error empty_instruction
  else
    match Lreader.Peek.i32 reader with
    | exception _ ->
        Statistics.incr_invalid_size stats;
        Error empty_instruction
    | bytes -> decode_arm (Int32.of_int addr) bytes

let merge_itblock addr itblock =
  let n = Array.length itblock in
  let sizes = Array.make (n + 1) 0 in
  for i = 1 to n do
    sizes.(i) <- sizes.(i - 1) + Dhunk.length itblock.(i - 1)
  done;
  let inner j i = i + sizes.(j) in
  let outer =
    let rec iter i map =
      if i = n then map
      else
        iter (i + 1)
          (Dba_types.Caddress.Map.add
             (Dba_types.Caddress.block_start_of_int (addr + (2 * i)))
             (Dba.Jump_target.inner sizes.(i))
             map)
    in
    let map = iter 1 Dba_types.Caddress.Map.empty in
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
  Arm32dba.decode ~thumb:true ~itstate ~addr bytes |> parse_result

let decode_from_reader_thumb addr reader =
  let addr =
    if addr mod 2 = 1 then (
      Lreader.rewind reader 1;
      addr - 1)
    else addr
  in
  match Lreader.Peek.u16 reader with
  | exception _ ->
      Statistics.incr_invalid_size stats;
      Error empty_instruction
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
      let itblock = Array.make n (fst empty_instruction)
      and ithunks = Array.make n (snd empty_instruction) in
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
              | Error _ -> raise @@ Failure ""
              | Ok (instr, dhunk) ->
                  itblock.(i) <- instr;
                  ithunks.(i) <- dhunk;
                  let size = Size.Byte.to_int instr.Instruction.Generic.size in
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
        Ok
          ( Instruction.Generic.create size opcode mnemonic,
            merge_itblock addr ithunks )
      with Failure _ -> Error empty_instruction)

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
  | Error _, Error i -> i
  | Error _, Ok (g, d) ->
      Statistics.incr_decoded (g, d) stats;
      (g, assert_mode thumb_mode d)
  | Ok (g, d), Error _ ->
      Statistics.incr_decoded (g, d) stats;
      (g, assert_mode (Dba.Expr.lognot thumb_mode) d)
  | Ok (g, d), Ok (g', d') ->
      let g, d = (merge_ginstr g g', merge_dhunk d d') in
      Statistics.incr_decoded (g, d) stats;
      (g, d)

let unwrap_result = function Error i -> i | Ok x -> x

let decode_from_reader addr reader =
  let ((Instruction.Generic.{ size; _ }, _) as r) =
    match Arm_options.SupportedModes.get () with
    | Arm_options.Both ->
        merge_result
          (decode_from_reader_arm addr reader)
          (decode_from_reader_thumb addr reader)
    | Arm_options.Arm -> decode_from_reader_arm addr reader |> unwrap_result
    | Arm_options.Thumb -> decode_from_reader_thumb addr reader |> unwrap_result
  in
  Lreader.advance reader (Size.Byte.to_int size);
  r

let decode reader (addr : Virtual_address.t) =
  let res = decode_from_reader (addr :> int) reader in
  Arm_options.Logger.debug ~level:5 "@[%a@]" Dhunk.pp (snd res);
  Arm_options.Logger.debug ~level:3 "@[%a@]" show_stats ();
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
;;

Disasm_core.register_decoder (Machine.armv7 Machine.LittleEndian) decode
  (fun x -> x)
