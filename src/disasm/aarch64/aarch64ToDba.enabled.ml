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
    Aarch64_options.Logger.fatal "Decoder message has no %s field. Aborting."
      key

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
  with Failure _ | Parsing.Parse_error ->
    Statistics.incr_parse_failed stats;
    let pos = lexeme_start_p lexbuf in
    Aarch64_options.Logger.error
      "@[<v 0>Probable parse error at line %d, column %d@ Lexeme was: %s@ \
       Entry was: %s@ Getting basic infos only ... @]"
      pos.pos_lnum pos.pos_cnum (Lexing.lexeme lexbuf) s;
    Error empty_instruction

let parse_result s =
  Aarch64_options.Logger.debug ~level:1 "@[<v 0>Parsing %s@]" s;
  let open Lexing in
  let lexbuf = from_string s in
  try
    let res = mk_instruction (Parser.decoder_msg Lexer.token lexbuf) in
    Statistics.incr_decoded res stats;
    Ok res
  with _ -> dummy_parse s

let decode addr bytes = Aarch64dba.decode ~addr bytes |> parse_result

let decode_from_reader (addr : Virtual_address.t) reader =
  if (addr :> int) mod 4 <> 0 then Error empty_instruction
  else
    match Lreader.Read.i32 reader with
    | exception _ ->
        Statistics.incr_invalid_size stats;
        Error empty_instruction
    | bytes -> decode (Virtual_address.to_int64 addr) bytes

let unwrap_result = function Error i -> i | Ok x -> x

let decode_from_reader addr reader =
  decode_from_reader addr reader |> unwrap_result

let decode reader addr =
  let res = decode_from_reader addr reader in
  Aarch64_options.Logger.debug ~level:5 "@[%a@]" Dhunk.pp (snd res);
  Aarch64_options.Logger.debug ~level:3 "@[%a@]" show_stats ();
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

Disasm_core.register_decoder (Machine.armv8 Machine.LittleEndian) cached_decode
  (fun x -> x)
