(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
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

let arm_failwith msg =
  let msg = Format.sprintf "arm : %s" msg in
  failwith msg

let get_result ic =
  let b = Buffer.create 2048 in
  let close_and_return () =
    close_in ic;
    Buffer.contents b
  in
  try
    let rec rloop () =
      Buffer.add_string b (Pervasives.input_line ic);
      Buffer.add_char b '\n';
      rloop ()
    in rloop ()
  with
  | End_of_file -> close_and_return ()
  | _ ->
    let s = close_and_return () in
    let msg = Format.sprintf "Could not parse %s" s in
    arm_failwith msg


let find key kvs =
  try List.assoc key kvs
  with Not_found ->
    Disasm_options.Logger.fatal "Decoder message has no %s field. Aborting." key;
    exit 1


(* Some conversion functions from parsed categorized value to the expected types
   in Disasm_types.GenericInstruction.create *)
let to_hex_opcode = function
  | Parse_helpers.Message.Value.Hex h -> Format.sprintf "%x" h
  | _ -> assert false


let to_mnemonic = function
  | Parse_helpers.Message.Value.Str s ->
    Disasm_types.Mnemonic.handled s Format.pp_print_string
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
  |> List.map snd
  |> Dba_types.Block.of_list


let mk_instruction (kvs, instructions) =
  let opcode   = find "opcode" kvs |> to_hex_opcode in
  let mnemonic = find "mnemonic" kvs |> to_mnemonic in
  let size = 4 in (* fixed size for ARM opcodes -- for now *)
  let block = to_block instructions in
  let ginstr = Disasm_types.GenericInstruction.create size opcode mnemonic in
  ginstr, block

(* Create a dummy instruction.
   This is used for "unfailing" mode where something is always returned, even in
   cases of Parser.Error.
*)
let dummy_instruction kvs =
  let block = Dba_types.(Instruction.stop (Some Dba.KO) |> Block.singleton) in
  let opcode   = find "opcode" kvs |> to_hex_opcode in
  let mnemonic = find "mnemonic" kvs |> to_mnemonic in
  let size = 4 in
  let ginstr = Disasm_types.GenericInstruction.create size opcode mnemonic in
  ginstr, block


let parse_result s =
  Disasm_options.Logger.debug "@[<v 0>Parsing %s@]" s;
  let open Lexing in
  let lexbuf = from_string s in
  try Parser.decoder_msg Lexer.token lexbuf |> mk_instruction
  with
  | Parser.Error  ->
    let pos = lexeme_start_p lexbuf in
    Disasm_options.Logger.fatal
      "@[<v 0>Probable parse error at line %d, column %d@ \
           Lexeme was: %s@ \
           Entry was: %s@ \
           Getting basic infos only ... \
         @]"
      pos.pos_lnum pos.pos_cnum (Lexing.lexeme lexbuf) s;
    let lexbuf = from_string s in
    Parser.decoder_base Lexer.token lexbuf |> dummy_instruction 


let get_and_parse_result ic = get_result ic |> parse_result


let run_external_decoder addr bytes =
  let exe = Disasm_options.ArmDecoder.get () in
  let cmd = Format.sprintf "%s 0x%x 0x%x" exe addr bytes in
  Unix.open_process_in cmd


let decode_from_reader addr reader =
  let bytes = Lreader.Read.u32 reader in
  run_external_decoder addr bytes |> get_and_parse_result 


let decode (addr:Dba_types.Virtual_address.t) =
  let reader = Lreader.of_img (Loader_utils.get_img ()) ~cursor:(addr:>int) in
  decode_from_reader (addr:>int) reader
