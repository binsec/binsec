(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

  type h = (int, unit) Hashtbl.t
  type opcode_tbl = (Instruction.Generic.t, unit) Hashtbl.t

  type t = {
    decoded : opcode_tbl;
    mutable n_instr : int;
    parse_failed : h;
    other_errors : h;
    not_implemented : h;
  }

  let empty = { decoded = Hashtbl.create 17;
                n_instr = 0;
                parse_failed = Hashtbl.create 7;
                other_errors = Hashtbl.create 7;
                not_implemented = Hashtbl.create 7;
              }

  let add_bytes bytes h = Hashtbl.add h bytes ()
  let size h = Hashtbl.length h
  let size_unique h =
    let open Basic_types in
    let s = Int.Set.empty in
    Hashtbl.fold (fun k _ s -> Int.Set.add k s) h s
    |> Int.Set.cardinal

  let pp_h ppf h =
    Format.fprintf ppf
      "@[<v 0>%d (%d)@ @[<hov 0>%a@]@]"
      (size h)
      (size_unique h)
      (fun ppf -> Hashtbl.iter (fun k _ -> Format.fprintf ppf "%x;@ " k))
      h

  let incr_decoded (i, _) t =
    t.n_instr <- t.n_instr + 1;
    Hashtbl.replace t.decoded i ()

  let incr_parse_failed ~bytes t =
    add_bytes bytes t.parse_failed

  let incr_errors ~bytes t = add_bytes bytes t.other_errors

  let incr_not_implemented ~bytes t = add_bytes bytes t.not_implemented


  let pp ppf t =
    Format.fprintf ppf
      "@[<v 0>\
       ARM decoding statistics@ \
       ----@ \
       Decoded (unique): %d (%d)@ \
       Failed parsing (unique): %a@ \
       Not implemented (unique): %a @ \
       Misc errors (unique): %a@ \
       @]"
      t.n_instr (Hashtbl.length t.decoded)
      pp_h t.parse_failed
      pp_h t.not_implemented
      pp_h t.other_errors

end

let stats = Statistics.empty

let show_stats ppf () = Statistics.pp ppf stats

let arm_failwith msg =
  let msg = Format.sprintf "arm : %s" msg in
  failwith msg

let get_result ic =
  let b = Buffer.create 2048 in
  let close_and_return () =
    ignore @@ Unix.close_process_in ic;
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
  with
  | Not_found ->
    Arm_options.Logger.fatal "Decoder message has no %s field. Aborting." key


(* Some conversion functions from parsed categorized value to the expected types
   in Instruction.Generic.create *)
let to_hex_opcode = function
  | Parse_helpers.Message.Value.Hex h -> Format.sprintf "%x" h
  | _ -> assert false


let to_mnemonic = function
  | Parse_helpers.Message.Value.Str s ->
    Mnemonic.supported s Format.pp_print_string
  | _ -> assert false

let just_integer = function
  | Parse_helpers.Message.Value.Int n -> n
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
  |> Dhunk.of_list


let mk_instruction (kvs, instructions) =
  let opcode = find "opcode" kvs |> to_hex_opcode in
  let mnemonic = find "mnemonic" kvs |> to_mnemonic in
  let size = find "size" kvs |> just_integer in
  let block = to_block instructions in
  let ginstr = Instruction.Generic.create size opcode mnemonic in
  ginstr, block

(* Create a dummy instruction.
   This is used for "unfailing" mode where something is always returned, even in
   cases of Parser.Error.
*)
let dummy_instruction kvs =
  let block = Dba.Instr.stop (Some Dba.KO) |> Dhunk.singleton in
  let opcode   = find "opcode" kvs |> to_hex_opcode in
  let mnemonic = find "mnemonic" kvs |> to_mnemonic in
  let size = 4 in
  let ginstr = Instruction.Generic.create size opcode mnemonic in
  ginstr, block


let empty_instruction =
  let block = Dba.Instr.stop (Some Dba.KO) |> Dhunk.singleton in
  let opcode = "" in
  let mnemonic = Mnemonic.unsupported () in
  let size = 4 in
  let ginstr = Instruction.Generic.create size opcode mnemonic in
  ginstr, block


type error_type =
  | ESize
  | EParser
  | EMnemonic


let dummy_parse ?(etype=EParser) s =
  let lexbuf = Lexing.from_string s in
  match Parser.decoder_base Lexer.token lexbuf |> dummy_instruction with
  | i -> Error (etype, i)
  | exception Failure _ -> Error (EMnemonic, empty_instruction)

let parse_result s =
  Arm_options.Logger.debug ~level:1 "@[<v 0>Parsing %s@]" s;
  let open Lexing in
  let lexbuf = from_string s in
  try
    let i = Parser.decoder_msg Lexer.token lexbuf |> mk_instruction in
    Ok i
  with
  | Errors.Mismatched_instruction_size _  ->
    dummy_parse ~etype:ESize s
  | Failure _ ->
    dummy_parse s
  | Parser.Error  ->
    let pos = lexeme_start_p lexbuf in
    Arm_options.Logger.error
      "@[<v 0>Probable parse error at line %d, column %d@ \
       Lexeme was: %s@ \
       Entry was: %s@ \
       Getting basic infos only ... \
       @]"
      pos.pos_lnum pos.pos_cnum (Lexing.lexeme lexbuf) s;
    dummy_parse s


let get_and_parse_result ic = get_result ic |> parse_result


let run_external_decoder addr bytes =
  let exe = Kernel_options.Decoder.get () in
  let cmd = Format.sprintf "%s arm 0x%x 0x%x" exe addr bytes in
  Unix.open_process_in cmd
;;

let read_sample_size = 4
(* This value is chosen to be large enough to get a sure opcode hit *)

let peek_at_most reader size =
  let rec aux n =
    if n <= 0 then failwith "Trying to decode an empty bitstream"
    else
      try Lreader.Peek.peek reader n
      with _ -> aux (n - 1)
  in aux size
;;

let decode_from_reader addr reader =
  let bytes = peek_at_most reader read_sample_size
              |> Bitvector.value_of |> Bigint.int_of_big_int in
  let r = run_external_decoder addr bytes
          |> get_and_parse_result in
  match r with
  | Ok i ->
    Statistics.incr_decoded i stats;
    i
  | Error (etype, i) ->
    begin
      match etype with
      | ESize -> Statistics.incr_errors ~bytes stats
      | EParser -> Statistics.incr_parse_failed ~bytes stats
      | EMnemonic -> Statistics.incr_not_implemented ~bytes stats
    end;
    i


let decode reader (addr:Virtual_address.t) =
  let res = decode_from_reader (addr:>int) reader in
  Arm_options.Logger.debug ~level:3 "@[%a@]" show_stats ();
  res
;;

let cached_decode reader =
  let h = Virtual_address.Htbl.create 7 in
  fun (addr:Virtual_address.t) ->
  match Virtual_address.Htbl.find h addr with
  | res -> res
  | exception Not_found ->
     let res = decode reader addr in
     Virtual_address.Htbl.add h addr res;
     res
;;
