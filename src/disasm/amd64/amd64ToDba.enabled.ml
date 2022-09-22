(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2022                                               *)
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
  type h = (string, unit) Hashtbl.t

  type opcode_tbl = (Instruction.Generic.t, unit) Hashtbl.t

  type t = {
    decoded : opcode_tbl;
    mutable n_instr : int;
    parse_failed : h;
    other_errors : h;
    not_implemented : h;
  }

  let empty =
    {
      decoded = Hashtbl.create 17;
      n_instr = 0;
      parse_failed = Hashtbl.create 7;
      other_errors = Hashtbl.create 7;
      not_implemented = Hashtbl.create 7;
    }

  let add_bytes bytes h = Hashtbl.add h bytes ()

  let size h = Hashtbl.length h

  let size_unique h =
    let open Basic_types in
    let s = String.Set.empty in
    Hashtbl.fold (fun k _ s -> String.Set.add k s) h s |> String.Set.cardinal

  let pp_h ppf h =
    Format.fprintf ppf "@[<v 0>%d (%d)@ @[<hov 0>%a@]@]" (size h)
      (size_unique h)
      (fun ppf -> Hashtbl.iter (fun k _ -> Format.fprintf ppf "%s;@ " k))
      h

  let incr_decoded (i, _) t =
    t.n_instr <- t.n_instr + 1;
    Hashtbl.replace t.decoded i ()

  let incr_parse_failed ~bytes t = add_bytes bytes t.parse_failed

  let incr_errors ~bytes t = add_bytes bytes t.other_errors

  let incr_not_implemented ~bytes t = add_bytes bytes t.not_implemented

  let pp ppf t =
    Format.fprintf ppf
      "@[<v 0>AMD64 decoding statistics@ ----@ Decoded (unique): %d (%d)@ \
       Failed parsing (unique): %a@ Not implemented (unique): %a @ Misc errors \
       (unique): %a@ @]"
      t.n_instr (Hashtbl.length t.decoded) pp_h t.parse_failed pp_h
      t.not_implemented pp_h t.other_errors
end

let stats = Statistics.empty

let show_stats ppf () = Statistics.pp ppf stats

let find key kvs =
  try List.assoc key kvs
  with Not_found ->
    Amd64_options.Logger.fatal "Decoder message has no %s field. Aborting." key

(* Some conversion functions from parsed categorized value to the expected types
   in Instruction.Generic.create *)
let to_hex_opcode = function
  | Parse_helpers.Message.Value.Int h -> Z.format "%x" h
  | Parse_helpers.Message.Value.Str s -> s

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

type error_type = ESize | EParser | EMnemonic

let dummy_parse ?(etype = EParser) s =
  let lexbuf = Lexing.from_string s in
  match Parser.decoder_base Lexer.token lexbuf |> dummy_instruction with
  | i -> Error (etype, i)
  | (exception Failure _) | (exception Parser.Error) ->
      Error (EMnemonic, empty_instruction)

let parse_result s =
  Amd64_options.Logger.debug ~level:1 "@[<v 0>Parsing %s@]" s;
  let open Lexing in
  let lexbuf = from_string s in
  try
    let i = Parser.decoder_msg Lexer.token lexbuf |> mk_instruction in
    Ok i
  with
  | Errors.Mismatched_instruction_size _ -> dummy_parse ~etype:ESize s
  | Failure _ | Parser.Error ->
      let pos = lexeme_start_p lexbuf in
      Amd64_options.Logger.error
        "@[<v 0>Probable parse error at line %d, column %d@ Lexeme was: %s@ \
         Entry was: %s@ Getting basic infos only ... @]"
        pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol)
        (Lexing.lexeme lexbuf) s;
      dummy_parse s

let decode addr bytes = Amd64dba.decode ~m64:true ~addr bytes |> parse_result

let read_sample_size = 15
(* This value is chosen to be large enough to get a sure opcode hit *)

let peek_at_most =
  let rec aux r reader = function
    | 0 -> List.rev r
    | n ->
        let r = try Lreader.Read.u8 reader :: r with _ -> r in
        aux r reader (n - 1)
  in
  aux []

let decode_from_reader addr reader =
  let bytes = peek_at_most reader read_sample_size in
  let bytes =
    Format.asprintf "%a"
      (Format.pp_print_list
         ~pp_sep:(fun _ () -> ())
         (fun ppf x -> Format.fprintf ppf "%02x" x))
      bytes
  in
  let r = decode addr bytes in
  match r with
  | Ok i ->
      Statistics.incr_decoded i stats;
      i
  | Error (etype, i) ->
      (match etype with
      | ESize -> Statistics.incr_errors ~bytes stats
      | EParser -> Statistics.incr_parse_failed ~bytes stats
      | EMnemonic -> Statistics.incr_not_implemented ~bytes stats);
      i

let decode reader (addr : Virtual_address.t) =
  let res = decode_from_reader (Virtual_address.to_int64 addr) reader in
  Amd64_options.Logger.debug ~level:3 "@[%a@]" show_stats ();
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

Disasm_core.register_decoder Machine.amd64 decode (fun x -> x)
