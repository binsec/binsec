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

type error = Undefined | Unimplemented | Unsupported | Failure

module Opcode = Basic_types.String

module Statistics = struct
  type t = {
    decoded : int Opcode.Htbl.t;
    undefined : int Opcode.Htbl.t;
    unimplemented : int Opcode.Htbl.t;
    unsupported : int Opcode.Htbl.t;
    failure : int Opcode.Htbl.t;
  }

  let empty () =
    {
      decoded = Opcode.Htbl.create 17;
      undefined = Opcode.Htbl.create 7;
      unimplemented = Opcode.Htbl.create 7;
      unsupported = Opcode.Htbl.create 7;
      failure = Opcode.Htbl.create 7;
    }

  let add_bytes tbl opcode =
    match Opcode.Htbl.find tbl opcode with
    | exception Not_found -> Opcode.Htbl.add tbl opcode 1
    | n -> Opcode.Htbl.replace tbl opcode (n + 1)

  let sum tbl = Opcode.Htbl.fold (fun _ n r -> n + r) tbl 0

  let pp ppf { decoded; undefined; unimplemented; unsupported; failure } =
    Format.fprintf ppf
      "@[<v 0>Decoded (unique): %d (%d)@ Undefined (unique): %d (%d)@ \
       Unimplemented (unique): %d (%d)@ Unsupported (unique): %d (%d)@ Misc \
       errors (unique): %d (%d)@]"
      (sum decoded)
      (Opcode.Htbl.length decoded)
      (sum undefined)
      (Opcode.Htbl.length undefined)
      (sum unimplemented)
      (Opcode.Htbl.length unimplemented)
      (sum unsupported)
      (Opcode.Htbl.length unsupported)
      (sum failure)
      (Opcode.Htbl.length failure)
end

(* Create a dummy instruction.
   This is used for "unfailing" mode where something is always returned, even in
   cases of Parser.Error.
*)
let empty_instruction =
  Instruction.Generic.create 0 "" (Mnemonic.unsupported ())

let die = Dhunk.singleton (Dba.Instr.stop (Some Dba.KO))

module Make (L : Logger.S) = struct
  let stats = Statistics.empty ()
  let pp_statistics ppf () = Statistics.pp ppf stats
  let incr_success opcode = Statistics.add_bytes stats.decoded opcode

  let incr_error err opcode =
    match err with
    | Undefined -> Statistics.add_bytes stats.undefined opcode
    | Unimplemented -> Statistics.add_bytes stats.unimplemented opcode
    | Unsupported -> Statistics.add_bytes stats.unsupported opcode
    | Failure -> Statistics.add_bytes stats.failure opcode

  let find key kvs = List.assoc key kvs

  (* Some conversion functions from parsed categorized value to the expected types
     in Instruction.Generic.create *)
  let to_hex_opcode = function
    | Parse_helpers.Message.Value.Int h -> Z.format "%02x" h
    | Parse_helpers.Message.Value.Str s -> s

  let to_mnemonic = function
    | Parse_helpers.Message.Value.Str s ->
        Mnemonic.supported s Format.pp_print_string
    | _ -> raise Not_found

  let just_integer = function
    | Parse_helpers.Message.Value.Int n -> Z.to_int n
    | _ -> raise Not_found

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

  let unsupported (ins : Instruction.Generic.t) =
    Dhunk.singleton
      (Dba.Instr.stop
         (Some (Dba.Unsupported (Mnemonic.to_string ins.mnemonic))))

  let basic_instruction infos =
    let opcode = to_hex_opcode (find "opcode" infos) in
    let mnemonic = to_mnemonic (find "mnemonic" infos) in
    let size = just_integer (find "size" infos) in
    Instruction.Generic.create size opcode mnemonic

  let parse_base s =
    let lexbuf = Lexing.from_string s in
    match basic_instruction (Parser.decoder_base Lexer.token lexbuf) with
    | exception (Parser.Error | Failure _ | Not_found) ->
        (empty_instruction, die, Some Failure)
    | ins -> (ins, unsupported ins, Some Failure)

  let parse_message s =
    L.debug ~level:1 "@[<v 0>Parsing %s@]" s;
    let lexbuf = Lexing.from_string s in
    match Parser.decoder_msg_eof Lexer.token lexbuf with
    | exception (Parser.Error | Failure _) ->
        let pos = Lexing.lexeme_start_p lexbuf in
        L.error
          "@[<v 0>Probable parse error at line %d, column %d@ Lexeme was: %s@ \
           Entry was: %s@ Getting basic infos only ... @]"
          pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol)
          (Lexing.lexeme lexbuf) s;
        parse_base s
    | _, Undefined -> (empty_instruction, die, Some Undefined)
    | infos, ((Unimplemented | Unsupported _ | Precise _) as semantics) -> (
        match basic_instruction infos with
        | exception Not_found -> (empty_instruction, die, Some Failure)
        | ins -> (
            match semantics with
            | Undefined -> assert false
            | Unimplemented -> (ins, unsupported ins, Some Unimplemented)
            | Unsupported _ -> (ins, unsupported ins, Some Unsupported)
            | Precise body -> (ins, to_block body, None)))
end
