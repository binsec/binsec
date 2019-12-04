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

open Dse_options

let test_policy pol =
  Logger.set_tagged_entry false;
  let p = Policy_engine.parse_policy_from_string_list pol in
  Policy_engine.print_policy p

let experiment () =
  let data = File_utils.load (Kernel_options.ExecFile.get ()) in
  let lexbuf = Lexing.from_string data in
  try
    let dba = Dbacsl_parser.term Dbacsl_token.token lexbuf in
    Logger.result "%a"
      Dba_printer.Unicode.pp_instruction dba
  with
  | Failure s ->
    assert ((=) s "lexing: empty token");
    let pos = (Lexing.lexeme_end_p lexbuf) in
    let l = pos.Lexing.pos_lnum in
    let c = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
    Logger.error "Lexing error at line %d, character %d." l c;
    exit 1
  | Parsing.Parse_error ->
    let pos = (Lexing.lexeme_end_p lexbuf) in
    let w = (Lexing.lexeme lexbuf) in
    let l = pos.Lexing.pos_lnum in
    let c = (pos.Lexing.pos_cnum - pos.Lexing.pos_bol - 1) in
    Logger.error "Parse error at word \"%s\", line %d, character %d." w l c;
    exit 1


(* Should be protected by an option *)
let run () =
  let open Trace_config in
  let open Config_piqi in
  let config = Trace_config.default in
  let p = config.configuration.Configuration.policy in
  match p with
  | _ :: _ -> test_policy p
  | [] -> ()

let xp () =
  if Kernel_options.Experimental.get () then experiment ()

let _ =
  Cli.Boot.enlist ~name:"test xp" ~f:xp;
  Cli.Boot.enlist ~name:"test policy" ~f:run
