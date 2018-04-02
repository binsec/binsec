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

exception Invalid_dba_string of string

let instruction_of_string (data:string): Dba.instruction =
  let lexbuf = Lexing.from_string data in
  try
    Dbacsl_parser.term Dbacsl_token.token lexbuf
  with
  | Failure s ->
    let pos = Lexing.lexeme_end_p lexbuf in
    let l = pos.Lexing.pos_lnum in
    let c = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
    let s = Format.asprintf "%s: Lexing error at line %d, character %d." s l c in
    raise (Invalid_dba_string s)
  | Parsing.Parse_error ->
    let pos = Lexing.lexeme_end_p lexbuf in
    let w = Lexing.lexeme lexbuf in
    let l = pos.Lexing.pos_lnum in
    let c = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - 1 in
    let s = Format.asprintf "Parse error at word \"%s\", line %d, character %d." w l c in
    raise (Invalid_dba_string s)
