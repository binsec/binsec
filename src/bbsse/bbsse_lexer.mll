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


{
open Bbsse_parser
}

let space = ' ' | '\t' | '\r'
let digit = ['0'-'9']
let alpha = ['a'-'z''A'-'Z']
let alpha_num = (alpha | digit)
let hex = '0' ['x']['0'-'9''A'-'F''a'-'f']+
let bin = '0' ['b']['0''1']+


rule token = parse 
  | "expect"        { EXPECT }
  | "is"            { IS }
  | "opaque"        { OPAQUE }
  | "branch"        { BRANCH }
  | "fallthrough"   { FALLTHROUGH }
  | "unreachable"   { UNREACHABLE }
  | "clear"         { CLEAR }
  | "skip"          { SKIP }
  | "process"       { PROCESS }
  | "call"          { CALL }
  | "at"            { AT }
  | hex as s        { ADDRESS (Z.of_string s)}
  | space+          { token lexbuf }
  | '#' [^'\n']* '\n'
  | "\n"            { Lexing.new_line lexbuf; token lexbuf }
  | eof             { EOF }
  | _
      {
        let open Lexing in
        let line = (lexeme_start_p lexbuf).pos_lnum in
        let msg =
          Printf.sprintf "Unkown lexeme %s at line %d" (lexeme lexbuf) line in
        failwith msg }
