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
open Parser_ghidra
}

let space = ' ' | '\t' | '\r'
let digit = ['0'-'9']
let alpha = ['a'-'z''A'-'Z']
let alpha_num = (alpha | digit)
let hex = '0' ['x']['0'-'9''A'-'F''a'-'f']+
let bin = '0' ['b']['0''1']+
let date = digit+ '-' digit+ '-' digit+ ' ' digit+ ':' digit+ ':' digit+
let file = [ '0'-'9' 'a'-'z' 'A'-'Z' '_' '.' ]+

rule token = parse
  | "INFO"          { INFO }
  | "address"       { ADDRESS }
  | "opcode"        { OPCODE }
  | "size"          { SIZE }
  | "mnemonic"      { MNEMONIC }
  | "kind"          { KIND }
  | "successors"    { SUCCESSORS }
  | '>'             { SUPER }
  | "("             { LPAR }
  | '.'             { DOT }
  | ")"             { RPAR }
  | ('"' (([^'>''"']|'>'[^'>''"'])* as st) '"')
                    { STRING st }
  | (hex | bin | digit+)  as s
                    { NUMERIC (Z.of_string s) }
  | date            { DATE }
  | file            { FILE }
  | space+          { token lexbuf }
  | "\n"            { Lexing.new_line lexbuf; token lexbuf }
  | eof             { EOF }
  | _
      {
        let open Lexing in
        let line = (lexeme_start_p lexbuf).pos_lnum in
        let msg =
          Printf.sprintf "Unkown lexeme %s at line %d" (lexeme lexbuf) line in
        failwith msg }
