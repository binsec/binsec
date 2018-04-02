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

{

  open Policy_parser
  let line = ref 1
}

let space = ' ' | '\t' | '\r'
let digit = ['0'-'9']
let hex = '0' ['x']['0'-'9''A'-'F''a'-'f']*
let str = ['a'-'z''A'-'Z''!''?''_']['a'-'z''A'-'Z''0'-'9''_''$']*

rule token = parse
  | ".." { INTERVALSEP }
  | "::" { DCOLON }
  | "=>" { TOACTION }
  | "*"  { WILDCARD }
  | "<:" { SUBTERM }
  | "default" { DEFAULT }
  | "S" { SYMB }
  | "C" { CONC }
  | "P" { PROP }
  | "Pc" { PROPC }
  | "Ps" { PROPS }
  | "&&" { AND2 }
  | "||" { OR2 }
  | "istainted" { TAINTCHECK }
  | "Ti" { TAINTI }
  | "Tp" { TAINTP }
  | "not" { NOT2 }

  | "@" { STORELOAD }
  | "+" { PLUS }
  | "-" { MINUS }
  | "-" { UMINUS }
  | "*u" { MULTU }
  | "*s" { MULTS }
  | "/" { DIVU }
  | "/s" { DIVS }
  | "modu" { MODU }
  | "mods" { MODS }
(*| "pow" { POW }*)
  | "and" { AND }
  | "or" { OR }
  | "xor" { XOR }
  | "!" { NOT }
  | ">>" { CONCAT }
(*| ":" { COLON }*)
(*| ";" { SEMICOLON }*)
  | "lshift" { LSHIFT }
  | "rshiftu" { RSHIFTU }
  | "rshifts" { RSHIFTS }
  | "lrotate" { LROTATE }
  | "rrotate" { RROTATE }
  | "=" { EQQ }
  | "<>" { NEQ }
  | "<=u" { LEU }
  | "<=s" { LES }
  | "<u" { LTU }
  | "<s" { LTS }
  | ">=u" { GEU }
  | ">=s" { GES }
  | ">u" { GTU }
  | ">s" { GTS }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "(" { LPAR }
  | ")" { RPAR }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "," { COMMA }
  | "goto" { NEXT }
  | "extu" { EXTU }
  | "exts" { EXTS }

  | ":=" { ASSIGN }
  | "<" { INFER }
  | ">" { SUPER }

  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IFJUMP }
  | "else" { ELSE }

(*| ('"' (([^'>''"']|'>'[^'>''"'])* as st) '"') { STRING st }*)
  | (str as s) { IDENT s }
  | (hex as s) { let b = Lexing.lexeme_start lexbuf in
     let e = Lexing.lexeme_end lexbuf in
     let size = (e - b - 2) * 4 in
     HEXA (s, size)
         }
  | (digit+ as s) { INT (s) }

  | space+ { token lexbuf }
  | '#' [^'\n']* '\n' {  incr line; Lexing.new_line lexbuf; token lexbuf }
  (* | [^'\n']*"\n" { incr line; Lexing.new_line lexbuf; token lexbuf } *)
  | "\n" { incr line; Lexing.new_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { failwith((Lexing.lexeme lexbuf) ^
        ": mistake at line " ^ string_of_int !line)}
