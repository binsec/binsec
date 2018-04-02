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
  open SMTParserWp
  exception Eof
  let line = ref 1
}

let digit = ['0'-'9''a'-'f']
let id = ['a'-'z''A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*


rule token = parse
| "sat" { SATT }
| "unsat" { UNSATT }
| "(" { LPAREN}
| ")" { RPAREN}
| "error" {ERROR}
| '"' "line " digit+ " column " digit+": model is not available" '"'  {ERROR_MSG}
| id as ident { IDENT(ident)}
| ('#' ['x' 'b' 'o'] digit+ as num) { NUM(num)}
| '\n' { incr line; Lexing.new_line lexbuf; token lexbuf }
| [' ' '\t'] { token lexbuf }
| eof { EOF }
| _ as c { let m = Format.asprintf "SMTLexerWp: Unexpected symbol : '%c' at line %d" c !line in failwith m }
