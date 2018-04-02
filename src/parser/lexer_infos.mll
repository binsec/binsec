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
  open Parser_infos
  let line = ref 1
}

let space = ' ' | '\t' | '\r'
let hex = '0' ['x']['0'-'9''A'-'F''a'-'f']*
let hexaddress = '@''0' ['x']['0'-'9''A'-'F''a'-'f']*
let digit = ['0'-'9']
let str = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*

rule token = parse


| "@" { STORELOAD }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*u" { MULTU }
  | "*s" { MULTS }
  | "/" { DIVU }
  | "/s" { DIVS }
  | "modu" { MODU }
  | "mods" { MODS }
  | "and" { AND }
  | "or" { OR }
  | "xor" { XOR }
  | "!" { NOT }
  | ">>" { CONCAT }
  | ":" { COLON }
  | ";" { SEMICOLON }
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
  | "lgoto" { LNEXT }
  | "extu" { EXTU }
  | "exts" { EXTS }
  | "->" { ARROW }
  | "<-" { ARROWINV }
  | ":=" { ASSIGN }
  | "<" { INFER }
  | ">" { SUPER }
  | "//" { ANNOT }
  | "call" { CALLFLAG }
  | "ret" { RETURNFLAG }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IFJUMP }
  | "else" { ELSE }
  | "stop" { STOP }
  | "assert" { ASSERT }
  | "assume" { ASSUME }
  | "nondet" { NONDET }
  | "nondet_assume" { NONDETASSUME }
  | "alternative" { ALTERNATIVE }
  | "addcarry" { ADDCARRY }
  | "addoverflow" { ADDOVERFLOW }
  | "high signed thresholds" { HIGHSIGNEDTHRESHOLDS }
  | "low signed thresholds" { LOWSIGNEDTHRESHOLDS }
  | "high unsigned thresholds" { HIGHUNSIGNEDTHRESHOLDS }
  | "low unsigned thresholds" { LOWUNSIGNEDTHRESHOLDS }
  | "@global thresholds" { GLOBALTHRESHOLDS }
  | "@local thresholds" { LOCALTHRESHOLDS }
  | "@global widening delay" { GLOBALWIDENINGDELAY }
  | "@local widening delay" { LOCALWIDENINGDELAY }
  | "cst" { CONSTANT }
  | "stack" { STACK }
  | "malloc" { MALLOC }
  | "free" { FREE }
  | "OK" { STATE_OK }
  | "KO" { STATE_KO }
  | "\\undef" { UNDEF }
  | "print" { PRINT }
  | "@recursive disassembly" { ENTR }
  | "@stops" { STOP }
  | "@jumps" { JUMP }
  | "@closed jumps" { CLOSEDJUMPS}
  | "@insert" { ADDINSTRAT }
  | "@replace" { REPLACEINSTRAT }
  | "@linear disassembly" { LINEAR }
  | ('"' (([^'>''"']|'>'[^'>''"'])* as st) '"') { STRING st }
  | (('-'? digit+) as s) { INT (s) }
  | (('-'? hex) as s) { let b = Lexing.lexeme_start lexbuf in
                 let e = Lexing.lexeme_end lexbuf in
                 let size = (e - b - 2) * 4 in
                 HEXA (s, size)
               }
                                | (hexaddress as s) { let b = Lexing.lexeme_start lexbuf in
                 let e = Lexing.lexeme_end lexbuf in
                 let size = (e - b - 3) * 4 in
                 HEXADDRESS (s, size)
               }

  | space+ { token lexbuf }
  | '#' [^'#' '\n']* '\n' {  incr line; Lexing.new_line lexbuf; token lexbuf }
  (* | [^'\n']*"\n" { incr line; Lexing.new_line lexbuf; token lexbuf } *)
  | "\n" { incr line; Lexing.new_line lexbuf; token lexbuf }
  | (str as s) { IDENT s }
  | eof { EOF }
  | _ { failwith((Lexing.lexeme lexbuf) ^
                    ": mistake at line " ^ string_of_int !line)}
