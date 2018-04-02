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
open Parser

let keywords = [
  "begin"         , BEGIN;
  "end"           , END;
  "modu"          , MODU;
  "mods"          , MODS;
  "and"           , AND ;
  "or"            , OR  ;
  "xor"           , XOR ;
  "not"           , NOT ;
  "lshift"        , LSHIFT  ;
  "rshiftu"       , RSHIFTU ;
  "rshifts"       , RSHIFTS ;
  "lrotate"       , LROTATE ;
  "rrotate"       , RROTATE ;
  "goto"          , GOTO ;
  "extu"          , EXTU ;
  "exts"          , EXTS ;
  "call"          , CALLFLAG ;
  "ret"           , RETURNFLAG ;
  "true"          , TRUE ;
  "false"         , FALSE ;
  "if"            , IF ;
  "else"          , ELSE ;
  "then"          , THEN ;
  "stop"          , STOP ;
  "assert"        , ASSERT ;
  "assume"        , ASSUME ;
  "nondet"        , NONDET ;
  "nondet_assume" , NONDETASSUME ;
  "alternative"   , ALTERNATIVE ;
  "addcarry"      , ADDCARRY ;
  "addoverflow"   , ADDOVERFLOW ;
  "cst"           , CONSTANT ;
  "stack"         , STACK ;
  "malloc"        , MALLOC ;
  "free"          , FREE ;
  "var"           , VAR;
  "print"         , PRINT;
  "big"           , BIG ;
  "little"        , LITTLE;
  "permissions"   , PERMISSIONS;
  "flag"          , FLAG;
  "temporary"     , TEMPORARY;
  "register"      , REGISTER;
  "entry_point"   , ENTRYPOINT;
  "word_size"     , WORDSIZE ;
  "endianness"    , ENDIANNESS;
]

let keyword_tbl =
  let h = Hashtbl.create (List.length keywords) in
  List.iter (fun (k, v) -> Hashtbl.add h k v) keywords;
  h

let kwd_or_ident name =
  match Hashtbl.find keyword_tbl name with
  | terminal -> terminal
  | exception Not_found -> IDENT name

}

let space = ' ' | '\t' | '\r'
let digit = ['0'-'9']
let hex = '0' ['x']['0'-'9''A'-'F''a'-'f']*
let ident = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*

rule token = parse
  | "@"             { AT }
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "*u"            { MULTU }
  | "*s"            { MULTS }
  | "/"             { DIVU }
  | "/s"            { DIVS }
  | "!"             { NOT }
  | ">>"            { CONCAT }
  | ":"             { COLON }
  | ";"             { SEMICOLON }
  | "="             { EQQ }
  | "<>"            { NEQ }
  | "<=u"           { LEU }
  | "<=s"           { LES }
  | "<u"            { LTU }
  | "<s"            { LTS }
  | ">=u"           { GEU }
  | ">=s"           { GES }
  | ">u"            { GTU }
  | ">s"            { GTS }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | "("             { LPAR }
  | ")"             { RPAR }
  | "["             { LBRACKET }
  | "]"             { RBRACKET }
  | ","             { COMMA }
  | '.'             { DOT }
  | "->"            { ARROW }
  | "<-"            { ARROWINV }
  | ":="            { ASSIGN }
  | "<"             { INFER }
  | ">"             { SUPER }
  | "//"            { ANNOT }
  | "<TEMP>"        { TEMPTAG }
  | "<FLAG>"        { FLAGTAG }
  | "\\addr"        { WORDSIZE }
  | "\\entry_point" { ENTRYPOINT }
  | "\\endianness"  { ENDIANNESS }
  | 'R'             { READ }
  | "!R"            { NREAD }
  | 'W'             { WRITE }
  | "!W"            { NWRITE }
  | "X"             { EXEC }
  | "!X"            { NEXEC }
  | "OK"            { SOK }
  | "KO"            { SKO }
  | "\\undef"       { UNDEF }
  | ('"' (([^'>''"']|'>'[^'>''"'])* as st) '"')
                    { STRING st }
  | ident as s      { kwd_or_ident s }
  | hex as s        { HEXA s }
  | digit+ as s     { INT s }
  | space+          { token lexbuf }
  | '#' [^'\n']* '\n'
  | "\n"            { Lexing.new_line lexbuf; token lexbuf }
  | eof             { EOF }
  | _
      {
        let open Lexing in
        let line = (lexeme_start_p lexbuf).pos_lnum in
        let msg =
          Format.asprintf "Unkown lexeme %s at line %d" (lexeme lexbuf) line in
        failwith msg }
