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
  open Format
  open Lexing
  open Smtlib_parser
  ;;

  let reserved = [
      "as"                    , AS;
      "let"                   , LET;
      "forall"                , FORALL;
      "exists"                , EXISTS;
      "set-logic"             , SETLOGIC;
      "set-option"            , SETOPTION;
      "get-option"            , GETOPTION;
      "set-info"              , SETINFO;
      "get-info"              , GETINFO;
      "declare-sort"          , DECLARESORT;
      "declare-fun"           , DECLAREFUN;
      "define-fun"            , DEFINEFUN;
      "define-fun-rec"        , DEFINEFUNREC;
      "declare-const"         , DECLARECONST;
      "push"                  , PUSH;
      "pop"                   , POP;
      "assert"                , ASSERT;
      "check-sat"             , CHECKSAT;
      "get-assertions"        , GETASSERTIONS;
      "get-proof"             , GETPROOF;
      "get-unsat-core"        , GETUNSATCORE;
      "get-value"             , GETVALUE;
      "get-assignment"        , GETASSIGNMENT;
      "get-model"             , GETMODEL;
      "get-unsat-assumptions" , GETUNSATASSUMPTIONS;
      "exit"                  , EXIT;
      "echo"                  , ECHO;
      "reset"                 , RESET;
      "reset-assertions"      , RESETASSERTIONS;
      (*      "lambda"                , LAMBDA; *)
      "par"                   , PAR;
      "!"                     , BANG;
      "_"                     , UNDERSCORE;
      "true"                  , BOOL(true);
      "false"                 , BOOL(false);
      "meta-info"             , METAINFO;
      "model"                 , MODEL;
  ];;

  let reserved_table =
    let len = List.length reserved in
    let h = Hashtbl.create len in
    List.iter
      (fun (s, k) -> Hashtbl.add h s k ) reserved;
    h
  ;;

(* To buffer string literals *)

let string_buffer = Buffer.create 256

let reset_string_buffer () =
  Buffer.reset string_buffer

let store_string_char c = Buffer.add_char string_buffer c

let store_string str = Buffer.add_string string_buffer str

let get_stored_string () =
  let s = Buffer.contents string_buffer in
  reset_string_buffer ();
  s

(* To store the position of the beginning of a string and comment *)
let string_start_loc = ref Locations.none;;

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;

  exception LexError of string ;;

}

(* Some regular expressions *)
let newline = ('\010' | '\013' | "\013\010")
let space = [' ' '\t' '\r']
let digit = ['0'-'9']
let numeral = ('0' | ['1'-'9'] digit*)
let hexadigit = (digit | ['a'-'f'])
let lower = ['a'-'z']
let upper = ['A'-'Z']
let other = ['~' '!' '@' '$' '%' '^' '&' '*'
                 '_' '-' '+' '=' '<' '>' '.' '?' '/''|'
            ]
let startchar= (lower | upper | other)

rule token = parse
  | newline   { Lexing.new_line lexbuf; token lexbuf }
  | space+    { token lexbuf }
  | ';'       { comment lexbuf; (* See the comment rule below  *)
                Lexing.new_line lexbuf;
                token lexbuf }
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | numeral   { NUMERAL(Lexing.lexeme lexbuf) }
  | "#b" (['0'-'1']+ as s)
              { BINARY(s) }
  | "#x" (hexadigit+ as s) { HEXADECIMAL(s) }
  | numeral '.' digit*
              { DECIMAL(Lexing.lexeme lexbuf) }
  | "\""
      { reset_string_buffer();
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Locations.none;
        string lexbuf;
        lexbuf.lex_start_p <- string_start;
        STRING (get_stored_string()) }
  | ':' (startchar+ as s) { KEYWORD s }
  | startchar (startchar | digit)*
      {
        let s = Lexing.lexeme lexbuf in
        try Hashtbl.find reserved_table s
        with Not_found -> SYMBOL s
      }
 | '|' ([^ '|' '\\']* as s) '|' {
       for i = 0 to String.length s - 1 do
           if s.[i] = '\n' then Lexing.new_line lexbuf;
       done;
       QUOTEDSYMBOL s }
  | eof       { EOF }
  | _
      { let msg = sprintf "@[Bad character %c@]" (Lexing.lexeme_char lexbuf 0) in
        raise (LexError msg);
      }
and string = parse
  | "\"\""
      { store_string_char '"'; string lexbuf; }
  | '"'
      { () }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        string lexbuf
      }
  | eof
      { raise Not_found }
  | _
      { store_string_char (Lexing.lexeme_char lexbuf 0);
        string lexbuf }

and comment = parse
| newline
    { () }
| eof
    { Format.eprintf "Warning: unterminated comment@." }
| _
    { comment lexbuf }
