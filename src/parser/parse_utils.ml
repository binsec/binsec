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

exception UserFriendlyParseError of string

let _ =
  Printexc.register_printer (function
    | UserFriendlyParseError s -> Some s
    | _ -> None)

let pp_pos ppf (pos : Lexing.position) =
  try
    let ic = open_in pos.pos_fname in
    let rec scan ic lnum r =
      let line = input_line ic in
      let len = String.length line in
      if len < r then scan ic (lnum + 1) (r - len - 1)
      else
        Format.fprintf ppf "(line %d, column %d in %s)@ %S@ %s^" lnum r
          pos.pos_fname line
          (String.make (r + 1) ' ')
    in
    scan ic 1 pos.pos_cnum
  with Sys_error _ | End_of_file -> ()

let try_and_parse ~parser ~lexer ~lexbuf context =
  try parser lexer lexbuf with
  | Failure _ ->
      let pos = Lexing.lexeme_end_p lexbuf in
      let line = pos.Lexing.pos_lnum in
      let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      let s =
        Printf.sprintf "Probable lexing error at line %d, column %d %s" line
          column (context ~line ~column)
      in
      raise (UserFriendlyParseError s)
  | Parsing.Parse_error | Parser.Error ->
      let pos = Lexing.lexeme_end_p lexbuf in
      let word = Lexing.lexeme lexbuf in
      let line = pos.Lexing.pos_lnum in
      let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - 1 in
      let s =
        Printf.sprintf "Parse error (line %d, column %d) at word `%s' %s" line
          column word (context ~line ~column)
      in
      raise (UserFriendlyParseError s)

let read_file ~parser ~lexer ~filename =
  try
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    let context ~line:_ ~column:_ = " in file " ^ filename in
    try
      let res = try_and_parse ~parser ~lexer ~lexbuf context in
      close_in ic;
      res
    with e ->
      close_in ic;
      raise e
  with Sys_error _ -> failwith ("Cannot open file " ^ filename)

let read_string ~parser ~lexer ~string =
  let lexbuf = Lexing.from_string string in
  let context ~line ~column =
    let lines = String.split_on_char '\n' string in
    let line_content = List.nth lines (line - 1) in
    let line_shadow =
      String.init (String.length line_content) (fun i ->
          if i = column then '^' else ' ')
    in
    Format.sprintf "\n%s\n%s\n" line_content line_shadow
  in
  try_and_parse ~parser ~lexer ~lexbuf context

let read_dba_file filename =
  let program = read_file ~parser:Parser.dba ~lexer:Lexer.token ~filename in
  let start_address = Kernel_functions.get_ep () in
  match start_address with
  | None -> program
  | Some address ->
      {
        program with
        Dba_types.start_address = Dba_types.Caddress.of_virtual_address address;
      }

exception Invalid_dba_string of string

let instruction_of_string data =
  let lexbuf = Lexing.from_string data in
  try Dbacsl_parser.term Dbacsl_token.token lexbuf with
  | Failure s ->
      let pos = Lexing.lexeme_end_p lexbuf in
      let l = pos.Lexing.pos_lnum in
      let c = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      let s =
        Format.sprintf "%s: Lexing error at line %d, character %d." s l c
      in
      raise (Invalid_dba_string s)
  | Parsing.Parse_error ->
      let pos = Lexing.lexeme_end_p lexbuf in
      let w = Lexing.lexeme lexbuf in
      let l = pos.Lexing.pos_lnum in
      let c = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - 1 in
      let s =
        Format.sprintf "Parse error at word \"%s\", line %d, character %d." w l
          c
      in
      raise (Invalid_dba_string s)
