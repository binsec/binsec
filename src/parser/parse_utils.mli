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

(** General BINSEC related utilities *)

exception UserFriendlyParseError of string
(** the exception for errors in read_* functions. Use `to_string` on it to get
 * a user-friendly error message *)

val pp_pos : Format.formatter -> Lexing.position -> unit
(** pretty print a lexing position with some context *)

val read_file :
  parser:('a -> Lexing.lexbuf -> 'b) -> lexer:'a -> filename:string -> 'b
(** parses a file with nice error messages *)

val read_string :
  parser:('a -> Lexing.lexbuf -> 'b) -> lexer:'a -> string:string -> 'b
(** parses the content of a string with nice error messages *)

val read_dba_file : string -> Dba_types.program

exception Invalid_dba_string of string

val instruction_of_string : string -> Dba.Instr.t
