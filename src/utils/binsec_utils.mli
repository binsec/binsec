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

(** General BINSEC related utilities *)

val read_file:
  parser:('a -> Lexing.lexbuf -> 'b) -> lexer:'a -> filename:string -> unit -> 'b

val read_dba_file : string -> 'a Dba_types.program

val load_dba_definition : Machine.isa -> 'a Dba_types.program

val read_optional_config_file : string option -> Infos.t
(** [read_optional_config_file optfile] parses [optfile] if it is not [None].
    Otherwise, or in case of parse error, it returns [Infos.default].
    If a start address has been set on the command line, it tries to set it.

    Caveat: if an entry point has been set both through a configuration file and
    the command line, [read_optional_config_file] fails.
*)
