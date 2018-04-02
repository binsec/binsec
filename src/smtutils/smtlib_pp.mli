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

(** Pretty-printer for SMT-LIB AST *)
val pp_spec_constant : Format.formatter -> Smtlib_ast.constant -> unit

val pp_loc : Format.formatter -> Locations.t -> unit 
(** pretty-prints a location *)

val pp_symbol : Format.formatter -> Smtlib_ast.symbol -> unit 
(** pretty-prints a SMT symbol *)

val pp_sort : Format.formatter -> Smtlib_ast.sort -> unit 
(** pretty-prints a SMT sort *)

val pp_term : Format.formatter -> Smtlib_ast.term -> unit 
(** pretty-prints a SMT term *)

val pp_qual_identifier : Format.formatter -> Smtlib_ast.qual_identifier -> unit 
(** pretty-prints a SMT qualified identifier *)

val pp: Format.formatter -> Smtlib_ast.script -> unit 
(** [pp fmt ast] pretty-prints a full SMT-LIB script onto a formatter *)


val pp_commands: Format.formatter -> Smtlib_ast.commands -> unit 
(** pp_commands pretty_prints an arbitrary command list onto a formatter.
    Used by pp.
*)

val pp_model : Format.formatter -> Smtlib_ast.model -> unit 

val pp_tofile: string -> Smtlib_ast.script -> unit 
(** [pp_tofile filename script] Prints a SMT-LIB script into the file named
 ** [filename]. The file is created if needed. Contents from any present file is
 ** not preserved.
*)
