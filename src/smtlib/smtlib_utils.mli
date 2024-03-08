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

val symbol_of_svar : Smtlib.sorted_var -> Smtlib.symbol
(** Extracts the symbol part out of the declaration of a sorted variable. *)

val sort_of_svar : Smtlib.sorted_var -> Smtlib.sort
(** Extracts the sort part out of the declaration of a sorted variable. *)

val symbol_of_vbinding : Smtlib.var_binding -> Smtlib.symbol
(** Extracts the newly defined symbol part out of a variable binding. *)

val symbols_of_sort : Smtlib.sort -> Smtlib.symbol list
val string_of_symbol : Smtlib.symbol -> string

val get_logic : Smtlib.script -> string
(** Extracts the logic name from a SMT script *)

val id_from_qid : Smtlib.qual_identifier -> Smtlib.identifier
(** [id_from_qid qid] extracts the id part of a qualified identifier *)

val is_constant_term : Smtlib.term -> bool
(** [is_constant t] checks if the term t is a constant or not.
 *  A real constant might be hidden under an annotated term.
*)

val is_variable_term : Smtlib.term -> bool
(** [is_variable t] checks if the term t is possibly a variable or not.
*)

(** {2 Creation functions} *)

val mk_symbol : string -> Smtlib.symbol
(** [mk_symbol name] creates a dummy symbol for name *)

val mk_localized_symbol : string -> Location.t -> Smtlib.symbol
val mk_idx_num : int -> Smtlib.index
val mk_id_symbol : Smtlib.symbol -> Smtlib.identifier
val mk_id_underscore : Smtlib.symbol -> Smtlib.indexes -> Smtlib.identifier
val mk_qual_identifier_identifier : Smtlib.identifier -> Smtlib.qual_identifier
val mk_sorted_var : Smtlib.symbol -> Smtlib.sort -> Smtlib.sorted_var
val mk_var_binding : Smtlib.symbol -> Smtlib.term -> Smtlib.var_binding
val mk_sort_identifier : Smtlib.symbol -> Smtlib.sort
val mk_sort_fun : Smtlib.symbol -> Smtlib.sorts -> Smtlib.sort
val mk_term_spec_constant : Smtlib.constant -> Smtlib.term
val mk_term_qual_identifier : Smtlib.qual_identifier -> Smtlib.term

val mk_term_qual_identifier_terms :
  Smtlib.qual_identifier -> Smtlib.terms -> Smtlib.term

val mk_term_let_term : Smtlib.var_bindings -> Smtlib.term -> Smtlib.term
val mk_term_forall_term : Smtlib.sorted_vars -> Smtlib.term -> Smtlib.term
val mk_term_exists_term : Smtlib.sorted_vars -> Smtlib.term -> Smtlib.term

val mk_fun_def :
  Smtlib.symbol ->
  Smtlib.sort ->
  Smtlib.sorted_vars ->
  Smtlib.term ->
  Smtlib.fun_def

val mk_cmd_declare_fun :
  Smtlib.symbol -> Smtlib.sorts -> Smtlib.sort -> Smtlib.command

val mk_cmd_define_fun : Smtlib.fun_def -> Smtlib.command

val mk_command : Smtlib.command_desc -> Smtlib.command
(** [mk_command cmd_des] creates a command with a dummy location *)
