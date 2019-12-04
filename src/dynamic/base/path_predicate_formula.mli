(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

(** SMT Path_predicate_formula manipulation module *)

(** @return the current index for the given variable *)
val get_varindex : Path_predicate_env.formula -> string -> int

(** Add a comment in in the formula (considered as a command) *)
val add_comment :
  Path_predicate_env.formula -> string -> Path_predicate_env.formula

(** @return [true] if the formula contains a variable
    named with the given string *)
val contains_variable: Path_predicate_env.formula -> string -> bool

(** Initialize the memory with all the values given in
    the map *)
val add_initial_state :
  Path_predicate_env.formula -> int Basic_types.Addr64.Map.t -> Path_predicate_env.formula

(** [add_symbolic_input name size] add a new bitvector in
    the formula. *)
val add_symbolic_input :
  Path_predicate_env.formula -> string -> int -> Path_predicate_env.formula

(** add the given [smt_expr] as constraint in the formula *)
val add_constraint :
  Path_predicate_env.formula ->
  Formula.bl_term -> Path_predicate_env.formula
(** [_comment] is unused in the body of this function *)

(** [build_formula_incremental pred ~ksteps ~forward ~pruning ~push ~file session solver]
    - [pred] predicate to check (if none just provide {!SmtTrue})
    - [~ksteps] indicate the number of backward steps to perform when running backward
    - [~forward] indicate weither or not the formula should be built forward or not (default forward)
    - [~pruning] {i don't modify this parameter}
    - [~push] indicate weither or not to push before adding the predicate
    - [~file] debug file to write output into
    - [session] solver session in which to send to request
    - [solver] solver used to solve
*)
val build_formula_incremental :
  Path_predicate_env.formula ->
  Formula.bl_term ->
  ?ksteps:int ->
  ?forward:bool ->
  ?pruning:bool ->
  ?push:bool ->
  Solver.solver_session ->
  Path_predicate_env.formula * Formula.status option

(** same as [build_formula_file] but provide a file name in which
    to output the generated formula *)
val build_formula_file :
  Path_predicate_env.formula ->
  Formula.bl_term ->
  ?ksteps:int ->
  ?forward:bool ->
  ?pruning:bool ->
  string ->
  Path_predicate_env.formula * Formula.status option

(** create unique temporary variable in the formula *)
val new_tmpvar:
  Path_predicate_env.formula -> int -> Path_predicate_env.formula * Formula.bv_term

(** getting a string code representing the optimizations activated
    in the formula *)
val optim_str :
  Path_predicate_env.formula -> string

(** [store_memory ~constraints abv_expr] add a new store
    in memory add optionally attach some additional [~constraints]
    constraints to it *)
val store_memory :
  Path_predicate_env.formula ->
  ?constraints:Formula.bl_term list ->
  Formula.ax_term -> Path_predicate_env.formula

(** main function to change a variable value in the formula.
    [change_variable name size low high ~csts expr]:
    - [name] variable name
    - [size] full size of a variable ex: register al fullsize in 32
    - [low] lower bound of an extraction ex: for al low=0
    - [high] high bouond of the variable ex: for al high=7
    - [~csts] constraints attached to the variable
    - [expr] smt_bv_expr the new variable logical value
*)
val change_variable :
  Path_predicate_env.formula ->
  string ->
  int ->
  int ->
  int ->
  ?constraints:Formula.bl_term list ->
  Formula.bv_term -> Path_predicate_env.formula

(** get a variable logical expression or create it if not
    existing. [get_var_or_create name fullsize low high](see
    [change_variable]).
    @return the formula modified and the logical value of the
    variable *)
val get_var_or_create :
  Path_predicate_env.formula ->
  string -> int -> int -> int -> Path_predicate_env.formula * Formula.bv_term

(** get a new variable name for a given variable
    (eg. if internal eax value is eax10 returns eax11) *)
val new_variable_name :
  Path_predicate_env.formula -> string -> string * Path_predicate_env.formula

(** print the logical store of a formula (all the variables) *)
val pp_stat_formula : Path_predicate_env.formula -> unit
