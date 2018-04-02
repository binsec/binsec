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

(** SMT Formula manipulation module *)

(** @return the current index for the given variable *)
val get_varindex : Formula_type.formula -> string -> int

(** Add a comment in in the formula (considered as a command) *)
val add_comment :
  Formula_type.formula -> string -> Formula_type.formula

(** @return [true] if the formula contains a variable
    named with the given string *)
val contains_variable: Formula_type.formula -> string -> bool

(** Initialize the memory with all the values given in 
    the map *)
val add_initial_state :
  Formula_type.formula -> int Basic_types.Addr64.Map.t -> Formula_type.formula

(** [add_symbolic_input name size] add a new bitvector in
    the formula. *)
val add_symbolic_input :
  Formula_type.formula -> string -> int -> Formula_type.formula

(** add the given [smt_expr] as constraint in the formula *)
val add_constraint :
  Formula_type.formula ->
  Smtlib2.smt_expr -> Formula_type.formula
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
  Formula_type.formula ->
  Smtlib2.smt_expr ->
  ?ksteps:int ->
  ?forward:bool ->
  ?pruning:bool ->
  ?push:bool ->
  ?file:string ->
  Solver.solver_session ->
  Common_piqi.solver_t -> Formula_type.formula * Smtlib2.smt_result option

(** same as [build_formula_file] but provide a file name in which
    to output the generated formula *)
val build_formula_file :
  Formula_type.formula ->
  Smtlib2.smt_expr ->
  ?ksteps:int ->
  ?forward:bool ->
  ?pruning:bool ->
  string ->
  Common_piqi.solver_t -> Formula_type.formula * Smtlib2.smt_result option

(** create unique temporary variable in the formula *)
val new_tmpvar:
  Formula_type.formula -> int -> Formula_type.formula * Smtlib2.smt_bv_expr

(** getting a string code representing the optimizations activated
    in the formula *)
val optim_str :
  Formula_type.formula -> string

(** [store_memory ~constraints abv_expr] add a new store
    in memory add optionally attach some additional [~constraints]
    constraints to it *)
val store_memory :
  Formula_type.formula ->
  ?constraints:Smtlib2.smt_expr list ->
  Smtlib2.smt_abv_expr -> Formula_type.formula

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
  Formula_type.formula ->
  string ->
  int ->
  int ->
  int ->
  ?constraints:Smtlib2.smt_expr list ->
  Smtlib2.smt_bv_expr -> Formula_type.formula

(** get a variable logical expression or create it if not
    existing. [get_var_or_create name fullsize low high](see
    [change_variable]).
    @return the formula modified and the logical value of the
    variable *)
val get_var_or_create :
  Formula_type.formula ->
  string -> int -> int -> int -> Formula_type.formula * Smtlib2.smt_bv_expr

(** get a new variable name for a given variable
    (eg. if internal eax value is eax10 returns eax11) *)
val new_variable_name :
  Formula_type.formula -> string -> string * Formula_type.formula

(** print the logical store of a formula (all the variables) *)
val pp_stat_formula : Formula_type.formula -> unit
