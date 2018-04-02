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

(** alias to string Map *)
module SymbVar = Basic_types.String.Map

(** the three different kind of entries in the path predicate:
    - [Comment]: used to generate easy to read formula
    - [VarDefinition]: converted to [define-fun] in smtlib2 and is
    used to define the different variable used in to formula
    - [Constraint] boolean expression converted in [assert] in smtlib2 *)
type formula_entry =
  | Comment of string
  | VarDefinition of Smtlib2.smt_expr * Smtlib2.smt_expr * Smtlib2.smt_expr list (** Variable reified in [smt_expr], expression, attached constraints *)
  | Constraint of Smtlib2.smt_expr

(** Path predicate type *)
type path_t = formula_entry list

type input_t = Smtlib2.SmtVarSet.t

(** structure used for the RoW hybrid. It represent
    each chunk of memory having the same base *)
type hybrid_mem_chunk = {
  base: Smtlib2.smt_bv_expr;
  name: string;
  mapping: Smtlib2.smt_bv_expr Basic_types.Addr64.Map.t;
}

(** any hybrid memory (just a list of chunks) *)
type hybrid_mem_t = hybrid_mem_chunk list

(** SMT formula as represented internally *)
type formula = {
  vars : (int * int * Smtlib2.smt_bv_expr) SymbVar.t;   (* Symbolic variables *)
  varsindex: int SymbVar.t;                             (* index of current variables (used to generate new id) *)
  path : path_t;                   (* path predicate list of either {!Comment}, {!VarDefinition} or {!Constraint} *)
  memory : Smtlib2.smt_abv_expr;   (* current memory *)
  inputs : input_t;                (* Free variables of the formula aka inputs *)
  addr_size : int;                 (* Size of addresses *)         

  (* Statistic fields *)
  nb_input: int; (* number of inputs *)
  nb_load: int;  (* number of select in memory *)
  nb_store: int; (* number of store in memory *)
  nb_let: int;   (* number of variable definition in the path predicate [VarDefinition] *)
  nb_op: int;    (* number of binary+unaru operations *)
  nb_constraint: int; (* number of constraints in the path predicate *)

  (* Optimization fields *)
  global_counter: int;   (* Used to identify in a unique manner every entries of the path predicate *)
  optim_cst_prop: bool;  (* Constant propagation activated or not *)
  optim_rebase: bool;    (* Rebase optimization activated or not *)
  aux_optim_rebase : bool; (* Rebase var1=var2 by var2 (not wished in some cases) *)
  optim_row: bool;       (* Read over Write (for memory) *)
  optim_row_k : int;     (* number of iteration for linear Read-Over-Write *)
  optim_rowplus: bool;   (* Read-over-Write hybrid *)
  hybrid_memory: hybrid_mem_t; (* Hybrid memory for RoW hybrid *)
  optim_eq_prop: bool;   (* Equalities propagation optimization (unsafe) *)
  optim_map: (int * (Smtlib2.smt_expr * Smtlib2.smt_expr * Smtlib2.smt_expr list)) SymbVar.t; (* quick access to some variable expression, (int allow to keep the order of elements) *)
  pushed_variable: Basic_types.String.Set.t; (* Hold variable+inputs already sent to the solver (use for incremental solving) *)
  last_constraint: Smtlib2.smt_expr; (* keep last constraint added (to do not add it twice if the same) *)

}

(** create an empty formula *)
val empty_formula :
  ?cst_pro:bool ->
  ?rebase:bool ->
  ?row:bool ->
  ?aux_rebase:bool -> ?row_plus:bool -> ?eq_prop:bool -> int -> formula

val to_stringmap : Smtlib2.SmtVarSet.t -> Basic_types.String.Set.t
