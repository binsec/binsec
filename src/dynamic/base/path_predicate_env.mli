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

(** alias to string Map *)
module SymbVar = Basic_types.String.Map

(** Path predicate type *)
type path_t = Formula.formula

type input_t = Formula.VarSet.t

(** Structure used for the RoW hybrid.
    Each chunk of memory is represented with the same base *)
type hybrid_mem_chunk = {
  base: Formula.bv_term;
  name: string;
  mapping: Formula.bv_term Basic_types.Addr64.Map.t;
}

(** any hybrid memory (just a list of chunks) *)
type hybrid_mem_t = hybrid_mem_chunk list

(** SMT formula as represented internally *)
type formula = {
  vars : (int * int * Formula.bv_term) SymbVar.t;   (* Symbolic variables *)
  varsindex: int SymbVar.t;                             (* index of current variables (used to generate new id) *)
  path : path_t;                   (* path predicate list of either {!Comment}, {!VarDefinition} or {!Constraint} *)
  memory : Formula.ax_term;   (* current memory *)
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
  optim_map: (int * (Formula.def * Formula.bl_term list)) SymbVar.t; (* quick access to some variable expression, (int allow to keep the order of elements) *)
  pushed_variable: Basic_types.String.Set.t; (* Hold variable+inputs already sent to the solver (use for incremental solving) *)
  last_constraint: Formula.bl_term; (* keep last constraint added (to do not add it twice if the same) *)

}

(** create an empty formula *)
val empty_formula :
  ?cst_pro:bool ->
  ?rebase:bool ->
  ?row:bool ->
  ?aux_rebase:bool -> ?row_plus:bool -> ?eq_prop:bool -> int -> formula

val to_stringmap : Formula.VarSet.t -> Basic_types.String.Set.t

(** DSE Environment *)

(** Environment type *)
type t = {
  mutable formula : formula; (** Path_predicate_formula containing path predicate *)
  mutable toplevel: bool;                 (** Switch sometime used to perform an action once on env *)
  mutable config: Config_piqi.configuration; (** configuration sent to the analysis *)
  mutable analysis: dse_analysis_sig_t;   (* the analysis itself *)
}

(** signature that should implement an analysis contained
    in the analysis field *)
and dse_analysis_sig_t =
  < get_taint : unit -> Tainting.tainting_engine;
    is_taint_computed : unit -> bool;
    get_current_dbacodeaddress : unit -> Dba.address;
    get_current_concrete_infos : unit -> Trace_type.trace_concrete_infos list;
    concretize_expr_bv : Dba.Expr.t -> ?is_lhs:bool -> t -> Bitvector.t;
    concretize_cond : Dba.Expr.t -> t -> bool;
    expr_to_smt :
      Dba.Expr.t -> ?apply_cs:bool -> t ->
      Formula.bv_term * Formula.bl_term list;

    compute : int;

    solve_predicate :
      Formula.bl_term ->
      ?print_stat:bool -> ?name:string -> ?push:bool -> ?pop:bool -> ?prek:int ->
      ?pruning:bool -> ?get_model:bool -> t ->
      Formula.status * Smt_model.t * float;
    exec : Dba_types.Statement.t -> t -> unit
  >

(** Create a new environment [new_env analysis conf addr_size].
    All the new arguments are the optimisations to perform on
    the formula *)
val new_env :
  dse_analysis_sig_t ->
  Config_piqi.configuration ->
  ?cst_pro:bool -> ?rebase:bool -> ?row:bool -> ?row_plus:bool ->
  ?eq_prop:bool -> int ->
  t
