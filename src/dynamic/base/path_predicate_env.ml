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


let to_stringmap set =
  let open Formula in
  Formula.VarSet.fold
    (fun elt map ->
       match elt with
       | BlVar bl -> Basic_types.String.Set.add bl.bl_name map
       | BvVar bv -> Basic_types.String.Set.add bv.bv_name map
       | AxVar ax -> Basic_types.String.Set.add ax.ax_name map)
    set Basic_types.String.Set.empty

module SymbVar = Basic_types.String.Map

type path_t = Formula.formula
type input_t = Formula.VarSet.t

type hybrid_mem_chunk = {
  base: Formula.bv_term;
  name: string;
  mapping: Formula.bv_term Basic_types.Addr64.Map.t;
}

type hybrid_mem_t = hybrid_mem_chunk list

type formula = {
  vars : (int * int * Formula.bv_term) SymbVar.t;   (* Symbolic variables *)
  varsindex: int SymbVar.t;
  path : path_t;                       (* list of smt_expr representing constraints on the path *)
  memory : Formula.ax_term;       (* current memory *)
  inputs : input_t;                    (* Free variables of the formula *)
  addr_size : int;                 (* Size of addresses *)

  (* Statistic fields *)
  nb_input: int;
  nb_load: int;
  nb_store: int;
  nb_let: int;
  nb_op: int;
  nb_constraint: int;

  (* Optimisation fields *)
  global_counter: int; (* Used to identify in a uniq manner every entries of the formula *)
  optim_cst_prop: bool;
  optim_rebase: bool;
  aux_optim_rebase : bool; (* Rebase var1=var2 by var2 (not wished in some cases) *)
  optim_row: bool; (* Read over Write (for memory) *)
  optim_row_k : int;
  optim_rowplus: bool;
  hybrid_memory: hybrid_mem_t;
  optim_eq_prop: bool;
  optim_map: (int * (Formula.def * Formula.bl_term list)) SymbVar.t;
  (* Map for optimisation (quick access to some variable expression) (int allow to keep the order of elements) *)
  pushed_variable: Basic_types.String.Set.t; (* Hold variable+inputs already sent to the solver (use for incremental solving) *)
  last_constraint: Formula.bl_term;
}

let empty_formula ?(cst_pro=false) ?(rebase=false) ?(row=false)
    ?(aux_rebase=true) ?(row_plus=false) ?(eq_prop=true) addr_size =
  let inputs = Formula.(VarSet.singleton (AxVar (ax_var "memory" addr_size 8))) in
  {vars = SymbVar.empty;
   varsindex = SymbVar.add "memory" 0 SymbVar.empty;
   path = Formula.empty;
   inputs;
   addr_size;
   memory =  Formula.(mk_ax_var (ax_var "memory" addr_size 8));
   nb_input = 0;
   nb_load = 0;
   nb_store = 0;
   nb_let = 0;
   nb_op = 0;
   nb_constraint = 0;
   global_counter = 0;
   optim_cst_prop = cst_pro;
   optim_rebase = rebase;
   aux_optim_rebase = aux_rebase;
   optim_rowplus = row_plus;
   optim_row = row;
   optim_row_k = 150;
   optim_eq_prop = eq_prop;
   optim_map = SymbVar.empty;
   pushed_variable  =  Basic_types.String.Set.empty;
   hybrid_memory = [];
   last_constraint = Formula.mk_bl_true;
  }

type t = {
  mutable formula : formula;
  mutable toplevel: bool;
  mutable config: Config_piqi.configuration;
  mutable analysis: dse_analysis_sig_t;
}

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

let new_env analysis config ?(cst_pro=false) ?(rebase=false) ?(row=false)
    ?(row_plus=false) ?(eq_prop=false) (addr_size:int) =
  let f = empty_formula ~cst_pro ~rebase ~row ~row_plus ~eq_prop addr_size in
  {formula=f; toplevel=true; config; analysis=(analysis :> dse_analysis_sig_t)}
