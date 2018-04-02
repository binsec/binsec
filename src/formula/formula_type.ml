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

open Smtlib2
open Basic_types

let to_stringmap set =
  SmtVarSet.fold
    (fun elt map ->
       match elt with
       | SmtABv(name,_,_) | SmtBv(name,_) -> String.Set.add name map)
    set String.Set.empty

module SymbVar = Basic_types.String.Map

type formula_entry =
  | Comment of string
  | VarDefinition of smt_expr * smt_expr * smt_expr list
  | Constraint of smt_expr

type path_t = formula_entry list
type input_t = SmtVarSet.t

type hybrid_mem_chunk = {
  base: smt_bv_expr;
  name: string;
  mapping: smt_bv_expr Basic_types.Addr64.Map.t;
}
type hybrid_mem_t = hybrid_mem_chunk list


type formula = {
  vars : (int * int * smt_bv_expr) SymbVar.t;   (* Symbolic variables *)
  varsindex: int SymbVar.t;
  path : path_t;                        (* list of smt_expr representing constraints on the path *)
  memory : smt_abv_expr;                       (* current memory *)
  inputs : input_t;                       (* Free variables of the formula *)
  addr_size : int;                            (* Size of addresses within the formula *)

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
  optim_map: (int * (smt_expr * smt_expr * smt_expr list)) SymbVar.t; (* Map for optimisation (quick access to some variable expression) (int allow to keep the order of elements) *)
  pushed_variable: String.Set.t; (* Hold variable+inputs already sent to the solver (use for incremental solving) *)
  last_constraint: smt_expr;
}

let empty_formula ?(cst_pro=false) ?(rebase=false) ?(row=false) ?(aux_rebase=true) ?(row_plus=false) ?(eq_prop=true) (addrsize:int) : formula =
  let input = SmtVarSet.add (SmtABv("memory", addrsize, 8)) SmtVarSet.empty in
  {vars=SymbVar.empty;
   varsindex=SymbVar.add "memory" 0 SymbVar.empty;
   path=[];
   inputs=input;
   addr_size=addrsize;
   memory=SmtABvArray("memory", addrsize, 8);
   nb_input=0;
   nb_load=0;
   nb_store=0;
   nb_let=0;
   nb_op=0;
   nb_constraint=0;

   global_counter=0;
   optim_cst_prop=cst_pro;
   optim_rebase=rebase;
   aux_optim_rebase=aux_rebase;
   optim_rowplus=row_plus;
   optim_row=row;
   optim_row_k=150;
   optim_eq_prop=eq_prop;
   optim_map=SymbVar.empty;
   pushed_variable=String.Set.empty;
   hybrid_memory=[];
   last_constraint=SmtTrue;
  }
