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


(*
  Note: The point here is that in order to make the programation easy some fields of the env are mutable
*)

type t = {
  mutable formula : Formula_type.formula;
  addr_size : int ; (* Size in bit ! *)
  mutable toplevel: bool;
  mutable config: Config_piqi.configuration;
  mutable analysis: dse_analysis_sig_t;
}

and dse_analysis_sig_t =
< get_taint : unit -> Tainting.tainting_engine;
  is_taint_computed : unit -> bool;
  get_current_dbacodeaddress : unit -> Dba.address;
  get_current_concrete_infos : unit -> Trace_type.trace_concrete_infos list;
  concretize_expr_bv : Dba.expr -> ?is_lhs:bool -> t -> Bitvector.t;
  concretize_cond : Dba.cond -> t -> bool;
  expr_to_smt :
    Dba.expr -> ?apply_cs:bool -> t ->
    Smtlib2.smt_bv_expr * Smtlib2.smt_expr list;

  compute : int;

  solve_predicate :
    Smtlib2.smt_expr ->
    ?print_stat:bool -> ?name:string -> ?push:bool -> ?pop:bool -> ?prek:int ->
    ?pruning:bool -> ?get_model:bool -> t ->
    Smtlib2.smt_result * Smt_model.t * float;
  exec : Dba_types.Statement.t -> t -> unit
>

let new_env analysis config ?(cst_pro=false) ?(rebase=false) ?(row=false)
    ?(row_plus=false) ?(eq_prop=false) (addr_size:int) =
  let f = Formula_type.empty_formula ~cst_pro ~rebase ~row ~row_plus ~eq_prop addr_size in
  {formula=f; addr_size=addr_size; toplevel=true; config; analysis=(analysis :> dse_analysis_sig_t)}
