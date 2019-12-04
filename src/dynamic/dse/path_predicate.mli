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

type trace_visit_action =
  | SkipExec
  | DoExec
  | StopExec

class type dse_analysis_t =
  object
    val mutable trace : Trace_type.trace
    val mutable default_formula_file : string
    val mutable config : Config_piqi.configuration
    val mutable do_compute_taint : bool
    val mutable cur_key_inst: int
    val is_remote : bool
    val trace_name : string

    (* --- Used internally by Binsec --- *)
    method get_taint : unit -> Tainting.tainting_engine
    method is_taint_computed : unit -> bool
    method expr_to_smt :
      Dba.Expr.t -> ?apply_cs:bool -> Path_predicate_env.t ->
      Formula.bv_term * Formula.bl_term list
    method get_current_dbacodeaddress : unit -> Dba.address
    method exec : Dba_types.Statement.t -> Path_predicate_env.t -> unit
    method get_current_concrete_infos : unit -> Trace_type.trace_concrete_infos list
    method concretize_expr_bv : Dba.Expr.t -> ?is_lhs:bool -> Path_predicate_env.t -> Bitvector.t
    method concretize_cond : Dba.Expr.t -> Path_predicate_env.t -> bool
    (* ----------------------------------- *)

    (* --- External methods --- *)
    method compute : int
    method solve_predicate :
      Formula.bl_term ->
      ?print_stat:bool -> ?name:string -> ?push:bool -> ?pop:bool -> ?prek:int ->
      ?pruning:bool -> ?get_model:bool -> Path_predicate_env.t ->
      Formula.status * Smt_model.t * float
    (* ------------------------- *)

    (* --- methods overridable by child class --- *)
    method private visit_instr_before : int -> Trace_type.trace_inst -> Path_predicate_env.t -> trace_visit_action
    method private visit_instr_after : int -> Trace_type.trace_inst -> Path_predicate_env.t -> trace_visit_action
    method private visit_dbainstr_before : int -> Trace_type.trace_inst -> Dba_types.Statement.t ->
      Path_predicate_env.t -> trace_visit_action
    method private visit_dbainstr_after : int -> Trace_type.trace_inst -> Dba_types.Statement.t ->
      Path_predicate_env.t -> trace_visit_action
    method private pre_execution : Path_predicate_env.t -> unit
    method private post_execution : Path_predicate_env.t -> int
    method private input_message_received : string -> string -> unit
    method private visit_metadata : Trace_type.trace_inst -> Trace_type.metadata -> Path_predicate_env.t -> unit
    (* ------------------------------------------ *)

    (* --- Methods callable by child class --- *)
    method private send_message : string -> string -> unit
    method private compare_address : Dba.Expr.t -> Bitvector.t -> ?apply_cs:bool
      -> Path_predicate_env.t -> Formula.bl_term
    method private is_symbolic_expression : Dba.Expr.t -> Path_predicate_env.t -> bool
    method private is_symbolic_condition : Dba.Expr.t -> Path_predicate_env.t -> bool
    method private build_address_comparison_predicate : Dba.Expr.t ->
      Bitvector.t -> ?apply_cs:bool -> Path_predicate_env.t -> Formula.bl_term
    method private add_witness_variable : string -> Dba.Expr.t ->
      ?apply_cs:bool -> Path_predicate_env.t -> unit
    method private build_witness_bitvector_comparison_predicate : string -> int ->
      Bitvector.t -> Formula.bl_term
    method private build_witness_expr_comparison_predicate : string -> int ->
      Dba.Expr.t -> ?apply_cs:bool -> Path_predicate_env.t -> Formula.bl_term
    method private build_cond_predicate : Dba.Expr.t -> Path_predicate_env.t -> Formula.bl_term
    method private build_multiple_condition_predicate :
      Formula.bl_term list -> Formula.bl_term
    (* --------------------------------------- *)
  end

class dse_analysis : Trace_config.t -> dse_analysis_t
