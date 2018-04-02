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
      Dba.expr -> ?apply_cs:bool -> Path_pred_env.t ->
      Smtlib2.smt_bv_expr * Smtlib2.smt_expr list
    method get_current_dbacodeaddress : unit -> Dba.address
    method exec : Dba_types.Statement.t -> Path_pred_env.t -> unit
    method get_current_concrete_infos : unit -> Trace_type.trace_concrete_infos list
    method concretize_expr_bv : Dba.expr -> ?is_lhs:bool -> Path_pred_env.t -> Bitvector.t
    method concretize_cond : Dba.cond -> Path_pred_env.t -> bool
    (* ----------------------------------- *)

    (* --- External methods --- *)
    method compute : int
    method solve_predicate :
      Smtlib2.smt_expr ->
      ?print_stat:bool -> ?name:string -> ?push:bool -> ?pop:bool -> ?prek:int ->
      ?pruning:bool -> ?get_model:bool -> Path_pred_env.t ->
      Smtlib2.smt_result * Smt_model.t * float
    (* ------------------------- *)

    (* --- methods overridable by child class --- *)
    method private visit_instr_before : int -> Trace_type.trace_inst -> Path_pred_env.t -> trace_visit_action
    method private visit_instr_after : int -> Trace_type.trace_inst -> Path_pred_env.t -> trace_visit_action
    method private visit_dbainstr_before : int -> Trace_type.trace_inst -> Dba_types.Statement.t ->
      Path_pred_env.t -> trace_visit_action
    method private visit_dbainstr_after : int -> Trace_type.trace_inst -> Dba_types.Statement.t ->
      Path_pred_env.t -> trace_visit_action
    method private pre_execution : Path_pred_env.t -> unit
    method private post_execution : Path_pred_env.t -> int
    method private input_message_received : string -> string -> unit
    method private visit_metadata : Trace_type.trace_inst -> Trace_type.metadata -> Path_pred_env.t -> unit
    (* ------------------------------------------ *)

    (* --- Methods callable by child class --- *)
    method private send_message : string -> string -> unit
    method private compare_address : Dba.expr -> Bitvector.t -> ?apply_cs:bool -> Path_pred_env.t -> Smtlib2.smt_expr
    method private is_symbolic_expression : Dba.expr -> Path_pred_env.t -> bool
    method private is_symbolic_condition : Dba.cond -> Path_pred_env.t -> bool
    method private build_address_comparison_predicate : Dba.expr -> Bitvector.t -> ?apply_cs:bool -> Path_pred_env.t -> Smtlib2.smt_expr
    method private add_witness_variable : string -> Dba.expr -> ?apply_cs:bool -> Path_pred_env.t -> unit
    method private build_witness_bitvector_comparison_predicate : string -> int -> Bitvector.t -> Smtlib2.smt_expr
    method private build_witness_expr_comparison_predicate : string -> int -> Dba.expr -> ?apply_cs:bool -> Path_pred_env.t -> Smtlib2.smt_expr
    method private build_cond_predicate : Dba.cond -> Path_pred_env.t -> Smtlib2.smt_expr
    method private build_multiple_condition_predicate : Smtlib2.smt_expr list -> Smtlib2.smt_expr
    (* --------------------------------------- *)
    end

class dse_analysis : Options.trace_analysis_config -> dse_analysis_t
