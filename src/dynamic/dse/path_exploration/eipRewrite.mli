(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
(*    VERIMAG                                                             *)
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
(**************************************************************************)

class eip_rewrite:
        Trace_config.t ->
       object
             val mutable check_init : bool ref
             val mutable config : Config_piqi.configuration
             val mutable counter : int
             val mutable cur_key_inst : int
             val mutable default_formula_file : string
             val mutable do_compute_taint : bool
             val mutable eip : Config_piqi.Config_piqi.uint64
             val mutable eip_is_rewrite : bool
             val init_mem : InvertChild.InitMemInput.input_entries_t
             val mutable input_entries :
               SymbolicInput.SymbolicInput.input_entries_t
             val input_from_files : InputFromFiles.symb_files
             val instruction_counter : (int64, int) Hashtbl.t
             val is_remote : bool
             val mutable list_inst_key_invert : int list
             val mutable list_location_invert :
               Exploration_type.location list
             val mutable name_trace : string
             val mutable new_conf_files : string list
             val mutable trace : Trace_type.trace
             val mutable trace_config_name : string ref
             val trace_name : string
             method private add_alias :
               string -> string -> Path_predicate_env.t -> unit
             method private add_aliases :
               (string * string) list -> Path_predicate_env.t -> unit
             method add_eip_check :
               Formula.bl_term -> Path_predicate_env.t -> Formula.bl_term
             method private add_init_mem :
               Int64.t ->
               int ->
               Trace_type.trace_concrete_infos list ->
               Path_predicate_env.t -> unit
             method private add_init_reg :
               string ->
               Trace_type.trace_concrete_infos list ->
               Path_predicate_env.t -> unit
             method private add_init_values :
               InvertChild.InitMemInput.input_entry_t ->
               Trace_type.trace_concrete_infos list ->
               Path_predicate_env.t -> unit
             method private add_input :
               SymbolicInput.SymbolicInput.input_entry_t ->
               Path_predicate_env.t -> unit
             method private add_input_constraint :
               SymbolicInput.SymbolicInput.input_entry_t ->
               Path_predicate_env.t -> bool -> unit
             method add_inst_key_to_invert : int -> unit
             method add_location_it_to_invert : int64 -> int -> unit
             method add_location_to_invert : int64 -> unit
             method private add_witness_variable :
               string ->
               Dba.Expr.t -> ?apply_cs:bool -> Path_predicate_env.t -> unit
             method private build_address_comparison_predicate :
               Dba.Expr.t ->
               Bitvector.t ->
               ?apply_cs:bool -> Path_predicate_env.t -> Formula.bl_term
             method private build_cond_predicate :
               Dba.Expr.t -> Path_predicate_env.t -> Formula.bl_term
             method private build_multiple_condition_predicate :
               Formula.bl_term list -> Formula.bl_term
             method private build_witness_bitvector_comparison_predicate :
               string -> int -> Bitvector.t -> Formula.bl_term
             method private build_witness_expr_comparison_predicate :
               string ->
               int ->
               Dba.Expr.t ->
               ?apply_cs:bool -> Path_predicate_env.t -> Formula.bl_term
             method change_entries :
               SymbolicInput.SymbolicInput.input_entries_t -> unit
             method private check_fread :
               Trace_type.trace_concrete_infos list ->
               Common_piqi.Common_piqi.uint64 *
               Libcall_piqi.Libcall_piqi.uint64 *
               Libcall_piqi.Libcall_piqi.uint64 *
               Libcall_piqi.Libcall_piqi.uint64 *
               Libcall_piqi.Libcall_piqi.uint64
             method check_fscanf :
               Trace_type.trace_concrete_infos list ->
               Libcall_piqi.Libcall_piqi.uint64 list *
               Libcall_piqi.Libcall_piqi.uint64
             method check_mmap :
               Trace_type.trace_concrete_infos list ->
               Common_piqi.Common_piqi.uint64 *
               Libcall_piqi.Libcall_piqi.uint64 *
               Libcall_piqi.Libcall_piqi.uint64
             method check_read :
               Trace_type.trace_concrete_infos list ->
               Common_piqi.Common_piqi.uint64 *
               Libcall_piqi.Libcall_piqi.uint64 *
               Libcall_piqi.Libcall_piqi.uint64 *
               Libcall_piqi.Libcall_piqi.uint64
             method private compare_address :
               Dba.Expr.t ->
               Bitvector.t ->
               ?apply_cs:bool -> Path_predicate_env.t -> Formula.bl_term
             method compute : int
             method concretize_cond :
               Dba.Expr.t -> Path_predicate_env.t -> bool
             method concretize_expr_bv :
               Dba.Expr.t ->
               ?is_lhs:bool -> Path_predicate_env.t -> Bitvector.t
             method exec :
               Dba_types.Statement.t -> Path_predicate_env.t -> unit
             method expr_to_smt :
               Dba.Expr.t ->
               ?apply_cs:bool ->
               Path_predicate_env.t -> Formula.bv_term * Formula.bl_term list
             method get_current_concrete_infos :
               unit -> Trace_type.trace_concrete_infos list
             method get_current_dbacodeaddress : unit -> Dba.address
             method get_new_conf_files : unit -> string list
             method get_taint : unit -> Tainting.tainting_engine
             method init_entries : unit -> unit
             method private input_message_received : string -> string -> unit
             method is_eip_rewrite : bool
             method private is_symbolic_condition :
               Dba.Expr.t -> Path_predicate_env.t -> bool
             method private is_symbolic_expression :
               Dba.Expr.t -> Path_predicate_env.t -> bool
             method is_taint_computed : unit -> bool
             method private post_execution : Path_predicate_env.t -> int
             method private pre_execution : Path_predicate_env.t -> unit
             method private send_message : string -> string -> unit
             method set_check_init : string -> unit
             method set_counter : int -> unit
             method solve_predicate :
               Formula.bl_term ->
               ?print_stat:bool ->
               ?name:string ->
               ?push:bool ->
               ?pop:bool ->
               ?prek:int ->
               ?pruning:bool ->
               ?get_model:bool ->
               Path_predicate_env.t -> Formula.status * Smt_model.t * float
             method private update_inputs :
               Config_piqi.Config_piqi.input_t list ->
               Config_piqi.Config_piqi.input_t list ->
               Config_piqi.Config_piqi.input_t list
             method private visit_dbainstr_after :
               int ->
               Trace_type.trace_inst ->
               Dba_types.Statement.t ->
               Path_predicate_env.t -> Path_predicate.trace_visit_action
             method visit_dbainstr_before :
               int ->
               Trace_type.trace_inst ->
               Dba_types.Statement.t ->
               Path_predicate_env.t -> Path_predicate.trace_visit_action
             method private visit_instr_after :
               int ->
               Trace_type.trace_inst ->
               Path_predicate_env.t -> Path_predicate.trace_visit_action
             method private visit_instr_before :
               int ->
               Trace_type.trace_inst ->
               Path_predicate_env.t -> Path_predicate.trace_visit_action
             method private visit_metadata :
               Trace_type.trace_inst ->
               Trace_type.metadata -> Path_predicate_env.t -> unit
           end
