(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
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

type heap_event_t =
  | ALLOC of (int * int64 * int64)
  | ALLOC_UAF of (int * int64 * int64)
  | FREE of (int * int64)
  | DFREE of (int * int64)

exception DOUBLEFREE of int * int64

class uaf_detection :
  Options.trace_analysis_config ->
  object inherit Path_predicate.dse_analysis
    val mutable alloc_addr : Int64.t ref
    val mutable alloc_nth : int ref
    method add_alloc :
      Path_pred_env.t ->
      Libcall_piqi.Libcall_piqi.uint64 ->
      Libcall_piqi.Libcall_piqi.uint64 -> bool -> unit
    method add_free : Libcall_piqi.Libcall_piqi.uint64 -> unit
    method add_inst_key_to_invert : int -> unit
    method add_location_it_to_invert : int64 -> int -> unit
    method add_location_to_invert : int64 -> unit
    method add_free_check :
      Smtlib2.smt_expr -> Path_pred_env.t -> Smtlib2.smt_expr
    method add_uaf_check :
      Smtlib2.smt_expr ->
      string * Dba.size -> Path_pred_env.t -> Smtlib2.smt_expr
    method call_free : Libcall_piqi.Libcall_piqi.free_t option -> unit
    method call_malloc :
      Path_pred_env.t ->
      Libcall_piqi.malloc_t option -> Int64.t -> int -> unit
    method change_entries :
      SymbolicInput.SymbolicInput.input_entries_t -> unit
    method check_fscanf :
      Trace_type.trace_concrete_infos list ->
      Libcall_piqi.Libcall_piqi.uint64 list *
      Libcall_piqi.Libcall_piqi.uint64
    method check_lib :
      Trace_type.trace_inst -> Path_pred_env.t -> int -> unit
    method check_mmap :
      Trace_type.trace_concrete_infos list ->
      Common_piqi.Common_piqi.uint64 * Libcall_piqi.Libcall_piqi.uint64 *
      Libcall_piqi.Libcall_piqi.uint64
    method check_read :
      Trace_type.trace_concrete_infos list ->
      Libcall_piqi.Libcall_piqi.uint64 * Libcall_piqi.Libcall_piqi.uint64 *
      Libcall_piqi.Libcall_piqi.uint64 * Libcall_piqi.Libcall_piqi.uint64
    method check_with_visitor : string -> Path_pred_env.t -> unit
    method get_new_conf_files : unit -> string list
    method init_entries : unit -> unit
    method is_uaf : unit -> bool
    method set_alloc : Int64.t -> unit
    method set_free : Int64.t -> unit
    method set_use : Int64.t -> unit
    method set_check_init : string -> unit
    method set_counter : int -> unit
    method set_nth_alloc : int -> unit
    method set_nth_free : int -> unit
    method set_nth_use : int -> unit
  end
