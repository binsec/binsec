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

open Conf_exploration

type location_t = Exploration_type.location 
type control_t = ConJump of location_t | DynJump of location_t



module type TypeTraceDSE =
sig
  type trace_t
  type child_t


  (*******************************************************************************)

  val name_of_trace         : trace_t -> string
  val config_of_trace       : trace_t -> Conf_exploration.conf_t
  val tracer_ret            : trace_t -> int

  val control_of_child      : child_t -> control_t
  val location_of_child     : child_t -> location_t
  val trace_of_child        : child_t -> trace_t
  val pp_child              : child_t -> string

  val compare_child         : child_t -> child_t -> int
  (*******************************************************************************)

  val clean : trace_t -> unit

  (* generate a trace by instrumenting the program with some input and configuration *)
  val get_trace : string -> string -> Conf_exploration.conf_t -> int -> trace_t

  (* get children of a trace*)
  val get_children : location_t option -> trace_t -> child_t list
  
  val get_children_uaf : location_t option -> trace_t ->(Int64.t * Int64.t list) list -> (Int64.t * Int64.t list) list -> (Int64.t * Int64.t list) list -> child_t list * child_t list * child_t list



  (* compute inputs corresponding other branchs of a child *)
  val get_inputs : bool -> child_t -> bool -> conf_t list

  val strcmp_heuristic : trace_t -> (location_t * int64) list

  (* access to all instr *)
  (*val get_all_instr : trace_t -> Trace_type.trace_inst InstrMap.t*)

  (* list of loop instr (All_loc_(addr) *)
  val get_loop_instr : trace_t -> location_t list
  
  val get_conditional_loop_instr : trace_t -> location_t list
  
  val generate_child_in_loop : trace_t -> (Exploration_type.location *  Int64.t) list -> ((child_t) list)  * ((child_t) list)
  val generate_child_in_strcmp : trace_t -> (child_t) list

  (* list of call-ret seq, with addr -> addr  *)

  val find_instr_cs : trace_t -> (int64 * (int64 list)) list -> (int64 * (int64 list)) list
  
  
  val exists_instr_cs : trace_t -> (int64 * (int64 list)) list -> bool
   
  val build_analysis_configuration : trace_t -> Options.trace_analysis_config

  val trace_to_list_addr : trace_t -> (int64 * TracesToTree.node_type * (Int64.t list)) list option

  val call_stack_of_child : child_t -> Int64.t list


end

