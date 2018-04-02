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

module type S = sig
  type score_input_t
  type score_t
  type children_t
  type child_t
  type trace_t
  type history_t
  val init_children : unit -> children_t

  val print_children : children_t -> unit

  (* select a child from the working list for examination *)
  val select_child : children_t -> child_t option * children_t

  (* get unexplored children from a trace given a pivot child *)
  val next_children : TypeTraceDSE.location_t option -> trace_t -> history_t -> children_t

  val add_children : children_t -> children_t -> children_t
  
  val add_children_max_score : children_t -> child_t list -> (children_t * int)
  
  val add_children_second_max_score : children_t -> child_t list -> (children_t * int)

  val set_score : score_input_t -> unit

  val reset_max_score : unit -> unit
  val enable_max_score : unit -> unit
  val disable_max_score : unit -> unit

end


module type TypeGuideDSE =
  functor (TraceDSE_v:TypeTraceDSE.TypeTraceDSE) ->
  functor (HistoryDSE_v:TypeHistoryDSE.TypeHistoryDSE) ->
    S
  with type child_t = TraceDSE_v.child_t
   and type trace_t = TraceDSE_v.trace_t
   and type history_t = HistoryDSE_v(TraceDSE_v).history_t 
  
