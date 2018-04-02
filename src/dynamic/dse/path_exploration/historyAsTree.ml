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

open TypeTraceDSE
open TracesToTree

module HistoryAsTree (TraceDSE_v:TypeTraceDSE) =
struct
  type history_t = TracesToTree.node_tree option

  (*******************************************************************************)

  let init_history () = None

  (*******************************************************************************)

  let trace_to_addrs trace =
    Utils.unsafe_get_opt (TraceDSE_v.trace_to_list_addr trace)

  let contains tree trace =
     match tree with
     | None -> Logger.debug "First trace to tree"; false
     | Some tree ->
       let lists_addr = trace_to_addrs trace in
       let c = TracesToTree.contains tree lists_addr in
       let len = List.length lists_addr in
       if c then Logger.debug "Trace already saw (%d): skipping it" len
       else Logger.debug "Trace never saw (%d)" len;
       c



  let update_history trace tree =
    let tree =
      let lists_addr = trace_to_addrs trace in
      match tree with
      | None -> list_to_tree lists_addr (TraceDSE_v.name_of_trace trace)
      | Some tree ->
        TracesToTree.add_list_to_tree
          tree lists_addr (TraceDSE_v.name_of_trace trace);
        tree
    in
    TracesToTree.add_id_to_tree tree;
    TracesToTree.export tree;
    TracesToTree.loop_export tree;
    Some tree

  let print_status _h = ()

  let sat_child tree child =
    let tree = Utils.unsafe_get_opt tree in
    TracesToTree.add_id_to_tree tree;
    let loc = TraceDSE_v.location_of_child child in
    TracesToTree.sat_child tree loc;
    TracesToTree.loop_export tree

  let unsat_child tree child =
    let tree = Utils.unsafe_get_opt tree in
    TracesToTree.add_id_to_tree tree;
    let loc = TraceDSE_v.location_of_child child in
    TracesToTree.unsat_child tree loc;
    TracesToTree.loop_export tree

end
