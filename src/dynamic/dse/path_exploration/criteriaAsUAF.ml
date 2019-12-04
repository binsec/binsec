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

open TypeTraceDSE
open TypeHistoryDSE
open Parsing_gueb

module CriteriaAsUAF = functor (TraceDSE_v:TypeTraceDSE) -> functor (HistoryDSE_v:TypeHistoryDSE) ->
struct
  type conf_criteria = string
  type trace_t = TraceDSE_v.trace_t
  type child_t = TraceDSE_v.child_t
  type history_t  = HistoryDSE_v(TraceDSE_v).history_t

  let list_alloc = ref [];;
  let list_free = ref [];;
  let list_use = ref [];;

  let init_criteria c =  add_list_event c list_alloc list_free list_use;;

  let check_uaf_event trace =
    let alloc = TraceDSE_v.find_instr_cs trace (!list_alloc) in
    let () = Printf.printf "find free\n" in
    let free = TraceDSE_v.find_instr_cs trace (!list_free) in
    let () = Printf.printf "find use\n" in
    let use = TraceDSE_v.find_instr_cs trace (!list_use) in
    let () = Printf.printf "Size check %d %d %d \n" (List.length alloc) (List.length free) (List.length use) in
    match (alloc,free,use) with
    | [] , _,_ | _,[],_| _,_,[] -> raise Not_found
    | _ ->
      (List.hd alloc, free,use);;

  let policy_uaf () =
    ["* :: * :: esp :: * => Pc";
     "* :: * :: ebp :: * => Pc";
     "* :: @[?a] := _ :: !a :: * => C";
     "* :: _ := ?e    :: @[!$$] <: !e :: * => C";
     "default => P"]

  let config_to_uaf (c:Config_piqi.configuration) =
    let open Config_piqi.Configuration in
    {c with policy = policy_uaf ()}

  let verdict trace =
    try
      let open Trace_config in
      let alloc, free, use = check_uaf_event trace in
      let config = TraceDSE_v.build_analysis_configuration trace  in
      config.configuration <- config_to_uaf config.configuration;
      let analyzer = new Uaf_detection.uaf_detection config in
      let addr = fst alloc in
      Dse_options.Logger.info "Start uaf detection %Lx %s"
        addr
        (List.fold_left (fun x y -> Printf.sprintf "%s,%Lx" x (fst y)) "" use);
      analyzer#init_entries ();
      analyzer#set_alloc addr;
      List.iter (fun x -> analyzer#set_free (fst x)) free;
      List.iter (fun x -> analyzer#set_use (fst x)) use;
      let _ret = analyzer#compute in
      analyzer#is_uaf ()
    with
      Not_found -> false;;


  (* we explore all execution paths, so stop criteria is always false *)
  let stop_criteria _history _invalid_children = false;;
end
