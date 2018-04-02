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
open TypeHistoryDSE
open TypeCriteriaDSE
open TypeGuideDSE

open TraceAsFile
open HistoryAsTree
open CriteriaAsDefault
open CriteriaAsUAF
open GuideAsDFS
open GuideAsBFS
open GuideAsRandom
open GuideAsUAF
open GuideAsStrcmp
  
module DSE
    (TraceDSE_v:TypeTraceDSE)
    (HistoryDSE_v:TypeHistoryDSE)
    (CriteriaDSE_v:TypeCriteriaDSE)
    (GuideDSE_v:TypeGuideDSE) =
struct
  module TraceDSE     = TraceDSE_v
  module HistoryDSE   = HistoryDSE_v (TraceDSE)
  module CriteriaDSE  = CriteriaDSE_v (TraceDSE) (HistoryDSE_v)
  module GuideDSE     = GuideDSE_v (TraceDSE) (HistoryDSE_v)

  exception STOP_VERDICT
  exception STOP_CRITERIA
  exception STOP_TIME

  let set_score score_file = GuideDSE.set_score score_file

  let set_criteria criteria_file = CriteriaDSE.init_criteria criteria_file

  let end_analysis starting_time trace_limit_length history valid_children invalid_children =
    let current_time = Unix.time () in
    Logger.debug "trace limit length: %d" trace_limit_length;
    HistoryDSE.print_status history;
    Logger.result "SAT children: %d; UNSAT children: %d"
      (List.length valid_children)
      (List.length invalid_children);
    Logger.info "Running time: %.2f s" (current_time -. starting_time)


  let process_input previous input program argv trace_limit_length timeout starting_time working_list history invalid_children =
    let trace = TraceDSE.get_trace program argv input trace_limit_length in
    try
      if CriteriaDSE.verdict trace then raise STOP_VERDICT
      else
        (* next children *)
      if not (HistoryDSE.contains history trace) then
        let children = GuideDSE.next_children previous trace history in
        let working_list = GuideDSE.add_children working_list children in
        let history = HistoryDSE.update_history trace history in
        TraceDSE.clean trace;
        if CriteriaDSE.stop_criteria history invalid_children then raise STOP_CRITERIA;
        let current_time = Unix.time () in
        if timeout > 0.0 && current_time -. starting_time >= timeout then raise STOP_TIME;
        working_list,history
      else
        working_list,history
    with Failure reason ->
      Logger.debug "Error in trace :%s\n" reason;
      working_list, history

  let process_child
      next_child program argv trace_limit_length timeout
      starting_time working_list history invalid_children valid_children =
    let working_list,history,invalid_children,valid_children,find_child =
      match next_child with
      | Some child ->
        (
          (match TraceDSE.location_of_child child with
           | Exploration_type.One_loc (address, iteration) ->
             Logger.debug "select child: (0x%Lx, %d)\n" address iteration
           | Exploration_type.All_loc _ -> failwith "child has wrong location type");
          (* compute inverted inputs *)
          let previous = Some (TraceDSE.location_of_child child) in
          let inputs =
            let rec get_inputs child polcc =
              try
                TraceDSE.get_inputs true child polcc
              with
                | InvertChild.SHOULD_INIT ->
                        let trace = TraceDSE.trace_of_child child in
                        TraceDSE.clean trace;
                        let trace_config = TraceDSE.config_of_trace trace in
                        ignore (TraceDSE.get_trace program argv trace_config trace_limit_length);
                        get_inputs child false
                | InvertChild.TIMEOUT_SOLVER ->
                  if not polcc then
                    let _trace = TraceDSE.trace_of_child child in
                    get_inputs child true
                  else [] (* timeout of cc *)

            in get_inputs child false
          in
          match inputs with
          | [] ->
            HistoryDSE.unsat_child history child;
            Logger.warning "Cannot find any new input for this child";
            working_list,history, child :: invalid_children, valid_children, false
          | _ ->
            HistoryDSE.sat_child history child;
            let  valid_children = child :: valid_children in
            (* the inner loop: for each generated input *)
            let working_list,history =
              List.fold_left (
                fun (working_list,history) x ->
                  process_input previous x program argv
                    trace_limit_length timeout starting_time
                    working_list history invalid_children
              ) (working_list,history) inputs in
            working_list,history,invalid_children,valid_children,true
        )
      | None -> failwith "stop since there is no more child."
    in working_list,history,invalid_children,valid_children,find_child

  let select_child working_list = GuideDSE.select_child working_list

  (****************************************************************************************************************)
  (*                                                 main DSE algorithm                                           *)
  (****************************************************************************************************************)
  let explore
      (program:string) (argv:string) (prog_config:string)
      (trace_limit_length:int) (random_seed:bool) (timeout:float)
      (enable_max_score:bool) =
    let starting_time = Unix.time () in
    let prog_config = prog_config,Conf_exploration.counter_conf () in
    let history  = HistoryDSE.init_history () in
    if enable_max_score then GuideDSE.enable_max_score ();
    (* initialization phase *)
    let trace_config =
      match random_seed with
      | false -> prog_config
      | true -> failwith "Random seed not yet implemented\n"
    in
    Printf.printf "Ici\n";
    let trace = TraceDSE.get_trace program argv (trace_config) trace_limit_length in
    Printf.printf "Ici\n";
    if (CriteriaDSE.verdict trace) then raise STOP_VERDICT;
    let working_list = GuideDSE.next_children None trace history in
    let history = HistoryDSE.update_history trace history in

    (* exploration phase *)
    (* the outer loop: for each child in working list *)
    let rec main_loop working_list history invalid_children valid_children =
      try
        let next_child, working_list = select_child working_list in
        let working_list,history,invalid_children,valid_children,find_child =
          process_child next_child program argv trace_limit_length timeout
            starting_time working_list history invalid_children valid_children in
        if find_child && enable_max_score then GuideDSE.enable_max_score ();
        main_loop working_list history invalid_children valid_children
      with
      | Failure reason ->
        Logger.error "%s" reason;
        end_analysis starting_time trace_limit_length history valid_children invalid_children
      | STOP_CRITERIA ->
        Logger.info "Stop since criteria found";
        end_analysis starting_time trace_limit_length history valid_children invalid_children
      | STOP_VERDICT ->
        Logger.info "Stop since verdict found";
        end_analysis starting_time trace_limit_length history valid_children invalid_children
      | STOP_TIME ->
        Logger.info "Stop because of timeout" ;
        end_analysis starting_time trace_limit_length history valid_children invalid_children
    in
    main_loop working_list history [] []

end

module DfsDSE = DSE (TraceAsFile) (HistoryAsTree) (CriteriaAsDefault) (GuideAsDFS)
module DfsDSETree = DSE (TraceAsFile) (HistoryAsTree) (CriteriaAsDefault) (GuideAsDFS)
module BfsDSE = DSE (TraceAsFile) (HistoryAsTree) (CriteriaAsDefault) (GuideAsBFS)
module RandomDSE = DSE (TraceAsFile) (HistoryAsTree) (CriteriaAsDefault) (GuideAsRandom)
module UAFDSE = DSE (TraceAsFile) (HistoryAsTree) (CriteriaAsUAF) (GuideAsUAF)


module GuideAsStrcmpUAF = GuideAsStrcmp (GuideAsUAF)
module UafStrcmpDSE = DSE (TraceAsFile) (HistoryAsTree) (CriteriaAsUAF) (GuideAsStrcmpUAF) 
