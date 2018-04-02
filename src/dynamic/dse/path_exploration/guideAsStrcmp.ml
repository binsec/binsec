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
open TypeGuideDSE


module GuideAsStrcmp =
  functor (GuideDSE_v:TypeGuideDSE) ->
  functor (TraceDSE_v:TypeTraceDSE) ->
  functor (HistoryDSE_v:TypeHistoryDSE) ->
struct
  module TraceDSE = TraceDSE_v
  module HistoryDSE = HistoryDSE_v(TraceDSE_v)
  module GuideDSE = GuideDSE_v (TraceDSE_v) (HistoryDSE_v)
  type trace_t = TraceDSE_v.trace_t
  type child_t = TraceDSE_v.child_t
  type history_t = HistoryDSE.history_t


  type score_input_t = GuideDSE.score_input_t
  type score_t = GuideDSE.score_t
  type children_t = GuideDSE.children_t

  (*******************************************************************************)

  let init_children () =
        GuideDSE.enable_max_score() ;
        GuideDSE.init_children ()
  let print_children children = GuideDSE.print_children children

  let prev_strcmp_trace = ref None

  let counter_strcmp = ref 0

  (*******************************************************************************)

  let strcmp_heuristic working_list =
    let () = Printf.printf "Begin loop heuristic\n" in
    let () = GuideDSE.reset_max_score () in
    let () = GuideDSE.disable_max_score () in
    let working_list =
    match !prev_strcmp_trace with
    | None -> Printf.printf "Strcmp never saw\n";working_list
    | Some trace ->
        let strcmp_heuristic = TraceDSE.strcmp_heuristic trace in
        match strcmp_heuristic with
        | [] ->
            let () = Printf.printf "################ Loop heuristic failed\n" in
            let () = GuideDSE.reset_max_score () in
            let () = GuideDSE.disable_max_score () in
            let () = TraceDSE.clean trace in
            working_list
        | l ->
            let compare_loc_size (_,a) (_,b) = Int64.compare a b in
            let l_loc_size = List.sort_uniq compare_loc_size l in
            let l_loc_size =
              List.filter (fun (_ , x) -> Int64.compare x 3L > 0) l_loc_size in
            let new_child_loop, _ =
              TraceDSE.generate_child_in_loop trace l_loc_size in
            TraceDSE.clean trace;
            Printf.printf "################ Loop heuristic  %d\n"
              (List.length new_child_loop);
            List.iter (fun (_,x) -> Printf.printf "size %Lx " x) l_loc_size;
            Printf.printf "################ end loop heuristic \n";
            GuideDSE.reset_max_score ();
            GuideDSE.disable_max_score ();
            let working_list, n =
              GuideDSE.add_children_max_score working_list new_child_loop in
            counter_strcmp := n;
            Printf.printf "Real added %d\n" n;
            working_list
    in
    GuideDSE.select_child working_list

  (* select next child for examination: DFS visitor => select the last child *)
  let select_child (children:children_t) =
    let () =
      if !counter_strcmp > 0 then begin
        decr counter_strcmp;
        Printf.printf "Counter strcmp %d\n" (!counter_strcmp)
      end
    in
    try GuideDSE.select_child children
    with
    | DseException.DIVERGE_GUIDE ->
      Printf.printf "Diverge \n";
      strcmp_heuristic children

  (*******************************************************************************)

  let prev_trace = ref None

  let next_children previous trace history =
    let strcmp_heuristic = TraceDSE.strcmp_heuristic trace in
    let () =
        match strcmp_heuristic with
        | [] -> ()
        | _ ->  let () = match (!prev_strcmp_trace) with None -> () | Some t -> TraceDSE.clean t in
                prev_strcmp_trace := Some trace
    in
    let () =  if(!counter_strcmp)> 0  then prev_trace := Some trace in
    GuideDSE.next_children previous trace history

  let add_children current_children new_children =
        let wl  = GuideDSE.add_children current_children new_children in
        if(!counter_strcmp)> 0  then
            match (!prev_trace) with
            | Some trace ->
                let new_child_strcmp = TraceDSE.generate_child_in_strcmp trace in
                let () = TraceDSE.clean trace in
                let () = Printf.printf "New child strcmp %d\n" (List.length new_child_strcmp) in
                let wl,n = GuideDSE.add_children_second_max_score wl new_child_strcmp in
                let () = Printf.printf "Strcmp add %d\n" n in
                wl
            | None -> wl
        else wl

  let add_children_max_score children child_list = GuideDSE.add_children_max_score children child_list

  let add_children_second_max_score children child_list = GuideDSE.add_children_second_max_score children child_list

  let set_score score_file =  GuideDSE.set_score score_file
  let reset_max_score () = if(!counter_strcmp)<1 then  GuideDSE.reset_max_score ()
  let enable_max_score  () = if(!counter_strcmp)<1 then GuideDSE.enable_max_score ()
  let disable_max_score () = GuideDSE.disable_max_score ()

end
