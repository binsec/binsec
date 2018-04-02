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

(* TODO Not working since the merge *)

module GuideAsRandom = functor (TraceDSE_v:TypeTraceDSE) -> functor (HistoryDSE_v:TypeHistoryDSE) ->
struct
  module HistoryDSE = HistoryDSE_v(TraceDSE_v)

  type trace_t = TraceDSE_v.trace_t
  type child_t = TraceDSE_v.child_t
  type history_t = HistoryDSE.history_t

  type score_input_t = string
  type score_t = int
  type children_t = TraceDSE_v.child_t list

  (*******************************************************************************)

  let init_children () = []
  let print_children (_children:children_t) = ()

  (*******************************************************************************)

  (* score of child simply return 0 *)
  let _score_child (_child:TraceDSE_v.child_t) (_history:HistoryDSE_v(TraceDSE_v).history_t) = 0

  (*******************************************************************************)

  (* select next child for examination: Random visitor *)
  let select_child (children:TraceDSE_v.child_t list) =
    match children with
    | _ :: _ ->
      let id = Random.int (List.length children) in
      let new_list = List.mapi (fun i x -> if (id = i) then (false,x) else (true,x)) children in
      let filterd_list = List.filter fst new_list in
      Some (List.nth children id), List.map snd filterd_list
    | _ -> None, []

  (*******************************************************************************)

  let next_children previous trace _history =
    TraceDSE_v.get_children previous trace

  (*******************************************************************************)

  let add_children current_children  new_children = List.append current_children new_children

  let add_children_max_score children _child_list = children,0

  let add_children_second_max_score children _child_list = children,0

  let set_score _score_file = ()
  let reset_max_score () = ()
  let enable_max_score  () = ()
  let disable_max_score () = ()

end
