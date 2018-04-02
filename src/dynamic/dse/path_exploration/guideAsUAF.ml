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

open TypeHistoryDSE
open TypeTraceDSE

module GuideAsUAF = functor (TraceDSE_v:TypeTraceDSE) -> functor (HistoryDSE_v:TypeHistoryDSE) ->
struct
  module HistoryDSE = HistoryDSE_v(TraceDSE_v)
  type trace_t = TraceDSE_v.trace_t
  type child_t = TraceDSE_v.child_t
  type history_t = HistoryDSE.history_t

  type state = ALLOC | FREE | USE
  type score_input_t = (string * string * string * string) (*list_event, alloc,free,use *)
  type score_t = state * int
  type scored_child_t = score_t * TraceDSE_v.child_t


  exception OUT_SUBGRAPH of bool
    
  module ChildrenScore =
  struct
        type t = scored_child_t
        (* if s1 >= s2 -> 0, if s1 > s2 -> 1, if s2 < s1 -> -1 *)
        let compare_state s1 s2 =
          if s1 = s2 then 0
          else
            match s1, s2 with
            | USE,USE -> 0
            | FREE,FREE -> 0
            | ALLOC,ALLOC -> 0
            | USE,_ -> 1
            | _,USE -> -1
            | FREE,_ -> 1
            | _,FREE -> -1

        let compare_score score_a score_b =
          match compare_state (fst score_a) (fst score_b) with
          | 0 -> compare (snd score_a) (snd score_b) 
          | 1 -> 1 
          | -1 -> -1  
          | _ -> failwith "Error compare score"
         
        let compare (child_a:scored_child_t) (child_b:scored_child_t) =
          let score_a,score_b = fst child_a, fst child_b in
          match compare_score score_a score_b with
          | 0 -> TraceDSE_v.compare_child (snd child_b) (snd child_a) (* frst : low address, prioritize small trace*)
          | v -> v
  end

  module SetScore = Set.Make(ChildrenScore)

  type children_t = SetScore.t

  let list_alloc = ref [];;
  let list_free = ref [];;
  let list_use = ref [];;
  let score_tbl_alloc = Hashtbl.create 100;;
  let score_tbl_free = Hashtbl.create 100;;
  let score_tbl_use = Hashtbl.create 100;;

  let max_score = ref None 

  let use_max_score = ref false;;

  (*******************************************************************************)

 
  let reset_max_score () = max_score := None 
  let enable_max_score () = use_max_score := true
  let disable_max_score () = use_max_score := false

  let pp_state s =
    match s with
    | ALLOC -> "alloc"
    | FREE -> "free"
    | USE -> "use"

  let pp_score s = Printf.sprintf "%d:%s" (snd s) (pp_state (fst s))
    
  (*******************************************************************************)

  let init_children () =

   SetScore.empty ;;
        

  (*******************************************************************************)

  let print_children _children = ()

  (*******************************************************************************)


  let score_child (child:TraceDSE_v.child_t) state = 
        let score_tbl = 
            match state with
            | ALLOC -> Printf.printf "Alloc\n"; (score_tbl_alloc)
            | FREE -> Printf.printf "Free\n";(score_tbl_free)
            | USE -> score_tbl_use
        in
        match TraceDSE_v.location_of_child child with
        | Exploration_type.One_loc (addr,it)  ->    
            begin
            match TraceDSE_v.control_of_child child with
                | ConJump (Exploration_type.One_loc(next_addr,_)) -> 
                    begin
                    try
                        let call_stack_of_child = TraceDSE_v.call_stack_of_child child in
                        Printf.printf "Try to find %Lx->%Lx (%s)\n" addr next_addr (String.concat ":" (List.map (fun x -> Printf.sprintf "%Lx" x) call_stack_of_child));
                        let l = Hashtbl.find score_tbl (addr,call_stack_of_child) in
                        Printf.printf "was found\n";
                           (* small check, if the current condition lead out of the subgraph, we should not look for next conditoin *)
                        let is_end_subgraph = 
                            try
                                let _,this_score = List.find (fun (t,_s) -> (Int64.compare t next_addr)=0 ) l in
                                if(this_score<0) then 
                                    let () = Printf.printf "The other cond are outside the subgraph\n" in
                                    true
                                else false
                            with Not_found -> false (* we explore a part of the graph witout score :) *)
                        in   
                        try
                            if(it > 100) then
                                raise (OUT_SUBGRAPH(is_end_subgraph))
                            else
                            try
                                let target,s = List.find (fun (t,_s) -> (Int64.compare t next_addr)!=0 ) l in
                                let () = Printf.printf "%Lx to %Lx -> %Lx score %d\n" addr next_addr target (s) in
                                if (s<0) then let () = Printf.printf "out of subgraph\n" in raise (OUT_SUBGRAPH(is_end_subgraph))
                                else ((state,s),is_end_subgraph)
                            with
                                Not_found -> let () = Printf.printf "%Lx to %Lx -> not found! \n" addr next_addr  in
                                             ((state,1),is_end_subgraph)

                        with
                            Not_found -> let () = Printf.printf "Target %Lx not know\n" addr in raise (OUT_SUBGRAPH(is_end_subgraph))
                    with
                        Not_found -> let () = Printf.printf "Target %Lx not know\n" addr in raise (OUT_SUBGRAPH(false))
                    end
                | ConJump (Exploration_type.All_loc(_)) -> failwith "Score on all loc not yet supported\n"
                | DynJump (_) -> failwith "Dyn jump not supported\n" 
                end
        | Exploration_type.All_loc(_) -> failwith ("not yet implemented ")

  (*******************************************************************************)

  (* select next child for examination *)
  let select_child (children:children_t) =
    try
        let elem = SetScore.max_elt children in
        let new_children = SetScore.remove elem children in
        let (score,child) = elem in
        let () = Printf.printf "---------------−> Score %s  (number elem %d -> %d) \n" (pp_score score) (SetScore.cardinal children) (SetScore.cardinal new_children) in
        let () =
            if(!use_max_score) then
                match !max_score with
                | Some m ->
                    if(ChildrenScore.compare_score score m >= 0) 
                    then max_score:=Some score
                    else raise DseException.DIVERGE_GUIDE
                | None ->max_score:=Some score
        
        in
        Some child, new_children
    with Not_found -> None, children

  (*******************************************************************************)

  let rec get_child_subgraph childs history state acc =
    match childs with
    | [] -> acc
    | child::tl ->
      try
        let s, is_end = score_child child state in
        let l = (s,child) :: acc in
        if is_end then l
        else get_child_subgraph tl history state l
      with
      | OUT_SUBGRAPH is_end ->
        if is_end then acc
        else  get_child_subgraph tl history state acc

  let next_children previous trace history =
    let children_with_scores =
      let childs_alloc,childs_free,childs_use = TraceDSE_v.get_children_uaf previous trace !list_alloc !list_free !list_use  in
      let s childs state = get_child_subgraph childs history state [] in
      let a,b,c = (s childs_alloc ALLOC),(s childs_free FREE),(s childs_use USE) in     
      let l a =List.length a in
      let () = Printf.printf "Split %d %d %d %d %d\n" (l a) (l b) (l c) (l childs_free) (l childs_use) in
      a@b@c
    in
    SetScore.of_list children_with_scores

  (*******************************************************************************)

  let add_children children new_children =
    SetScore.fold (fun new_child children -> SetScore.add new_child children)  new_children children

  let add_children_max_score children child_list =    
    let list_children = SetScore.elements children in
    let counter = ref 0 in
    let find_child x = List.find (fun (_,c) -> (TraceDSE_v.compare_child x c) = 0) list_children in
    (List.fold_left (fun children c -> 
        try 
            let child = find_child c in
            let () = Printf.printf "child found during strcmp add (%s) \n" (TraceDSE_v.pp_child c) in
            let children = SetScore.remove child children  in
            let () = counter:=(!counter)+1 in
            SetScore.add ((USE,Pervasives.max_int),c) children
        with
            Not_found -> let () = Printf.printf "child not found during strcmp add (%s) \n" (TraceDSE_v.pp_child c) in 
                         children
    ) children child_list,!counter) 

  let add_children_second_max_score children child_list =    
    let list_children = SetScore.elements children in
    let counter = ref 0 in
    let find_child x = List.find (fun (_,c) -> (TraceDSE_v.compare_child x c) = 0) list_children in
    (List.fold_left (fun children c -> 
        try 
            let child = find_child c in
            let () = Printf.printf "child found during strcmp add (%s) \n" (TraceDSE_v.pp_child c) in
            let children = SetScore.remove child children  in
            let () = counter:=(!counter)+1 in
            SetScore.add ((USE,Pervasives.max_int-1),c) children
        with
            Not_found -> let () = Printf.printf "child not found during strcmp second add (%s) \n" (TraceDSE_v.pp_child c) in 
                         children
    ) children child_list,!counter) 


  let set_score (l_event,alloc,free,use) =
    let () = Parsing_gueb.add_list_event l_event list_alloc list_free list_use in
    let () = Parsing_gueb.add_score_file alloc score_tbl_alloc in
    let () = Parsing_gueb.add_score_file free score_tbl_free in
    let () = Parsing_gueb.add_score_file use score_tbl_use in
    ()

end
