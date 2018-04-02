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

open Trace_type


type node_addr = int64

type node_type =
  | NODE_EIP
  | NODE_JMP
  | NODE_CALL
  | NODE_LIBCALL
  | NODE_RET
  | NODE_DYN_JMP
  | NODE_OTHER
  | LEAF of string list

type sat_type = SAT | UNSAT | NOT_TRY

type node_tree = {
  addr : node_addr ;
  mutable sons : node_tree list;
  mutable id : int;
  mutable t : node_type;
  mutable is_sat : sat_type;
  callsite : Int64.t list;
}

type t = (node_addr * node_type * int64 list) list

let mark_nodes inst_map =
  let prev_was_jmp = ref false in
  InstrMap.mapi (fun i y ->
      if i = 0 then true,y,NODE_EIP
      else
        match InstParsing.is_cond_jump y, InstParsing.is_call y,
              InstParsing.is_ret y, !prev_was_jmp with
        | (true,_,_,_) -> prev_was_jmp := true; (true, y, NODE_JMP)
        | (_,true,_,_) ->
          prev_was_jmp := false;
          let t = if InstParsing.is_libcall y then NODE_LIBCALL else NODE_CALL
          in true, y, t
        | (_,_,true,_) ->  prev_was_jmp := false; (true, y, NODE_RET)
        | (false,_,_,true) -> prev_was_jmp := false; (true, y, NODE_OTHER)
        | (false,_,_,false) -> (false, y, NODE_OTHER)

    ) inst_map

let trace_to_list_addr filename =
  try
    let tr = Trace_loader.load_partial_trace_from_file (open_in_bin filename) in
    let inst_jmp = mark_nodes tr.instrs in
    Some (
      List.rev (
        InstrMap.fold (
          fun _ (keep, elem, t) l ->
            if keep then (elem.location , t, []) :: l else l
      ) inst_jmp []))
  with _ -> None


let list_to_tree list_addr leaf_name =
  match List.rev list_addr with
  | (hd, _, callsite_son) :: tl ->
    List.fold_left
      (fun son father ->
         match father with
         | father_addr, father_t, father_callsite ->
           { addr = father_addr;
             sons = [son];
             id = 0;
             t = father_t;
             is_sat = NOT_TRY;
             callsite = father_callsite}
      )
      { addr = hd; sons = []; id = 0; t = LEAF([leaf_name]);
        is_sat = NOT_TRY; callsite = callsite_son}
      tl
  | [] -> assert false


let add_list_to_tree tree l leaf_name =
  match l with
  | (h_addr, _, _) :: h_next :: tl ->
    if h_addr <> tree.addr then failwith "Error: different eip\n";
    let rec add node h t =
      let sons = node.sons in
      try
        match t with
        | hd :: tl ->
          let next_ = List.find (fun node_s -> node_s.addr = h_addr) sons in
          add next_ hd tl
        | [] -> assert false
      with
      | _ -> node.sons <- (list_to_tree (h :: t) leaf_name) :: node.sons
    in add tree h_next tl
  | _ -> assert false


let print_begin_dot oc =
  Printf.fprintf oc "strict digraph g {\n node [shape=box];\n"

let print_end_dot oc =
  Printf.fprintf oc "}\n"

let pp_node oc id addr = function
  | NODE_EIP -> Printf.fprintf oc "%d[\"label\"=\"0x%Lx\",color=hotpink,style=filled]\n" id addr
  | NODE_CALL -> Printf.fprintf oc "%d[\"label\"=\"0x%Lx\",color=green,style=filled]\n" id addr
  | NODE_RET -> Printf.fprintf oc "%d[\"label\"=\"0x%Lx\",color=chocolate2,style=filled]\n" id addr
  | NODE_LIBCALL -> Printf.fprintf oc "%d[\"label\"=\"0x%Lx\",color=yellow3,style=filled]\n" id addr
  | NODE_JMP|NODE_DYN_JMP|NODE_OTHER|LEAF _ -> Printf.fprintf oc "%d[\"label\"=\"0x%Lx\"]\n" id addr

let rec print_node_dot oc tree =
  let addr,sons,id,t = tree.addr,tree.sons,tree.id,tree.t in
  pp_node oc id addr t;
  List.iter (fun s ->
      let id_son=s.id in
      match t with
      | NODE_JMP -> Printf.fprintf oc "%d -> %d[style=dashed]\n" id id_son
      | NODE_EIP|NODE_CALL|NODE_LIBCALL|NODE_RET|NODE_DYN_JMP|NODE_OTHER|LEAF _ ->
        Printf.fprintf oc "%d -> %d\n" id id_son
    ) sons;
  List.iter (fun x -> print_node_dot oc x) sons

let print_node_loop_dot oc tree =
  let callsite_id = Hashtbl.create 200 in
  let counter_callsite =
    let count = ref (-1) in
    fun () ->
      incr count;
      !count
  in
  let callsite_to_uniq_id callsite =
    try
      Hashtbl.find callsite_id callsite
    with
      Not_found -> let new_id = counter_callsite() in
      let _ = Hashtbl.add callsite_id callsite new_id in
      new_id
  in
  let create_id callsite addr = Printf.sprintf "%d%Ld" (callsite_to_uniq_id callsite) addr in
  let print_cluster callsite addr oc =
    Printf.fprintf oc
      "subgraph cluster_%d {label=\"Func %d\"; %s ; }\n"
      (callsite_to_uniq_id callsite)  (callsite_to_uniq_id callsite)
      (create_id callsite addr)
  in
  let rec print_node_loop_dot_rec oc tree =
    let addr,sons,(*id,*)t,is_sat,callsite = tree.addr,tree.sons,(*tree.id,*)tree.t,tree.is_sat,tree.callsite in
    let id = create_id callsite addr in
    print_cluster callsite addr oc;
    (match t, is_sat with
     | _ , SAT -> Printf.fprintf oc "%s[\"label\"=\"0x%Lx\",color=blue,style=filled]\n" id addr
     | _ , UNSAT -> Printf.fprintf oc "%s[\"label\"=\"0x%Lx\",color=black,style=filled]\n" id addr
     | NODE_EIP, _ -> Printf.fprintf oc "%s[\"label\"=\"0x%Lx\",color=hotpink,style=filled]\n" id addr
     | NODE_CALL, _ -> Printf.fprintf oc "%s[\"label\"=\"0x%Lx\",color=green,style=filled]\n" id addr
     | NODE_RET, _ -> Printf.fprintf oc "%s[\"label\"=\"0x%Lx\",color=chocolate2,style=filled]\n" id addr
     | NODE_LIBCALL, _ -> Printf.fprintf oc "%s[\"label\"=\"0x%Lx\",color=yellow3,style=filled]\n" id addr
     | (NODE_JMP | NODE_DYN_JMP | NODE_OTHER | LEAF _), NOT_TRY -> Printf.fprintf oc "%s[\"label\"=\"0x%Lx\"]\n" id addr);
    List.iter
      (fun s ->
         let addr_son,callsite_son = s.addr,s.callsite in
         let id_son = create_id callsite_son addr_son in
         match t with
         | NODE_JMP ->
           Printf.fprintf oc "%s -> %s[style=dashed]\n" id id_son
         | NODE_EIP | NODE_CALL | NODE_LIBCALL | NODE_RET | NODE_DYN_JMP
         | NODE_OTHER | LEAF _ ->
           Printf.fprintf oc "%s -> %s\n" id id_son
      ) sons;
    List.iter (fun x -> print_node_loop_dot_rec oc x) sons
  in print_node_loop_dot_rec oc tree


let export_tree oc tree print_begin print_end print_node =
  print_begin oc;
  print_node oc tree;
  print_end oc

let do_export filename tree print_begin print_end print_node =
  let oc = open_out filename in
  export_tree oc tree print_begin print_end print_node;
  close_out oc

let export ?(filename="export.dot") tree =
  do_export filename tree print_begin_dot print_end_dot print_node_dot

let loop_export ?(filename="export-loop.dot") tree =
  do_export filename tree print_begin_dot print_end_dot print_node_loop_dot

let add_id_to_tree tree =
  let counter =
    let count = ref (-1) in
    fun () -> incr count; !count
  in
  let rec explore tree =
    tree.id <- counter ();
    List.iter explore tree.sons
  in explore tree


let sat_child tree loc =
  let addr,_it = match loc with
    | Exploration_type.All_loc(addr) -> addr,(-1)
    | Exploration_type.One_loc(addr,it) -> addr,it
  in
  let rec explore tree =
    if tree.addr = addr then tree.is_sat <- SAT;
    List.iter explore tree.sons
  in explore tree


let unsat_child tree loc =
  let addr = match loc with | Exploration_type.All_loc addr | Exploration_type.One_loc (addr,_) -> addr in
  let rec explore tree =
    if tree.addr = addr && tree.is_sat = NOT_TRY then tree.is_sat <- UNSAT;
    List.iter explore tree.sons
  in explore tree


let rec contains tree l =
    match l with
    | [] -> true
    | hd::tl ->
        let (hd_addr,_,_) = hd in
        if ((Int64.compare hd_addr tree.addr)!=0) then false
        else
            if (List.length tree.sons)=0 then true
            else
                List.fold_left (fun b x -> (||) b (contains x tl)) false tree.sons   
(*  match l with
  | [] -> true
  | (hd_addr, _, _) :: tl ->
    hd_addr = tree.addr
    &&
    ( List.length tree.sons = 0
    ||
    List.exists (fun x -> contains x tl) tree.sons )*)


let traces_to_dot filenames =
  let rec loop acc = function
    | [] -> acc
    | fname :: fnames ->
      match trace_to_list_addr fname with
      | None -> acc
      | Some v -> loop ((v, fname) :: acc) fnames
  in
  match loop [] filenames with
  | (first_l, first_name) :: lists_addr ->
    let tree = list_to_tree first_l first_name in
    List.iter (fun (l, n) -> add_list_to_tree tree l n) lists_addr;
    add_id_to_tree tree;
    export tree;
    loop_export tree
  | [] -> assert false

(* Unused
let trace_to_dot filename =
  let list_addr = trace_to_list_addr filename in
  let list_addr = Utils.unsafe_get_opt list_addr in
  let tree = list_to_tree list_addr filename in
  let oc = open_out (Printf.sprintf "%s.dot" filename) in
  add_id_to_tree tree;
  export_tree oc tree print_begin_dot print_end_dot print_node_dot;
  close_out oc
*)
