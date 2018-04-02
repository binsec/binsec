(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
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
(*                                                                        *)
(**************************************************************************)

open Dba


type dbastmt = {
  mutable instr : instruction;
  mutable preds : Dba_types.Caddress.Set.t;
  mutable succs : Dba_types.Caddress.Set.t;
  mutable closed_succs : bool;
  mutable binstr : Disasm_types.GenericInstruction.t option ;
  mutable heuristical_succs : Dba_types.Caddress.Set.t;
  (* Experimental field (for Call immediate successor) *)
}

type t = dbastmt Dba_types.Caddress.Map.t


let make inst_map djumps_map =
  (* FIXME : set reference *)
  let open Dba_types in
  let opt_addr = ref Caddress.Set.empty in
  let get_successors inst addr =
    match inst with
    | Dba.IkSJump (JInner off, tgs) ->
      (match tgs with
       | Some (Dba.Call ad) -> opt_addr := Caddress.Set.singleton ad
       | _ -> ());
      let addr = Caddress.reid addr off in
      Caddress.Set.singleton addr, true
    | Dba.IkSJump (JOuter a, tgs) ->
      (match tgs with
       | Some (Dba.Call ad) -> opt_addr := Caddress.Set.singleton ad
       | _ -> ());
      Caddress.Set.singleton a, true
    | Dba.IkDJump _ ->
      begin
        try Caddress.Map.find addr djumps_map, false
        with Not_found -> Caddress.Set.empty, false
      end
    | Dba.IkIf (_, JInner off1, off2) ->
      let a1 = Caddress.reid addr off1
      and a2 = Caddress.reid addr off2 in
      Caddress.Set.add a1 (Caddress.Set.singleton a2), true
    | Dba.IkIf (_, JOuter a, off2) ->
      Caddress.Set.add a (Caddress.Set.singleton (Caddress.reid addr off2)), true
    | Dba.IkStop _ -> Caddress.Set.empty, true
    | Dba.IkAssign (_, _, off)
    | Dba.IkAssert (_, off)
    | Dba.IkAssume (_, off)
    | Dba.IkNondetAssume (_, _, off)
    | Dba.IkNondet (_, _, off)
    | Dba.IkUndef (_, off)
    | Dba.IkMalloc (_, _, off)
    | Dba.IkFree (_, off)
    | Dba.IkPrint (_, off) -> Caddress.Set.singleton (Caddress.reid addr off), true
  in
  let build_aux current_address (inst, opc) ast =
    let successors, closed_successors =
      get_successors inst current_address in
    let ast =
      Caddress.Set.fold
        (fun successor_addr ast ->
           if Caddress.Map.mem successor_addr ast then
             let item  = Caddress.Map.find successor_addr ast in
             item.preds <- Caddress.Set.add current_address item.preds;
             Caddress.Map.add successor_addr item ast
           else
           if Caddress.Map.mem successor_addr inst_map then
             let instr, opc = Caddress.Map.find successor_addr inst_map in
             let item = {
               instr;
               preds=Caddress.Set.singleton current_address ;
               succs = Caddress.Set.empty; closed_succs = false;
               binstr = opc; heuristical_succs = Caddress.Set.empty} in
             Caddress.Map.add successor_addr item ast
           else ast) successors ast
    in
    let predecessors =
      try
        let item = Caddress.Map.find current_address ast in
        item.preds
      with Not_found -> Caddress.Set.empty
    in
    let item = {instr=inst ; preds=predecessors ; succs = successors;
                closed_succs = closed_successors; binstr = opc;
                heuristical_succs = !opt_addr} in
    Caddress.Map.add current_address item ast
  in Caddress.Map.fold build_aux inst_map Caddress.Map.empty


(* FIXME : unused *)
let _display_ast ast =
  let pp_set fmt addrset =
    Dba_types.Caddress.Set.iter
      (fun caddress ->
         Format.fprintf fmt "%a,@ " Dba_printer.Ascii.pp_code_address caddress)
      addrset
  in
  let f addr stmt =
    Logger.info
      "@[<v 2>%a %a:@ \
       preds: @[<hov 0>%a@]@ \
       succs: @[<hov 0>%a@]@ \
       closed: %s@ \
       binstr: TODO@ \
       @]"
      Dba_printer.Ascii.pp_code_address addr
      Dba_printer.Ascii.pp_instruction stmt.instr
      pp_set stmt.preds
      pp_set stmt.succs
      (string_of_bool stmt.closed_succs)
    ;
  in Dba_types.Caddress.Map.iter f ast



let build_sequences_map ast l0 dba_display =
  let ast = ref ast in

  let rec get_successors addr =
    let open Dba_types in
    try
      let item = Caddress.Map.find addr !ast in
      let instr = item.instr in
      ast := Dba_types.Caddress.Map.remove addr !ast;
      if dba_display then item.succs
      else
        match instr with
        | Dba.IkSJump (JOuter a, _) -> Dba_types.Caddress.Set.singleton a
        | Dba.IkIf (_, JInner off1, off2) ->
          let a1 = Caddress.reid addr off1
          and a2 = Caddress.reid addr off2 in
          Dba_types.Caddress.Set.union (get_successors a1) (get_successors a2)
        | Dba.IkIf (_, JOuter a, off2) ->
          Dba_types.Caddress.Set.add a (get_successors (Caddress.reid addr off2))
        | Dba.IkDJump (_, _) -> item.succs
        | Dba.IkStop _ -> Dba_types.Caddress.Set.empty
        | Dba.IkSJump (JInner off, _)
        | Dba.IkAssign (_, _, off)
        | Dba.IkAssert (_, off)
        | Dba.IkAssume (_, off)
        | Dba.IkNondetAssume (_, _, off)
        | Dba.IkNondet (_, _, off)
        | Dba.IkUndef (_, off)
        | Dba.IkMalloc (_, _, off)
        | Dba.IkFree (_, off)
        | Dba.IkPrint (_, off) -> get_successors (Caddress.reid addr off )
    with Not_found -> Dba_types.Caddress.Set.empty
  in

  let rec update_sequences_map addr item sequence sequences_map =
    let successors = get_successors addr in
    let sequence = sequence @ [addr, item, successors] in
    match Dba_types.Caddress.Set.cardinal successors with
    | 0 ->
      let init_sequence_address, _, _ = List.hd sequence in
      Dba_types.Caddress.Map.add init_sequence_address sequence sequences_map
    | 1 ->
      begin
        try
          let addr_suiv = Dba_types.Caddress.Set.choose successors in
          let item = Dba_types.Caddress.Map.find addr_suiv !ast in
          if Dba_types.Caddress.Set.cardinal item.preds = 1 then
            update_sequences_map addr_suiv item sequence sequences_map
          else
            let sequences_map =
              let init_sequence_address, _, _ = List.hd sequence in
              Dba_types.Caddress.Map.add init_sequence_address sequence sequences_map in
            update_sequences_map addr_suiv item [] sequences_map
        with Not_found ->
          let init_sequence_address, _, _ = List.hd sequence in
          Dba_types.Caddress.Map.add init_sequence_address sequence sequences_map
      end
    | _ ->
      let sequences_map =
        let init_sequence_address, _, _ = List.hd sequence in
        Dba_types.Caddress.Map.add init_sequence_address sequence sequences_map in
      Dba_types.Caddress.Set.fold (fun addr sequences_map ->
          try
            let item = Dba_types.Caddress.Map.find addr !ast in
            update_sequences_map addr item [] sequences_map
          with Not_found ->
            let init_sequence_address, _, _ = List.hd sequence in
            Dba_types.Caddress.Map.add init_sequence_address sequence sequences_map
        ) successors sequences_map
  in

  let rec build_sequences_map_with_new_entry_point sequences_map l0 =
    try
      let (addr, item) =
        match l0 with
        | Some a -> (a, Dba_types.Caddress.Map.find a !ast)
        | None -> Dba_types.Caddress.Map.min_binding !ast
      in
      let sequences_map = update_sequences_map addr item [] sequences_map in
      build_sequences_map_with_new_entry_point sequences_map l0
    with
      Not_found -> sequences_map
  in
  build_sequences_map_with_new_entry_point Dba_types.Caddress.Map.empty l0


let pp_opt_opcode ppf = function
  | None -> Format.fprintf ppf ""
  | Some binstr -> Disasm_types.GenericInstruction.pp_opcode ppf binstr

let cfg_of_sequences_map sequences_map active_addresses conditions dba_display =
  let sequence_label =
    let b = Buffer.create 256 in
    fun sequence ->
      Buffer.clear b;
      let rec aux = function
        | [] -> Buffer.contents b
        | (addr, item, _) :: tl ->
          if dba_display then
            let instr = String.escaped
                (Format.asprintf "%a\\l"
                   Dba_types.Statement.pp
                   (Dba_types.Statement.create addr item.instr))
            in
            Buffer.add_string b instr;
            aux tl
          else
            let addr =
              String.sub
                (Format.asprintf "%a" Dba_printer.Ascii.pp_code_address addr) 1 10
            in
            let txt =
              Format.asprintf "@[<hov 0>%s:@ %a\\l@]"
                addr pp_opt_opcode item.binstr
            in Buffer.add_string b txt;
            aux tl
      in aux sequence
  in

  let rec edge_label addr conditions acc =
    match conditions with
    | [] ->
      if String.length acc > 10000 then
        (String.sub acc 0 10000) ^ "..."
      else acc
    | (_, v) :: tl ->
      let label = Region_bitvector.to_string v in
      let label =
        if String.length label > 1000 then
          String.sub label 0 1000 ^ "...\\l"
        else label ^ "\\l"
      in
      edge_label addr tl (acc ^ label)
  in

  let f addr sequence cfg =
    let mk_node node_name =
      let mk =
        if Dba_types.Caddress.Set.mem addr active_addresses then Cfgraph.G.mk_active_node
        else Cfgraph.G.mk_inactive_node
      in mk node_name
    in
    let node1 = mk_node (sequence_label sequence) in
    let cfg = Cfgraph.G.add_vertex cfg node1 in
    assert (List.length sequence >= 1);
    let (addr, _, successors) = List_utils.last sequence in
    Dba_types.Caddress.Set.fold (fun addr_suiv cfg ->
        try
          let sequence = Dba_types.Caddress.Map.find addr_suiv sequences_map in
          let node2 = mk_node (sequence_label sequence) in
          let label =
            try edge_label addr (Dba_types.Caddress.Map.find addr_suiv conditions) ""
            with Not_found -> ""
          in
          let edge = Cfgraph.G.E.create node1 label node2 in
          Cfgraph.G.add_edge_e cfg edge
        with Not_found -> cfg
      ) successors cfg
  in
  Dba_types.Caddress.Map.fold f sequences_map Cfgraph.G.empty


let cfg_opcode_of_ast ast l0 active_addresses conditions =
  let sequences_map = build_sequences_map ast l0 false in
  cfg_of_sequences_map sequences_map active_addresses conditions false


let cfg_dba_of_ast ast l0 active_addresses conditions =
  let sequences_map = build_sequences_map ast l0 true in
  cfg_of_sequences_map sequences_map active_addresses conditions true
