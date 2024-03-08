(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
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

module Key = struct
  type t = Virtual_address.t

  let compare a1 a2 = compare a1 a2
  let hash (a : t) = (a :> int)
  let equal a1 a2 = a1 = a2
end

module Value = struct
  type t = Instruction.t

  let hash (i : t) = (i.Instruction.address :> int)
  let equal i1 i2 = i1 = i2
end

module type S = sig
  include Cfg.S

  val ordered_iter_vertex :
    compare:(vertex -> vertex -> int) -> (vertex -> unit) -> t -> unit

  val iter_vertex_by_address : (vertex -> unit) -> t -> unit

  val output_graph :
    out_channel -> t -> entry:vertex -> Virtual_address.t list -> unit

  val dump : filename:string -> t -> unit
end

module Make (H : Hashtbl.HashedType) = struct
  module G = Cfg.Make (Key) (Value) (H)
  include G

  type block = {
    leader : V.t;
    block : V.t list;
    succs : V.t list;
    preds : V.t list;
  }

  module D = Graph.Imperative.Digraph.ConcreteBidirectional (struct
    type t = block

    let compare b1 b2 = V.compare b1.leader b2.leader
    let hash b = V.hash b.leader
    let equal b1 b2 = V.equal b1.leader b2.leader
  end)

  module L = Graph.Leaderlist.Make (G)
  module H = Hashtbl.Make (V)

  let get_pred t v = match pred t v with [ v ] -> Some v | _ -> None
  let get_succ t v = match succ t v with [ v ] -> Some v | _ -> None

  let rec compare_preds_succs g v pred succ =
    match (pred, succ) with
    | None, None -> assert false
    | Some _, None -> -1
    | None, Some _ -> 1
    | Some p, Some s ->
        if V.equal v p then -1
        else if V.equal v s then 1
        else compare_preds_succs g v (get_pred g p) (get_succ g s)

  let compare_vertex g v1 v2 =
    if G.V.equal v1 v2 then 0
    else compare_preds_succs g v1 (get_pred g v2) (get_succ g v2)

  let rec diff lst1 lst2 acc =
    match (lst1, lst2) with
    | ls, [] -> List.rev_append ls acc
    | [], _ -> acc
    | a1 :: ls1, a2 :: ls2 ->
        if G.V.compare a1 a2 < 0 then diff ls1 lst2 (a1 :: acc)
        else if G.V.compare a1 a2 > 0 then diff lst1 ls2 acc
        else diff ls1 ls2 acc

  let diff lst1 lst2 = List.rev (diff lst1 lst2 [])

  let build_block g block =
    let succs, preds =
      List.fold_left
        (fun (succs, preds) v ->
          ( List.fold_left (fun l e -> e :: l) succs (succ g v),
            List.fold_left (fun l e -> e :: l) preds (pred g v) ))
        ([], []) block
    in
    let block = List.sort_uniq V.compare block in
    let succs = diff (List.sort_uniq V.compare succs) block in
    let preds = diff (List.sort_uniq V.compare preds) block in
    let block = List.sort_uniq (compare_vertex g) block in
    { leader = List.hd block; block; succs; preds }

  let build_block_graph cfg entry =
    let blocks = List.map (build_block cfg) (L.leader_lists cfg entry) in
    let htbl = H.create 17 in
    List.iter (fun b -> List.iter (fun v -> H.add htbl v b) b.block) blocks;
    let t = D.create () in
    List.iter
      (fun block ->
        let vertex = D.V.create block in
        D.add_vertex t vertex;
        List.iter
          (fun succ -> D.add_edge t vertex (H.find htbl succ))
          block.succs;
        List.iter
          (fun pred -> D.add_edge t (H.find htbl pred) vertex)
          block.preds)
      blocks;
    t

  let html_block callees block =
    let open Format in
    let align = "align=\"left\"" in
    let border = "border=\"1\"" in
    let open Colors in
    let color1 = asprintf "bgcolor=\"%a\"" pp FlatUI.greensea in
    let color2 = asprintf "bgcolor=\"%a\"" pp FlatUI.silver in
    let pp_mnemonic ppf vert =
      match V.inst vert with
      | None -> ()
      | Some inst ->
          let a = Instruction.address inst in
          let m = Instruction.mnemonic inst in
          if List.mem a callees then
            fprintf ppf "<font color=\"%a\">%a</font>" pp FlatUI.alizarin
              Mnemonic.pp m
          else Mnemonic.pp ppf m
    in
    block.block
    |> List.map (fun vert ->
           asprintf "<tr><td %s %s>0x%x</td><td %s %s %s>%a</td></tr>" border
             color1
             (V.addr vert :> int)
             border color2 align pp_mnemonic vert)
    |> String.concat "\n"
    |> sprintf "<table border=\"0\" cellspacing=\"0\">\n%s\n</table>"

  let output_graph c g ~entry ca =
    let g = build_block_graph g entry in
    let module Dot = struct
      include Graph.Graphviz.Dot (struct
        include D

        let graph_attributes _ = []
        let default_vertex_attributes _ = [ `Shape `Plaintext ]
        let vertex_name b = Printf.sprintf "%i" (Hashtbl.hash b)
        let vertex_attributes b = [ `HtmlLabel (html_block ca b) ]
        let get_subgraph _ = None
        let default_edge_attributes _ = []
        let edge_attributes _ = [ `Minlen 1 ]
      end)
    end in
    Dot.output_graph c g

  let dump_oc oc g =
    let module Dot = Graph.Graphviz.Dot (struct
      include G

      let graph_attributes _ = []
      let default_vertex_attributes _ = []

      let vertex_name v =
        Format.asprintf "\"%a %a\"" Virtual_address.pp (V.addr v)
          (fun ppf v ->
            let open Format in
            match V.inst v with
            | None -> pp_print_string ppf ""
            | Some i -> fprintf ppf "%a" Mnemonic.pp (Instruction.mnemonic i))
          v

      let vertex_attributes _ = []
      let get_subgraph _ = None
      let default_edge_attributes _ = []
      let edge_attributes _ = []
    end) in
    Dot.output_graph oc g

  let dump ~filename g =
    let oc = open_out_bin filename in
    dump_oc oc g;
    close_out oc

  let ordered_iter_vertex ~compare (f : vertex -> unit) g =
    (* It is way better to use arrays (and even lists) than trees *)
    let dummy_v = G.V.of_addr (Virtual_address.create 0) in
    let a = Array.make (G.nb_vertex g) dummy_v in
    let i = ref 0 in
    iter_vertex
      (fun v ->
        a.(!i) <- v;
        incr i)
      g;
    Array.sort compare a;
    Array.iter f a

  let iter_vertex_by_address = ordered_iter_vertex ~compare:Key.compare
end

module S = struct
  type t = string

  let hash s = Hashtbl.hash s
  let equal s1 s2 = s1 = s2
end

module F = Make (S)
include F
