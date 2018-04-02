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

open Disasm_types

module H = Hashtbl.Make
    (struct
      type t = int
      let equal (x: int) (y: int) = x = y
      let hash (x: int) = x
    end)

module G =  Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
    (struct
      type t = Instruction.t
      let compare i1 i2 = compare i1 i2
      let hash i = (i.Instruction.address :> int)
      let equal i1 i2 = i1 = i2
    end)
    (struct
      type t = int
      let compare (x: int) (y: int) = compare x y
      let default = 0
    end)

include G

module Vertex =
struct
  include V
end

module Edge =
struct
  include E
  let create v1 v2 = create v1 0 v2
end

let are_adjacent i1 i2 =
  let open Instruction in
  (i1.address :> int) + (i1.size :> int) = (i2.address :> int) ||
  (i2.address :> int) + (i2.size :> int) = (i1.address :> int)

type block = {
  id : int;
  address : Dba_types.Virtual_address.t;
  block : Instruction.t list;
  succs : int list;
  preds : int list;
}

module D =  Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
    (struct
      type t = block
      let compare b1 b2 = compare b1.id b2.id
      let hash b = b.id
      let equal b1 b2 = b1 = b2
    end)
    (struct
      type t = int
      let compare (x: int) (y: int) = compare x y
      let default = 0
    end)

let rec mark_succs cfg htbl mark inst =
  H.replace htbl (inst.Instruction.address :> int) mark;
  match succ cfg inst with
  | [succ] ->
    if are_adjacent inst succ
    then
      (match pred cfg succ with
       | [_] -> mark_succs cfg htbl mark succ
       | _ -> ())
  | _ -> ()

let rec mark_preds cfg htbl mark inst =
  H.replace htbl (inst.Instruction.address :> int) mark;
  match pred cfg inst with
  | [pred] ->
    if are_adjacent inst pred
    then
      (match succ cfg pred with
       | [_] -> mark_preds cfg htbl mark pred
       | _ -> ())
  | _ -> ()

let mark_cfg cfg =
  let htbl = H.create 257 in
  let mark = ref 0 in
  iter_vertex
    (fun v ->
       if not (H.mem htbl (v.Instruction.address :> int))
       then
         (mark_succs cfg htbl !mark v;
          mark_preds cfg htbl !mark v;
          incr mark))
    cfg;
  htbl

let build_block cfg htbl (id, insts) =
  let block =
    List.sort
      (fun i1 i2 -> compare
          (i1.Instruction.address :> int)
          (i2.Instruction.address :> int))
      insts
  in
  let address = (List.hd block).Instruction.address in
  let succs, preds =
    let aux f =
      List.fold_left
        (fun acc inst ->
           f cfg inst
           |> List.map (fun i -> H.find htbl (i.Instruction.address :> int))
           |> List.filter (fun i -> i <> id)
           |> List.rev_append acc)
        [] block
    in
    aux succ, aux pred
  in
  { id; address; block; succs; preds }

let build_blocks cfg htbl =
  let tmp = H.create 257 in
  let bks = H.create 257 in
  iter_vertex
    (fun inst ->
       let mark = H.find htbl (inst.Instruction.address :> int) in
       let lst = try H.find tmp mark with Not_found -> [] in
       H.replace tmp mark (inst :: lst))
    cfg;
  H.fold (fun mark insts acc -> (mark, insts) :: acc) tmp []
  |> List.map (build_block cfg htbl)
  |> List.iter (fun block -> H.add bks block.id block);
  bks

let build_block_graph cfg =
  let htbl = mark_cfg cfg in
  let blocks = build_blocks cfg htbl in
  let t = D.create () in
  H.iter
    (fun _ block ->
       List.iter
         (fun succ ->
            D.add_edge t (D.V.create block) (D.V.create (H.find blocks succ)))
         block.succs)
    blocks;
  t

let html_block callees block =
  let open Format in
  let align  = "align=\"left\"" in
  let border = "border=\"1\"" in
  let open Colors in
  let color1 = asprintf "bgcolor=\"%a\"" pp FlatUI.greensea in
  let color2 = asprintf "bgcolor=\"%a\"" pp FlatUI.silver in
  let pp_mnemonic ppf inst =
    let a = inst.Instruction.address in
    let m = inst.Instruction.mnemonic in
    if List.mem a callees then
      fprintf ppf "<font color=\"%a\">%a</font>" pp FlatUI.alizarin Mnemonic.pp m
    else Mnemonic.pp ppf m
  in
  block.block
  |> List.map
    (fun inst ->
       asprintf "<tr><td %s %s>0x%x</td><td %s %s %s>%a</td></tr>"
         border color1 (inst.Instruction.address :> int)
         border color2 align pp_mnemonic inst)
  |> String.concat "\n"
  |> sprintf "<table border=\"0\" cellspacing=\"0\">\n%s\n</table>"


let output_graph c g ca =
  let g = build_block_graph g in
  let module Dot =
  struct
  include Graph.Graphviz.Dot
      (struct
        include D
        let graph_attributes _ = []
        let default_vertex_attributes _ = [`Shape `Plaintext]
        let vertex_name b = Printf.sprintf "%i" (Hashtbl.hash b)
        let vertex_attributes b = [`HtmlLabel (html_block ca b)]
        let get_subgraph _ = None
        let default_edge_attributes _ = []
        let edge_attributes _ = [`Minlen 1]
      end)
  end
  in Dot.output_graph c g
