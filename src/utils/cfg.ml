(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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



type direction = Graph.Fixpoint.direction = Forward |  Backward

module type S =
sig

  type addr
  type inst
  type symb

  type t

  module V : sig
    type t

    val compare : t -> t -> int
    val hash : t -> int
    val equal : t -> t -> bool

    val of_addr : addr -> t
    val of_inst : addr -> inst -> t
    val of_symb : addr -> symb -> t

    val addr : t -> addr
    val inst : t -> inst option
    val symb : t -> symb option
  end
  type vertex = V.t

  module E : sig
    type t
    type label

    val compare : t -> t -> int
    val label : t -> label
    val src : t -> vertex
    val dst : t -> vertex

    val create : vertex -> label -> vertex -> t
  end
  type edge = E.t

  module Fixpoint
      (X : sig
         type data
         val direction : direction
         val join : data -> data -> data
         val equal : data -> data -> bool
         val analyze : E.t -> data -> data
       end) :
  sig
    val analyze : (V.t -> X.data) -> t -> V.t -> X.data
  end

  type trace = vertex Sequence.t

  val is_directed : bool

  val create : int -> t
  val clear: t -> unit
  val copy: t -> t

  val add_vertex : t -> vertex -> unit
  val add_addr : t -> addr -> unit
  val add_inst : t -> addr -> inst -> unit
  val add_symb : t -> addr -> symb -> unit

  val remove_vertex : t -> vertex -> unit
  val remove_addr : t -> addr -> unit
  val remove_inst : t -> addr -> unit
  val remove_symb : t -> addr -> unit

  val add_edge : t -> vertex -> vertex -> unit
  val add_edge_a : t -> addr -> addr -> unit
  val add_edge_e : t -> edge -> unit

  val remove_edge : t -> vertex -> vertex -> unit
  val remove_edge_a : t -> addr -> addr -> unit
  val remove_edge_e : t -> edge -> unit

  val is_empty : t -> bool
  val nb_vertex : t -> int
  val nb_edges : t -> int

  val out_degree : t -> vertex -> int
  val in_degree : t -> vertex -> int

  val mem_vertex : t -> vertex -> vertex option
  val mem_vertex_a : t -> addr -> vertex option
  val mem_edge : t -> vertex -> vertex -> edge option
  val mem_edge_a : t -> addr -> addr -> edge option
  val mem_edge_e : t -> edge -> edge option

  val succ : t -> vertex -> vertex list
  val pred : t -> vertex -> vertex list
  val succ_e : t -> vertex -> edge list
  val pred_e : t -> vertex -> edge list

  val iter_vertex : (vertex -> unit) -> t -> unit
  val iter_edges : (vertex -> vertex -> unit) -> t -> unit
  val fold_vertex : (vertex -> 'a -> 'a) -> t  -> 'a -> 'a
  val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a

  val iter_edges_e : (edge -> unit) -> t -> unit
  val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a

  val iter_succ : (vertex -> unit) -> t -> vertex -> unit
  val iter_pred : (vertex -> unit) -> t -> vertex -> unit
  val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
  val fold_pred : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a

  val iter_succ_e : (edge -> unit) -> t -> vertex -> unit
  val fold_succ_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
  val iter_pred_e : (edge -> unit) -> t -> vertex -> unit
  val fold_pred_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
end

module Make (A: Graph.Sig.COMPARABLE) (I: Graph.Sig.HASHABLE) (S: Graph.Sig.HASHABLE) :
  S with type addr = A.t
     and type inst = I.t
     and type symb = S.t =
struct
  module WA = Weak.Make(A)
  module WI = Weak.Make(I)
  module WS = Weak.Make(S)

  let weak_addr = WA.create 17
  let weak_inst = WI.create 17
  let weak_symb = WS.create 17

  module Elt :
  sig
    type t = private {
      addr : A.t;
      mutable inst : I.t option;
      mutable symb : S.t option;
    }

    val compare : t -> t -> int
    val hash : t -> int
    val equal : t -> t -> bool

    val of_addr : A.t -> t
    val of_inst : A.t -> I.t -> t
    val of_symb : A.t -> S.t -> t

    val update_inst : t -> I.t option -> unit
    val update_symb : t -> S.t option -> unit
  end = struct
    type t = {
      addr : A.t;
      mutable inst : I.t option;
      mutable symb : S.t option;
    }

    let compare t1 t2 = A.compare t1.addr t2.addr
    let hash t = A.hash t.addr
    let equal t1 t2 = A.equal t1.addr t2.addr

    let of_addr addr =
      let addr = WA.merge weak_addr addr in
      let inst = None in
      let symb = None in
      { addr; inst; symb }

    let of_inst addr inst =
      let addr = WA.merge weak_addr addr in
      let inst = Some (WI.merge weak_inst inst) in
      let symb = None in
      { addr; inst; symb }

    let of_symb addr symb =
      let addr = WA.merge weak_addr addr in
      let inst = None in
      let symb = Some (WS.merge weak_symb symb) in
      { addr; inst; symb }

    let update_inst e inst = e.inst <- inst
    let update_symb e symb = e.symb <- symb
  end

  module H = Hashtbl.Make(A)
  module G = Graph.Imperative.Digraph.ConcreteBidirectional(Elt)

  type addr = A.t
  type inst = I.t
  type symb = S.t

  type t = { graph : G.t; htbl : Elt.t H.t }

  module V =
  struct
    include G.V

    let of_addr addr = Elt.of_addr addr
    let of_inst addr inst = Elt.of_inst addr inst
    let of_symb addr symb = Elt.of_symb addr symb

    let addr v = v.Elt.addr
    let inst v = v.Elt.inst
    let symb v = v.Elt.symb
  end

  type vertex = V.t

  module E =
  struct
    include G.E
    let create v1 l v2 = create v1 l v2
  end

  type edge = E.t

  module Fixpoint
      (X : sig
         type data
         val direction : direction
         val join : data -> data -> data
         val equal : data -> data -> bool
         val analyze : E.t -> data -> data
       end) =
  struct
    include Graph.Fixpoint.Make(G)
        (struct
          include X
          type g = G.t
          type vertex = V.t
          type edge = E.t
        end)
    let analyze f t = analyze f t.graph
  end

  type trace = vertex Sequence.t

  let is_directed = true

  let create size =
    let graph = G.create ~size () in
    let htbl = H.create size in
    { graph; htbl }

  let clear t =
    G.clear t.graph;
    H.clear t.htbl

  let copy t =
    let graph = G.copy t.graph in
    let htbl = H.copy t.htbl in
    { graph; htbl }

  let find t v =
    try H.find t v.Elt.addr
    with Not_found -> H.add t v.Elt.addr v; v

  let merge t v =
    let e = find t v in
    (match v.Elt.inst with
     | Some _ -> Elt.update_inst e v.Elt.inst
     | None -> ());
    (match v.Elt.symb with
     | Some _ -> Elt.update_symb e v.Elt.symb
     | None -> ());
    e

  let add_vertex t v = G.add_vertex t.graph (merge t.htbl v)

  let add_addr t addr = add_vertex t (Elt.of_addr addr)
  let add_inst t addr inst = add_vertex t (Elt.of_inst addr inst)
  let add_symb t addr symb = add_vertex t (Elt.of_symb addr symb)

  let remove_vertex t v = G.remove_vertex t.graph v; H.remove t.htbl v.Elt.addr

  let remove_addr t addr = remove_vertex t (Elt.of_addr addr)
  let remove_inst t addr = Elt.update_inst (find t.htbl (Elt.of_addr addr)) None
  let remove_symb t addr = Elt.update_symb (find t.htbl (Elt.of_addr addr)) None

  let add_edge t v1 v2 = G.add_edge t.graph (merge t.htbl v1) (merge t.htbl v2)

  let add_edge_a t a1 a2 = add_edge t (Elt.of_addr a1) (Elt.of_addr a2)
  let add_edge_e t e = add_edge t (E.src e) (E.dst e)

  let remove_edge t v1 v2 = G.remove_edge t.graph v1 v2
  let remove_edge_a t a1 a2 = G.remove_edge t.graph (Elt.of_addr a1) (Elt.of_addr a2)
  let remove_edge_e t e = G.remove_edge_e t.graph e

  let is_empty t = G.is_empty t.graph
  let nb_vertex t = G.nb_vertex t.graph
  let nb_edges t = G.nb_edges t.graph
  let out_degree t v = G.out_degree t.graph v
  let in_degree t v = G.in_degree t.graph v

  let mem_vertex t v =
    if G.mem_vertex t.graph v
    then Some (H.find t.htbl v.Elt.addr)
    else None

  let mem_vertex_a t a = mem_vertex t (Elt.of_addr a)

  let mem_edge t v1 v2 =
    match G.find_edge t.graph v1 v2 with
    | e ->
       let label = E.label e in
       Some (E.create (H.find t.htbl v1.Elt.addr) label
               (H.find t.htbl v2.Elt.addr))
    | exception Not_found -> None

  let mem_edge_a t a1 a2 = mem_edge t (Elt.of_addr a1) (Elt.of_addr a2)
  let mem_edge_e t e = mem_edge t (E.src e) (E.dst e)

  let succ t v = G.succ t.graph v
  let pred t v = G.pred t.graph v
  let succ_e t v = G.succ_e t.graph v
  let pred_e t v = G.succ_e t.graph v

  let iter_vertex f t = G.iter_vertex f t.graph
  let iter_edges f t = G.iter_edges f t.graph
  let iter_edges_e f t = G.iter_edges_e f t.graph
  let fold_vertex f t acc = G.fold_vertex f t.graph acc
  let fold_edges f t acc = G.fold_edges f t.graph acc
  let fold_edges_e f t acc = G.fold_edges_e f t.graph acc

  let iter_succ f t v = G.iter_succ f t.graph v
  let iter_pred f t v = G.iter_pred f t.graph v
  let fold_succ f t v acc = G.fold_succ f t.graph v acc
  let fold_pred f t v acc = G.fold_pred f t.graph v acc

  let iter_succ_e f t v = G.iter_succ_e f t.graph v
  let iter_pred_e f t v = G.iter_pred_e f t.graph v
  let fold_succ_e f t v acc = G.fold_succ_e f t.graph v acc
  let fold_pred_e f t v acc = G.fold_pred_e f t.graph v acc
end
