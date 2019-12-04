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

(** Control Flow Graphs *)

type direction = Forward |  Backward

module type S =
sig

  type addr (* Addresses    *)
  type inst (* Instructions *)
  type symb (* Symbols      *)

  (** Abstract type of graphs *)
  type t

  (** Vertices *)
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

  (** Edges *)
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
         (** Information stored at each vertex. *)

         val direction : direction
         (** The direction of the analysis. *)

         val join : data -> data -> data
         (** Operation how to join data when paths meet. *)

         val equal : data -> data -> bool
         (** Predicate to determine the fixpoint. *)

         val analyze : E.t -> data -> data
         (** The actual analysis of one edge; provided the edge and the incoming
           * data, it needs to compute the outgoing data. *)
       end) :
  sig
    val analyze : (V.t -> X.data) -> t -> V.t -> X.data
    (** [analyze f g] computes the fixpoint on the given graph using the work
      * list algorithm. Beware that a misconstructed Fixpoint will not terminate!
      * [f] is used to create the initial analysis data. The function returned is
      * a map to see what data was computed for which node.
      *
      * Beware of applying function [analyze] partially, to arguments [f] and [g]
      * only. The result is a function that is to be used to query the result of
      * the analysis. *)
  end

  type trace = vertex Sequence.t

  val is_directed : bool
  (** is this an implementation of directed graphs? *)

  (** {2 Graph constructors and destructors} *)

  val create : int -> t
  (** Return an empty graph. Optionally, a size can be given, which should be on
    * the order of the expected number of vertices that will be in the graph (for
    * hash tables-based implementations). The graph grows as needed, so [size] is
    * just an initial guess. *)

  val clear: t -> unit
  (** Remove all vertices and edges from the given graph. *)

  val copy : t -> t
  (** [copy g] returns a copy of [g]. Vertices and edges (and eventually marks,
    * see module [Mark]) are duplicated. *)

  val add_vertex : t -> vertex -> unit
  (** [add_vertex g v] adds the vertex [v] in graph [g]. Do nothing if [v]
    * is already in [g]. *)

  val add_addr : t -> addr -> unit
  val add_inst : t -> addr -> inst -> unit
  val add_symb : t -> addr -> symb -> unit

  val remove_vertex : t -> vertex -> unit
  (** [remove g v] removes the vertex [v] from the graph [g] (and all the edges
    * going from [v] in [g]). Do nothing if [v] is not in [g]. *)

  val remove_addr : t -> addr -> unit
  val remove_inst : t -> addr -> unit
  val remove_symb : t -> addr -> unit

  val add_edge : t -> vertex -> vertex -> unit
  (** [add_edge g v1 v2] adds an edge from the vertex [v1] to the vertex [v2] in
    * the graph [g]. Add also [v1] (resp. [v2]) in [g] if [v1] (resp. [v2]) is not
    * in [g]. Do nothing if this edge is already in [g]. *)

  val add_edge_a : t -> addr -> addr -> unit

  val add_edge_e : t -> edge -> unit
  (** [add_edge_e g e] adds the edge [e] in the graph [g]. Add also [E.src e]
    * (resp. [E.dst e]) in [g] if [E.src e] (resp. [E.dst e]) is not in [g]. Do
    * nothing if [e] is already in [g]. *)

  val remove_edge : t -> vertex -> vertex -> unit
  (** [remove_edge g v1 v2] removes the edge going from [v1] to [v2] from the
    * graph [g]. Do nothing if this edge is not in [g].
    * @raise Invalid_argument if [v1] or [v2] are not in [g]. *)

  val remove_edge_a : t -> addr -> addr -> unit

  val remove_edge_e : t -> edge -> unit
  (** [remove_edge_e g e] removes the edge [e] from the graph [g]. Do nothing if
    * [e] is not in [g].
    * @raise Invalid_argument if [E.src e] or [E.dst e] are not in [g]. *)

  (** {2 Size functions} *)

  val is_empty  : t -> bool
  val nb_vertex : t -> int
  val nb_edges  : t -> int

  (** Degree of a vertex *)

  val out_degree : t -> vertex -> int

  (** [out_degree g v] returns the out-degree of [v] in [g].
    * @raise Invalid_argument if [v] is not in [g]. *)

  val in_degree : t -> vertex -> int
  (** [in_degree g v] returns the in-degree of [v] in [g].
    * @raise Invalid_argument if [v] is not in [g]. *)

  (** {2 Membership functions} *)

  val mem_vertex   : t -> vertex -> vertex option
  val mem_vertex_a : t -> addr -> vertex option

  val mem_edge   : t -> vertex -> vertex -> edge option
  val mem_edge_a : t -> addr -> addr -> edge option
  val mem_edge_e : t -> edge -> edge option

  (** {2 Successors and predecessors of a vertex} *)

  val succ : t -> vertex -> vertex list
  (** [succ g v] returns the successors of [v] in [g].
    * @raise Invalid_argument if [v] is not in [g]. *)

  val pred : t -> vertex -> vertex list
  (** [pred g v] returns the predecessors of [v] in [g].
    * @raise Invalid_argument if [v] is not in [g]. *)

  (** Labeled edges going from/to a vertex *)

  val succ_e : t -> vertex -> edge list
  (** [succ_e g v] returns the edges going from [v] in [g].
    * @raise Invalid_argument if [v] is not in [g]. *)

  val pred_e : t -> vertex -> edge list
  (** [pred_e g v] returns the edges going to [v] in [g].
    * @raise Invalid_argument if [v] is not in [g]. *)

  (** {2 Graph iterators} *)

  (** iter/fold on all vertices/edges of a graph *)

  val iter_vertex : (vertex -> unit) -> t -> unit
  val iter_edges  : (vertex -> vertex -> unit) -> t -> unit
  val fold_vertex : (vertex -> 'a -> 'a) -> t  -> 'a -> 'a
  val fold_edges  : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a

  (** iter/fold on all labeled edges of a graph *)

  val iter_edges_e : (edge -> unit) -> t -> unit
  val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a

  (** {2 Vertex iterators}

      Each iterator [iterator f v g] iters [f] to the successors/predecessors of
      [v] in the graph [g] and raises [Invalid_argument] if [v] is not in [g]. *)

  (** iter/fold on all successors/predecessors of a vertex. *)

  val iter_succ : (vertex -> unit) -> t -> vertex -> unit
  val iter_pred : (vertex -> unit) -> t -> vertex -> unit
  val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
  val fold_pred : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a

  (** iter/fold on all edges going from/to a vertex. *)

  val iter_succ_e : (edge -> unit) -> t -> vertex -> unit
  val fold_succ_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
  val iter_pred_e : (edge -> unit) -> t -> vertex -> unit
  val fold_pred_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
end

module Make
    (A: Sigs.HASHABLE)
    (I: Hashtbl.HashedType)
    (S: Hashtbl.HashedType) :
  S with type addr = A.t
     and type inst = I.t
     and type symb = S.t
