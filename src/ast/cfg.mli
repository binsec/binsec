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

(** abstract type of graphs *)
type t

(** Vertices *)
module Vertex : sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool

  val create : Disasm_types.Instruction.t -> t
end

type vertex = Vertex.t

(** Edges *)
module Edge : sig
  type t
  val compare : t -> t -> int

  val src : t -> vertex
  val dst : t -> vertex

  val create : vertex -> vertex -> t
end

type edge = Edge.t

(** is this an implementation of directed graphs? *)
val is_directed : bool

(** {2 Graph constructors and destructors} *)

val create : ?size:int -> unit -> t
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
(** [add_vertex g v] adds the vertex [v] from the graph [g]. Do nothing if [v]
  * is already in [g]. *)

val remove_vertex : t -> vertex -> unit
(** [remove g v] removes the vertex [v] from the graph [g] (and all the edges
  * going from [v] in [g]). Do nothing if [v] is not in [g]. *)

val add_edge : t -> vertex -> vertex -> unit
(** [add_edge g v1 v2] adds an edge from the vertex [v1] to the vertex [v2] in
  * the graph [g]. Add also [v1] (resp. [v2]) in [g] if [v1] (resp. [v2]) is not
  * in [g]. Do nothing if this edge is already in [g]. *)

val add_edge_e : t -> edge -> unit
(** [add_edge_e g e] adds the edge [e] in the graph [g]. Add also [E.src e]
  * (resp. [E.dst e]) in [g] if [E.src e] (resp. [E.dst e]) is not in [g]. Do
  * nothing if [e] is already in [g]. *)

val remove_edge : t -> vertex -> vertex -> unit
(** [remove_edge g v1 v2] removes the edge going from [v1] to [v2] from the
  * graph [g]. Do nothing if this edge is not in [g].
  * @raise Invalid_argument if [v1] or [v2] are not in [g]. *)

val remove_edge_e : t -> edge -> unit
(** [remove_edge_e g e] removes the edge [e] from the graph [g]. Do nothing if
  * [e] is not in [g].
  * @raise Invalid_argument if [E.src e] or [E.dst e] are not in [g]. *)

(** {2 Size functions} *)

val is_empty : t -> bool
val nb_vertex : t -> int
val nb_edges : t -> int

(** Degree of a vertex *)

val out_degree : t -> vertex -> int

(** [out_degree g v] returns the out-degree of [v] in [g].
  * @raise Invalid_argument if [v] is not in [g]. *)

val in_degree : t -> vertex -> int
(** [in_degree g v] returns the in-degree of [v] in [g].
  * @raise Invalid_argument if [v] is not in [g]. *)

(** {2 Membership functions} *)

val mem_vertex : t -> vertex -> bool
val mem_edge : t -> vertex -> vertex -> bool
val mem_edge_e : t -> edge -> bool
val find_edge : t -> vertex -> vertex -> edge
val find_all_edges : t -> vertex -> vertex -> edge list

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
val iter_edges : (vertex -> vertex -> unit) -> t -> unit
val fold_vertex : (vertex -> 'a -> 'a) -> t  -> 'a -> 'a
val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a

(** map iterator on vertex *)

val map_vertex : (vertex -> vertex) -> t -> t

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

val output_graph :
  Pervasives.out_channel -> t -> Dba_types.Virtual_address.t list -> unit
