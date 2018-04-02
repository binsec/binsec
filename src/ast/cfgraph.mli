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

type state = private
  | Active
  | Inactive

module Node : Graph.Sig.COMPARABLE with type t = string * state

module Edge : Graph.Sig.ORDERED_TYPE_DFT with type t = string

module G : sig
  include Graph.Sig.P with type V.t = Node.t
                       and type V.label = Node.t
                       and type E.t = Node.t * Edge.t * Node.t
                       and type E.label = Edge.t

  val mk_active_node : string -> V.t
  val mk_inactive_node : string -> V.t
end

module Dot : sig
  val fprint_graph : Format.formatter -> G.t -> unit
  val output_graph : Pervasives.out_channel -> G.t -> unit
  val output_graph_to_file : string -> G.t -> unit
end
