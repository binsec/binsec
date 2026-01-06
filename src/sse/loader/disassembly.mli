(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

module Revision : sig
  include Ir.GRAPH
  include Ir.INSTRUMENT with type t := t and type vertex := vertex

  val is_new_vertex : t -> vertex -> bool
  val iter_new_vertex : (vertex -> unit) -> t -> unit
end

type stage = Early | Late

type 'a hook =
  | Fetch : (Virtual_address.t -> Ir.Graph.t option) hook
  | Decode : (Virtual_address.t -> int Reader.t -> Ir.Graph.t option) hook
  | Disasm : (Instruction.t -> Ir.Graph.t option) hook
  | Rewrite : (Ir.Graph.t -> unit) hook

(** Information to be used by optimization. *)
type 'a knowledge =
  | May_read : Dba_types.Var.Set.t option knowledge
      (** The (overapproximed) set of all variables a builtin may access.
          Variables that are not in this set may have an undefined value.
          [None] means any variable may be read. *)
  | Must_write : Dba_types.Var.Set.t knowledge
      (** The (underapproximed) set of all variables a builtin must overwrite.
          Previous values of these variables are deemed no longer reachable. *)

module Callback : sig
  type t

  val empty : t

  val register_hook :
    t -> Virtual_address.t Interval.t -> ?stage:stage -> 'a hook -> 'a -> t

  val register_instrumentation : t -> (Revision.t -> unit) -> t
  val register_knowledge : t -> 'a knowledge -> (Ir.builtin -> 'a option) -> t
end

type t

val create :
  Callback.t ->
  decoder:(int Reader.t -> Virtual_address.t -> Instruction.t) ->
  Virtual_address.t ->
  Virtual_address.t Reader.t ->
  Z.t ->
  t

val create_small :
  Callback.t ->
  decoder:(int Reader.t -> Virtual_address.t -> Instruction.t) ->
  Virtual_address.t ->
  int Reader.t ->
  int ->
  t

val address : t -> Virtual_address.t
val graph : t -> Ir.View.t
val callback : t -> Callback.t
val killset : t -> Ir.Graph.vertex -> Dba_types.Var.Set.t
val fetch_no_link : t -> Virtual_address.t -> Ir.Graph.vertex
val disassemble_from : t -> Virtual_address.t -> Ir.Graph.vertex
