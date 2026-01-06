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

open Types

type 'a hook = 'a Binsec_sse_loader.Disassembly.hook =
  | Fetch : (Virtual_address.t -> Ir.Graph.t option) hook
  | Decode : (Virtual_address.t -> int Reader.t -> Ir.Graph.t option) hook
  | Disasm : (Instruction.t -> Ir.Graph.t option) hook
  | Rewrite : (Ir.Graph.t -> unit) hook

(** Information to be used by optimization. *)
type 'a knowledge = 'a Binsec_sse_loader.Disassembly.knowledge =
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

type 'a config = 'a Compiler.config
type 'a t

val create :
  Callback.t ->
  decoder:(int Reader.t -> Virtual_address.t -> Instruction.t) ->
  Virtual_address.t ->
  Virtual_address.t Reader.t ->
  Z.t ->
  'a config ->
  'a t

val create_small :
  Callback.t ->
  decoder:(int Reader.t -> Virtual_address.t -> Instruction.t) ->
  Virtual_address.t ->
  int Reader.t ->
  int ->
  'a config ->
  'a t

val address : 'a t -> Virtual_address.t
val graph : 'a t -> Ir.View.t
val callback : 'a t -> Callback.t
val killset : 'a t -> Ir.Graph.vertex -> Dba_types.Var.Set.t
val fetch_no_link : 'a t -> Virtual_address.t -> ([ `All ], 'a) Types.fiber
val disassemble_from : 'a t -> Virtual_address.t -> ([ `All ], 'a) Types.fiber
