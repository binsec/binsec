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

type trace = No | Assembly | Ir

type 'a primitive = 'a Types.primitive =
  | Unknown
  | Apply of ('a -> unit)
  | Call of ('a -> 'a Types.continuation)

(** Information to be used by optimization. *)
type 'a knowledge =
  | May_read : Dba_types.Var.Set.t option knowledge
      (** The (overapproximed) set of all variables a builtin may access.
          Variables that are not in this set may have an undefined value.
          [None] means any variable may be read. *)
  | May_write : Dba_types.Var.Set.t option knowledge
      (** The (overapproximed) set of all variables a builtin may modify.
          Absent variables are deemed to keep the same value. *)

val invalid_successor : ([ `All ], 'a) Types.fiber

val relink :
  ?taken:bool ->
  pred:([ `All ], 'a) Types.fiber ->
  ([ `All ], 'a) Types.fiber ->
  unit

module type ASSEMBLER = sig
  type 'a t

  val empty : (module Types.PATH with type t = 'a) -> 'a t
  val assign : 'a t -> Dba.Var.t -> Dba.Expr.t -> unit
  val clobber : 'a t -> Dba.Var.t -> unit
  val symbolize : 'a t -> Dba.Var.t -> unit
  val forget : 'a t -> Dba.Var.t -> unit

  val load :
    'a t ->
    Dba.Var.t ->
    string option ->
    Machine.endianness ->
    Dba.Expr.t ->
    unit

  val store :
    'a t ->
    string option ->
    Machine.endianness ->
    addr:Dba.Expr.t ->
    Dba.Expr.t ->
    unit

  val assume : 'a t -> Dba.Expr.t -> unit
  val check : 'a t -> Dba.Expr.t -> unit

  val apply :
    'a t ->
    ?input:Dba_types.Var.Set.t ->
    ?output:Dba_types.Var.Set.t ->
    ('a -> unit) ->
    unit

  val commit :
    'a t -> pred:([ `All ], 'a) Types.fiber -> ([ `All ], 'a) Types.fiber
end

module Straight : ASSEMBLER
module Default : ASSEMBLER
module Cse : ASSEMBLER

type 'a config

val make_config :
  ?debug:trace ->
  echo:('a -> string -> unit) ->
  step:('a -> Virtual_address.t -> int -> unit) ->
  (module Types.PATH with type t = 'a) ->
  (module ASSEMBLER) ->
  'a config

val register_builtin_callback :
  'a config -> (Ir.builtin -> 'a primitive) -> unit

val register_knowledge :
  'a config -> 'b knowledge -> (Ir.builtin -> 'b option) -> unit

val set_annotation_printer :
  'a config -> (Format.formatter -> Virtual_address.t -> unit) option -> unit

val resolve_builtin : 'a config -> Ir.builtin -> 'a primitive

type 'a cache = ([ `All ], 'a) Types.fiber Basic_types.Integers.Int.Htbl.t
type 'a t

val create :
  'a config ->
  ?killset:(Ir.View.vertex -> Dba_types.Var.Set.t) ->
  ?fibers:'a cache ->
  Ir.View.t ->
  'a t

val get : 'a t -> Ir.View.vertex -> ([ `All ], 'a) Types.fiber
