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

type trilean = Basic_types.Ternary.t = False | True | Unknown
type unary = Term.unary
type binary = Term.binary
type 'a operator = 'a Term.operator

module Bv = Bitvector
module BiMap = Basic_types.Integers.Bigint.Map

module rec Expr : sig
  include Term.S with type a := string and type b := Memory.t
end

and Memory : sig
  type 'a node =
    | None : [< `None | `Any ] node
    | Symbol : {
        id : int;
        name : string;
        index : int;
      }
        -> [< `Some | `Symbol | `Any ] node
    | Layer : {
        id : int;
        over : t;
        addr : Expr.t;
        store : Store.t;
      }
        -> [< `Some | `Any ] node

  and symbol = [ `Symbol ] node
  and nullable = [ `Any ] node
  and t = [ `Some ] node

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val none : nullable
  val any : t -> nullable
  val root : int -> t
  val symbol : string -> int -> t
  val layer : Expr.t -> Store.t -> t -> t
  val base : t -> symbol
end

and Store : sig
  include Lmap.S with type v := Chunk.t

  val singleton : Bv.t -> Chunk.t -> t
  val store : Bv.t -> Chunk.t -> t -> t
  val select : (Z.t -> int -> Chunk.t) -> Bv.t -> int -> t -> Chunk.t
  val iter_term : (Z.t -> Expr.t -> unit) -> t -> unit
  val fold_term : (Z.t -> Expr.t -> 'a -> 'a) -> 'a -> t -> 'a
end

and Chunk : sig
  type t

  type hunk =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type kind = Hunk of hunk | Term of Expr.t

  val inspect : t -> kind
  val of_hunk : hunk -> t
  val of_term : Expr.t -> t
  val to_term : t -> Expr.t
  val equal : t -> t -> bool
  val len : t -> int

  (** low level API *)

  val is_hunk : t -> bool
  val is_term : t -> bool
  val unsafe_to_hunk : t -> hunk
  val unsafe_to_term : t -> Expr.t
end

module BvTbl : Hashtbl.S with type key = Expr.t
module AxTbl : Hashtbl.S with type key = Memory.t
module AsMap : Map.S with type key = Memory.symbol
module AsTbl : Hashtbl.S with type key = Memory.symbol
module StTbl : Hashtbl.S with type key = string

module Model : sig
  type t = {
    symbols : Expr.t StTbl.t;
    values : Bv.t BvTbl.t;
    arrays : char Basic_types.Integers.Bigint.Htbl.t AsTbl.t;
  }

  val empty : unit -> t

  val eval :
    ?symbols:(Expr.t -> Bitvector.t) ->
    ?memory:(Memory.symbol -> Bitvector.t -> char) ->
    t ->
    Expr.t ->
    Bv.t

  val complement : from:t -> t -> unit
  val pp : Format.formatter -> t -> unit

  val pp_with_sections :
    (Virtual_address.t -> string option) -> Format.formatter -> t -> unit
end

module BvSet : Set.S with type elt := Expr.t
module BvMap : Map.S with type key := Expr.t
module BvHmap : Hmap.S with type key := Expr.t
