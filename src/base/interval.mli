(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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

type 'a t = 'a Basic_types.interval = { lo : 'a; hi : 'a }

val belongs    : ('a -> 'a -> int) -> 'a -> 'a t -> bool
val intersects : ('a -> 'a -> int) -> 'a t -> 'a t -> bool

module type S =
sig
  type point
  type interval
  type t

  val empty : t
  val singleton : interval -> t
  val add    : interval -> t -> t
  val remove : interval -> t -> t

  val is_empty : t -> bool
  val mem : interval -> t -> bool

  val belongs    : point    -> t -> interval list
  val intersects : interval -> t -> interval list

  val iter : (interval -> unit) -> t -> unit
  val fold : (interval -> 'a -> 'a) -> t -> 'a -> 'a

  val union : t -> t -> t
  val inter : t -> t -> t
end

(* Set of intervals. [add] and [remove] have the same semantic than in
 * [Set.S with type elt = interval], but optimized for intervals. *)
module Make (Ord : Sigs.COMPARABLE) : S
  with type point = Ord.t
   and type interval = Ord.t t

(* Set of values represented with flattened intervals.
 * [add] and [remove] add or remove intervals of values. *)
module Flat (Ord : Sigs.ITERABLE) : S
  with type point = Ord.t
   and type interval = Ord.t t

module Int : S
  with type point = int
   and type interval = int t

module IntFlat : S
  with type point = int
   and type interval = int t

module Float : S
  with type point = float
   and type interval = float t

module FloatFlat : S
  with type point = float
   and type interval = float t

(* Unsigned bitvector intervals *)
module BitVec :
sig
  include S
    with type point = Bitvector.t
     and type interval = Bitvector.t t

  val ule : Bitvector.t -> t
  val uge : Bitvector.t -> t
  val ult : Bitvector.t -> t
  val ugt : Bitvector.t -> t

  val sle : Bitvector.t -> t
  val sge : Bitvector.t -> t
  val slt : Bitvector.t -> t
  val sgt : Bitvector.t -> t
end

module BitVecFlat :
sig
  include S
    with type point = Bitvector.t
     and type interval = Bitvector.t t

  val top : int -> t
  val bot : int -> t

  val equal : Bitvector.t -> t
  val distinct : Bitvector.t -> t

  val ule : Bitvector.t -> t
  val uge : Bitvector.t -> t
  val ult : Bitvector.t -> t
  val ugt : Bitvector.t -> t

  val sle : Bitvector.t -> t
  val sge : Bitvector.t -> t
  val slt : Bitvector.t -> t
  val sgt : Bitvector.t -> t
end
