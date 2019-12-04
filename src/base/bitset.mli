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

(** Bitsets

    A dense representation for sets of integers.

    If your set is sparse with a big upper-bound, think twice about using this
    data structure since both absence and presence are explicitly marked as a
    bit value.
*)

type t

val create : int -> t
(** [create n] creates a bitset containing at most [n] 0-indexed elements.
 *)

val set : t -> int -> t
(** [set b i] sets the [i]th bit of bitset [b] *)

val get : t -> int -> bool
(** [get b i] gets the [i]th bit of bitset [b] *)

val remove : t -> int -> t
(** [remove b i] unsets the [i]th bit of bitset [b] *)

val flip : t -> int -> t
(** [flip b i] flips the [i]th bit of bitset [b] *)

val equal : t -> t -> bool

val union : t -> t -> t
val inter : t -> t -> t

val subset : t -> t -> bool
(** [subset b1 b2] tests if [b1] is a subset of [b2].

    @requires [size b1 <= size b2]. Otherwise returns [false].
*)

val size : t -> int
(** [size b] returns the number of elements that [b] can contain *)

val cardinal : t -> int
(** [cardinal b] returns the number of elements that [b] does contain, i.e,
    the number of bits actually set in [b].
 *)

val pp : Format.formatter -> t -> unit
(** [pp ppf b] outputs a string representation of [b] into the pretty-print
    formatter [ppf].

    The string representation is the hexadecimal string representing the bitset.
*)

val resize : t -> int -> t
(** [resize b n] adjust the size of [b] *)

val fold : ('a -> int -> 'a) -> 'a -> t -> 'a

val map : (int -> bool) -> t -> t
