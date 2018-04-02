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

(** Bitvector representation *)
module type Common =
sig
  type t

  val create: Bigint.t -> int -> t
  val create_from_tuple: Bigint.t * int -> t
(*
  val resize: t -> int -> t
  val update: t -> Bigint.t -> t
*)
  val value_of  : t -> Bigint.t
  val signed_of : t -> Bigint.t
  val size_of : t -> int

  val zero : t (* zero = { value = 0; size = 1 } *)
  val one  : t (* one  = { value = 1; size = 1 } *)

  val zeros : int -> t (* zeros n = { value = 0; size = n } *)
  val ones  : int -> t (* ones  n = { value = 1; size = n } *) 
  val fill : ?lo:int -> ?hi:int -> int -> t
  (** [fill lo hi n] returns a bitvector of size [n] where bits from [lo] to
     [hi] are set to one. By default, [lo] is equal to zero and [hi] is equal to
     [n]. Raise [Invalid_argument] if [lo] or [hi] have incoherent values. *)

  val is_zero : t -> bool
  val is_one  : t -> bool
  (** [is_zero t] (resp. [is_one t]) checks if [t] is equal to [zero] (resp.
     [one]) *)

  val is_zeros : t -> bool
  val is_ones  : t -> bool
  (** [is_zeros t] (resp. [is_ones t]) checks if it exists [n] such that [t] is
     equal to [zeros n] (resp. [ones n]) *)

  val max_ubv : int -> t
  val max_sbv : int -> t
  (** [max_ubv n] (resp. [max_sbv n]) returns a bitvector of size [n] containing
     the biggest possible unsigned (resp. signed) value for its size *)

  val is_max_ubv : t -> bool
  val is_max_sbv : t -> bool
  (** [is_max_ubv t] (resp. [is_max_sbv t]) returns [true] if [t] is a bitvector
     containing the biggest possible unsigned (resp. signed) value for its size,
     or returns [false] otherwise *)

  (* Comparison *)
  include Sigs.Comparisons with type t := t

  (* Arithmetic *)
  include Sigs.Arithmetic with type t := t

  val succ : t -> t
  val pred : t -> t

  val umax : t -> t -> t
  val umin : t -> t -> t

  val smax : t -> t -> t
  val smin : t -> t -> t

  val is_neg : t -> bool

  (* Logical *)

  (* land, lor, lxor and lnot are keywords... *)
  include Sigs.Bitwise with type t := t
    
  val reduce : t -> int -> t
  val extend : t -> int -> t
  val extend_signed : t -> int -> t
  val extend_unsafe : t -> int -> t

  val get_bit   : t -> int -> bool
  val set_bit   : t -> int -> t
  val clear_bit : t -> int -> t
  val flip_bit  : t -> int -> t

  val append  : t -> t -> t
  val concat  : t list -> t
  val extract : t -> int -> int -> t
end

type t
include Common with type t := t

(* Conversion *)

val pp : Format.formatter -> t -> unit
(** [pp ppf bv] prints the decimal value of [bv] into [ppf] *)

val pp_hex : Format.formatter -> t -> unit
(** [pp_hex ppf bv] prints the hexadecimal value of [bv] into [ppf] *)
  
val print : t -> string

val of_bool : bool -> t
val to_bool : t -> bool (* to_bool t = not (is_zero t) *)

val of_int32 : int32 -> t
(** [of_int32 n] creates a bitvector of size 32 and value [n] *)

val to_int32 : t -> int32

val of_int64 : int64 -> t
(** [of_int64 n] creates a bitvector of size 64 and value [n] *)

val to_int64 : t -> int64

val of_hexstring : string -> t
val to_hexstring : t -> string
