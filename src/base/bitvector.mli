(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
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
module type Common = sig
  type t

  val create : Z.t -> int -> t
  val create_from_tuple : Z.t * int -> t

  (*
  val resize: t -> int -> t
  val update: t -> Z.t -> t
*)
  val value_of : t -> Z.t
  val signed_of : t -> Z.t
  val size_of : t -> int
  val compare : t -> t -> int
  val hash : t -> int
  val zero : t (* zero = { value = 0; size = 1 } *)
  val one : t (* one  = { value = 1; size = 1 } *)
  val zeros : int -> t (* zeros n = { value = 0; size = n } *)
  val ones : int -> t (* ones  n = { value = 1; size = n } *)

  val fill : ?lo:int -> ?hi:int -> int -> t
  (** [fill lo hi n] returns a bitvector of size [n] where bits from [lo] to
      [hi] are set to one. By default, [lo] is equal to zero and [hi] is equal to
      [n]. Raise [Invalid_argument] if [lo] or [hi] have incoherent values. *)

  val is_zero : t -> bool

  val is_one : t -> bool
  (** [is_zero t] (resp. [is_one t]) checks if [t] is equal to [zero] (resp.
      [one]) *)

  val is_zeros : t -> bool
  val is_ones : t -> bool

  val is_fill : t -> bool
  (** [is_zeros t] (resp. [is_ones t]) checks if it exists [n] such that [t] is
      equal to [zeros n] (resp. [ones n]) *)

  val max_ubv : int -> t
  val max_sbv : int -> t

  val min_sbv : int -> t
  (** [max_ubv n] (resp. [max_sbv n]) returns a bitvector of size [n] containing
      the biggest possible unsigned (resp. signed) value for its size *)

  val is_max_ubv : t -> bool
  val is_max_sbv : t -> bool

  val is_min_sbv : t -> bool
  (** [is_max_ubv t] (resp. [is_max_sbv t]) returns [true] if [t] is a bitvector
      containing the biggest possible unsigned (resp. signed) value for its size,
      or returns [false] otherwise *)

  (* Comparison *)
  include Sigs.COMPARISON with type t := t and type boolean = bool

  (* Arithmetic *)
  include Sigs.ARITHMETIC with type t := t

  val pow : t -> t -> t
  val succ : t -> t
  val pred : t -> t
  val add_int : t -> int -> t
  val umax : t -> t -> t
  val umin : t -> t -> t
  val smax : t -> t -> t
  val smin : t -> t -> t
  val is_neg : t -> bool

  val is_pos : t -> bool
  (** [is_neg t] (resp. [is_pos t]) returns [true] if the signed interpretation
      of [t] is strictly negative (resp. strictly positive) *)

  (* Logical *)

  (* land, lor, lxor and lnot are keywords... *)
  include Sigs.BITWISE with type t := t

  val reduce : t -> int -> t
  val extend : t -> int -> t
  val extend_signed : t -> int -> t
  val extend_unsafe : t -> int -> t
  val num_bits : t -> int
  val get_bit : t -> int -> bool
  val set_bit : t -> int -> t
  val clear_bit : t -> int -> t
  val flip_bit : t -> int -> t
  val append : t -> t -> t

  val concat : t list -> t
  (** concat [bv1, bv2, ..., bvn] is bv1 @ bv2 @ ... @ bvn *)

  val extract : t -> int Basic_types.interval -> t
end

type t

include Common with type t := t and type boolean = bool

(* Conversion *)

(** {3 Creation functions } *)

val rand : int -> t
(** [rand sz] creates a bitvector of size sz with random value *)

val of_bool : bool -> t
val to_bool : t -> bool (* to_bool t = not (is_zero t) *)
val of_char : char -> t
val to_char : t -> char

val of_int32 : int32 -> t
(** [of_int32 n] creates a bitvector of size 32 and value [n] *)

val to_int32 : t -> int32

val of_int64 : int64 -> t
(** [of_int64 n] creates a bitvector of size 64 and value [n] *)

val to_int64 : t -> int64
val of_int : size:int -> int -> t
val to_int : t -> int
val to_uint : t -> int

val of_bits : string -> t
(** [of_bits s] cast an ascii string [s] to its bitvector representation. *)

val of_string : string -> t
(** [of_string s] converts a string [s] whose format respects
    ([+-])?0[xb][0-9]+ i.e., hex and bin strings

    Otherwise raises Invalid_argument
*)

val to_string : t -> string
(** [to_string s] converts a bitvector to hexadecimal notation (if size is a multiple of 4)
    or to binary (otherwise). *)

val of_hexstring : string -> t
(** [of_hexstring] is an alias for [of_string] *)

val to_bitstring : t -> string
(** [to_bitstring bv] returns the binary string representation of [bv],
    i.e., the string ["0b"[01]+] with the same value and size as [bv].
*)

val to_hexstring : t -> string
(** [to_hexstring bv] returns the hex string representation of [bv],
    i.e., the string ["0x"[0-9a-f]+] with the same value and size as [bv].
*)

val to_asciistring : t -> string
(** [to_asciistring bv] returns the ascii string representation of [bv].
*)

(** {3 Printing functions }*)

val pp : Format.formatter -> t -> unit
(** [pp ppf bv] prints the decimal value of [bv] into [ppf] *)

val pp_hex : Format.formatter -> t -> unit
(** [pp_hex ppf bv] prints the hexadecimal value of [bv] into [ppf] *)

val pp_hex_or_bin : Format.formatter -> t -> unit
(** [pp_hex ppf bv] prints the hexadecimal value of [bv] into [ppf] if
   possible, else prints the binary value of [bv]. *)

val print : t -> string
(** [print bv] behaves like
    [pp Format.str_formatter bv; Format.flush_str_formatter ()]
*)

module Collection : Sigs.Collection with type t := t
