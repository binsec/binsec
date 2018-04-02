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

(** Big integer wrapper for BINSEC *)
type t

val zero_big_int : t
val unit_big_int : t
val minus_big_int : t -> t
val abs_big_int : t -> t
val add_big_int : t -> t -> t
val succ_big_int : t -> t
val add_int_big_int : int -> t -> t
val sub_big_int : t -> t -> t
val pred_big_int : t -> t
val mult_big_int : t -> t -> t
val mult_int_big_int : int -> t -> t
val square_big_int : t -> t
val sqrt_big_int : t -> t
val quomod_big_int : t -> t -> t * t
val div_big_int : t -> t -> t
val mod_big_int : t -> t -> t
val gcd_big_int : t -> t -> t
val power : t -> int -> t
val power_big : t -> t -> t
val power_int_positive_int : int -> int -> t
val power_big_int_positive_int : t -> int -> t
val power_int_positive_big_int : int -> t -> t
val power_big_int_positive_big_int : t -> t -> t
val sign_big_int : t -> int
val compare_big_int : t -> t -> int
val eq_big_int : t -> t -> bool
val le_big_int : t -> t -> bool
val ge_big_int : t -> t -> bool
val lt_big_int : t -> t -> bool
val gt_big_int : t -> t -> bool
val max_big_int : t -> t -> t
val min_big_int : t -> t -> t
val num_digits_big_int : t -> int
val string_of_big_int : t -> string
val big_int_of_string : string -> t
val big_int_of_int : int -> t
val is_int_big_int : t -> bool
val int_of_big_int : t -> int
val big_int_of_int32 : int32 -> t
val big_int_of_nativeint : nativeint -> t
val big_int_of_int64 : int64 -> t
val int32_of_big_int : t -> int32
val nativeint_of_big_int : t -> nativeint
val int64_of_big_int : t -> int64
val float_of_big_int : t -> float
val and_big_int : t -> t -> t
val or_big_int : t -> t -> t
val xor_big_int : t -> t -> t
val shift_left_big_int : t -> int -> t
val shift_right_big_int : t -> int -> t
val shift_right_towards_zero_big_int : t -> int -> t
val extract_big_int : t -> int -> int -> t

