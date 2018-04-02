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

(** Interval sub-domains *)
module type S = sig
  type t = private { lo : Bitvector.t; hi : Bitvector.t; }

  val is_disjoint : t -> t -> bool
  val is_point : t -> bool
  val set_lo : Bitvector.t -> t -> t
  val set_hi : Bitvector.t -> t -> t
  val pp : Format.formatter -> t -> unit
  val create : Bitvector.t -> Bitvector.t -> t
  val of_point : Bitvector.t -> t
  val is_unit : t -> bool
  val equal : t -> t -> bool
  val bool_true : t
  val bool_false : t
  val bool_any : t

  val top : int -> t
  val topify : t -> t
  val maxify : t -> t
  val minify : t -> t

  val low : t -> t option
  val high : t -> t option

  val meet : t -> t -> t option
  val join : t -> t -> t

  val contains : t -> t -> bool
  val contains_zero : t -> bool

  val logand : t -> t -> t
  val logor  : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t

  val shift_left  : t -> t -> t
  val shift_right : t -> t -> t
  val shift_right_signed : t -> t -> t

  val rotate_left  : t -> t -> t
  val rotate_right : t -> t -> t

  include Sigs.Arithmetic with type t := t
  val neg : t -> t
  val eq : t -> t -> t
  val diff  : t -> t -> t

  val ule : t -> t -> t
  val uge : t -> t -> t
  val ult : t -> t -> t
  val ugt : t -> t -> t

  val sle : t -> t -> t
  val sge : t -> t -> t
  val slt : t -> t -> t
  val sgt : t -> t -> t
  val extend_signed : t -> int -> t
  val extend : t -> int -> t
end


module Signed : sig
  include S
  val smin : int -> Bitvector.t
end

module Unsigned : sig
  include S

  val map : (Bitvector.t -> 'a) -> t -> 'a list option
end
