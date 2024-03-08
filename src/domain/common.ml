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

exception Empty

type projection = Top | Point of Z.t | Seq of { start : Z.t; n : Z.t }

module type S = sig
  type t

  val pp : Format.formatter -> t -> unit

  val included : size:int -> t -> t -> bool
  (** [included  ~size t t'] tests if all values in [t] are in [t']. *)

  val disjoint : size:int -> t -> t -> bool
  (** [disjoint ~size t t'] returns [true]
      if the intersection between [t] and [t'] is empty. *)

  val union : size:int -> t -> t -> t

  val inter : size:int -> t -> t -> t
  (** [inter ~size t t'] returns the intersection of the intervals [t] and [t'].

      @raise [Empty] If there is no intersection.
  *)

  val project : size:int -> t -> projection
  val top : int -> t
  val constant : size:int -> Z.t -> t
  val zero : t
  val one : t
  val uminus : size:int -> t -> t
  val uminus_feedback : size:int -> t -> t -> t
  val add : size:int -> t -> t -> t
  val add_feedback : size:int -> t -> t -> t -> t * t
  val sub : size:int -> t -> t -> t
  val sub_feedback : size:int -> t -> t -> t -> t * t
  val mul : size:int -> t -> t -> t
  val mul_feedback : size:int -> t -> t -> t -> t * t
  val smod : size:int -> t -> t -> t
  val smod_feedback : size:int -> t -> t -> t -> t * t
  val umod : size:int -> t -> t -> t
  val umod_feedback : size:int -> t -> t -> t -> t * t
  val udiv : size:int -> t -> t -> t
  val udiv_feedback : size:int -> t -> t -> t -> t * t
  val sdiv : size:int -> t -> t -> t
  val sdiv_feedback : size:int -> t -> t -> t -> t * t
  val append : size1:int -> t -> size2:int -> t -> t
  val append_feedback : size1:int -> t -> size2:int -> t -> t -> t * t
  val equal : size:int -> t -> t -> t
  val equal_feedback : size:int -> t -> t -> t -> t * t
  val diff : size:int -> t -> t -> t
  val diff_feedback : size:int -> t -> t -> t -> t * t
  val ule : size:int -> t -> t -> t
  val ule_feedback : size:int -> t -> t -> t -> t * t
  val uge : size:int -> t -> t -> t
  val uge_feedback : size:int -> t -> t -> t -> t * t
  val ult : size:int -> t -> t -> t
  val ult_feedback : size:int -> t -> t -> t -> t * t
  val ugt : size:int -> t -> t -> t
  val ugt_feedback : size:int -> t -> t -> t -> t * t
  val sle : size:int -> t -> t -> t
  val sle_feedback : size:int -> t -> t -> t -> t * t
  val sge : size:int -> t -> t -> t
  val sge_feedback : size:int -> t -> t -> t -> t * t
  val slt : size:int -> t -> t -> t
  val slt_feedback : size:int -> t -> t -> t -> t * t
  val sgt : size:int -> t -> t -> t
  val sgt_feedback : size:int -> t -> t -> t -> t * t
  val logand : size:int -> t -> t -> t
  val logand_feedback : size:int -> t -> t -> t -> t * t
  val logor : size:int -> t -> t -> t
  val logor_feedback : size:int -> t -> t -> t -> t * t
  val lognot : size:int -> t -> t
  val lognot_feedback : size:int -> t -> t -> t
  val logxor : size:int -> t -> t -> t
  val logxor_feedback : size:int -> t -> t -> t -> t * t
  val shift_left : size:int -> t -> t -> t
  val shift_left_feedback : size:int -> t -> t -> t -> t * t
  val shift_right : size:int -> t -> t -> t
  val shift_right_feedback : size:int -> t -> t -> t -> t * t
  val shift_right_signed : size:int -> t -> t -> t
  val shift_right_signed_feedback : size:int -> t -> t -> t -> t * t
  val rotate_left : size:int -> t -> t -> t
  val rotate_left_feedback : size:int -> t -> t -> t -> t * t
  val rotate_right : size:int -> t -> t -> t
  val rotate_right_feedback : size:int -> t -> t -> t -> t * t
  val uext : int -> size:int -> t -> t
  val uext_feedback : int -> size:int -> t -> t -> t
  val sext : int -> size:int -> t -> t
  val sext_feedback : int -> size:int -> t -> t -> t
  val restrict : lo:int -> hi:int -> size:int -> t -> t
  val restrict_feedback : lo:int -> hi:int -> size:int -> t -> t -> t
end
