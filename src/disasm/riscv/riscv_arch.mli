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

module type S = sig
  type t

  val zero : t
  (** Zero (hard-wired) aka x0 *)

  val ra : t
  (** Return address / x1 *)

  val sp : t
  val gp : t
  val tp : t

  val fp : t
  (** Same as s0 *)

  val a0 : t
  val a1 : t
  val a2 : t
  val a3 : t
  val a4 : t
  val a5 : t
  val a6 : t
  val a7 : t
  val t0 : t
  val t1 : t
  val t2 : t
  val t3 : t
  val t4 : t
  val t5 : t
  val t6 : t
  val s0 : t
  val s1 : t
  val s2 : t
  val s3 : t
  val s4 : t
  val s5 : t
  val s6 : t
  val s7 : t
  val s8 : t
  val s9 : t
  val s10 : t
  val s11 : t
  val name : t -> string
  val size : t -> int
  val num : t -> int
  val bvnum : t -> Bitvector.t
  val of_string : string -> t option
  val of_int_exn : int -> t
  val of_int : int -> t option
  val expr : t -> Dba.Expr.t
  val lval : t -> Dba.LValue.t
end

module Mode : sig
  [@@@warning "-37"]

  type t

  val m32 : t
  val m64 : t
  val m128 : t
  val is_m32 : t -> bool
  val is_m64 : t -> bool
  val is_m128 : t -> bool

  val size : t -> int
  (** size in bits *)
end

(** Parameter for register module,
    size is one of Mode.m32, Mode.m64 or Mode.m128 (Not yet supported) *)
module type RegisterSize = sig
  val size : Mode.t
end

module Register (M : RegisterSize) : S
