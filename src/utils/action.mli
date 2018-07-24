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

(** Actions are general located goals that one would like to achieve during an
 *  analysis
 *)

type side = private
  | Consequent
  | Alternative

module Count : sig
  type t = private
    | Unlimited
    | Count of int

  val pp : Format.formatter -> t -> unit
  val count : int -> t
  val unlimited : t
  val decr : t -> t
  val is_zero : t -> bool
end

type goal = private
  | Reach of Count.t
  | Enumerate of Count.t * Dba.Expr.t
  | Cut
  | Restrict of Dba.Expr.t * Dba.Expr.t
  | Choice of side

type t = {
    address : Virtual_address.t;
    goal : goal;
  }

val check_and_decr : t -> t option
val goal : t -> goal
val address : t -> Virtual_address.t

val pp : Format.formatter -> t -> unit

(** {2 Constructors} *)

val reach : ?n:int -> Virtual_address.t -> t
val reach_all : Virtual_address.t -> t

val enumerate : ?n:int -> Dba.Expr.t -> Virtual_address.t -> t
val enumerate_all : Dba.Expr.t -> Virtual_address.t -> t

val cut : Virtual_address.t -> t

val restrict : Dba.Expr.t -> Dba.Expr.t -> Virtual_address.t -> t

val choose_alternative: Virtual_address.t -> t
val choose_consequent: Virtual_address.t -> t
