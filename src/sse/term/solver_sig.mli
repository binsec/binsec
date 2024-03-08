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

open Sexpr

module type S = sig
  type result = Sat | Unsat | Unknown
  type term

  val put : Suid.t -> Expr.t list -> unit
  val set_memory : addr:Z.t -> Z.t -> unit
  val neq : term -> Z.t -> unit
  val bind : Suid.t -> Expr.t -> Expr.t list -> term
  val iter_free_variables : (string -> Expr.t -> unit) -> unit
  val iter_free_arrays : (string -> Memory.t -> unit) -> unit
  val get : Expr.t -> term
  val check_sat : unit -> result
  val get_value : term -> Z.t
  val get_array : Memory.t -> (Z.t * char) array
  val close : unit -> unit
end

module type FACTORY = functor () -> S
