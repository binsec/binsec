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

module Translate : sig
  val expr :
    Smt_symbolic.State.t -> Dba.Expr.t -> Formula.bv_term * Smt_symbolic.State.t
  (** missing bitvectors are implicitely declared so this returns a new symbolic state *)

  val assign :
    ?wild:bool ->
    Dba.LValue.t ->
    Dba.Expr.t ->
    Smt_symbolic.State.t ->
    Smt_symbolic.State.t

  val havoc :
    ?naming_hint:string ->
    ?wild:bool ->
    Dba.LValue.t ->
    Smt_symbolic.State.t ->
    Smt_symbolic.State.t

  val assume : Dba.Expr.t -> Smt_symbolic.State.t -> Smt_symbolic.State.t
end

module Solver : Smt_sig.Solver
