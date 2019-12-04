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

module Query_stats : sig
  val pp : Format.formatter -> unit -> unit
end

module Solver : sig
  val with_solver :
    ?keep:Formula.VarSet.t ->
    Sse_types.Path_state.t -> (Solver.Session.t -> 'a) -> (float * 'a) option

  val check_satistifiability :
    Sse_types.Path_state.t -> Formula.status * Sse_types.Path_state.t

  val get_model : Sse_types.Path_state.t -> Smt_model.t option

  val enumerate_values :
    int -> Formula.bv_term -> Sse_types.Path_state.t
    -> Bitvector.t list * Sse_types.Path_state.t
end

module Translate : sig
  val expr : Sse_symbolic.State.t -> Dba.Expr.t -> Formula.bv_term

  val assign :
    ?wild:bool ->
    Dba.LValue.t -> Dba.Expr.t -> Sse_symbolic.State.t -> Sse_symbolic.State.t

  val assignment :
    ?wild:bool ->
    Dba.LValue.t -> Dba.Expr.t -> Sse_types.Path_state.t -> Sse_types.Path_state.t

  val nondet:
    ?naming_hint:string ->
    ?wild:bool ->
    Dba.LValue.t -> Sse_types.Path_state.t -> Sse_types.Path_state.t

  val assume:
    Dba.Expr.t -> Sse_types.Path_state.t -> Sse_types.Path_state.t
end
