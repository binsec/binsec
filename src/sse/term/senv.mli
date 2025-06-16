(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2025                                               *)
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

val get_solver : ?solver:Smt.Smt_options.solver -> unit -> (module Solver.OPEN)

module State
    (D : Domains.S)
    (Solver : Solver.GET_MODEL_WITH_STATS)
    (QS : Types.QUERY_STATISTICS) :
  Types.RAW_STATE with type Value.t = Sexpr.Expr.t

type _ Types.value += Term : Sexpr.Expr.t Types.value
type Options.Engine.t += Vanilla | Multi_checks

type 'a Types.feature +=
  | VisibleSymbols : Sexpr.Expr.t Dba_types.Var.Map.t Types.feature
  | VisibleMemory : Sexpr.Memory.t Types.feature
