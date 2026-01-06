(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

open Types

type status = Smtlib.Solver.status = Sat | Unsat | Unknown

module type S = sig
  val visit_formula : Expr.t list -> unit
  val iter_free_variables : (string -> Expr.t -> unit) -> unit
  val iter_free_arrays : (string -> Memory.symbol -> unit) -> unit
  val assert_formula : Expr.t -> unit
  val assert_distinct : Expr.t -> Expr.t -> unit
  val check_sat : ?timeout:float -> unit -> status
  val check_sat_assuming : ?timeout:float -> Expr.t -> status
  val get_value : Expr.t -> Z.t
  val fold_array_values : (Z.t -> Z.t -> 'a -> 'a) -> Memory.symbol -> 'a -> 'a
  val push : unit -> unit
  val pop : unit -> unit
  val close : unit -> unit
end

module Dummy : S = struct
  let visit_formula _ = ()
  let iter_free_variables _ = ()
  let iter_free_arrays _ = ()
  let assert_formula _ = ()
  let assert_distinct _ _ = ()
  let check_sat ?timeout:_ _ = Unknown
  let check_sat_assuming ?timeout:_ _ = Unknown
  let get_value _ = raise (Invalid_argument "get_value")
  let fold_array_values _ _ _ = raise (Invalid_argument "fold_array_values")
  let push () = ()
  let pop () = ()
  let close () = ()
end
