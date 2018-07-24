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

(** Shelter for functions over DBA *)



module Expr : sig
  exception Fold_const_failed
  val fold_expr : Dba.Expr.t -> Bitvector.t
end

val checksize_address : Dba.address -> bool
val checksize_dbacond : Dba.Expr.t -> bool
val checksize_instruction : Dba.Instr.t -> bool (* basic typing *)
val computesize_dbalhs : Dba.LValue.t -> int
val computesize_dbaexpr : Dba.Expr.t -> int

val contains_lhs : Dba.LValue.t -> Dba.LValue.t -> bool

val globalize_address :
  Dba.address -> Dba.id Dba.jump_target -> Dba.address
(** [globalize_address root addr] generates a global address from [addr],
 *   rooting it at [root] if it is local.
*)


val eval_alternatives : ('a -> 'b) -> ('b -> 'b -> bool) -> 'a list -> 'b
(* [eval_alternatives eval eq exprs]
   returns the evaluation of the head of the expression list provided all other
   alternatives are equal to it.
   Raise [Errors.Alternative_conflict_values] if one of the value differs
   Raise [Failure "eval_alternatives"] if the list is empty
*)

val substitute_dba_expr: Dba.Expr.t -> Dba.Expr.t -> Dba.Expr.t -> Dba.Expr.t
