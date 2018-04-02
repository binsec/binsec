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

(** Shelter for functions over DBA *)

module Expr : sig
  val fold_expr : Dba.expr -> Bitvector.t                             
end

val checksize_address : Dba.address -> bool
val checksize_dbacond : Dba.cond -> bool
val checksize_instruction : Dba.instruction -> bool (* basic typing *)
val computesize_dbalhs : Dba.lhs -> int
val computesize_dbaexpr : Dba.expr -> int

val contains_lhs : Dba.lhs -> Dba.lhs -> bool

val globalize_address :
  Dba.address -> Dba.jump_target -> Dba.address
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

val substitute_dba_expr: Dba.expr -> Dba.expr -> Dba.expr -> Dba.expr

val substitute_dba_expr_cond: Dba.expr -> Dba.expr -> Dba.cond -> Dba.cond
