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

(** Path_predicate_formula optimizations *)


(** {2 Constant propagation}

    Functions trying to propagate constants on the
    given expression.
    @return the expression simplified *)

val propagate_cst :
  Path_predicate_env.formula ->
  ?recursive:int -> Formula.bl_term -> Formula.bl_term

val propagate_cst_bv :
  Path_predicate_env.formula ->
  ?recursive:int -> Formula.bv_term -> Formula.bv_term

val propagate_cst_abv :
  Path_predicate_env.formula ->
  ?recursive:int -> Formula.ax_term -> Formula.ax_term


(** {2 Rebase}

    In the formula all the variables are in a SSA
    form. The rebase optimization will intent to
    redefine variables used in the expression by
    older variables declaration eg:
    esp1 := esp0 + 4
    esp2 := esp1 + 4
    esp3 := esp2 + 4 => esp2+4 will be replaced by esp1+8
    thus obsolating the esp2 declaration. This reduce
    the number of variables defined and is crucial for some
    other optimizations *)

val rebase_expr :
  Path_predicate_env.formula -> Formula.bl_term -> Formula.bl_term

val rebase_bvexpr :
  Path_predicate_env.formula -> Formula.bv_term -> Formula.bv_term

val rebase_abvexpr :
  Path_predicate_env.formula -> Formula.ax_term -> Formula.ax_term


(** {2 Read-Over-Write}

    The Read-Over-Write as an array theory optimization
    based on the fact that read(store(M, addr, X),addr) = X.
    Thus the optimization will try for every read to replace
    it by its store value (if any). To do so the classic linear
    method look in the store chain if the read address has
    previously been written. The hybrid method does the same
    but in a more efficient manner. *)

(** @return some stats about replacement that took place
    in the RoW optimization. Return values are:
    - min number of iteration for the lookup
    - max number of iteration for the lookup
    - moy number of iteration for the lookup
    - found number of read replaced
    - rebase number of read rebased on an older memory
    - disjoint others *)
val get_row_stats: unit -> (int * int * int * int * int * int)

(** {3 Classic (linear)} *)

val read_over_write :
  Path_predicate_env.formula -> Formula.bl_term -> Formula.bl_term

val read_over_write_bv :
  Path_predicate_env.formula -> Formula.bv_term -> Formula.bv_term

val read_over_write_abv :
  Path_predicate_env.formula -> Formula.ax_term -> Formula.ax_term

(** {3 Hybrid} *)

val update_hybrid_memory :
  Path_predicate_env.formula ->
  string ->
  Formula.ax_term -> Path_predicate_env.hybrid_mem_t * Formula.ax_term

val read_over_write_hybrid :
  Path_predicate_env.formula -> Formula.bl_term -> Formula.bl_term

val read_over_write_hybrid_bv :
  Path_predicate_env.formula -> Formula.bv_term -> Formula.bv_term

val read_over_write_hybrid_abv :
  Path_predicate_env.formula -> Formula.ax_term -> Formula.ax_term



(** {2 Memory flattening}

    We call memory flattening the act of removing
    every array operations in the specific cases
    where all select/store are being made on constant
    addresses. This allows to generate formulas "solvable"
    by solvers not supporting the SMT array theory *)
(*
(** Class that remove all read in memory by constant
    bitvector values *)
class memory_flattener_visitor : object inherit Formula_visitor.smt_map_visitor

  method get_new_symbols : unit -> Formula.SmtVarSet.t
  (** @return all the bitvector inputs that have been
      created by replacing select in memory by bitvectors *)
end
*)
