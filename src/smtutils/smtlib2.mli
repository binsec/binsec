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

(** Smtlib2 data representation (AST) *)

(** This module provides a light SMTLIB 2 expression
    representation for the {b bitvector} and 
    {b array} theories. 
*)

(** Raise if tying to extract beyond the bitvector
    bound or if an expression is inconsistent in
    terms of expression sizes *)
exception SmtBadSize of string

type smt_bv_unary =
  | SmtBvNeg                  (** 2's complement minus [bvneg] *)
  | SmtBvNot                  (** bitwise negation [bvnot] *)
  | SmtBvExtract of int * int (** [SmtBvExtract l h] extract bits from [l] (lower bound) to [h] (higher bound)*)
  | SmtBvZeroExtend of int    (** extend with '0' the given number of bits *)
  | SmtBvSignExtend of int    (** sign extend the given number of bits *)
  | SmtBvRotateL of int       (** Rotate Left *)
  | SmtBvRotateR of int       (** Rotate Right *)

(** Bitvector binary operators *)
type smt_bv_binary =
  (* linear arithmetic *)
  | SmtBvAdd
  | SmtBvSub
  (* non-linear arithmetic *)
  | SmtBvMult
  | SmtBvUdiv
  | SmtBvSdiv
  | SmtBvUrem
  | SmtBvSrem
  | SmtBvSmod
  (* logical *)
  | SmtBvOr
  | SmtBvNor
  | SmtBvAnd
  | SmtBvNand
  | SmtBvXor
  | SmtBvXnor
  | SmtBvConcat
  | SmtBvShl
  | SmtBvLshr
  | SmtBvAshr
  (* comparison *)
  | SmtBvComp
  | SmtBvDiff
  (* the following operators return a boolean *)
  | SmtBvUle
  | SmtBvUlt
  | SmtBvUge
  | SmtBvUgt
  | SmtBvSle
  | SmtBvSlt
  | SmtBvSge
  | SmtBvSgt

(** [smt_bv_var] Represent a bitvector sort defined by a
    [name] and a [size] in bits *)
type smt_bv_var = string * int

(** [smt_abv_arry name index content] Represent an array sort
    used to represent the memory defined by a [name] and the
    size of addresses [index] and the size of the data [content]
    usually 8 bits *)
type smt_abv_array = string * int * int

(** inputs which are either bitvectors or arrays (memory) *)
type smt_var_decl = 
  | SmtBv of smt_bv_var
  | SmtABv of smt_abv_array

(** smtlib2 bitvector expression *)
type smt_bv_expr = 
  | SmtBvCst of Bitvector.t  (** constant: reification of {Bitvector.t} *)
  | SmtBvVar of smt_bv_var   (** variable: reification of [smt_bv_var] *)
  | SmtBvUnary of smt_bv_unary * smt_bv_expr                 (** unary expression *)
  | SmtBvBinary of smt_bv_binary * smt_bv_expr * smt_bv_expr (** binary expression *)
  | SmtBvIte of smt_expr * smt_bv_expr * smt_bv_expr         (** If-The-Else *)
  | SmtABvSelect of smt_abv_expr * smt_bv_expr               (** read in an array *)
  | SmtBvLet of ((smt_expr * smt_expr) list) * smt_bv_expr   (** locale definition of variables *)
  | SmtABvLoad32 of smt_abv_expr * smt_bv_expr (** syntactic sugar for 4-bytes read in arrays *)
  | SmtBvToken (** special type acting as placeholder for nested expresion replacement *)

(** smtlib2 array expression *)
and smt_abv_expr = 
  | SmtABvArray of smt_abv_array (** Array variable (reification of [smt_abv_array]) *)
  | SmtABvStore of smt_abv_expr * smt_bv_expr * smt_bv_expr (** store in the array *)
  | SmtABvLet of ((smt_expr * smt_expr) list) * smt_abv_expr (** local binding of variables *)
  | SmtABvStore32 of smt_abv_expr * smt_bv_expr * smt_bv_expr (* syntactic sugar for 4-bytes store in arrays *)

(** smtlib2 expression (boolean) *)
and smt_expr = 
  | SmtBvExpr of smt_bv_expr        (** reification of [smt_bv_expr] *)
  | SmtABvArrayExpr of smt_abv_expr (** reification of [smt_abv_expr] *)
  | SmtAnd of smt_expr * smt_expr
  | SmtOr of smt_expr * smt_expr
  | SmtComp of smt_expr * smt_expr
  | SmtNot of smt_expr
  | SmtLet of ((smt_expr * smt_expr) list) * smt_expr
  | SmtIte of smt_expr * smt_expr * smt_expr
  | SmtTrue
  | SmtFalse
  | SmtToken (** special type acting as placeholder for nested expresion replacement *)
  | SmtComment of string (** special type to print comment in formulas *)

(** variant for different smtlib2 outcomes *)
type smt_result = | SAT | UNSAT | TIMEOUT | UNKNOWN

(** associate an exit code for the different [smt_result]:
    - SAT: 0
    - UNSAT: 10
    - TIMEOUT: 11
    - UNKNOWN: 12 *)
val smt_result_to_exit_code: smt_result -> int

(** {2 Constructors} *)

val constant : Bitvector.t -> smt_bv_expr
  
val lognot : smt_expr -> smt_expr
val logand : smt_expr -> smt_expr -> smt_expr
val logor : smt_expr -> smt_expr -> smt_expr

val pp_result : Format.formatter -> smt_result -> unit 

(** Set of [smt_var_decl], used to hold inputs *)
module SmtVarSet : Set.S with type elt = smt_var_decl

(** Set of [smt_bv_var] to hold a set of variables *)
module SmtBvVarSet : Set.S with type elt = smt_bv_var

(** add one to the given smt_bv_expr *)
val smtbv_add_one: smt_bv_expr -> smt_bv_expr

(** add the given int value to the expression *)
val smtbv_add_int: smt_bv_expr -> int -> smt_bv_expr

(** decrement the given [smt_bv_expr] by the constant 1 *)
val smtbv_sub_one: smt_bv_expr -> smt_bv_expr

(** [smtbv_extract low high] create an extract expression
    extracting from low to high bits *)
val smtbv_extract: smt_bv_expr -> int -> int -> smt_bv_expr

(** return [true] only if the [smt_expr] is [SmtTrue] *)
val smtexpr_is_true: smt_expr -> bool

(** smt_bv_expr which value is 1 of size 1 *)
val bvone: smt_bv_expr

val one : smt_expr
(** smt_bv_expr which value is 0 of size 1 *)

val bvzero: smt_bv_expr

val zero : smt_expr
  
(** @return [true] if expressions are equal *)
val expr_equal: smt_expr -> smt_expr -> bool

(** @return [true] if the two bitvector expressions are equal *)
val bvexpr_equal: smt_bv_expr -> smt_bv_expr -> bool

(** @return [true] if the two array expression expressions are equal *)
val abvexpr_equal: smt_abv_expr -> smt_abv_expr -> bool

(** @return the size of a [smt_bv_expr] *)
val size_bvexpr: smt_bv_expr -> int

(** @return the size of a [smt_abv_expr] *)
val size_abvexpr: smt_abv_expr -> int * int
