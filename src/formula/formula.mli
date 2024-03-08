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

(** Definition of abstract representation for logical formulas
    (based on SMTLIB's syntax and semantics) *)

type status = SAT | UNSAT | TIMEOUT | UNKNOWN

val status_to_exit_code : status -> int

type bl_unop = BlNot
type bl_bnop = BlImply | BlAnd | BlOr | BlXor
type bl_comp = BlEqual | BlDistinct

type bv_unop =
  | BvNot
  | BvNeg
  | BvRepeat of int
  | BvZeroExtend of int  (** result has size: `size_of(bv) + int` **)
  | BvSignExtend of int
  | BvRotateLeft of int
  | BvRotateRight of int
  | BvExtract of int Interval.t

type bv_bnop =
  | BvConcat
  | BvAnd
  | BvNand
  | BvOr
  | BvNor
  | BvXor
  | BvXnor
  | BvCmp
  | BvAdd
  | BvSub
  | BvMul
  | BvUdiv
  | BvSdiv
  | BvUrem
  | BvSrem
  | BvSmod
  | BvShl
  | BvAshr
  | BvLshr

type bv_comp =
  | BvEqual
  | BvDistinct
  | BvUlt
  | BvUle
  | BvUgt
  | BvUge
  | BvSlt
  | BvSle
  | BvSgt
  | BvSge

type ax_comp = AxEqual | AxDistinct
type sort = BlSort | BvSort of int | AxSort of int * int
type bl_var = private { bl_hash : int; bl_name : string }
type bv_var = private { bv_hash : int; bv_name : string; bv_size : int }

type ax_var = private {
  ax_hash : int;
  ax_name : string;
  idx_size : int;
  elt_size : int;
}

type var = BlVar of bl_var | BvVar of bv_var | AxVar of ax_var

type term_desc = BlTerm of bl_term | BvTerm of bv_term | AxTerm of ax_term
and term = private { term_hash : int; term_desc : term_desc }

and bl_term_desc =
  | BlTrue
  | BlFalse
  | BlFun of bl_var * term list
  | BlLet of def list * bl_term
  | BlUnop of bl_unop * bl_term
  | BlBnop of bl_bnop * bl_term * bl_term
  | BlComp of bl_comp * bl_term * bl_term
  | BvComp of bv_comp * bv_term * bv_term
  | AxComp of ax_comp * ax_term * ax_term
  | BlIte of bl_term * bl_term * bl_term

and bl_term = private { bl_term_hash : int; bl_term_desc : bl_term_desc }

and bv_term_desc =
  | BvCst of Bitvector.t
  | BvFun of bv_var * term list
  | BvLet of def list * bv_term
  | BvUnop of bv_unop * bv_term
  | BvBnop of bv_bnop * bv_term * bv_term
  | BvIte of bl_term * bv_term * bv_term
  | Select of int * ax_term * bv_term
(* TODO: endianness *)

and bv_term = private {
  bv_term_hash : int;
  bv_term_desc : bv_term_desc;
  bv_term_size : int;
}

and ax_term_desc =
  | AxFun of ax_var * term list
  | AxLet of def list * ax_term
  | AxIte of bl_term * ax_term * ax_term
  | Store of int * ax_term * bv_term * bv_term
(* TODO: endianness *)

and ax_term = private {
  ax_term_hash : int;
  ax_term_desc : ax_term_desc;
  idx_term_size : int;
  elt_term_size : int;
}

and def_desc =
  | BlDef of bl_var * decl list * bl_term
  | BvDef of bv_var * decl list * bv_term
  | AxDef of ax_var * decl list * ax_term

and def = private { def_hash : int; def_desc : def_desc }

and decl_desc =
  | BlDecl of bl_var * sort list
  | BvDecl of bv_var * sort list
  | AxDecl of ax_var * sort list

and decl = private { decl_hash : int; decl_desc : decl_desc }

type entry_desc =
  | Declare of decl
  | Define of def
  | Assert of bl_term
  | Assume of bl_term
  | Comment of string

type entry = private { entry_hash : int; entry_desc : entry_desc }
type formula = private { entries : entry Sequence.t }

val empty : formula
val length : formula -> int
val append : formula -> formula -> formula
val push_front : entry -> formula -> formula
val push_back : entry -> formula -> formula
val peek_front : formula -> entry option
val peek_back : formula -> entry option
val pop_front : formula -> formula option
val pop_back : formula -> formula option
val map_forward : (entry -> entry) -> formula -> formula
val map_backward : (entry -> entry) -> formula -> formula
val iter_forward : (entry -> unit) -> formula -> unit
val iter_backward : (entry -> unit) -> formula -> unit
val fold_forward : (entry -> 'a -> 'a) -> formula -> 'a -> 'a
val fold_backward : (entry -> 'a -> 'a) -> formula -> 'a -> 'a
val push_front_declare : decl -> formula -> formula
val push_front_define : def -> formula -> formula
val push_front_assert : bl_term -> formula -> formula
val push_front_assume : bl_term -> formula -> formula
val push_front_comment : string -> formula -> formula
val push_back_declare : decl -> formula -> formula
val push_back_define : def -> formula -> formula
val push_back_assert : bl_term -> formula -> formula
val push_back_assume : bl_term -> formula -> formula
val push_back_comment : string -> formula -> formula

(** Basic printing of formulas *)
module Printing : sig
  val p_blterm : Format.formatter -> bl_term -> unit
  val p_bvterm : Format.formatter -> bv_term -> unit
  val p_axterm : Format.formatter -> ax_term -> unit
  val p_term : Format.formatter -> term -> unit
  val p_entry : Format.formatter -> entry -> unit
  val p_formula : Format.formatter -> formula -> unit
end

val equal_bl_term : bl_term -> bl_term -> bool
val equal_bv_term : bv_term -> bv_term -> bool
val equal_ax_term : ax_term -> ax_term -> bool

(* Smart constructors *)

val bl_sort : sort
val bv_sort : int -> sort
val ax_sort : int -> int -> sort
val bl_var : string -> bl_var
val bv_var : string -> int -> bv_var
val ax_var : string -> int -> int -> ax_var
val term : term_desc -> term
val bl_term : bl_term_desc -> bl_term
val bv_term : bv_term_desc -> bv_term
val ax_term : ax_term_desc -> ax_term
val def : def_desc -> def
val decl : decl_desc -> decl
val entry : entry_desc -> entry
val mk_bl_term : bl_term -> term
val mk_bv_term : bv_term -> term
val mk_ax_term : ax_term -> term
val mk_bl_true : bl_term
val mk_bl_false : bl_term
val mk_bl_var : bl_var -> bl_term
val mk_bl_fun : bl_var -> term list -> bl_term
val mk_bl_let : def list -> bl_term -> bl_term
val mk_bl_unop : bl_unop -> bl_term -> bl_term
val mk_bl_bnop : bl_bnop -> bl_term -> bl_term -> bl_term
val mk_bl_comp : bl_comp -> bl_term -> bl_term -> bl_term
val mk_bv_comp : bv_comp -> bv_term -> bv_term -> bl_term
val mk_ax_comp : ax_comp -> ax_term -> ax_term -> bl_term
val mk_bl_ite : bl_term -> bl_term -> bl_term -> bl_term
val mk_bv_cst : Bitvector.t -> bv_term
val mk_bv_var : bv_var -> bv_term
val mk_bv_fun : bv_var -> term list -> bv_term
val mk_bv_let : def list -> bv_term -> bv_term
val mk_bv_unop : bv_unop -> bv_term -> bv_term
val mk_bv_bnop : bv_bnop -> bv_term -> bv_term -> bv_term
val mk_bv_ite : bl_term -> bv_term -> bv_term -> bv_term
val mk_select : int -> ax_term -> bv_term -> bv_term
val mk_ax_var : ax_var -> ax_term
val mk_ax_fun : ax_var -> term list -> ax_term
val mk_ax_let : def list -> ax_term -> ax_term
val mk_ax_ite : bl_term -> ax_term -> ax_term -> ax_term
val mk_store : int -> ax_term -> bv_term -> bv_term -> ax_term
val mk_bl_def : bl_var -> decl list -> bl_term -> def
val mk_bv_def : bv_var -> decl list -> bv_term -> def
val mk_ax_def : ax_var -> decl list -> ax_term -> def
val mk_bl_decl : bl_var -> sort list -> decl
val mk_bv_decl : bv_var -> sort list -> decl
val mk_ax_decl : ax_var -> sort list -> decl
val mk_declare : decl -> entry
val mk_define : def -> entry
val mk_assert : bl_term -> entry
val mk_assume : bl_term -> entry
val mk_comment : string -> entry
val mk_bl_not : bl_term -> bl_term
val mk_bv_not : bv_term -> bv_term
val mk_bv_neg : bv_term -> bv_term
val mk_bv_repeat : int -> bv_term -> bv_term
val mk_bv_zero_extend : int -> bv_term -> bv_term
val mk_bv_sign_extend : int -> bv_term -> bv_term
val mk_bv_rotate_left : int -> bv_term -> bv_term
val mk_bv_rotate_right : int -> bv_term -> bv_term
val mk_bv_extract : int Interval.t -> bv_term -> bv_term
val mk_bl_imply : bl_term -> bl_term -> bl_term
val mk_bl_and : bl_term -> bl_term -> bl_term
val mk_bl_or : bl_term -> bl_term -> bl_term
val mk_bl_xor : bl_term -> bl_term -> bl_term
val mk_bv_concat : bv_term -> bv_term -> bv_term
val mk_bv_and : bv_term -> bv_term -> bv_term
val mk_bv_nand : bv_term -> bv_term -> bv_term
val mk_bv_or : bv_term -> bv_term -> bv_term
val mk_bv_nor : bv_term -> bv_term -> bv_term
val mk_bv_xor : bv_term -> bv_term -> bv_term
val mk_bv_xnor : bv_term -> bv_term -> bv_term
val mk_bv_cmp : bv_term -> bv_term -> bv_term
val mk_bv_add : bv_term -> bv_term -> bv_term
val mk_bv_sub : bv_term -> bv_term -> bv_term
val mk_bv_mul : bv_term -> bv_term -> bv_term
val mk_bv_udiv : bv_term -> bv_term -> bv_term
val mk_bv_sdiv : bv_term -> bv_term -> bv_term
val mk_bv_urem : bv_term -> bv_term -> bv_term
val mk_bv_srem : bv_term -> bv_term -> bv_term
val mk_bv_smod : bv_term -> bv_term -> bv_term
val mk_bv_shl : bv_term -> bv_term -> bv_term
val mk_bv_ashr : bv_term -> bv_term -> bv_term
val mk_bv_lshr : bv_term -> bv_term -> bv_term
val mk_bl_equal : bl_term -> bl_term -> bl_term
val mk_bl_distinct : bl_term -> bl_term -> bl_term
val mk_bv_equal : bv_term -> bv_term -> bl_term
val mk_bv_distinct : bv_term -> bv_term -> bl_term
val mk_ax_equal : ax_term -> ax_term -> bl_term
val mk_ax_distinct : ax_term -> ax_term -> bl_term
val mk_bv_ult : bv_term -> bv_term -> bl_term
val mk_bv_ule : bv_term -> bv_term -> bl_term
val mk_bv_ugt : bv_term -> bv_term -> bl_term
val mk_bv_uge : bv_term -> bv_term -> bl_term
val mk_bv_slt : bv_term -> bv_term -> bl_term
val mk_bv_sle : bv_term -> bv_term -> bl_term
val mk_bv_sgt : bv_term -> bv_term -> bl_term
val mk_bv_sge : bv_term -> bv_term -> bl_term
val mk_bv_zero : bv_term
val mk_bv_one : bv_term
val mk_bv_zeros : int -> bv_term
val mk_bv_ones : int -> bv_term
val mk_bv_fill : int -> bv_term
val mk_bv_add_int : bv_term -> int -> bv_term
val mk_bv_sub_int : bv_term -> int -> bv_term

module VarSet : Set.S with type elt = var
module BlVarSet : Set.S with type elt = bl_var
module BvVarSet : Set.S with type elt = bv_var
module AxVarSet : Set.S with type elt = ax_var
module BlVarHashtbl : Hashtbl.S with type key = bl_var
module BvVarHashtbl : Hashtbl.S with type key = bv_var
module AxVarHashtbl : Hashtbl.S with type key = ax_var
module BlTermHashtbl : Hashtbl.S with type key = bl_term
module BvTermHashtbl : Hashtbl.S with type key = bv_term
module AxTermHashtbl : Hashtbl.S with type key = ax_term
