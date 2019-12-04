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

(** Utility functions for formula creation *)

open Formula

type stats = {
  var : int; cst : int;
  unop : int; bnop : int; comp : int;
  select : int; store : int;
}
;;

val bl_term_stats : bl_term -> stats
val bv_term_stats : bv_term -> stats
val ax_term_stats : ax_term -> stats

val bl_term_variables : bl_term -> VarSet.t
val bv_term_variables : bv_term -> VarSet.t
val ax_term_variables : ax_term -> VarSet.t

(** @return [true] if the expression is syntactically symbolic
    meaning there is at least one variable *)
val is_symbolic_bl_term : bl_term -> bool
val is_symbolic_bv_term : bv_term -> bool
val is_symbolic_ax_term : ax_term -> bool

(* Some accessors *)

val bv_size : bv_term -> int
val ax_size : ax_term -> int * int (* idx_size, elt_size *)

val bv_desc_size : bv_term_desc -> int
val ax_desc_size : ax_term_desc -> int * int (* idx_size, elt_size *)

val is_bl_desc_cst : bl_term_desc -> bool option
val is_bv_desc_cst : bv_term_desc -> Bitvector.t option

val is_bl_cst : bl_term -> bool option
val is_bv_cst : bv_term -> Bitvector.t option

val is_bl_desc_var : bl_term_desc -> bl_var option
val is_bv_desc_var : bv_term_desc -> bv_var option
val is_ax_desc_var : ax_term_desc -> ax_var option

val is_bl_var : bl_term -> bl_var option
val is_bv_var : bv_term -> bv_var option
val is_ax_var : ax_term -> ax_var option

val is_select : bv_term -> (int * ax_term * bv_term) option
val is_store  : ax_term -> (int * ax_term * bv_term * bv_term) option

val var_hash : var -> int

val bl_var_hash : bl_var -> int
val bv_var_hash : bv_var -> int
val ax_var_hash : ax_var -> int

val var_name : var -> string

val bl_var_name : bl_var -> string
val bv_var_name : bv_var -> string
val ax_var_name : ax_var -> string

val bl_term_desc_name : bl_term_desc -> string option
val bv_term_desc_name : bv_term_desc -> string option
val ax_term_desc_name : ax_term_desc -> string option

val bl_term_name : bl_term -> string option
val bv_term_name : bv_term -> string option
val ax_term_name : ax_term -> string option

val decl_desc_name : decl_desc -> string
val def_desc_name  : def_desc  -> string

val decl_name : decl -> string
val def_name  : def  -> string

module BindEnv :
sig

  type t

  type ('var, 'term) status =
    | Free
    | Declared of ('var * sort list)
    | Defined  of ('var * decl list * 'term)

  val create : int -> t

  val decl : t -> decl -> unit
  val def  : t -> def  -> unit

  val undecl : t -> decl -> unit
  val undef  : t -> def  -> unit

  val bl_lookup : t -> bl_var -> (bl_var, bl_term) status
  val bv_lookup : t -> bv_var -> (bv_var, bv_term) status
  val ax_lookup : t -> ax_var -> (ax_var, ax_term) status

  val is_bl_cst : t -> bl_term -> bool option
  val is_bv_cst : t -> bv_term -> Bitvector.t option

  val is_bl_var : t -> bl_term -> bl_var option
  val is_bv_var : t -> bv_term -> bv_var option
  val is_ax_var : t -> ax_term -> ax_var option
end

