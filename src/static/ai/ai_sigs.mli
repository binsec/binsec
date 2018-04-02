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

(** Domain signatures *)


type globals = Dba_types.Caddress.Set.t
                       
module type AbstractDomain =
sig
  type t
  type equalities
  type thresholds = int array * int array * int array * int array
  type elementsRecord = Region_bitvector.t list Dba_types.AddressStack.Map.t
  type naturalPredicatesRecord = (Dba.cond * Dba.cond) Dba_types.Caddress.Map.t

  (** [get_initial_state] *)
  val get_initial_state: Dba.instruction list -> t

  (** [top] *)
  val top: (t * High_level_predicate.t * equalities)

  (** [bottom] *)
  val bottom: (t * High_level_predicate.t * equalities)

  (** [is_empty] *)
  val is_empty: t -> bool

  (** [leq s1 s2] is true if the s2 allows more semantic than s1. *)
  val leq: t -> t -> bool

  val join:
    (t * High_level_predicate.t * equalities) ->
    (t * High_level_predicate.t * equalities) ->
    (t * High_level_predicate.t * equalities)

  val widen:
    (t * High_level_predicate.t * equalities) ->
    (t * High_level_predicate.t * equalities) ->
    thresholds ->
    (t * High_level_predicate.t * equalities)

  val meet: t -> t -> t

  (** [to_string s] prints the informations of the state *)
  val to_string: (t * equalities) -> string
  val pp : Format.formatter -> t -> unit
  val pp_equalities : Format.formatter -> equalities -> unit


  val regs_in_expr_to_string:
    Dba.expr -> Format.formatter -> t * High_level_predicate.t * equalities -> unit

 (** [post addr instr m] *)
  val post :
    (t * High_level_predicate.t * equalities) ->
    Dba_types.AddressStack.Map.key ->
    Dba.instruction ->
    (elementsRecord * naturalPredicatesRecord) ->
    Smt_bitvectors.smtBvExprAlt list ->
    Dba_types.Caddress.Set.t ->
    Dba_types.Caddress.Set.t Dba_types.AddressStack.Map.t ->
    Dba_types.Caddress.Set.t Dba_types.Caddress.Map.t ->
    Region_bitvector.t list ->
    ((Dba_types.AddressStack.Map.key * t * High_level_predicate.t * equalities) list) *
      (elementsRecord * naturalPredicatesRecord) *
      (Smt_bitvectors.smtBvExprAlt list) *
      (Dba_types.Caddress.Set.t Dba_types.AddressStack.Map.t)

  val env_to_smt_list:
    t ->
    int Basic_types.String.Map.t ->
    Smtlib2.SmtVarSet.t ->
    Smtlib2.smt_bv_expr list * Smtlib2.SmtVarSet.t

  val refine_state: t ->
    ((Smtlib2.smt_expr * Smtlib2.smt_expr) list *
        Smtlib2.smt_bv_expr list * Smtlib2.SmtVarSet.t * string *
        int Basic_types.String.Map.t) ->
    t

end


module type Domain =
sig
  type t

  exception Elements_of_top

  val universe: t
  val empty : t
  val is_empty : t -> bool
  val singleton: Region_bitvector.t -> t
  val of_bounds: (Region_bitvector.t * Region_bitvector.t) -> t
  val join: t -> t -> t
  val widen: t -> t -> (int array * int array * int array * int array) -> t
  val meet: t -> t -> t
  val contains: t -> t -> bool
  val neg: t -> t
  val lognot: t -> t
  val add: t -> t -> t
  val sub: t -> t -> t
  val umul: t -> t -> t
  val smul: t -> t -> t
  val power: t -> t -> t
  val udiv: t -> t -> t
  val sdiv: t -> t -> t
  val umod: t -> t -> t
  val smod: t -> t -> t
  val logor: t -> t -> t
  val logand: t -> t -> t
  val logxor: t -> t -> t
  val lshift: t -> t -> t
  val rshiftU: t -> t -> t
  val rshiftS: t -> t -> t
  val rotate_left: t -> t -> t
  val rotate_right: t -> t -> t
  val eq: t -> t -> t
  val diff: t -> t -> t
  val leqU: t -> t -> t
  val ltU: t -> t -> t
  val geqU: t -> t -> t
  val gtU: t -> t -> t
  val leqS: t -> t -> t
  val ltS: t -> t -> t
  val geqS: t -> t -> t
  val gtS: t -> t -> t
  val extension: t -> int -> t
  val signed_extension: t -> int -> t
  val equal: t -> t -> bool
  val is_true: t -> Smt_bitvectors.condition_env -> globals -> Basic_types.Ternary.t
  val guard: Dba.binary_op -> t -> t -> t * t
  val pp : Format.formatter -> t -> unit
  val to_string: t -> string
  val max : t -> Region_bitvector.t
  val elements: t -> Region_bitvector.t list
  val restrict: t -> int -> int -> t
  val concat: t -> t -> t
  val to_smt: t -> Smtlib2.smt_bv_expr -> Smtlib2.smt_bv_expr list
  val smt_refine: t -> ((Smtlib2.smt_expr * Smtlib2.smt_expr) list *
                             Smtlib2.smt_bv_expr list * Smtlib2.SmtVarSet.t * string *
                             int Basic_types.String.Map.t) -> string -> t

end
