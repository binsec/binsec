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

val assign_to_f : Dba.lhs -> Dba.expr -> int Basic_types.String.Map.t
  -> (int * int * Smtlib2.smt_bv_expr) Basic_types.String.Map.t -> Smtlib2.SmtVarSet.t
  -> (Smtlib2.smt_expr * Smtlib2.smt_expr) * (int * int * Smtlib2.smt_bv_expr) Basic_types.String.Map.t * Smtlib2.SmtVarSet.t * int Basic_types.String.Map.t

val cond_to_f :
  Dba.cond -> Smtlib2.SmtVarSet.t
  -> int Basic_types.String.Map.t -> Smtlib2.smt_bv_expr * Smtlib2.SmtVarSet.t


val load_to_smt :
  Dba.expr -> Dba.size -> Dba.endianness -> Smtlib2.SmtVarSet.t ->
  int Basic_types.String.Map.t -> Smtlib2.smt_bv_expr * Smtlib2.SmtVarSet.t


val apply_smt_elements_recovery :
  (Smtlib2.smt_expr * Smtlib2.smt_expr) list -> Smtlib2.smt_bv_expr list ->
  Smtlib2.SmtVarSet.t -> string -> int Basic_types.String.Map.t -> Region_bitvector.t list

val is_sat : (Smtlib2.smt_expr * Smtlib2.smt_expr) list * Smtlib2.smt_bv_expr list *
Smtlib2.SmtVarSet.t * string * int Basic_types.String.Map.t -> string -> bool

val get_upper_bound : (Smtlib2.smt_expr * Smtlib2.smt_expr) list * Smtlib2.smt_bv_expr list *
Smtlib2.SmtVarSet.t * 'a * int Basic_types.String.Map.t ->
  string -> Bitvector.t -> Bitvector.t

val get_lower_bound : (Smtlib2.smt_expr * Smtlib2.smt_expr) list * Smtlib2.smt_bv_expr list *
Smtlib2.SmtVarSet.t * 'a * int Basic_types.String.Map.t ->
string -> Bitvector.t -> Bitvector.t
