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

val assign_to_f :
  Dba.LValue.t -> Dba.Expr.t -> int Basic_types.String.Map.t ->
  (int * int * Formula.bv_term) Basic_types.String.Map.t ->
  Formula.VarSet.t ->
  Formula.def * (int * int * Formula.bv_term) Basic_types.String.Map.t
  * Formula.VarSet.t * int Basic_types.String.Map.t

val cond_to_f :
  condition:Dba.Expr.t -> Formula.VarSet.t ->
  int Basic_types.String.Map.t -> Formula.bv_term * Formula.VarSet.t


val load_to_smt :
  Dba.Expr.t -> Dba.size -> Machine.endianness -> Formula.VarSet.t ->
  int Basic_types.String.Map.t -> Formula.bv_term * Formula.VarSet.t


val apply_smt_elements_recovery :
  Formula.def list -> Formula.bl_term list ->
  Formula.VarSet.t -> string -> int Basic_types.String.Map.t -> Region_bitvector.t list

val is_sat :
  Formula.def list * Formula.bl_term list *
  Formula.VarSet.t * string * int Basic_types.String.Map.t -> string -> bool

val get_upper_bound :
  Formula.def list * Formula.bl_term list *
  Formula.VarSet.t * 'a * int Basic_types.String.Map.t ->
  string -> Bitvector.t -> Bitvector.t

val get_lower_bound :
  Formula.def list * Formula.bl_term list *
  Formula.VarSet.t * 'a * int Basic_types.String.Map.t ->
  string -> Bitvector.t -> Bitvector.t
