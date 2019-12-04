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

open Formula

val rename_bl_var : (string -> string) -> bl_var -> bl_var
val rename_bv_var : (string -> string) -> bv_var -> bv_var
val rename_ax_var : (string -> string) -> ax_var -> ax_var

val rename_bl_term : (string -> string) -> bl_term -> bl_term
val rename_bv_term : (string -> string) -> bv_term -> bv_term
val rename_ax_term : (string -> string) -> ax_term -> ax_term

val replace_bl_term : def -> bl_term -> bl_term
val replace_bv_term : def -> bv_term -> bv_term
val replace_ax_term : def -> ax_term -> ax_term

val constant_propagation : ?keep:VarSet.t -> formula -> formula
val prune_and_inline     : ?keep:VarSet.t -> formula -> formula
val read_over_write : ?lst:int -> ?rbs:bool -> ?itv:bool -> formula -> formula
val static_single_assignment : formula -> formula

val taint : (var -> bool) -> formula -> formula

val optimize :
  ?keep:VarSet.t ->
  ?lst:int -> ?cst:bool -> ?itv:bool -> ?prn:bool -> ?rbs:bool -> ?row:bool ->
  ?ssa:bool ->
  ?is_controlled:(VarSet.elt -> bool) ->
  formula -> formula

val optimize_from_options :
  ?keep:VarSet.t -> ?is_controlled:(VarSet.elt -> bool) -> formula -> formula
