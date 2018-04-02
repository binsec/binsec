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

(** Generic utility functions for DBA simplification *)

val statistics :
  (Dba.instruction * 'a) Dba_types.Caddress.Map.t -> int * int * int * int

val display_results :
  (Dba.instruction * 'a) Dba_types.Caddress.Map.t ->
  Format.formatter -> float -> unit


val must_lhs_expr_equal : Dba.lhs -> Dba.expr -> bool

val lhs_mustkilled_by_lhs : Dba.lhs -> Dba.lhs -> bool
val lhs_mayused_in_expr : Dba.lhs -> Dba.expr -> bool
val lhs_mayused_in_lhs : Dba.lhs -> Dba.lhs -> bool
val lhs_mayused_in_bcond : Dba.lhs -> Dba.cond -> bool

val is_not_mayused :
  (Dba.instruction * 'a) Dba_types.Caddress.Map.t ->
  Dba_types.Caddress.Map.key -> int -> Dba.lhs ->
  bool Basic_types.String.Map.t Dba_types.Caddress.Map.t ->
  bool Basic_types.String.Map.t Dba_types.Caddress.Map.t * bool
