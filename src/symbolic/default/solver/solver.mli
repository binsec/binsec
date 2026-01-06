(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

open Types
include module type of Session

type lazy_memory = {
  contents : Loader_types.buffer option Zmap.t AsMap.t;
  mutable lemmas : Expr.t list;
}

type result = Sat of Model.t | Unsat | Unknown
type cache

val empty_cache : unit -> cache
val clear_cache : cache -> unit

type mode = One_shot | Multi_checks of cache

val open_session :
  ?carbon_copy:(unit -> string) -> Smtlib.Solver.backend -> unit -> (module S)

val check_sat :
  (unit -> (module S)) ->
  mode ->
  ?timeout:float ->
  lazy_memory ->
  Expr.t list ->
  result

type enumeration

val enumerate_values :
  (unit -> (module S)) ->
  ?timeout:float ->
  lazy_memory ->
  Expr.t list ->
  Expr.t ->
  except:Bv.t list ->
  enumeration

val next_value : enumeration -> (Bv.t * Model.t) option
val release_enumeration : enumeration -> unit
