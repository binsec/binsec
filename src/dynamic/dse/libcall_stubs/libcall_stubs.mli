(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
(*    VERIMAG                                                             *)
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
(**************************************************************************)

val apply_libcall_policy :
  Libcall_piqi.libcall_pol list ->
  int -> Trace_type.trace_inst ->
  Common_piqi.call_convention_t ->
  Common_piqi.action ->
  Path_pred_env.t ->
  unit


val libcall_to_string :
  Libcall_piqi.libcall_t ->
  string

val check_libcall_policy_consistency :
  Libcall_piqi.libcall_pol list ->
  Common_piqi.action
  -> bool


val serialize_stack_params : Libcall_piqi.libcall_t -> string

val apply_default_stub : int -> Common_piqi.call_convention_t
  -> Path_pred_env.t -> unit
