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

open Trace_type
open Common_piqi
open Smtlib2

module type CallConvention =
sig
  val get_param: int -> Path_pred_env.t -> Dba.expr
  val set_param: int -> string -> int64 -> Common_piqi.action -> conc_infos -> Common_piqi.action -> Path_pred_env.t -> Dba.expr option
  val set_param_pointer: int -> string -> memory_pol -> memory_t -> conc_infos -> Common_piqi.action -> Path_pred_env.t -> Dba.expr option
  val set_ret: string -> int64 -> ?supp:int64 option -> Common_piqi.action -> conc_infos -> Common_piqi.action -> Path_pred_env.t -> unit                   
  val set_epilog: int -> string -> conc_infos -> Path_pred_env.t -> unit
  val add_alias: int -> string -> conc_infos -> Path_pred_env.t -> smt_bv_expr
  val default_stub: string -> Path_pred_env.t -> unit
end

module Cdecl: CallConvention

module Stdcall: CallConvention