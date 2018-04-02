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

open Taint_types
open Trace_type

(** DSE Tainting engine {b (deprecated)} *)

(** Class that hold the internal state of the taint.
    This class is {b deprecated} because uses old style
    data structure *)
class tainting_engine: taint_strategy ->
  object
  method compute_taint_instr:
    trace_inst -> TaintInfoSet.t list
  (** compute the taint on the given instruction *)

  method is_tainted:
    taint -> bool
  (** return [false] if taint is {!NoTaint} true otherwise *)

  method expr_to_taint:
    Dba.expr -> trace_concrete_infos list -> taint
   (** compute the taint on the given expression according to the
       current internal taint state *)
end