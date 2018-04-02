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

(** Instruction stubs (DSE) *)

(** This module provides logical stubs for not decoded
    instruction. Indeed, while it might be hard to
    provide an exact semantic decoding of an instruction
    it might be handy to provided a stub to concretize
    a given value or to approximate it by injecting a new
    symbol into.
*)

(** convert the binary string of the opcode to a variant
    of type {!Instruction_piqi.instr_ident} *)
val opcode_to_mnemonic : string -> Instruction_piqi.instr_ident

(** Dispatch an instruction ident to the right symbolic
    stub *)
val dispatch_instruction : Instruction_piqi.instr_pol list ->
  Trace_type.trace_inst -> Path_pred_env.t -> unit
