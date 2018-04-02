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
open Instruction_piqi

let opcode_to_mnemonic opc =
  match opc with
  | "\x0f\xa2" -> `cpuid
  | _ -> `invalid_inst


let dispatch_instruction (_:instr_pol list) (instr:trace_inst) (_:Path_pred_env.t): unit =
  match  opcode_to_mnemonic instr.opcode with
  | `cpuid -> ()
  | `invalid_inst -> Logger.warning ~level:2 "Undecoded instr ignored (no policy for it)"
