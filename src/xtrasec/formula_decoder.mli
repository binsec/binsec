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

(** DBA -> Formula decoder interface. *)

module Instr_to_Formula:sig
  include Generic_decoder_sig.Instr_Input
  (* Initial state: nothing is known about the memory or the registers. *)
  val initial_state: State.t

  (* Says that we do not know anything about the state, e.g. after the
     call to an unknown function. Could be refined using the ABI. *)
  val clear_memory: State.t -> State.t

  (* The path condition leading to this state. Note: this interface
     only works for single-path symbolic execution and is subject to
     change. *)
  val get_formula: State.t -> Formula.formula

end
