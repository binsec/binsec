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

(** LLVM decoder interface *)

(* Represents the place where instructions are placed. *)
type state

module Instr_to_LLVM:Generic_decoder_sig.Instr_Input with type State.t = state


(* Create a new module and a void -> void function, and generate code
   inside of this function. Returns the corresponding module (e.g. for
   printing). *)
val generate_in_function:
  modname:string -> funname:string ->
  (state -> state) -> Llvm.llmodule
;;
  
val pretty: Format.formatter -> Dhunk.t -> unit


