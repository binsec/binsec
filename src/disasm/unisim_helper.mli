(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2025                                               *)
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

type error = Undefined | Unimplemented | Unsupported | Failure

module Opcode = Basic_types.String

module Statistics : sig
  type t = {
    decoded : int Opcode.Htbl.t;
    undefined : int Opcode.Htbl.t;
    unimplemented : int Opcode.Htbl.t;
    unsupported : int Opcode.Htbl.t;
    failure : int Opcode.Htbl.t;
  }

  include Sigs.PRINTABLE with type t := t
end

val empty_instruction : Instruction.Generic.t
val die : Dhunk.t

module Make (L : Logger.S) : sig
  val parse_message : string -> Instruction.Generic.t * Dhunk.t * error option
  val incr_success : Opcode.t -> unit
  val incr_error : error -> Opcode.t -> unit
  val pp_statistics : Format.formatter -> unit -> unit
end
