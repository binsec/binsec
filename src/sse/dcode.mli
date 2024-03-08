(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
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

module Make
    (Stats : Types.EXPLORATION_STATISTICS_FULL)
    (Path : Path.S)
    (State : Types.STATE) : sig
  module Fiber :
    Fiber.S
      with type builtin :=
        Virtual_address.t ->
        Path.t ->
        int ->
        State.t ->
        (State.t, Types.status) Result.t

  type t

  val single :
    ?hooks:(string * Script.Instr.t list) list * Script.env ->
    task:unit Basic_types.Int.Htbl.t ->
    Virtual_address.t ->
    Lreader.t ->
    int ->
    [ `All ] Fiber.t * Instruction.t option

  val script :
    task:unit Basic_types.Int.Htbl.t ->
    Virtual_address.t ->
    ?fallthrough:bool ->
    Script.Instr.t list ->
    Script.env ->
    [ `All ] Fiber.t

  val create :
    ?volatile:bool ->
    ?hooks:
      (string * Script.Instr.t list) list Virtual_address.Map.t * Script.env ->
    task:unit Basic_types.Int.Htbl.t ->
    Virtual_address.t ->
    Lreader.t ->
    int ->
    t

  val get : t -> Virtual_address.t -> [ `All ] Fiber.t

  module type CALLBACK = sig
    val instruction_callback :
      (Ast.Instr.t -> Script.env -> Ir.fallthrough list) option

    val process_callback :
      ((module Ir.GRAPH with type t = 'a) -> 'a -> unit) option

    val builtin_callback :
      (Ir.builtin ->
      (Virtual_address.t ->
      Path.t ->
      int ->
      State.t ->
      (State.t, Types.status) Result.t)
      option)
      option
  end

  val register_callback : (module CALLBACK) -> unit

  val register_opcode_hook :
    (Lreader.t -> (Script.Instr.t list * Script.env) option) -> unit
end
