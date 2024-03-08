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

module type EXTENSION = sig
  type path
  and state

  val initialization_callback : (path -> state -> state) option

  val declaration_callback :
    (Ast.t -> Script.env -> path -> state -> state option) option

  val instruction_callback :
    (Ast.Instr.t -> Script.env -> Ir.fallthrough list) option

  val process_callback :
    ((module Ir.GRAPH with type t = 'a) -> 'a -> unit) option

  val builtin_callback :
    (Ir.builtin ->
    (Virtual_address.t ->
    path ->
    int ->
    state ->
    (state, Types.status) Result.t)
    option)
    option

  val builtin_printer : (Format.formatter -> Ir.builtin -> bool) option
  val at_exit_callback : (unit -> unit) option
end

module type PLUGIN = sig
  val name : string

  val grammar_extension :
    ( unit,
      Libparser.obj,
      unit,
      unit,
      Libparser.obj Dyp.dyplexbuf )
    Dyp.dyp_action
    list

  val instruction_printer : (Format.formatter -> Ast.Instr.t -> bool) option
  val declaration_printer : (Format.formatter -> Ast.t -> bool) option

  val extension :
    (module Types.EXPLORATION_STATISTICS) ->
    (module Path.S with type t = 'a) ->
    (module Types.STATE with type t = 'b) ->
    (module EXTENSION with type path = 'a and type state = 'b) option
end

val register_plugin : (module PLUGIN) -> unit

module Run (SF : Types.STATE_FACTORY) (W : Types.WORKLIST) () : sig
  val unit : unit
end
