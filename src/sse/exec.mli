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

module type EXTENSION = sig
  type path
  and state

  val initialization_callback : (path -> state -> state) option
  (** [let initialization_callback = Some f]
      registers the function [f] to be called to initialize the initial
      [state].

      It can be used to set the initial value of some [Dba] variables
      or declare new field in the [path] structure.

      The function [f] is called before any instruction in the script.
      It will be called once and it is the only safe place to call
      [Path.register_key].
  *)

  val declaration_callback :
    (Ast.t -> Script.env -> path -> state -> state option) option
  (** [let declaration_callback = Some f]
      registers the handler [f] to perform new top level script commands
      ([Ast.t]).

      The handler [f] can use the parser environment ([Script.env]) to
      parse and resolve script expressions with [Script.eval_loc] and
      [Script.eval_expr].

      The handler [f] should return an [Some state] for the script commands
      it supports, [None] otherwise.
  *)

  val instruction_callback :
    (Ast.Instr.t -> Script.env -> Ir.fallthrough list) option
  (** [let instruction_callback = Some f]
      registers the handler [f] to translate the new script instructions
      ([Ast.Instr.t]) to one or several micro-instruction ([Ir]),
      including new [builtin]s.

      The handler [f] can use the parser environment ([Script.env]) to
      parse and resolve script expressions with [Script.eval_loc] and
      [Script.eval_expr].

      The handler [f] should return an non empty instruction [list]
      for the script instructions it supports, [\[\]] otherwise.
  *)

  val process_callback :
    ((module Ir.GRAPH with type t = 'a) -> 'a -> unit) option
  (** [let process_callback = Some f]
      registers the funcion [f] to instrument the intermediate
      representation ([Ir]).

      It can be used to automatically insert new micro-instructions,
      including new [builtins] at strategic points (e.g. memory access,
      branch conditions, etc.).

      The function [f] is called each time the engine discover new
      assembly instructions. The function [f] can explore the new
      micro-instructions by using the [Ir.GRAPH] interface.
      It can add new micro-instructions with the functions
      [insert_before] and [insert_list_before].

      The function should only modify the newly disassembled part of
      the graph ([is_new_vertex], [iter_new_vertex]).
      Inserting new instructions in an already visited part will result
      in an error.

      The function should not insert instructions before an [Instruction]
      label. To instrument the [Instruction] labels, use the [insert_before]
      or [insert_list_before] function on the successor of the node.
  *)

  val builtin_callback :
    (Ir.builtin ->
    (Virtual_address.t ->
    path ->
    int ->
    state ->
    (state, Types.status) Result.t)
    option)
    option
  (** [let builtin_callback = Some f]
      registers the function [f] in charge to resolve the handler of a
      given [builtin].

      The resolver [f] should return [Some h] for the [builtin]s it
      supports, [None] otherwise.
      It is called once per [builtin] occurrence in the intermediate
      representation ([Ir]).

      It is an error to not handle a [builtin] added by the plugin.

      The handler [h] has access to:
      - the current program counter ([Virtual_address]);
      - the per-path data ([Path]), including registered ones;
      - the current instruction depth of the path ([int]);
      - the current symbolic state ([State]).

      It should return [Ok state] to continue the exploration along
      this path. [Error status] cuts the path and logs the given status.
      The special value [Error Halt] terminates the whole exploration.

      The handler [h] is called each time the path reaches the given
      [builtin].
  *)

  val builtin_printer : (Format.formatter -> Ir.builtin -> bool) option
  (** [let builtin_printer = Some f]
      registers the custom printer [f] to format the new [builtin]s
      in debug outputs.

      The printer [f] should return [true] for the [builtin]s it
      handles, [false] otherwise.
  *)

  val at_exit_callback : (unit -> unit) option
  (** [let at_exit_callback = Some f]
      registers the function [f] to be called when the exploration
      is about to terminate.

      It can be used to output some global wise statistics or result.
  *)
end

module type PLUGIN = sig
  val name : string
  (** The name of the {b plugin}.

      It should be unique.
  *)

  val grammar_extension :
    ( unit,
      Libparser.obj,
      unit,
      unit,
      Libparser.obj Dyp.dyplexbuf )
    Dyp.dyp_action
    list
  (** A list of {b dypgen} grammar rules ([Dyp.dyp_action]). *)

  val instruction_printer : (Format.formatter -> Ast.Instr.t -> bool) option
  (** [let instruction_printer = Some f]
      registers the custom printer [f] to format the new script
      instruction ([Ast.Instr.t]) in debug outputs.

      The printer [f] should return [true] for the instructions it
      handles, [false] otherwise.
  *)

  val declaration_printer : (Format.formatter -> Ast.t -> bool) option
  (** [let instruction_printer = Some f]
      registers the custom printer [f] to format the new top level script
      commands ([Ast.t]) in debug outputs.

      The printer [f] should return [true] for the instructions it
      handles, [false] otherwise.
  *)

  val extension :
    (module Types.EXPLORATION_STATISTICS) ->
    (module Path.S with type t = 'a) ->
    (module Types.STATE with type t = 'b) ->
    (module EXTENSION with type path = 'a and type state = 'b) option
  (** [extension stats path state]
      should returns [Some (module E : EXTENSION)] to activate the {b plugin}.

      This function is called once per symbolic execution [Run].
  *)
end

val register_plugin : (module PLUGIN) -> unit

module Run (SF : Types.STATE_FACTORY) (W : Types.WORKLIST) () : sig
  val unit : unit
end
