(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

type trilean = Basic_types.Ternary.t = False | True | Unknown

(** The reason a path execution ends (via
    {!constructor-Signal}). *)
type status = Metrics.status =
  | Halt
  | Cut
  | Merged
  | Stashed
  | Unsatisfiable_assumption
  | Assertion_failure
  | Max_depth
  | Enumeration_limit
  | Unresolved_formula
  | Non_executable_code
  | Error of string

(** The return of a {!module-Ir.builtin} {!constructor-Call}. *)
type 'path continuation =
  | Continue of ([ `All ], 'path) fiber
      (** Reroutes the execution to the given low-level instructions. *)
  | Call of ('path -> 'path continuation) * 'path continuation
      (** Call the given OCaml function. Then, when the function yields
          {!constructor-Return}, evaluates the given continuation. *)
  | Tail_call of ('path -> 'path continuation)
      (** Call the given OCaml function without altering the callstack. *)
  | Return  (** Returns from a {!constructor-Call}. *)
  | Return_to of ([ `All ], 'path) fiber
      (** Return from a {!constructor-Call} but reroutes the execution
          to the given low-level instruction instead of the
          {!constructor-Call} continuation. *)
  | Signal of status
      (** Ends the path execution.

          If the status is {!constructor-Stashed}, the excecution
          can be resumed with the function
          {!module-type-ENGINE.val-resume} or a {!constructor-Merge}
          request. *)
  | Fork of 'path continuation * 'path continuation
      (** Request for a path fork. The parent path will evaluate the
          first continuation while the child path will evaluate the
          second one. *)
  | Merge of 'path list * 'path continuation
      (** Request for a path merging. The symbolic engine will try
          to merge the current path with the given ones.

          All the listed paths must have been paused
          with {!constructor-Signal} {!constructor-Stashed}.

          The merge result will evaluate the given continuation.
          If a path can not be merged, it will evaluate the given
          continuation too. *)

and (_, 'path) fiber =
  | Step : {
      addr : Virtual_address.t;
      n : int;
      mutable succ : ([ `All ], 'path) fiber;
    }
      -> ([< `Step | `All ], 'path) fiber
  | Assign : {
      var : Dba.Var.t;
      rval : Dba.Expr.t;
      mutable succ : ([ `All ], 'path) fiber;
    }
      -> ([< `Assign | `All ], 'path) fiber
  | Clobber : {
      var : Dba.Var.t;
      mutable succ : ([ `All ], 'path) fiber;
    }
      -> ([< `Clobber | `All ], 'path) fiber
  | Load : {
      var : Dba.Var.t;
      base : string option;
      dir : Machine.endianness;
      addr : Dba.Expr.t;
      mutable succ : ([ `All ], 'path) fiber;
    }
      -> ([< `Load | `All ], 'path) fiber
  | Store : {
      base : string option;
      dir : Machine.endianness;
      addr : Dba.Expr.t;
      rval : Dba.Expr.t;
      mutable succ : ([ `All ], 'path) fiber;
    }
      -> ([< `Store | `All ], 'path) fiber
  | Symbolize : {
      var : Dba.Var.t;
      mutable succ : ([ `All ], 'path) fiber;
    }
      -> ([< `Symbolize | `All ], 'path) fiber
  | Apply : {
      f : 'path -> unit;
      mutable succ : ([ `All ], 'path) fiber;
    }
      -> ([< `Apply | `All ], 'path) fiber
  | Assume : {
      test : Dba.Expr.t;
      mutable succ : ([ `All ], 'path) fiber;
    }
      -> ([< `Assume | `All ], 'path) fiber
  | Assert : {
      test : Dba.Expr.t;
      mutable succ : ([ `All ], 'path) fiber;
    }
      -> ([< `Assert | `All ], 'path) fiber
  | Branch : {
      test : Dba.Expr.t;
      mutable taken : ([ `All ], 'path) fiber;
      mutable fallthrough : ([ `All ], 'path) fiber;
    }
      -> ([< `Branch | `All ], 'path) fiber
  | Goto : Virtual_address.t -> ([< `Goto | `All ], 'path) fiber
  | Jump : Dba.Expr.t -> ([< `Jump | `All ], 'path) fiber
  | Call : {
      f : 'path -> 'path continuation;
      mutable succ : ([ `All ], 'path) fiber;
    }
      -> ([< `Call | `All ], 'path) fiber
  | Tail_call :
      ('path -> 'path continuation)
      -> ([< `Tail_call | `All ], 'path) fiber

module type STATE = Symbolic.State.S
module type PATH = Symbolic.Path.S

type stage = Binsec_sse_loader.Disassembly.stage = Early | Late

type 'a hook = {
  scope : Virtual_address.t Interval.t option;
  stage : stage;
  callback : 'a;
}
(** A hook declaration as used by the {!type-extension}s.

    The hooks are evaluated in the following phase order:
    {!constructor-Fetch_hook}, {!constructor-Decode_hook},
    {!constructor-Disasm_hook} and {!constructor-Rewrite_hook}.

    Within the same phase, {!constructor-Early} stage hooks
    are evaluated before {!constructor-Late} stage hooks.

    Within the same stage, hooks are evaluated in the order
    of insertion.

    A hook is only active under a given [scope] (i.e range of
    addresses or [None] for {i always}). *)

(** Return value of the {!constructor-Builtin_resolver} callback. *)
type 'a primitive =
  | Unknown
  | Apply of ('a -> unit)
  | Call of ('a -> 'a continuation)

type 'path extension =
  | Initialization_callback of ('path -> unit)
      (** Registers the function [f] to be called during initialization of the first [path].

          It can be used to set the initial value of some [Dba] variables in the state
          or initialize some fields in the [path] structure.

          The function [f] is called only once and before any instruction in the script. *)
  | Fetch_hook of (Virtual_address.t -> Ir.Graph.t option) hook
      (** Registers the {!type-hook} [h] to be called each time a new virtual address
          in the [scope] is discovered.

          Given the virtual address, the [callback] can return a new instruction graph
          ({!module-Ir.module-Graph.type-t}) to hook this address.

          If the graph ends with the {!module-Ir.constructor-EndOfHook} builtin,
          the disassembly continues to proceed with the following hooks.
          Otherwise, the hook acts as a replacement and skips the subsequent ones.

          The hooks of the same stage are evaluated in the same order they are inserted. *)
  | Decode_hook of (Virtual_address.t -> int Reader.t -> Ir.Graph.t option) hook
      (** Registers the {!type-hook} [h] to be called each time a new virtual address
          in the [scope] is about to be decoded (after all fetch hooks).

          Given the virtual address and the byte opcode ({!module-Reader.type-t}),
          the [callback] can return a new instruction graph ({!module-Ir.module-Graph.type-t})
          to hook this address.

          If the graph ends with the {!module-Ir.constructor-EndOfHook} builtin,
          the disassembly continues to proceed with the following hooks.
          Otherwise, the hook acts as a replacement and skips the subsequent ones.

          The hooks of the same stage are evaluated in the same order they are inserted. *)
  | Disasm_hook of (Instruction.t -> Ir.Graph.t option) hook
      (** Registers the {!type-hook} [h] to be called each time a new virtual address
          in the [scope] has been decoded (after all decode hooks).

          Given the decoded instruction ({!module-Instruction.type-t}),
          the [callback] can return a new instruction graph ({!module-Ir.module-Graph.type-t})
          to hook this address.

          If the graph ends with the {!module-Ir.constructor-EndOfHook} builtin,
          the disassembly continues to proceed with the following hooks.
          Otherwise, the hook acts as a replacement and skips the subsequent ones.

          The hooks of the same stage are evaluated in the same order they are inserted. *)
  | Rewrite_hook of (Ir.Graph.t -> unit) hook
      (** Registers the {!type-hook} [h] to be called each time a new virtual address
          in the [scope] is about to yield a instruction graph
          ({!module-Ir.module-Graph.type-t}) (after all disasm hooks).

          Given the instruction graph, the [callback] can modify it in place using the
          {!module-Ir.module-Graph} interface (e.g.
          {!module-Ir.module-Graph.val-insert_before},
          {!module-Ir.module-Graph.val-insert_list_before},
          {!module-Ir.module-Graph.val-replace_node} or
          {!module-Ir.module-Graph.val-append_node}).

          The hooks of the same stage are evaluated in the same order they are inserted. *)
  | Instrumentation_routine of (Revision.t -> unit)
      (** Registers the funcion [f] to instrument the intermediate representation
          ({!module-Ir}).

          It can be used to automatically insert new micro-instructions, including
          custom {!module-Ir.type-builtin}s at strategic points (e.g. memory access,
          branch conditions, etc.).

          The function [f] is called each time the engine discover new assembly
          instructions. The function [f] can explore the new micro-instructions by
          using the {!module-Revision} interface. It can add new
          micro-instructions with the functions
          {!module-Revision.val-insert_before} and
          {!module-Revision.val-insert_list_before}.

          The function should only modify the newly disassembled part of the graph
          ({!module-Revision.val-is_new_vertex},
          {!module-Revision.val-iter_new_vertex}).
          Inserting instructions in a previously visited part will raise
          {!exception-Invalid_argument}.

          The function should not insert instructions before an
          {!module-Ir.constructor-Instruction} label.
          To instrument the {!module-Ir.constructor-Instruction} labels, use the
          {!module-Revision.val-insert_before} or
          {!module-Revision.val-insert_list_before} function on
          the successor of the node. *)
  | Grammar_extension of
      ( unit,
        Binsec_script.obj,
        unit,
        unit,
        Binsec_script.obj Dyp.dyplexbuf )
      Dyp.dyp_action
      list
      (** A list of {b dypgen} grammar rules ({!module-Dyp.val-dyp_action}). *)
  | Instruction_resolver of
      (Script.Ast.Instr.t -> Script.env -> Ir.fallthrough list)
      (** Registers the handler [f] to translate custom script instructions
          ({!module-Script.module-Ast.module-Instr.type-t}) to one or several
          micro-instruction ({!module-Ir.type-fallthrough}), including custom
          {!module-Ir.type-builtin}s.

          The handler [f] can use the parser environment ({!module-Script.type-env})
          to parse and resolve script expressions with {!module-Script.val-eval_loc}
          and {!module-Script.val-eval_expr}.

          The handler [f] should return a non empty instruction [list] for the
          script instructions it supports, [[]] otherwise. *)
  | Instruction_printer of (Format.formatter -> Script.Ast.Instr.t -> bool)
      (** Registers the custom printer [f] to format the new script instruction
          ({!module-Script.module-Ast.module-Instr.type-t}) in debug outputs.

          The printer [f] should return [true] for the instructions it handles,
          [false] otherwise. *)
  | Command_handler of (Script.Ast.t -> Script.env -> 'path -> bool)
      (** Registers the handler [f] to perform custom top level script commands
          ({!module-Script.module-Ast.type-t}).

          The handler [f] can use the parser environment ({!module-Script.type-env})
          to parse and resolve script expressions with {!module-Script.val-eval_loc}
          and {!module-Script.val-eval_expr}.

          The handler [f] should return [true] when the command has been treated,
          [false] otherwise. *)
  | Command_printer of (Format.formatter -> Script.Ast.t -> bool)
      (** Registers the custom printer [f] to format the new top level script
          commands ({!module-Script.module-Ast.type-t}) in debug outputs.

          The printer [f] should return [true] for the instructions it handles,
          [false] otherwise. *)
  | Builtin_resolver of (Ir.builtin -> 'path primitive)
      (** Registers the function [f] in charge to resolve the handler of a given
          {!module-Ir.type-builtin}.

          The resolver [f] should return {!constructor-Call} [h] or
          {!constructor-Apply} [h] for the [builtin]s it supports,
          {!constructor-Unknown} otherwise. It is called once per [builtin]
          occurrence in the intermediate representation ([Ir]).

          It is an error to not handle a [builtin] added by the plugin.

          The handler [h] has access to the [path] interface ({!module-type-PATH}).

          It should return {!constructor-Return} to resume the exploration
          along this path. {!constructor-Signal}[ status] cuts the path and logs
          the given status.

          The handler [h] is called each time the path reaches the given
          {!module-Ir.type-builtin}. *)
  | Builtin_may_read of (Ir.builtin -> Dba_types.Var.Set.t option option)
      (** Registers the knowledge provider [f] for custom
          {!module-Ir.type-builtin}s.

          It must return [Some k] for the [builtin] it handles, [None] otherwise.
          - [k = Some set] is the (overapproximated) set of all variables the builtin
          may access. It is an error to access a variable not listed here and may
          result in undefined value.
          - [k = None] means any variable may be accessed. *)
  | Builtin_may_write of (Ir.builtin -> Dba_types.Var.Set.t option option)
      (** Registers the knowledge provider [f] for custom
          {!module-Ir.type-builtin}s.

          It must return [Some k] for the [builtin] it handles, [None] otherwise.
          - [k = Some set] is the (overapproximated) set of all variables the builtin
          may modify. It is an error to modify a variable not listed here and may
          result in undefined value.
          - [k = None] means any variable may be modified. *)
  | Builtin_must_write of (Ir.builtin -> Dba_types.Var.Set.t option)
      (** Registers the knowledge provider [f] for custom
          {!module-Ir.type-builtin}s.

          It must return [Some set] for the [builtin] it handles, [None] otherwise.
          [set] is the (underapproximated) set of the variables a builtin must
          overwrite. Previous values of these variables are deemed
          no longer reachable. *)
  | Builtin_printer of (Format.formatter -> Ir.builtin -> bool)
      (** Registers the custom printer [f] to format
          the custom {!module-Ir.type-builtin}s in debug outputs.

          The printer [f] should return [true] when the {!module-Ir.type-builtin}
          is treated, [false] otherwise. *)
  | Fork_callback of ('path -> 'path -> unit)
      (** Registers the function [f] to be called when a path execution is about
          to fork.

          It can be used to maintain invariants between the parent (first parameter)
          and the child (second parameter) paths. *)
  | Signal_callback of ('path -> status -> unit)
      (** Registers the function [f] to be called when a path receives a signal.
          The {!type-status} gives information about why the execution is
          interrupted. *)
  | Exit_callback of (unit -> unit)
      (** Registers the function [f] to be called when the exploration is about
          to terminate.

          It can be used to output some global wise statistics or results. *)

type ('value, 'model, 'state, 'path, 'a) field_id = ..
(** An identifier used to declare new field in the {!module-type-PATH} structure
    via the {!module-type-PLUGIN} interface.

    A {!module-type-PATH.type-key} to a value of type ['a] can be queried with
    the function {!module-type-ENGINE.val-lookup} if the related {!type-field}
    is present in one {!module-type-PLUGIN.val-fields} list.
    The type ['a] can be anything, including a function, and may depend on the
    type parameters ['value], ['model], ['state] and ['path].

    For instance, the field {!module-type-PATH.val-models} could have been added
    as follows.
    {[type ('value, 'model, 'state, 'path, 'a) field_id +=
      | Models : ('value, 'model, 'state, 'path, 'model list) field_id

    let fields _ =
      [
        Field
        {
          id = Models;
          default = [];
          copy = None;
          merge = Some (fun x y -> Some (List.append x y));
        };
      ]]} *)

(** A field declaration used to declare new field in the {!module-type-PATH}
    structure via the {!module-type-PLUGIN.val-fields} list.

    The declaration contain an identifier [id] ({!type-field_id}),
    the [default] value, an optional [copy] function and an optional
    [merge] function.

    The [copy] function is called each time a path is forked to create
    a new local copy of the field value (by {b default} {!module-Fun.val-id}).

    The [merge] function is called each time two paths are about to merge.
    It returns [Some] when the field values are mergeable, and [None] otherwise
    (by {b default}, it returns [Some] if both values are physically equal). *)
and field =
  | Field : {
      id : ('value, 'model, 'state, 'path, 'a) field_id;
      default : 'a;
      copy : ('a -> 'a) option;
      merge : ('a -> 'a -> 'a option) option;
    }
      -> field

module type ENGINE = sig
  val isa : Machine.isa
  val image : Image.t

  val fs : string -> Loader_types.buffer
  (** [fs filepath] returns the content of [filepath].
      It uses the virtual file system which may differ from the host one.

       @raise Not_found if the file does not exist.
  *)

  module Path : PATH

  module Metrics : sig
    module Exploration : Metrics.EXPLORATION
    module Queries : Metrics.QUERY
  end

  module Debug : sig
    val reverse_section : Virtual_address.t -> (string * Z.t) option
    (** [reverse_section addr] returns the name and the offset from the start of
        the section from which the address belongs. *)

    val reverse_symbol : Virtual_address.t -> (string * Z.t) option
    (** [reverse_symbol addr] returns the name and the offset from the start of
        the symbol from which the address belongs. *)
  end

  val lookup :
    (Path.value, Path.model, Path.state, Path.t, 'a) field_id -> 'a Path.key
  (** [lookup fid] returns the key associated to the field identifier [fid]
      registered via {!module-type-PLUGIN.val-fields}. *)

  val resume : Path.t -> Path.t continuation -> unit
  (** [resume path] resumes the execution of a path which was paused with
      {!constructor-Signal} {!constructor-Stashed}.

      It adds back [path] to the running path worklist. *)
end

module type PLUGIN = sig
  val name : string
  (** The name of the {b plugin}.

      It should be unique. *)

  val fields : (module PATH) -> field list
  (** [fields path] should return a list of custom fields ([\[ field0; field1; ... \]])
      to register into the path structure.
      
      The associated {!module-type-PATH.type-key} can be retrieved via
      the function {!module-type-ENGINE.val-lookup}.
      *)

  val extensions : (module ENGINE with type Path.t = 'a) -> 'a extension list
  (** [extensions engine] should return a list of custom extentions to activate
      for the {b plugin}.

      This function is called only once per symbolic execution run and comes after
      {!val-fields}. *)
end

module type EXTENSIONS = sig
  type path

  val list : path extension list
end
