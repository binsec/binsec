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

type builtin = ..
type builtin += Inline of Dhunk.t | EndOfHook

val pp_builtin : Format.formatter -> builtin -> unit
val register_builtin_printer : (Format.formatter -> builtin -> bool) -> unit

type 'a opcode =
  | Nop : [ `Fallthrough ] opcode
  | Instruction : Instruction.t -> [< `Label | `Fallthrough ] opcode
  | Hook : {
      addr : Virtual_address.t;
      info : string;
    }
      -> [< `Label | `Fallthrough ] opcode
  | Assign : { var : Dba.Var.t; rval : Dba.Expr.t } -> [ `Fallthrough ] opcode
  | Clobber : Dba.Var.t -> [ `Fallthrough ] opcode
  | Forget : Dba.Var.t -> [ `Fallthrough ] opcode
  | Load : {
      var : Dba.Var.t;
      base : string option;
      dir : Machine.endianness;
      addr : Dba.Expr.t;
    }
      -> [ `Fallthrough ] opcode
  | Store : {
      base : string option;
      dir : Machine.endianness;
      addr : Dba.Expr.t;
      rval : Dba.Expr.t;
    }
      -> [ `Fallthrough ] opcode
  | Symbolize : Dba.Var.t -> [ `Fallthrough ] opcode
  | Assume : Dba.Expr.t -> [ `Fallthrough ] opcode
  | Assert : Dba.Expr.t -> [ `Fallthrough ] opcode
  | Builtin : builtin -> [< `Fallthrough | `Terminator ] opcode
  | Goto : {
      target : Virtual_address.t;
      tag : Dba.tag;
    }
      -> [< `Fallthrough | `Terminator ] opcode
  | Jump : { target : Dba.Expr.t; tag : Dba.tag } -> [ `Terminator ] opcode
  | Halt : [ `Terminator ] opcode
  | Cut : [ `Terminator ] opcode
  | Die : string -> [ `Terminator ] opcode

and label = [ `Label ] opcode
and fallthrough = [ `Fallthrough ] opcode
and terminator = [ `Terminator ] opcode

val pp_opcode : Format.formatter -> 'a opcode -> unit

type node =
  | Fallthrough of { label : label; kind : fallthrough; succ : int }
  | Branch of {
      label : label;
      test : Dba.Expr.t;
      target : int;
      fallthrough : int;
    }
  | Terminator of { label : label; kind : terminator }

val pp_node : Format.formatter -> node -> unit
val label_of : node -> label

type stmt =
  | Nop
  | Label of string  (** [label]: *)
  | Opcode of fallthrough
  | If of Dba.Expr.t * string  (** if [rval] then goto [label] *)
  | Goto of string  (** goto [label] *)
  | End of terminator

module type GRAPH = sig
  include Graph.Sig.G with type V.t = int and type E.t = int * bool * int

  val node : t -> vertex -> node
  val fold_entries : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_exits : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_entries : (vertex -> unit) -> t -> unit
  val iter_exits : (vertex -> unit) -> t -> unit
end

module type INSTRUMENT = sig
  type t
  type vertex

  val insert_before : t -> vertex -> ?label:label -> fallthrough -> unit
  val insert_before_v : t -> vertex -> ?label:label -> fallthrough -> vertex
  (* Same as {val-insert_before} but also returns the new vertex. *)

  val insert_list_before :
    t -> vertex -> ?label:label -> fallthrough list -> unit

  val insert_list_before_v :
    t -> vertex -> ?label:label -> fallthrough list -> vertex
  (* Same as {val-insert_list_before} but also returns the vertex of the
     first element of the list. *)
end

module View : GRAPH

module Graph : sig
  include GRAPH with type t = private View.t
  include INSTRUMENT with type t := t and type vertex := vertex

  val length : t -> int
  val empty : unit -> t
  val copy : t -> t
  val of_instruction : Instruction.t -> t

  val of_script :
    Virtual_address.t -> string -> ?eoh:terminator -> stmt list -> t

  val replace_node : t -> vertex -> node -> unit
  val append_node : t -> node -> vertex
  val append : t -> from:t -> vertex
end

module Killset : sig
  type t = Dba_types.Var.Set.t Basic_types.Integers.Int.Htbl.t

  val is_deadstore : t -> Dba.Var.t -> View.vertex -> bool

  val analyze :
    View.t ->
    may_read:(builtin -> Dba_types.Var.Set.t option) ->
    must_write:(builtin -> Dba_types.Var.Set.t) ->
    ?sink:Basic_types.Integers.Int.Set.t ->
    t ->
    unit
end
