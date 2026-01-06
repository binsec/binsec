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

module Source : sig
  type kind = Input | Clobber | Symbolic
  type t = Dba.Var.t * int * kind

  include Sigs.HASHABLE with type t := t
  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

module rec Expr : (Term.S with type a := Source.t and type b := Layer.t)

and Store : sig
  type t

  val iter : (Z.t -> Expr.t -> unit) -> t -> unit
  val rev_iter : (Z.t -> Expr.t -> unit) -> t -> unit
end

and Layer : sig
  type t = private
    | Base of string option
    | Layer of {
        id : int;
        over : t;
        base : string option;
        addr : Expr.t;
        store : Store.t;
      }

  val base : t -> string option

  include Sigs.HASHABLE with type t := t
end

type var = ([ `Var ], Source.t, Layer.t) Expr.term

module Env : sig
  type t = private {
    id : int;
    vars : Expr.t Dba_types.Var.Map.t;
    layers : (Layer.t * bool) Basic_types.String.Map.t;
    rev_reads : Expr.t list;
    sources : var list Dba_types.Var.Map.t;
  }

  val empty : t
  val is_empty : t -> bool
  val assign : Dba.Var.t -> Dba.Expr.t -> t -> t
  val clobber : Dba.Var.t -> t -> t
  val symbolize : Dba.Var.t -> t -> t
  val forget : Dba.Var.t -> t -> t

  val load :
    Dba.Var.t -> string option -> Machine.endianness -> Dba.Expr.t -> t -> t

  val store :
    string option ->
    Machine.endianness ->
    addr:Dba.Expr.t ->
    Dba.Expr.t ->
    t ->
    t

  val eval : Dba.Expr.t -> t -> Expr.t * t
end

type 'a operator = 'a Term.operator
and unary = Term.unary
and binary = Term.binary

type 'a node =
  | Constant : Bitvector.t -> [< `Value | `Opcode ] node
  | Value : int -> [< `Value | `Opcode ] node
  | Variable : Dba.Var.t -> [< `Value | `Opcode ] node
  | Unary : unary operator * [ `Value ] node -> [< `Value | `Opcode ] node
  | Binary :
      binary operator * [ `Value ] node * [ `Value ] node
      -> [< `Value | `Opcode ] node
  | Ite :
      [ `Value ] node * [ `Value ] node * [ `Value ] node
      -> [< `Value | `Opcode ] node
  | Load :
      string option * [ `Value ] node * Machine.endianness * int
      -> [ `Opcode ] node
  | Store :
      string option * [ `Value ] node * Machine.endianness * [ `Value ] node
      -> [ `Opcode ] node
  | Assign : Dba.Var.t * [ `Value ] node -> [ `Opcode ] node
  | Clobber : Dba.Var.t -> [ `Opcode ] node
  | Symbolize : Dba.Var.t -> [ `Opcode ] node

and value = [ `Value ] node
and opcode = [ `Opcode ] node

val pp_opcode : Format.formatter -> 'a node -> unit
val commit : Env.t -> opcode array
val partial_commit : Env.t -> Dba_types.Var.Set.t -> Env.t * opcode array
