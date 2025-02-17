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

module rec Expr : (Term.S with type a := Dba.Var.t and type b := Layer.t)

and Store : sig
  type t

  val iter : (Z.t -> Expr.t -> unit) -> t -> unit
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

module VarMap : Map.S with type key = Dba.Var.t
module StrMap : Map.S with type key = string

module Env : sig
  type t = private {
    vars : Expr.t VarMap.t;
    layers : (Layer.t * bool) StrMap.t;
    rev_reads : Expr.t list;
    input_vars : Expr.t VarMap.t;
  }

  val empty : t
  val is_empty : t -> bool
  val assign : Dba.Var.t -> Dba.Expr.t -> t -> t
  val clobber : Dba.Var.t -> t -> t
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

val commit : Env.t -> Ir.fallthrough list
