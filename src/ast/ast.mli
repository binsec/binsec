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

module Obj : sig
  type t = ..
end

type 'a loc = 'a * Lexing.position

val loc : 'a -> Lexing.position -> 'a loc

module Symbol : sig
  type t = string * Dba.Var.Tag.attribute

  val create : ?attr:Dba.Var.Tag.attribute -> string -> t
  val pp : Format.formatter -> t -> unit
end

module rec Size : sig
  type t =
    | Implicit
    | Explicit of int
    | Sizeof of Loc.t loc
    | Eval of Expr.t loc

  val none : t
  val some : int -> t
  val sizeof : Loc.t loc -> t
  val eval : Expr.t loc -> t
  val pp : Format.formatter -> t -> unit
end

and Loc : sig
  type t =
    | Var of string * Size.t
    | Load of int * Machine.endianness option * Expr.t loc * string option
    | Sub of int Interval.t * t loc

  val var : ?size:Size.t -> string -> t

  val load :
    ?array:string -> int -> ?endianness:Machine.endianness -> Expr.t loc -> t

  val restrict : hi:int -> lo:int -> t loc -> t
  val pp : Format.formatter -> t -> unit
end

and Expr : sig
  type t =
    | Int of Z.t
    | Bv of Bitvector.t
    | Symbol of Symbol.t loc
    | Loc of Loc.t loc
    | Unary of Dba.Unary_op.t * t loc
    | Binary of Dba.Binary_op.t * t loc * t loc
    | Ite of t loc * t loc * t loc

  val zero : t
  val one : t
  val succ : t loc -> t
  val integer : Z.t -> t
  val constant : Bitvector.t -> t
  val symbol : Symbol.t loc -> t
  val loc : Loc.t loc -> t
  val add : t loc -> t loc -> t
  val sub : t loc -> t loc -> t
  val mul : t loc -> t loc -> t
  val neg : t loc -> t

  (* Unsigned operations *)
  val udiv : t loc -> t loc -> t
  val umod : t loc -> t loc -> t

  (* Signed operations *)
  val sdiv : t loc -> t loc -> t
  val smod : t loc -> t loc -> t
  val logand : t loc -> t loc -> t
  val logor : t loc -> t loc -> t
  val lognot : t loc -> t
  val logxor : t loc -> t loc -> t

  include Sigs.COMPARISON with type t := t loc and type boolean := t

  val shift_left : t loc -> t loc -> t
  val shift_right : t loc -> t loc -> t
  val shift_right_signed : t loc -> t loc -> t
  val rotate_left : t loc -> t loc -> t
  val rotate_right : t loc -> t loc -> t
  val sext : int -> t loc -> t
  val uext : int -> t loc -> t
  val restrict : hi:int -> lo:int -> t loc -> t
  val append : t loc -> t loc -> t
  val ite : t loc -> t loc -> t loc -> t
  val pp : Format.formatter -> t -> unit
end

module Instr : sig
  type t = ..

  type t +=
    | Nop
    | Label of string  (** [label]: *)
    | Assign of Loc.t loc * Expr.t loc  (** [lval] := [rval] *)
    | Undef of Loc.t loc  (** [lval] := undef *)
    | Nondet of Loc.t loc  (** [lval] := nondet *)
    | Assume of Expr.t loc  (** assume [rval] *)
    | Assert of Expr.t loc  (** assert [rval] *)
    | If of Expr.t loc * string  (** if [rval] then goto [label] *)
    | Goto of string  (** goto [label] *)
    | Jump of Expr.t loc  (** jump at [rval] *)
    | Halt

  val nop : t
  val label : string -> t
  val assign : Loc.t loc -> Expr.t loc -> t
  val undef : Loc.t loc -> t
  val nondet : Loc.t loc -> t
  val assume : Expr.t loc -> t
  val dynamic_assert : Expr.t loc -> t
  val conditional_jump : Expr.t loc -> string -> t
  val dynamic_jump : Expr.t loc -> t
  val goto : string -> t
  val halt : t
  val pp : Format.formatter -> t -> unit
  val register_pp : (Format.formatter -> t -> bool) -> unit
end

type t = ..
