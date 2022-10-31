(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2022                                               *)
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

module type AST = sig
  module Expr : sig
    type t

    val size_of : t -> int

    val zero : t

    val succ : t -> t

    val constant : Bitvector.t -> t

    val var : ?tag:Dba.VarTag.t -> string -> int -> t

    include Sigs.ARITHMETIC with type t := t

    include Sigs.COMPARISON with type t := t and type boolean := t

    include Sigs.BITWISE with type t := t

    val sext : int -> t -> t

    val uext : int -> t -> t

    val restrict : int -> int -> t -> t

    val ite : t -> t -> t -> t
  end

  module LValue : sig
    type t

    val size_of : t -> int

    val store : int -> Machine.endianness -> Expr.t -> string -> t

    val to_expr : t -> Expr.t
  end

  module Instr : sig
    type t

    val assign : LValue.t -> Expr.t -> t

    val undef : LValue.t -> t

    val nondet : LValue.t -> t

    val assume : Expr.t -> t

    val dynamic_assert : Expr.t -> t

    val conditional_jump : Expr.t -> string -> t

    val dynamic_jump : Expr.t -> t

    val goto : string -> t

    val label : string -> t

    val halt : t
  end
end

module type ENV = sig
  type lval

  and expr

  val lookup : string -> int -> lval

  val lookup_symbol : string -> Dba.VarTag.attribute -> expr

  val wordsize : int

  val endianness : Machine.endianness
end
