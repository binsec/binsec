(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
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

(** Definition of DBA type *)

type endianness =
  | LittleEndian
  | BigEndian

type size = int

type malloc_size = Bigint.t

type id = int
(** An [id] is a local identifier which characterizes an atomic instruction
    inside a Dba.block *)

type address = {
  base : Bitvector.t;
  id : id;
}
(** A DBA [address] is the association of a DBA block address represented by
    [base] and a unique [id].
    The first element of a block has [id] [0]. *)

type addresses = address list

type jump_target =
  | JInner of id (** Jump inside the same block *)
  | JOuter of address (** Jump outside the block to its first element *)

type tag =
  | Call of address
  | Return (** For call address of return site *)

type state =
  | OK
  | KO
  | Undefined of string
  | Unsupported of string

type alternativeTag = AddCarry | AddOverflow

type unary_op =
  | UMinus
  | Not

type binary_op =
  | Plus
  | Minus
  | MultU
  | MultS
  | DivU
  | DivS
  | ModU
  | ModS
  | Or
  | And
  | Xor
  | Concat
  | LShift
  | RShiftU
  | RShiftS
  | LeftRotate
  | RightRotate
  | Eq (* reified comparison: return a 1-bit value *)
  | Diff
  | LeqU
  | LtU
  | GeqU
  | GtU
  | LeqS
  | LtS
  | GeqS
  | GtS

type malloc_status = Freed | Freeable
type restricted_region =
  [ `Stack
  | `Malloc of (int * address) * Bigint.t ]

type region = [ `Constant | restricted_region ]

type expr =
  | ExprVar of string * size * vartag option  (* size: bits *)
  | ExprLoad of size * endianness * expr (* size: bytes *)
  | ExprCst of region * Bitvector.t
  | ExprUnary of  unary_op * expr
  | ExprBinary of binary_op * expr * expr
  | ExprRestrict of  expr * int * int (** Restriction of an expression from lo
                                       ** to hi *)
  | ExprExtU of expr * size (* size is new size in bits *)
  | ExprExtS of expr * size (* size is new size in bits *)
  | ExprIte of cond *  expr *  expr (* sugar operator *)
  | ExprAlternative of exprs * alternativeTag option

and exprs = expr list

and cond =
  | CondReif of expr (* size of expr should be one bit *)
  | CondNot of cond
  | CondAnd of cond * cond
  | CondOr  of cond * cond
  | True
  | False

and compare =
  | FlgCmp of expr * expr
  | FlgSub of expr * expr
  | FlgTest of expr * expr
  | FlgUnspecified

and vartag =
  | Flag of compare
  | Temp

type printable =
  | Exp of expr
  | Str of string

type lhs =
  | LhsVar of string * size * vartag option (* size in bits *)
  | LhsVarRestrict of string * size * int * int
  | LhsStore of size * endianness * expr  (* size in bytes *)

type instruction =
  | IkAssign of lhs *  expr *  id
  | IkSJump of jump_target * tag option
  | IkDJump of expr * tag option
  | IkIf of cond * jump_target * id
  | IkStop of state option
  | IkAssert of cond * id
  | IkAssume of cond * id
  | IkNondetAssume of lhs list * cond * id
  | IkNondet of lhs * region * id
  | IkUndef of lhs * id
  | IkMalloc of lhs * expr * id
  | IkFree of expr * id
  | IkPrint of printable list * id

