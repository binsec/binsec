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

(** Definition of DBA type *)

type size = int

type id = int
(** An [id] is a local identifier which characterizes an atomic instruction
    inside a Dba.block *)

type address = { base : Virtual_address.t; id : id }
(** A DBA [address] is the association of a DBA block address represented by
    [base] and a unique [id].
    The first element of a block has [id] [0]. *)

type addresses = address list

type 'a jump_target =
  | JInner of 'a  (** Jump inside the same block, to a label *)
  | JOuter of address  (** Jump outside the block to its first element *)

type tag =
  | Default
  | Call of address
  | Return  (** For call address of return site *)

type state =
  | OK
  | KO
  | Undecoded of string  (** Stop because of unanticipated string of bytes **)
  | Unsupported of string  (** Stop because instr is not supported by Binsec **)

module Unary_op : sig
  type t =
    | UMinus
    | Not
    | Sext of size  (** result has size `size` **)
    | Uext of size
    | Restrict of int Interval.t
end

module Binary_op : sig
  type t =
    | Plus
    | Minus
    | Mult
    | DivU (* Corresponds to *)
    | DivS (* the truncated division *)
    | ModU (* of C99 and most *)
    | ModS (* processors *)
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

  val invert : t -> t
  (** [invert t] inverts [t] if it has an inverse version.
      Raise [Failure "BinaryOperator.invert "] otherwise
  *)

  val has_inverse : t -> bool
end

module Var : sig
  module Tag : sig
    type attribute = Value | Size | Last | Plt

    val pp_attribute : Format.formatter -> attribute -> unit

    type t =
      | Flag
      | Temp
      | Register
      | Symbol of attribute * Bitvector.t lazy_t
      | Empty

    include Sigs.HASHABLE with type t := t
  end

  type t = private { id : int; name : string; size : size; info : Tag.t }

  val create : string -> bitsize:Size.Bit.t -> tag:Tag.t -> t

  val flag : ?bitsize:Size.Bit.t -> string -> t
  (** [flag ~size fname] creates a flag variable.
      - [size] defaults to 1
  *)

  val temporary : string -> Size.Bit.t -> t

  val temp : Size.Bit.t -> t
  (** [temp n] creates a lvalue representing a temporary of size [n] with name
      [Format.sprintf "temp%d" n]. *)

  val compare : t -> t -> int

  include Hashtbl.HashedType with type t := t

  val from_id : int -> t
  (** [from_id id] returns the variable identified by [id].

      @raise Not_found if [id] is not a valid identifier.
  *)
end

module Expr : sig
  type t = private
    | Var of Var.t
    | Load of size * Machine.endianness * t * string option (* size: bytes *)
    | Cst of Bitvector.t
    | Unary of Unary_op.t * t
    | Binary of Binary_op.t * t * t
    | Ite of t * t * t
  (* sugar operator *)

  val size_of : t -> int
  val is_equal : t -> t -> bool
  val is_constant : t -> bool

  val var : ?tag:Var.Tag.t -> string -> int -> t
  (** {2 Constructors} *)

  val v : Var.t -> t
  val temporary : size:int -> string -> t

  val constant : Bitvector.t -> t
  (** [constant bv] creates a constant expression from the bitvector [bv].
  *)

  (** {3 Specific constants }*)

  val zeros : int -> t
  (** [zeros n] creates a constant expression of value 0 with length [n] *)

  val ones : int -> t
  (** [ones n] creates a constant expression of value 1 with length [n].
      I.e.; it has (n - 1) zeros in binary.
   *)

  val one : t
  val zero : t
  val _true : t
  val _false : t

  val binary : Binary_op.t -> t -> t -> t
  (** {3 Binary expressions} *)

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val smod : t -> t -> t
  val umod : t -> t -> t
  val udiv : t -> t -> t
  val sdiv : t -> t -> t
  val append : t -> t -> t

  include Sigs.COMPARISON with type t := t and type boolean = t

  val unary : Unary_op.t -> t -> t
  val uminus : t -> t

  include Sigs.EXTENDED_LOGICAL with type t := t

  val sext : int -> t -> t
  (** [sext sz e] performs a signed extension of expression [e] to size [sz] *)

  val uext : int -> t -> t
  (** [uext sz e] performs an unsigned extension expression [e] to size [sz] *)

  val shift_left : t -> t -> t
  val shift_right : t -> t -> t

  val shift_right_signed : t -> t -> t
  (** [shift_(left|right) e q] shifts expression [e] by quantity [q], padding
      with zeroes *)

  val rotate_left : t -> t -> t

  val rotate_right : t -> t -> t
  (** [rotate_(left|right) e q] rotates expression [e] by quantity [q] *)

  val ite : t -> t -> t -> t
  (** [ite cond then_e else_e] creates [Dba.ExprIte(cond, then_e, else_e)] *)

  val restrict : int -> int -> t -> t
  (** [restrict lo hi e] creates [Dba.ExprUnary(Restrict(lo, hi), e)] if
      [hi >= lo && lo >=0] .
  *)

  val bit_restrict : int -> t -> t
  (** [bit_restrict o e] is [restrict o o e] *)

  val load : ?array:string -> Size.Byte.t -> Machine.endianness -> t -> t
  (** [load nbytes endianness t] *)

  val is_max : t -> bool
  (** [is_max e] is [true] if [e] is
      - constant; and
      - the maximum unsigned representable for the size of this expression *)
end

type exprs = Expr.t list
type printable = Exp of Expr.t | Str of string

module LValue : sig
  type t = private
    | Var of Var.t
    | Restrict of Var.t * int Interval.t
    | Store of
        size (* size in bytes *) * Machine.endianness * Expr.t * string option

  include Sigs.Eq with type t := t

  val size_of : t -> int
  (** [size_of lv] yields the size of [lv] in bits **)

  val var : ?tag:Var.Tag.t -> bitsize:Size.Bit.t -> string -> t
  (** [var tag name ~size] creates a DBA lvalue for a variable *)

  val v : Var.t -> t

  val flag : ?bitsize:Size.Bit.t -> string -> t
  (** [flag ~size fname] creates a flag variable.
      - [size] defaults to 1
  *)

  val temporary : string -> Size.Bit.t -> t
  val _restrict : string -> Size.Bit.t -> int -> int -> t

  val _bit_restrict : string -> Size.Bit.t -> int -> t
  (** [_restrict] and [_bit_restrict] are deprecated. Use the other forms
   ** below.
   *)

  val restrict : Var.t -> int -> int -> t
  val bit_restrict : Var.t -> int -> t
  val store : ?array:string -> Size.Byte.t -> Machine.endianness -> Expr.t -> t

  val temp : Size.Bit.t -> t
  (** [temp n] creates a lvalue representing a temporary of size [n] with name
      [Format.sprintf "temp%d" n]. *)

  val is_expr_translatable : Expr.t -> bool
  (** [is_expr_translatable e] returns true is the expression can have a valid
      lvalue translation *)

  val of_expr : Expr.t -> t
  (** [of_expr e] translates an expression to its lvalue equivalent if possible.

      Use [is_expr_translatable e] to check the feasability of this translation.

      @raise Failure "LValue.of_expr ..." if it is not possible.
  *)

  val to_expr : t -> Expr.t
  (** [to_expr e] translates an lvalue to its equivalent rvalue.  *)

  val bitsize : t -> Size.Bit.t
  (** [bitsize lv] returns the size in bits of [lv].
  *)

  val resize : Size.Bit.t -> t -> t
  (** [resize bitsize lv] patches the lvalue [lv] and gives it a size of
      [bitsize].
  *)
end

module Tag : sig
  type t = tag

  include Sigs.Eq with type t := t
end

module Jump_target : sig
  type 'a t = 'a jump_target

  val outer : address -> 'a t
  val inner : 'a -> 'a t
  val is_inner : 'a t -> bool
  val is_outer : 'a t -> bool
end

module Instr : sig
  type t = private
    | Assign of LValue.t * Expr.t * id
    | SJump of id jump_target * tag
    | DJump of Expr.t * tag
    | If of Expr.t * id jump_target * id
    | Stop of state option
    | Assert of Expr.t * id
    | Assume of Expr.t * id
    | Nondet of LValue.t * id
    | Undef of LValue.t * id
        (** value of lval is undefined
                              ** e.g. AF flag for And instruction in x86 **)

  (** {7 Constructors} *)

  val assign : LValue.t -> Expr.t -> int -> t
  (** [assign lv e successor_id] creates the assignment of expression [e] to
      l-value [lv], going to DBA instruction successor [id] *)

  val ite : Expr.t -> id Jump_target.t -> int -> t
  val undefined : LValue.t -> int -> t
  val non_deterministic : LValue.t -> int -> t
  val static_jump : ?tag:Tag.t -> id Jump_target.t -> t
  val static_inner_jump : ?tag:Tag.t -> int -> t
  val static_outer_jump : ?tag:Tag.t -> Virtual_address.t -> t
  val call : return_address:address -> id Jump_target.t -> t
  val dynamic_jump : ?tag:Tag.t -> Expr.t -> t
  val _assert : Expr.t -> int -> t
  val assume : Expr.t -> int -> t
  val stop : state option -> t
end
