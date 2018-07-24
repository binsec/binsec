(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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

type 'a jump_target =
  | JInner of 'a (** Jump inside the same block *)
  | JOuter of address (** Jump outside the block to its first element *)

type tag =
  | Call of address
  | Return (** For call address of return site *)

type state =
  | OK
  | KO
  | Undefined of string
  | Unsupported of string

module Unary_op = struct
  type t =
    | UMinus
    | Not
    | Sext of size
    | Uext of size
    | Restrict of int Interval.t
end

module Binary_op = struct

  type t =
    | Plus
    | Minus
    | Mult
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


  let invert = function
    | Eq   -> Diff
    | Diff -> Eq
    | LeqU -> GtU
    | LtU  -> GeqU
    | GeqU -> LtU
    | GtU  -> LeqU
    | LeqS -> GtS
    | LtS  -> GeqS
    | GeqS -> LtS
    | GtS  -> LeqS
    | _ -> failwith "BinaryOperator.invert"

  let has_inverse = function
    | Eq  | Diff | LeqU | LtU  | GeqU
    | GtU | LeqS | LtS  | GeqS | GtS -> true
    | _ -> false

end

type malloc_status = Freed | Freeable
type restricted_region =
  [ `Stack
  | `Malloc of (int * address) * Bigint.t ]

type region = [ `Constant | restricted_region ]

(* Region *)
module ComparableRegion = struct
  type _t = region

  let compare r1 r2 =
    match r1, r2 with
    | `Constant, `Constant -> 0
    | `Stack, `Stack -> 0
    | `Malloc ((id1, _), _), `Malloc ((id2, _), _) -> Pervasives.compare id1 id2
    | `Constant, _ -> 1
    | `Stack, `Constant -> -1
    | `Stack, _ -> 1
    | `Malloc _, _ -> -1
end

module rec Expr :
sig
  type t = private
    | Var of string * size * VarTag.t option  (* size: bits *)
    | Load of size * endianness * t (* size: bytes *)
    | Cst of region * Bitvector.t
    | Unary of Unary_op.t * t
    | Binary of Binary_op.t * t * t
    | Ite of t *  t *  t (* sugar operator *)

  val var : string -> int -> VarTag.t option -> t

  val equal : t -> t -> bool
  val size_of : t -> int
(*
 *   val var : Size.Bit.t -> string -> Tag.t option -> t
 *   val flag : ?bits:Size.Bit.t -> string -> t
 *
 *)

  val is_constant : t -> bool
  val constant : ?region:region -> Bitvector.t -> t
  val temporary : size:int -> string -> t

  val zeros : int -> t
  val ones : int -> t
  val one : t
  val _true : t
  val zero : t
  val _false : t
  val binary: Binary_op.t -> t -> t -> t
  val add                : t -> t -> t
  val sub                : t -> t -> t
  val mul                : t -> t -> t
  val smod               : t -> t -> t
  val umod               : t -> t -> t
  val udiv               : t -> t -> t
  val sdiv               : t -> t -> t
  val append             : t -> t -> t
  val eq                 : t -> t -> t
  val diff               : t -> t -> t
  val ule                : t -> t -> t
  val sle                : t -> t -> t
  val ult                : t -> t -> t
  val slt                : t -> t -> t
  val uge                : t -> t -> t
  val sge                : t -> t -> t
  val ugt                : t -> t -> t
  val sgt                : t -> t -> t

  val unary              : Unary_op.t -> t -> t
  val uminus             : t -> t

  include Sigs.Logical with type t := t

  val logxor : t -> t -> t
  val shift_left  : t -> t -> t
  val shift_right : t -> t -> t
  val shift_right_signed : t -> t -> t

  val rotate_left  : t -> t -> t
  val rotate_right : t -> t -> t

  val sext : int -> t -> t
  val uext : int -> t -> t


  val ite : t -> t -> t -> t

  val restrict : int -> int -> t -> t
  val bit_restrict : int -> t -> t
  val load : Size.Byte.t -> endianness -> t -> t

  val is_max : t -> bool
end
= struct
  open Binary_op
  type t =
    | Var of string * size * VarTag.t option  (* size: bits *)
    | Load of size * endianness * t (* size: bytes *)
    | Cst of region * Bitvector.t
    | Unary of Unary_op.t * t
    | Binary of Binary_op.t * t * t
    | Ite of t *  t *  t (* sugar operator *)

  let rec size_of = function
    | Cst(_, b) -> Bitvector.size_of b
    | Var(_, size, _) -> size
    | Load(bytesize, _, _) -> 8 * bytesize
    | Ite(_, e, _)
    | Unary((Unary_op.UMinus | Unary_op.Not), e) -> size_of e
    | Unary ((Unary_op.Sext bits | Unary_op.Uext bits), _) -> bits
    | Unary (Unary_op.Restrict {Interval.lo; Interval.hi}
            , _) -> hi - lo + 1
    | Binary(bop, e1, e2) ->
      begin match bop with
        | Concat -> size_of e1 + size_of e2
        | Eq | Diff | LeqU | LtU | GeqU | GtU | LeqS | LtS | GeqS | GtS -> 1
        | Plus | Minus | Mult
        | DivU | DivS | ModU | ModS | Or
        | And | Xor | LShift | RShiftU | RShiftS | LeftRotate | RightRotate
          -> size_of e1
      end

  let rec equal e1 e2 =
    match e1, e2 with
    | Var (n1, sz1, t1),
      Var (n2, sz2, t2) -> n1 = n2 && sz1 = sz2 && t1 = t2
    | Load (sz1, en1, e1),
      Load (sz2, en2, e2) -> sz1 = sz2 && en1 = en2 && equal e1 e2
    | Cst (r1, bv1), Cst (r2, bv2) ->
      ComparableRegion.compare r1 r2 = 0 && Bitvector.equal bv1 bv2
    | Unary (unop1, e1), Unary (unop2, e2) ->
      unop1 = unop2 && equal e1 e2
    | Binary (binop1, lexpr1, rexpr1),
      Binary (binop2, lexpr2, rexpr2) ->
      binop1 = binop2 && equal lexpr1 lexpr2 && equal rexpr1 rexpr2
    | Ite (c1, e11, e12),
      Ite (c2, e21, e22) ->
      equal c1 c2 && equal e11 e21 && equal e12 e22
    | _, _ -> false


  let is_constant = function
    | Cst _ -> true
    | _ -> false

  let _is_zero = function
    | Cst (`Constant, bv) -> Bitvector.is_zeros bv
    | _ -> false

  let _is_one = function
    | Cst (_, bv) -> Bitvector.is_ones bv
    | _ -> false

  let is_max = function
    | Cst (`Constant, bv) -> Bitvector.is_max_ubv bv
    | _ -> false

  let var name nbits vtagopt = Var (name, nbits, vtagopt)
  let temporary ~size name = var name size (Some VarTag.temp)

  let constant ?(region=`Constant) bv = Cst (region, bv)

  let zeros length = constant (Bitvector.zeros length)
  let ones length = constant (Bitvector.ones length)

  let zero = constant (Bitvector.zero)
  let _false = zero
  let one = constant (Bitvector.one)
  let _true = one

  let ite condition then_expr else_expr =
    (* Valid conditions are bitvectors of size one  only *)
    assert (size_of condition = 1);
    Ite (condition, then_expr, else_expr)

  let load nbytes endianness e =
    let nbytes = Size.Byte.to_int nbytes in
    Load (nbytes, endianness, e)

  module Straight = struct
    let binary op e1 e2 = Binary (op, e1, e2)

    let append = binary Concat
    let shift_left = binary LShift
    let shift_right = binary RShiftU
    let shift_right_signed = binary RShiftS
    let rotate_left = binary LeftRotate
    let rotate_right = binary RightRotate

    let symmetric_binary op e1 e2 =
      assert (size_of e1 = size_of e2);
      binary op e1 e2

    let add    = symmetric_binary Plus
    let sub    = symmetric_binary Minus
    let mul    = symmetric_binary Mult
    let smod   = symmetric_binary ModS
    let umod   = symmetric_binary ModU
    let udiv   = symmetric_binary DivU
    let sdiv   = symmetric_binary DivS
    let logor  = symmetric_binary Or
    let logxor = symmetric_binary Xor
    let logand = symmetric_binary And
    let eq     = symmetric_binary Eq
    let diff   = symmetric_binary Diff
    let ule    = symmetric_binary LeqU
    let sle    = symmetric_binary LeqS
    let ult    = symmetric_binary LtU
    let slt    = symmetric_binary LtS
    let uge    = symmetric_binary GeqU
    let sge    = symmetric_binary GeqS
    let ugt    = symmetric_binary GtU
    let sgt    = symmetric_binary GtS


    let unary op e = Unary (op, e)

    let lognot = unary Unary_op.Not
    let uminus = unary Unary_op.UMinus

    let sext bits  = unary (Unary_op.Sext bits)

    let uext bits = unary (Unary_op.Uext bits)

    let restrict lo hi =
      assert (hi >= lo && lo >= 0);
      (* Probably should check later on or now that off1 & off2 are within the
       * bounds of the size of e *)
      unary (Unary_op.Restrict {Interval.lo; Interval.hi})

  end


  (* All the following construction functions are defined w.r.t to the
   * "straight" ones. Hence no "rec" keyword after the let is usually *not* an
   * error
  *)
  let rec uminus = function
    | Cst (`Constant, bv) -> Bitvector.neg bv |> constant ~region:`Constant
    | Unary (Unary_op.UMinus, e) -> e
    | e -> Straight.uminus e

  and lognot = function
    | Cst (`Constant, bv) -> Bitvector.lognot bv |> constant ~region:`Constant
    | Unary (Unary_op.Not, e) -> e
    | e -> Straight.lognot e

  and uext size = function
    | e when size = size_of e -> e
    | Cst (`Constant, bv) ->
      Bitvector.extend bv size |> constant ~region:`Constant
    | Unary (Unary_op.Uext _, e) | e -> Straight.uext size e

  and sext size = function
    | e when size = size_of e -> e
    | Cst (`Constant, bv) ->
      Bitvector.extend_signed bv size |> constant ~region:`Constant
    | Unary (Unary_op.Uext _, e) -> Straight.uext size e
    | Unary (Unary_op.Sext _, e) | e -> Straight.sext size e

  and restrict lo hi = function
    | e when lo = 0 && hi = size_of e - 1 -> e
    | Cst (`Constant, bv) ->
      Bitvector.extract bv Interval.{lo; hi} |> constant ~region:`Constant
    | Unary (Unary_op.Restrict {Interval.lo=lo'; _}, e) ->
      Straight.restrict (lo' + lo) (lo' + hi) e
    | Unary ((Unary_op.Uext _ | Unary_op.Sext _), e) when size_of e > hi ->
      Straight.restrict lo hi e
    | Binary ((Binary_op.And | Binary_op.Or | Binary_op.Xor) as op, e1, e2) ->
      restrict lo hi e2 |> binary op @@ restrict lo hi e1
    | Binary (Binary_op.LShift, _, Cst (`Constant, b2))
      when Bitvector.to_int b2 > hi ->
      hi - lo + 1 |> zeros
    | Binary (Binary_op.LShift, e1, Cst (`Constant, b2))
      when Bitvector.to_int b2 <= lo ->
      restrict (lo - Bitvector.to_int b2) (hi - Bitvector.to_int b2) e1
    | Binary ((Binary_op.RShiftU | Binary_op.RShiftS), e1, Cst (`Constant, b2))
      when size_of e1 > hi + Bitvector.to_int b2 ->
      restrict (lo + Bitvector.to_int b2) (hi + Bitvector.to_int b2) e1
    | Binary (Binary_op.Concat, _, e2) when hi < size_of e2 ->
      restrict lo hi e2
    | Binary (Binary_op.Concat, e1, e2) when lo >= size_of e2 ->
      restrict (lo - size_of e2) (hi - size_of e2) e1
    | e -> Straight.restrict lo hi e

  and bit_restrict off = restrict off off

  and unary op e = match op with
    | Unary_op.Not -> lognot e
    | Unary_op.UMinus -> uminus e
    | Unary_op.Sext s -> sext s e
    | Unary_op.Uext s -> uext s e
    | Unary_op.Restrict {Interval.lo; Interval.hi} ->
      restrict lo hi e

  and add e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.add b1 b2 |> constant ~region:`Constant
    | Cst (`Constant, b1), Binary (Binary_op.Plus, Cst (`Constant, b2), e3) ->
      add (Bitvector.add b1 b2 |> constant ~region:`Constant) e3
    | Cst (`Constant, b1), Binary (Binary_op.Minus, Cst (`Constant, b2), e3) ->
      sub (Bitvector.add b1 b2 |> constant ~region:`Constant) e3
    | Cst (`Constant, b1), Binary (Binary_op.Minus, e3, Cst (`Constant, b2)) ->
      add (Bitvector.sub b1 b2 |> constant ~region:`Constant) e3
    | Cst (`Constant, b1), _ when Bitvector.is_zeros b1 -> e2
    | _, Cst (`Constant, _) -> add e2 e1
    | _, Binary (Binary_op.Minus, e3, e4) when equal e1 e4 -> e3
    | Binary (Binary_op.Minus, e3, e4), _ when equal e4 e2 -> e3
    | _, _ -> Straight.add e1 e2

  and sub e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.sub b1 b2 |> constant ~region:`Constant
    | Cst (`Constant, b1), Binary (Binary_op.Plus, Cst (`Constant, b2), e3) ->
      sub (Bitvector.sub b1 b2 |> constant ~region:`Constant) e3
    | Cst (`Constant, b1), Binary (Binary_op.Minus, Cst (`Constant, b2), e3) ->
      add (Bitvector.sub b1 b2 |> constant ~region:`Constant) e3
    | Cst (`Constant, b1), Binary (Binary_op.Minus, e3, Cst (`Constant, b2)) ->
      sub (Bitvector.add b1 b2 |> constant ~region:`Constant) e3
    | Cst (`Constant, b1), _ when Bitvector.is_zeros b1 -> uminus e2
    | _, Cst (`Constant, b2) when Bitvector.is_zeros b2 -> e1
    | _, _ when equal e1 e2 -> size_of e1 |> zeros
    | _, Binary (Binary_op.Plus, e3, e4) when equal e1 e3 -> uminus e4
    | _, Binary (Binary_op.Plus, e3, e4) when equal e1 e4 -> uminus e3
    | Binary (Binary_op.Plus, e3, e4), _ when equal e2 e3 -> e4
    | Binary (Binary_op.Plus, e3, e4), _ when equal e2 e4 -> e3
    | _, Binary (Binary_op.Minus, e3, e4) when equal e1 e3 -> e4
    | Binary (Binary_op.Minus, e3, e4), _ when equal e2 e3 -> uminus e4
    | _, _ -> Straight.sub e1 e2

  and mul e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.mul b1 b2 |> constant ~region:`Constant
    (* neutral element *)
    | Cst (`Constant, b1), _ when Bitvector.is_ones b1 -> e2
    (* abosrbing element *)
    | Cst (`Constant, b1), _ when Bitvector.is_zeros b1 -> e1
    | _, Cst (`Constant, _) -> mul e2 e1
    | _, _ -> Straight.mul e1 e2

  and udiv e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.udiv b1 b2 |> constant ~region:`Constant
    (* neutral element *)
    | _, Cst (`Constant, b2) when Bitvector.is_ones b2 -> e1
    | _, _ -> Straight.udiv e1 e2

  and umod e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.umod b1 b2 |> constant ~region:`Constant
    | _, _ -> Straight.umod e1 e2

  and sdiv e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.sdiv b1 b2 |> constant ~region:`Constant
    (* neutral element *)
    | _, Cst (`Constant, b2) when Bitvector.is_ones b2 -> e1
    | _, _ -> Straight.sdiv e1 e2

  and smod e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.smod b1 b2 |> constant ~region:`Constant
    | _, _ -> Straight.smod e1 e2

  and logxor e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.logxor b1 b2 |> constant ~region:`Constant
    | Cst (`Constant, b1), _ when Bitvector.is_zeros b1 -> e2
    | Cst (`Constant, b1), _ when Bitvector.is_fill b1 -> lognot e2
    | _, Cst (`Constant, _) -> logxor e2 e1
    | _, _ when equal e1 e2 -> size_of e1 |> zeros
    | _, _ -> Straight.logxor e1 e2

  and logor e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.logor b1 b2 |> constant ~region:`Constant
    | Cst (`Constant, b1), _ when Bitvector.is_zeros b1 -> e2
    | Cst (`Constant, b1), _ when Bitvector.is_fill b1 -> e1
    | _, Cst (`Constant, _) -> logor e2 e1
    | _, _ -> Straight.logor e1 e2

  and logand e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.logand b1 b2 |> constant ~region:`Constant
    | Cst (`Constant, b1), _ when Bitvector.is_zeros b1 -> e1
    | Cst (`Constant, b1), _ when Bitvector.is_fill b1 -> e2
    | _, Cst (`Constant, _) -> logand e2 e1
    | _, _ -> Straight.logand e1 e2

  and append e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.append b1 b2 |> constant ~region:`Constant
    | Cst (`Constant, b1), _ when Bitvector.is_zeros b1 ->
      e2 |> uext @@ size_of e1 + size_of e2
    | Unary (Unary_op.Restrict {Interval.lo; Interval.hi}, e1),
      Unary (Unary_op.Restrict {Interval.lo=lo'; Interval.hi=hi'}, e2)
      when hi + 1 = lo' && equal e1 e2 ->
      restrict lo hi' e1
    | Binary ((Binary_op.And | Binary_op.Or | Binary_op.Xor) as op, e1, e2),
      Binary (op', e1', e2') when op = op' ->
      append e2 e2' |> binary op @@ append e1 e1'
    | _, _ -> Straight.append e1 e2

  and eq e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.equal b1 b2 |> Bitvector.of_bool |> constant ~region:`Constant
    | _, _ when equal e1 e2 -> one
    | _, _ -> Straight.eq e1 e2

  and diff e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.diff b1 b2 |> Bitvector.of_bool |> constant ~region:`Constant
    | _, _ when equal e1 e2 -> zero
    | _, _ -> Straight.diff e1 e2

  and ult e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.ult b1 b2 |> Bitvector.of_bool |> constant ~region:`Constant
    | _, _ when equal e1 e2 -> zero
    | _, _ -> Straight.ult e1 e2

  and ule e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.ule b1 b2 |> Bitvector.of_bool |> constant ~region:`Constant
    | _, _ when equal e1 e2 -> one
    | _, _ -> Straight.ule e1 e2

  and ugt e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.ugt b1 b2 |> Bitvector.of_bool |> constant ~region:`Constant
    | _, _ when equal e1 e2 -> zero
    | _, _ -> Straight.ugt e1 e2

  and uge e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.uge b1 b2 |> Bitvector.of_bool |> constant ~region:`Constant
    | _, _ when equal e1 e2 -> one
    | _, _ -> Straight.uge e1 e2

  and slt e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.slt b1 b2 |> Bitvector.of_bool |> constant ~region:`Constant
    | _, _ when equal e1 e2 -> zero
    | _, _ -> Straight.slt e1 e2

  and sle e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.sle b1 b2 |> Bitvector.of_bool |> constant ~region:`Constant
    | _, _ when equal e1 e2 -> one
    | _, _ -> Straight.sle e1 e2

  and sgt e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.sgt b1 b2 |> Bitvector.of_bool |> constant ~region:`Constant
    | _, _ when equal e1 e2 -> zero
    | _, _ -> Straight.sgt e1 e2

  and sge e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.sge b1 b2 |> Bitvector.of_bool |> constant ~region:`Constant
    | _, _ when equal e1 e2 -> one
    | _, _ -> Straight.sge e1 e2

  and rotate_left e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.value_of b2 |> Bigint.int_of_big_int |>
      Bitvector.rotate_left b1 |> constant ~region:`Constant
    | _, Cst (`Constant, b2) when Bitvector.is_zeros b2 -> e1
    | _, _ -> Straight.rotate_left e1 e2

  and rotate_right e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.value_of b2 |> Bigint.int_of_big_int |>
      Bitvector.rotate_right b1 |> constant ~region:`Constant
    | _, Cst (`Constant, b2) when Bitvector.is_zeros b2 -> e1
    | _, _ -> Straight.rotate_right e1 e2

  and shift_left e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.value_of b2 |> Bigint.int_of_big_int |>
      Bitvector.shift_left b1 |> constant ~region:`Constant
    | _, Cst (`Constant, b2) when Bitvector.is_zeros b2 -> e1
    | _, _ -> Straight.shift_left e1 e2

  and shift_right e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.value_of b2 |> Bigint.int_of_big_int |>
      Bitvector.shift_right b1 |> constant ~region:`Constant
    | _, Cst (`Constant, b2) when Bitvector.is_zeros b2 -> e1
    | _, _ -> Straight.shift_right e1 e2

  and shift_right_signed e1 e2 = match e1, e2 with
    | Cst (`Constant, b1), Cst (`Constant, b2) ->
      Bitvector.value_of b2 |> Bigint.int_of_big_int |>
      Bitvector.shift_right_signed b1 |> constant ~region:`Constant
    | _, Cst (`Constant, b2) when Bitvector.is_zeros b2 -> e1
    | _, _ -> Straight.shift_right_signed e1 e2

  and binary op = match op with
    | Plus        -> add
    | Minus       -> sub
    | Mult        -> mul
    | DivU        -> udiv
    | DivS        -> sdiv
    | ModU        -> umod
    | ModS        -> smod
    | And         -> logand
    | Or          -> logor
    | Xor         -> logxor
    | Concat      -> append
    | LShift      -> shift_left
    | RShiftU     -> shift_right
    | RShiftS     -> shift_right_signed
    | LeftRotate  -> rotate_left
    | RightRotate -> rotate_right
    | Eq          -> eq
    | Diff        -> diff
    | LeqU        -> ule
    | LtU         -> ult
    | LeqS        -> sle
    | LtS         -> slt
    | GeqU        -> uge
    | GtU         -> ugt
    | GeqS        -> sge
    | GtS         -> sgt
end

and Flag : sig
  type t =
    | Cmp of Expr.t * Expr.t
    | Sub of Expr.t * Expr.t
    | Test of Expr.t * Expr.t
    | Unspecified

  val comparison  : Expr.t -> Expr.t -> t
  val subtraction : Expr.t -> Expr.t -> t
  val test        : Expr.t -> Expr.t -> t
  val unspecified : t
end = struct
  type t =
    | Cmp of Expr.t * Expr.t
    | Sub of Expr.t * Expr.t
    | Test of Expr.t * Expr.t
    | Unspecified

  let comparison e1 e2 = Cmp(e1, e2)
  let subtraction e1 e2 = Sub(e1, e2)
  let test e1 e2 = Test(e1, e2)
  let unspecified = Unspecified
end

and VarTag : sig
  type t =
    | Flag of Flag.t
    | Temp

  val flag : Flag.t -> t
  val temp : t
end = struct
  type t =
    | Flag of Flag.t
    | Temp

  let flag f = Flag f
  let temp = Temp
end


type exprs = Expr.t list

type printable =
  | Exp of Expr.t
  | Str of string

module Tag = struct
  type t = tag
  let equal t1 t2 = Pervasives.compare t1 t2 = 0
end

module Jump_target = struct
  type 'a t = 'a jump_target

  let inner n = JInner n
  let outer a = JOuter a

  let is_inner = function
    | JInner _ -> true
    | JOuter _ -> false

  let is_outer = function
    | JOuter _ -> true
    | JInner _ -> false
end


module type INSTR = sig
  type t
  include Sigs.Arithmetic with type t := t
  include Sigs.Bitwise with type t := t
end


module LValue = struct
  type t =
    | Var of string * size * VarTag.t option (* size in bits *)
    | Restrict of string * size * int Interval.t
    | Store of size * endianness * Expr.t  (* size in bytes *)

  let equal lv1 lv2 =
    match lv1, lv2 with
    | Var (x1, sz1, _), Var (x2, sz2, _) ->
      x1 = x2 && sz1 = sz2
    | Restrict (x1, sz1, {Interval.lo=o11; Interval.hi=o12}),
      Restrict (x2, sz2, {Interval.lo=o21; Interval.hi=o22}) ->
      x1 = x2 && sz1 = sz2 && o11 = o21 && o12 = o22
    | Store (sz1, en1, e1), Store (sz2, en2, e2) ->
      sz1 = sz2 && en1 = en2 && Expr.equal e1 e2
    | _, _ -> false

  let size_of = function
    | Var (_, sz, _) -> sz
    | Restrict (_, sz, {Interval.lo; Interval.hi}) ->
      let restricted_size = hi - lo + 1 in
      assert (restricted_size <= sz);
      restricted_size
    | Store (sz, _, _) -> 8 * sz

  let var ~bitsize name tagopt =
    let bsz = Size.Bit.to_int bitsize in
    Var(name, bsz, tagopt)

  let flag
      ?(bitsize=Size.Bit.bits1)
      ?(flag_t=Flag.unspecified) flagname =
    var flagname ~bitsize (Some (VarTag.flag flag_t))

  let temporary tempname bitsize = var tempname ~bitsize (Some VarTag.temp)

  let temp nbits =
    let name = Format.asprintf "temp%a" Size.Bit.pp nbits in
    temporary name nbits

  let restrict name sz lo hi =
    assert(lo <= hi);
    assert(lo >= 0);
    let bsz = Size.Bit.to_int sz in
    assert(hi < bsz); (* TODO? : create a pure variable if hi - lo + 1 = sz *)
    Restrict(name, bsz, {Interval.lo; Interval.hi})

  let bit_restrict name sz bit = restrict name sz bit bit

  let store nbytes endianness e =
    let sz = Size.Byte.to_int nbytes in
(*    Format.printf "store : %d@." (Expr.size_of e); *)
    assert (Expr.size_of e = Machine.Word_size.get ());
    Store(sz, endianness, e)

  let is_expr_translatable = function
    | Expr.Var _
    | Expr.Load _
    | Expr.Unary(Unary_op.Restrict _ , Expr.Var _) -> true
    | Expr.Cst _
    | Expr.Unary _
    | Expr.Binary _
    | Expr.Ite _ -> false

  let of_expr = function
    | Expr.Var(name, sz, tag) ->
      var name ~bitsize:(Size.Bit.create sz) tag
    | Expr.Load(size, endian, e) ->
      store (Size.Byte.create size) endian e
    | Expr.Unary(Unary_op.Restrict {Interval.lo; Interval.hi},
                 Expr.Var(name, sz, _)) ->
      restrict name (Size.Bit.create sz) lo hi
    | Expr.Cst _
    | Expr.Unary _
    | Expr.Binary _
    | Expr.Ite _ ->
      failwith "LValue.of_expr : Cannot create lvalue from expression"

  let to_expr = function
    | Var(name,size,tag) -> Expr.var name size tag
    | Restrict(name,size, { Interval.lo; hi}) ->
       Expr.restrict lo hi (Expr.var name size None)
    | Store(size,endianness,address) ->
       Expr.load (Size.Byte.create size) endianness address


  (* size expected for rhs *)
  let bitsize = function
    | Var (_vname, sz, _tag) ->
      Size.Bit.create sz
    | Restrict(_vname, sz, {Interval.lo; Interval.hi}) ->
      let res = hi - lo + 1 in
      assert (sz >= res);
      Size.Bit.create res
    | Store (sz, _endianness, _e) ->
      Size.Byte.(create sz |> to_bitsize)

  let resize size = function
    | Var (vname, _sz, tag) -> var vname ~bitsize:size tag
    | Restrict(vname, _sz, {Interval.lo; Interval.hi}) ->
      restrict vname size lo hi
    | Store (_sz, endianness, e) -> store size endianness e
end


module Instr = struct

  type t =
    | Assign of LValue.t * Expr.t *  id
    | SJump of id jump_target * tag option
    | DJump of Expr.t * tag option
    | If of Expr.t * id jump_target * id
    | Stop of state option
    | Assert of Expr.t * id
    | Assume of Expr.t * id
    | NondetAssume of LValue.t list * Expr.t * id
    | Nondet of LValue.t * region * id
    | Undef of LValue.t * id
    | Malloc of LValue.t * Expr.t * id
    | Free of Expr.t * id
    | Print of printable list * id



  let assign lval rval nid =
    let lval_sz = LValue.bitsize lval |> Size.Bit.to_int
    and rval_sz = Expr.size_of rval in
    assert ( lval_sz = rval_sz );
    Assign(lval, rval, nid)

  let static_jump ?(tag=None) jt = SJump (jt, tag)

  let static_inner_jump ?(tag=None) n =
    static_jump (Jump_target.inner n) ~tag

  let call ~return_address jt =
    let tag = Some (Call return_address) in
    static_jump ~tag jt

  let dynamic_jump ?(tag=None) e = DJump (e, tag)

  let stop state = Stop state

  let ite c goto nid = If (c, goto, nid)

  let undefined lv nid = Undef (lv, nid)

  let non_deterministic lv r nid = Nondet (lv, r, nid)

  let malloc lv e nid = Malloc (lv, e, nid)

  let free e id = Free (e, id)

  let _assert c nid = Assert (c, nid)

  let assume c nid = Assume (c, nid)

  let non_deterministic_assume lvs c id = NondetAssume (lvs, c, id)

  let print args id = Print (args, id)
end
