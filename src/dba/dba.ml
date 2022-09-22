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

(** Definition of DBA type *)

let invalid_boolean = Invalid_argument "not a boolean expression"

let bad_bound = Invalid_argument "index out of bound"

let mismatched_operands = Invalid_argument "mismatched operands size"

let invalid_assignment = Invalid_argument "mismatched assign"

type size = int

type malloc_size = Z.t

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

type tag = Call of address | Return  (** For call address of return site *)

type state =
  | OK
  | KO
  | Undecoded of string  (** Stop because of unanticipated string of bytes **)
  | Unsupported of string  (** Stop because instr is not supported by Binsec **)

type 'a var = { name : string; size : size; info : 'a }
(** The base type for variables *)

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
    | Eq -> Diff
    | Diff -> Eq
    | LeqU -> GtU
    | LtU -> GeqU
    | GeqU -> LtU
    | GtU -> LeqU
    | LeqS -> GtS
    | LtS -> GeqS
    | GeqS -> LtS
    | GtS -> LeqS
    | _ -> failwith "BinaryOperator.invert"

  let has_inverse = function
    | Eq | Diff | LeqU | LtU | GeqU | GtU | LeqS | LtS | GeqS | GtS -> true
    | _ -> false
end

type malloc_status = Freed | Freeable

type restricted_region = [ `Stack | `Malloc of (int * address) * Z.t ]

type region = [ `Constant | restricted_region ]

module VarTag = struct
  type attribute = Value | Size | Last

  let pp_attribute ppf = function
    | Value -> ()
    | Size -> Format.pp_print_string ppf ":size"
    | Last -> Format.pp_print_string ppf ":last"

  type t =
    | Flag
    | Temp
    | Register
    | Symbol of attribute * Bitvector.t lazy_t
    | Empty
end

module Expr : sig
  type t = private
    | Var of VarTag.t var (* size: bits *)
    | Load of size * Machine.endianness * t (* size: bytes *)
    | Cst of Bitvector.t
    | Unary of Unary_op.t * t
    | Binary of Binary_op.t * t * t
    | Ite of t * t * t
  (* sugar operator *)

  val v : VarTag.t var -> t

  val var : ?tag:VarTag.t -> string -> int -> t

  val is_equal : t -> t -> bool

  val size_of : t -> int
  (*
   *   val var : Size.Bit.t -> string -> Tag.t option -> t
   *   val flag : ?bits:Size.Bit.t -> string -> t
   *
   *)

  val is_constant : t -> bool

  val constant : Bitvector.t -> t

  val temporary : size:int -> string -> t

  val zeros : int -> t

  val ones : int -> t

  val one : t

  val _true : t

  val zero : t

  val _false : t

  val binary : Binary_op.t -> t -> t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val smod : t -> t -> t

  val umod : t -> t -> t

  val udiv : t -> t -> t

  val sdiv : t -> t -> t

  val append : t -> t -> t

  include Sigs.Comparisons with type t := t and type boolean = t

  val unary : Unary_op.t -> t -> t

  val uminus : t -> t

  include Sigs.Logical with type t := t

  val logxor : t -> t -> t

  val shift_left : t -> t -> t

  val shift_right : t -> t -> t

  val shift_right_signed : t -> t -> t

  val rotate_left : t -> t -> t

  val rotate_right : t -> t -> t

  val sext : int -> t -> t

  val uext : int -> t -> t

  val ite : t -> t -> t -> t

  val restrict : int -> int -> t -> t

  val bit_restrict : int -> t -> t

  val load : Size.Byte.t -> Machine.endianness -> t -> t

  val is_max : t -> bool
end = struct
  open Binary_op

  type t =
    | Var of VarTag.t var (* size: bits *)
    | Load of size * Machine.endianness * t (* size: bytes *)
    | Cst of Bitvector.t
    | Unary of Unary_op.t * t
    | Binary of Binary_op.t * t * t
    | Ite of t * t * t
  (* sugar operator *)

  type boolean = t

  let rec size_of = function
    | Cst b -> Bitvector.size_of b
    | Var v -> v.size
    | Load (bytesize, _, _) -> 8 * bytesize
    | Ite (_, e, _) | Unary ((Unary_op.UMinus | Unary_op.Not), e) -> size_of e
    | Unary ((Unary_op.Sext bits | Unary_op.Uext bits), _) -> bits
    | Unary (Unary_op.Restrict { Interval.lo; Interval.hi }, _) -> hi - lo + 1
    | Binary (bop, e1, e2) -> (
        match bop with
        | Concat -> size_of e1 + size_of e2
        | Eq | Diff | LeqU | LtU | GeqU | GtU | LeqS | LtS | GeqS | GtS -> 1
        | Plus | Minus | Mult | DivU | DivS | ModU | ModS | Or | And | Xor
        | LShift | RShiftU | RShiftS | LeftRotate | RightRotate ->
            size_of e1)

  let rec is_equal e1 e2 =
    match (e1, e2) with
    | Var v1, Var v2 -> v1 = v2
    | Load (sz1, en1, e1), Load (sz2, en2, e2) ->
        sz1 = sz2 && en1 = en2 && is_equal e1 e2
    | Cst bv1, Cst bv2 -> Bitvector.equal bv1 bv2
    | Unary (unop1, e1), Unary (unop2, e2) -> unop1 = unop2 && is_equal e1 e2
    | Binary (binop1, lexpr1, rexpr1), Binary (binop2, lexpr2, rexpr2) ->
        binop1 = binop2 && is_equal lexpr1 lexpr2 && is_equal rexpr1 rexpr2
    | Ite (c1, e11, e12), Ite (c2, e21, e22) ->
        is_equal c1 c2 && is_equal e11 e21 && is_equal e12 e22
    | _, _ -> false

  let is_constant = function Cst _ -> true | _ -> false

  let _is_zero = function Cst bv -> Bitvector.is_zeros bv | _ -> false

  let _is_one = function Cst bv -> Bitvector.is_ones bv | _ -> false

  let is_max = function Cst bv -> Bitvector.is_max_ubv bv | _ -> false

  let v va = Var va

  let var ?(tag = VarTag.Empty) name size = Var { name; size; info = tag }

  let temporary ~size name = var name size ~tag:VarTag.Temp

  let constant bv = Cst bv

  let zeros length = constant (Bitvector.zeros length)

  let ones length = constant (Bitvector.ones length)

  let zero = constant Bitvector.zero

  let _false = zero

  let one = constant Bitvector.one

  let _true = one

  let ite condition then_expr else_expr =
    (* Valid conditions are bitvectors of size one  only *)
    if size_of condition <> 1 then raise invalid_boolean;
    if size_of then_expr <> size_of else_expr then raise mismatched_operands;
    match condition with
    | Cst b when Bitvector.is_zero b -> else_expr
    | Cst b when Bitvector.is_one b -> then_expr
    | _ -> Ite (condition, then_expr, else_expr)

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
      if size_of e1 <> size_of e2 then raise mismatched_operands;
      binary op e1 e2

    let add = symmetric_binary Plus

    let sub = symmetric_binary Minus

    let mul = symmetric_binary Mult

    let smod = symmetric_binary ModS

    let umod = symmetric_binary ModU

    let udiv = symmetric_binary DivU

    let sdiv = symmetric_binary DivS

    let logor = symmetric_binary Or

    let logxor = symmetric_binary Xor

    let logand = symmetric_binary And

    let equal = symmetric_binary Eq

    let diff = symmetric_binary Diff

    let ule = symmetric_binary LeqU

    let sle = symmetric_binary LeqS

    let ult = symmetric_binary LtU

    let slt = symmetric_binary LtS

    let uge = symmetric_binary GeqU

    let sge = symmetric_binary GeqS

    let ugt = symmetric_binary GtU

    let sgt = symmetric_binary GtS

    let unary op e = Unary (op, e)

    let lognot = unary Unary_op.Not

    let uminus = unary Unary_op.UMinus

    let sext bits = unary (Unary_op.Sext bits)

    let uext bits = unary (Unary_op.Uext bits)

    let restrict lo hi e =
      if hi >= size_of e || hi < lo || lo < 0 then raise bad_bound;
      unary (Unary_op.Restrict { Interval.lo; Interval.hi }) e
  end

  (* f e1 (e2::e3) -> g (f e1_(|e3|..|e1| - 1) e2) (f e1_(0..|e3| - 1) e3) *)
  let rec split_apply f g e1 e2 e3 =
    let s1 = size_of e1 and s3 = size_of e3 in
    let e1_2 = restrict s3 (s1 - 1) e1 and e1_3 = restrict 0 (s3 - 1) e1 in
    g (f e1_2 e2) (f e1_3 e3)

  (* All the following construction functions are defined w.r.t to the
   * "straight" ones. Hence no "rec" keyword after the let is usually *not* an
   * error
   *)
  and uminus = function
    | Cst bv -> constant (Bitvector.neg bv)
    | Unary (Unary_op.UMinus, e) -> e
    | e -> Straight.uminus e

  and lognot = function
    | Cst bv -> constant (Bitvector.lognot bv)
    | Unary (Unary_op.Not, e) -> e
    | Binary (op, e1, e2) when Binary_op.has_inverse op ->
        binary (Binary_op.invert op) e1 e2
    | e -> Straight.lognot e

  and uext size = function
    | e when size = size_of e -> e
    | Cst bv -> constant Bitvector.(extend bv size)
    | Unary (Unary_op.Uext _, e) | e -> Straight.uext size e

  and sext size = function
    | e when size = size_of e -> e
    | Cst bv -> constant Bitvector.(extend_signed bv size)
    | Unary (Unary_op.Uext _, e) -> Straight.uext size e
    | Unary (Unary_op.Sext _, e) | e -> Straight.sext size e

  and restrict lo hi = function
    | e when lo = 0 && hi = size_of e - 1 -> e
    | Cst bv -> constant (Bitvector.extract bv Interval.{ lo; hi })
    | Load (sz, LittleEndian, addr) when (8 * sz) - hi > 8 ->
        let sz' = Size.Byte.create (sz - (((8 * sz) - hi - 1) / 8)) in
        restrict lo hi (load sz' LittleEndian addr)
    | Load (sz, LittleEndian, addr) when lo >= 8 ->
        let bz' = lo / 8 in
        let lo' = lo - (8 * bz') and hi' = hi - (8 * bz') in
        let sz' = Size.Byte.create (sz - bz') in
        let size = size_of addr in
        let addr' = add addr (constant (Bitvector.of_int ~size bz')) in
        restrict lo' hi' (load sz' LittleEndian addr')
    | Unary (Unary_op.Restrict { Interval.lo = lo'; _ }, e) ->
        Straight.restrict (lo' + lo) (lo' + hi) e
    | Unary (Unary_op.Uext _, e) when size_of e <= lo -> zeros (hi - lo + 1)
    | Unary ((Unary_op.Uext _ | Unary_op.Sext _), e) when size_of e > hi ->
        restrict lo hi e
    | Unary (Unary_op.Uext _, e) ->
        uext (hi - lo + 1) (restrict lo (size_of e - 1) e)
    | Unary (Unary_op.Sext _, e) when lo < size_of e ->
        sext (hi - lo + 1) (restrict (min lo (size_of e - 1)) (size_of e - 1) e)
    | Binary (((Binary_op.And | Binary_op.Or | Binary_op.Xor) as op), e1, e2) ->
        binary op (restrict lo hi e1) (restrict lo hi e2)
    | Binary (Binary_op.LShift, _, Cst b2) when Bitvector.to_uint b2 > hi ->
        zeros (hi - lo + 1)
    | Binary (Binary_op.LShift, e1, Cst b2) when Bitvector.to_uint b2 <= lo ->
        restrict (lo - Bitvector.to_uint b2) (hi - Bitvector.to_uint b2) e1
    | Binary ((Binary_op.RShiftU | Binary_op.RShiftS), e1, Cst b2)
      when size_of e1 > hi + Bitvector.to_uint b2 ->
        restrict (lo + Bitvector.to_uint b2) (hi + Bitvector.to_uint b2) e1
    | Binary (Binary_op.Concat, _, e2) when hi < size_of e2 -> restrict lo hi e2
    | Binary (Binary_op.Concat, e1, e2) when lo >= size_of e2 ->
        restrict (lo - size_of e2) (hi - size_of e2) e1
    | Binary (Binary_op.Concat, e1, e2) ->
        append
          (restrict 0 (hi - size_of e2) e1)
          (restrict lo (size_of e2 - 1) e2)
    | e -> Straight.restrict lo hi e

  and bit_restrict off = restrict off off

  and unary op e =
    match op with
    | Unary_op.Not -> lognot e
    | Unary_op.UMinus -> uminus e
    | Unary_op.Sext s -> sext s e
    | Unary_op.Uext s -> uext s e
    | Unary_op.Restrict { Interval.lo; Interval.hi } -> restrict lo hi e

  and add e1 e2 =
    match (e1, e2) with
    (* Constant propagation *)
    | Cst b1, Cst b2 -> constant (Bitvector.add b1 b2)
    (* Invariant: A constant is always on the right side of the root *)
    | Cst _, _ -> add e2 e1
    | Binary (Binary_op.Plus, e3, Cst b1), Cst b2 ->
        add e3 (constant (Bitvector.add b1 b2))
    | Binary (Binary_op.Plus, e3, (Cst _ as c4)), _ -> add (add e3 e2) c4
    | Binary (Binary_op.Minus, e3, Cst b1), Cst b2 when Bitvector.sge b2 b1 ->
        add e3 (constant (Bitvector.sub b2 b1))
    | Binary (Binary_op.Minus, e3, Cst b1), Cst b2 ->
        sub e3 (constant (Bitvector.sub b1 b2))
    (* Except when it is on the left side of a substraction root *)
    | Binary (Binary_op.Minus, Cst b1, e3), Cst b2 ->
        sub (constant (Bitvector.add b1 b2)) e3
    (* Invariant: Linear structure of expression *)
    | Binary (Binary_op.Plus, _, _), Binary (Binary_op.Plus, e3, e4) ->
        add (add e1 e3) e4
    (* Straightforward elimination *)
    | _, Binary (Binary_op.Minus, e3, e4) when is_equal e1 e4 -> e3
    | Binary (Binary_op.Minus, e3, e4), _ when is_equal e4 e2 -> e3
    (* Neutral element *)
    | _, Cst b2 when Bitvector.is_zeros b2 -> e1
    (* Default *)
    | _, _ -> Straight.add e1 e2

  and sub e1 e2 =
    match (e1, e2) with
    (* Constant propagation *)
    | Cst b1, Cst b2 -> constant (Bitvector.sub b1 b2)
    (* Invariant: Constant is only on: *)
    (* - the left side of the substraction root *)
    (* - the right side of the root *)
    | Cst b1, Binary (Binary_op.Plus, e3, Cst b2) ->
        sub (constant (Bitvector.sub b1 b2)) e3
    | Cst b1, Binary (Binary_op.Minus, e3, Cst b2) ->
        sub (constant (Bitvector.add b1 b2)) e3
    | Cst b1, Binary (Binary_op.Minus, Cst b2, e3) when Bitvector.sge b1 b2 ->
        add e3 (constant (Bitvector.sub b1 b2))
    | Cst b1, Binary (Binary_op.Minus, Cst b2, e3) ->
        sub e3 (constant (Bitvector.sub b2 b1))
    | Binary (Binary_op.Plus, e3, Cst b1), Cst b2 when Bitvector.sge b1 b2 ->
        add e3 (constant (Bitvector.sub b1 b2))
    | Binary (Binary_op.Plus, e3, Cst b1), Cst b2 ->
        sub e3 (constant (Bitvector.sub b2 b1))
    | Binary (Binary_op.Minus, Cst b1, e3), Cst b2 ->
        sub (constant (Bitvector.sub b1 b2)) e3
    | Binary (Binary_op.Minus, e3, Cst b1), Cst b2 ->
        sub e3 (constant (Bitvector.add b1 b2))
    | Binary (Binary_op.Minus, e3, (Cst _ as c4)), _ -> sub (sub e3 e2) c4
    (* Neutral element *)
    | Cst b1, _ when Bitvector.is_zeros b1 -> uminus e2
    | _, Cst b2 when Bitvector.is_zeros b2 -> e1
    (* Straightforward elimination *)
    | _, _ when is_equal e1 e2 -> zeros (size_of e1)
    | _, Binary (Binary_op.Plus, e3, e4) when is_equal e1 e3 -> uminus e4
    | _, Binary (Binary_op.Plus, e3, e4) when is_equal e1 e4 -> uminus e3
    | Binary (Binary_op.Plus, e3, e4), _ when is_equal e2 e3 -> e4
    | Binary (Binary_op.Plus, e3, e4), _ when is_equal e2 e4 -> e3
    | _, Binary (Binary_op.Minus, e3, e4) when is_equal e1 e3 -> e4
    | Binary (Binary_op.Minus, e3, e4), _ when is_equal e2 e3 -> uminus e4
    (* Default *)
    | _, _ -> Straight.sub e1 e2

  and mul e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.mul b1 b2)
    (* neutral element *)
    | Cst b1, _ when Bitvector.is_ones b1 -> e2
    (* abosrbing element *)
    | Cst b1, _ when Bitvector.is_zeros b1 -> e1
    | _, Cst _ -> mul e2 e1
    | _, _ -> Straight.mul e1 e2

  and udiv e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.udiv b1 b2)
    (* neutral element *)
    | _, Cst b2 when Bitvector.is_ones b2 -> e1
    | _, _ -> Straight.udiv e1 e2

  and umod e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.umod b1 b2)
    | _, _ -> Straight.umod e1 e2

  and sdiv e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.sdiv b1 b2)
    (* neutral element *)
    | _, Cst b2 when Bitvector.is_ones b2 -> e1
    | _, _ -> Straight.sdiv e1 e2

  and smod e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.smod b1 b2)
    | _, _ -> Straight.smod e1 e2

  and logxor e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.logxor b1 b2)
    | Cst b1, _ when Bitvector.is_zeros b1 -> e2
    | Cst b1, _ when Bitvector.is_fill b1 -> lognot e2
    | Cst b1, Binary (Binary_op.Concat, e3, e4) ->
        let s2 = size_of e2 and s4 = size_of e4 in
        let b3 =
          Bitvector.extract b1 { Interval.lo = s4; Interval.hi = s2 - 1 }
        in
        let b4 =
          Bitvector.extract b1 { Interval.lo = 0; Interval.hi = s4 - 1 }
        in
        let x3 = logxor (constant b3) e3 in
        let x4 = logxor (constant b4) e4 in
        append x3 x4
    | _, Cst _ -> logxor e2 e1
    | _, _ when is_equal e1 e2 -> zeros (size_of e1)
    | _, _ -> Straight.logxor e1 e2

  and logor e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.logor b1 b2)
    | Cst b1, _ when Bitvector.is_zeros b1 -> e2
    | Cst b1, _ when Bitvector.is_fill b1 -> e1
    | Cst b1, Binary (Binary_op.Concat, e3, e4) ->
        let s2 = size_of e2 and s4 = size_of e4 in
        let b3 =
          Bitvector.extract b1 { Interval.lo = s4; Interval.hi = s2 - 1 }
        in
        let b4 =
          Bitvector.extract b1 { Interval.lo = 0; Interval.hi = s4 - 1 }
        in
        let x3 = logor (constant b3) e3 in
        let x4 = logor (constant b4) e4 in
        append x3 x4
    | _, Cst _ -> logor e2 e1
    | _, Unary (Unary_op.Uext _, e3) ->
        try_merge ~k:(fun _ -> Straight.logor e1 e2) e3 ~at:0 e1
    | _, Binary (Binary_op.LShift, Unary (Unary_op.Uext _, e3), Cst b1) ->
        let at = Bitvector.to_uint b1 in
        try_merge ~k:(fun _ -> Straight.logor e1 e2) e3 ~at e1
    | Unary (Unary_op.Uext _, _), _
    | Binary (Binary_op.LShift, Unary (Unary_op.Uext _, _), Cst _), _ ->
        logor e2 e1
    | _, _ when is_equal e1 e2 -> e1
    | _, _ -> Straight.logor e1 e2

  and logand e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.logand b1 b2)
    | Cst b1, _ when Bitvector.is_zeros b1 -> e1
    | Cst b1, _ when Bitvector.is_fill b1 -> e2
    | _, Cst _ -> logand e2 e1
    | Cst b1, Binary (Binary_op.Concat, e3, e4) ->
        let rec try_refine ~f ~k b e1 e2 =
          let hi = Bitvector.size_of b - 1 in
          let lo = hi - size_of e1 + 1 in
          let b1 = Bitvector.extract b { Interval.lo; hi } in
          if Bitvector.is_fill b1 || Bitvector.is_zeros b1 then
            let e1 = if Bitvector.is_fill b1 then e1 else constant b1 in
            let b2 = Bitvector.extract b { Interval.lo = 0; hi = lo - 1 } in
            match e2 with
            | Binary (Binary_op.Concat, e3, e4) ->
                try_refine ~f ~k:(fun r -> k (append e1 r)) b2 e3 e4
            | _ -> k (append e1 (logand (constant b2) e2))
          else f ()
        in
        try_refine ~f:(fun _ -> Straight.logand e1 e2) ~k:(fun x -> x) b1 e3 e4
    | _, _ when is_equal e1 e2 -> e1
    | _, _ -> Straight.logand e1 e2

  and append e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.append b1 b2)
    | Cst b1, _ when Bitvector.is_zeros b1 -> uext (size_of e1 + size_of e2) e2
    | Unary (Unary_op.Uext s1, e3), _ -> uext (s1 + size_of e2) (append e3 e2)
    | _, Unary (Unary_op.Uext s, e2) ->
        Straight.append e1 (Straight.append (zeros (s - size_of e2)) e2)
    | ( Unary (Unary_op.Restrict { Interval.lo; Interval.hi }, e1),
        Unary (Unary_op.Restrict { Interval.lo = lo'; Interval.hi = hi' }, e2) )
      when hi' + 1 = lo && is_equal e1 e2 ->
        restrict lo' hi e1
    | ( Binary (((Binary_op.And | Binary_op.Or | Binary_op.Xor) as op), e1, e2),
        Binary (op', e1', e2') )
      when op = op' ->
        binary op (append e1 e1') (append e2 e2')
    | Binary (Binary_op.Concat, e3, e4), _ -> append e3 (append e4 e2)
    | _, _ -> Straight.append e1 e2

  and equal e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.of_bool (Bitvector.equal b1 b2))
    | Cst _, Binary (Binary_op.Concat, e3, e4) ->
        split_apply equal logand e1 e3 e4
    | Cst _, Unary (Unary_op.Uext n, e3) ->
        split_apply equal logand e1 (constant (Bitvector.zeros n)) e3
    | Binary (Binary_op.Concat, e3, e4), Binary (Binary_op.Concat, e5, e6)
      when size_of e3 = size_of e5 ->
        logand (equal e3 e4) (equal e4 e6)
    | _, Cst _ -> equal e2 e1
    | _, _ when is_equal e1 e2 -> one
    | _, _ -> Straight.equal e1 e2

  and diff e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.of_bool (Bitvector.diff b1 b2))
    | Cst _, Binary (Binary_op.Concat, e3, e4) ->
        split_apply diff logor e1 e3 e4
    | Cst _, Unary (Unary_op.Uext n, e3) ->
        split_apply diff logor e1 (constant (Bitvector.zeros n)) e3
    | Binary (Binary_op.Concat, e3, e4), Binary (Binary_op.Concat, e5, e6)
      when size_of e3 = size_of e5 ->
        logor (diff e3 e4) (diff e4 e6)
    | _, Cst _ -> diff e2 e1
    | _, _ when is_equal e1 e2 -> zero
    | _, _ -> Straight.diff e1 e2

  and ult e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.of_bool (Bitvector.ult b1 b2))
    | _, _ when is_equal e1 e2 -> zero
    | _, _ -> Straight.ult e1 e2

  and ule e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.of_bool (Bitvector.ule b1 b2))
    | _, _ when is_equal e1 e2 -> one
    | _, _ -> Straight.ule e1 e2

  and ugt e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.of_bool (Bitvector.ugt b1 b2))
    | _, _ when is_equal e1 e2 -> zero
    | _, _ -> Straight.ugt e1 e2

  and uge e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.of_bool (Bitvector.uge b1 b2))
    | _, _ when is_equal e1 e2 -> one
    | _, _ -> Straight.uge e1 e2

  and slt e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.of_bool (Bitvector.slt b1 b2))
    | _, _ when is_equal e1 e2 -> zero
    | _, _ -> Straight.slt e1 e2

  and sle e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.of_bool (Bitvector.sle b1 b2))
    | _, _ when is_equal e1 e2 -> one
    | _, _ -> Straight.sle e1 e2

  and sgt e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.of_bool (Bitvector.sgt b1 b2))
    | _, _ when is_equal e1 e2 -> zero
    | _, _ -> Straight.sgt e1 e2

  and sge e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 -> constant (Bitvector.of_bool (Bitvector.sge b1 b2))
    | _, _ when is_equal e1 e2 -> one
    | _, _ -> Straight.sge e1 e2

  and rotate_left e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 ->
        constant (Bitvector.rotate_left b1 (Bitvector.to_uint b2))
    | _, Cst b2 when Bitvector.is_zeros b2 -> e1
    | _, _ -> Straight.rotate_left e1 e2

  and rotate_right e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 ->
        constant (Bitvector.rotate_right b1 (Bitvector.to_uint b2))
    | _, Cst b2 when Bitvector.is_zeros b2 -> e1
    | _, _ -> Straight.rotate_right e1 e2

  and shift_left e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 ->
        constant (Bitvector.shift_left b1 (Bitvector.to_uint b2)) (* w << 0 *)
    | _, Cst b2 when Bitvector.is_zeros b2 -> e1 (* 0 << w *)
    | Cst b1, _ when Bitvector.is_zeros b1 -> e1
    | _, _ -> Straight.shift_left e1 e2

  and shift_right e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 ->
        constant (Bitvector.shift_right b1 (Bitvector.to_uint b2)) (* w >> 0 *)
    | _, Cst b2 when Bitvector.is_zeros b2 -> e1 (* 0 >> w *)
    | Cst b1, _ when Bitvector.is_zeros b1 -> e1
    | _, _ -> Straight.shift_right e1 e2

  and shift_right_signed e1 e2 =
    match (e1, e2) with
    | Cst b1, Cst b2 ->
        constant (Bitvector.shift_right_signed b1 (Bitvector.to_uint b2))
        (* w >> 0 *)
    | _, Cst b2 when Bitvector.is_zeros b2 -> e1 (* 0 >> w *)
    | Cst b1, _ when Bitvector.is_zeros b1 -> e1
    | _, _ -> Straight.shift_right_signed e1 e2

  and try_merge ~k e1 ~at e2 =
    let sz1 = size_of e1 and sz2 = size_of e2 in
    if at + sz1 <= sz2 && restrict at (at + sz1 - 1) e2 = zeros sz1 then
      if at = 0 then append (restrict sz1 (sz2 - 1) e2) e1
      else if at + sz1 = sz2 then append e1 (restrict 0 (at - 1) e2)
      else
        append
          (append (restrict (at + sz1) (sz2 - 1) e2) e1)
          (restrict 0 (at - 1) e2)
    else k ()

  and binary op =
    match op with
    | Plus -> add
    | Minus -> sub
    | Mult -> mul
    | DivU -> udiv
    | DivS -> sdiv
    | ModU -> umod
    | ModS -> smod
    | And -> logand
    | Or -> logor
    | Xor -> logxor
    | Concat -> append
    | LShift -> shift_left
    | RShiftU -> shift_right
    | RShiftS -> shift_right_signed
    | LeftRotate -> rotate_left
    | RightRotate -> rotate_right
    | Eq -> equal
    | Diff -> diff
    | LeqU -> ule
    | LtU -> ult
    | LeqS -> sle
    | LtS -> slt
    | GeqU -> uge
    | GtU -> ugt
    | GeqS -> sge
    | GtS -> sgt
end

type exprs = Expr.t list

type printable = Exp of Expr.t | Str of string

module Tag = struct
  type t = tag

  let equal = ( = )
end

module Jump_target = struct
  type 'a t = 'a jump_target

  let inner n = JInner n

  let outer a = JOuter a

  let is_inner = function JInner _ -> true | JOuter _ -> false

  let is_outer = function JOuter _ -> true | JInner _ -> false
end

module type INSTR = sig
  type t

  include Sigs.Arithmetic with type t := t

  include Sigs.Bitwise with type t := t
end

module Var = struct
  type t = VarTag.t var

  let create name ~bitsize ~tag =
    { name; size = Size.Bit.to_int bitsize; info = tag }

  let flag ?(bitsize = Size.Bit.bits1) flagname =
    create flagname ~bitsize ~tag:VarTag.Flag

  let temporary tempname bitsize = create tempname ~bitsize ~tag:VarTag.Temp

  let temp nbits =
    let name = Format.asprintf "temp%a" Size.Bit.pp nbits in
    temporary name nbits
end

module LValue = struct
  type t =
    | Var of VarTag.t var (* size in bits *)
    | Restrict of VarTag.t var * int Interval.t
    | Store of size * Machine.endianness * Expr.t
  (* size in bytes *)

  let equal lv1 lv2 =
    match (lv1, lv2) with
    | Var { name = x1; size = sz1; _ }, Var { name = x2; size = sz2; _ } ->
        x1 = x2 && sz1 = sz2
    | ( Restrict (v1, { Interval.lo = o11; Interval.hi = o12 }),
        Restrict (v2, { Interval.lo = o21; Interval.hi = o22 }) ) ->
        v1.name = v2.name && v1.size = v2.size && o11 = o21 && o12 = o22
    | Store (sz1, en1, e1), Store (sz2, en2, e2) ->
        sz1 = sz2 && en1 = en2 && Expr.is_equal e1 e2
    | _, _ -> false

  let size_of = function
    | Var v -> v.size
    | Restrict (_, { Interval.lo; Interval.hi }) ->
        let restricted_size = hi - lo + 1 in
        restricted_size
    | Store (sz, _, _) -> 8 * sz

  let v va = Var va

  let var ?(tag = VarTag.Empty) ~bitsize name =
    let size = Size.Bit.to_int bitsize in
    Var { name; size; info = tag }

  let flag ?(bitsize = Size.Bit.bits1) flagname =
    var flagname ~bitsize ~tag:VarTag.Flag

  let temporary tempname bitsize = var tempname ~bitsize ~tag:VarTag.Temp

  let temp nbits =
    let name = Format.asprintf "temp%a" Size.Bit.pp nbits in
    temporary name nbits

  let restrict v lo hi =
    if hi >= v.size || hi < lo || lo < 0 then raise bad_bound;
    if hi - lo + 1 = v.size then Var v
    else Restrict (v, { Interval.lo; Interval.hi })

  let _restrict name sz lo hi =
    let v = { name; size = Size.Bit.to_int sz; info = VarTag.Empty } in
    restrict v lo hi

  let bit_restrict v bit = restrict v bit bit

  let _bit_restrict name sz bit = _restrict name sz bit bit

  let store nbytes endianness e =
    let sz = Size.Byte.to_int nbytes in
    (*    Format.printf "store : %d@." (Expr.size_of e); *)
    Store (sz, endianness, e)

  let is_expr_translatable = function
    | Expr.Var _ | Expr.Load _ | Expr.Unary (Unary_op.Restrict _, Expr.Var _) ->
        true
    | Expr.Cst _ | Expr.Unary _ | Expr.Binary _ | Expr.Ite _ -> false

  let of_expr = function
    | Expr.Var { name; size = sz; info = tag } ->
        var ~bitsize:(Size.Bit.create sz) ~tag name
    | Expr.Load (size, endian, e) -> store (Size.Byte.create size) endian e
    | Expr.Unary (Unary_op.Restrict { Interval.lo; Interval.hi }, Expr.Var v) ->
        restrict v lo hi
    | Expr.Cst _ | Expr.Unary _ | Expr.Binary _ | Expr.Ite _ ->
        failwith "LValue.of_expr : Cannot create lvalue from expression"

  let to_expr = function
    | Var { name; size; info = tag } -> Expr.var name size ~tag
    | Restrict (v, { Interval.lo; hi }) ->
        Expr.restrict lo hi (Expr.var v.name v.size ~tag:v.info)
    | Store (size, endianness, address) ->
        Expr.load (Size.Byte.create size) endianness address

  (* size expected for rhs *)
  let bitsize = function
    | Var { size; _ } -> Size.Bit.create size
    | Restrict (_, { Interval.lo; Interval.hi }) ->
        let res = hi - lo + 1 in
        Size.Bit.create res
    | Store (sz, _endianness, _e) -> Size.Byte.(to_bitsize (create sz))

  let resize size = function
    | Var { name; info = tag; _ } -> var name ~bitsize:size ~tag
    | Restrict (v, { Interval.lo; Interval.hi }) -> restrict v lo hi
    | Store (_sz, endianness, e) ->
        store (Size.Byte.of_bitsize size) endianness e
end

module Instr = struct
  type t =
    | Assign of LValue.t * Expr.t * id
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
    if Size.Bit.to_int (LValue.bitsize lval) <> Expr.size_of rval then
      raise invalid_assignment;
    Assign (lval, rval, nid)

  let static_jump ?tag jt = SJump (jt, tag)

  let static_inner_jump ?tag n = static_jump (Jump_target.inner n) ?tag

  let static_outer_jump ?tag base =
    static_jump (Jump_target.outer { base; id = 0 }) ?tag

  let call ~return_address jt =
    let tag = Some (Call return_address) in
    static_jump ?tag jt

  let dynamic_jump ?tag e =
    match e with
    | Expr.Cst v ->
        let addr = { id = 0; base = Virtual_address.of_bitvector v } in
        static_jump (Jump_target.outer addr)
    | _ -> DJump (e, tag)

  let stop state = Stop state

  let ite c goto nid =
    if Expr.(is_equal c zero) then static_inner_jump nid
    else if Expr.(is_equal c one) then static_jump goto
    else If (c, goto, nid)

  let undefined lv nid = Undef (lv, nid)

  let non_deterministic ?(region = `Constant) lv nid = Nondet (lv, region, nid)

  let malloc lv e nid = Malloc (lv, e, nid)

  let free e id = Free (e, id)

  let _assert c nid = Assert (c, nid)

  let assume c nid = Assume (c, nid)

  let non_deterministic_assume lvs c id = NondetAssume (lvs, c, id)

  let print args id = Print (args, id)
end
