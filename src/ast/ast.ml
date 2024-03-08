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

module Obj = struct
  type t = ..
end

type 'a loc = 'a * Lexing.position

let loc a position = (a, position)

let pp_range ppf { Interval.hi; lo } =
  if hi = lo then Format.fprintf ppf "{%d}" hi
  else Format.fprintf ppf "{%d..%d}" hi lo

module Symbol = struct
  type t = string * Dba.Var.Tag.attribute

  let create ?(attr = Dba.Var.Tag.Value) name = (name, attr)

  let pp ppf (name, attr) =
    Format.fprintf ppf "<%s%a>" name Dba.Var.Tag.pp_attribute attr
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
end = struct
  type t =
    | Implicit
    | Explicit of int
    | Sizeof of Loc.t loc
    | Eval of Expr.t loc

  let none = Implicit
  let some size = Explicit size
  let sizeof lval = Sizeof lval
  let eval expr = Eval expr

  let pp ppf = function
    | Implicit -> ()
    | Explicit n -> Format.fprintf ppf "<%d>" n
    | Sizeof (lval, _) -> Format.fprintf ppf "<sizeof(%a)>" Loc.pp lval
    | Eval (expr, _) -> Format.fprintf ppf "<%a>" Expr.pp expr
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
end = struct
  type t =
    | Var of string * Size.t
    | Load of int * Machine.endianness option * Expr.t loc * string option
    | Sub of int Interval.t * t loc

  let var ?(size = Size.none) name = Var (name, size)
  let load ?array len ?endianness addr = Load (len, endianness, addr, array)
  let restrict ~hi ~lo t = Sub ({ hi; lo }, t)

  let rec pp ppf lval =
    match lval with
    | Var (name, size) -> Format.fprintf ppf "%s%a" name Size.pp size
    | Load (len, endianness, (addr, _), array) ->
        Format.fprintf ppf "%a[%a%a, %d]"
          (Format.pp_print_option
             ~none:(fun ppf () -> Format.pp_print_char ppf '@')
             (fun ppf name -> Format.pp_print_string ppf name))
          array Expr.pp addr
          (Format.pp_print_option (fun ppf (e : Machine.endianness) ->
               match e with
               | LittleEndian -> Format.pp_print_string ppf ", <-"
               | BigEndian -> Format.pp_print_string ppf ", ->"))
          endianness len
    | Sub (r, (lval, _)) -> Format.fprintf ppf "%a%a" pp lval pp_range r
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
end = struct
  type t =
    | Int of Z.t
    | Bv of Bitvector.t
    | Symbol of Symbol.t loc
    | Loc of Loc.t loc
    | Unary of Dba.Unary_op.t * t loc
    | Binary of Dba.Binary_op.t * t loc * t loc
    | Ite of t loc * t loc * t loc

  let zero = Bv Bitvector.zero
  let one = Bv Bitvector.one
  let succ x = Binary (Plus, x, (Int Z.one, Lexing.dummy_pos))
  let integer z = Int z
  let constant bv = Bv bv
  let symbol sym = Symbol sym
  let loc lval = Loc lval
  let binary op x y = Binary (op, x, y)
  let neg x = Unary (UMinus, x)
  let add = binary Plus
  let sub = binary Minus
  let mul = binary Mult
  let smod = binary ModS
  let umod = binary ModU
  let udiv = binary DivU
  let sdiv = binary DivS
  let lognot x = Unary (Not, x)
  let logor = binary Or
  let logxor = binary Xor
  let logand = binary And
  let equal = binary Eq
  let diff = binary Diff
  let ule = binary LeqU
  let sle = binary LeqS
  let ult = binary LtU
  let slt = binary LtS
  let uge = binary GeqU
  let sge = binary GeqS
  let ugt = binary GtU
  let sgt = binary GtS
  let shift_left = binary LShift
  let shift_right = binary RShiftU
  let shift_right_signed = binary RShiftS
  let rotate_left = binary LeftRotate
  let rotate_right = binary RightRotate
  let sext n x = Unary (Sext n, x)
  let uext n x = Unary (Uext n, x)
  let restrict ~hi ~lo x = Unary (Restrict { hi; lo }, x)
  let append = binary Concat
  let ite t x y = Ite (t, x, y)

  let rec pp ppf e =
    match e with
    | Int z -> Z.pp_print ppf z
    | Bv bv -> Bitvector.pp_hex_or_bin ppf bv
    | Symbol (sym, _) -> Symbol.pp ppf sym
    | Loc (lval, _) -> Loc.pp ppf lval
    | Unary (Uext n, (x, _)) -> Format.fprintf ppf "(uext%d %a)" n pp x
    | Unary (Sext n, (x, _)) -> Format.fprintf ppf "(sext%d %a)" n pp x
    | Unary (Restrict r, (x, _)) -> Format.fprintf ppf "%a%a" pp x pp_range r
    | Unary (op, (x, _)) ->
        Format.fprintf ppf "%a (%a)" Dba_printer.Ascii.pp_unary_op op pp x
    | Binary (op, (x, _), (y, _)) ->
        Format.fprintf ppf "(%a %a %a)" pp x Dba_printer.Ascii.pp_binary_op op
          pp y
    | Ite ((t, _), (x, _), (y, _)) ->
        Format.fprintf ppf "%a ? %a : %a" pp t pp x pp y
end

module Instr = struct
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

  let nop = Nop
  let label name = Label name
  let assign lval rval = Assign (lval, rval)
  let undef lval = Nondet lval
  let nondet lval = Nondet lval
  let assume rval = Assume rval
  let dynamic_assert rval = Assert rval
  let conditional_jump rval label = If (rval, label)
  let dynamic_jump rval = Jump rval
  let goto label = Goto label
  let halt = Halt
  let printers = ref []
  let register_pp f = printers := f :: !printers

  let rec resolve_pp ppf inst = function
    | [] -> Format.pp_print_string ppf "unknown"
    | pp :: printers -> if not (pp ppf inst) then resolve_pp ppf inst printers

  let pp ppf inst =
    match inst with
    | Nop -> Format.pp_print_string ppf "nop"
    | Label name -> Format.fprintf ppf "%s:" name
    | Assign ((lval, _), (rval, _)) ->
        Format.fprintf ppf "%a := %a" Loc.pp lval Expr.pp rval
    | Undef (lval, _) -> Format.fprintf ppf "%a := undef" Loc.pp lval
    | Nondet (lval, _) -> Format.fprintf ppf "%a := nondet" Loc.pp lval
    | Assume (rval, _) -> Format.fprintf ppf "assume %a" Expr.pp rval
    | Assert (rval, _) -> Format.fprintf ppf "assert %a" Expr.pp rval
    | If ((rval, _), label) ->
        Format.fprintf ppf "if %a then goto %s" Expr.pp rval label
    | Goto label -> Format.fprintf ppf "goto %s" label
    | Jump (rval, _) -> Format.fprintf ppf "jump at %a" Expr.pp rval
    | Halt -> Format.pp_print_string ppf "halt"
    | _ -> resolve_pp ppf inst !printers
end

type t = ..
