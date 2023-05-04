(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2023                                               *)
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

open Types

module Expr = struct
  include Dba.Expr

  let neg = uminus

  let succ e = add e (constant (Bitvector.create Z.one (size_of e)))
end

module Pexpr : Ast_types.PARSER_EXPRESSION with type expr := Expr.t = struct
  type t = Int of Z.t | Expr of Expr.t

  let int_to_expr n i =
    if Z.numbits i > n then
      Options.Logger.fatal "integer %a does not fit in %a" Z.pp_print i
        (fun ppf n ->
          if n = 1 then Format.pp_print_string ppf "bool"
          else Format.fprintf ppf "bitvector of %d bits" n)
        n
    else Expr.constant (Bitvector.create i n)

  let to_expr n x =
    match x with
    | Int i -> int_to_expr n i
    | Expr e ->
        if Expr.size_of e <> n then
          Options.Logger.fatal "%a was expected to be a bitvector of %d bit%s"
            Dba_printer.Ascii.pp_bl_term e n
            (if n = 1 then "" else "s")
        else e

  let to_bool = to_expr 1

  let unary (o : Dba.Unary_op.t) x =
    match x with
    | Int _ ->
        Options.Logger.fatal "unable to apply %a operator on an integer"
          Dba_printer.Ascii.pp_unary_op o
    | Expr e -> Expr (Expr.unary o e)

  let binary (o : Dba.Binary_op.t) x y =
    match (x, y) with
    | Int _, Int _ ->
        Options.Logger.fatal "unable to apply %a operator on two integers"
          Dba_printer.Ascii.pp_binary_op o
    | Int i, Expr e -> Expr Expr.(binary o (int_to_expr (size_of e) i) e)
    | Expr e, Int i -> Expr Expr.(binary o e (int_to_expr (size_of e) i))
    | Expr e, Expr e' -> Expr (Expr.binary o e e')

  let add = binary Plus

  let sub = binary Minus

  let mul = binary Mult

  let neg = unary UMinus

  let udiv = binary DivU

  let umod = binary ModU

  let urem = binary ModU

  let sdiv = binary DivS

  let smod = binary ModS

  let srem = binary ModS

  let equal = binary Eq

  let diff = binary Diff

  let ule = binary LeqU

  let uge = binary GeqU

  let ult = binary LtU

  let ugt = binary GtU

  let sle = binary LeqS

  let sge = binary GeqS

  let slt = binary LtS

  let sgt = binary GtS

  let logand = binary And

  let logor = binary Or

  let lognot = unary Not

  let logxor = binary Xor

  let shift_left = binary LShift

  let shift_right = binary RShiftU

  let shift_right_signed = binary RShiftS

  let rotate_left = binary LeftRotate

  let rotate_right = binary RightRotate

  let sext n x = unary (Sext n) x

  let uext n x = unary (Uext n) x

  let restrict lo hi x =
    match x with
    | Int i ->
        let n = hi - lo + 1 in
        Expr (Expr.constant (Bitvector.create (Z.extract i lo n) n))
    | Expr e -> Expr (Expr.restrict lo hi e)

  let append x y =
    match (x, y) with
    | Int _, _ | _, Int _ ->
        Options.Logger.fatal "unable to concatenate integer without size"
    | Expr a, Expr b -> Expr (Expr.append a b)

  let ite c x y =
    let t, e =
      match (x, y) with
      | Int _, Int _ ->
          Options.Logger.fatal "unable to infer the size of ite body"
      | Int i, Expr e -> (int_to_expr (Expr.size_of e) i, e)
      | Expr e, Int i -> (e, int_to_expr (Expr.size_of e) i)
      | Expr e, Expr e' -> (e, e')
    in
    Expr (Expr.ite c t e)
end

module LValue = Dba.LValue

module Instr = struct
  type t =
    | Assign of LValue.t * Expr.t
    | Undef of LValue.t
    | Nondet of LValue.t
    | Assume of Expr.t
    | Assert of Expr.t
    | It of Expr.t * string
    | Goto of string
    | Jump of Expr.t
    | Label of string
    | Halt

  let assign loc value = Assign (loc, value)

  let undef loc = Undef loc

  let nondet loc = Nondet loc

  let assume test = Assume test

  let dynamic_assert test = Assert test

  let conditional_jump test target = It (test, target)

  let goto target = Goto target

  let dynamic_jump target = Jump target

  let label name = Label name

  let halt = Halt

  let pp_list ppf hunk =
    Format.pp_open_vbox ppf 0;
    Format.pp_open_vbox ppf 2;
    List.iter
      (fun instr ->
        match instr with
        | Assign (lval, rval) ->
            Format.fprintf ppf "@ @[<hov>%a := %a@]" Dba_printer.Ascii.pp_lhs
              lval Dba_printer.Ascii.pp_bl_term rval
        | Undef lval ->
            Format.fprintf ppf "@ @[<hov>%a := \\undef@]"
              Dba_printer.Ascii.pp_lhs lval
        | Nondet lval ->
            Format.fprintf ppf "@ @[<hov>%a := \\nondet@]"
              Dba_printer.Ascii.pp_lhs lval
        | Assume test ->
            Format.fprintf ppf "@ @[<hov>assume %a@]"
              Dba_printer.Ascii.pp_bl_term test
        | Assert test ->
            Format.fprintf ppf "@ @[<hov>assert %a]"
              Dba_printer.Ascii.pp_bl_term test
        | It (test, target) ->
            Format.fprintf ppf "@ @[<hov>if %a@ goto %s@]"
              Dba_printer.Ascii.pp_bl_term test target
        | Goto target -> Format.fprintf ppf "@ goto %s" target
        | Jump target ->
            Format.fprintf ppf "@ @[<hov>jump %a@]" Dba_printer.Ascii.pp_bl_term
              target
        | Label name -> Format.fprintf ppf "@]@ @[<v 2>%s:" name
        | Halt -> Format.fprintf ppf "@ halt")
      hunk;
    Format.pp_close_box ppf ();
    Format.pp_close_box ppf ()
end

module Directive = struct
  type t =
    | Cut of Expr.t option
    | Assume of Expr.t
    | Assert of Expr.t
    | Reach of int * Expr.t option * Output.t list
    | Enumerate of int * Expr.t
end

type t =
  | Start_from of Expr.t * Instr.t list
  | Start_from_core of Instr.t list
  | Load_sections of string list
  | Load_data of Expr.t
  | Import_symbols of (string * Dba.Var.Tag.attribute) list * string
  | Stub of Expr.t list * Instr.t list
  | Init of Instr.t list
  | Directive of Expr.t * Directive.t
