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

open Types

module Expr = struct
  include Dba.Expr

  let neg = uminus

  let succ e = add e (constant (Bitvector.create Z.one (size_of e)))
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
