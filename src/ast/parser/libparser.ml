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

module Syntax = Syntax
open Ast

type obj =
  ( Ast.Expr.t Ast.loc -> Ast.Expr.t Ast.loc -> Ast.Expr.t,
    t,
    Ast.Expr.t Ast.loc,
    Ast.Instr.t,
    unit,
    Buffer.t,
    unit,
    Buffer.t,
    Ast.Loc.t Ast.loc,
    Obj.t,
    unit,
    string option,
    Dba.Var.Tag.attribute,
    Dba.Var.Tag.attribute,
    Dba.Var.Tag.attribute option,
    ((Ast.Expr.t Ast.loc -> Ast.Expr.t Ast.loc -> Ast.Expr.t)
    * Ast.Expr.t Ast.loc)
    list,
    Ast.Expr.t Ast.loc * Ast.Expr.t Ast.loc,
    (Ast.Expr.t Ast.loc * Ast.Expr.t Ast.loc) list,
    (Ast.Expr.t Ast.loc * Ast.Expr.t Ast.loc) list,
    Ast.Instr.t list,
    Ast.Instr.t list list,
    Ast.Instr.t list list,
    Ast.Expr.t Ast.loc * Ast.Instr.t list,
    (Ast.Expr.t Ast.loc * Ast.Instr.t list) list,
    (Ast.Expr.t Ast.loc * Ast.Instr.t list) list,
    Z.t,
    Ast.Instr.t list,
    Ast.Instr.t list option,
    string,
    string option,
    Ast.Instr.t list list,
    Z.t option,
    Machine.endianness,
    Machine.endianness option,
    Z.t,
    Z.t option,
    (Ast.Expr.t Ast.loc -> Ast.Expr.t Ast.loc -> Ast.Expr.t)
    * Ast.Expr.t Ast.loc,
    ((Ast.Expr.t Ast.loc -> Ast.Expr.t Ast.loc -> Ast.Expr.t)
    * Ast.Expr.t Ast.loc)
    list,
    Machine.endianness,
    unit,
    int Basic_types.interval,
    unit,
    t list,
    Ast.Instr.t list,
    string,
    Ast.Symbol.t Ast.loc,
    Ast.Expr.t Ast.loc -> Ast.Expr.t )
  Syntax.obj
