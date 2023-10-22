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
  ( Expr.t loc -> Expr.t loc -> Expr.t,
    t,
    Expr.t loc,
    Instr.t,
    unit,
    Buffer.t,
    unit,
    Buffer.t,
    Loc.t loc,
    Obj.t,
    unit,
    string option,
    Dba.Var.Tag.attribute,
    Dba.Var.Tag.attribute,
    Dba.Var.Tag.attribute option,
    ((Expr.t loc -> Expr.t loc -> Expr.t) * Expr.t loc) list,
    Expr.t loc * Expr.t loc,
    (Expr.t loc * Expr.t loc) list,
    (Expr.t loc * Expr.t loc) list,
    Expr.t loc * Instr.t list,
    (Expr.t loc * Instr.t list) list,
    (Expr.t loc * Instr.t list) list,
    Instr.t list,
    Instr.t list option,
    string,
    Basic_types.BigInt.t,
    string option,
    Instr.t list list,
    Basic_types.BigInt.t option,
    Machine.endianness,
    Machine.endianness option,
    Basic_types.BigInt.t,
    Basic_types.BigInt.t option,
    (Expr.t loc -> Expr.t loc -> Expr.t) * Expr.t loc,
    ((Expr.t loc -> Expr.t loc -> Expr.t) * Expr.t loc) list,
    Machine.endianness,
    unit,
    int Interval.t,
    unit,
    t list,
    Instr.t list,
    string,
    Symbol.t loc,
    Expr.t loc -> Expr.t )
  Syntax.obj
