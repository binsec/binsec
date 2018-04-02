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

open Smtlib2

exception NoSmtEquivalent

let unary = function
  | Dba.UMinus -> SmtBvNeg
  | Dba.Not -> SmtBvNot

let binary = function
  | Dba.Plus -> SmtBvAdd
  | Dba.Minus -> SmtBvSub
  | Dba.MultU -> SmtBvMult
  | Dba.MultS -> SmtBvMult
  | Dba.DivU -> SmtBvUdiv
  | Dba.DivS -> SmtBvSdiv
  | Dba.ModU -> SmtBvSrem
  | Dba.ModS -> SmtBvSmod
  | Dba.Or -> SmtBvOr
  | Dba.And -> SmtBvAnd
  | Dba.Xor -> SmtBvXor
  | Dba.Concat -> SmtBvConcat
  | Dba.LShift -> SmtBvShl
  | Dba.RShiftU -> SmtBvLshr
  | Dba.RShiftS -> SmtBvAshr
  | Dba.LeftRotate -> raise NoSmtEquivalent
  | Dba.RightRotate -> raise NoSmtEquivalent
  | Dba.Eq -> SmtBvComp
  | Dba.Diff -> SmtBvDiff
  | Dba.LeqU -> SmtBvUle
  | Dba.LtU -> SmtBvUlt
  | Dba.GeqU -> SmtBvUge
  | Dba.GtU -> SmtBvUgt
  | Dba.LeqS -> SmtBvSle
  | Dba.LtS -> SmtBvSlt
  | Dba.GeqS -> SmtBvSge
  | Dba.GtS -> SmtBvSgt
