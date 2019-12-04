(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

open Formula

exception NoSmtEquivalent

let unary = function
  | Dba.Unary_op.UMinus -> BvNeg
  | Dba.Unary_op.Not -> BvNot
  | _ -> assert false

let binary = function (* DBAs are broken *)
  | Dba.Binary_op.Plus -> `Bnop BvAdd
  | Dba.Binary_op.Minus -> `Bnop BvSub
  | Dba.Binary_op.Mult -> `Bnop BvMul
  | Dba.Binary_op.DivU -> `Bnop BvUdiv
  | Dba.Binary_op.DivS -> `Bnop BvSdiv
  | Dba.Binary_op.ModU -> `Bnop BvSrem
  | Dba.Binary_op.ModS -> `Bnop BvSmod
  | Dba.Binary_op.Or -> `Bnop BvOr
  | Dba.Binary_op.And -> `Bnop BvAnd
  | Dba.Binary_op.Xor -> `Bnop BvXor
  | Dba.Binary_op.Concat -> `Bnop BvConcat
  | Dba.Binary_op.LShift -> `Bnop BvShl
  | Dba.Binary_op.RShiftU -> `Bnop BvLshr
  | Dba.Binary_op.RShiftS -> `Bnop BvAshr
  | Dba.Binary_op.LeftRotate -> `Unop (BvRotateLeft 0)
  | Dba.Binary_op.RightRotate -> `Unop (BvRotateRight 0)
  | Dba.Binary_op.Eq -> `Comp BvEqual
  | Dba.Binary_op.Diff -> `Comp BvDistinct
  | Dba.Binary_op.LeqU -> `Comp BvUle
  | Dba.Binary_op.LtU -> `Comp BvUlt
  | Dba.Binary_op.GeqU -> `Comp BvUge
  | Dba.Binary_op.GtU -> `Comp BvUgt
  | Dba.Binary_op.LeqS -> `Comp BvSle
  | Dba.Binary_op.LtS -> `Comp BvSlt
  | Dba.Binary_op.GeqS -> `Comp BvSge
  | Dba.Binary_op.GtS -> `Comp BvSgt
