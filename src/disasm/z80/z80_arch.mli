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

type 'a t = private
  | F : { name : string; loc : Dba.LValue.t; expr : Dba.Expr.t } -> [ `x1 ] t
  | S : { name : string; loc : Dba.LValue.t; expr : Dba.Expr.t } -> [ `x8 ] t
  | D : {
      name : string;
      var : Dba.Var.t;
      loc : Dba.LValue.t;
      expr : Dba.Expr.t;
      hi : [ `x8 ] t;
      lo : [ `x8 ] t;
      mem : string;
      store1 : Dba.LValue.t;
      store2 : Dba.LValue.t;
      load1 : Dba.Expr.t;
      load2 : Dba.Expr.t;
    }
      -> [ `x16 ] t

val a : [ `x8 ] t
val f : [ `x8 ] t
val b : [ `x8 ] t
val c : [ `x8 ] t
val d : [ `x8 ] t
val e : [ `x8 ] t
val h : [ `x8 ] t
val l : [ `x8 ] t
val a' : [ `x8 ] t
val f' : [ `x8 ] t
val b' : [ `x8 ] t
val c' : [ `x8 ] t
val d' : [ `x8 ] t
val e' : [ `x8 ] t
val h' : [ `x8 ] t
val l' : [ `x8 ] t
val i : [ `x8 ] t
val r : [ `x8 ] t
val ixh : [ `x8 ] t
val ixl : [ `x8 ] t
val iyh : [ `x8 ] t
val iyl : [ `x8 ] t
val bc : [ `x16 ] t
val de : [ `x16 ] t
val hl : [ `x16 ] t
val bc' : [ `x16 ] t
val de' : [ `x16 ] t
val hl' : [ `x16 ] t
val ix : [ `x16 ] t
val iy : [ `x16 ] t
val sp : [ `x16 ] t
val sf : [ `x1 ] t
val zf : [ `x1 ] t
val yf : [ `x1 ] t
val hf : [ `x1 ] t
val xf : [ `x1 ] t
val pf : [ `x1 ] t
val vf : [ `x1 ] t
val nf : [ `x1 ] t
val cf : [ `x1 ] t
val ifft1 : [ `x1 ] t
val ifft2 : [ `x1 ] t
val name : _ t -> string
val size : _ t -> int
val expr : _ t -> Dba.Expr.t
val lval : _ t -> Dba.LValue.t
val var : [ `x16 ] t -> Dba.Var.t
val hi : [ `x16 ] t -> [ `x8 ] t
val lo : [ `x16 ] t -> [ `x8 ] t
val mem : [ `x16 ] t -> string
val store1 : [ `x16 ] t -> Dba.LValue.t
val store2 : [ `x16 ] t -> Dba.LValue.t
val load1 : [ `x16 ] t -> Dba.Expr.t
val load2 : [ `x16 ] t -> Dba.Expr.t
val flags : [ `x1 ] t array
val registers8 : [ `x8 ] t array
val registers16 : [ `x16 ] t array
