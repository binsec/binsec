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

let b1, b2 = (Size.Byte.create 1, Size.Byte.create 2)

module M = Machine
module V = Dba.Var
module T = Dba.Var.Tag
module L = Dba.LValue
module E = Dba.Expr

type 'a t =
  | F : { name : string; loc : Dba.LValue.t; expr : Dba.Expr.t } -> [ `x1 ] t
  | S : { name : string; loc : L.t; expr : E.t } -> [ `x8 ] t
  | D : {
      name : string;
      var : V.t;
      loc : L.t;
      expr : E.t;
      hi : [ `x8 ] t;
      lo : [ `x8 ] t;
      mem : string;
      store1 : L.t;
      store2 : L.t;
      load1 : E.t;
      load2 : E.t;
    }
      -> [ `x16 ] t

let pair subh subl name =
  let var = V.create name ~bitsize:Size.Bit.bits16 ~tag:T.Register in
  let loc = L.v var and expr = E.v var in
  let hi =
    S
      {
        name = subh name;
        loc = L.restrict var 8 15;
        expr = E.restrict 8 15 expr;
      }
  and lo =
    S { name = subl name; loc = L.restrict var 0 7; expr = E.restrict 0 7 expr }
  in
  let mem = Format.sprintf "(%s)" name in
  let store1 = L.store b1 M.LittleEndian expr
  and store2 = L.store b2 M.LittleEndian expr
  and load1 = E.load b1 M.LittleEndian expr
  and load2 = E.load b2 M.LittleEndian expr in
  D { name; var; loc; expr; hi; lo; mem; store1; store2; load1; load2 }

let bc, de, hl =
  let subh s = String.sub s 0 1 and subl s = String.sub s 1 1 in
  let pair = pair subh subl in
  (pair "BC", pair "DE", pair "HL")

let bc', de', hl' =
  let subh s = String.init 2 (function 0 -> String.get s 0 | _ -> '\'')
  and subl s = String.init 2 (function 0 -> String.get s 1 | _ -> '\'') in
  let pair = pair subh subl in
  (pair "BC'", pair "DE'", pair "HL'")

let ix, iy, sp =
  let subh s = s ^ "h" and subl s = s ^ "l" in
  let pair = pair subh subl in
  (pair "IX", pair "IY", pair "SP")

let r8 name =
  let var = V.create name ~bitsize:Size.Bit.bits8 ~tag:T.Register in
  S { name; loc = L.v var; expr = E.v var }

let a = r8 "A"

let f, sf, zf, yf, hf, xf, pf, vf, nf, cf =
  let name = "F" in
  let var = V.create name ~bitsize:Size.Bit.bits8 ~tag:T.Flag in
  let expr = E.v var in
  let mk p name =
    F { name; loc = L.restrict var p p; expr = E.restrict p p expr }
  in
  let pf = mk 2 "PF" in
  ( S { name; loc = L.v var; expr },
    mk 7 "SF",
    mk 6 "ZF",
    mk 5 "YF",
    mk 4 "HF",
    mk 3 "XF",
    pf,
    (let (F { loc; expr; _ }) = pf in
     F { name = "VF"; loc; expr }),
    mk 1 "NF",
    mk 0 "CF" )

let (D { hi = b; lo = c; _ }) = bc
let (D { hi = d; lo = e; _ }) = de
let (D { hi = h; lo = l; _ }) = hl
let a' = r8 "A'"
let f' = r8 "F'"
let (D { hi = b'; lo = c'; _ }) = bc'
let (D { hi = d'; lo = e'; _ }) = de'
let (D { hi = h'; lo = l'; _ }) = hl'
let i = r8 "I"
let r = r8 "R"

let ifft1, ifft2 =
  let mk name =
    let var = V.flag name in
    F { name; loc = L.v var; expr = E.v var }
  in
  (mk "ifft1", mk "ifft2")

let (D { hi = ixh; lo = ixl; _ }) = ix
let (D { hi = iyh; lo = iyl; _ }) = iy

let name : type a. a t -> string = function
  | F { name; _ } | S { name; _ } | D { name; _ } -> name

let size : type a. a t -> int = function F _ -> 1 | S _ -> 8 | D _ -> 16

let expr : type a. a t -> E.t = function
  | F { expr; _ } | S { expr; _ } | D { expr; _ } -> expr

let lval : type a. a t -> L.t = function
  | F { loc; _ } | S { loc; _ } | D { loc; _ } -> loc

let var (D { var; _ }) = var
let hi (D { hi; _ }) = hi
let lo (D { lo; _ }) = lo
let mem (D { mem; _ }) = mem
let store1 (D { store1; _ }) = store1
let store2 (D { store2; _ }) = store2
let load1 (D { load1; _ }) = load1
let load2 (D { load2; _ }) = load2
let flags = [| sf; zf; hf; nf; cf; ifft1; ifft2 |]

let registers8 =
  [|
    a;
    f;
    b;
    c;
    d;
    e;
    h;
    l;
    a';
    f';
    b';
    c';
    d';
    e';
    h';
    l';
    ixh;
    ixl;
    iyh;
    iyl;
    i;
    r;
  |]

let registers16 = [| bc; de; hl; bc'; de'; hl'; ix; iy; sp |]
