(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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

type stats = {
  var : int; cst : int;
  unop : int; bnop : int; comp : int;
  select : int; store : int;
}

let empty_stats = {
  var = 0; cst = 0;
  unop = 0; bnop = 0; comp = 0;
  select = 0; store = 0;
}

let rec term_stats acc tm =
  term_desc_stats acc tm.term_desc

and term_desc_stats acc = function
  | BlTerm bl -> bl_term_stats acc bl
  | BvTerm bv -> bv_term_stats acc bv
  | AxTerm ax -> ax_term_stats acc ax

and bl_term_stats acc bl =
  bl_term_desc_stats acc bl.bl_term_desc

and bl_term_desc_stats acc = function
  | BlTrue
  | BlFalse -> {acc with cst = acc.cst+1}
  | BlFun (_,ls) -> funs_stats {acc with var = acc.var+1} ls
  | BlLet (bn,bl) -> bl_term_stats (lets_stats acc bn) bl
  | BlUnop (_,bl) -> bl_term_stats {acc with unop = acc.unop+1} bl
  | BlBnop (_,bl1,bl2) -> bl_term_stats (bl_term_stats {acc with bnop = acc.bnop+1} bl1) bl2
  | BlComp (_,bl1,bl2) -> bl_term_stats (bl_term_stats {acc with bnop = acc.comp+1} bl1) bl2
  | BvComp (_,bv1,bv2) -> bv_term_stats (bv_term_stats {acc with bnop = acc.comp+1} bv1) bv2
  | AxComp (_,ax1,ax2) -> ax_term_stats (ax_term_stats {acc with bnop = acc.comp+1} ax1) ax2
  | BlIte (bl,bl1,bl2) -> bl_term_stats (bl_term_stats (bl_term_stats acc bl) bl1) bl2

and bv_term_stats acc bv =
  bv_term_desc_stats acc bv.bv_term_desc

and bv_term_desc_stats acc = function
  | BvCst _ -> {acc with cst = acc.cst+1}
  | BvFun (_,ls) -> funs_stats {acc with var = acc.var+1} ls
  | BvLet (bn,bv) -> bv_term_stats (lets_stats acc bn) bv
  | BvUnop (_,bv) -> bv_term_stats {acc with unop = acc.unop+1} bv
  | BvBnop (_,bv1,bv2) -> bv_term_stats (bv_term_stats {acc with bnop = acc.bnop+1} bv1) bv2
  | BvIte  (bl,bv1,bv2) -> bv_term_stats (bv_term_stats (bl_term_stats acc bl) bv1) bv2
  | Select (n,ax,bv) -> bv_term_stats (ax_term_stats {acc with select = acc.select+n} ax) bv

and ax_term_stats acc ax =
  ax_term_desc_stats acc ax.ax_term_desc

and ax_term_desc_stats acc = function
  | AxFun (_,ls) -> funs_stats {acc with var = acc.var+1} ls
  | AxLet (bn,ax) -> ax_term_stats (lets_stats acc bn) ax
  | AxIte (bl,ax1,ax2) -> ax_term_stats (ax_term_stats (bl_term_stats acc bl) ax1) ax2
  | Store (n,ax,bv1,bv2) ->
    bv_term_stats (bv_term_stats (ax_term_stats {acc with select = acc.select+n} ax) bv1) bv2

and def_stats acc df =
  def_desc_stats acc df.def_desc

and def_desc_stats acc = function
  | BlDef (_,_,bl) -> bl_term_stats acc bl
  | BvDef (_,_,bv) -> bv_term_stats acc bv
  | AxDef (_,_,ax) -> ax_term_stats acc ax

and funs_stats acc ls =
  List.fold_left term_stats acc ls

and lets_stats acc ls =
  List.fold_left def_stats acc ls


let bl_term_stats bl = bl_term_stats empty_stats bl
let bv_term_stats bv = bv_term_stats empty_stats bv
let ax_term_stats ax = ax_term_stats empty_stats ax


let rec term_variables acc tm =
  term_desc_variables acc tm.term_desc

and term_desc_variables acc = function
  | BlTerm bl -> bl_term_variables acc bl
  | BvTerm bv -> bv_term_variables acc bv
  | AxTerm ax -> ax_term_variables acc ax

and bl_term_variables acc bl =
  bl_term_desc_variables acc bl.bl_term_desc

and bl_term_desc_variables acc = function
  | BlTrue | BlFalse -> acc
  | BlFun (v,ls) -> funs_variables (VarSet.add (BlVar v) acc) ls
  | BlLet (bn,bl) -> bl_term_variables (lets_variables acc bn) bl
  | BlUnop (_,bl) -> bl_term_variables acc bl
  | BlBnop (_,bl1,bl2) -> bl_term_variables (bl_term_variables acc bl1) bl2
  | BlComp (_,bl1,bl2) -> bl_term_variables (bl_term_variables acc bl1) bl2
  | BvComp (_,bv1,bv2) -> bv_term_variables (bv_term_variables acc bv1) bv2
  | AxComp (_,ax1,ax2) -> ax_term_variables (ax_term_variables acc ax1) ax2
  | BlIte (bl,bl1,bl2) -> bl_term_variables (bl_term_variables (bl_term_variables acc bl) bl1) bl2

and bv_term_variables acc bv =
  bv_term_desc_variables acc bv.bv_term_desc

and bv_term_desc_variables acc = function
  | BvCst _ -> acc
  | BvFun (v,ls) -> funs_variables (VarSet.add (BvVar v) acc) ls
  | BvLet (bn,bv) -> bv_term_variables (lets_variables acc bn) bv
  | BvUnop (_,bv) -> bv_term_variables acc bv
  | BvBnop (_,bv1,bv2) -> bv_term_variables (bv_term_variables acc bv1) bv2
  | BvIte (bl,bv1,bv2) -> bv_term_variables (bv_term_variables (bl_term_variables acc bl) bv1) bv2
  | Select (_,ax,bv) -> bv_term_variables (ax_term_variables acc ax) bv

and ax_term_variables acc ax =
  ax_term_desc_variables acc ax.ax_term_desc

and ax_term_desc_variables acc = function
  | AxFun (v,ls) -> funs_variables (VarSet.add (AxVar v) acc) ls
  | AxLet (bn,ax) -> ax_term_variables (lets_variables acc bn) ax
  | AxIte (bl,ax1,ax2) -> ax_term_variables (ax_term_variables (bl_term_variables acc bl) ax1) ax2
  | Store (_,ax,bv1,bv2) -> bv_term_variables (bv_term_variables (ax_term_variables acc ax) bv1) bv2

and def_variables acc df =
  def_desc_variables acc df.def_desc

and def_desc_variables acc = function
  | BlDef (_,_,bl) -> bl_term_variables acc bl
  | BvDef (_,_,bv) -> bv_term_variables acc bv
  | AxDef (_,_,ax) -> ax_term_variables acc ax

and funs_variables acc ls =
  List.fold_left term_variables acc ls

and lets_variables acc ls =
  List.fold_left def_variables acc ls


let bl_term_variables bl = bl_term_variables VarSet.empty bl
let bv_term_variables bv = bv_term_variables VarSet.empty bv
let ax_term_variables ax = ax_term_variables VarSet.empty ax

let is_symbolic_bl_term bl = (bl_term_stats bl).var > 0
let is_symbolic_bv_term bv = (bv_term_stats bv).var > 0
let is_symbolic_ax_term ax = (ax_term_stats ax).var > 0

(* Some accessors *)

let bv_size bv = bv.bv_term_size

let ax_size ax = ax.idx_term_size, ax.elt_term_size

let bv_desc_size = function
  | BvCst bv -> Bitvector.size_of bv
  | BvFun (v,_) -> v.bv_size
  | BvLet (_,bv) -> bv_size bv
  | BvUnop (u,bv) ->
    (match u with
     | BvNot | BvNeg -> bv_size bv
     | BvRepeat i -> i * bv_size bv
     | BvZeroExtend i
     | BvSignExtend i -> i + bv_size bv
     | BvRotateLeft _
     | BvRotateRight _ -> bv_size bv
     | BvExtract i -> i.Interval.hi - i.Interval.lo + 1)
  | BvBnop (bv,bv1,bv2) ->
    let bv1 = bv_size bv1 in
    let bv2 = bv_size bv2 in
    (match bv with
     | BvConcat -> bv1 + bv2
     | BvCmp -> assert (bv1 = bv2); 1
     | BvAnd | BvNand
     | BvOr  | BvNor
     | BvXor | BvXnor
     | BvAdd | BvSub | BvMul
     | BvUdiv | BvSdiv
     | BvUrem | BvSrem | BvSmod
     | BvShl  | BvAshr | BvLshr -> assert (bv1 = bv2); bv1)
  | BvIte (_,bv1,bv2) ->
    let bv1 = bv_size bv1 in
    let bv2 = bv_size bv2 in
    assert (bv1 = bv2); bv1
  | Select (n,ax,bv) ->
    let bv = bv_size bv in
    let idx, elt = ax_size ax in
    assert (idx = bv); n * elt

let ax_desc_size = function
  | AxFun (v,_) -> v.idx_size, v.elt_size
  | AxLet (_,ax) -> ax_size ax
  | AxIte (_,ax1,ax2) ->
    let idx1, elt1 = ax_size ax1 in
    let idx2, elt2 = ax_size ax2 in
    assert (idx1 = idx2);
    assert (elt1 = elt2);
    idx1, elt1
  | Store (n,ax,bv1,bv2) ->
    let idx, elt = ax_size ax in
    let bv1 = bv_size bv1 in
    let bv2 = bv_size bv2 in
    assert (idx = bv1);
    assert (n * elt = bv2);
    idx, elt


let is_bl_desc_cst = function
  | BlTrue -> Some true
  | BlFalse -> Some false
  | BlFun (_,_) -> None
  | BlLet (_,_) -> None
  | BlUnop (_,_) -> None
  | BlBnop (_,_,_) -> None
  | BlComp (_,_,_) -> None
  | BvComp (_,_,_) -> None
  | AxComp (_,_,_) -> None
  | BlIte (_,_,_) -> None

let is_bl_cst bl =
  is_bl_desc_cst bl.bl_term_desc

let is_bv_desc_cst = function
  | BvCst bv -> Some bv
  | BvFun (_,_) -> None
  | BvLet (_,_) -> None
  | BvUnop (_,_) -> None
  | BvBnop (_,_,_) -> None
  | BvIte (_,_,_) -> None
  | Select (_,_,_) -> None

let is_bv_cst bv =
  is_bv_desc_cst bv.bv_term_desc

let is_bl_desc_var = function
  | BlTrue -> None
  | BlFalse -> None
  | BlFun (v,l) ->
    (match l with
     | [] -> Some v
     | _ :: _ -> None)
  | BlLet (_,_) -> None
  | BlUnop (_,_) -> None
  | BlBnop (_,_,_) -> None
  | BlComp (_,_,_) -> None
  | BvComp (_,_,_) -> None
  | AxComp (_,_,_) -> None
  | BlIte (_,_,_) -> None

let is_bl_var bl =
  is_bl_desc_var bl.bl_term_desc

let is_bv_desc_var = function
  | BvCst _ -> None
  | BvFun (v,l) ->
    (match l with
     | [] -> Some v
     | _ :: _ -> None)
  | BvLet (_,_) -> None
  | BvUnop (_,_) -> None
  | BvBnop (_,_,_) -> None
  | BvIte (_,_,_) -> None
  | Select (_,_,_) -> None

let is_bv_var bv =
  is_bv_desc_var bv.bv_term_desc

let is_ax_desc_var = function
  | AxFun (v,l) ->
    (match l with
     | [] -> Some v
     | _ :: _ -> None)
  | AxLet (_,_) -> None
  | AxIte (_,_,_) -> None
  | Store (_,_,_,_) -> None

let is_ax_var ax =
  is_ax_desc_var ax.ax_term_desc

let is_select { bv_term_desc; _ } =
  match bv_term_desc with
  | Select (n,ax,bv) -> Some (n, ax, bv)
  | BvCst _ -> None
  | BvFun (_,_) -> None
  | BvLet (_,_) -> None
  | BvUnop (_,_) -> None
  | BvBnop (_,_,_) -> None
  | BvIte (_,_,_) -> None

let is_store { ax_term_desc; _ } =
  match ax_term_desc with
  | Store (n,ax,bv1,bv2) -> Some (n, ax, bv1, bv2)
  | AxFun (_,_) -> None
  | AxLet (_,_) -> None
  | AxIte (_,_,_) -> None


let bl_var_hash bl = bl.bl_hash
let bv_var_hash bv = bv.bv_hash
let ax_var_hash ax = ax.ax_hash

let var_hash = function
  | BlVar v -> bl_var_hash v
  | BvVar v -> bv_var_hash v
  | AxVar v -> ax_var_hash v

let bl_var_name bl = bl.bl_name
let bv_var_name bv = bv.bv_name
let ax_var_name ax = ax.ax_name


let var_name = function
  | BlVar v -> bl_var_name v
  | BvVar v -> bv_var_name v
  | AxVar v -> ax_var_name v

let rec bl_term_desc_name = function
  | BlFun (v,l) ->
    (match l with
     | [] -> Some v.bl_name
     | _ :: _ -> None)
  | BlLet (_,bl) -> bl_term_name bl
  | BlTrue | BlFalse
  | BlUnop _ | BlBnop _
  | BlComp _ | BvComp _
  | AxComp _ | BlIte  _ -> None

and bl_term_name bl = bl_term_desc_name bl.bl_term_desc

let rec bv_term_desc_name = function
  | BvFun (v,l) ->
    (match l with
     | [] -> Some v.bv_name
     | _ :: _ -> None)
  | BvLet (_,bv) -> bv_term_name bv
  | BvCst _  | BvIte _
  | BvUnop _ | BvBnop _
  | Select _ -> None

and bv_term_name bv = bv_term_desc_name bv.bv_term_desc

let rec ax_term_desc_name = function
  | AxFun (v,l) ->
    (match l with
     | [] -> Some v.ax_name
     | _ :: _ -> None)
  | AxLet (_,ax) -> ax_term_name ax
  | Store (_,ax,_,_) -> ax_term_name ax
  | AxIte _ -> None

and ax_term_name ax = ax_term_desc_name ax.ax_term_desc

let decl_desc_name = function
  | BlDecl (v,_) -> v.bl_name
  | BvDecl (v,_) -> v.bv_name
  | AxDecl (v,_) -> v.ax_name

let decl_name dc =
  decl_desc_name dc.decl_desc

let def_desc_name = function
  | BlDef (v,_,_) -> v.bl_name
  | BvDef (v,_,_) -> v.bv_name
  | AxDef (v,_,_) -> v.ax_name

let def_name df =
  def_desc_name df.def_desc

