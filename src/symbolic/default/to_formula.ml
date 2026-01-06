(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

module Formula = Smtlib.Formula
open Types

type k =
  | Bl of Expr.t
  | Bv of Expr.t
  | Ax of Memory.t
  | Assert of Expr.t
  | BvDefine of Formula.bv_var * Expr.t
  | AxDefine of Formula.ax_var * Memory.t

and t = {
  mutable id : Suid.t;
  bv_decl : Formula.decl BvTbl.t;
  ax_decl : Formula.decl AxTbl.t;
  bl_cons : Formula.bl_var BvTbl.t;
  bv_cons : Formula.bv_var BvTbl.t;
  ax_cons : Formula.ax_var AxTbl.t;
  ordered : k Queue.t;
  mutable idx_size : int;
  debug : name:string -> label:string -> string;
}

let create ?(debug = fun ~name ~label:_ -> name) ~next_id () =
  {
    id = next_id;
    bv_decl = BvTbl.create 16;
    ax_decl = AxTbl.create 1;
    bl_cons = BvTbl.create 32;
    bv_cons = BvTbl.create 128;
    ax_cons = AxTbl.create 64;
    ordered = Queue.create ();
    idx_size = 0;
    debug;
  }

let bl_once = Formula.bl_var "!"
let bv_once = Formula.bv_var "!" 1
let ax_once = Formula.ax_var "!" 1 1

let rec visit_bl ctx bl =
  try
    if BvTbl.find ctx.bl_cons bl == bl_once then (
      let name = Suid.to_string ctx.id in
      ctx.id <- Suid.incr ctx.id;
      BvTbl.replace ctx.bl_cons bl (Formula.bl_var name))
  with Not_found -> (
    match bl with
    | Cst _ -> ()
    | Load _ (* cannot be a bl<1> *) -> assert false
    | Unary { f = Not; x; _ } ->
        BvTbl.add ctx.bl_cons bl bl_once;
        visit_bl ctx x;
        Queue.push (Bl bl) ctx.ordered
    | Binary { f = And | Or; x; y; _ } ->
        BvTbl.add ctx.bl_cons bl bl_once;
        visit_bl ctx x;
        visit_bl ctx y;
        Queue.push (Bl bl) ctx.ordered
    | Binary
        {
          f = Eq | Diff | Uge | Ule | Ugt | Ult | Sge | Sle | Sgt | Slt;
          x;
          y;
          _;
        } ->
        BvTbl.add ctx.bl_cons bl bl_once;
        visit_bv ctx x;
        visit_bv ctx y;
        Queue.push (Bl bl) ctx.ordered
    | Ite { c; t; e; _ } ->
        BvTbl.add ctx.bl_cons bl bl_once;
        visit_bl ctx c;
        visit_bl ctx t;
        visit_bl ctx e;
        Queue.push (Bl bl) ctx.ordered
    | Var _ | Unary _ | Binary _ -> visit_bv ctx bl)

and visit_bv ctx bv =
  try
    if BvTbl.find ctx.bv_cons bv == bv_once then (
      let name = Suid.to_string ctx.id in
      ctx.id <- Suid.incr ctx.id;
      BvTbl.replace ctx.bv_cons bv (Formula.bv_var name (Expr.sizeof bv)))
  with Not_found -> (
    match bv with
    | Var { name; size; label; _ } ->
        let name = ctx.debug ~name ~label in
        let var = Formula.bv_var name size in
        BvTbl.add ctx.bv_cons bv var;
        BvTbl.add ctx.bv_decl bv (Formula.mk_bv_decl var [])
    | Load { addr; label; _ } ->
        ctx.idx_size <- Expr.sizeof addr;
        BvTbl.add ctx.bv_cons bv bv_once;
        visit_bv ctx addr;
        visit_ax ctx label;
        Queue.push (Bv bv) ctx.ordered
    | Cst _ ->
        BvTbl.add ctx.bv_cons bv bv_once;
        Queue.push (Bv bv) ctx.ordered
    | Unary { x; _ } ->
        BvTbl.add ctx.bv_cons bv bv_once;
        visit_bv ctx x;
        Queue.push (Bv bv) ctx.ordered
    | Binary
        { f = Eq | Diff | Uge | Ule | Ugt | Ult | Sge | Sle | Sgt | Slt; _ } ->
        BvTbl.add ctx.bv_cons bv bv_once;
        visit_bl ctx bv;
        Queue.push (Bv bv) ctx.ordered
    | Binary { x; y; _ } ->
        BvTbl.add ctx.bv_cons bv bv_once;
        visit_bv ctx x;
        visit_bv ctx y;
        Queue.push (Bv bv) ctx.ordered
    | Ite { c; t; e; _ } ->
        BvTbl.add ctx.bv_cons bv bv_once;
        visit_bl ctx c;
        visit_bv ctx t;
        visit_bv ctx e;
        Queue.push (Bv bv) ctx.ordered)

and visit_ax ctx ax =
  try
    if AxTbl.find ctx.ax_cons ax == ax_once then (
      let name = Suid.to_string ctx.id in
      ctx.id <- Suid.incr ctx.id;
      AxTbl.replace ctx.ax_cons ax (Formula.ax_var name ctx.idx_size 8))
  with Not_found -> (
    match ax with
    | Symbol { name; index; _ } ->
        let var = Formula.ax_var (ctx.debug ~name ~label:"") index 8 in
        AxTbl.add ctx.ax_cons ax var;
        AxTbl.add ctx.ax_decl ax (Formula.mk_ax_decl var [])
    | Layer { addr; store; over; _ } ->
        AxTbl.add ctx.ax_cons ax ax_once;
        visit_bv ctx addr;
        Store.iter_term (fun _ bv -> visit_bv ctx bv) store;
        visit_ax ctx over;
        Queue.push (Ax ax) ctx.ordered)

let assert_bl ctx bl =
  visit_bl ctx bl;
  Queue.push (Assert bl) ctx.ordered

let define_bv ctx name bv =
  visit_bv ctx bv;
  let var = Formula.bv_var name (Expr.sizeof bv) in
  Queue.push (BvDefine (var, bv)) ctx.ordered

let define_ax ctx name ax =
  visit_ax ctx ax;
  let var = Formula.ax_var name ctx.idx_size 8 in
  Queue.push (AxDefine (var, ax)) ctx.ordered

let mk_unop (op : Term.unary Term.operator) =
  match op with
  | Not -> Formula.BvNot
  | Minus -> Formula.BvNeg
  | Uext n -> Formula.BvZeroExtend n
  | Sext n -> Formula.BvSignExtend n
  | Restrict i -> Formula.BvExtract i

let mk_comp (op : Term.binary Term.operator) =
  match op with
  | Eq -> Formula.BvEqual
  | Diff -> Formula.BvDistinct
  | Uge -> Formula.BvUge
  | Ule -> Formula.BvUle
  | Ugt -> Formula.BvUgt
  | Ult -> Formula.BvUlt
  | Sge -> Formula.BvSge
  | Sle -> Formula.BvSle
  | Sgt -> Formula.BvSgt
  | Slt -> Formula.BvSlt
  | Plus | Minus | Mul | Udiv | Sdiv | Urem | Srem | Or | And | Xor | Concat
  | Lsl | Lsr | Asr | Rol | Ror ->
      assert false

let mk_bnop (op : Term.binary Term.operator) =
  match op with
  | Eq | Diff | Uge | Ule | Ugt | Ult | Sge | Sle | Sgt | Slt -> assert false
  | Plus -> Formula.BvAdd
  | Minus -> Formula.BvSub
  | Mul -> Formula.BvMul
  | Udiv -> Formula.BvUdiv
  | Sdiv -> Formula.BvSdiv
  | Urem -> Formula.BvUrem
  | Srem -> Formula.BvSrem
  | Or -> Formula.BvOr
  | And -> Formula.BvAnd
  | Xor -> Formula.BvXor
  | Concat -> Formula.BvConcat
  | Lsl -> Formula.BvShl
  | Lsr -> Formula.BvLshr
  | Asr -> Formula.BvAshr
  | Rol | Ror -> assert false

let rec mk_bl ctx bl =
  try
    let var = BvTbl.find ctx.bl_cons bl in
    if var == bl_once then mk_bl_no_cons ctx bl else Formula.mk_bl_var var
  with Not_found -> Formula.mk_bv_equal (mk_bv ctx bl) Formula.mk_bv_one

and mk_bl_no_cons ctx bl =
  match bl with
  | Cst bv ->
      if Bitvector.is_one bv then Formula.mk_bl_true else Formula.mk_bl_false
  | Load _ (* cannot be a bl<1> *) -> assert false
  | Unary { f = Not; x; _ } -> Formula.mk_bl_not (mk_bl ctx x)
  | Binary { f = And; x; y; _ } -> Formula.mk_bl_and (mk_bl ctx x) (mk_bl ctx y)
  | Binary { f = Or; x; y; _ } -> Formula.mk_bl_or (mk_bl ctx x) (mk_bl ctx y)
  | Binary
      {
        f = (Eq | Diff | Uge | Ule | Ugt | Ult | Sge | Sle | Sgt | Slt) as f;
        x;
        y;
        _;
      } ->
      Formula.mk_bv_comp (mk_comp f) (mk_bv ctx x) (mk_bv ctx y)
  | Ite { c; t; e; _ } ->
      Formula.mk_bl_ite (mk_bl ctx c) (mk_bl ctx t) (mk_bl ctx e)
  | Var _ | Unary _ | Binary _ ->
      Formula.mk_bv_equal (mk_bv ctx bl) Formula.mk_bv_one

and mk_bv ctx bv =
  let var = BvTbl.find ctx.bv_cons bv in
  if var == bv_once then mk_bv_no_cons ctx bv else Formula.mk_bv_var var

and mk_bv_no_cons ctx bv =
  match bv with
  | Var _ -> assert false
  | Load { len; dir = LittleEndian; addr; label; _ } ->
      Formula.mk_select len (mk_ax ctx label) (mk_bv ctx addr)
  | Load { len; dir = BigEndian; addr; label; _ } ->
      mk_select_be len (mk_ax ctx label) (mk_bv ctx addr)
  | Cst bv -> Formula.mk_bv_cst bv
  | Unary { f; x; _ } -> Formula.mk_bv_unop (mk_unop f) (mk_bv ctx x)
  | Binary { f = Eq | Diff | Uge | Ule | Ugt | Ult | Sge | Sle | Sgt | Slt; _ }
    ->
      Formula.mk_bv_ite (mk_bl ctx bv) Formula.mk_bv_one Formula.mk_bv_zero
  | Binary { f = Rol; x; y = Cst bv; _ } ->
      Formula.mk_bv_rotate_left (Bv.to_uint bv) (mk_bv ctx x)
  | Binary { f = Ror; x; y = Cst bv; _ } ->
      Formula.mk_bv_rotate_right (Bv.to_uint bv) (mk_bv ctx x)
  | Binary { f = Rol; x; y; _ } ->
      Formula.mk_bv_or
        (Formula.mk_bv_shl (mk_bv ctx x) (mk_bv ctx y))
        (Formula.mk_bv_lshr (mk_bv ctx x)
           (Formula.mk_bv_sub_int (mk_bv ctx y) (Expr.sizeof x)))
  | Binary { f = Ror; x; y; _ } ->
      Formula.mk_bv_or
        (Formula.mk_bv_lshr (mk_bv ctx x) (mk_bv ctx y))
        (Formula.mk_bv_shl (mk_bv ctx x)
           (Formula.mk_bv_sub_int (mk_bv ctx y) (Expr.sizeof x)))
  | Binary { f; x; y; _ } ->
      Formula.mk_bv_bnop (mk_bnop f) (mk_bv ctx x) (mk_bv ctx y)
  | Ite { c; t; e; _ } ->
      Formula.mk_bv_ite (mk_bl ctx c) (mk_bv ctx t) (mk_bv ctx e)

and mk_ax ctx ax =
  let var = AxTbl.find ctx.ax_cons ax in
  if var == ax_once then mk_ax_no_cons ctx ax else Formula.mk_ax_var var

and mk_ax_no_cons : t -> Memory.t -> Formula.ax_term =
 fun ctx ax ->
  match ax with
  | Symbol _ -> assert false
  | Layer { addr; store; over; _ } ->
      let addr = mk_bv ctx addr in
      Store.fold_term
        (fun i bv store ->
          Formula.mk_store
            (Expr.sizeof bv lsr 3)
            store
            (Formula.mk_bv_add addr
               (Formula.mk_bv_cst (Bitvector.create i ctx.idx_size)))
            (mk_bv ctx bv))
        (mk_ax ctx over) store

and mk_select_be =
  let rec mk_select_be len ax bv sel =
    if len = 0 then sel
    else
      let len = len - 1 in
      mk_select_be len ax bv
        (Formula.mk_bv_concat sel
           (Formula.mk_select 1 ax (Formula.mk_bv_add_int bv len)))
  in
  fun len ax bv ->
    let len = len - 1 in
    mk_select_be len ax bv
      (Formula.mk_select 1 ax (Formula.mk_bv_add_int bv len))

let to_formula ctx =
  Queue.fold
    (fun formula -> function
      | Bl bl ->
          let var = BvTbl.find ctx.bl_cons bl in
          if var != bl_once then
            Formula.push_front_define
              (Formula.mk_bl_def var [] (mk_bl_no_cons ctx bl))
              formula
          else formula
      | Bv bv ->
          let var = BvTbl.find ctx.bv_cons bv in
          if var != bv_once then
            Formula.push_front_define
              (Formula.mk_bv_def var [] (mk_bv_no_cons ctx bv))
              formula
          else formula
      | Ax ax ->
          let var = AxTbl.find ctx.ax_cons ax in
          if var != ax_once then
            Formula.push_front_define
              (Formula.mk_ax_def var [] (mk_ax_no_cons ctx ax))
              formula
          else formula
      | Assert bl -> Formula.push_front_assert (mk_bl ctx bl) formula
      | BvDefine (var, bv) ->
          Formula.push_front_define
            (Formula.mk_bv_def var [] (mk_bv ctx bv))
            formula
      | AxDefine (var, ax) ->
          Formula.push_front_define
            (Formula.mk_ax_def var [] (mk_ax ctx ax))
            formula)
    (BvTbl.fold
       (fun _ decl formula -> Formula.push_front_declare decl formula)
       ctx.bv_decl
       (AxTbl.fold
          (fun _ decl formula -> Formula.push_front_declare decl formula)
          ctx.ax_decl Formula.empty))
    ctx.ordered
