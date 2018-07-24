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
open Formula_utils

let rename_bl_var f bl =
  bl_var (f bl.bl_name)

let rename_bv_var f bv =
  bv_var (f bv.bv_name) bv.bv_size

let rename_ax_var f ax =
  ax_var (f ax.ax_name) ax.idx_size ax.elt_size

let rename_list : 'env 'a 'b.
  ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list =
  fun f env ls -> List.map (f env) ls

let rec rename_term f tm =
  rename_term_desc f tm.term_desc

and rename_term_desc f = function
  | BlTerm bl -> mk_bl_term (rename_bl_term f bl)
  | BvTerm bv -> mk_bv_term (rename_bv_term f bv)
  | AxTerm ax -> mk_ax_term (rename_ax_term f ax)

and rename_bl_term f bl =
  rename_bl_term_desc f bl.bl_term_desc

and rename_bl_term_desc f = function
  | BlTrue -> mk_bl_true
  | BlFalse -> mk_bl_false
  | BlFun (v,ls) ->
    mk_bl_fun (rename_bl_var f v) (rename_list rename_term f ls)
  | BlLet (bn,bl) ->
    mk_bl_let (rename_list rename_def f bn) (rename_bl_term f bl)
  | BlUnop (u,bl) ->
    mk_bl_unop u (rename_bl_term f bl)
  | BlBnop (b,bl1,bl2) ->
    mk_bl_bnop b (rename_bl_term f bl1) (rename_bl_term f bl2)
  | BlComp (c,bl1,bl2) ->
    mk_bl_comp c (rename_bl_term f bl1) (rename_bl_term f bl2)
  | BvComp (c,bv1,bv2) ->
    mk_bv_comp c (rename_bv_term f bv1) (rename_bv_term f bv2)
  | AxComp (c,ax1,ax2) ->
    mk_ax_comp c (rename_ax_term f ax1) (rename_ax_term f ax2)
  | BlIte (bl,bl1,bl2) ->
    mk_bl_ite (rename_bl_term f bl) (rename_bl_term f bl1) (rename_bl_term f bl2)

and rename_bv_term f bv =
  rename_bv_term_desc f bv.bv_term_desc

and rename_bv_term_desc f = function
  | BvCst bv -> mk_bv_cst bv
  | BvFun (v,ls) ->
    mk_bv_fun (rename_bv_var f v) (rename_list rename_term f ls)
  | BvLet (bn,bv) ->
    mk_bv_let (rename_list rename_def f bn) (rename_bv_term f bv)
  | BvUnop (u,bv) ->
    mk_bv_unop u (rename_bv_term f bv)
  | BvBnop (b,bv1,bv2) ->
    mk_bv_bnop b (rename_bv_term f bv1) (rename_bv_term f bv2)
  | BvIte (bl,bv1,bv2) ->
    mk_bv_ite (rename_bl_term f bl) (rename_bv_term f bv1) (rename_bv_term f bv2)
  | Select (n,ax,bv) -> mk_select n (rename_ax_term f ax) (rename_bv_term f bv)

and rename_ax_term f ax =
  rename_ax_term_desc f ax.ax_term_desc

and rename_ax_term_desc f = function
  | AxFun (v,ls) ->
    mk_ax_fun (rename_ax_var f v) (rename_list rename_term f ls)
  | AxLet (bn,ax) ->
    mk_ax_let (rename_list rename_def f bn) (rename_ax_term f ax)
  | AxIte (bl,ax1,ax2) ->
    mk_ax_ite (rename_bl_term f bl) (rename_ax_term f ax1) (rename_ax_term f ax2)
  | Store (n,ax,bv1,bv2) ->
    mk_store n (rename_ax_term f ax) (rename_bv_term f bv1) (rename_bv_term f bv2)

and rename_def f df =
  rename_def_desc f df.def_desc

and rename_def_desc f = function
  | BlDef (v,ls,bl) ->
    mk_bl_def (rename_bl_var f v) (rename_list rename_decl f ls) (rename_bl_term f bl)
  | BvDef (v,ls,bv) ->
    mk_bv_def (rename_bv_var f v) (rename_list rename_decl f ls) (rename_bv_term f bv)
  | AxDef (v,ls,ax) ->
    mk_ax_def (rename_ax_var f v) (rename_list rename_decl f ls) (rename_ax_term f ax)

and rename_decl f dc =
  rename_decl_desc f dc.decl_desc

and rename_decl_desc f = function
  | BlDecl (v,ls) -> mk_bl_decl (rename_bl_var f v) ls
  | BvDecl (v,ls) -> mk_bv_decl (rename_bv_var f v) ls
  | AxDecl (v,ls) -> mk_ax_decl (rename_ax_var f v) ls


let defs_shadow s dfs =
  match s.def_desc with
  | BlDef (v,_,_) ->
    List.fold_left (fun bool df ->
        bool || match df.def_desc with
        | BvDef _ | AxDef _ -> false
        | BlDef (v',_,_) -> v = v')
      false dfs
  | BvDef (v,_,_) ->
    List.fold_left (fun bool df ->
        bool || match df.def_desc with
        | BlDef _ | AxDef _ -> false
        | BvDef (v',_,_) -> v = v')
      false dfs
  | AxDef (v,_,_) ->
    List.fold_left (fun bool df ->
        bool || match df.def_desc with
        | BlDef _ | BvDef _ -> false
        | AxDef (v',_,_) -> v = v')
      false dfs

let decls_shadow s dcs =
  match s.def_desc with
  | BlDef (v,_,_) ->
    List.fold_left (fun bool dc ->
        bool || match dc.decl_desc with
        | BvDecl _ | AxDecl _ -> false
        | BlDecl (v',_) -> v = v')
      false dcs
  | BvDef (v,_,_) ->
    List.fold_left (fun bool dc ->
        bool || match dc.decl_desc with
        | BlDecl _ | AxDecl _ -> false
        | BvDecl (v',_) -> v = v')
      false dcs
  | AxDef (v,_,_) ->
    List.fold_left (fun bool dc ->
        bool || match dc.decl_desc with
        | BlDecl _ | BvDecl _ -> false
        | AxDecl (v',_) -> v = v')
      false dcs

let replace_list : 'env 'a 'b.
  ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list =
  fun f env ls -> List.map (f env) ls

let rec replace_term s tm =
  replace_term_desc s tm.term_desc

and replace_term_desc s = function
  | BlTerm bl -> mk_bl_term (replace_bl_term s bl)
  | BvTerm bv -> mk_bv_term (replace_bv_term s bv)
  | AxTerm ax -> mk_ax_term (replace_ax_term s ax)

and replace_bl_term s bl =
  replace_bl_term_desc s bl.bl_term_desc

and replace_bl_term_desc s = function
  | BlTrue -> mk_bl_true
  | BlFalse -> mk_bl_false
  | BlFun (v,ls) ->
    (match s.def_desc with
     | BvDef (_,_,_) | AxDef (_,_,_) ->
       mk_bl_fun v (replace_list replace_term s ls)
     | BlDef (v',[],bl') ->
       if v = v' then
         if ls = [] then bl'
         else invalid_arg "Ill-typed replacement"
       else mk_bl_fun v (replace_list replace_term s ls)
     | BlDef _ -> invalid_arg "Ill-typed replacement")
  | BlLet (bn,bl) ->
    if defs_shadow s bn then mk_bl_let (replace_list replace_def s bn) bl
    else mk_bl_let (replace_list replace_def s bn) (replace_bl_term s bl)
  | BlUnop (u,bl) ->
    mk_bl_unop u (replace_bl_term s bl)
  | BlBnop (b,bl1,bl2) ->
    mk_bl_bnop b (replace_bl_term s bl1) (replace_bl_term s bl2)
  | BlComp (c,bl1,bl2) ->
    mk_bl_comp c (replace_bl_term s bl1) (replace_bl_term s bl2)
  | BvComp (c,bv1,bv2) ->
    mk_bv_comp c (replace_bv_term s bv1) (replace_bv_term s bv2)
  | AxComp (c,ax1,ax2) ->
    mk_ax_comp c (replace_ax_term s ax1) (replace_ax_term s ax2)
  | BlIte (bl,bl1,bl2) ->
    mk_bl_ite (replace_bl_term s bl) (replace_bl_term s bl1) (replace_bl_term s bl2)

and replace_bv_term s bv =
  replace_bv_term_desc s bv.bv_term_desc

and replace_bv_term_desc s = function
  | BvCst bv -> mk_bv_cst bv
  | BvFun (v,ls) ->
    (match s.def_desc with
     | BlDef (_,_,_) | AxDef (_,_,_) ->
       mk_bv_fun v (replace_list replace_term s ls)
     | BvDef (v',[],bv') ->
       if v = v' then
         if ls = [] then bv'
         else invalid_arg "Ill-typed replacement"
       else mk_bv_fun v (replace_list replace_term s ls)
     | BvDef _ -> invalid_arg "Ill-typed replacement")
  | BvLet (bn,bv) ->
    if defs_shadow s bn then mk_bv_let (replace_list replace_def s bn) bv
    else mk_bv_let (replace_list replace_def s bn) (replace_bv_term s bv)
  | BvUnop (u,bv) ->
    mk_bv_unop u (replace_bv_term s bv)
  | BvBnop (b,bv1,bv2) ->
    mk_bv_bnop b (replace_bv_term s bv1) (replace_bv_term s bv2)
  | BvIte (bl,bv1,bv2) ->
    mk_bv_ite (replace_bl_term s bl) (replace_bv_term s bv1) (replace_bv_term s bv2)
  | Select (n,ax,bv) ->
    mk_select n (replace_ax_term s ax) (replace_bv_term s bv)

and replace_ax_term s ax =
  replace_ax_term_desc s ax.ax_term_desc

and replace_ax_term_desc s = function
  | AxFun (v,ls) ->
    (match s.def_desc with
     | BlDef (_,_,_) | BvDef (_,_,_) ->
       mk_ax_fun v (replace_list replace_term s ls)
     | AxDef (v',[],ax') ->
       if v = v' then
         if ls = [] then ax'
         else invalid_arg "Ill-typed replacement"
       else mk_ax_fun v (replace_list replace_term s ls)
     | AxDef _ -> invalid_arg "Ill-typed replacement")
  | AxLet (bn,ax) ->
    if defs_shadow s bn then mk_ax_let (replace_list replace_def s bn) ax
    else mk_ax_let (replace_list replace_def s bn) (replace_ax_term s ax)
  | AxIte (bl,ax1,ax2) ->
    mk_ax_ite (replace_bl_term s bl) (replace_ax_term s ax1) (replace_ax_term s ax2)
  | Store (n,ax,bv1,bv2) ->
    mk_store n (replace_ax_term s ax) (replace_bv_term s bv1) (replace_bv_term s bv2)

and replace_def s df =
  replace_def_desc s df.def_desc

and replace_def_desc s = function
  | BlDef (v,ls,bl) ->
    if decls_shadow s ls then mk_bl_def v ls bl
    else mk_bl_def v ls (replace_bl_term s bl)
  | BvDef (v,ls,bv) ->
    if decls_shadow s ls then mk_bv_def v ls bv
    else mk_bv_def v ls (replace_bv_term s bv)
  | AxDef (v,ls,ax) ->
    if decls_shadow s ls then mk_ax_def v ls ax
    else mk_ax_def v ls (replace_ax_term s ax)


(* Assert table  *)

module Assertbl :
sig
  type t
  val create : unit -> t
  val get : t -> bl_term -> bool
  val set : t -> bl_term -> unit
end = struct
  type t = bool BlTermHashtbl.t

  let create () =
    let t = BlTermHashtbl.create 1 in
    BlTermHashtbl.add t mk_bl_true true;
    t

  let get t bl =
    try BlTermHashtbl.find t bl
    with Not_found -> false

  let set t bl =
    BlTermHashtbl.replace t bl true
end


(* Constant propagation *)

module ConstantPropagation =
struct

  type env = {
    assertbl: Assertbl.t;
    bl_htbl : bl_term BlVarHashtbl.t;
    bv_htbl : bv_term BvVarHashtbl.t;
  }

  let create n = {
    assertbl= Assertbl.create ();
    bl_htbl = BlVarHashtbl.create n;
    bv_htbl = BvVarHashtbl.create n;
  }

  let get_assertbl env bl = Assertbl.get env.assertbl bl
  let set_assertbl env bl = Assertbl.set env.assertbl bl

  let is_bl_cst env bl =
    match is_bl_cst bl with
    | Some _ as opt -> opt
    | None ->
      match is_bl_var bl with
      | Some v -> is_bl_cst (BlVarHashtbl.find env.bl_htbl v)
      | None -> None

  let is_bl_var env bl =
    match is_bl_var bl with
    | None -> None
    | Some v -> is_bl_var (BlVarHashtbl.find env.bl_htbl v)

  let is_bv_cst env bv =
    match is_bv_cst bv with
    | Some _ as opt -> opt
    | None ->
      match is_bv_var bv with
      | Some v -> is_bv_cst (BvVarHashtbl.find env.bv_htbl v)
      | None -> None

  let is_bv_var env bv =
    match is_bv_var bv with
    | None -> None
    | Some v -> is_bv_var (BvVarHashtbl.find env.bv_htbl v)

  let add_bl_term env v bl =
    BlVarHashtbl.add env.bl_htbl v
      (try match is_bl_var env bl with
         | None -> bl
         | Some v -> BlVarHashtbl.find env.bl_htbl v
       with Not_found -> bl)

  let add_bv_term env v bv =
    BvVarHashtbl.add env.bv_htbl v
      (try match is_bv_var env bv with
         | None -> bv
         | Some v -> BvVarHashtbl.find env.bv_htbl v
       with Not_found -> bv)

  let add_def env df =
    match df.def_desc with
    | BlDef (v,_,bl) -> add_bl_term env v bl
    | BvDef (v,_,bv) -> add_bv_term env v bv
    | AxDef (_,_,_) -> ()

  let add_defs env dfs = List.iter (add_def env) dfs

  let remove_defs env dfs =
    List.iter
      (fun df ->
         match df.def_desc with
         | BlDef (v,_,_) -> BlVarHashtbl.remove env.bl_htbl v
         | BvDef (v,_,_) -> BvVarHashtbl.remove env.bv_htbl v
         | AxDef (_,_,_) -> ())
      dfs

  let filter_defs env dfs =
    List.filter
      (fun df ->
         match df.def_desc with
         | BlDef (_,_,bl) -> is_bl_cst env bl = None
         | BvDef (_,_,bv) -> is_bv_cst env bv = None
         | AxDef _ -> true)
      dfs

  let add_decl env dc =
    match dc.decl_desc with
    | BlDecl (v,_) -> add_bl_term env v (mk_bl_var v)
    | BvDecl (v,_) -> add_bv_term env v (mk_bv_var v)
    | AxDecl (_,_) -> ()

  let add_decls env dcs = List.iter (add_decl env) dcs

  let remove_decls env dcs =
    List.iter
      (fun dc ->
         match dc.decl_desc with
         | BlDecl (v,_) -> BlVarHashtbl.remove env.bl_htbl v
         | BvDecl (v,_) -> BvVarHashtbl.remove env.bv_htbl v
         | AxDecl (_,_) -> ())
      dcs

  let visit_list : 'env 'a 'b.
    ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list =
    fun f env ls -> List.map (f env) ls

  let rec visit_term env tm =
    visit_term_desc env tm.term_desc

  and visit_term_desc env = function
    | BlTerm bl -> mk_bl_term (visit_bl_term env bl)
    | BvTerm bv -> mk_bv_term (visit_bv_term env bv)
    | AxTerm ax -> mk_ax_term (visit_ax_term env ax)

  and visit_bl_term env bl =
    visit_bl_term_desc env bl.bl_term_desc

  and visit_bl_term_desc env = function
    | BlTrue -> mk_bl_true
    | BlFalse -> mk_bl_false

    | BlFun (v,ls) ->
      let ls = visit_list visit_term env ls in
      let bl = mk_bl_fun v ls in
      (match is_bl_cst env bl with
       | Some bl -> if bl then mk_bl_true else mk_bl_false
       | None ->
         match is_bl_var env bl with
         | Some v -> mk_bl_fun v ls
         | None -> bl)

    | BlLet (bn,bl) ->
      let bn = visit_list visit_def env bn in
      add_defs env bn;
      let bl = visit_bl_term env bl in
      let bn' = filter_defs env bn in
      remove_defs env bn;
      mk_bl_let bn' bl

    | BlUnop (u,bl) ->
      let bl = visit_bl_term env bl in
      mk_bl_unop u bl

    | BlBnop (b,bl1,bl2) ->
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_bnop b bl1 bl2

    | BlComp (c,bl1,bl2) ->
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_comp c bl1 bl2

    | BvComp (c,bv1,bv2) ->
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_comp c bv1 bv2

    | AxComp (c,ax1,ax2) ->
      let ax1 = visit_ax_term env ax1 in
      let ax2 = visit_ax_term env ax2 in
      mk_ax_comp c ax1 ax2

    | BlIte (bl,bl1,bl2) ->
      let bl = visit_bl_term env bl in
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_ite bl bl1 bl2

  and visit_bv_term env bv =
    visit_bv_term_desc env bv.bv_term_desc

  and visit_bv_term_desc env = function
    | BvCst bv -> mk_bv_cst bv

    | BvFun (v,ls) ->
      let ls = visit_list visit_term env ls in
      let bv = mk_bv_fun v ls in
      (match is_bv_cst env bv with
       | Some bv -> mk_bv_cst bv
       | None ->
         match is_bv_var env bv with
         | Some v -> mk_bv_fun v ls
         | None -> bv)

    | BvLet (bn,bv) ->
      let bn = visit_list visit_def env bn in
      add_defs env bn;
      let bv = visit_bv_term env bv in
      let bn' = filter_defs env bn in
      remove_defs env bn;
      mk_bv_let bn' bv

    | BvUnop (u,bv) ->
      let bv = visit_bv_term env bv in
      mk_bv_unop u bv

    | BvBnop (b,bv1,bv2) ->
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_bnop b bv1 bv2

    | BvIte (bl,bv1,bv2) ->
      let bl = visit_bl_term env bl in
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_ite bl bv1 bv2

    | Select (n,ax,bv) ->
      let ax = visit_ax_term env ax in
      let bv = visit_bv_term env bv in
      mk_select n ax bv

  and visit_ax_term env ax =
    visit_ax_term_desc env ax.ax_term_desc

  and visit_ax_term_desc env = function
    | AxFun (v,ls) ->
      let ls = visit_list visit_term env ls in
      mk_ax_fun v ls

    | AxLet (bn,ax) ->
      let bn = visit_list visit_def env bn in
      add_defs env bn;
      let ax = visit_ax_term env ax in
      let bn' = filter_defs env bn in
      remove_defs env bn;
      mk_ax_let bn' ax

    | AxIte (bl,ax1,ax2) ->
      let bl = visit_bl_term env bl in
      let ax1 = visit_ax_term env ax1 in
      let ax2 = visit_ax_term env ax2 in
      mk_ax_ite bl ax1 ax2

    | Store (n,ax,bv1,bv2) ->
      let ax = visit_ax_term env ax in
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_store n ax bv1 bv2

  and visit_def env df =
    visit_def_desc env df.def_desc

  and visit_def_desc env = function
    | BlDef (v,ls,bl) ->
      add_decls env ls;
      let bl = visit_bl_term env bl in
      remove_decls env ls;
      mk_bl_def v ls bl
    | BvDef (v,ls,bv) ->
      add_decls env ls;
      let bv = visit_bv_term env bv in
      remove_decls env ls;
      mk_bv_def v ls bv
    | AxDef (v,ls,ax) ->
      add_decls env ls;
      let ax = visit_ax_term env ax in
      remove_decls env ls;
      mk_ax_def v ls ax

  and visit_entry env en =
    visit_entry_desc env en.entry_desc

  and visit_entry_desc env = function
    | Declare dc ->
      add_decl env dc;
      mk_declare dc
    | Define df ->
      let df = visit_def env df in
      add_def env df;
      mk_define df
    | Assert bl ->
      let bl = visit_bl_term env bl in
      (match bl.bl_term_desc with
       | BlComp (BlEqual,bl1,bl2) ->
         (match is_bl_var env bl1, is_bl_var env bl2 with
          | None, None | Some _, Some _ -> ()
          | Some v, None -> add_bl_term env v bl2
          | None, Some v -> add_bl_term env v bl1)
       | BvComp (BvEqual,bv1,bv2) ->
         (match is_bv_var env bv1, is_bv_var env bv2 with
          | None, None | Some _, Some _ -> ()
          | Some v, None -> add_bv_term env v bv2
          | None, Some v -> add_bv_term env v bv1)
       | _ -> ());
      mk_assert bl
    | Comment s -> mk_comment s
    | Echo s -> mk_echo s
    | Check_sat -> mk_check_sat

end

let constant_propagation ?(keep=VarSet.empty) fm =
  let env = ConstantPropagation.create (length fm / 4) in
  fold_forward
    (fun entry fm ->
       let entry = ConstantPropagation.visit_entry env entry in
       match entry.entry_desc with
       | Declare _ | Comment _ | Echo _ | Check_sat -> push_front entry fm
       | Define df ->
         if
           (match df.def_desc with
            | BlDef (v,_,bl) ->
              (is_bl_cst bl = None && is_bl_var bl = None)
              || VarSet.mem (BlVar v) keep
            | BvDef (v,_,bv) ->
              (is_bv_cst bv = None && is_bv_var bv = None)
              || VarSet.mem (BvVar v) keep
            | AxDef (_,_,_) -> true)
         then push_front entry fm
         else fm
       | Assert bl ->
         if ConstantPropagation.get_assertbl env bl then fm
         else (ConstantPropagation.set_assertbl env bl; push_front entry fm))
    fm empty


(* Prune and inline *)

module PruneAndInline =
struct
  type env = {
    keep : VarSet.t;
    bl_count : int BlVarHashtbl.t;
    bv_count : int BvVarHashtbl.t;
    ax_count : int AxVarHashtbl.t;
    bl_htbl  : bl_term BlVarHashtbl.t;
    bv_htbl  : bv_term BvVarHashtbl.t;
    ax_htbl  : ax_term AxVarHashtbl.t;
  }

  let create keep n = {
    keep;
    bl_count = BlVarHashtbl.create n;
    bv_count = BvVarHashtbl.create n;
    ax_count = AxVarHashtbl.create n;
    bl_htbl  = BlVarHashtbl.create n;
    bv_htbl  = BvVarHashtbl.create n;
    ax_htbl  = AxVarHashtbl.create n;
  }

  let count_bl_var env v =
    try BlVarHashtbl.find env.bl_count v
    with Not_found -> 0

  let count_bv_var env v =
    try BvVarHashtbl.find env.bv_count v
    with Not_found -> 0

  let count_ax_var env v =
    try AxVarHashtbl.find env.ax_count v
    with Not_found -> 0

  let incr_bl_var env v =
    BlVarHashtbl.replace env.bl_count v (count_bl_var env v + 1)

  let incr_bv_var env v =
    BvVarHashtbl.replace env.bv_count v (count_bv_var env v + 1)

  let incr_ax_var env v =
    AxVarHashtbl.replace env.ax_count v (count_ax_var env v + 1)

  let find_bl_var env v =
    try Some (BlVarHashtbl.find env.bl_htbl v)
    with Not_found -> None

  let find_bv_var env v =
    try Some (BvVarHashtbl.find env.bv_htbl v)
    with Not_found -> None

  let find_ax_var env v =
    try Some (AxVarHashtbl.find env.ax_htbl v)
    with Not_found -> None

  let add_def env df =
    match df.def_desc with
    | BlDef (v,_,bl) -> BlVarHashtbl.add env.bl_htbl v bl
    | BvDef (v,_,bv) -> BvVarHashtbl.add env.bv_htbl v bv
    | AxDef (v,_,ax) -> AxVarHashtbl.add env.ax_htbl v ax

  let add_defs env dfs = List.iter (add_def env) dfs

  let remove_defs env dfs =
    List.iter
      (fun df ->
         match df.def_desc with
         | BlDef (v,_,_) -> BlVarHashtbl.remove env.bl_htbl v
         | BvDef (v,_,_) -> BvVarHashtbl.remove env.bv_htbl v
         | AxDef (v,_,_) -> AxVarHashtbl.remove env.ax_htbl v)
      dfs

  let filter_defs env dfs =
    List.filter
      (fun df ->
         match df.def_desc with
         | BlDef (v,_,_) -> count_bl_var env v > 0
         | BvDef (v,_,_) -> count_bv_var env v > 0
         | AxDef (v,_,_) -> count_ax_var env v > 0)
      dfs

  let add_decl env dc =
    match dc.decl_desc with
    | BlDecl (v,_) -> BlVarHashtbl.add env.bl_htbl v (mk_bl_var v)
    | BvDecl (v,_) -> BvVarHashtbl.add env.bv_htbl v (mk_bv_var v)
    | AxDecl (v,_) -> AxVarHashtbl.add env.ax_htbl v (mk_ax_var v)

  let add_decls env dcs = List.iter (add_decl env) dcs

  let remove_decls env dcs =
    List.iter
      (fun dc ->
         match dc.decl_desc with
         | BlDecl (v,_) -> BlVarHashtbl.remove env.bl_htbl v
         | BvDecl (v,_) -> BvVarHashtbl.remove env.bv_htbl v
         | AxDecl (v,_) -> AxVarHashtbl.remove env.ax_htbl v)
      dcs

  let keep_bl_def env v =
    count_bl_var env v > 1 || VarSet.mem (BlVar v) env.keep

  let keep_bv_def env v =
    count_bv_var env v > 1 || VarSet.mem (BvVar v) env.keep

  let keep_ax_def env v ax =
    count_ax_var env v > 1 || (count_ax_var env v = 1 && is_ax_var ax = None)
    || VarSet.mem (AxVar v) env.keep

  let keep_bl_decl env v =
    count_bl_var env v > 0 || VarSet.mem (BlVar v) env.keep

  let keep_bv_decl env v =
    count_bv_var env v > 0 || VarSet.mem (BvVar v) env.keep

  let keep_ax_decl env v =
    count_ax_var env v > 0 || VarSet.mem (AxVar v) env.keep

  let count_list : 'env 'a.
    ('env -> 'a -> unit) -> 'env -> 'a list -> unit =
    fun f env ls -> List.iter (f env) ls


  let rec count_term env tm =
    count_term_desc env tm.term_desc

  and count_term_desc env = function
    | BlTerm bl -> count_bl_term env bl
    | BvTerm bv -> count_bv_term env bv
    | AxTerm ax -> count_ax_term env ax

  and count_bl_term env bl =
    count_bl_term_desc env bl.bl_term_desc

  and count_bl_term_desc env = function
    | BlTrue | BlFalse -> ()
    | BlFun (v,ls) ->
      incr_bl_var env v;
      count_list count_term env ls
    | BlLet (bn,bl) ->
      count_bl_term env bl;
      count_list count_def env bn
    | BlUnop (_,bl) ->
      count_bl_term env bl
    | BlBnop (_,bl1,bl2) ->
      count_bl_term env bl1;
      count_bl_term env bl2
    | BlComp (_,bl1,bl2) ->
      count_bl_term env bl1;
      count_bl_term env bl2
    | BvComp (_,bv1,bv2) ->
      count_bv_term env bv1;
      count_bv_term env bv2
    | AxComp (_,ax1,ax2) ->
      count_ax_term env ax1;
      count_ax_term env ax2
    | BlIte (bl,bl1,bl2) ->
      count_bl_term env bl;
      count_bl_term env bl1;
      count_bl_term env bl2

  and count_bv_term env bv =
    count_bv_term_desc env bv.bv_term_desc

  and count_bv_term_desc env = function
    | BvCst _ -> ()
    | BvFun (v,ls) ->
      incr_bv_var env v;
      count_list count_term env ls
    | BvLet (bn,bv) ->
      count_bv_term env bv;
      count_list count_def env bn
    | BvUnop (_,bv) ->
      count_bv_term env bv
    | BvBnop (_,bv1,bv2) ->
      count_bv_term env bv1;
      count_bv_term env bv2
    | BvIte (bl,bv1,bv2) ->
      count_bl_term env bl;
      count_bv_term env bv1;
      count_bv_term env bv2
    | Select (_,ax,bv) ->
      count_ax_term env ax;
      count_bv_term env bv

  and count_ax_term env ax =
    count_ax_term_desc env ax.ax_term_desc

  and count_ax_term_desc env = function
    | AxFun (v,ls) ->
      incr_ax_var env v;
      count_list count_term env ls
    | AxLet (bn,ax) ->
      count_ax_term env ax;
      count_list count_def env bn
    | AxIte (bl,ax1,ax2) ->
      count_bl_term env bl;
      count_ax_term env ax1;
      count_ax_term env ax2
    | Store (_,ax,bv1,bv2) ->
      count_ax_term env ax;
      count_bv_term env bv1;
      count_bv_term env bv2

  and count_def env df =
    count_def_desc env df.def_desc

  and count_def_desc env = function
    | BlDef (v,ls,bl) ->
      add_decls env ls;
      if keep_bl_decl env v then count_bl_term env bl;
      remove_decls env ls
    | BvDef (v,ls,bv) ->
      add_decls env ls;
      if keep_bv_decl env v then count_bv_term env bv;
      remove_decls env ls
    | AxDef (v,ls,ax) ->
      add_decls env ls;
      if keep_ax_decl env v then count_ax_term env ax;
      remove_decls env ls

  and count_entry env en =
    count_entry_desc env en.entry_desc

  and count_entry_desc env = function
    | Declare _ -> ()
    | Define df -> add_def env df; count_def env df
    | Assert bl -> count_bl_term env bl
    | Comment _ | Echo _ | Check_sat -> ()


  let visit_list : 'env 'a.
    ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list =
    fun f env ls -> List.map (f env) ls

  let rec visit_term env tm =
    visit_term_desc env tm.term_desc

  and visit_term_desc env = function
    | BlTerm bl -> mk_bl_term (visit_bl_term env bl)
    | BvTerm bv -> mk_bv_term (visit_bv_term env bv)
    | AxTerm ax -> mk_ax_term (visit_ax_term env ax)

  and visit_bl_term env bl =
    visit_bl_term_desc env bl.bl_term_desc

  and visit_bl_term_desc env = function
    | BlTrue -> mk_bl_true
    | BlFalse -> mk_bl_false

    | BlFun (v,ls) ->
      let ls = visit_list visit_term env ls in
      if VarSet.mem (BlVar v) env.keep then mk_bl_fun v ls
      else
      if count_bl_var env v = 1 then
        match find_bl_var env v with
        | None -> mk_bl_fun v ls
        | Some bl ->
          match is_bl_var bl with
          | None -> visit_bl_term env bl
          | Some v' ->
            if v = v' then mk_bl_fun v ls
            else visit_bl_term env (mk_bl_fun v' ls)
      else mk_bl_fun v ls

    | BlLet (bn,bl) ->
      let bn = visit_list visit_def env bn in
      add_defs env bn;
      let bl = visit_bl_term env bl in
      let bn' = filter_defs env bn in
      remove_defs env bn;
      mk_bl_let bn' bl

    | BlUnop (u,bl) ->
      let bl = visit_bl_term env bl in
      mk_bl_unop u bl
    | BlBnop (b,bl1,bl2) ->
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_bnop b bl1 bl2
    | BlComp (c,bl1,bl2) ->
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_comp c bl1 bl2
    | BvComp (c,bv1,bv2) ->
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_comp c bv1 bv2
    | AxComp (c,ax1,ax2) ->
      let ax1 = visit_ax_term env ax1 in
      let ax2 = visit_ax_term env ax2 in
      mk_ax_comp c ax1 ax2
    | BlIte (bl,bl1,bl2) ->
      let bl = visit_bl_term env bl in
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_ite bl bl1 bl2

  and visit_bv_term env bv =
    visit_bv_term_desc env bv.bv_term_desc

  and visit_bv_term_desc env = function
    | BvCst bv -> mk_bv_cst bv

    | BvFun (v,ls) ->
      let ls = visit_list visit_term env ls in
      if VarSet.mem (BvVar v) env.keep then mk_bv_fun v ls
      else
      if count_bv_var env v = 1 then
        match find_bv_var env v with
        | None -> mk_bv_fun v ls
        | Some bv ->
          match is_bv_var bv with
          | None -> visit_bv_term env bv
          | Some v' ->
            if v = v' then mk_bv_fun v ls
            else visit_bv_term env (mk_bv_fun v' ls)
      else mk_bv_fun v ls

    | BvLet (bn,bv) ->
      let bn = visit_list visit_def env bn in
      add_defs env bn;
      let bv = visit_bv_term env bv in
      let bn' = filter_defs env bn in
      remove_defs env bn;
      mk_bv_let bn' bv

    | BvUnop (u,bv) ->
      let bv = visit_bv_term env bv in
      mk_bv_unop u bv
    | BvBnop (b,bv1,bv2) ->
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_bnop b bv1 bv2
    | BvIte (bl,bv1,bv2) ->
      let bl = visit_bl_term env bl in
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_ite bl bv1 bv2
    | Select (n,ax,bv) ->
      let ax = visit_ax_term env ax in
      let bv = visit_bv_term env bv in
      mk_select n ax bv

  and visit_ax_term env ax =
    visit_ax_term_desc env ax.ax_term_desc

  and visit_ax_term_desc env = function
    | AxFun (v,ls) ->
      let ls = visit_list visit_term env ls in
      if VarSet.mem (AxVar v) env.keep then mk_ax_fun v ls
      else (* Arbitrary choice: no array inlining *)
        (match find_ax_var env v with
         | None -> mk_ax_fun v ls
         | Some ax ->
           match is_ax_var ax with
           | None -> mk_ax_fun v ls
           | Some v' ->
             if v = v' then mk_ax_fun v ls
             else visit_ax_term env (mk_ax_fun v' ls))

    | AxLet (bn,ax) ->
      let bn = visit_list visit_def env bn in
      add_defs env bn;
      let ax = visit_ax_term env ax in
      let bn' = filter_defs env bn in
      remove_defs env bn;
      mk_ax_let bn' ax

    | AxIte (bl,ax1,ax2) ->
      let bl = visit_bl_term env bl in
      let ax1 = visit_ax_term env ax1 in
      let ax2 = visit_ax_term env ax2 in
      mk_ax_ite bl ax1 ax2
    | Store (n,ax,bv1,bv2) ->
      let ax = visit_ax_term env ax in
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_store n ax bv1 bv2

  and visit_def env df =
    visit_def_desc env df.def_desc

  and visit_def_desc env = function
    | BlDef (v,ls,bl) ->
      add_decls env ls;
      let bl = if keep_bl_def env v then visit_bl_term env bl else bl in
      remove_decls env ls;
      mk_bl_def v ls bl
    | BvDef (v,ls,bv) ->
      add_decls env ls;
      let bv = if keep_bv_def env v then visit_bv_term env bv else bv in
      remove_decls env ls;
      mk_bv_def v ls bv
    | AxDef (v,ls,ax) ->
      add_decls env ls;
      let ax = if keep_ax_def env v ax then visit_ax_term env ax else ax in
      remove_decls env ls;
      mk_ax_def v ls ax

  and visit_entry env en =
    visit_entry_desc env en.entry_desc

  and visit_entry_desc env = function
    | Declare dc -> mk_declare dc
    | Define df -> mk_define (visit_def env df)
    | Assert bl -> mk_assert (visit_bl_term env bl)
    | Comment s -> mk_comment s
    | Echo s -> mk_echo s
    | Check_sat -> mk_check_sat
end

let prune_and_inline ?(keep=VarSet.empty) fm =
  let env = PruneAndInline.create keep (length fm / 4) in
  iter_backward (PruneAndInline.count_entry env) fm;
  fold_backward
    (fun entry fm ->
       let entry = PruneAndInline.visit_entry env entry in
       match entry.entry_desc with
       | Declare dc ->
         if
           (match dc.decl_desc with
            | BlDecl (v,_) -> PruneAndInline.keep_bl_decl env v
            | BvDecl (v,_) -> PruneAndInline.keep_bv_decl env v
            | AxDecl (v,_) -> PruneAndInline.keep_ax_decl env v)
         then push_back entry fm
         else fm
       | Define df ->
         if
           (match df.def_desc with
            | BlDef (v,_,_) -> PruneAndInline.keep_bl_def env v
            | BvDef (v,_,_) -> PruneAndInline.keep_bv_def env v
            | AxDef (v,_,ax) -> PruneAndInline.keep_ax_def env v ax)
         then push_back entry fm
         else fm
       | Assert _ | Comment _ | Echo _ | Check_sat -> push_back entry fm)
    fm empty


(* Read over write *)

module ReadOverWrite =
struct

  module PH (K: Hashtbl.HashedType) = struct

    module H = Hashtbl.Make(K)

    type 'a t = { mutable data : 'a data }
    and 'a data =
      | Array of 'a option H.t
      | Diff of K.t * 'a option * 'a t

    let create n = { data = Array (H.create n) }

    let safe_find a x =
      try H.find a x
      with Not_found -> H.add a x None; None

    let rec rebase t k =
      match t.data with
      | Array _ -> k ()
      | Diff (idx, elt, t') ->
        rebase t'
          (fun () ->
             (match t'.data with
              | Array a as p ->
                let elt' = safe_find a idx in
                H.replace a idx elt;
                t.data <- p;
                t'.data <- Diff (idx, elt', t)
              | Diff _ -> assert false);
             k())

    let rebase t = rebase t (fun () -> ())

    let set t idx elt =
      rebase t;
      match t.data with
      | Array a as p ->
        let old = safe_find a idx in
        if old == elt then t
        else (
          H.replace a idx elt;
          let res = { data = p } in
          t.data <- Diff (idx, old, res);
          res)
      | Diff _ -> assert false
(*
    let clear t idx = set t idx None
*)
    let set t idx elt = set t idx (Some elt)

    let get t =
      match t.data with
      | Array a -> a
      | Diff (_,_,_) ->
        rebase t;
        match t.data with
        | Array a -> a
        | Diff (_,_,_) -> assert false
(*
    let fold f t =
      let t = get t in
      H.fold
        (fun key elt acc ->
           match elt with
           | None -> acc
           | Some elt -> f key elt acc)
        t
*)
    let get t idx = safe_find (get t) idx
  end

  type address = {
    base : bv_term;
    delta : Bigint.t;
  }

  let default base = { base; delta = Bigint.zero_big_int }

  let get_address bv =
    match bv.bv_term_desc with
    | BvCst bv ->
      let base = mk_bv_zero in
      let delta = Bitvector.value_of bv in
      { base; delta }
    | BvBnop (b,bv1,bv2) ->
      (match b with
       | BvAdd ->
         (match is_bv_cst bv1, is_bv_cst bv2 with
          | Some bv1, Some bv2 ->
            default (mk_bv_cst (Bitvector.add bv1 bv2))
          | Some bv1, None ->
            { base = bv2; delta = Bitvector.value_of bv1 }
          | None, Some bv2 ->
            { base = bv1; delta = Bitvector.value_of bv2 }
          | None, None -> default bv)
       | BvSub ->
         (match is_bv_cst bv1, is_bv_cst bv2 with
          | Some bv1, Some bv2 ->
            default (mk_bv_cst (Bitvector.sub bv1 bv2))
          | Some bv1, None ->
            { base = mk_bv_neg bv2; delta = Bitvector.value_of bv1 }
          | None, Some bv2 ->
            { base = bv1; delta = Bigint.minus_big_int (Bitvector.value_of bv2)}
          | None, None -> default bv)
       | _ -> default bv)
    | BvFun (_,_)
    | BvLet (_,_)
    | BvUnop (_,_)
    | BvIte (_,_,_)
    | Select (_,_,_) -> default bv

  let no_result results = Basic_types.Array.for_all ((=) None) results

  let get_result results =
    let bv = ref (results.(0)) in
    Array.iteri
      (fun i opt -> if i > 0 then
          match !bv with
          | None -> ()
          | Some bv1 ->
            match opt with
            | None -> bv := None
            | Some bv2 -> bv := Some (mk_bv_concat bv2 bv1))
      results; !bv

  let update_result results address m addr n bv =
    let size = bv.bv_term_size / n in
    let open Bigint in
    if eq_big_int address.delta addr.delta && m = n && no_result results
    then Some bv (* perfect match *)
    else begin
      let delta_m = pred_big_int (add_int_big_int m address.delta) in
      let delta_n = pred_big_int (add_int_big_int n addr.delta) in
      if lt_big_int delta_m addr.delta ||
         lt_big_int delta_n address.delta
      then None (* no interval intersection *)
      else
        let max_big = min_big_int delta_m delta_n in
        let min_big = max_big_int address.delta addr.delta in
        let base_m  = sub_big_int min_big address.delta |> int_of_big_int in
        let base_n  = sub_big_int min_big addr.delta |> int_of_big_int in
        let loop = (sub_big_int max_big min_big |> int_of_big_int) in
        for i = 0 to loop do
          if results.(i + base_m) = None then
            let lo = (base_n + i) * size in
            let hi = (base_n + i + 1) * size - 1 in
            let bv = mk_bv_extract Interval.{lo; hi} bv in
            results.(i + base_m) <- Some bv
        done;
        get_result results
    end

  module type S =
  sig
    type t
    val create : int -> t
    val lookup : t -> int -> ax_term -> bv_term -> bv_term
    val update : t -> ax_term -> unit
    val alias  : t -> ax_var -> ax_term -> unit
    val unalias: t -> ax_var -> unit
    val assume : t -> bl_term -> unit
  end

  module DummyEnv : S =
  struct
    type t = unit

    let create _ = ()
    let lookup () n ax bv = mk_select n ax bv
    let update () _ = ()
    let alias () _ _ = ()
    let unalias () _ = ()
    let assume () _ = ()
  end

  let depth = ref max_int

  module ListEnv : S =
  struct
    type t = ax_term AxVarHashtbl.t
    let create n = AxVarHashtbl.create n

    let rec lookup t address m last ax bv results fuel =
      if fuel <= 0 then
        if no_result results && is_ax_var ax <> None
        then mk_select m ax bv
        else mk_select m last bv
      else
        match ax.ax_term_desc with
        | AxFun (v,[]) ->
          let last = if no_result results then ax else last in
          let ax' = AxVarHashtbl.find t v in
          if equal_ax_term ax ax'
          then mk_select m last bv
          else lookup t address m last ax' bv results (fuel-1)
        | AxFun (_,_)
        | AxLet (_,_)
        | AxIte (_,_,_) -> mk_select m last bv
        | Store (n,ax',bv1,bv2) ->
          let addr = get_address bv1 in
          if equal_bv_term address.base addr.base then
            match update_result results address m addr n bv2 with
            | Some bv -> bv
            | None -> lookup t address m last ax' bv results (fuel-1)
          else mk_select m last bv

    let lookup t n ax bv =
      let address = get_address bv in
      let results = Array.init n (fun  _ -> None) in
      lookup t address n ax ax bv results !depth

    let update _ _ = ()

    let alias t v ax =
      let ax =
        match is_ax_var ax with
        | None -> ax
        | Some v ->
          try AxVarHashtbl.find t v
          with Not_found -> ax
      in
      AxVarHashtbl.add t v ax

    let unalias t v = AxVarHashtbl.remove t v

    let assume _ _ = ()
  end

  module MapEnv : S =
  struct

    module PH = PH
        (struct
          type t = Bigint.t
          let equal t1 t2 = Bigint.eq_big_int t1 t2
          let hash t = Hashtbl.hash t
        end)

    type t = (ax_term * bv_term * bv_term PH.t) option AxTermHashtbl.t
    let create n = AxTermHashtbl.create n

    let lookup t n ax bv =
      match AxTermHashtbl.find t ax with
      | None -> mk_select n ax bv
      | Some (array,base,map) ->
        let address = get_address bv in
        if equal_bv_term address.base base
        then
          let results =
            Array.init n
              (fun i -> PH.get map (Bigint.add_int_big_int i address.delta))
          in
          match get_result results with
          | Some bv -> bv
          | None ->
            match is_ax_var array with
            | Some _ -> mk_select n (if no_result results then array else ax) bv
            | None -> mk_select n ax bv
        else mk_select n ax bv

    let rec update map delta n bv size =
      if n < 0 then map
      else
        let lo = n * size in
        let hi = (n + 1) * size - 1 in
        let bv' = mk_bv_extract Interval.{lo; hi} bv in
        update
          (PH.set map (Bigint.add_int_big_int n delta) bv')
          delta (n-1) bv size

    let update t ax =
      match ax.ax_term_desc with
      | AxFun (_,[]) -> ()
      (*match AxTermHashtbl.find t ax with
        | None -> ()
        | Some (ax',bv,map) ->
         match is_ax_var ax' with
         (* if 'ax' is a variable which aliases a term, then replace the full
          * representation by the variable to avoid term duplication *)
         | None -> AxTermHashtbl.replace t ax (Some (ax,bv,map))
         | Some _ -> ()*)
      | AxFun (_,_)
      | AxLet (_,_)
      | AxIte (_,_,_) -> AxTermHashtbl.add t ax None
      | Store (n,ax',bv1,bv2) ->
        let address = get_address bv1 in
        let ax',bv,map =
          match AxTermHashtbl.find t ax' with
          | None -> ax', address.base, PH.create n
          | Some (_,bv,_ as tp) ->
            if equal_bv_term address.base bv then tp
            else ax', address.base, PH.create n
        in
        AxTermHashtbl.add t ax
          (Some (ax',bv,update map address.delta (n-1) bv2 ax.elt_term_size))

    let alias t v ax =
      AxTermHashtbl.add t (mk_ax_var v)
        (try AxTermHashtbl.find t ax
         with Not_found -> None)

    let unalias t v = AxTermHashtbl.remove t (mk_ax_var v)

    let assume _ _ = ()
  end

  module ItvEnv : S =
  struct

    module PH = PH
        (struct
          type t = Bigint.t
          let equal t1 t2 = Bigint.eq_big_int t1 t2
          let hash t = Hashtbl.hash t
        end)

    type t = {
      htbl : (ax_term * bv_term * Interval.BitVecFlat.t * bv_term PH.t) list AxTermHashtbl.t;
      itbl : Interval.BitVecFlat.t BvTermHashtbl.t;
    }
    let create n = {
      htbl = AxTermHashtbl.create n;
      itbl = BvTermHashtbl.create n;
    }

    let get_interval t bv =
      try BvTermHashtbl.find t.itbl bv
      with Not_found -> Interval.BitVecFlat.top (bv_size bv)

    let set_interval t bv itv =
      get_interval t bv
      |> Interval.BitVecFlat.inter itv
      |> BvTermHashtbl.replace t.itbl bv

    let lookup t n ax bv =
      let address = get_address bv in
      match
        List.filter
          (fun (_,base,_,_) -> equal_bv_term address.base base)
          (AxTermHashtbl.find t.htbl ax)
      with
      | [] -> mk_select n ax bv
      | [(array,_,_,map)] ->
        let results =
          Array.init n
            (fun i -> PH.get map (Bigint.add_int_big_int i address.delta))
        in
        (match get_result results with
         | Some bv -> bv
         | None ->
           match is_ax_var array with
           | Some _ -> mk_select n (if no_result results then array else ax) bv
           | None -> mk_select n ax bv)
      | _ -> assert false

    let shift n itv bv =
      let lo = Bitvector.create bv (Bitvector.size_of itv.Interval.lo) in
      let lo = Bitvector.add itv.Interval.lo lo in
      let hi = Bitvector.create (Bigint.add_int_big_int n bv) (Bitvector.size_of itv.Interval.hi) in
      let hi = Bitvector.add itv.Interval.hi hi in
      if Bitvector.ule lo hi
      then Interval.BitVecFlat.(inter (uge lo) (ule hi))
      else Interval.BitVecFlat.(union (uge lo) (ule hi))

    let shift n itv bv =
      Interval.BitVecFlat.fold
        (fun itv acc -> Interval.BitVecFlat.union acc (shift n itv bv))
        itv Interval.BitVecFlat.empty

    let rec update map delta n bv size =
      if n < 0 then map
      else
        let lo = n * size in
        let hi = (n + 1) * size - 1 in
        let bv' = mk_bv_extract Interval.{lo; hi} bv in
        update
          (PH.set map (Bigint.add_int_big_int n delta) bv')
          delta (n-1) bv size

    let update t ax =
      match ax.ax_term_desc with
      | AxFun (_,[]) -> ()
      | AxFun (_,_)
      | AxLet (_,_)
      | AxIte (_,_,_) -> AxTermHashtbl.add t.htbl ax []
      | Store (n,ax',bv1,bv2) ->
        let address = get_address bv1 in
        let itv = get_interval t address.base in
        let sft = shift n itv address.delta in
        let (ax',bv,itv,map),list =
          List.fold_left
            (fun (opt,list) (_,base,itv,_ as c) ->
               if equal_bv_term address.base base
               then c, list
               else if Interval.BitVecFlat.(is_empty (inter sft itv))
               then opt, c :: list
               else opt, list)
            ((ax',address.base,itv,PH.create n),[]) (AxTermHashtbl.find t.htbl ax')
        in
        let itv = Interval.BitVecFlat.union itv sft in
        let map = update map address.delta (n-1) bv2 ax.elt_term_size in
        AxTermHashtbl.add t.htbl ax ((ax',bv,itv,map) :: list)

    let alias t v ax =
      AxTermHashtbl.add t.htbl (mk_ax_var v)
        (try AxTermHashtbl.find t.htbl ax
         with Not_found -> [])

    let unalias t v = AxTermHashtbl.remove t.htbl (mk_ax_var v)

    let rec assume t bl =
      match bl.bl_term_desc with
      | BlBnop (BlAnd,bl1,bl2) ->
        (match assume t bl1, assume t bl2 with
         | None, None | Some _, None | None, Some _ -> None
         | Some (bv1,t1), Some (bv2,t2) ->
           if equal_bv_term bv1 bv2
           then Some (bv1, Interval.BitVecFlat.inter t1 t2)
           else None)

      | BlBnop (BlOr,bl1,bl2) ->
        (match assume t bl1, assume t bl2 with
         | None, None | Some _, None | None, Some _ -> None
         | Some (bv1,t1), Some (bv2,t2) ->
           if equal_bv_term bv1 bv2
           then Some (bv1, Interval.BitVecFlat.union t1 t2)
           else None)

      | BvComp (c,bv1,bv2) ->
        (match is_bv_cst bv2 with
         | None -> None
         | Some bv2 ->
           let open Interval.BitVecFlat in
           Some (bv1,
                 match c with
                 | BvEqual -> equal bv2
                 | BvDistinct -> distinct bv2
                 | BvUlt -> ult bv2
                 | BvUle -> ule bv2
                 | BvUgt -> ugt bv2
                 | BvUge -> uge bv2
                 | BvSlt -> slt bv2
                 | BvSle -> sle bv2
                 | BvSgt -> sgt bv2
                 | BvSge -> sge bv2))
      | _ -> None

    let assume t bl =
      match assume t bl with
      | None -> ()
      | Some (bv,itv) -> set_interval t bv itv

  end

  type pack =
    | Dummy of DummyEnv.t
    | List  of ListEnv.t
    | Map   of MapEnv.t
    | Itv   of ItvEnv.t

  type env = {
    pack     : pack;
    rebase   : bool;
    assertbl : Assertbl.t;
    bl_htbl  : bl_term BlVarHashtbl.t;
    bv_htbl  : bv_term BvVarHashtbl.t;
  }

  let create ~lst ~rbs ~itv n = {
    pack =
      (match lst with
       | None ->
         if itv then Itv (ItvEnv.create n)
         else Map (MapEnv.create n);
       | Some i ->
         if i > 0 then List (depth := i; ListEnv.create n)
         else Dummy (DummyEnv.create n));
    rebase   = rbs;
    assertbl = Assertbl.create ();
    bl_htbl  = BlVarHashtbl.create n;
    bv_htbl  = BvVarHashtbl.create n;
  }

  let get_assertbl env bl = Assertbl.get env.assertbl bl
  let set_assertbl env bl = Assertbl.set env.assertbl bl

  let lookup env n ax bv =
    match env.pack with
    | Dummy env -> DummyEnv.lookup env n ax bv
    | List env -> ListEnv.lookup env n ax bv
    | Map env -> MapEnv.lookup env n ax bv
    | Itv env -> ItvEnv.lookup env n ax bv

  let update env ax =
    match env.pack with
    | Dummy env -> DummyEnv.update env ax
    | List env -> ListEnv.update env ax
    | Map env -> MapEnv.update env ax
    | Itv env -> ItvEnv.update env ax

  let alias env v ax =
    match env.pack with
    | Dummy env -> DummyEnv.alias env v ax
    | List env -> ListEnv.alias env v ax
    | Map env -> MapEnv.alias env v ax
    | Itv env -> ItvEnv.alias env v ax

  let unalias env v =
    match env.pack with
    | Dummy env -> DummyEnv.unalias env v
    | List env -> ListEnv.unalias env v
    | Map env -> MapEnv.unalias env v
    | Itv env -> ItvEnv.unalias env v

  let assume env bl =
    match env.pack with
    | Dummy env -> DummyEnv.assume env bl
    | List env -> ListEnv.assume env bl
    | Map env -> MapEnv.assume env bl
    | Itv env -> ItvEnv.assume env bl

  let is_bl_cst env bl =
    if not env.rebase then None
    else
      match is_bl_cst bl with
      | Some _ as opt -> opt
      | None ->
        match is_bl_var bl with
        | Some v -> is_bl_cst (BlVarHashtbl.find env.bl_htbl v)
        | None -> None

  let is_bl_var env bl =
    if not env.rebase then None
    else
      match is_bl_var bl with
      | None -> None
      | Some v -> is_bl_var (BlVarHashtbl.find env.bl_htbl v)

  let is_linear env bv =
    if not env.rebase then None
    else
      let bv =
        match is_bv_var bv with
        | Some v ->
          (try BvVarHashtbl.find env.bv_htbl v
           with Not_found -> bv)
        | None -> bv
      in
      match bv.bv_term_desc with
      | BvCst _ -> Some bv
      | BvBnop (BvAdd, bv1, bv2) | BvBnop (BvSub, bv1, bv2) ->
        if (is_bv_cst bv1 <> None && is_bv_var bv2 <> None) ||
           (is_bv_var bv1 <> None && is_bv_cst bv2 <> None)
        then Some bv else None
      | _ -> None

  let is_bv_var env bv =
    if not env.rebase then None
    else
      match is_bv_var bv with
      | None -> None
      | Some v -> is_bv_var (BvVarHashtbl.find env.bv_htbl v)

  let add_bl_term env v bl =
    if env.rebase then
      BlVarHashtbl.add env.bl_htbl v
        (try match is_bl_var env bl with
           | None -> bl
           | Some v -> BlVarHashtbl.find env.bl_htbl v
         with Not_found -> bl)

  let add_bv_term env v bv =
    if env.rebase then
      match is_linear env bv with
      | Some bv -> BvVarHashtbl.add env.bv_htbl v bv
      | None -> BvVarHashtbl.add env.bv_htbl v (mk_bv_var v)

  let remove_bl_term env v =
    if env.rebase then BlVarHashtbl.remove env.bl_htbl v

  let remove_bv_term env v =
    if env.rebase then BvVarHashtbl.remove env.bv_htbl v

  let find_linear env v =
    if env.rebase then BvVarHashtbl.find env.bv_htbl v
    else mk_bv_var v

  let add_def env df =
    match df.def_desc with
    | BlDef (v,_,bl) -> add_bl_term env v bl
    | BvDef (v,_,bv) -> add_bv_term env v bv
    | AxDef (v,_,ax) -> alias env v ax

  let add_defs env dfs = List.iter (add_def env) dfs

  let remove_def env df =
    match df.def_desc with
    | BlDef (v,_,_) -> remove_bl_term env v
    | BvDef (v,_,_) -> remove_bv_term env v
    | AxDef (v,_,_) -> unalias env v

  let remove_defs env dfs = List.iter (remove_def env) dfs

  let add_decl env dc =
    match dc.decl_desc with
    | BlDecl (v,_) -> add_bl_term env v (mk_bl_var v)
    | BvDecl (v,_) -> add_bv_term env v (mk_bv_var v)
    | AxDecl (v,_) -> alias env v (mk_ax_var v)

  let add_decls env dcs = List.iter (add_decl env) dcs

  let remove_decl env dc =
    match dc.decl_desc with
    | BlDecl (v,_) -> remove_bl_term env v
    | BvDecl (v,_) -> remove_bv_term env v
    | AxDecl (v,_) -> unalias env v

  let remove_decls env dcs = List.iter (remove_decl env) dcs

  let rec add_assert env bl =
    match bl.bl_term_desc with
    | BlFun (v,[]) ->
      let bl' = BlVarHashtbl.find env.bl_htbl v in
      if not (equal_bl_term bl bl') then add_assert env bl'
    | BlComp (BlEqual,bl1,bl2) ->
      (match is_bl_var env bl1, is_bl_var env bl2 with
       | None, None | Some _, Some _ -> ()
       | Some v, None -> add_bl_term env v bl2
       | None, Some v -> add_bl_term env v bl1)
    | BvComp (BvEqual,bv1,bv2) ->
      (match is_bv_var env bv1, is_bv_var env bv2 with
       | None, None | Some _, Some _ -> ()
       | Some v, None -> add_bv_term env v bv2
       | None, Some v -> add_bv_term env v bv1)
    | _ -> ()


  let visit_list : 'env 'a 'b.
    ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list =
    fun f env ls -> List.map (f env) ls

  let rec visit_term env tm =
    visit_term_desc env tm.term_desc

  and visit_term_desc env = function
    | BlTerm bl -> mk_bl_term (visit_bl_term env bl)
    | BvTerm bv -> mk_bv_term (visit_bv_term env bv)
    | AxTerm ax -> mk_ax_term (visit_ax_term env ax)

  and visit_bl_term env bl =
    visit_bl_term_desc env bl.bl_term_desc

  and visit_bl_term_desc env = function
    | BlTrue -> mk_bl_true
    | BlFalse ->  mk_bl_false

    | BlFun (v,ls) ->
      let ls = visit_list visit_term env ls in
      let bl = mk_bl_fun v ls in
      (match is_bl_cst env bl with
       | Some bl -> if bl then mk_bl_true else mk_bl_false
       | None ->
         match is_bl_var env bl with
         | Some v -> mk_bl_fun v ls
         | None -> bl)

    | BlLet (bn,bl) ->
      let bn = visit_list visit_def env bn in
      add_defs env bn;
      let bl = visit_bl_term env bl in
      remove_defs env bn;
      mk_bl_let bn bl
    | BlUnop (u,bl) ->
      let bl = visit_bl_term env bl in
      mk_bl_unop u bl
    | BlBnop (b,bl1,bl2) ->
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_bnop b bl1 bl2
    | BlComp (c,bl1,bl2) ->
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_comp c bl1 bl2
    | BvComp (c,bv1,bv2) ->
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_comp c bv1 bv2
    | AxComp (c,ax1,ax2) ->
      let ax1 = visit_ax_term env ax1 in
      let ax2 = visit_ax_term env ax2 in
      mk_ax_comp c ax1 ax2
    | BlIte (bl,bl1,bl2) ->
      let bl = visit_bl_term env bl in
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_ite bl bl1 bl2

  and visit_bv_term env bv =
    visit_bv_term_desc env bv.bv_term_desc

  and visit_bv_term_desc env = function
    | BvCst bv -> mk_bv_cst bv
    | BvFun (v,ls) ->
      let ls = visit_list visit_term env ls in
      if ls = [] then find_linear env v
      else mk_bv_fun v ls
    | BvLet (bn,bv) ->
      let bn = visit_list visit_def env bn in
      add_defs env bn;
      let bv = visit_bv_term env bv in
      remove_defs env bn;
      mk_bv_let bn bv
    | BvUnop (u,bv) ->
      let bv = visit_bv_term env bv in
      mk_bv_unop u bv
    | BvBnop (b,bv1,bv2) ->
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_bnop b bv1 bv2
    | BvIte (bl,bv1,bv2) ->
      let bl = visit_bl_term env bl in
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_ite bl bv1 bv2
    | Select (n,ax,bv) ->
      let ax = visit_ax_term env ax in
      let bv = visit_bv_term env bv in
      lookup env n ax bv

  and visit_ax_term env ax =
    visit_ax_term_desc env ax.ax_term_desc

  and visit_ax_term_desc env = function
    | AxFun (v,ls) ->
      let ls = visit_list visit_term env ls in
      let res = mk_ax_fun v ls in
      update env res; res
    | AxLet (bn,ax) ->
      let bn = visit_list visit_def env bn in
      add_defs env bn;
      let ax = visit_ax_term env ax in
      remove_defs env bn;
      let res = mk_ax_let bn ax in
      update env res; res
    | AxIte (bl,ax1,ax2) ->
      let bl = visit_bl_term env bl in
      let ax1 = visit_ax_term env ax1 in
      let ax2 = visit_ax_term env ax2 in
      let res = mk_ax_ite bl ax1 ax2 in
      update env res; res
    | Store (n,ax,bv1,bv2) ->
      let ax = visit_ax_term env ax in
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      let res = mk_store n ax bv1 bv2 in
      update env res; res

  and visit_def env df =
    visit_def_desc env df.def_desc

  and visit_def_desc env = function
    | BlDef (v,ls,bl) ->
      add_decls env ls;
      let bl = visit_bl_term env bl in
      remove_decls env ls;
      mk_bl_def v ls bl
    | BvDef (v,ls,bv) ->
      add_decls env ls;
      let bv = visit_bv_term env bv in
      remove_decls env ls;
      mk_bv_def v ls bv
    | AxDef (v,ls,ax) ->
      add_decls env ls;
      let ax = visit_ax_term env ax in
      remove_decls env ls;
      mk_ax_def v ls ax

  and visit_entry env en =
    visit_entry_desc env en.entry_desc

  and visit_entry_desc env = function
    | Declare dc ->
      add_decl env dc;
      mk_declare dc
    | Define df ->
      let df = visit_def env df in
      add_def env df;
      mk_define df
    | Assert bl ->
      let bl = visit_bl_term env bl in
      assume env bl;
      add_assert env bl;
      mk_assert bl
    | Comment c -> mk_comment c
    | Echo s -> mk_echo s
    | Check_sat -> mk_check_sat

end

let read_over_write ?lst ?(rbs=true) ?(itv=true) ?(keep=VarSet.empty) fm =
  let env = ReadOverWrite.create ~lst ~rbs ~itv (length fm / 4) in
  fold_forward
    (fun entry fm ->
       let entry = ReadOverWrite.visit_entry env entry in
       match entry.entry_desc with
       | Declare _ | Comment _ | Echo _ | Check_sat -> push_front entry fm
       | Define df ->
         if
           (match df.def_desc with
            | BlDef (_,_,_) -> true
            | BvDef (v,_,bv) ->
              ReadOverWrite.is_linear env bv = None || VarSet.mem (BvVar v) keep
            | AxDef (_,_,_) -> true)
         then push_front entry fm
         else fm
       | Assert bl ->
         if ReadOverWrite.get_assertbl env bl then fm
         else (ReadOverWrite.set_assertbl env bl; push_front entry fm))
    fm empty


(* Static single assignment *)

module StaticSingleAssignment =
struct

  type env = {
    names : int Basic_types.String.Htbl.t;
    bl_htbl : bl_var BlTermHashtbl.t;
    bv_htbl : bv_var BvTermHashtbl.t;
    ax_htbl : ax_var AxTermHashtbl.t;
  }

  let create n = {
    names = Basic_types.String.Htbl.create n;
    bl_htbl = BlTermHashtbl.create n;
    bv_htbl = BvTermHashtbl.create n;
    ax_htbl = AxTermHashtbl.create n;
  }

  let do_nothing s x = s, x

  let k_identity f s x = f s x

  let rec fresh_name env name =
    let i =
      try Basic_types.String.Htbl.find env.names name
      with Not_found -> 0
    in
    Basic_types.String.Htbl.replace env.names name (i+1);
    let fresh = Printf.sprintf "%s_%i" name i in
    if Basic_types.String.Htbl.mem env.names fresh
    then fresh_name env name
    else (Basic_types.String.Htbl.add env.names fresh 0; fresh)

  let fresh_bl_var env name bl =
    let name =
      try (BlTermHashtbl.find env.bl_htbl bl).bl_name
      with Not_found -> fresh_name env name
    in
    bl_var name

  let add_bl_term env seq v bl =
    let seq =
      if BlTermHashtbl.mem env.bl_htbl bl then seq
      else
        (BlTermHashtbl.add env.bl_htbl bl v;
         push_front_define (mk_bl_def v [] bl) seq)
    in seq

  let push_front_bl_term bool (name,env) seq bl =
    if bool then seq, bl
    else
      match bl.bl_term_desc with
      | BlTrue | BlFalse | BlFun (_,_) -> seq, bl
      | BlLet (_,_) -> assert false
      | BlUnop (_,_) ->
        let v = fresh_bl_var env (name ^ "_bl_unop") bl in
        add_bl_term env seq v bl, mk_bl_var v
      | BlBnop (_,_,_) ->
        let v = fresh_bl_var env (name ^ "_bl_bnop") bl in
        add_bl_term env seq v bl, mk_bl_var v
      | BlComp (_,_,_) ->
        let v = fresh_bl_var env (name ^ "_bl_comp") bl in
        add_bl_term env seq v bl, mk_bl_var v
      | BvComp (_,_,_) ->
        let v = fresh_bl_var env (name ^ "_bv_comp") bl in
        add_bl_term env seq v bl, mk_bl_var v
      | AxComp (_,_,_) ->
        let v = fresh_bl_var env (name ^ "_ax_comp") bl in
        add_bl_term env seq v bl, mk_bl_var v
      | BlIte (_,_,_) ->
        let v = fresh_bl_var env (name ^ "_bl_ite") bl in
        add_bl_term env seq v bl, mk_bl_var v

  let fresh_bv_var env name bv =
    let name =
      try (BvTermHashtbl.find env.bv_htbl bv).bv_name
      with Not_found -> fresh_name env name
    in
    bv_var name bv.bv_term_size

  let add_bv_term env seq v bv =
    let seq =
      if BvTermHashtbl.mem env.bv_htbl bv then seq
      else
        (BvTermHashtbl.add env.bv_htbl bv v;
         push_front_define (mk_bv_def v [] bv) seq)
    in seq

  let push_front_bv_term bool (name,env) seq bv =
    if bool then seq, bv
    else
      match bv.bv_term_desc with
      | BvCst _ | BvFun (_,_) -> seq, bv
      | BvLet (_,_) -> assert false
      | BvUnop (_,_) ->
        let v = fresh_bv_var env (name ^ "_bv_unop") bv in
        add_bv_term env seq v bv, mk_bv_var v
      | BvBnop (_,_,_) ->
        let v = fresh_bv_var env (name ^ "_bv_bnop") bv in
        add_bv_term env seq v bv, mk_bv_var v
      | BvIte (_,_,_) ->
        let v = fresh_bv_var env (name ^ "_bv_ite") bv in
        add_bv_term env seq v bv, mk_bv_var v
      | Select (_,_,_) ->
        let v = fresh_bv_var env (name ^ "_select") bv in
        add_bv_term env seq v bv, mk_bv_var v

  let fresh_ax_var env name ax =
    let name =
      try (AxTermHashtbl.find env.ax_htbl ax).ax_name
      with Not_found -> fresh_name env name
    in
    ax_var name ax.idx_term_size ax.elt_term_size

  let add_ax_term env seq v ax =
    let seq =
      if AxTermHashtbl.mem env.ax_htbl ax then seq
      else
        (AxTermHashtbl.add env.ax_htbl ax v;
         push_front_define (mk_ax_def v [] ax) seq)
    in seq

  let push_front_ax_term bool (name,env) seq ax =
    if bool then seq, ax
    else
      match ax.ax_term_desc with
      | AxFun (_,_) -> seq, ax
      | AxLet (_,_) -> assert false
      | AxIte (_,_,_) ->
        let v = fresh_ax_var env (name ^ "_ax_ite") ax in
        add_ax_term env seq v ax, mk_ax_var v
      | Store (_,_,_,_) ->
        let v = fresh_ax_var env (name ^ "_store") ax in
        add_ax_term env seq v ax, mk_ax_var v

  (* There is certainly a bug here, mk_.._fun should takes a term list we do not
   * have here... *)
  let push_front_define env df seq =
    match df.def_desc with
    | BlDef (v,ls,bl) ->
      let bl =
        try mk_bl_fun (BlTermHashtbl.find env.bl_htbl bl) []
        with Not_found -> bl
      in
      BlTermHashtbl.add env.bl_htbl bl v;
      push_front_define (mk_bl_def v ls bl) seq
    | BvDef (v,ls,bv) ->
      let bv =
        try mk_bv_fun (BvTermHashtbl.find env.bv_htbl bv) []
        with Not_found -> bv
      in
      BvTermHashtbl.add env.bv_htbl bv v;
      push_front_define (mk_bv_def v ls bv) seq
    | AxDef (v,ls,ax) ->
      let ax =
        try mk_ax_fun (AxTermHashtbl.find env.ax_htbl ax) []
        with Not_found -> ax
      in
      AxTermHashtbl.add env.ax_htbl ax v;
      push_front_define (mk_ax_def v ls ax) seq

  let reserve_entry env em =
    match em.entry_desc with
    | Assert _ | Comment _ | Echo _ | Check_sat -> ()
    | Declare dc ->
      Basic_types.String.Htbl.add env.names (decl_name dc) 0
    | Define df ->
      Basic_types.String.Htbl.add env.names (def_name df) 0

  let visit_list : 'env 'a 'b.
    ('env -> 'a -> ('env * 'b)) -> 'env -> 'a list -> 'env * 'b list =
    fun f env ls ->
      let env,acc =
        List.fold_left
          (fun (env,acc) x -> let env,x = f env x in env, x::acc)
          (env,[]) ls
      in
      env, List.rev acc

  let rec visit_term env seq tm =
    visit_term_desc env seq tm.term_desc

  and visit_term_desc env seq = function
    | BlTerm bl ->
      let seq,bl = visit_bl_term k_identity false env seq bl in
      seq, mk_bl_term bl
    | BvTerm bv ->
      let seq,bv = visit_bv_term k_identity false env seq bv in
      seq, mk_bv_term bv
    | AxTerm ax ->
      let seq,ax = visit_ax_term k_identity false env seq ax in
      seq, mk_ax_term ax

  and visit_bl_term k bool env seq bl =
    visit_bl_term_desc k bool env seq bl.bl_term_desc

  and visit_bl_term_desc k bool (_,e as env) seq = function
    | BlTrue -> k do_nothing seq mk_bl_true
    | BlFalse -> k do_nothing seq mk_bl_false

    | BlFun (v,ls) ->
      let seq,ls = visit_list (visit_term (v.bl_name,e)) seq ls in
      k do_nothing seq (mk_bl_fun v ls)

    | BlLet (bn,bl) ->
      let seq,bn = visit_list (visit_def e) seq bn in
      let seq = List.fold_left (fun seq df -> push_front_define e df seq) seq bn in
      visit_bl_term k false env seq bl

    | BlUnop (u,bl) ->
      let seq,bl = visit_bl_term k_identity false env seq bl in
      k (push_front_bl_term bool env) seq (mk_bl_unop u bl)

    | BlBnop (b,bl1,bl2) ->
      let seq,bl1 = visit_bl_term k_identity false env seq bl1 in
      let seq,bl2 = visit_bl_term k_identity false env seq bl2 in
      k (push_front_bl_term bool env) seq (mk_bl_bnop b bl1 bl2)

    | BlComp (c,bl1,bl2) ->
      let seq,bl1 = visit_bl_term k_identity false env seq bl1 in
      let seq,bl2 = visit_bl_term k_identity false env seq bl2 in
      k (push_front_bl_term bool env) seq (mk_bl_comp c bl1 bl2)

    | BvComp (c,bv1,bv2) ->
      let seq,bv1 = visit_bv_term k_identity false env seq bv1 in
      let seq,bv2 = visit_bv_term k_identity false env seq bv2 in
      k (push_front_bl_term bool env) seq (mk_bv_comp c bv1 bv2)

    | AxComp (c,ax1,ax2) ->
      let seq,ax1 = visit_ax_term k_identity false env seq ax1 in
      let seq,ax2 = visit_ax_term k_identity false env seq ax2 in
      k (push_front_bl_term bool env) seq (mk_ax_comp c ax1 ax2)

    | BlIte (bl,bl1,bl2) ->
      let seq,bl = visit_bl_term k_identity false env seq bl in
      let seq,bl1 = visit_bl_term k_identity false env seq bl1 in
      let seq,bl2 = visit_bl_term k_identity false env seq bl2 in
      k (push_front_bl_term bool env) seq (mk_bl_ite bl bl1 bl2)

  and visit_bv_term k bool env seq bv =
    visit_bv_term_desc k bool env seq bv.bv_term_desc

  and visit_bv_term_desc k bool (_,e as env) seq = function
    | BvCst bv -> k do_nothing seq (mk_bv_cst bv)

    | BvFun (v,ls) ->
      let seq,ls = visit_list (visit_term (v.bv_name,e)) seq ls in
      k do_nothing seq (mk_bv_fun v ls)

    | BvLet (bn,bv) ->
      let seq,bn = visit_list (visit_def e) seq bn in
      let seq = List.fold_left (fun seq df -> push_front_define e df seq) seq bn in
      visit_bv_term k false env seq bv

    | BvUnop (u,bv) ->
      let seq,bv = visit_bv_term k_identity false env seq bv in
      k (push_front_bv_term bool env) seq (mk_bv_unop u bv)

    | BvBnop (b,bv1,bv2) ->
      let seq,bv1 = visit_bv_term k_identity false env seq bv1 in
      let seq,bv2 = visit_bv_term k_identity false env seq bv2 in
      k (push_front_bv_term bool env) seq (mk_bv_bnop b bv1 bv2)

    | BvIte (bl,bv1,bv2) ->
      let seq,bl = visit_bl_term k_identity false env seq bl in
      let seq,bv1 = visit_bv_term k_identity false env seq bv1 in
      let seq,bv2 = visit_bv_term k_identity false env seq bv2 in
      k (push_front_bv_term bool env) seq (mk_bv_ite bl bv1 bv2)

    | Select (n,ax,bv) ->
      let seq,ax = visit_ax_term k_identity false env seq ax in
      let seq,bv = visit_bv_term k_identity false env seq bv in
      k (push_front_bv_term bool env) seq (mk_select n ax bv)

  and visit_ax_term k bool env seq ax =
    visit_ax_term_desc k bool env seq ax.ax_term_desc

  and visit_ax_term_desc k bool (_,e as env) seq = function
    | AxFun (v,ls) ->
      let seq,ls = visit_list (visit_term (v.ax_name,e)) seq ls in
      k do_nothing seq (mk_ax_fun v ls)

    | AxLet (bn,ax) ->
      let seq,bn = visit_list (visit_def e) seq bn in
      let seq = List.fold_left (fun seq df -> push_front_define e df seq) seq bn in
      visit_ax_term k false env seq ax

    | AxIte (bl,ax1,ax2) ->
      let seq,bl = visit_bl_term k_identity false env seq bl in
      let seq,ax1 = visit_ax_term k_identity false env seq ax1 in
      let seq,ax2 = visit_ax_term k_identity false env seq ax2 in
      k (push_front_ax_term bool env) seq (mk_ax_ite bl ax1 ax2)

    | Store (n,ax,bv1,bv2) ->
      let seq,ax = visit_ax_term k_identity false env seq ax in
      let seq,bv1 = visit_bv_term k_identity false env seq bv1 in
      let seq,bv2 = visit_bv_term k_identity false env seq bv2 in
      k (push_front_ax_term bool env) seq (mk_store n ax bv1 bv2)

  and visit_def env seq df =
    visit_def_desc env seq df.def_desc

  and visit_def_desc env seq = function
    | BlDef (v,ls,bl) ->
      if ls = [] then
        let seq,bl = visit_bl_term k_identity true (v.bl_name,env) seq bl in
        seq, mk_bl_def v ls bl
      else seq, mk_bl_def v ls bl
    | BvDef (v,ls,bv) ->
      if ls = [] then
        let seq,bv = visit_bv_term k_identity true (v.bv_name,env) seq bv in
        seq, mk_bv_def v ls bv
      else seq, mk_bv_def v ls bv
    | AxDef (v,ls,ax) ->
      if ls = [] then
        let seq,ax = visit_ax_term k_identity true (v.ax_name,env) seq ax in
        seq, mk_ax_def v ls ax
      else seq, mk_ax_def v ls ax

  and visit_entry env seq en =
    visit_entry_desc env seq en.entry_desc

  and visit_entry_desc env seq = function
    | Declare dc -> push_front_declare dc seq
    | Define df ->
      let seq,df = visit_def env seq df in
      push_front_define env df seq
    | Assert bl ->
      let seq,bl = visit_bl_term k_identity false ("assert",env) seq bl in
      push_front_assert bl seq
    | Comment c -> push_front_comment c seq
    | Echo s -> push_front_echo s seq
    | Check_sat -> push_front_check_sat seq
end

let static_single_assignment fm =
  let env = StaticSingleAssignment.create (length fm / 4) in
  iter_forward (StaticSingleAssignment.reserve_entry env) fm;
  fold_forward
    (fun en seq -> StaticSingleAssignment.visit_entry env seq en)
    fm empty

let optimize ?(keep=VarSet.empty)
    ?lst ?(cst=true) ?(itv=false) ?(prn=true) ?(rbs=true) ?(row=true) ?(ssa=true) fm =
  let apply_if cond f x = if cond then f x else x in
  fm
  |> apply_if prn (prune_and_inline ~keep)
  |> apply_if cst (constant_propagation ~keep)
  |> apply_if ssa static_single_assignment
  |> apply_if row (read_over_write ?lst ~rbs ~itv ~keep)
  |> apply_if (ssa && row) static_single_assignment
  |> apply_if (prn && row) (prune_and_inline ~keep)
  |> apply_if (cst && (row || ssa)) (constant_propagation ~keep)
  |> apply_if (prn && (cst || row || ssa)) (prune_and_inline ~keep)


let optimize_from_options ?(keep=VarSet.empty) fm =
  let open Formula_options in
  let cst = OptimAll.get () || OptimCst.get () in
  let itv = OptimAll.get () || OptimItv.get () in
  let prn = OptimAll.get () || OptimPrn.get () in
  let rbs = OptimAll.get () || OptimRbs.get () in
  let row = OptimAll.get () || OptimRow.get () in
  let ssa = OptimAll.get () || OptimSsa.get () in
  let lst =
    let i = OptimLst.get () in
    if i = 0 then None else Some i
  in optimize ~keep ?lst ~cst ~itv ~prn ~rbs ~row ~ssa fm
