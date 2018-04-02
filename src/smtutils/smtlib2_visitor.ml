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

open Basic_types
open Smtlib2

(* -------------------------------------------------- *)
(* ---------------- Generic Visitors ---------------- *)

class type ['expr,'bv_expr,'abv_expr] smt_visitor =
  object
    method smt_bv_expr  : 'bv_expr -> 'expr
    method smt_abv_expr : 'abv_expr -> 'expr
    method smt_and      : 'expr -> 'expr -> 'expr
    method smt_or       : 'expr -> 'expr -> 'expr
    method smt_comp     : 'expr -> 'expr -> 'expr
    method smt_not      : 'expr -> 'expr
    method smt_let      : ('expr * 'expr) list -> 'expr -> 'expr
    method smt_ite      : 'expr -> 'expr -> 'expr -> 'expr
    method smt_true     : 'expr
    method smt_false    : 'expr
    method smt_token    : 'expr
    method smt_comment  : string -> 'expr

    method smt_bv_cst     : Bitvector.t -> 'bv_expr
    method smt_bv_var     : smt_bv_var -> 'bv_expr
    method smt_bv_unary   : smt_bv_unary -> 'bv_expr -> 'bv_expr
    method smt_bv_binary  : smt_bv_binary -> 'bv_expr -> 'bv_expr -> 'bv_expr
    method smt_bv_ite     : 'expr -> 'bv_expr -> 'bv_expr -> 'bv_expr
    method smt_abv_select : 'abv_expr -> 'bv_expr -> 'bv_expr
    method smt_bv_let     : ('expr * 'expr) list -> 'bv_expr -> 'bv_expr
    method smt_abv_load32 : 'abv_expr -> 'bv_expr -> 'bv_expr
    method smt_bv_token   : 'bv_expr

    method smt_abv_array   : smt_abv_array -> 'abv_expr
    method smt_abv_store   : 'abv_expr -> 'bv_expr -> 'bv_expr -> 'abv_expr
    method smt_abv_let     : ('expr * 'expr) list -> 'abv_expr -> 'abv_expr
    method smt_abv_store32 : 'abv_expr -> 'bv_expr -> 'bv_expr -> 'abv_expr

    method pre_smt_expr     : smt_expr     -> smt_expr
    method pre_smt_bv_expr  : smt_bv_expr  -> smt_bv_expr
    method pre_smt_abv_expr : smt_abv_expr -> smt_abv_expr

    method post_smt_expr     : 'expr     -> 'expr
    method post_smt_bv_expr  : 'bv_expr  -> 'bv_expr
    method post_smt_abv_expr : 'abv_expr -> 'abv_expr

    method visit_smt_expr     : smt_expr     -> 'expr
    method visit_smt_bv_expr  : smt_bv_expr  -> 'bv_expr
    method visit_smt_abv_expr : smt_abv_expr -> 'abv_expr
  end


class virtual ['expr,'bv_expr,'abv_expr] smt_expr_visitor =
  object(self)
    method visit_smt_expr (e: smt_expr) =
      self#post_smt_expr
        (match self#pre_smt_expr e with
         | SmtBvExpr e_bv ->
           let e_bv = self#bv_expr e_bv in
           self#smt_bv_expr e_bv
         | SmtABvArrayExpr e_abv ->
           let e_abv = self#abv_expr e_abv in
           self#smt_abv_expr e_abv
         | SmtAnd (e1,e2) ->
           let e1 = self#expr e1 in
           let e2 = self#expr e2 in
           self#smt_and e1 e2
         | SmtOr (e1,e2) ->
           let e1 = self#expr e1 in
           let e2 = self#expr e2 in
           self#smt_or e1 e2
         | SmtComp (e1,e2) ->
           let e1 = self#expr e1 in
           let e2 = self#expr e2 in
           self#smt_comp e1 e2
         | SmtNot e -> self#smt_not (self#expr e)
         | SmtLet (lst,e) ->
           let lst =
             (List.map (fun (e1,e2) ->
                  let e1 = self#expr e1 in
                  let e2 = self#expr e2 in
                  e1,e2)
                 lst) in
           let e = self#expr e in
           self#smt_let lst e
         | SmtIte (e,e1,e2) ->
           let e  = self#expr e in
           let e1 = self#expr e1 in
           let e2 = self#expr e2 in
           self#smt_ite e e1 e2
         | SmtTrue  -> self#smt_true
         | SmtFalse -> self#smt_false
         | SmtToken -> self#smt_token
         | SmtComment s -> self#smt_comment s)

    method pre_smt_expr  e = e
    method post_smt_expr e = e

    method private virtual expr     : 'smt_expr     -> 'expr
    method private virtual bv_expr  : 'smt_bv_expr  -> 'bv_expr
    method private virtual abv_expr : 'smt_abv_expr -> 'abv_expr

    method virtual smt_bv_expr  : 'bv_expr -> 'expr
    method virtual smt_abv_expr : 'abv_expr -> 'expr
    method virtual smt_and      : 'expr -> 'expr -> 'expr
    method virtual smt_or       : 'expr -> 'expr -> 'expr
    method virtual smt_comp     : 'expr -> 'expr -> 'expr
    method virtual smt_not      : 'expr -> 'expr
    method virtual smt_let      : ('expr * 'expr) list -> 'expr -> 'expr
    method virtual smt_ite      : 'expr -> 'expr -> 'expr -> 'expr
    method virtual smt_true     : 'expr
    method virtual smt_false    : 'expr
    method virtual smt_token    : 'expr
    method virtual smt_comment  : string -> 'expr
  end

class virtual ['expr,'bv_expr,'abv_expr] smt_bv_expr_visitor =
  object(self)
    method visit_smt_bv_expr (e: smt_bv_expr) =
      self#post_smt_bv_expr
        (match self#pre_smt_bv_expr e with
         | SmtBvCst bv  -> self#smt_bv_cst bv
         | SmtBvVar var -> self#smt_bv_var var
         | SmtBvUnary (u,e_bv) -> self#smt_bv_unary u (self#bv_expr e_bv)
         | SmtBvBinary (b,e_bv1,e_bv2) ->
           let e_bv1 = self#bv_expr e_bv1 in
           let e_bv2 = self#bv_expr e_bv2 in
           self#smt_bv_binary b e_bv1 e_bv2
         | SmtBvIte (e,e_bv1,e_bv2) ->
           let e     = self#expr e in
           let e_bv1 = self#bv_expr e_bv1 in
           let e_bv2 = self#bv_expr e_bv2 in
           self#smt_bv_ite e e_bv1 e_bv2
         | SmtABvSelect (e_abv,e_bv) ->
           let e_abv = self#abv_expr e_abv in
           let e_bv  = self#bv_expr e_bv in
           self#smt_abv_select e_abv e_bv
         | SmtBvLet (lst,e_bv) ->
           let lst =
             (List.map (fun (e1,e2) ->
                  let e1 = self#expr e1 in
                  let e2 = self#expr e2 in
                  e1,e2)
                 lst) in
           let e_bv = self#bv_expr e_bv in
           self#smt_bv_let lst e_bv
         | SmtABvLoad32 (e_abv,e_bv) ->
           let e_abv = self#abv_expr e_abv in
           let e_bv  = self#bv_expr e_bv in
           self#smt_abv_load32 e_abv e_bv
         | SmtBvToken -> self#smt_bv_token)

    method pre_smt_bv_expr  e = e
    method post_smt_bv_expr e = e

    method private virtual expr     : 'smt_expr     -> 'expr
    method private virtual bv_expr  : 'smt_bv_expr  -> 'bv_expr
    method private virtual abv_expr : 'smt_abv_expr -> 'abv_expr

    method virtual smt_bv_cst     : Bitvector.t -> 'bv_expr
    method virtual smt_bv_var     : smt_bv_var -> 'bv_expr
    method virtual smt_bv_unary   : smt_bv_unary -> 'bv_expr -> 'bv_expr
    method virtual smt_bv_binary  : smt_bv_binary -> 'bv_expr -> 'bv_expr -> 'bv_expr
    method virtual smt_bv_ite     : 'expr -> 'bv_expr -> 'bv_expr -> 'bv_expr
    method virtual smt_abv_select : 'abv_expr -> 'bv_expr -> 'bv_expr
    method virtual smt_bv_let     : ('expr * 'expr) list -> 'bv_expr -> 'bv_expr
    method virtual smt_abv_load32 : 'abv_expr -> 'bv_expr -> 'bv_expr
    method virtual smt_bv_token   : 'bv_expr
  end

class virtual ['expr,'bv_expr,'abv_expr] smt_abv_expr_visitor =
  object(self)
    method visit_smt_abv_expr (e: smt_abv_expr) =
      self#post_smt_abv_expr
        (match self#pre_smt_abv_expr e with
         | SmtABvArray abv -> self#smt_abv_array abv
         | SmtABvStore (e_abv,e_bv1,e_bv2) ->
           let e_abv = self#abv_expr e_abv in
           let e_bv1 = self#bv_expr e_bv1 in
           let e_bv2 = self#bv_expr e_bv2 in
           self#smt_abv_store e_abv e_bv1 e_bv2
         | SmtABvLet (lst,e_abv) ->
           let lst =
             (List.map (fun (e1,e2) ->
                  let e1 = self#expr e1 in
                  let e2 =  self#expr e2 in
                  e1,e2)
                 lst) in
           let e_abv = self#abv_expr e_abv in
           self#smt_abv_let lst e_abv
         | SmtABvStore32 (e_abv,e_bv1,e_bv2) ->
           let e_abv = self#abv_expr e_abv in
           let e_bv1 = self#bv_expr e_bv1 in
           let e_bv2 = self#bv_expr e_bv2 in
           self#smt_abv_store32 e_abv e_bv1 e_bv2)

    method pre_smt_abv_expr  e = e
    method post_smt_abv_expr e = e

    method private virtual expr     : 'smt_expr     -> 'expr
    method private virtual bv_expr  : 'smt_bv_expr  -> 'bv_expr
    method private virtual abv_expr : 'smt_abv_expr -> 'abv_expr

    method virtual smt_abv_array   : smt_abv_array -> 'abv_expr
    method virtual smt_abv_store   : 'abv_expr -> 'bv_expr -> 'bv_expr -> 'abv_expr
    method virtual smt_abv_let     : ('expr * 'expr) list -> 'abv_expr -> 'abv_expr
    method virtual smt_abv_store32 : 'abv_expr -> 'bv_expr -> 'bv_expr -> 'abv_expr
  end

class virtual ['expr,'bv_expr,'abv_expr] smt_visitor_virtual =
  object(self)
    inherit ['expr,'bv_expr,'abv_expr] smt_expr_visitor
    inherit ['expr,'bv_expr,'abv_expr] smt_bv_expr_visitor
    inherit ['expr,'bv_expr,'abv_expr] smt_abv_expr_visitor

    method private expr     = self#visit_smt_expr
    method private bv_expr  = self#visit_smt_bv_expr
    method private abv_expr = self#visit_smt_abv_expr
  end


class smt_iter_visitor : [unit,unit,unit] smt_visitor =
  object
    inherit [unit,unit,unit] smt_visitor_virtual

    method smt_bv_expr _  = ()
    method smt_abv_expr _ = ()
    method smt_and _ _    = ()
    method smt_or _ _     = ()
    method smt_comp _ _   = ()
    method smt_not _      = ()
    method smt_let _ _    = ()
    method smt_ite _ _ _  = ()
    method smt_true       = ()
    method smt_false      = ()
    method smt_token      = ()
    method smt_comment _  = ()

    method smt_bv_cst _        = ()
    method smt_bv_var _        = ()
    method smt_bv_unary _ _    = ()
    method smt_bv_binary _ _ _ = ()
    method smt_bv_ite _ _ _    = ()
    method smt_abv_select _ _  = ()
    method smt_bv_let _ _      = ()
    method smt_abv_load32 _ _  = ()
    method smt_bv_token        = ()

    method smt_abv_array _       = ()
    method smt_abv_store _ _ _   = ()
    method smt_abv_let _ _       = ()
    method smt_abv_store32 _ _ _ = ()
  end


class smt_map_visitor : [smt_expr,smt_bv_expr,smt_abv_expr] smt_visitor =
  object
    inherit [smt_expr,smt_bv_expr,smt_abv_expr] smt_visitor_virtual

    method smt_bv_expr bv   = SmtBvExpr bv
    method smt_abv_expr abv = SmtABvArrayExpr abv
    method smt_and e1 e2    = SmtAnd (e1,e2)
    method smt_or  e1 e2    = SmtOr  (e1,e2)
    method smt_comp e1 e2   = SmtComp (e1,e2)
    method smt_not e        = SmtNot e
    method smt_let lst e    = SmtLet (lst,e)
    method smt_ite e e1 e2  = SmtIte (e,e1,e2)
    method smt_true         = SmtTrue
    method smt_false        = SmtFalse
    method smt_token        = SmtToken
    method smt_comment s    = SmtComment s

    method smt_bv_cst bv         = SmtBvCst bv
    method smt_bv_var var        = SmtBvVar var
    method smt_bv_unary u e      = SmtBvUnary (u,e)
    method smt_bv_binary b e1 e2 = SmtBvBinary (b,e1,e2)
    method smt_bv_ite e bv1 bv2  = SmtBvIte (e,bv1,bv2)
    method smt_abv_select abv bv = SmtABvSelect (abv,bv)
    method smt_bv_let lst e      = SmtBvLet (lst,e)
    method smt_abv_load32 abv bv = SmtABvLoad32 (abv,bv)
    method smt_bv_token          = SmtBvToken

    method smt_abv_array abv           = SmtABvArray abv
    method smt_abv_store abv bv1 bv2   = SmtABvStore (abv,bv1,bv2)
    method smt_abv_let lst e           = SmtABvLet (lst,e)
    method smt_abv_store32 abv bv1 bv2 = SmtABvStore32 (abv,bv1,bv2)
  end


class ['a] smt_fold_visitor : ['a->'a,'a->'a,'a->'a] smt_visitor =
  object
    inherit ['a->'a,'a->'a,'a->'a] smt_visitor_virtual

    method smt_bv_expr bv   = fun acc -> bv acc
    method smt_abv_expr abv = fun acc -> abv acc
    method smt_and e1 e2    = fun acc -> e2 (e1 acc)
    method smt_or  e1 e2    = fun acc -> e2 (e1 acc)
    method smt_comp e1 e2   = fun acc -> e2 (e1 acc)
    method smt_not e        = fun acc -> e acc
    method smt_let lst e    = fun acc ->
      List.fold_left (fun acc (e1,e2) -> e2 (e1 acc)) (e acc) lst
    method smt_ite e e1 e2  = fun acc -> e2 (e1 (e acc))
    method smt_true         = fun acc -> acc
    method smt_false        = fun acc -> acc
    method smt_token        = fun acc -> acc
    method smt_comment _    = fun acc -> acc

    method smt_bv_cst _          = fun acc -> acc
    method smt_bv_var _          = fun acc -> acc
    method smt_bv_unary _ e      = fun acc -> e acc
    method smt_bv_binary _ e1 e2 = fun acc -> e2 (e1 acc)
    method smt_bv_ite e bv1 bv2  = fun acc -> bv2 (bv1 (e acc))
    method smt_abv_select abv bv = fun acc -> bv (abv acc)
    method smt_bv_let lst e      = fun acc ->
      List.fold_left (fun acc (e1,e2) -> e2 (e1 acc)) (e acc) lst
    method smt_abv_load32 abv bv = fun acc -> bv (abv acc)
    method smt_bv_token          = fun acc -> acc

    method smt_abv_array _             = fun acc -> acc
    method smt_abv_store abv bv1 bv2   = fun acc -> bv2 (bv1 (abv acc))
    method smt_abv_let lst e           = fun acc ->
      List.fold_left (fun acc (e1,e2) -> e2 (e1 acc)) (e acc) lst
    method smt_abv_store32 abv bv1 bv2 = fun acc -> bv2 (bv1 (abv acc))
  end


class smt_exists_visitor : [bool,bool,bool] smt_visitor =
  object
    inherit [bool,bool,bool] smt_visitor_virtual

    method smt_bv_expr bv   = bv
    method smt_abv_expr abv = abv
    method smt_and e1 e2    = e1 || e2
    method smt_or  e1 e2    = e1 || e2
    method smt_comp e1 e2   = e1 || e2
    method smt_not e        = e
    method smt_let lst e    = List.fold_left (fun acc (e1,e2) -> acc || e1 || e2) e lst
    method smt_ite e e1 e2  = e || e1 || e2
    method smt_true         = false
    method smt_false        = false
    method smt_token        = false
    method smt_comment _    = false

    method smt_bv_cst _          = false
    method smt_bv_var _          = false
    method smt_bv_unary _ e      = e
    method smt_bv_binary _ e1 e2 = e1 || e2
    method smt_bv_ite e bv1 bv2  = e || bv1 || bv2
    method smt_abv_select abv bv = abv || bv
    method smt_bv_let lst e      = List.fold_left (fun acc (e1,e2) -> acc || e1 || e2) e lst
    method smt_abv_load32 abv bv = abv || bv
    method smt_bv_token          = false

    method smt_abv_array _             = false
    method smt_abv_store abv bv1 bv2   = abv || bv1 || bv2
    method smt_abv_let lst e           = List.fold_left (fun acc (e1,e2) -> acc || e1 || e2) e lst
    method smt_abv_store32 abv bv1 bv2 = abv || bv1 || bv2
  end


class smt_forall_visitor : [bool,bool,bool] smt_visitor =
  object
    inherit [bool,bool,bool] smt_visitor_virtual

    method smt_bv_expr bv   = bv
    method smt_abv_expr abv = abv
    method smt_and e1 e2    = e1 && e2
    method smt_or  e1 e2    = e1 && e2
    method smt_comp e1 e2   = e1 && e2
    method smt_not e        = e
    method smt_let lst e    = List.fold_left (fun acc (e1,e2) -> acc && e1 && e2) e lst
    method smt_ite e e1 e2  = e && e1 && e2
    method smt_true         = true
    method smt_false        = true
    method smt_token        = true
    method smt_comment _    = true

    method smt_bv_cst _          = true
    method smt_bv_var _          = true
    method smt_bv_unary _ e      = e
    method smt_bv_binary _ e1 e2 = e1 && e2
    method smt_bv_ite e bv1 bv2  = e && bv1 && bv2
    method smt_abv_select abv bv = abv && bv
    method smt_bv_let lst e      = List.fold_left (fun acc (e1,e2) -> acc && e1 && e2) e lst
    method smt_abv_load32 abv bv = abv && bv
    method smt_bv_token          = true

    method smt_abv_array _             = true
    method smt_abv_store abv bv1 bv2   = abv && bv1 && bv2
    method smt_abv_let lst e           = List.fold_left (fun acc (e1,e2) -> acc && e1 && e2) e lst
    method smt_abv_store32 abv bv1 bv2 = abv && bv1 && bv2
  end


let with_visitor (v: ('a,'b,'c) smt_visitor) =
  fun
    ?(smt_bv_expr=v#smt_bv_expr)
    ?(smt_abv_expr=v#smt_abv_expr)
    ?(smt_and=v#smt_and)
    ?(smt_or=v#smt_or)
    ?(smt_comp=v#smt_comp)
    ?(smt_not=v#smt_not)
    ?(smt_let=v#smt_let)
    ?(smt_ite=v#smt_ite)
    ?(smt_true=v#smt_true)
    ?(smt_false=v#smt_false)
    ?(smt_token=v#smt_token)
    ?(smt_comment=v#smt_comment)

    ?(smt_bv_cst=v#smt_bv_cst)
    ?(smt_bv_var=v#smt_bv_var)
    ?(smt_bv_unary=v#smt_bv_unary)
    ?(smt_bv_binary=v#smt_bv_binary)
    ?(smt_bv_ite=v#smt_bv_ite)
    ?(smt_abv_select=v#smt_abv_select)
    ?(smt_bv_let=v#smt_bv_let)
    ?(smt_abv_load32=v#smt_abv_load32)
    ?(smt_bv_token=v#smt_bv_token)

    ?(smt_abv_array=v#smt_abv_array)
    ?(smt_abv_store=v#smt_abv_store)
    ?(smt_abv_let=v#smt_abv_let)
    ?(smt_abv_store32=v#smt_abv_store32)
    () ->
    object
      inherit ['a,'b,'c] smt_visitor_virtual

      method smt_bv_expr = smt_bv_expr
      method smt_abv_expr = smt_abv_expr
      method smt_and = smt_and
      method smt_or = smt_or
      method smt_comp = smt_comp
      method smt_not = smt_not
      method smt_let = smt_let
      method smt_ite = smt_ite
      method smt_true = smt_true
      method smt_false = smt_false
      method smt_token = smt_token
      method smt_comment = smt_comment

      method smt_bv_cst = smt_bv_cst
      method smt_bv_var= smt_bv_var
      method smt_bv_unary = smt_bv_unary
      method smt_bv_binary = smt_bv_binary
      method smt_bv_ite = smt_bv_ite
      method smt_abv_select = smt_abv_select
      method smt_bv_let = smt_bv_let
      method smt_abv_load32 = smt_abv_load32
      method smt_bv_token = smt_bv_token

      method smt_abv_array = smt_abv_array
      method smt_abv_store = smt_abv_store
      method smt_abv_let = smt_abv_let
      method smt_abv_store32 = smt_abv_store32
    end

let smt_bv_expr  smt_bv_expr v  = with_visitor v ~smt_bv_expr ()
let smt_abv_expr smt_abv_expr v = with_visitor v ~smt_abv_expr ()
let smt_and      smt_and v      = with_visitor v ~smt_and ()
let smt_or       smt_or v       = with_visitor v ~smt_or ()
let smt_comp     smt_comp v     = with_visitor v ~smt_comp ()
let smt_not      smt_not v      = with_visitor v ~smt_not ()
let smt_let      smt_let v      = with_visitor v ~smt_let ()
let smt_ite      smt_ite v      = with_visitor v ~smt_ite ()
let smt_true     smt_true v     = with_visitor v ~smt_true ()
let smt_false    smt_false v    = with_visitor v ~smt_false ()
let smt_token    smt_token v    = with_visitor v ~smt_token ()
let smt_comment  smt_comment v  = with_visitor v ~smt_comment ()

let smt_bv_cst     smt_bv_cst v     = with_visitor v ~smt_bv_cst ()
let smt_bv_var     smt_bv_var v     = with_visitor v ~smt_bv_var ()
let smt_bv_unary   smt_bv_unary v   = with_visitor v ~smt_bv_unary ()
let smt_bv_binary  smt_bv_binary v  = with_visitor v ~smt_bv_binary ()
let smt_bv_ite     smt_bv_ite v     = with_visitor v ~smt_bv_ite ()
let smt_abv_select smt_abv_select v = with_visitor v ~smt_abv_select ()
let smt_bv_let     smt_bv_let v     = with_visitor v ~smt_bv_let ()
let smt_abv_load32 smt_abv_load32 v = with_visitor v ~smt_abv_load32 ()
let smt_bv_token   smt_bv_token v   = with_visitor v ~smt_bv_token ()

let smt_abv_array   smt_abv_array v   = with_visitor v ~smt_abv_array ()
let smt_abv_store   smt_abv_store v   = with_visitor v ~smt_abv_store ()
let smt_abv_let     smt_abv_let v     = with_visitor v ~smt_abv_let ()
let smt_abv_store32 smt_abv_store32 v = with_visitor v ~smt_abv_store32 ()

(* --------------------------------------------------------- *)
(* --------------------------------------------------------- *)

class get_var_visitor =
  object inherit smt_iter_visitor
    val mutable vars = String.Set.empty

    method get_vars = vars

    method! smt_bv_var (name,_) =
      vars <- String.Set.add name vars

    method! smt_abv_array (name,_,_) =
      vars <- String.Set.add name vars

    method clear () =
      vars <- String.Set.empty
  end

let get_var_expr (e:smt_expr) =
  let visitor = new get_var_visitor in
  visitor#visit_smt_expr e;
  visitor#get_vars

let get_var_bvexpr (e:smt_bv_expr) =
  let visitor = new get_var_visitor in
  visitor#visit_smt_bv_expr e;
  visitor#get_vars

let get_var_abvexpr (e:smt_abv_expr) =
  let visitor = new get_var_visitor in
  visitor#visit_smt_abv_expr e;
  visitor#get_vars


class replace_expr_visitor (token:smt_expr) (sub:smt_expr) =
  object inherit smt_map_visitor

    method! post_smt_expr (e:smt_expr): smt_expr =
      if expr_equal e token then
        sub
      else
        e
  end

let replace_expr (f:smt_expr) (token:smt_expr) (f_sub:smt_expr): smt_expr =
  let visitor = new replace_expr_visitor token f_sub in
  visitor#visit_smt_expr f

let replace_token_expr (f:smt_expr) (f_sub:smt_expr) =
  replace_expr f SmtToken f_sub


class replace_bvexpr_visitor (token:smt_bv_expr) (sub:smt_bv_expr) =
  object inherit smt_map_visitor

    method! pre_smt_bv_expr (e:smt_bv_expr): smt_bv_expr =
      if bvexpr_equal e token then
        sub
      else
        e
  end

let replace_bvexpr (f:smt_bv_expr) (token:smt_bv_expr) (f_sub:smt_bv_expr): smt_bv_expr =
  let visitor = new replace_bvexpr_visitor token f_sub in
  visitor#visit_smt_bv_expr f



class statistics_visitor =
  object inherit smt_iter_visitor
    val mutable vars = 0
    val mutable csts = 0
    val mutable op_unary = 0
    val mutable op_binary = 0
    val mutable selects = 0
    val mutable stores = 0

    method get_stats =
      vars, csts, op_unary, op_binary, selects, stores

    method! smt_bv_var _ = vars <- vars+1
    method! smt_abv_array _ = vars <- vars+1
    method! smt_bv_cst _  = csts <- csts+1
    method! smt_bv_unary _ _ = op_unary <- op_unary+1
    method! smt_bv_binary _ _ _ = op_binary <- op_binary+1
    method! smt_abv_select _ _ = selects <- selects+1
    method! smt_abv_store _ _ _ = stores <- stores+1
  end

let stat_expr (f:smt_expr): (int * int * int * int * int * int) =
  let visitor = new statistics_visitor in
  visitor#visit_smt_expr f;
  visitor#get_stats

let stat_bvexpr (f:smt_bv_expr): (int * int * int * int * int * int) =
  let visitor = new statistics_visitor in
  visitor#visit_smt_bv_expr f;
  visitor#get_stats

(* return if an expression is syntactically symbolic *)
(* symbolic = at least 1 variable or a select *)
let is_symbolic_expr (f:smt_expr): bool = (*  NOTE: We could have re-written a visitor.. *)
  let visitor = new statistics_visitor in
  visitor#visit_smt_expr f;
  let v, _, _, _, se, _ = visitor#get_stats in
  v > 0 || se > 0


let is_symbolic_bvexpr (f:smt_bv_expr): bool =
  let visitor = new statistics_visitor in
  visitor#visit_smt_bv_expr f;
  let v, _, _, _, se, _ = visitor#get_stats in
  v > 0 || se > 0
