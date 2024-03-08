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

class type inplace_visitor_t = object
  method visit_assert : Dba.Expr.t -> unit
  method visit_assign : Dba.LValue.t -> Dba.Expr.t -> unit
  method visit_assume : Dba.Expr.t -> unit
  method visit_binary : Dba.Binary_op.t -> Dba.Expr.t -> Dba.Expr.t -> unit
  method visit_cond : Dba.Expr.t -> unit
  method visit_cst : Bitvector.t -> unit
  method visit_dbainstr : Dba_types.Statement.t -> unit
  method visit_djump : Dba.Expr.t -> unit
  method visit_expr : Dba.Expr.t -> unit
  method visit_exts : Dba.Expr.t -> Dba.size -> unit
  method visit_extu : Dba.Expr.t -> Dba.size -> unit
  method visit_if : Dba.Expr.t -> Dba.id Dba.jump_target -> Dba.id -> unit
  method visit_instrkind : Dba.Instr.t -> unit
  method visit_ite : Dba.Expr.t -> Dba.Expr.t -> Dba.Expr.t -> unit
  method visit_lhs : Dba.LValue.t -> unit

  method visit_lhs_var :
    string -> Dba.size -> Dba.id -> Dba.id -> Dba.Var.Tag.t -> unit

  method visit_load :
    Dba.size -> Machine.endianness -> Dba.Expr.t -> string option -> unit

  method visit_local_if : Dba.Expr.t -> Dba.id -> Dba.id -> unit
  method visit_nondet : Dba.LValue.t -> unit
  method visit_remote_if : Dba.Expr.t -> Dba.address -> Dba.id -> unit
  method visit_restrict : Dba.Expr.t -> Dba.id -> Dba.id -> unit
  method visit_sjump : Dba.id Dba.jump_target -> Dba.tag -> unit
  method visit_stop : Dba.state option -> unit

  method visit_store :
    Dba.size -> Machine.endianness -> Dba.Expr.t -> string option -> unit

  method visit_unary : Dba.Unary_op.t -> Dba.Expr.t -> unit
  method visit_undef : Dba.LValue.t -> unit
  method visit_var : string -> Dba.size -> Dba.Var.Tag.t -> unit
end

class dba_inplace_visitor : inplace_visitor_t =
  (* Visitor to visit an smt expression without changing anything *)
  object (self)
    method visit_expr (expr : Dba.Expr.t) : unit =
      match expr with
      | Dba.Expr.Cst bv -> self#visit_cst bv
      | Dba.(Expr.Var { name; size = sz; info = opts; _ }) ->
          self#visit_var name sz opts
      | Dba.Expr.Load (sz, en, expr, arr) ->
          self#visit_load sz en expr arr;
          self#visit_expr expr
      | Dba.Expr.Unary (Dba.Unary_op.Uext n, expr) ->
          self#visit_extu expr n;
          self#visit_expr expr
      | Dba.Expr.Unary (Dba.Unary_op.Sext n, expr) ->
          self#visit_exts expr n;
          self#visit_expr expr
      | Dba.Expr.Unary
          (Dba.Unary_op.Restrict { Interval.lo = i; Interval.hi = j }, expr) ->
          self#visit_restrict expr i j;
          self#visit_expr expr
      | Dba.Expr.Unary (uop, expr) ->
          self#visit_unary uop expr;
          self#visit_expr expr
      | Dba.Expr.Binary (bop, expr1, expr2) ->
          self#visit_binary bop expr1 expr2;
          self#visit_expr expr1;
          self#visit_expr expr2
      | Dba.Expr.Ite (cond, e1, e2) ->
          self#visit_ite cond e1 e2;
          self#visit_cond cond;
          self#visit_expr e1;
          self#visit_expr e2

    method visit_cond cond = self#visit_expr cond

    method visit_lhs (lhs : Dba.LValue.t) : unit =
      match lhs with
      | Dba.(LValue.Var { name; size = sz; info = opts; _ }) ->
          self#visit_lhs_var name sz 0 (sz - 1) opts
      | Dba.LValue.Restrict (v, { Interval.lo; Interval.hi }) ->
          self#visit_lhs_var v.name v.size lo hi v.info
      | Dba.LValue.Store (sz, en, e, arr) ->
          self#visit_store sz en e arr;
          self#visit_expr e

    method visit_instrkind (inst : Dba.Instr.t) : unit =
      match inst with
      | Dba.Instr.Assign (lhs, expr, _) ->
          self#visit_assign lhs expr;
          self#visit_lhs lhs;
          self#visit_expr expr
      | Dba.Instr.SJump (addr, opts) -> self#visit_sjump addr opts
      | Dba.Instr.DJump (expr, _) ->
          self#visit_djump expr;
          self#visit_expr expr
      | Dba.Instr.If (cond, addr, off2) -> (
          self#visit_if cond addr off2;
          self#visit_cond cond;
          match addr with
          | Dba.JInner off1 -> self#visit_local_if cond off1 off2
          | Dba.JOuter addr -> self#visit_remote_if cond addr off2)
      | Dba.Instr.Stop opts -> self#visit_stop opts
      | Dba.Instr.Nondet (lhs, _) ->
          self#visit_nondet lhs;
          self#visit_lhs lhs
      | Dba.Instr.Assume (bcond, _) ->
          self#visit_assume bcond;
          self#visit_cond bcond
      | Dba.Instr.Assert (bcond, _) ->
          self#visit_assert bcond;
          self#visit_cond bcond
      | Dba.Instr.Undef (blhs, _) ->
          self#visit_undef blhs;
          self#visit_lhs blhs

    method visit_dbainstr (instr : Dba_types.Statement.t) : unit =
      self#visit_instrkind (Dba_types.Statement.instruction instr)

    method visit_cst (_bv : Bitvector.t) : unit = ()
    method visit_var (_name : string) (_sz : Dba.size) _opts : unit = ()

    method visit_load (_sz : Dba.size) (_en : Machine.endianness)
        (_expr : Dba.Expr.t) (_arr : string option) =
      ()

    method visit_unary (_uop : Dba.Unary_op.t) (_expr : Dba.Expr.t) : unit = ()

    method visit_binary (_bop : Dba.Binary_op.t) (_expr1 : Dba.Expr.t)
        (_expr2 : Dba.Expr.t) : unit =
      ()

    method visit_restrict (_expr : Dba.Expr.t) (_ : Dba.id) (_j : Dba.id) : unit
        =
      ()

    method visit_extu (_expr : Dba.Expr.t) (_size : Dba.size) : unit = ()
    method visit_exts (_expr : Dba.Expr.t) (_size : Dba.size) : unit = ()
    method visit_ite _ _ _ = ()

    method visit_lhs_var (_name : string) (_size : Dba.size) _low _high _tag =
      ()

    method visit_store (_size : Dba.size) (_en : Machine.endianness)
        (_expr : Dba.Expr.t) (_arr : string option) : unit =
      ()

    method visit_assign (_lhs : Dba.LValue.t) (_expr : Dba.Expr.t) : unit = ()
    method visit_sjump _addr (_tag : Dba.tag) : unit = ()
    method visit_djump (_expr : Dba.Expr.t) : unit = ()
    method visit_if _ _addr (_off2 : Dba.id) : unit = ()
    method visit_local_if _ (_off1 : Dba.id) (_off2 : Dba.id) : unit = ()
    method visit_remote_if _ (_addr : Dba.address) (_off2 : Dba.id) : unit = ()
    method visit_stop _state_option = ()
    method visit_nondet (_lhs : Dba.LValue.t) : unit = ()
    method visit_assume _cond : unit = ()
    method visit_assert _cond : unit = ()
    method visit_undef _lvalue = ()
  end
