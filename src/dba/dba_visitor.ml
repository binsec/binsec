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

class type inplace_visitor_t = object
  method visit_alternative : Dba.exprs -> Dba.alternativeTag option -> unit
  method visit_assert : Dba.cond -> unit
  method visit_assign : Dba.lhs -> Dba.expr -> unit
  method visit_assume : Dba.cond -> unit
  method visit_binary : Dba.binary_op -> Dba.expr -> Dba.expr -> unit
  method visit_cond : Dba.cond -> unit
  method visit_cond_and : Dba.cond -> Dba.cond -> unit
  method visit_cond_or : Dba.cond -> Dba.cond -> unit
  method visit_cst : Bitvector.t -> unit
  method visit_dbainstr : Dba_types.Statement.t -> unit
  method visit_djump : Dba.expr -> unit
  method visit_expr : Dba.expr -> unit
  method visit_exts : Dba.expr -> Dba.size -> unit
  method visit_extu : Dba.expr -> Dba.size -> unit
  method visit_free : Dba.expr -> unit
  method visit_if : Dba.cond -> Dba.jump_target -> Dba.id -> unit
  method visit_instrkind : Dba.instruction -> unit
  method visit_ite : Dba.cond -> Dba.expr -> Dba.expr -> unit
  method visit_lhs : Dba.lhs -> unit
  method visit_lhs_var :
    string -> Dba.size -> Dba.id -> Dba.id -> Dba.vartag option -> unit
  method visit_load : Dba.size -> Dba.endianness -> Dba.expr -> unit
  method visit_local_if : Dba.cond -> Dba.id -> Dba.id -> unit
  method visit_malloc : Dba.lhs -> Dba.expr -> unit
  method visit_nondet : Dba.lhs -> unit
  method visit_nondet_assume : Dba.lhs list -> Dba.cond -> unit
  method visit_remote_if : Dba.cond -> Dba.address -> Dba.id -> unit
  method visit_restrict : Dba.expr -> Dba.id -> Dba.id -> unit
  method visit_sjump : Dba.jump_target -> Dba.tag option -> unit
  method visit_stop : Dba.state option -> unit
  method visit_store : Dba.size -> Dba.endianness -> Dba.expr -> unit
  method visit_unary : Dba.unary_op -> Dba.expr -> unit
  method visit_undef : Dba.lhs -> unit
  method visit_var : string -> Dba.size -> Dba.vartag option -> unit
end

class dba_inplace_visitor : inplace_visitor_t = (* Visitor to visit an smt expression without changing anything *)
object(self)

  method visit_expr (expr:Dba.expr): unit =
    match expr with
    | Dba.ExprCst(_,bv) -> self#visit_cst bv
    | Dba.ExprVar(name, sz, opts) -> self#visit_var name sz opts
    | Dba.ExprLoad (sz,en,expr) -> self#visit_load sz en expr; self#visit_expr expr
    | Dba.ExprUnary (uop, expr) -> self#visit_unary uop expr; self#visit_expr expr
    | Dba.ExprBinary(bop,expr1,expr2) -> self#visit_binary bop expr1 expr2;
                                        self#visit_expr expr1;
                                        self#visit_expr expr2
    | Dba.ExprRestrict(expr,i,j) -> self#visit_restrict expr i j; self#visit_expr expr
    | Dba.ExprExtU(expr,n) -> self#visit_extu expr n; self#visit_expr expr
    | Dba.ExprExtS(expr,n) -> self#visit_exts expr n; self#visit_expr expr
    | Dba.ExprIte(cond,e1,e2) ->  self#visit_ite cond e1 e2;
                                 self#visit_cond cond;
                                 self#visit_expr e1;
                                 self#visit_expr e2
   | Dba.ExprAlternative (e_list, tag_option) ->
      self#visit_alternative e_list tag_option;
      List.iter (fun i -> self#visit_expr i) e_list

  method visit_cond (cond:Dba.cond): unit =
    match cond with
    | Dba.CondReif(exp) -> self#visit_expr exp
    | Dba.CondNot(c1) -> self#visit_cond c1
    | Dba.CondAnd(c1, c2) -> self#visit_cond_and c1 c2; self#visit_cond c1; self#visit_cond c2
    | Dba.CondOr(c1,c2) -> self#visit_cond_or c1 c2; self#visit_cond c1; self#visit_cond c2
    | Dba.True -> ()
    | Dba.False -> ()

  method visit_lhs (lhs:Dba.lhs): unit =
    match lhs with
    | Dba.LhsVar(name, sz, opts) -> self#visit_lhs_var name sz 0 (sz-1) opts
    | Dba.LhsVarRestrict(name, sz, l, h) -> self#visit_lhs_var name sz l h None
    | Dba.LhsStore(sz, en, e) -> self#visit_store sz en e; self#visit_expr e

  method visit_instrkind (inst:Dba.instruction): unit =
    match inst with
    | Dba.IkAssign(lhs,expr, _) -> self#visit_assign lhs expr; self#visit_lhs lhs; self#visit_expr expr
    | Dba.IkSJump(addr,opts) -> self#visit_sjump addr opts
    | Dba.IkDJump(expr,_) -> self#visit_djump expr; self#visit_expr expr
    | Dba.IkIf(cond, addr, off2) -> self#visit_if cond addr off2; self#visit_cond cond;
      begin match addr with
      | Dba.JInner off1 -> self#visit_local_if cond off1 off2
      | Dba.JOuter addr -> self#visit_remote_if cond addr off2
      end
    | Dba.IkStop opts -> self#visit_stop opts
    | Dba.IkPrint(_, _) -> ()
    | Dba.IkNondetAssume (l, cond, _) -> self#visit_nondet_assume l cond; List.iter self#visit_lhs l;
                                        self#visit_cond cond
    | Dba.IkNondet (lhs, _, _) -> self#visit_nondet lhs; self#visit_lhs lhs
    | Dba.IkAssume (bcond, _) -> self#visit_assume bcond; self#visit_cond bcond
    | Dba.IkAssert (bcond, _) -> self#visit_assert bcond; self#visit_cond bcond
    | Dba.IkMalloc (lhs, bexpr, _) -> self#visit_malloc lhs bexpr; self#visit_lhs lhs; self#visit_expr bexpr
    | Dba.IkFree (bexpr, _) -> self#visit_free bexpr; self#visit_expr bexpr
    | Dba.IkUndef (blhs, _) -> self#visit_undef blhs; self#visit_lhs blhs

  method visit_dbainstr (instr:Dba_types.Statement.t) : unit =
    self#visit_instrkind (Dba_types.Statement.instruction instr)

  method visit_cst (_bv:Bitvector.t) : unit = ()

  method visit_var (_name:string) (_sz:Dba.size) (_opts:Dba.vartag option) : unit = ()

  method visit_load (_sz:Dba.size) (_en:Dba.endianness) (_expr:Dba.expr) : unit = ()

  method visit_unary (_uop:Dba.unary_op) (_expr:Dba.expr) : unit = ()

  method visit_binary (_bop:Dba.binary_op) (_expr1:Dba.expr) (_expr2:Dba.expr) : unit = ()

  method visit_restrict (_expr:Dba.expr) (_:Dba.id) (_j:Dba.id) : unit = ()

  method visit_extu (_expr:Dba.expr) (_size:Dba.size) : unit = ()

  method visit_exts (_expr:Dba.expr) (_size:Dba.size) : unit = ()

  method visit_ite (_cond:Dba.cond) (_expr1:Dba.expr) (_expr2:Dba.expr) : unit = ()

  method visit_alternative (_e_list:Dba.expr list) (_tag_option: Dba.alternativeTag option) : unit = ()

  method visit_cond_and (_cond1:Dba.cond) (_cond2:Dba.cond) : unit = ()

  method visit_cond_or (_cond1:Dba.cond) (_cond2:Dba.cond) : unit = ()

  method visit_lhs_var (_name:string) (_size:Dba.size) (_low:Dba.id) (_high:Dba.id) (_opts:Dba.vartag option): unit = ()

  method visit_store (_size:Dba.size) (_en:Dba.endianness) (_expr:Dba.expr) : unit = ()

  method visit_assign (_lhs:Dba.lhs) (_expr:Dba.expr) : unit = ()

  method visit_sjump (_addr:Dba.jump_target) (_opts:Dba.tag option) : unit = ()

  method visit_djump (_expr:Dba.expr) : unit = ()

  method visit_if (_cond:Dba.cond) (_addr:Dba.jump_target) (_off2:Dba.id) : unit = ()

  method visit_local_if (_cond:Dba.cond) (_off1:Dba.id) (_off2:Dba.id) : unit = ()

  method visit_remote_if (_cond:Dba.cond) (_addr:Dba.address) (_off2:Dba.id) : unit = ()

  method visit_stop _state_option = ()

  method visit_nondet_assume _lvalues (_cond:Dba.cond)  = ()

  method visit_nondet (_lhs:Dba.lhs) : unit = ()

  method visit_assume (_cond:Dba.cond) : unit = ()

  method visit_assert (_cond:Dba.cond) : unit = ()

  method visit_malloc (_lhs:Dba.lhs) (_expr:Dba.expr) : unit = ()

  method visit_free (_expr:Dba.expr) : unit = ()

  method visit_undef _lvalue = ()

end;;


class get_var_visitor =
  object inherit dba_inplace_visitor
  val mutable vars_read = String.Set.empty
  val mutable vars_written = String.Set.empty

  method get_read_vars = vars_read
  method get_written_vars = vars_written

  method! visit_lhs_var name _size _low _high _opts =
    vars_written <- String.Set.add name vars_written

  method! visit_var name _ _ =
    vars_read <- String.Set.add name vars_read

end;;

let get_var_dbainstr (instr:Dba_types.Block.t): String.Set.t * String.Set.t =
  let visitor = new get_var_visitor in
  Dba_types.Block.iter (fun i -> visitor#visit_instrkind i) instr;
  visitor#get_read_vars, visitor#get_written_vars
