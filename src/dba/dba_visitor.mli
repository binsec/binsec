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

class dba_inplace_visitor : inplace_visitor_t
