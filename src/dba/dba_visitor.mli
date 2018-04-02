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

class dba_inplace_visitor : inplace_visitor_t

val get_var_dbainstr :
  Dba_types.Block.t -> Basic_types.String.Set.t * Basic_types.String.Set.t
