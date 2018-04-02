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

(* -------------------------------------------------- *)
(* ---------------- Generic Visitors ---------------- *)

open Smtlib2

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

class smt_iter_visitor : [unit,unit,unit] smt_visitor
class smt_map_visitor  : [smt_expr,smt_bv_expr,smt_abv_expr] smt_visitor

class ['a] smt_fold_visitor : ['a->'a,'a->'a,'a->'a] smt_visitor

class smt_exists_visitor : [bool,bool,bool] smt_visitor
class smt_forall_visitor : [bool,bool,bool] smt_visitor

val smt_bv_expr : ('a -> 'b) ->
  ('b,'a,'c) smt_visitor -> ('b,'a,'c) smt_visitor
val smt_abv_expr : ('a -> 'b) ->
  ('b,'c,'a) smt_visitor -> ('b,'c,'a) smt_visitor
val smt_and : ('a -> 'a -> 'a) ->
  ('a,'b,'c) smt_visitor -> ('a,'b,'c) smt_visitor
val smt_or : ('a -> 'a -> 'a) ->
  ('a,'b,'c) smt_visitor -> ('a,'b,'c) smt_visitor
val smt_comp : ('a -> 'a -> 'a) ->
  ('a,'b,'c) smt_visitor -> ('a,'b,'c) smt_visitor
val smt_not : ('a -> 'a) ->
  ('a,'b,'c) smt_visitor -> ('a,'b,'c) smt_visitor
val smt_let : (('a * 'a) list -> 'a -> 'a) ->
  ('a,'b,'c) smt_visitor -> ('a,'b,'c) smt_visitor
val smt_ite : ('a -> 'a -> 'a -> 'a) ->
  ('a,'b,'c) smt_visitor -> ('a,'b,'c) smt_visitor
val smt_true : 'a ->
  ('a,'b,'c) smt_visitor -> ('a,'b,'c) smt_visitor
val smt_false : 'a ->
  ('a,'b,'c) smt_visitor -> ('a,'b,'c) smt_visitor
val smt_token : 'a ->
  ('a,'b,'c) smt_visitor -> ('a,'b,'c) smt_visitor
val smt_comment : (string -> 'a) ->
  ('a,'b,'c) smt_visitor -> ('a,'b,'c) smt_visitor

val smt_bv_cst : (Bitvector.t -> 'a) ->
  ('b,'a,'c) smt_visitor -> ('b,'a,'c) smt_visitor
val smt_bv_var : (Smtlib2.smt_bv_var -> 'a) ->
  ('b,'a,'c) smt_visitor -> ('b,'a,'c) smt_visitor
val smt_bv_unary : (Smtlib2.smt_bv_unary -> 'a -> 'a) ->
  ('b,'a,'c) smt_visitor -> ('b,'a,'c) smt_visitor
val smt_bv_binary : (Smtlib2.smt_bv_binary -> 'a -> 'a -> 'a) ->
  ('b,'a,'c) smt_visitor -> ('b,'a,'c) smt_visitor
val smt_bv_ite : ('a -> 'b -> 'b -> 'b) ->
  ('a,'b,'c) smt_visitor -> ('a,'b,'c) smt_visitor
val smt_abv_select : ('a -> 'b -> 'b) ->
  ('c,'b,'a) smt_visitor -> ('c,'b,'a) smt_visitor
val smt_bv_let : (('a * 'a) list -> 'b -> 'b) ->
  ('a,'b,'c) smt_visitor -> ('a,'b,'c) smt_visitor
val smt_abv_load32 : ('a -> 'b -> 'b) ->
  ('c,'b,'a) smt_visitor -> ('c,'b,'a) smt_visitor
val smt_bv_token : 'a ->
  ('b,'a,'c) smt_visitor -> ('b,'a,'c) smt_visitor

val smt_abv_array : (Smtlib2.smt_abv_array -> 'a) ->
  ('b,'c,'a) smt_visitor -> ('b,'c,'a) smt_visitor
val smt_abv_store : ('a -> 'b -> 'b -> 'a) ->
  ('c,'b,'a) smt_visitor -> ('c,'b,'a) smt_visitor
val smt_abv_let : (('a * 'a) list -> 'b -> 'b) ->
  ('a,'c,'b) smt_visitor -> ('a,'c,'b) smt_visitor
val smt_abv_store32 : ('a -> 'b -> 'b -> 'a) ->
  ('c,'b,'a) smt_visitor -> ('c,'b,'a) smt_visitor


(** class that visit the given smt expression and provide a 
    method [get_vars] that returns all variables found into it *)
class get_var_visitor :
  object inherit smt_iter_visitor

    (** @return a a string set of all the variable label
        found in the expression *)
    method get_vars: Basic_types.String.Set.t

    method clear: unit -> unit
  end

(** @return all the variables names found in an [smt_expr] *)
val get_var_expr: smt_expr -> Basic_types.String.Set.t

(** @return all the variables names found in an [smt_bv_expr] *)
val get_var_bvexpr: smt_bv_expr -> Basic_types.String.Set.t

(** @return all the variables names found in an [smt_abv_expr] *)
val get_var_abvexpr: smt_abv_expr -> Basic_types.String.Set.t

(** [replace_expr expr token sub] replace the [token] sub expression
    by [sub] in [expr] if [token] found *)
val replace_expr: smt_expr -> smt_expr -> smt_expr -> smt_expr

(** [replace_token_expr expr sub] replace the expression SmtToken
    expression by [sub] in [expr] if SmtToken found *)
val replace_token_expr: smt_expr -> smt_expr -> smt_expr

(** [replace_bvexpr expr token sub] replace the [token] sub expression
    by [sub] in [expr] if [token] found *)
val replace_bvexpr: smt_bv_expr -> smt_bv_expr -> smt_bv_expr -> smt_bv_expr

(** class to compute various stats about an smt expression *)
class statistics_visitor :
  object inherit smt_iter_visitor
    method get_stats: int * int * int * int * int * int
  end

(** compute stats about the given smt_expr. Stats are in the right
    order:
    - number of variables
    - number of constants
    - number of unary operation
    - number of binary operation
    - number of read in the memory(array)
    - number of store in the memory(array) *)
val stat_expr: smt_expr -> int * int * int * int * int * int

(** see [stat_expr] *)
val stat_bvexpr: smt_bv_expr -> int * int * int * int * int * int

(** Return [true] if the expression is syntactically symbolic
    meaning there is at least one variable or on select in an array *)
val is_symbolic_expr: smt_expr -> bool

(** see [is_symbolic_expr] *)
val is_symbolic_bvexpr: smt_bv_expr -> bool

