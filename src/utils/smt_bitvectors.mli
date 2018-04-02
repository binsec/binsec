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

(** Types for SMT-LIB bitvectors *)

type basic_value = [ `Value of Dba.region * Bitvector.t | `Undef  of int]                                              
type smtBvVarAlt = string * int * basic_value (* name, size, value *)

type smtBvExprAlt =
  | SmtBvCstAlt of Bitvector.t
  | SmtBvVarAlt of smtBvVarAlt
  | SmtBvUnaryAlt of Smtlib2.smt_bv_unary * smtBvExprAlt
  | SmtBvBinaryAlt of Smtlib2.smt_bv_binary * smtBvExprAlt * smtBvExprAlt
  | SmtBvIteAlt of smtBvExprAlt * smtBvExprAlt * smtBvExprAlt
  | SmtBvUndefAlt of int * int

type condition_env = smtBvExprAlt list

val smtBvExprAlt_to_smtBvExpr : smtBvExprAlt -> Smtlib2.smt_bv_expr
val is_equal_smtBvExpr : smtBvExprAlt -> smtBvExprAlt -> bool
val smtBvExpr_to_hstring : smtBvExprAlt -> string
val smtBvExpr_to_string : smtBvExprAlt -> string

val gen_undef : int -> smtBvExprAlt

exception Assume_condition of smtBvExprAlt
