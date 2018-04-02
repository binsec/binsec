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

type cs_status =
  | Conc (* Concretize systematically *)
  | KeepOrConc
  | KeepOrSymb
  | Symb (* Symbolise whatever the value is *)

type cs_action = cs_status * string (* String *)


type metavar_item =
  | InstReif of Dba.instruction
  | LhsReif of Dba.lhs
  | CondReif of Dba.cond
  | ExprReif of Dba.expr

type loc_pred =
  | LocWildcard
  | LocSet of int64 list
  | LocInterval of int64 * int64

type inst_pred =
  | InstWildcard
  | InstPattern of Dba.instruction

type exp_pred =
  | ExpWildcard
  | ExpSubTerm of Dba.expr * Dba.expr
  | ExpDba of Dba.expr

type sigma_binary = And | Or
type sigma_unary = Not

type sigma_pred =
  | SigmaWildcard
  | SigmaUnary of sigma_unary * sigma_pred
  | SigmaBinary of sigma_binary * sigma_pred * sigma_pred
  | TaintCheck of Taint_types.taint * Dba.expr

type rule_t = {
  loc_p : loc_pred;
  inst_p : inst_pred;
  exp_p: exp_pred;
  sigma_p : sigma_pred;
  action: cs_action;
}

type policy = rule_t list
