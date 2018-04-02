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

open Format

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

exception Assume_condition of smtBvExprAlt

let rec smtBvExprAlt_to_smtBvExpr e =
  let open Smtlib2 in
  match e with
  | SmtBvCstAlt bv -> SmtBvCst bv
  | SmtBvVarAlt (s, t, _) -> SmtBvVar (s, t)
  | SmtBvUnaryAlt (u, e1) ->
    let smt_e = smtBvExprAlt_to_smtBvExpr e1 in
    SmtBvUnary (u, smt_e)
  | SmtBvBinaryAlt (c, e1, e2) ->
    let smt_e1 = smtBvExprAlt_to_smtBvExpr e1 in
    let smt_e2 = smtBvExprAlt_to_smtBvExpr e2 in
    SmtBvBinary (c, smt_e1, smt_e2)
  | SmtBvIteAlt (c,e1,e2) ->
    let smt_c = smtBvExprAlt_to_smtBvExpr c in
    let smt_e1 = smtBvExprAlt_to_smtBvExpr e1 in
    let smt_e2 = smtBvExprAlt_to_smtBvExpr e2 in
    SmtBvIte (SmtBvExpr smt_c, smt_e1, smt_e2)
  | SmtBvUndefAlt _ -> assert false 


let smtBvBinary_to_string p1 p2 = (function
    | Smtlib2.SmtBvAdd -> Format.asprintf "(bvadd %s %s)" p1 p2
    | Smtlib2.SmtBvSub -> Format.asprintf "(bvsub %s %s)" p1 p2
    | Smtlib2.SmtBvMult -> Format.asprintf "(bvmul %s %s)" p1 p2
    | Smtlib2.SmtBvUdiv -> Format.asprintf "(bvudiv %s %s)" p1 p2
    | Smtlib2.SmtBvSdiv -> Format.asprintf "(bvsdiv %s %s)" p1 p2
    | Smtlib2.SmtBvUrem -> Format.asprintf "(bvurem %s %s)" p1 p2
    | Smtlib2.SmtBvSrem -> Format.asprintf "(bvsrem %s %s)" p1 p2
    | Smtlib2.SmtBvSmod -> Format.asprintf "(bvsmod %s %s)" p1 p2
    | Smtlib2.SmtBvOr -> Format.asprintf "(bvor %s %s)" p1 p2
    | Smtlib2.SmtBvNor -> Format.asprintf "(bvnor %s %s)" p1 p2
    | Smtlib2.SmtBvAnd -> Format.asprintf "(bvand %s %s)" p1 p2
    | Smtlib2.SmtBvNand -> Format.asprintf "(bvnand %s %s)" p1 p2
    | Smtlib2.SmtBvXor -> Format.asprintf "(bvxor %s %s)" p1 p2
    | Smtlib2.SmtBvXnor -> Format.asprintf "(bvxnor %s %s)" p1 p2
    | Smtlib2.SmtBvConcat -> Format.asprintf "(concat %s %s)" p1 p2
    | Smtlib2.SmtBvShl -> Format.asprintf "(bvshl %s %s)" p1 p2
    | Smtlib2.SmtBvLshr -> Format.asprintf "(bvlshr %s %s)" p1 p2
    | Smtlib2.SmtBvAshr -> Format.asprintf "(bvashr %s %s)" p1 p2
    | Smtlib2.SmtBvComp -> Format.asprintf
                     "(ite (= %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
    | Smtlib2.SmtBvDiff -> Format.asprintf
                     "(ite (distinct %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
    | Smtlib2.SmtBvUle -> Format.asprintf
                    "(ite (bvule %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
    | Smtlib2.SmtBvUlt -> Format.asprintf
                    "(ite (bvult %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
    | Smtlib2.SmtBvUge -> Format.asprintf
                    "(ite (bvuge %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
    | Smtlib2.SmtBvUgt -> Format.asprintf
                    "(ite (bvugt %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
    | Smtlib2.SmtBvSle -> Format.asprintf
                    "(ite (bvsle %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
    | Smtlib2.SmtBvSlt -> Format.asprintf
                    "(ite (bvslt %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
    | Smtlib2.SmtBvSge -> Format.asprintf
                    "(ite (bvsge %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
    | Smtlib2.SmtBvSgt -> Format.asprintf
                    "(ite (bvsgt %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
  )





(*******************************to_string***********************)
let smtBvUnary_to_hstring = (function
    | Smtlib2.SmtBvNeg -> "-"
    | Smtlib2.SmtBvNot -> "not"
    | Smtlib2.SmtBvExtract(i, j) -> sprintf "{%i...%i}" j i
    | Smtlib2.SmtBvZeroExtend(i) -> sprintf "ze (%i)" i
    | Smtlib2.SmtBvSignExtend(i) -> sprintf "se (%i)" i
    | Smtlib2.SmtBvRotateL(i) -> sprintf "rl (%i)" i
    | Smtlib2.SmtBvRotateR(i) -> sprintf "rr (%i)" i
  )


let smtBvBinary_to_hstring p1 p2 = function
    | Smtlib2.SmtBvAdd -> Format.asprintf "(%s + %s)" p1 p2
    | Smtlib2.SmtBvSub -> Format.asprintf "(%s - %s)" p1 p2
    | Smtlib2.SmtBvMult -> Format.asprintf "(%s * %s)" p1 p2
    | Smtlib2.SmtBvUdiv -> Format.asprintf "(%s /u %s)" p1 p2
    | Smtlib2.SmtBvSdiv -> Format.asprintf "(%s /s %s)" p1 p2
    | Smtlib2.SmtBvUrem -> Format.asprintf "(%s %_u %s)" p1 p2
    | Smtlib2.SmtBvSrem -> Format.asprintf "(%s %_s %s)" p1 p2
    | Smtlib2.SmtBvSmod -> Format.asprintf "(%s %_s %s)" p1 p2
    | Smtlib2.SmtBvOr -> Format.asprintf "(%s or %s)" p1 p2
    | Smtlib2.SmtBvNor -> Format.asprintf "(%s nor %s)" p1 p2
    | Smtlib2.SmtBvAnd -> Format.asprintf "(%s and %s)" p1 p2
    | Smtlib2.SmtBvNand -> Format.asprintf "(%s nand %s)" p1 p2
    | Smtlib2.SmtBvXor -> Format.asprintf "(%s xor %s)" p1 p2
    | Smtlib2.SmtBvXnor -> Format.asprintf "(%s xnor %s)" p1 p2
    | Smtlib2.SmtBvConcat -> Format.asprintf "(%s :: %s)" p1 p2
    | Smtlib2.SmtBvShl -> Format.asprintf "(%s << %s)" p1 p2
    | Smtlib2.SmtBvLshr -> Format.asprintf "(%s >>u %s)" p1 p2
    | Smtlib2.SmtBvAshr -> Format.asprintf "(%s >>s %s)" p1 p2
    | Smtlib2.SmtBvComp -> Format.asprintf "(%s = %s)" p1 p2
    | Smtlib2.SmtBvDiff -> Format.asprintf "(%s <> %s)" p1 p2
    | Smtlib2.SmtBvUle -> Format.asprintf "(%s <=u %s)" p1 p2
    | Smtlib2.SmtBvUlt -> Format.asprintf "(%s <u %s)" p1 p2
    | Smtlib2.SmtBvUge -> Format.asprintf "(%s >=u %s)" p1 p2
    | Smtlib2.SmtBvUgt -> Format.asprintf "(%s > %s)" p1 p2
    | Smtlib2.SmtBvSle -> Format.asprintf "(%s <=s %s)" p1 p2
    | Smtlib2.SmtBvSlt -> Format.asprintf "(%s <s %s)" p1 p2
    | Smtlib2.SmtBvSge -> Format.asprintf "(%s >=s %s)" p1 p2
    | Smtlib2.SmtBvSgt -> Format.asprintf "(%s >s %s)" p1 p2



let rec smtBvExpr_to_hstring bvexpr =
  let binop_to_hstring op e1 e2 =
    let p1 = smtBvExpr_to_hstring e1 in
    let p2 = smtBvExpr_to_hstring e2 in
    smtBvBinary_to_hstring p1 p2 op
  in
  let open Smtlib2 in
  match bvexpr with
  | SmtBvCstAlt bv ->
    let size = Bitvector.size_of bv in
    let value = Bitvector.value_of bv in
    begin
      let i = Bigint.string_of_big_int value in
      sprintf "%s<%d>" i size
    end
  | SmtBvVarAlt(name, _size, _v) -> name
  | SmtBvUnaryAlt(op, e) ->
    let operand = (smtBvExpr_to_hstring e) in
    sprintf "(%s %s)" (smtBvUnary_to_hstring op) operand
  | SmtBvBinaryAlt(SmtBvComp as op, (SmtBvCstAlt b1 as e1), (SmtBvCstAlt b2 as e2)) ->
    (* Bitvectors of size 1 and of value 0 or 1 are interpreted as booleans *)
    let s1 = Bitvector.size_of b1 in
    let s2 = Bitvector.size_of b2 in
    if s1 = 1 && s2 = 1 then
      let b1 = Bitvector.value_of b1 in
      let b2 = Bitvector.value_of b2 in
      string_of_bool (Bigint.eq_big_int b1 b2)
    else binop_to_hstring op e1 e2
  | SmtBvBinaryAlt (SmtBvAdd as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvSub as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvMult as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvUdiv as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvSdiv as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvUrem as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvSrem as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvSmod as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvOr as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvNor as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvAnd as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvNand as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvXor as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvXnor as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvConcat as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvShl as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvLshr as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvAshr as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvComp as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvDiff as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvUle as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvUlt as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvUge as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvUgt as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvSle as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvSlt as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvSge as op, e1, e2)
  | SmtBvBinaryAlt (SmtBvSgt as op, e1, e2) -> binop_to_hstring op e1 e2
  | SmtBvIteAlt(e, e1, e2) ->
    sprintf "(ite (%s) {%s} {%s})"
      (smtBvExpr_to_hstring e)
      (smtBvExpr_to_hstring e1)
      (smtBvExpr_to_hstring e2)
  | SmtBvUndefAlt (i, _size) -> sprintf "u_%d" i


let rec smtBvExpr_to_string =
  function
  | SmtBvCstAlt bv ->
    let size = Bitvector.size_of bv in
    let value = Bitvector.value_of bv in
    begin
      let i = Bigint.string_of_big_int value in
      sprintf "((_ int2bv %d) %s)" size i
    end
  | SmtBvVarAlt(name, _size, _v) -> name
  | SmtBvUnaryAlt(op, e) ->
    let operand = (smtBvExpr_to_string e) in
    sprintf "(%s %s)" (Smtlib2print.smtbvunary_to_string op) operand
  | SmtBvBinaryAlt(op, e1, e2) ->
    let p1 = (smtBvExpr_to_string e1) in
    let p2 = (smtBvExpr_to_string e2) in
    sprintf "%s" (smtBvBinary_to_string p1 p2 op)
  | SmtBvIteAlt(e, e1, e2) ->
    let c = (smtBvExpr_to_string e) in
    let op1 = (smtBvExpr_to_string e1) in
    let op2 = (smtBvExpr_to_string e2) in
    sprintf "(ite %s %s %s)" c op1 op2
  | SmtBvUndefAlt (i, _size) -> sprintf "undef_%d" i

let is_equal_smtBvExpr a b =
  let e1 = smtBvExprAlt_to_smtBvExpr a in
  let e2 = smtBvExprAlt_to_smtBvExpr b in
  Smtlib2.bvexpr_equal e1 e2


let gen_undef size =
  let count = ref 0 in
  incr count;
  SmtBvUndefAlt (!count, size)
