(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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
  | SmtBvUnaryAlt of Formula.bv_unop * smtBvExprAlt
  | SmtBvBinaryAlt of Formula.bv_bnop * smtBvExprAlt * smtBvExprAlt
  | SmtBvComparisonAlt of Formula.bv_comp * smtBvExprAlt * smtBvExprAlt
  | SmtBvIteAlt of smtBvExprAlt * smtBvExprAlt * smtBvExprAlt
  | SmtBvUndefAlt of int * int

type condition_env = smtBvExprAlt list

exception Assume_condition of smtBvExprAlt

let rec smtBvExprAlt_to_smtBvExpr e =
  let open Formula in
  match e with
  | SmtBvCstAlt bv -> mk_bv_cst bv
  | SmtBvVarAlt (s, t, _) -> mk_bv_var (bv_var s t)
  | SmtBvUnaryAlt (u, e1) ->
    let smt_e = smtBvExprAlt_to_smtBvExpr e1 in
    mk_bv_unop u smt_e
  | SmtBvBinaryAlt (b, e1, e2) ->
    let smt_e1 = smtBvExprAlt_to_smtBvExpr e1 in
    let smt_e2 = smtBvExprAlt_to_smtBvExpr e2 in
    mk_bv_bnop b smt_e1 smt_e2
  | SmtBvComparisonAlt (c, e1, e2) ->
    let smt_e1 = smtBvExprAlt_to_smtBvExpr e1 in
    let smt_e2 = smtBvExprAlt_to_smtBvExpr e2 in
    mk_bv_ite (mk_bv_comp c smt_e1 smt_e2) (mk_bv_one) (mk_bv_zero)
  | SmtBvIteAlt (c,e1,e2) ->
    let smt_c = smtBvExprAlt_to_smtBvExpr c in
    let smt_e1 = smtBvExprAlt_to_smtBvExpr e1 in
    let smt_e2 = smtBvExprAlt_to_smtBvExpr e2 in
    mk_bv_ite (mk_bv_equal smt_c (mk_bv_one)) smt_e1 smt_e2
  | SmtBvUndefAlt _ -> assert false


let smtBvBinary_to_string p1 p2 = function
  | Formula.BvAdd -> Format.asprintf "(bvadd %s %s)" p1 p2
  | Formula.BvSub -> Format.asprintf "(bvsub %s %s)" p1 p2
  | Formula.BvMul -> Format.asprintf "(bvmul %s %s)" p1 p2
  | Formula.BvUdiv -> Format.asprintf "(bvudiv %s %s)" p1 p2
  | Formula.BvSdiv -> Format.asprintf "(bvsdiv %s %s)" p1 p2
  | Formula.BvUrem -> Format.asprintf "(bvurem %s %s)" p1 p2
  | Formula.BvSrem -> Format.asprintf "(bvsrem %s %s)" p1 p2
  | Formula.BvSmod -> Format.asprintf "(bvsmod %s %s)" p1 p2
  | Formula.BvOr -> Format.asprintf "(bvor %s %s)" p1 p2
  | Formula.BvNor -> Format.asprintf "(bvnor %s %s)" p1 p2
  | Formula.BvAnd -> Format.asprintf "(bvand %s %s)" p1 p2
  | Formula.BvNand -> Format.asprintf "(bvnand %s %s)" p1 p2
  | Formula.BvXor -> Format.asprintf "(bvxor %s %s)" p1 p2
  | Formula.BvXnor -> Format.asprintf "(bvxnor %s %s)" p1 p2
  | Formula.BvConcat -> Format.asprintf "(concat %s %s)" p1 p2
  | Formula.BvShl -> Format.asprintf "(bvshl %s %s)" p1 p2
  | Formula.BvLshr -> Format.asprintf "(bvlshr %s %s)" p1 p2
  | Formula.BvAshr -> Format.asprintf "(bvashr %s %s)" p1 p2
  | Formula.BvCmp -> Format.asprintf "(bvcomp %s %s)" p1 p2

let smtBvComparison_to_string p1 p2 = function
  | Formula.BvEqual -> Format.asprintf
                         "(ite (= %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
  | Formula.BvDistinct -> Format.asprintf
                            "(ite (distinct %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
  | Formula.BvUle -> Format.asprintf
                       "(ite (bvule %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
  | Formula.BvUlt -> Format.asprintf
                       "(ite (bvult %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
  | Formula.BvUge -> Format.asprintf
                       "(ite (bvuge %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
  | Formula.BvUgt -> Format.asprintf
                       "(ite (bvugt %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
  | Formula.BvSle -> Format.asprintf
                       "(ite (bvsle %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
  | Formula.BvSlt -> Format.asprintf
                       "(ite (bvslt %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
  | Formula.BvSge -> Format.asprintf
                       "(ite (bvsge %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2
  | Formula.BvSgt -> Format.asprintf
                       "(ite (bvsgt %s %s) ((_ int2bv 1) 1) ((_ int2bv 1) 0))" p1 p2




(*******************************to_string***********************)
let smtBvUnary_to_hstring = function
  | Formula.BvNeg -> "-"
  | Formula.BvNot -> "not"
  | Formula.BvRepeat(i) -> sprintf "repeat (%i)" i
  | Formula.BvZeroExtend(i) -> sprintf "ze (%i)" i
  | Formula.BvSignExtend(i) -> sprintf "se (%i)" i
  | Formula.BvRotateLeft(i) -> sprintf "rl (%i)" i
  | Formula.BvRotateRight(i) -> sprintf "rr (%i)" i
  | Formula.BvExtract i -> sprintf "{%i...%i}" i.Interval.hi i.Interval.lo


let smtBvBinary_to_hstring p1 p2 = function
  | Formula.BvAdd -> Format.asprintf "(%s + %s)" p1 p2
  | Formula.BvSub -> Format.asprintf "(%s - %s)" p1 p2
  | Formula.BvMul -> Format.asprintf "(%s * %s)" p1 p2
  | Formula.BvUdiv -> Format.asprintf "(%s /u %s)" p1 p2
  | Formula.BvSdiv -> Format.asprintf "(%s /s %s)" p1 p2
  | Formula.BvUrem -> Format.asprintf "(%s %_u %s)" p1 p2
  | Formula.BvSrem -> Format.asprintf "(%s %_s %s)" p1 p2
  | Formula.BvSmod -> Format.asprintf "(%s %_s %s)" p1 p2
  | Formula.BvOr -> Format.asprintf "(%s or %s)" p1 p2
  | Formula.BvNor -> Format.asprintf "(%s nor %s)" p1 p2
  | Formula.BvAnd -> Format.asprintf "(%s and %s)" p1 p2
  | Formula.BvNand -> Format.asprintf "(%s nand %s)" p1 p2
  | Formula.BvXor -> Format.asprintf "(%s xor %s)" p1 p2
  | Formula.BvXnor -> Format.asprintf "(%s xnor %s)" p1 p2
  | Formula.BvConcat -> Format.asprintf "(%s :: %s)" p1 p2
  | Formula.BvShl -> Format.asprintf "(%s << %s)" p1 p2
  | Formula.BvLshr -> Format.asprintf "(%s >>u %s)" p1 p2
  | Formula.BvAshr -> Format.asprintf "(%s >>s %s)" p1 p2
  | Formula.BvCmp -> Format.asprintf "(%s = %s)" p1 p2

let smtBvComparison_to_hstring p1 p2 = function
  | Formula.BvEqual -> Format.asprintf "(%s = %s)" p1 p2
  | Formula.BvDistinct -> Format.asprintf "(%s <> %s)" p1 p2
  | Formula.BvUle -> Format.asprintf "(%s <=u %s)" p1 p2
  | Formula.BvUlt -> Format.asprintf "(%s <u %s)" p1 p2
  | Formula.BvUge -> Format.asprintf "(%s >=u %s)" p1 p2
  | Formula.BvUgt -> Format.asprintf "(%s > %s)" p1 p2
  | Formula.BvSle -> Format.asprintf "(%s <=s %s)" p1 p2
  | Formula.BvSlt -> Format.asprintf "(%s <s %s)" p1 p2
  | Formula.BvSge -> Format.asprintf "(%s >=s %s)" p1 p2
  | Formula.BvSgt -> Format.asprintf "(%s >s %s)" p1 p2


let rec smtBvExpr_to_hstring bvexpr =
  let bnop_to_hstring op e1 e2 =
    let p1 = smtBvExpr_to_hstring e1 in
    let p2 = smtBvExpr_to_hstring e2 in
    smtBvBinary_to_hstring p1 p2 op
  in
  let comp_to_hstring op e1 e2 =
    let p1 = smtBvExpr_to_hstring e1 in
    let p2 = smtBvExpr_to_hstring e2 in
    smtBvComparison_to_hstring p1 p2 op
  in
  let open Formula in
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
  | SmtBvBinaryAlt(BvCmp as op, (SmtBvCstAlt b1 as e1), (SmtBvCstAlt b2 as e2)) ->
    (* Bitvectors of size 1 and of value 0 or 1 are interpreted as booleans *)
    let s1 = Bitvector.size_of b1 in
    let s2 = Bitvector.size_of b2 in
    if s1 = 1 && s2 = 1 then
      let b1 = Bitvector.value_of b1 in
      let b2 = Bitvector.value_of b2 in
      string_of_bool (Bigint.eq_big_int b1 b2)
    else bnop_to_hstring op e1 e2
  | SmtBvBinaryAlt (BvAdd as op, e1, e2)
  | SmtBvBinaryAlt (BvSub as op, e1, e2)
  | SmtBvBinaryAlt (BvMul as op, e1, e2)
  | SmtBvBinaryAlt (BvUdiv as op, e1, e2)
  | SmtBvBinaryAlt (BvSdiv as op, e1, e2)
  | SmtBvBinaryAlt (BvUrem as op, e1, e2)
  | SmtBvBinaryAlt (BvSrem as op, e1, e2)
  | SmtBvBinaryAlt (BvSmod as op, e1, e2)
  | SmtBvBinaryAlt (BvOr as op, e1, e2)
  | SmtBvBinaryAlt (BvNor as op, e1, e2)
  | SmtBvBinaryAlt (BvAnd as op, e1, e2)
  | SmtBvBinaryAlt (BvNand as op, e1, e2)
  | SmtBvBinaryAlt (BvXor as op, e1, e2)
  | SmtBvBinaryAlt (BvXnor as op, e1, e2)
  | SmtBvBinaryAlt (BvConcat as op, e1, e2)
  | SmtBvBinaryAlt (BvShl as op, e1, e2)
  | SmtBvBinaryAlt (BvLshr as op, e1, e2)
  | SmtBvBinaryAlt (BvAshr as op, e1, e2)
  | SmtBvBinaryAlt (BvCmp as op, e1, e2) -> bnop_to_hstring op e1 e2
  | SmtBvComparisonAlt (BvEqual as op, e1, e2)
  | SmtBvComparisonAlt (BvDistinct as op, e1, e2)
  | SmtBvComparisonAlt (BvUle as op, e1, e2)
  | SmtBvComparisonAlt (BvUlt as op, e1, e2)
  | SmtBvComparisonAlt (BvUge as op, e1, e2)
  | SmtBvComparisonAlt (BvUgt as op, e1, e2)
  | SmtBvComparisonAlt (BvSle as op, e1, e2)
  | SmtBvComparisonAlt (BvSlt as op, e1, e2)
  | SmtBvComparisonAlt (BvSge as op, e1, e2)
  | SmtBvComparisonAlt (BvSgt as op, e1, e2) -> comp_to_hstring op e1 e2
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
    sprintf "(%s %s)" (Formula_pp.print_bv_unop op) operand
  | SmtBvBinaryAlt(op, e1, e2) ->
    let p1 = (smtBvExpr_to_string e1) in
    let p2 = (smtBvExpr_to_string e2) in
    sprintf "%s" (smtBvBinary_to_string p1 p2 op)
  | SmtBvComparisonAlt(op, e1, e2) ->
    let p1 = (smtBvExpr_to_string e1) in
    let p2 = (smtBvExpr_to_string e2) in
    sprintf "%s" (smtBvComparison_to_string p1 p2 op)
  | SmtBvIteAlt(e, e1, e2) ->
    let c = (smtBvExpr_to_string e) in
    let op1 = (smtBvExpr_to_string e1) in
    let op2 = (smtBvExpr_to_string e2) in
    sprintf "(ite %s %s %s)" c op1 op2
  | SmtBvUndefAlt (i, _size) -> sprintf "undef_%d" i

let is_equal_smtBvExpr a b =
  let e1 = smtBvExprAlt_to_smtBvExpr a in
  let e2 = smtBvExprAlt_to_smtBvExpr b in
  Formula.equal_bv_term e1 e2


let gen_undef size =
  let count = ref 0 in
  incr count;
  SmtBvUndefAlt (!count, size)
