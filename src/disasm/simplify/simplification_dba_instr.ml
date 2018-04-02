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

let rec remove_true_false_from_cond condition =
  let open Dba_types in
  match condition with
  | Dba.CondNot cnd ->
    remove_true_false_from_cond cnd |> Condition.cnot
  | Dba.CondAnd (cnd1, cnd2) ->
    let cnd1 = remove_true_false_from_cond cnd1
    and cnd2 = remove_true_false_from_cond cnd2 in
    Condition.cand cnd1 cnd2
  | Dba.CondOr(cnd1, cnd2) ->
    let cnd1 = remove_true_false_from_cond cnd1
    and cnd2 = remove_true_false_from_cond cnd2 in
    begin
      match (cnd1, cnd2) with
      | (Dba.True, _) -> Dba.True
      | (_, Dba.True) -> Dba.True
      | (Dba.False, _) -> cnd2
      | (_, Dba.False) -> cnd1
      | (_, _) -> Dba.CondOr(cnd1, cnd2)
    end
  | Dba.CondReif _
  | Dba.True
  | Dba.False -> condition


let simplify_conjunction lcondition rcondition =
  (* TODO : Adds reduction to false if the condition cannot be satisfied 
     as in a > b && a < b
  *)
  let open Dba_types in
  let build f e1 e2 e3 e4 =
    if Expr.equal e1 e3 && Expr.equal e2 e4
    then Dba.CondReif (f e1 e2)
    else Condition.cand lcondition rcondition
  in
  match lcondition, rcondition with 
  | Dba.CondReif (Dba.ExprBinary (Dba.GeqU, e1, e2)),
    Dba.CondReif (Dba.ExprBinary (Dba.GtU, e3, e4))
  | Dba.CondReif (Dba.ExprBinary (Dba.GtU, e3, e4)),
    Dba.CondReif (Dba.ExprBinary (Dba.GeqU, e1, e2)) ->
    build Expr.ugt e1 e2 e3 e4
  | Dba.CondReif (Dba.ExprBinary (Dba.GeqU, e1, e2)),
    Dba.CondReif (Dba.ExprBinary (Dba.LeqU, e3, e4))
  | Dba.CondReif (Dba.ExprBinary (Dba.LeqU, e3, e4)),
    Dba.CondReif (Dba.ExprBinary (Dba.GeqU, e1, e2))
  | Dba.CondReif (Dba.ExprBinary (Dba.GeqS, e1, e2)),
    Dba.CondReif (Dba.ExprBinary (Dba.LeqS, e3, e4))
  | Dba.CondReif (Dba.ExprBinary (Dba.LeqS, e3, e4)),
    Dba.CondReif (Dba.ExprBinary (Dba.GeqS, e1, e2))
    -> build Expr.eq e1 e2 e3 e4
  | Dba.CondReif (Dba.ExprBinary (Dba.GeqS, e1, e2)),
    Dba.CondReif (Dba.ExprBinary (Dba.GtS, e3, e4))
  | Dba.CondReif (Dba.ExprBinary (Dba.GtS, e3, e4)),
    Dba.CondReif (Dba.ExprBinary (Dba.GeqS, e1, e2))
    -> build Expr.sgt e1 e2 e3 e4
  | _, _ -> Condition.cand lcondition rcondition

let rec remove_some_arith_pred_from_cond cond  =
  match cond with
  | Dba.CondReif _ -> cond
  | Dba.CondNot cnd ->
    let cnd = remove_some_arith_pred_from_cond cnd in
    begin
      match cnd with
      | Dba.CondReif (Dba.ExprBinary (bop, e1, e2))
        when Dba_types.BinaryOperator.has_inverse bop  ->
        Dba.CondReif (Dba.ExprBinary (Dba_types.BinaryOperator.invert bop, e1, e2))
      | Dba.CondNot c -> c
      | _ -> Dba.CondNot cnd
    end
  | Dba.CondAnd (cnd1, cnd2) ->
    let cnd1 = remove_some_arith_pred_from_cond cnd1
    and cnd2 = remove_some_arith_pred_from_cond cnd2 in
    simplify_conjunction cnd1 cnd2
    
  | Dba.CondOr(cnd1, cnd2) ->
    let cnd1 = remove_some_arith_pred_from_cond cnd1
    and cnd2 = remove_some_arith_pred_from_cond cnd2 in
    Dba.CondOr (cnd1, cnd2)
  | Dba.True
  | Dba.False -> cond


let simplify_dbacond  cond  =
  remove_some_arith_pred_from_cond (remove_true_false_from_cond cond)


let simplify_bop bop e1 e2 =
  let open Dba_types.Expr in
  match bop with
  | Dba.Plus ->
    if is_zero e1 then e2
    else if is_zero e2 then e1
    else binary bop e1 e2
  | Dba.Minus ->
    if is_zero e2 then e1
    else if is_zero e1 then Dba.ExprUnary (Dba.UMinus, e2)
    else binary bop e1 e2
  | Dba.MultU | Dba.MultS ->
    if is_zero e1 then constant (Bitvector.zeros (size_of e1))
    else if is_zero e2 then constant (Bitvector.zeros (size_of e2))
    else binary bop e1 e2
  | Dba.Xor ->
    if equal e1 e2
    then constant (Bitvector.zeros (size_of e1))
    else binary bop e1 e2
  | Dba.Or ->
    if equal e1 e2 then e1
    else if is_zero e1 then e2
    else if is_zero e2 then e1
    else if is_max e1 then
      constant (Bitvector.max_ubv (size_of e1))
    else if is_max e2 then
      constant (Bitvector.max_ubv (size_of e2))
    else binary bop e1 e2
  | Dba.And ->
    if equal e1 e2 then e1
    else if is_zero e1 || is_zero e2 then
      constant (Bitvector.zeros (size_of e1))
    else if is_max e1 then e2
    else if is_max e2 then e1
    else binary bop e1 e2
  | Dba.Eq ->
    begin
      if equal e1 e2 then constant (Bitvector.one)
      else
        match e1, e2 with
        | Dba.ExprCst (`Constant, bv), e
        | e, Dba.ExprCst(`Constant, bv) ->
          if Bitvector.is_zero bv then lognot e
          else if Bitvector.is_one bv then e
          else binary bop e1 e2
        | _, _ -> binary bop e1 e2
    end
  | Dba.Diff ->
    begin
      if equal e1 e2 then constant (Bitvector.zero)
      else
        match e1, e2 with
        | Dba.ExprCst (`Constant, bv), e when Bitvector.is_zero bv -> e
        | Dba.ExprCst (`Constant, bv), e when Bitvector.is_one bv -> lognot e
        | e, Dba.ExprCst (`Constant, bv) when Bitvector.is_zero bv -> e
        | e, Dba.ExprCst (`Constant, bv) when Bitvector.is_one bv -> lognot e
        | _, _ -> binary bop e1 e2
    end
  | Dba.DivU
  | Dba.DivS
  | Dba.ModU
  | Dba.ModS
  | Dba.Concat
  | Dba.LShift
  | Dba.RShiftU
  | Dba.RShiftS
  | Dba.LeftRotate
  | Dba.RightRotate
  | Dba.LeqU
  | Dba.LtU
  | Dba.GeqU
  | Dba.GtU
  | Dba.LeqS
  | Dba.LtS
  | Dba.GeqS
  | Dba.GtS -> binary bop e1 e2


let rec simplify_expr expr : Dba.expr =
  match expr with
  | Dba.ExprLoad (size, endian, e) ->
    Dba.ExprLoad (size, endian, simplify_expr e)
  | Dba.ExprUnary (uop, e) ->
    begin
      let e = simplify_expr e in
      match uop with
      | Dba.UMinus ->
        if Dba_types.Expr.is_zero e then e else expr
      | Dba.Not ->
        begin
          match e with
          | Dba.ExprCst (`Constant, b) when Bitvector.size_of b = 1 ->
            let b = Bitvector.value_of b in
            if (Bigint.eq_big_int b Bigint.zero_big_int)
            then (Dba.ExprCst (`Constant, Bitvector.one))
            else (Dba.ExprCst (`Constant, Bitvector.zero))
          | _ -> expr
        end
    end
  | Dba.ExprBinary (bop, e1, e2) ->
    let e1 = simplify_expr e1 in
    let e2 = simplify_expr e2 in
    simplify_bop bop e1 e2 
  | Dba.ExprRestrict (e, i, j) ->
    begin
      let e = simplify_expr e in
      match e with
      | Dba.ExprCst (`Constant, bv) ->
        let bi = Bitvector.value_of bv in
        let bv = Bitvector.create (Bigint.extract_big_int bi i (j - i + 1)) (j - i + 1) in
        Dba.ExprCst (`Constant, bv)
      | _ -> Dba.ExprRestrict (e, i, j)
    end
  | Dba.ExprExtU (e, size) ->
    begin
      let e = simplify_expr e in
      match e with
      | Dba.ExprCst (r, bi) ->
        let bi = Bitvector.value_of bi in
        Dba.ExprCst (r, Bitvector.create bi size)
      | _ -> Dba.ExprExtU (e, size)
    end
  | Dba.ExprExtS (e,size) ->
    begin
      let e = simplify_expr e in
      match e with
      | Dba.ExprCst (`Constant, bi) ->
        Dba.ExprCst (`Constant, (Bitvector.extend_signed bi size))
      | _ -> Dba.ExprExtS (e, size)
    end
  | Dba.ExprIte (cond, e1, e2) ->
    Dba.ExprIte (cond, simplify_expr e1, simplify_expr e2)
  | Dba.ExprCst (_, _)
  | Dba.ExprVar (_, _, _) -> expr
  | Dba.ExprAlternative (_) -> failwith "dba.ml: simplify_expr Alternative"



and resolve_idioms_lhs lhs = lhs

and resolve_idioms_cond cond  =
  match cond with
    Dba.CondReif e -> Dba.CondReif (simplify_expr e)
  | Dba.CondNot cond -> Dba.CondNot (resolve_idioms_cond cond)
  | Dba.CondAnd (cond1, cond2) ->
    Dba.CondAnd (resolve_idioms_cond cond1, resolve_idioms_cond cond2)
  | Dba.CondOr (cond1, cond2) ->
    Dba.CondOr (resolve_idioms_cond cond1, resolve_idioms_cond cond2)
  | Dba.True -> Dba.True
  | Dba.False -> Dba.False


let simplify_instruction ik =
  match ik with
  | Dba.IkAssign (lhs, e, id) ->
    Dba.IkAssign (resolve_idioms_lhs lhs, simplify_expr e, id)
  | Dba.IkDJump (e, tag) -> Dba.IkDJump (simplify_expr e, tag)
  | Dba.IkIf (cond, id1, id2) ->
    let cond = resolve_idioms_cond cond |> simplify_dbacond in
    Dba.IkIf(cond, id1, id2)
  | Dba.IkSJump _
  | Dba.IkPrint (_, _)
  | Dba.IkAssume (_, _)
  | Dba.IkAssert (_, _)
  | Dba.IkMalloc (_, _, _)
  | Dba.IkFree (_, _)
  | Dba.IkUndef (_, _)
  | Dba.IkNondet (_, _, _)
  | Dba.IkStop (_)
  | Dba.IkNondetAssume (_, _, _) -> ik

