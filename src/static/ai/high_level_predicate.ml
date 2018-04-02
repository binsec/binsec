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

exception Not_constant_condition

type t = (Dba.expr * Dba.vartag option) Basic_types.String.Map.t option

let empty = Some Basic_types.String.Map.empty

let bottom = None

let rec is_equal_expr e1 e2 =
  match e1, e2 with
  | Dba.ExprVar (name1, s1, tag1), Dba.ExprVar (name2, s2, tag2) ->
    name1 = name2 && (s1 = s2) && (is_equal_vartag_option tag1 tag2)
  | Dba.ExprLoad (s1, end1, exp1), Dba.ExprLoad (s2, end2, exp2) ->
    (s1 = s2) && (end1 = end2) && (is_equal_expr exp1 exp2)
  | Dba.ExprCst (r1, b1), Dba.ExprCst (r2, b2) ->
    let size1 = Bitvector.size_of b1 in
    let b1 = Bitvector.value_of b1 in
    let size2 = Bitvector.size_of b2 in
    let b2 = Bitvector.value_of b2 in
    (Region_bitvector.region_equal r1 r2) && (Bigint.eq_big_int b1 b2)
    && (size1 = size2)
  | Dba.ExprUnary (uop1, exp1), Dba.ExprUnary (uop2, exp2) ->
    (uop1 = uop2) && (is_equal_expr exp1 exp2)
  | Dba.ExprBinary (bop1, exp11, exp12),
    Dba.ExprBinary (bop2, exp21, exp22) ->
    (bop1 = bop2) &&
    (is_equal_expr exp11 exp21) && (is_equal_expr exp12 exp22)
  | Dba.ExprRestrict (exp1, o11, o12),
    Dba.ExprRestrict (exp2, o21, o22) ->
    (o11 = o21) && (o12 = o22) && (is_equal_expr exp1 exp2)
  | Dba.ExprExtU (exp1, s1), Dba.ExprExtU (exp2, s2) ->
    (s1 = s2) && (is_equal_expr exp1 exp2)
  | Dba.ExprExtS (exp1, s1), Dba.ExprExtS (exp2, s2) ->
    (s1 = s2) && (is_equal_expr exp1 exp2)
  | Dba.ExprIte (c1, exp11, exp12), Dba.ExprIte (c2, exp21, exp22) ->
    (is_equal_cond c1 c2) &&
    (is_equal_expr exp11 exp21) && (is_equal_expr exp12 exp22)
  | Dba.ExprAlternative (_exp_list1, _tag1),
    Dba.ExprAlternative (_exp_list2, _tag2) ->
    failwith "is_egal Dba.ExprAlternative"
  | _, _ -> false

and is_equal_cond e1 e2 =
  match e1, e2 with
  | Dba.CondReif exp1, Dba.CondReif exp2 -> (is_equal_expr exp1 exp2)
  | Dba.CondNot c1, Dba.CondNot c2 -> (is_equal_cond c1 c2)
  | Dba.CondAnd (c11, c12), Dba.CondAnd (c21, c22) ->
    (is_equal_cond c11 c21) && (is_equal_cond c12 c22)
  | Dba.CondOr (c11, c12), Dba.CondOr (c21, c22) ->
    (is_equal_cond c11 c21) && (is_equal_cond c12 c22)
  | Dba.True, Dba.True -> true
  | Dba.False, Dba.False -> true
  | _, _ -> false

and is_equal_vartag_option tag1 tag2 =
  match tag1, tag2 with
  | None, None -> true
  | None, Some _
  | Some _, None -> false
  | Some t1, Some t2 ->
    match t1, t2 with
    | Dba.Flag c1, Dba.Flag c2 -> is_equal_flag c1 c2
    | Dba.Temp, Dba.Temp -> true
    | _, _ -> false

and is_equal_flag c1 c2 =
  match c1, c2 with
  | Dba.FlgCmp (e11, e12), Dba.FlgCmp (e21, e22) ->
    (is_equal_expr e11 e21) && (is_equal_expr e12 e22)
  | Dba.FlgSub (e11, e12), Dba.FlgSub (e21, e22) ->
    (is_equal_expr e11 e21) && (is_equal_expr e12 e22)
  | Dba.FlgTest (e11, e12), Dba.FlgTest (e21, e22) ->
    (is_equal_expr e11 e21) && (is_equal_expr e12 e22)
  | Dba.FlgUnspecified, Dba.FlgUnspecified -> true
  | _, _ -> false

let update_flags lhs expr flags =
  let flags_set =
    match flags with
    | None -> Basic_types.String.Map.empty
    | Some flgs -> flgs
  in
  match lhs with
  | Dba.LhsVar (name, _sz, (Some (Dba.Flag _) as tag)) ->
    Some (Basic_types.String.Map.add name (expr, tag) flags_set)
  | Dba.LhsVar (name, _sz, Some Dba.Temp)     ->
    Some (Basic_types.String.Map.add name (expr, Some Dba.Temp) flags_set)
  | Dba.LhsVar _
  | Dba.LhsVarRestrict _
  | Dba.LhsStore _ -> flags




let rec substitute_loads cond op load_op =
  match cond with
  | Dba.CondReif e ->
    let e = substitute_loads_in_expr e op load_op in
    Dba.CondReif e
  | Dba.CondNot c ->
    let c = substitute_loads c op load_op in
    Dba.CondNot c
  | Dba.CondAnd (c1, c2) ->
    let c1 = substitute_loads c1 op load_op in
    let c2 = substitute_loads c2 op load_op in
    Dba.CondAnd (c1, c2)
  | Dba.CondOr  (c1, c2) ->
    let c1 = substitute_loads c1 op load_op in
    let c2 = substitute_loads c2 op load_op in
    Dba.CondOr (c1, c2)
  | Dba.True
  | Dba.False as cond -> cond


and substitute_loads_in_expr expr op load_op =
  match expr with
  | Dba.ExprVar (_name, _size, _) -> expr
  | Dba.ExprLoad (sz, endianness, e) ->
    if is_equal_expr op expr then load_op
    else let e = substitute_loads_in_expr e op load_op in
      Dba.ExprLoad (sz, endianness, e)
  | Dba.ExprCst (_a, _b) -> expr
  | Dba.ExprUnary (unop, e) ->
    let e = substitute_loads_in_expr e op load_op in
    Dba.ExprUnary (unop, e)
  | Dba.ExprBinary (binop, e1, e2)       ->
    let e1 = substitute_loads_in_expr e1 op load_op in
    let e2 = substitute_loads_in_expr e2 op load_op in
    Dba.ExprBinary (binop, e1, e2)
  | Dba.ExprRestrict (e, o1, o2)        ->
    let e = substitute_loads_in_expr e op load_op in
    Dba.ExprRestrict (e, o1, o2)
  | Dba.ExprExtU (e, sz)                ->
    let e = substitute_loads_in_expr e op load_op in
    Dba.ExprExtU (e, sz)
  | Dba.ExprExtS (e, sz)                ->
    let e = substitute_loads_in_expr e op load_op in
    Dba.ExprExtS (e, sz)
  | Dba.ExprIte  (c, e1, e2)            ->
    let e1 = substitute_loads_in_expr e1 op load_op in
    let e2 = substitute_loads_in_expr e2 op load_op in
    let c  = substitute_loads c op load_op in
    Dba.ExprIte  (c, e1, e2)
  | Dba.ExprAlternative (_, _)          -> expr

let hide_loads cond op1 op2 =
  match op1, op2 with
  | Dba.ExprLoad (s1, _end1, _exp1), Dba.ExprLoad (s2, _end2, _exp2) ->
    let load_op1 = Dba.ExprVar ("load_op1", s1 * 8, Some Dba.Temp) in
    let load_op2 = Dba.ExprVar ("load_op2", s2 * 8, Some Dba.Temp) in
    let cond = substitute_loads cond op1 load_op1 in
    let cond = substitute_loads cond op2 load_op2 in
    cond, load_op1, load_op2
  | Dba.ExprLoad (s1, _end1, _exp1), _ ->
    let load_op1 = Dba.ExprVar ("load_op1", s1 * 8, Some Dba.Temp) in
    let cond = substitute_loads cond op1 load_op1 in
    cond, load_op1, op2
  | _, Dba.ExprLoad (s2, _end2, _exp2) ->
    let load_op2 = Dba.ExprVar ("load_op2", s2 * 8, Some Dba.Temp) in
    let cond = substitute_loads cond op2 load_op2 in
    cond, op1, load_op2
  | _, _ -> cond, op1, op2


let rec recover_loads_in_expr expr load_op1 load_op2 op1 op2 =
  match expr with
  | Dba.ExprVar (_name, _size, _) ->
    if is_equal_expr expr load_op1 then op1
    else if is_equal_expr expr load_op2 then op2
    else expr
  | Dba.ExprLoad _
  | Dba.ExprAlternative _
  | Dba.ExprCst _ -> expr
  | Dba.ExprUnary (unop, e) ->
    let e = recover_loads_in_expr e load_op1 load_op2 op1 op2 in
    Dba.ExprUnary (unop, e)
  | Dba.ExprBinary (binop, e1, e2)       ->
    let e1 = recover_loads_in_expr e1 load_op1 load_op2 op1 op2 in
    let e2 = recover_loads_in_expr e2 load_op1 load_op2 op1 op2 in
    Dba.ExprBinary (binop, e1, e2)
  | Dba.ExprRestrict (e, o1, o2)        ->
    let e = recover_loads_in_expr e load_op1 load_op2 op1 op2 in
    Dba.ExprRestrict (e, o1, o2)
  | Dba.ExprExtU (e, sz)                ->
    let e = recover_loads_in_expr e load_op1 load_op2 op1 op2 in
    Dba.ExprExtU (e, sz)
  | Dba.ExprExtS (e, sz)                ->
    let e = recover_loads_in_expr e load_op1 load_op2 op1 op2 in
    Dba.ExprExtS (e, sz)
  | Dba.ExprIte  (c, e1, e2)            ->
    let e1 = recover_loads_in_expr e1 load_op1 load_op2 op1 op2 in
    let e2 = recover_loads_in_expr e2 load_op1 load_op2 op1 op2 in
    let c  = recover_loads_in_cond c load_op1 load_op2 op1 op2 in
    Dba.ExprIte  (c, e1, e2)


and recover_loads_in_cond cond load_op1 load_op2 op1 op2 =
  match cond with
  | Dba.CondReif e ->
    let e = recover_loads_in_expr e load_op1 load_op2 op1 op2 in
    Dba.CondReif e
  | Dba.CondNot c ->
    let c = recover_loads_in_cond c load_op1 load_op2 op1 op2 in
    Dba.CondNot c
  | Dba.CondAnd (c1, c2) ->
    let c1 = recover_loads_in_cond c1 load_op1 load_op2 op1 op2 in
    let c2 = recover_loads_in_cond c2 load_op1 load_op2 op1 op2 in
    Dba.CondAnd (c1, c2)
  | Dba.CondOr  (c1, c2) ->
    let c1 = recover_loads_in_cond c1 load_op1 load_op2 op1 op2 in
    let c2 = recover_loads_in_cond c2 load_op1 load_op2 op1 op2 in
    Dba.CondOr (c1, c2)
  | Dba.True  -> Dba.True
  | Dba.False -> Dba.False



let rec replace_operands_by_csts cond op1 op2 v1 v2 =
  match cond with
  | Dba.CondReif e ->
    let e =  replace_operands_by_csts_in_expr e op1 op2 v1 v2 in
    Dba.CondReif e
  | Dba.CondNot c ->
    let c = replace_operands_by_csts c op1 op2 v1 v2 in
    Dba.CondNot c
  | Dba.CondAnd (c1, c2) ->
    let c1 = replace_operands_by_csts c1 op1 op2 v1 v2 in
    let c2 = replace_operands_by_csts c2 op1 op2 v1 v2 in
    Dba.CondAnd (c1, c2)
  | Dba.CondOr  (c1, c2) ->
    let c1 = replace_operands_by_csts c1 op1 op2 v1 v2 in
    let c2 = replace_operands_by_csts c2 op1 op2 v1 v2 in
    Dba.CondOr (c1, c2)
  | Dba.True  -> Dba.True
  | Dba.False -> Dba.False

and replace_operands_by_csts_in_expr expr op1 op2 v1 v2 =
  match expr with
    Dba.ExprVar (_name, _size, _) ->
    if (is_equal_expr expr op1) then v1
    else if (is_equal_expr expr op2) then v2
    else expr
  | Dba.ExprLoad (_sz, _endianness, _e) ->
    if (is_equal_expr expr op1) then v1
    else if (is_equal_expr expr op2) then v2
    else expr
  | Dba.ExprCst (_, _) ->  expr
  | Dba.ExprUnary (unop, e) ->
    let e = replace_operands_by_csts_in_expr e op1 op2 v1 v2 in
    Dba.ExprUnary (unop, e)
  | Dba.ExprBinary (binop, e1, e2)       ->
    let e1 = replace_operands_by_csts_in_expr e1 op1 op2 v1 v2 in
    let e2 = replace_operands_by_csts_in_expr e2 op1 op2 v1 v2 in
    Dba.ExprBinary (binop, e1, e2)
  | Dba.ExprRestrict (e, o1, o2)        ->
    if (is_equal_expr expr op1) then v1
    else if (is_equal_expr expr op2) then v2
    else
      let e = replace_operands_by_csts_in_expr e op1 op2 v1 v2 in
      Dba.ExprRestrict (e, o1, o2)
  | Dba.ExprExtU (e, sz)                ->
    let e = replace_operands_by_csts_in_expr e op1 op2 v1 v2 in
    Dba.ExprExtU (e, sz)
  | Dba.ExprExtS (e, sz)                ->
    let e = replace_operands_by_csts_in_expr e op1 op2 v1 v2 in
    Dba.ExprExtS (e, sz)
  | Dba.ExprIte  (c, e1, e2)            ->
    let e1 = replace_operands_by_csts_in_expr e1 op1 op2 v1 v2 in
    let e2 = replace_operands_by_csts_in_expr e2 op1 op2 v1 v2 in
    let c  = replace_operands_by_csts c op1 op2 v1 v2 in
    Dba.ExprIte  (c, e1, e2)
  | Dba.ExprAlternative (_, _)          -> expr


let is_operand e =
  match e with
    Dba.ExprVar _ -> true
  | Dba.ExprCst _ -> true
  | Dba.ExprLoad _ -> true
  | Dba.ExprRestrict (Dba.ExprVar _, _, _) -> true
  | Dba.ExprRestrict (Dba.ExprLoad _, _, _) -> true
  | _ -> false

let rec is_natural_cond cond =
  match cond with
  | Dba.CondReif e -> is_natural_expr e
  | Dba.True | Dba.False -> true
  | _ -> false

and is_natural_expr e =
  match e with
  | Dba.ExprBinary (_binop, e1, e2) -> (is_operand e1) && (is_operand e2)
  | _ -> false


let can_be_equal op =
  match op with
  | Dba.LeqU | Dba.GeqU | Dba.LeqS | Dba.GeqS | Dba.Eq -> true
  | Dba.LtU | Dba.GtU | Dba.LtS | Dba.GtS | Dba.Diff -> false
  | _ -> true

let can_not_be_equal op =
  match op with
  | Dba.LeqU | Dba.GeqU | Dba.LeqS | Dba.GeqS | Dba.Eq -> false
  | Dba.LtU | Dba.GtU | Dba.LtS | Dba.GtS | Dba.Diff -> true
  | _ -> true

let can_be_lower_same_sign operand op =
  match operand with
    Dba.ExprCst (`Constant, bv) ->
    let size = Bitvector.size_of bv in
    let bv_max = Bitvector.max_ubv size in
    let bv_smax = Bitvector.max_sbv size in
    if (Bitvector.equal bv bv_smax) || (Bitvector.equal bv bv_max) then true
    else (
      match op with
      | Dba.LeqU | Dba.LtU | Dba.LeqS | Dba.LtS -> true
      | Dba.GeqU | Dba.GtU | Dba.GeqS | Dba.GtS -> false
      | _ -> true
    )
  | _ -> failwith "high_level_predicate.ml: not constant operand in condition 2"


let can_not_be_lower_same_sign operand op =
  match operand with
    Dba.ExprCst (`Constant, bv) ->
    let size = Bitvector.size_of bv in
    let bv_max = Bitvector.max_ubv size in
    let bv_smax = Bitvector.max_sbv size in
    if (Bitvector.equal bv bv_smax) || (Bitvector.equal bv bv_max) then true
    else (
      match op with
      | Dba.LeqU | Dba.LtU | Dba.LeqS | Dba.LtS -> false
      | Dba.GeqU | Dba.GtU | Dba.GeqS | Dba.GtS -> true
      | _ -> true
    )
  | _ -> failwith "high_level_predicate.ml: not constant operand in condition 2"


let can_be_lower_diff_sign operand op =
  match operand with
  |  Dba.ExprCst (`Constant, bv) ->
    begin
      let size = Bitvector.size_of bv in
      let bv_smax = Bitvector.max_sbv size in
      if Bitvector.ugt bv bv_smax then
        match op with
        | Dba.LeqS | Dba.LtS | Dba.GeqU | Dba.GtU -> true
        | Dba.LeqU | Dba.LtU | Dba.GeqS | Dba.GtS -> false
        | _ -> true
      else
        match op with
        | Dba.LeqS | Dba.LtS | Dba.GeqU | Dba.GtU -> false
        | Dba.LeqU | Dba.LtU | Dba.GeqS | Dba.GtS -> true
        | _ -> true
    end
  | _ -> failwith "high_level_predicate.ml: not constant operand in condition 3"

let can_not_be_lower_diff_sign operand op =
  match operand with
    Dba.ExprCst (`Constant, bv) ->
    let size = Bitvector.size_of bv in
    let bv_smax = Bitvector.max_sbv size in
    if (Bitvector.ule bv bv_smax) then (
      match op with
      | Dba.LeqS | Dba.LtS | Dba.GeqU | Dba.GtU -> true
      | Dba.LeqU | Dba.LtU | Dba.GeqS | Dba.GtS -> false
      | _ -> true
    )
    else (
      match op with
      | Dba.LeqS | Dba.LtS | Dba.GeqU | Dba.GtU -> false
      | Dba.LeqU | Dba.LtU | Dba.GeqS | Dba.GtS -> true
      | _ -> true
    )
  | _ -> failwith "high_level_predicate.ml: not constant operand in condition 4"


let rec eval_cond_csts cond =
  match cond with
  | Dba.CondReif e ->
    let bv = eval_expr_csts e in
    (match Bitvector.value_of bv, Bitvector.size_of bv with
     | (b, 1) when Bigint.eq_big_int b Bigint.zero_big_int -> false
     | (b, 1) when Bigint.eq_big_int b Bigint.unit_big_int -> true
     | _ ->
       let rbv = (`Value (`Constant, bv)) in
       raise (Errors.Bad_condition (Region_bitvector.to_string rbv))
    )
  | Dba.CondNot c -> not (eval_cond_csts c)
  | Dba.CondAnd (c1, c2) -> (eval_cond_csts c1) && (eval_cond_csts c2)
  | Dba.CondOr  (c1, c2) -> (eval_cond_csts c1) || (eval_cond_csts c2)
  | Dba.True  -> true
  | Dba.False -> false

and eval_expr_csts expr =
  match expr with
  | Dba.ExprVar (_name, _size, _) -> raise Not_constant_condition
  | Dba.ExprLoad (_sz, _endianness, _e) -> raise Not_constant_condition
  | Dba.ExprCst (_, v) -> v
  | Dba.ExprUnary (uop, e) -> (
      match uop with
        Dba.UMinus -> Bitvector.neg (eval_expr_csts e)
      | Dba.Not -> Bitvector.lognot (eval_expr_csts e)
    )
  | Dba.ExprBinary (bop, e1, e2) ->
    let v1 = (eval_expr_csts e1) in
    let v2 = (eval_expr_csts e2) in
    begin
      match bop with
      | Dba.Plus -> Bitvector.add v1 v2
      | Dba.Minus ->	Bitvector.sub v1 v2
      | Dba.MultU ->	Bitvector.umul v1 v2
      | Dba.MultS ->	Bitvector.smul v1 v2
      | Dba.DivU -> Bitvector.udiv v1 v2
      | Dba.DivS -> Bitvector.sdiv v1 v2
      | Dba.ModU -> Bitvector.umod v1 v2
      | Dba.ModS -> Bitvector.smod v1 v2
      | Dba.Or -> Bitvector.logor v1 v2
      | Dba.And -> Bitvector.logand v1 v2
      | Dba.Xor -> Bitvector.logxor v1 v2
      | Dba.Concat -> Bitvector.append v1 v2
      | Dba.LShift -> Bitvector.shift_left v1 (Bigint.int_of_big_int (Bitvector.value_of v2))
      | Dba.RShiftU -> Bitvector.shift_right v1 (Bigint.int_of_big_int (Bitvector.value_of v2))
      | Dba.RShiftS -> Bitvector.shift_right_signed v1 (Bigint.int_of_big_int (Bitvector.value_of v2))
      | Dba.LeftRotate -> Bitvector.rotate_left v1 (Bigint.int_of_big_int (Bitvector.value_of v2))
      | Dba.RightRotate -> Bitvector.rotate_right v1 (Bigint.int_of_big_int (Bitvector.value_of v2))
      | Dba.Eq  -> Bitvector.equal v1 v2 |> Bitvector.of_bool
      | Dba.Diff -> Bitvector.diff v1 v2 |> Bitvector.of_bool
      | Dba.LeqU -> Bitvector.ule v1 v2 |> Bitvector.of_bool
      | Dba.LtU  -> Bitvector.ult v1 v2 |> Bitvector.of_bool
      | Dba.GeqU -> Bitvector.uge v1 v2 |> Bitvector.of_bool
      | Dba.GtU  -> Bitvector.ugt v1 v2 |> Bitvector.of_bool
      | Dba.LeqS -> Bitvector.sle v1 v2 |> Bitvector.of_bool
      | Dba.LtS  -> Bitvector.slt v1 v2 |> Bitvector.of_bool
      | Dba.GeqS -> Bitvector.sge v1 v2 |> Bitvector.of_bool
      | Dba.GtS  -> Bitvector.sgt v1 v2 |> Bitvector.of_bool
    end
  | Dba.ExprRestrict (e, offset1, offset2) ->
    let v = (eval_expr_csts e) in
    Bitvector.extract v offset1 offset2
  | Dba.ExprExtU (e, size) ->
    Bitvector.extend (eval_expr_csts e) size
  | Dba.ExprExtS (e, size) ->
    Bitvector.extend_signed (eval_expr_csts e) size
  | Dba.ExprIte (cond, e1, e2) ->
    if (eval_cond_csts cond)
    then (eval_expr_csts e1)
    else (eval_expr_csts e2)
  | Dba.ExprAlternative (alternatives, _tag) ->
    Dba_utils.eval_alternatives eval_expr_csts Bitvector.equal alternatives
    (* let eval alternatives =
     *   match l with
     *   | [] -> assert false
     *   | [e] -> eval_expr_csts e
     *   | e1 :: alts ->
     *     let v1 = eval_expr_csts e1 in
     *     if List.map eval_expr_csts alts |> List.for_all (Bitvector.equal v1)
     *     then v1
     *     else raise Alternative_conflict_values
     * in eval alternatives *)


let substitute_eval_cond_csts cond op1 op2 v1 v2 =
  Display.display (Display.CondReplacement (cond, op1, op2, v1, v2, "before replacement"));
  let cond = replace_operands_by_csts cond op1 op2 v1 v2 in
  Display.display (Display.CondReplacement (cond, op1, op2, v1, v2, "after replacement"));
  eval_cond_csts cond


let is_cst op =
  match op with
  | Dba.ExprCst _ -> true
  | _ -> false

let max_same_sign op =
  match op with
    Dba.ExprCst (`Constant, bv) ->
    let size = Bitvector.size_of bv in
    let bv_max = Bitvector.max_ubv size in
    let bv_smax = Bitvector.max_sbv size in
    let max = Dba.ExprCst (`Constant, bv_max) in
    let smax = Dba.ExprCst (`Constant, bv_smax) in
    if (Bitvector.ule bv bv_smax) then smax
    else max
  | _ -> failwith "high_level_predicate.ml: not constant operand in condition"



let max_diff_sign op =
  match op with
    Dba.ExprCst (`Constant, bv) ->
    let size = Bitvector.size_of bv in
    let bv_max = Bitvector.max_ubv size in
    let bv_smax = Bitvector.max_sbv size in
    let max = Dba.ExprCst (`Constant, bv_max) in
    let smax = Dba.ExprCst (`Constant, bv_smax) in
    if (Bitvector.ule bv bv_smax) then max
    else smax
  | _ -> failwith "high_level_predicate.ml: not constant operand in condition"

let bootstrap_predicates predicates cond op1 op2 =
  let size = Dba_utils.computesize_dbaexpr op1 in
  let one = Dba.ExprCst (`Constant, Bitvector.ones size) in
  let zero = Dba.ExprCst (`Constant, Bitvector.zeros size) in
  let max = Dba.ExprCst (`Constant, Bitvector.max_ubv size) in
  if (is_cst op1) && (is_cst op2)
  then predicates
  else
    let predicates =
      let c_equal =
        if not (is_cst op1) && not (is_cst op2)
        then substitute_eval_cond_csts cond op1 op2 zero zero
        else if (is_cst op1)
        then substitute_eval_cond_csts cond op1 op2 op1 op1
        else substitute_eval_cond_csts cond op1 op2 op2 op2
      in
      if c_equal then List.filter can_be_equal predicates
      else List.filter can_not_be_equal predicates
    in
    Display.display (Display.Predicates predicates);
    let predicates =
      let operand, c_lower_same_sign =
        if not (is_cst op1) && not (is_cst op2)
        then zero, substitute_eval_cond_csts cond op1 op2 zero one
        else if (is_cst op1)
        then op1, substitute_eval_cond_csts cond op1 op2 op1 (max_same_sign op1)
        else op2, (not (substitute_eval_cond_csts cond op1 op2 (max_same_sign op2) op2))
      in
      if c_lower_same_sign then List.filter (can_be_lower_same_sign operand) predicates
      else List.filter (can_not_be_lower_same_sign operand) predicates
    in
    Display.display (Display.Predicates predicates);
    let predicates =
      let operand, c_lower_diff_sign =
        if not (is_cst op1) && not (is_cst op2)
        then zero, substitute_eval_cond_csts cond op1 op2 zero max
        else if (is_cst op1)
        then op1, substitute_eval_cond_csts cond op1 op2 op1 (max_diff_sign op1)
        else op2, (not (substitute_eval_cond_csts cond op1 op2 (max_diff_sign op2) op2))
      in
      if c_lower_diff_sign then List.filter (can_be_lower_diff_sign operand) predicates
      else List.filter (can_not_be_lower_diff_sign operand) predicates
    in
    Display.display (Display.Predicates predicates);
    predicates


let rec substitute_flags_in_epxr expr flags acc =
  match expr with
    Dba.ExprVar (name, _size, _) ->
    if Basic_types.String.Map.mem name flags then
      let e, tag = Basic_types.String.Map.find name flags in
      let e, acc = substitute_flags_in_epxr e flags acc in
      e, tag :: acc
    else expr, acc
  | Dba.ExprLoad  _
  | Dba.ExprCst _ -> expr, acc
  | Dba.ExprUnary (unop, e)             ->
    let e, acc = substitute_flags_in_epxr e flags acc in
    Dba.ExprUnary (unop, e), acc
  | Dba.ExprBinary (binop, e1, e2)       ->
    let e1, acc = substitute_flags_in_epxr e1 flags acc in
    let e2, acc = substitute_flags_in_epxr e2 flags acc in
    Dba.ExprBinary (binop, e1, e2), acc
  | Dba.ExprRestrict (e, o1, o2)        ->
    let e, acc = substitute_flags_in_epxr e flags acc in
    Dba.ExprRestrict (e, o1, o2), acc
  | Dba.ExprExtU (e, sz)                ->
    let e, acc = substitute_flags_in_epxr e flags acc in
    Dba.ExprExtU (e, sz), acc
  | Dba.ExprExtS (e, sz)                ->
    let e, acc = substitute_flags_in_epxr e flags acc in
    Dba.ExprExtS (e, sz), acc
  | Dba.ExprIte  (c, e1, e2)            ->
    let e1, acc = substitute_flags_in_epxr e1 flags acc in
    let e2, acc = substitute_flags_in_epxr e2 flags acc in
    let c, acc  = substitute_flags c flags acc in
    Dba.ExprIte  (c, e1, e2), acc
  | Dba.ExprAlternative (_, _)          -> expr, acc


and apply_cmp_pattern operands c cond =
  match operands with
    None            -> incr Options.nb_failed_nat_predicate_recoveries; cond
  | Some (op1, op2) ->
    incr Options.nb_recovered_nat_predicates;
    match c with
    | Dba.CondAnd(Dba.CondNot(Dba.CondReif(Dba.ExprVar("CF",1, _))),
                  Dba.CondNot(Dba.CondReif(Dba.ExprVar("ZF",1, _)))) ->
      Dba.CondReif (Dba.ExprBinary (Dba.GtU, op1, op2))

    | Dba.CondNot(Dba.CondReif(Dba.ExprVar ("CF", 1, _))) ->
      Dba.CondReif (Dba.ExprBinary (Dba.GeqU, op1, op2))

    | Dba.CondReif(Dba.ExprVar ("CF", 1, _)) ->
      Dba.CondReif (Dba.ExprBinary (Dba.LtU, op1, op2))

    | Dba.CondOr (Dba.CondReif(Dba.ExprVar ("CF", 1, _)),
                  Dba.CondReif(Dba.ExprVar ("ZF", 1, _))) ->
      Dba.CondReif (Dba.ExprBinary (Dba.GeqU, op1, op2))

    | Dba.CondReif(Dba.ExprVar ("ZF", 1, _)) ->
      Dba.CondReif (Dba.ExprBinary (Dba.Eq, op1, op2))

    | Dba.CondNot(Dba.CondReif(Dba.ExprVar ("ZF", 1, _))) ->
      Dba.CondReif (Dba.ExprBinary (Dba.Diff, op1, op2))

    | Dba.CondAnd (Dba.CondNot(Dba.CondReif(Dba.ExprVar ("ZF", 1, _))),
                   (Dba.CondReif(
                       Dba.ExprBinary(Dba.Eq,
                                      Dba.ExprVar ("SF", 1, _),
                                      Dba.ExprVar ("OF", 1, _))))) ->
      Dba.CondReif (Dba.ExprBinary (Dba.GtS, op1, op2))


    | Dba.CondReif(Dba.ExprBinary(Dba.Eq,
                                  Dba.ExprVar ("SF", 1, _),
                                  Dba.ExprVar ("OF", 1, _))) ->
      Dba.CondReif (Dba.ExprBinary (Dba.GeqS, op1, op2))

    | Dba.CondReif(Dba.ExprBinary(Dba.Diff,
                                  Dba.ExprVar ("SF", 1, _),
                                  Dba.ExprVar ("OF", 1, _))) ->
      Dba.CondReif (Dba.ExprBinary (Dba.LtS, op1, op2))

    | Dba.CondOr (Dba.CondReif(Dba.ExprVar ("ZF", 1, _)),
                  (Dba.CondReif(
                      Dba.ExprBinary(Dba.Diff,
                                     Dba.ExprVar ("SF", 1, _),
                                     Dba.ExprVar ("OF", 1, _))))) ->
      Dba.CondReif (Dba.ExprBinary (Dba.LeqS, op1, op2))
    | _ ->
      Options.nb_recovered_nat_predicates := !Options.nb_recovered_nat_predicates - 1;
      incr Options.nb_failed_nat_predicate_recoveries; cond


and check_size_operands op1 op2 =
  let s1 = Dba_utils.computesize_dbaexpr op1 in
  let s2 = Dba_utils.computesize_dbaexpr op2 in
  if s1 = s2 then op1, op2
  else if s1 < s2 then
    op1, Dba.ExprRestrict (op2, 0, s1 - 1)
  else
    Dba.ExprRestrict (op1, 0, s2 - 1), op2


and retrieve_comparison c flags addr rcd_conds =
  match flags with
    None ->
    incr Options.nb_failed_nat_predicate_recoveries;
    let cond, _tags = substitute_flags c Basic_types.String.Map.empty [] in
    cond, rcd_conds
  | Some flgs ->
    let cond, tags = substitute_flags c flgs [] in
    try
      let pred_cond, pred_nat_cond = Dba_types.Caddress.Map.find addr rcd_conds in
      if is_equal_cond cond pred_cond
      then (
        incr Options.nb_conditional_cache_uses;
        pred_nat_cond, rcd_conds
      )
      else raise Not_found
    with Not_found ->
      if (is_natural_cond cond) then (
        Display.display (Display.CondNat (addr, cond, "by propagation only"));
        let rcd_conds = Dba_types.Caddress.Map.add addr (cond, cond) rcd_conds in
        cond, rcd_conds
      )
      else if Ai_options.X86FlagPatterns.get () then
        let operands = operands_of_cmp tags in
        let compteur_simpl = Unix.gettimeofday () in
        let nat_cond = apply_cmp_pattern operands c cond in
        let rcd_conds = Dba_types.Caddress.Map.add addr (cond, nat_cond) rcd_conds in
        Options.time_nat_flag_recovery :=
          !Options.time_nat_flag_recovery +. (Unix.gettimeofday() -. compteur_simpl);
        nat_cond, rcd_conds
      else
        let operands = operands_of_cond cond [] in
        match operands with
        | op1 :: op2 :: _ ->
          begin
            let op1, op2 = check_size_operands op1 op2 in
            let temp_cond, temp_op1, temp_op2 = hide_loads cond op1 op2 in
            let predicates =
              let open Dba in
              [ Eq; Diff;
                LeqU; LtU; GeqU; GtU;
                LeqS; LtS; GeqS; GtS]
            in
            Display.display (Display.CondReplacement (cond, temp_op1, temp_op2, temp_op1, temp_op2, "boot"));
            let compteur_simpl = Unix.gettimeofday () in
            let predicates = bootstrap_predicates predicates cond op1 op2 in
            let predicate =
              Normalize_predicate.apply_smt_natural_cond_recovery predicates temp_cond temp_op1 temp_op2 in
            Options.time_nat_flag_recovery :=
              !Options.time_nat_flag_recovery +. (Unix.gettimeofday() -. compteur_simpl);
            match predicate with
            | None ->
              Logger.debug "Predicate = %a"
                Dba_printer.Ascii.pp_cond temp_cond;
              incr Options.nb_failed_nat_predicate_recoveries;
              let rcd_conds = Dba_types.Caddress.Map.add addr (cond, cond) rcd_conds in
              cond, rcd_conds
            | Some expr ->
              let expr = recover_loads_in_expr expr temp_op1 temp_op2 op1 op2 in
              incr Options.nb_recovered_nat_predicates;
              let nat_cond = Dba.CondReif expr in
              Display.display (Display.CondNat (addr, nat_cond, ""));
              let rcd_conds = Dba_types.Caddress.Map.add addr (cond, nat_cond) rcd_conds in
              nat_cond, rcd_conds
          end
        | _ ->
          incr Options.nb_failed_nat_predicate_recoveries;
          let rcd_conds = Dba_types.Caddress.Map.add addr (cond, cond) rcd_conds in
          cond, rcd_conds

and operands_of_expr expr acc =
  match expr with
  | Dba.ExprVar (_name, _size, None) -> expr :: acc
  | Dba.ExprVar (_name, _size, _) -> acc
  | Dba.ExprLoad (_size, _endianness, _e) -> expr :: acc
  | Dba.ExprCst (_region, _bv) -> expr :: acc
  | Dba.ExprUnary (_unop, e) -> operands_of_expr e acc
  | Dba.ExprBinary (_binop, e1, e2) ->
    let acc = operands_of_expr e2 acc in
    operands_of_expr e1 acc
  | Dba.ExprRestrict (e, o1, o2) ->
    if o1 = o2 then operands_of_expr e acc
    else expr :: acc
  | Dba.ExprExtU (e, _)
  | Dba.ExprExtS (e, _) -> operands_of_expr e acc
  | Dba.ExprIte (_c, e1, e2) ->
    let acc = operands_of_expr e1 acc in
    operands_of_expr e2 acc
  | Dba.ExprAlternative (e_list, _tag) ->
    List.fold_left (fun acc e -> operands_of_expr e acc) acc e_list


and operands_of_cond cond acc =
  match cond with
  | Dba.CondReif e ->
    operands_of_expr e acc
  | Dba.CondNot c ->
    operands_of_cond c acc
  | Dba.CondAnd (c1, c2) ->
    let acc = operands_of_cond  c1 acc in
    operands_of_cond c2 acc
  | Dba.CondOr  (c1, c2) ->
    let acc = operands_of_cond c1 acc in
    operands_of_cond c2 acc
  | Dba.True  -> acc
  | Dba.False -> acc


and operands_of_cmp = function
  | Some Dba.Temp :: l | None :: l -> operands_of_cmp l
  | Some (Dba.Flag (Dba.FlgCmp (op1, op2))) :: _ -> Some (op1, op2)
  | _                 -> None

and substitute_flags cond flags acc =
  match cond with
  | Dba.CondReif e ->
    let expr, tags = substitute_flags_in_epxr e flags acc in
    let expr = Simplification_dba_instr.simplify_expr expr in
    Dba.CondReif expr, tags
  | Dba.CondNot c ->
    let c, acc = substitute_flags c flags acc in
    Dba.CondNot c, acc
  | Dba.CondAnd (c1, c2) ->
    let c1, acc = substitute_flags c1 flags acc in
    let c2, acc = substitute_flags c2 flags acc in
    Dba.CondAnd (c1, c2), acc
  | Dba.CondOr (c1, c2) ->
    let c1, acc = substitute_flags c1 flags acc in
    let c2, acc = substitute_flags c2 flags acc in
    Dba.CondOr (c1, c2), acc
  | Dba.True
  | Dba.False -> cond, acc



let join flags1 flags2 =
  match flags1, flags2 with
  | flg, None
  | None, flg -> flg
  | Some flg1, Some flg2 ->
    let flg =
      Basic_types.String.Map.merge (fun _ elem1 elem2 ->
          match elem1, elem2 with
          | None, None
          | None, Some _
          | Some _, None -> None
          | Some (e1, tag1) , Some (e2, tag2) ->
            if (is_equal_vartag_option tag1 tag2) && (is_equal_expr e1 e2)
            then Some (e1, tag1)
            else None
        ) flg1 flg2
    in
    Some flg


let leq flags1 flags2 =
  match flags1, flags2 with
  | None, _ -> true
  | Some _, None -> false
  | Some flgs1, Some flgs2 ->
    let predicate name (e1, tag1) =
      try
        let e2, tag2 =
          Basic_types.String.Map.find name flgs2 in
        is_equal_vartag_option tag1 tag2 && is_equal_expr e1 e2
      with Not_found -> false
    in Basic_types.String.Map.for_all predicate flgs1
