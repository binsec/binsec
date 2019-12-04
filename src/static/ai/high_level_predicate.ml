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

open Ai_options

exception Not_constant_condition

type t = (Dba.Expr.t * Dba.VarTag.t) Basic_types.String.Map.t option

let empty = Some Basic_types.String.Map.empty

let bottom = None

(* TODO : it's almost Equal.expr : remove duplicates *)
let rec is_equal_expr e1 e2 =
  match e1, e2 with
  | Dba.Expr.Var v1, Dba.Expr.Var v2 -> v1 = v2
  | Dba.Expr.Load (s1, end1, exp1), Dba.Expr.Load (s2, end2, exp2) ->
    (s1 = s2) && (end1 = end2) && (is_equal_expr exp1 exp2)
  | Dba.Expr.Cst (r1, b1), Dba.Expr.Cst (r2, b2) ->
    let size1 = Bitvector.size_of b1 in
    let b1 = Bitvector.value_of b1 in
    let size2 = Bitvector.size_of b2 in
    let b2 = Bitvector.value_of b2 in
    Region_bitvector.region_equal r1 r2 && (Bigint.eq_big_int b1 b2)
    && (size1 = size2)
  | Dba.Expr.Unary (uop1, exp1), Dba.Expr.Unary (uop2, exp2) ->
    uop1 = uop2 && is_equal_expr exp1 exp2
  | Dba.Expr.Binary (bop1, exp11, exp12),
    Dba.Expr.Binary (bop2, exp21, exp22) ->
    bop1 = bop2 &&
    (is_equal_expr exp11 exp21) && (is_equal_expr exp12 exp22)
  | Dba.Expr.Ite (c1, exp11, exp12), Dba.Expr.Ite (c2, exp21, exp22) ->
    is_equal_expr c1 c2 &&
    is_equal_expr exp11 exp21 && is_equal_expr exp12 exp22
  | _, _ -> false



let update_flags lhs expr flags =
  let flags_set =
    match flags with
    | None -> Basic_types.String.Map.empty
    | Some flgs -> flgs
  in
  match lhs with
  | Dba.(LValue.Var { info = VarTag.Empty; _})
  | Dba.LValue.Restrict _
  | Dba.LValue.Store _ -> flags
  | Dba.(LValue.Var {name; info; _}) ->
    Some (Basic_types.String.Map.add name (expr, info) flags_set)

let rec substitute_loads_in_expr expr op load_op =
  let open Dba.Expr in
  match expr with
  | Dba.Expr.Var _ -> expr
  | Dba.Expr.Load (sz, endianness, e) ->
    if is_equal_expr op expr then load_op
    else
      let e = substitute_loads_in_expr e op load_op in
      let sz = Size.Byte.create sz in
      load sz endianness e
  | Dba.Expr.Cst (_a, _b) -> expr
  | Dba.Expr.Unary (unop, e) ->
    substitute_loads_in_expr e op load_op |> unary unop

  | Dba.Expr.Binary (binop, e1, e2)       ->
    let e1 = substitute_loads_in_expr e1 op load_op in
    let e2 = substitute_loads_in_expr e2 op load_op in
    binary binop e1 e2
  | Dba.Expr.Ite  (c, e1, e2)            ->
    let e1 = substitute_loads_in_expr e1 op load_op in
    let e2 = substitute_loads_in_expr e2 op load_op in
    let c  = substitute_loads_in_expr c  op load_op in
    ite c e1 e2

let hide_loads cond op1 op2 =
  let open Dba.Expr in
  match op1, op2 with
  | Dba.Expr.Load (s1, _end1, _exp1), Dba.Expr.Load (s2, _end2, _exp2) ->
    let load_op1 = temporary "load_op1" ~size:(s1 * 8) in
    let load_op2 = temporary "load_op2" ~size:(s2 * 8) in
    let cond = substitute_loads_in_expr cond op1 load_op1 in
    let cond = substitute_loads_in_expr cond op2 load_op2 in
    cond, load_op1, load_op2
  | Dba.Expr.Load (s1, _end1, _exp1), _ ->
    let load_op1 = temporary "load_op1" ~size:(s1 * 8) in
    let cond = substitute_loads_in_expr cond op1 load_op1 in
    cond, load_op1, op2
  | _, Dba.Expr.Load (s2, _end2, _exp2) ->
    let load_op2 = temporary "load_op2" ~size:(s2 * 8)  in
    let cond = substitute_loads_in_expr cond op2 load_op2 in
    cond, op1, load_op2
  | _, _ -> cond, op1, op2


let rec recover_loads_in_expr expr load_op1 load_op2 op1 op2 =
  let open Dba.Expr in
  match expr with
  | Dba.Expr.Var _ ->
    if is_equal_expr expr load_op1 then op1
    else if is_equal_expr expr load_op2 then op2
    else expr
  | Dba.Expr.Load _
  | Dba.Expr.Cst _ -> expr
  | Dba.Expr.Unary (unop, e) ->
    recover_loads_in_expr e load_op1 load_op2 op1 op2
    |> unary unop

  | Dba.Expr.Binary (binop, e1, e2)       ->
    let e1 = recover_loads_in_expr e1 load_op1 load_op2 op1 op2 in
    let e2 = recover_loads_in_expr e2 load_op1 load_op2 op1 op2 in
    binary binop e1 e2
  | Dba.Expr.Ite  (c, e1, e2)            ->
    let e1 = recover_loads_in_expr e1 load_op1 load_op2 op1 op2 in
    let e2 = recover_loads_in_expr e2 load_op1 load_op2 op1 op2 in
    let c  = recover_loads_in_expr c load_op1 load_op2 op1 op2 in
    ite c e1 e2


let rec replace_operands_by_csts_in_expr expr op1 op2 v1 v2 =
  let open Dba.Expr in
  match expr with
  | Dba.Expr.Var _ ->
    if is_equal_expr expr op1 then v1
    else if is_equal_expr expr op2 then v2
    else expr
  | Dba.Expr.Load (_sz, _endianness, _e) ->
    if is_equal_expr expr op1 then v1
    else if is_equal_expr expr op2 then v2
    else expr
  | Dba.Expr.Cst (_, _) ->  expr
  | Dba.Expr.Unary (unop, e) ->
    replace_operands_by_csts_in_expr e op1 op2 v1 v2 |> unary unop

  | Dba.Expr.Binary (binop, e1, e2)       ->
    let e1 = replace_operands_by_csts_in_expr e1 op1 op2 v1 v2 in
    let e2 = replace_operands_by_csts_in_expr e2 op1 op2 v1 v2 in
    binary binop e1 e2

  (* | Dba.Expr.Restrict (e, o1, o2)        ->
   *   if is_equal_expr expr op1 then v1  -- FIXME: This is weird: intentional
     or bug?
   *   else if is_equal_expr expr op2 then v2
   *   else
   *     let e = replace_operands_by_csts_in_expr e op1 op2 v1 v2 in
   *     restrict e o1 o2 *)

  | Dba.Expr.Ite  (c, e1, e2)            ->
    let e1 = replace_operands_by_csts_in_expr e1 op1 op2 v1 v2 in
    let e2 = replace_operands_by_csts_in_expr e2 op1 op2 v1 v2 in
    let c  = replace_operands_by_csts_in_expr c  op1 op2 v1 v2 in
    ite c e1 e2


let is_operand e =
  let open Dba in
  match e with
  | Dba.Expr.Var _
  | Dba.Expr.Cst _
  | Dba.Expr.Load _
  | Dba.Expr.Unary (Unary_op.Restrict _, (Expr.Var _ | Expr.Load _ )) -> true
  | _ -> false

let rec is_natural_cond = function
  | Dba.Expr.Cst _ -> true
  | e -> is_natural_expr e

and is_natural_expr e =
  match e with
  | Dba.Expr.Binary (_binop, e1, e2) -> is_operand e1 && is_operand e2
  | _ -> false


let can_be_equal op =
  match op with
  | Dba.Binary_op.LeqU | Dba.Binary_op.GeqU | Dba.Binary_op.LeqS | Dba.Binary_op.GeqS | Dba.Binary_op.Eq -> true
  | Dba.Binary_op.LtU | Dba.Binary_op.GtU | Dba.Binary_op.LtS | Dba.Binary_op.GtS | Dba.Binary_op.Diff -> false
  | _ -> true

let can_not_be_equal op =
  match op with
  | Dba.Binary_op.LeqU | Dba.Binary_op.GeqU
  | Dba.Binary_op.LeqS | Dba.Binary_op.GeqS | Dba.Binary_op.Eq -> false
  | Dba.Binary_op.LtU | Dba.Binary_op.GtU | Dba.Binary_op.LtS | Dba.Binary_op.GtS | Dba.Binary_op.Diff -> true
  | _ -> true


let can_be_lower_same_sign operand op =
  match operand with
    Dba.Expr.Cst (`Constant, bv) ->
    let size = Bitvector.size_of bv in
    let bv_max = Bitvector.max_ubv size in
    let bv_smax = Bitvector.max_sbv size in
    Bitvector.equal bv bv_smax
    || Bitvector.equal bv bv_max
    ||
    (match op with
     | Dba.Binary_op.LeqU | Dba.Binary_op.LtU | Dba.Binary_op.LeqS | Dba.Binary_op.LtS -> true
     | Dba.Binary_op.GeqU | Dba.Binary_op.GtU | Dba.Binary_op.GeqS | Dba.Binary_op.GtS -> false
     | _ -> true
    )
  | _ -> failwith "high_level_predicate.ml: not constant operand in condition 2"


let can_not_be_lower_same_sign operand op =
  match operand with
    Dba.Expr.Cst (`Constant, bv) ->
    let size = Bitvector.size_of bv in
    let bv_max = Bitvector.max_ubv size in
    let bv_smax = Bitvector.max_sbv size in
    if (Bitvector.equal bv bv_smax) || (Bitvector.equal bv bv_max) then true
    else (
      match op with
      | Dba.Binary_op.LeqU | Dba.Binary_op.LtU | Dba.Binary_op.LeqS | Dba.Binary_op.LtS -> false
      | Dba.Binary_op.GeqU | Dba.Binary_op.GtU | Dba.Binary_op.GeqS | Dba.Binary_op.GtS -> true
      | _ -> true
    )
  | _ -> failwith "high_level_predicate.ml: not constant operand in condition 2"


let can_be_lower_diff_sign operand op =
  match operand with
  |  Dba.Expr.Cst (`Constant, bv) ->
    begin
      let size = Bitvector.size_of bv in
      let bv_smax = Bitvector.max_sbv size in
      if Bitvector.ugt bv bv_smax then
        match op with
        | Dba.Binary_op.LeqS | Dba.Binary_op.LtS | Dba.Binary_op.GeqU | Dba.Binary_op.GtU -> true
        | Dba.Binary_op.LeqU | Dba.Binary_op.LtU | Dba.Binary_op.GeqS | Dba.Binary_op.GtS -> false
        | _ -> true
      else
        match op with
        | Dba.Binary_op.LeqS | Dba.Binary_op.LtS | Dba.Binary_op.GeqU | Dba.Binary_op.GtU -> false
        | Dba.Binary_op.LeqU | Dba.Binary_op.LtU | Dba.Binary_op.GeqS | Dba.Binary_op.GtS -> true
        | _ -> true
    end
  | _ -> failwith "high_level_predicate.ml: not constant operand in condition 3"

let can_not_be_lower_diff_sign operand op =
  match operand with
    Dba.Expr.Cst (`Constant, bv) ->
    let size = Bitvector.size_of bv in
    let bv_smax = Bitvector.max_sbv size in
    if (Bitvector.ule bv bv_smax) then (
      match op with
      | Dba.Binary_op.LeqS | Dba.Binary_op.LtS | Dba.Binary_op.GeqU | Dba.Binary_op.GtU -> true
      | Dba.Binary_op.LeqU | Dba.Binary_op.LtU | Dba.Binary_op.GeqS | Dba.Binary_op.GtS -> false
      | _ -> true
    )
    else (
      match op with
      | Dba.Binary_op.LeqS | Dba.Binary_op.LtS | Dba.Binary_op.GeqU | Dba.Binary_op.GtU -> false
      | Dba.Binary_op.LeqU | Dba.Binary_op.LtU | Dba.Binary_op.GeqS | Dba.Binary_op.GtS -> true
      | _ -> true
    )
  | _ -> failwith "high_level_predicate.ml: not constant operand in condition 4"



let rec eval_expr_csts expr =
  match expr with
  | Dba.Expr.Var _
  | Dba.Expr.Load _ -> raise Not_constant_condition
  | Dba.Expr.Cst (_, v) -> v
  | Dba.Expr.Unary (uop, e) ->
    let e' = eval_expr_csts e in
    begin  match uop with
      | Dba.Unary_op.UMinus -> Bitvector.neg e'
      | Dba.Unary_op.Not -> Bitvector.lognot e'
      | Dba.Unary_op.Uext size -> Bitvector.extend e' size
      | Dba.Unary_op.Sext size -> Bitvector.extend_signed e' size
      | Dba.Unary_op.Restrict i -> Bitvector.extract e' i
    end
  | Dba.Expr.Binary (bop, e1, e2) ->
    let v1 = (eval_expr_csts e1) in
    let v2 = (eval_expr_csts e2) in
    begin
      match bop with
      | Dba.Binary_op.Plus -> Bitvector.add v1 v2
      | Dba.Binary_op.Minus ->	Bitvector.sub v1 v2
      | Dba.Binary_op.Mult ->	Bitvector.mul v1 v2
      | Dba.Binary_op.DivU -> Bitvector.udiv v1 v2
      | Dba.Binary_op.DivS -> Bitvector.sdiv v1 v2
      | Dba.Binary_op.ModU -> Bitvector.umod v1 v2
      | Dba.Binary_op.ModS -> Bitvector.smod v1 v2
      | Dba.Binary_op.Or -> Bitvector.logor v1 v2
      | Dba.Binary_op.And -> Bitvector.logand v1 v2
      | Dba.Binary_op.Xor -> Bitvector.logxor v1 v2
      | Dba.Binary_op.Concat -> Bitvector.append v1 v2
      | Dba.Binary_op.LShift -> Bitvector.shift_left v1 (Bigint.int_of_big_int (Bitvector.value_of v2))
      | Dba.Binary_op.RShiftU -> Bitvector.shift_right v1 (Bigint.int_of_big_int (Bitvector.value_of v2))
      | Dba.Binary_op.RShiftS -> Bitvector.shift_right_signed v1 (Bigint.int_of_big_int (Bitvector.value_of v2))
      | Dba.Binary_op.LeftRotate -> Bitvector.rotate_left v1 (Bigint.int_of_big_int (Bitvector.value_of v2))
      | Dba.Binary_op.RightRotate -> Bitvector.rotate_right v1 (Bigint.int_of_big_int (Bitvector.value_of v2))
      | Dba.Binary_op.Eq  -> Bitvector.equal v1 v2 |> Bitvector.of_bool
      | Dba.Binary_op.Diff -> Bitvector.diff v1 v2 |> Bitvector.of_bool
      | Dba.Binary_op.LeqU -> Bitvector.ule v1 v2 |> Bitvector.of_bool
      | Dba.Binary_op.LtU  -> Bitvector.ult v1 v2 |> Bitvector.of_bool
      | Dba.Binary_op.GeqU -> Bitvector.uge v1 v2 |> Bitvector.of_bool
      | Dba.Binary_op.GtU  -> Bitvector.ugt v1 v2 |> Bitvector.of_bool
      | Dba.Binary_op.LeqS -> Bitvector.sle v1 v2 |> Bitvector.of_bool
      | Dba.Binary_op.LtS  -> Bitvector.slt v1 v2 |> Bitvector.of_bool
      | Dba.Binary_op.GeqS -> Bitvector.sge v1 v2 |> Bitvector.of_bool
      | Dba.Binary_op.GtS  -> Bitvector.sgt v1 v2 |> Bitvector.of_bool
    end

  | Dba.Expr.Ite (cond, e1, e2) ->
    if Bitvector.is_one (eval_expr_csts cond)
    then eval_expr_csts e1
    else eval_expr_csts e2

let eval_constant_condition c =
  let bv = eval_expr_csts c in
  if Bitvector.is_one bv then true
  else if Bitvector.is_zero bv then false
  else assert false

let substitute_eval_cond_csts cond op1 op2 v1 v2 =
  replace_operands_by_csts_in_expr cond op1 op2 v1 v2
  |> eval_constant_condition


let max_same_sign op =
  match op with
    Dba.Expr.Cst (`Constant, bv) ->
    let size = Bitvector.size_of bv in
    let bv_max = Bitvector.max_ubv size in
    let bv_smax = Bitvector.max_sbv size in
    let max = Dba.Expr.constant bv_max in
    let smax = Dba.Expr.constant bv_smax in
    if Bitvector.ule bv bv_smax then smax
    else max
  | _ -> failwith "high_level_predicate.ml: not constant operand in condition"



let max_diff_sign op =
  match op with
  | Dba.Expr.Cst (`Constant, bv) ->
    let size = Bitvector.size_of bv in
    let bv_max = Bitvector.max_ubv size in
    let bv_smax = Bitvector.max_sbv size in
    let max = Dba.Expr.constant bv_max in
    let smax = Dba.Expr.constant bv_smax in
    if Bitvector.ule bv bv_smax then max
    else smax
  | _ -> failwith "high_level_predicate.ml: not constant operand in condition"


let bootstrap_predicates predicates cond op1 op2 =
  let size = Dba_utils.computesize_dbaexpr op1 in
  let one = Dba.Expr.ones size in
  let zero = Dba.Expr.zeros size in
  let max = Dba.Expr.constant (Bitvector.max_ubv size) in
  let is_op1_cst = Dba.Expr.is_constant op1
  and is_op2_cst = Dba.Expr.is_constant op2 in
  if is_op1_cst && is_op2_cst then predicates
  else
    let predicates =
      let c_equal =
        if not is_op1_cst && not is_op2_cst
        then substitute_eval_cond_csts cond op1 op2 zero zero
        else if is_op1_cst
        then substitute_eval_cond_csts cond op1 op2 op1 op1
        else substitute_eval_cond_csts cond op1 op2 op2 op2
      in
      if c_equal then List.filter can_be_equal predicates
      else List.filter can_not_be_equal predicates
    in
    Display.display (Display.Predicates predicates);
    let predicates =
      let operand, c_lower_same_sign =
        if not is_op1_cst && not is_op2_cst
        then zero, substitute_eval_cond_csts cond op1 op2 zero one
        else if is_op1_cst
        then op1, substitute_eval_cond_csts cond op1 op2 op1 (max_same_sign op1)
        else op2, (not (substitute_eval_cond_csts cond op1 op2 (max_same_sign op2) op2))
      in
      if c_lower_same_sign then List.filter (can_be_lower_same_sign operand) predicates
      else List.filter (can_not_be_lower_same_sign operand) predicates
    in
    Display.display (Display.Predicates predicates);
    let predicates =
      let operand, c_lower_diff_sign =
        if not is_op1_cst && not is_op2_cst
        then zero, substitute_eval_cond_csts cond op1 op2 zero max
        else if is_op1_cst
        then op1, substitute_eval_cond_csts cond op1 op2 op1 (max_diff_sign op1)
        else op2, (not (substitute_eval_cond_csts cond op1 op2 (max_diff_sign op2) op2))
      in
      if c_lower_diff_sign then List.filter (can_be_lower_diff_sign operand) predicates
      else List.filter (can_not_be_lower_diff_sign operand) predicates
    in
    Display.display (Display.Predicates predicates);
    predicates


let rec substitute_flags_in_expr expr flags acc =
  let open Dba.Expr in
  match expr with
  | Dba.(Expr.Var {name; _}) ->
    if Basic_types.String.Map.mem name flags then
      let e, tag = Basic_types.String.Map.find name flags in
      let e, acc = substitute_flags_in_expr e flags acc in
      e, tag :: acc
    else expr, acc
  | Dba.Expr.Load  _
  | Dba.Expr.Cst _ -> expr, acc
  | Dba.Expr.Unary (unop, e)             ->
    let e, acc = substitute_flags_in_expr e flags acc in
    unary unop e, acc
  | Dba.Expr.Binary (binop, e1, e2)       ->
    let e1, acc = substitute_flags_in_expr e1 flags acc in
    let e2, acc = substitute_flags_in_expr e2 flags acc in
    binary binop e1 e2, acc
  | Dba.Expr.Ite  (c, e1, e2)            ->
    let e1, acc = substitute_flags_in_expr e1 flags acc in
    let e2, acc = substitute_flags_in_expr e2 flags acc in
    let c, acc  = substitute_flags_in_expr c flags acc in
    ite c e1 e2, acc


and apply_cmp_pattern operands c cond =
  let open Dba in
  let open Binary_op in
  match operands with
  | None            ->
    incr Ai_options.nb_failed_nat_predicate_recoveries;
    cond
  | Some (op1, op2) ->
    incr Ai_options.nb_recovered_nat_predicates;
    match c with
    | Expr.Binary (And,
                   Expr.Unary (Unary_op.Not, Expr.Var { name = "CF"; size = 1; _}),
                   Expr.Unary (Unary_op.Not, Expr.Var { name = "ZF"; size = 1; _})) ->
      Expr.ugt op1 op2
    | Expr.Binary (Or, Expr.Var { name = "CF"; size = 1; _},
                   Expr.Var { name = "ZF"; size = 1;  _}) ->
      Expr.uge op1 op2

    | Expr.Var { name = "CF"; size = 1; _} -> Expr.ult op1 op2
    | Expr.Unary (Unary_op.Not, Expr.Var{ name = "CF"; size = 1; _ }) ->
      Expr.uge op1 op2

    | Expr.Var{ name = "ZF"; size = 1; _ } -> Expr.equal op1 op2
    | Expr.Unary (Unary_op.Not, Expr.Var{ name = "ZF"; size = 1; _ }) ->
      Expr.diff op1 op2
    | Expr.Binary(And,
                  Expr.Unary(Unary_op.Not, Expr.Var { name = "ZF"; size = 1; _ }),
                  Expr.Binary(Eq,
                              Expr.Var { name = "SF"; size = 1; _ },
                              Expr.Var { name = "OF"; size = 1; _ }
                             )) ->
      Expr.sgt op1 op2

    | Expr.Binary(Or,
                  Expr.Var { name = "ZF"; size = 1; _ },
                  Expr.Binary(Diff,
                              Expr.Var { name = "SF"; size = 1; _ },
                              Expr.Var { name = "OF"; size = 1; _ })) ->
      Expr.sle op1 op2
    | _ ->
      decr Ai_options.nb_recovered_nat_predicates;
      incr Ai_options.nb_failed_nat_predicate_recoveries;
      cond


and check_size_operands op1 op2 =
  let s1 = Dba_utils.computesize_dbaexpr op1 in
  let s2 = Dba_utils.computesize_dbaexpr op2 in
  if s1 = s2 then op1, op2
  else if s1 < s2 then
    op1, Dba.Expr.restrict 0 (s1 - 1) op2
  else
    Dba.Expr.restrict 0 (s2 - 1) op1, op2


and retrieve_comparison ~condition (flags:t) addr rcd_conds =
  match flags with
  | None ->
    incr Ai_options.nb_failed_nat_predicate_recoveries;
    let cond, _tags =
      substitute_flags_in_expr condition Basic_types.String.Map.empty [] in
    cond, rcd_conds
  | Some flgs ->
    let cond, tags = substitute_flags_in_expr condition flgs [] in
    try
      let pred_cond, pred_nat_cond =
        Dba_types.Caddress.Map.find addr rcd_conds in
      if is_equal_expr cond pred_cond
      then (
        incr Ai_options.nb_conditional_cache_uses;
        pred_nat_cond, rcd_conds
      )
      else raise Not_found
    with Not_found ->
      if is_natural_cond cond then (
        let rcd_conds = Dba_types.Caddress.Map.add addr (cond, cond) rcd_conds in
        cond, rcd_conds
      )
      else if Ai_options.X86FlagPatterns.get () then
        let operands = operands_of_cmp tags in
        let compteur_simpl = Unix.gettimeofday () in
        let nat_cond = apply_cmp_pattern operands condition cond in
        let rcd_conds = Dba_types.Caddress.Map.add addr (cond, nat_cond) rcd_conds in
        Ai_options.time_nat_flag_recovery :=
          !Ai_options.time_nat_flag_recovery +. (Unix.gettimeofday() -. compteur_simpl);
        nat_cond, rcd_conds
      else
        let operands = operands_of_expr cond [] in
        match operands with
        | op1 :: op2 :: _ ->
          begin
            let op1, op2 = check_size_operands op1 op2 in
            let temp_cond, temp_op1, temp_op2 = hide_loads cond op1 op2 in
            let predicates =
              let open Dba.Binary_op in
              [ Eq; Diff;
                LeqU; LtU; GeqU; GtU;
                LeqS; LtS; GeqS; GtS]
            in
            let predicates = bootstrap_predicates predicates cond op1 op2 in
            let predicate =
              Normalize_predicate.apply_smt_natural_cond_recovery predicates
                ~condition:temp_cond temp_op1 temp_op2 in
            match predicate with
            | None ->
              Logger.debug ~level:3 "Predicate = %a"
                Dba_printer.Ascii.pp_bl_term temp_cond;
              incr Ai_options.nb_failed_nat_predicate_recoveries;
              let rcd_conds = Dba_types.Caddress.Map.add addr (cond, cond) rcd_conds in
              cond, rcd_conds
            | Some expr ->
              let nat_cond = recover_loads_in_expr expr temp_op1 temp_op2 op1 op2 in
              let rcd_conds = Dba_types.Caddress.Map.add addr (cond, nat_cond) rcd_conds in
              nat_cond, rcd_conds
          end
        | _ ->
          incr Ai_options.nb_failed_nat_predicate_recoveries;
          let rcd_conds = Dba_types.Caddress.Map.add addr (cond, cond) rcd_conds in
          cond, rcd_conds

and operands_of_expr expr acc =
  let open Dba in
  match expr with
  | Expr.Var { info = VarTag.Empty; _ } -> expr :: acc
  | Expr.Var _ -> acc
  | Expr.Load (_size, _endianness, _e) -> expr :: acc
  | Expr.Cst (_region, _bv) -> expr :: acc
  | Expr.Unary (Unary_op.Restrict {Interval.lo; Interval.hi;}, e) ->
    if lo = hi then operands_of_expr e acc else expr :: acc
  | Expr.Unary (_unop, e) -> operands_of_expr e acc
  | Expr.Binary (_, e1, e2) ->
    operands_of_expr e1 acc |> operands_of_expr e2
  | Expr.Ite (c, e1, e2) ->
    operands_of_expr e1 acc |> operands_of_expr e2 |> operands_of_expr c


and operands_of_cmp = function
  | Dba.VarTag.Temp :: l
  | Dba.VarTag.Empty :: l -> operands_of_cmp l
  | Dba.VarTag.Flag (Dba.Flag.Cmp (op1, op2)) :: _ -> Some (op1, op2)
  | _ -> None


let join flags1 flags2 =
  match flags1, flags2 with
  | flg, None
  | None, flg -> flg
  | Some flg1, Some flg2 ->
    let flg =
      Basic_types.String.Map.merge
        (fun _ elem1 elem2 ->
          match elem1, elem2 with
          | None, None
          | None, Some _
          | Some _, None -> None
          | Some (e1, tag1) , Some (e2, tag2) ->
            if tag1 = tag2 && is_equal_expr e1 e2
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
        tag1 = tag2 && is_equal_expr e1 e2
      with Not_found -> false
    in Basic_types.String.Map.for_all predicate flgs1
