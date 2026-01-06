(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

module Session () : Common.S = struct
  let ctx = Z3.mk_context []

  let bv_sort =
    let module Tbl = Binsec_base.Basic_types.Integers.Int.Htbl in
    let tbl = Tbl.create 32 in
    fun sz ->
      try Tbl.find tbl sz
      with Not_found ->
        let sort = Z3.BitVector.mk_sort ctx sz in
        Tbl.add tbl sz sort;
        sort

  let bv_sort1 = bv_sort 1
  let bv_zero = Z3.Expr.mk_numeral_int ctx 0 bv_sort1
  let bv_one = Z3.Expr.mk_numeral_int ctx 1 bv_sort1

  module Bl = struct
    type t = Z3.Expr.expr

    let const name = Z3.Boolean.mk_const_s ctx name
    let top = Z3.Boolean.mk_true ctx
    let bot = Z3.Boolean.mk_false ctx
    let lognot x = Z3.Boolean.mk_not ctx x
    let logand x y = Z3.Boolean.mk_and ctx [ x; y ]
    let logor x y = Z3.Boolean.mk_or ctx [ x; y ]
    let logxor x y = Z3.Boolean.mk_xor ctx x y
    let ite c x y = Z3.Boolean.mk_ite ctx c x y
    let equal x y = Z3.Boolean.mk_eq ctx x y
    let diff x y = Z3.Boolean.mk_distinct ctx [ x; y ]
    let implies x y = Z3.Boolean.mk_implies ctx x y
    let to_bv x = Z3.Boolean.mk_ite ctx x bv_one bv_zero
  end

  module Bv = struct
    type t = Z3.Expr.expr

    let const sz name = Z3.BitVector.mk_const_s ctx name sz

    let value sz x =
      let sort = bv_sort sz in
      try Z3.Expr.mk_numeral_int ctx (Z.to_int x) sort
      with Z.Overflow -> Z3.Expr.mk_numeral_string ctx (Z.to_string x) sort

    let equal x y = Z3.Boolean.mk_eq ctx x y
    let diff x y = Z3.Boolean.mk_distinct ctx [ x; y ]
    let ule x y = Z3.BitVector.mk_ule ctx x y
    let uge x y = Z3.BitVector.mk_uge ctx x y
    let ult x y = Z3.BitVector.mk_ult ctx x y
    let ugt x y = Z3.BitVector.mk_ugt ctx x y
    let sle x y = Z3.BitVector.mk_sle ctx x y
    let sge x y = Z3.BitVector.mk_sge ctx x y
    let slt x y = Z3.BitVector.mk_slt ctx x y
    let sgt x y = Z3.BitVector.mk_sgt ctx x y
    let add x y = Z3.BitVector.mk_add ctx x y
    let sub x y = Z3.BitVector.mk_sub ctx x y
    let mul x y = Z3.BitVector.mk_mul ctx x y
    let neg x = Z3.BitVector.mk_neg ctx x
    let udiv x y = Z3.BitVector.mk_udiv ctx x y
    let umod x y = Z3.BitVector.mk_urem ctx x y
    let urem = umod
    let sdiv x y = Z3.BitVector.mk_sdiv ctx x y
    let smod x y = Z3.BitVector.mk_smod ctx x y
    let srem x y = Z3.BitVector.mk_srem ctx x y
    let logand x y = Z3.BitVector.mk_and ctx x y
    let logor x y = Z3.BitVector.mk_or ctx x y
    let lognot x = Z3.BitVector.mk_not ctx x
    let logxor x y = Z3.BitVector.mk_xor ctx x y
    let lognand x y = Z3.BitVector.mk_nand ctx x y
    let lognor x y = Z3.BitVector.mk_nor ctx x y
    let logxnor x y = Z3.BitVector.mk_xnor ctx x y
    let shift_left x y = Z3.BitVector.mk_shl ctx x y
    let shift_right x y = Z3.BitVector.mk_lshr ctx x y
    let shift_right_signed x y = Z3.BitVector.mk_ashr ctx x y
    let rotate_left x y = Z3.BitVector.mk_ext_rotate_left ctx x y
    let rotate_right x y = Z3.BitVector.mk_ext_rotate_right ctx x y
    let rotate_lefti x i = Z3.BitVector.mk_rotate_left ctx i x
    let rotate_righti x i = Z3.BitVector.mk_rotate_right ctx i x
    let append x y = Z3.BitVector.mk_concat ctx x y
    let extract ~hi ~lo x = Z3.BitVector.mk_extract ctx hi lo x
    let uext n x = Z3.BitVector.mk_zero_ext ctx n x
    let sext n x = Z3.BitVector.mk_sign_ext ctx n x
    let ite c x y = Z3.Boolean.mk_ite ctx c x y

    let succ x =
      Z3.BitVector.mk_add ctx x
        (Z3.Expr.mk_numeral_int ctx 1 (Z3.Expr.get_sort x))

    let to_bl x = Z3.Boolean.mk_eq ctx x bv_one
  end

  module Ax = struct
    type t = Z3.Expr.expr
    type sort = Z3.Sort.sort

    let sort ~idx elm = Z3.Z3Array.mk_sort ctx (bv_sort idx) (bv_sort elm)

    let const sort name =
      Z3.Z3Array.mk_const_s ctx name
        (Z3.Z3Array.get_domain sort)
        (Z3.Z3Array.get_range sort)

    let store t a x = Z3.Z3Array.mk_store ctx t a x
    let select t a = Z3.Z3Array.mk_select ctx t a
    let equal x y = Z3.Boolean.mk_eq ctx x y
    let diff x y = Z3.Boolean.mk_distinct ctx [ x; y ]
    let ite c x y = Z3.Boolean.mk_ite ctx c x y
  end

  let solver = Z3.Solver.mk_solver_s ctx "QF_ABV"
  let assert_formula bl = Z3.Solver.add solver [ bl ]
  let push () = Z3.Solver.push solver
  let pop () = Z3.Solver.pop solver 1
  let soft_timeout = Z3.Symbol.mk_string ctx "timeout"

  let check_sat ?timeout assumptions : Common.status =
    let params = Z3.Params.mk_params ctx in
    Z3.Params.add_int params soft_timeout
      (Option.fold ~none:4294967295
         ~some:(fun timeout -> int_of_float (1000. *. timeout))
         timeout);
    Z3.Solver.set_parameters solver params;
    match Z3.Solver.check solver assumptions with
    | SATISFIABLE -> Sat
    | UNSATISFIABLE -> Unsat
    | UNKNOWN -> Unknown

  let check_sat_assuming ?timeout bl = check_sat ?timeout [ bl ]
  let check_sat ?timeout () = check_sat ?timeout []
  let to_z v = Z.of_string (Z3.BitVector.numeral_to_string v)

  let get_bv_value x =
    let model = Option.get (Z3.Solver.get_model solver) in
    Option.fold ~none:Z.zero ~some:to_z (Z3.Model.eval model x false)

  let rec fold_ax_values f ax x =
    if Z3.Z3Array.is_store ax then
      match Z3.Expr.get_args ax with
      | [ ax; a; v ] -> fold_ax_values f ax (f (to_z a) (to_z v) x)
      | _ -> assert false
    else x

  let fold_ax_values f ax x =
    let model = Option.get (Z3.Solver.get_model solver) in
    match Z3.Model.eval model ax false with
    | None -> x
    | Some ax -> fold_ax_values f ax x

  let close () = ()
end

let factory = Some (module Session : Common.OPEN)
