(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
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

open Smt_options

module Translate = struct
  open Dba

  let unary e = function
    | Unary_op.Not -> Formula.mk_bv_not
    | Unary_op.UMinus -> Formula.mk_bv_neg
    | Unary_op.Sext n -> Formula.mk_bv_sign_extend (n - Dba.Expr.size_of e)
    | Unary_op.Uext n -> Formula.mk_bv_zero_extend (n - Dba.Expr.size_of e)
    | Unary_op.Restrict interval -> Formula.mk_bv_extract interval

  let as_bv bop e1 e2 = Formula.(mk_bv_ite (bop e1 e2) mk_bv_one mk_bv_zero)
  let rotate_right_const n = Formula.mk_bv_rotate_right n
  let rotate_left_const n = Formula.mk_bv_rotate_left n

  let rotate shift_func rev_shift_func const_rot_func value shift =
    let open Formula in
    match shift.bv_term_desc with
    | BvCst x ->
        let op = Bitvector.value_of x |> Z.to_int |> const_rot_func in
        op value
    | _ ->
        let part1 = shift_func value shift
        and shift_size = Formula_utils.bv_size shift
        and value_size = Formula_utils.bv_size value |> Z.of_int in
        let value_size = Bitvector.create value_size shift_size |> mk_bv_cst in
        let offset = mk_bv_sub value_size shift in
        let part2 = rev_shift_func value offset in
        mk_bv_or part1 part2

  let rotate_right =
    rotate Formula.mk_bv_lshr Formula.mk_bv_shl rotate_right_const

  let rotate_left =
    rotate Formula.mk_bv_shl Formula.mk_bv_lshr rotate_left_const

  let binary op =
    let open Binary_op in
    match op with
    | Plus -> Formula.mk_bv_add
    | Minus -> Formula.mk_bv_sub
    | Mult -> Formula.mk_bv_mul
    | DivU -> Formula.mk_bv_udiv
    | DivS -> Formula.mk_bv_sdiv
    | ModU -> Formula.mk_bv_urem
    | ModS -> Formula.mk_bv_srem
    | Eq -> as_bv Formula.mk_bv_equal
    | Diff -> as_bv Formula.mk_bv_distinct
    | LeqU -> as_bv Formula.mk_bv_ule
    | LtU -> as_bv Formula.mk_bv_ult
    | GeqU -> as_bv Formula.mk_bv_uge
    | GtU -> as_bv Formula.mk_bv_ugt
    | LeqS -> as_bv Formula.mk_bv_sle
    | LtS -> as_bv Formula.mk_bv_slt
    | GeqS -> as_bv Formula.mk_bv_sge
    | GtS -> as_bv Formula.mk_bv_sgt
    | Xor -> Formula.mk_bv_xor
    | And -> Formula.mk_bv_and
    | Or -> Formula.mk_bv_or
    | Concat -> Formula.mk_bv_concat
    | LShift -> Formula.mk_bv_shl
    | RShiftU -> Formula.mk_bv_lshr
    | RShiftS -> Formula.mk_bv_ashr
    | LeftRotate -> rotate_left
    | RightRotate -> rotate_right

  let rec expr symbolic_state e =
    let smt_unary = unary and smt_binary = binary in
    let open Dba.Expr in
    match e with
    | Var { name; size; _ } ->
        Smt_symbolic.State.get_bv name (Size.Bit.create size) symbolic_state
    | Cst bv -> (Formula.mk_bv_cst bv, symbolic_state)
    | Load (bytes, _endianness, e, None) ->
        let smt_e, st = expr symbolic_state e in
        let mem = Smt_symbolic.State.get_memory st in
        (Formula.mk_select bytes mem smt_e, st)
    | Load _ -> assert false
    | Binary (bop, lop, rop) as e ->
        Logger.debug ~level:6 "Translating binary %a"
          Dba_printer.Ascii.pp_bl_term e;
        let l_smt_e, st = expr symbolic_state lop in
        let r_smt_e, st' = expr st rop in
        (smt_binary bop l_smt_e r_smt_e, st')
    | Unary (uop, e) ->
        let v, st = expr symbolic_state e in
        (smt_unary e uop v, st)
    | Ite (c, then_e, else_e) ->
        let cond, st = expr symbolic_state c in
        let then_smt, st' = expr st then_e in
        let else_smt, st'' = expr st' else_e in
        let v =
          Formula.(mk_bv_ite (mk_bv_equal cond mk_bv_one) then_smt else_smt)
        in
        (v, st'')

  open Smt_symbolic

  let lvalue_with_rval_update symbolic_state logical_rval = function
    | LValue.Var { name; size = bitsize; _ } ->
        ( name,
          Formula.bv_sort bitsize,
          Formula.mk_bv_term logical_rval,
          symbolic_state )
    | LValue.Restrict ({ name; size = bitsize; _ }, { Interval.lo; Interval.hi })
      ->
        let size = Size.Bit.create bitsize in
        let t = Formula.bv_sort bitsize in
        let svar, st = State.get_bv name size symbolic_state in
        let concat_lo = lo - 1 and concat_hi = hi + 1 in
        let max_bit = bitsize - 1 in
        let rval =
          let open Formula in
          match (concat_lo < 0, concat_hi > max_bit) with
          | false, false ->
              mk_bv_concat
                (mk_bv_extract
                   { Interval.lo = concat_hi; Interval.hi = max_bit }
                   svar)
                (mk_bv_concat logical_rval
                   (mk_bv_extract
                      { Interval.lo = 0; Interval.hi = concat_lo }
                      svar))
          | true, false ->
              mk_bv_concat
                (mk_bv_extract
                   { Interval.lo = concat_hi; Interval.hi = max_bit }
                   svar)
                logical_rval
          | false, true ->
              mk_bv_concat logical_rval
                (mk_bv_extract
                   { Interval.lo = 0; Interval.hi = concat_lo }
                   svar)
          | true, true -> logical_rval
        in
        (name, t, Formula.mk_bv_term rval, st)
    | LValue.Store (sz, _, e, None) ->
        let mem = State.get_memory symbolic_state in
        let value = logical_rval in
        let index, st = expr symbolic_state e in
        let n, s, v = State.memory_term (Formula.mk_store sz mem index value) in
        (n, s, v, st)
    | LValue.Store _ -> assert false

  let assign ?(wild = false) lval rval symstate =
    let logical_rval_base, st = expr symstate rval in
    let name, var_type, logical_rval, st' =
      lvalue_with_rval_update st logical_rval_base lval
    in
    Smt_symbolic.State.assign ~wild name var_type logical_rval st'

  let gen_unknown =
    let count = ref 0 in
    fun ?naming_hint () ->
      match naming_hint with
      | None ->
          incr count;
          "bs_unknown" ^ string_of_int !count
      | Some name -> name

  let havoc ?naming_hint ?(wild = false) lvalue ss =
    let size = LValue.size_of lvalue in
    let name = gen_unknown ?naming_hint () in
    let symstate =
      Smt_symbolic.State.declare ~wild name (Formula.BvSort size) ss
    in
    let logical_rval_base, st =
      Smt_symbolic.State.get_bv name (Size.Bit.create size) symstate
    in
    let name, var_type, logical_rval, st' =
      lvalue_with_rval_update st logical_rval_base lvalue
    in
    Smt_symbolic.State.assign name var_type logical_rval st'

  let assume cond state =
    assert (Expr.size_of cond = 1);
    let c, state = expr state cond in
    Smt_symbolic.State.constrain Formula.(mk_bv_equal c mk_bv_one) state
end

module Utils = struct
  let sse_dirname = "binsec_sse"

  let mk_file ~dir =
    let n = ref 0 in
    fun () ->
      incr n;
      let temp_dir = dir () in
      if not (Sys.file_exists temp_dir) then (
        Logger.debug ~level:6 "Creating directory %s" temp_dir;
        Unix.mkdir temp_dir 0o700);
      let filename =
        Filename.concat temp_dir @@ Printf.sprintf "sse_%d.smt2" !n
      in
      Logger.debug ~level:5 "Creating temporary %s" filename;
      filename

  let temp_file =
    let dir () =
      let tmpdir = SMT_dir.get () in
      Filename.concat tmpdir sse_dirname
    in
    mk_file ~dir
end

module Solver = struct
  let queries = ref 0

  type time = { mutable sec : float }

  let cumulated_time = { sec = 0. }
  let query_stat () = !queries
  let time_stat () = cumulated_time.sec

  type t = Solver.Session.t * float

  let open_session () =
    let timeout = Formula_options.Solver.Timeout.get () in
    let file =
      if SMT_dir.is_set () then (
        let filename = Utils.temp_file () in
        Logger.debug ~level:3 "@[<h>Using SMT script file %s@]" filename;
        Some filename)
      else None
    in
    let solver = Formula_options.Solver.get () in
    let t = Unix.gettimeofday () in
    (Solver.Session.create ?file ~timeout solver, t)

  let put (solver, _) entry = Solver.Session.put_entry solver entry

  let check_sat (solver, _) =
    incr queries;
    try Solver.Session.check_sat solver with
    | Failure msg ->
        Logger.warning "SMT solver failed on %s" msg;
        Solver.Session.destroy solver;
        if not (KeepGoing.get ()) then (
          Logger.error
            "@[<v 0>@[SMT solver failed with message:@].@ @[%s@]@ @[Aborting. \
             Use -keep-going to ignore@]@]"
            msg;
          failwith msg);
        Formula.UNKNOWN
    | e ->
        Solver.Session.destroy solver;
        Logger.warning "Destroyed session";
        raise e

  let value_of_constant cst =
    let open Smtlib in
    match cst with
    | CstHexadecimal s -> Bitvector.of_hexstring ("0x" ^ s)
    | CstBinary s -> Bitvector.of_string ("0b" ^ s)
    | CstDecimalSize (value, size) ->
        Bitvector.create (Z.of_string value) (int_of_string size)
    | CstBool b -> Bitvector.of_bool b
    | CstNumeral _ | CstString _ | CstDecimal _ ->
        Logger.fatal
          "Model construction: unexpected constant %a as bitvector value"
          Smtlib_pp.pp_spec_constant cst

  let extract_bv terms =
    let open Smtlib in
    match terms with
    | [ (_, { term_desc = TermSpecConstant cst; _ }) ] -> value_of_constant cst
    | _ -> assert false

  let get_bv_value (solver, _) e =
    extract_bv (Solver.Session.get_value solver (Formula.mk_bv_term e))

  let get_ax_values (solver, _) e =
    let model = Smt_model.create () in
    Smt_model.add_memory_term model
      (Solver.Session.get_value solver (Formula.mk_ax_term e));
    Smt_model.memory_bindings model

  let close_session (solver, t) =
    Solver.Session.destroy solver;
    cumulated_time.sec <- cumulated_time.sec +. Unix.gettimeofday () -. t

  let check_sat_and_close solver =
    let res = check_sat solver in
    close_session solver;
    res
end
