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

open Formula
open Formula_pp

(* Converts a dba expression into SMT formula *)
let rec dbaExpr_to_smtExpr expr inputs : bv_term * VarSet.t =
  match expr with
  | Dba.(Expr.Var {name; size; _}) ->
    let var = bv_var name size in
    let inputs = VarSet.add (BvVar var) inputs in
    mk_bv_var var, inputs
  | Dba.Expr.Load (size, endian, e) ->
    let smt_load, inputs = load_to_smt e size endian inputs in
    smt_load, inputs
  | Dba.Expr.Cst (region, bv) ->
    (match region with  (* Do not handle regions *)
     | `Constant -> mk_bv_cst bv, inputs
     | _ -> failwith "region not supported")
  | Dba.Expr.Unary (op, e) ->
    let open Dba in
    let e, inputs = dbaExpr_to_smtExpr e inputs in
    begin match op with
      | Unary_op.Uext size ->
        let s = Formula_utils.bv_size e in
        mk_bv_zero_extend (size - s) e, inputs
      | Unary_op.Sext size ->
        let s = Formula_utils.bv_size e in
        mk_bv_sign_extend (size - s) e, inputs
      | Unary_op.Restrict interval ->
        mk_bv_extract interval e, inputs
      | Unary_op.UMinus | Unary_op.Not ->
        mk_bv_unop (Dba_to_formula.unary op) e, inputs
    end

  | Dba.Expr.Binary(op, e1, e2) ->
    let expr1, inputs = dbaExpr_to_smtExpr e1 inputs in
    let expr2, inputs = dbaExpr_to_smtExpr e2 inputs in
    (match Dba_to_formula.binary op with
     | `Unop u ->
       (match u with
        | BvRotateLeft _ ->
          (match Formula_utils.is_bv_cst expr2 with
           | Some bv ->
             let value = Bitvector.value_of bv in
             mk_bv_rotate_left (Bigint.int_of_big_int value) expr1, inputs
           (* smtlib only takes constant as shift index, so fail if not a SmtBvCst *)
           | None ->
             Format.printf "%a|%s" Dba_printer.Ascii.pp_bl_term e2 (print_bv_term expr2);
             failwith "shift index for rotate left cannot be expr (must be const)")
        | BvRotateRight _ ->
          (match Formula_utils.is_bv_cst expr2 with
           | Some bv ->
             let value = Bitvector.value_of bv in
             mk_bv_rotate_right (Bigint.int_of_big_int value) expr1, inputs
           | _ ->
             Format.printf "%a|%s" Dba_printer.Ascii.pp_bl_term e2 (print_bv_term expr2);
             failwith "shift index for rotate right cannot be expr (must be const)")
        | _ -> assert false)
     | `Bnop b ->
       (match b with
        | BvShl | BvAshr | BvLshr ->
          (* Check size of the two bitvectors because DBA accepts different
             but not smtlib *)
          let size1 = Formula_utils.bv_size expr1 in
          let size2 = Formula_utils.bv_size expr2 in
          if size1 = size2 then
            mk_bv_bnop b expr1 expr2, inputs
          else if size1 > size2 then
            let expr2 = mk_bv_zero_extend (size1 - size2) expr2 in
            mk_bv_bnop b expr1 expr2, inputs
          else
            failwith "shift x y avec taille(y) > taille(x)"
        | _ ->
          mk_bv_bnop b expr1 expr2, inputs)
     | `Comp c ->
       (* Create formula to return #b1 or #b0 instead of a boolean *)
       mk_bv_ite (mk_bv_comp c expr1 expr2) (mk_bv_one) (mk_bv_zero), inputs
    ) (* Normal case *)
  | Dba.Expr.Ite(c, e1, e2) ->
    let f_c, inputs = dbaExpr_to_smtExpr c inputs in
    let smt_e1, inputs = dbaExpr_to_smtExpr e1 inputs in
    let smt_e2, inputs = dbaExpr_to_smtExpr e2 inputs in
    mk_bv_ite (mk_bv_equal f_c (mk_bv_one)) smt_e1 smt_e2, inputs

and logical_load (addr: bv_term) size : bv_term = (* size in BYTES *)
  let memory = mk_ax_var
                 (ax_var "memory" (Kernel_options.Machine.word_size ()) 8) in
  mk_select size memory addr
(*
  (* Recursive function to concat each byte read into a single bitvector *)
  let rec concat_select_symb sz =
    let to_add = size - sz in
    let new_addr =
      if to_add = 0
      then addr
      else (smtbv_add_int addr to_add)
    in
    let memory = SmtABvArray ("memory", Machine.word_size (), 8) in
    match sz with
    | 1 -> SmtABvSelect (memory, new_addr)
    | 4 -> SmtABvLoad32 (memory, addr)
    | _ ->
      let op1 = concat_select_symb (sz - 1) in
      let op2 = SmtABvSelect (memory, new_addr) in
      SmtBvBinary (SmtBvConcat, op1, op2)
  in
  concat_select_symb size
*)

(* Converts a load in expression *)
and load_to_smt expr size endianness inputs : bv_term * VarSet.t =
  match endianness with
  | Machine.BigEndian -> failwith "Big endian is not implemented\n"
  | Machine.LittleEndian ->
    let expr_f, inputs = dbaExpr_to_smtExpr expr inputs in
    logical_load expr_f size, inputs


let print_check_unsat smt_cond predicate =
  let assertion = mk_bv_distinct smt_cond predicate in
  Format.asprintf "(assert %s)\n\n" (print_bl_term assertion) ^
  "(check-sat)\n"


let make_smt_program smt_cond predicate inputs =
  (print_header ()) ^ "\n\n" ^
  print_varset inputs ^ "\n\n" ^
  print_check_unsat smt_cond predicate


let apply_smt_natural_cond_recovery predicates ~condition op1 op2 =
  let inputs =
    VarSet.singleton (AxVar (ax_var "memory"
                               (Kernel_options.Machine.word_size ()) 8)) in
  let smt_cond, inputs = dbaExpr_to_smtExpr condition inputs in
  let _smt_op1, inputs = dbaExpr_to_smtExpr op1 inputs in
  let _smt_op2, inputs = dbaExpr_to_smtExpr op2 inputs in
  let rec check_unsat predicates =
    match predicates with
      [] -> None
    | binop :: tl ->
      let dba_predicate = Dba.Expr.binary binop op1 op2 in
      let predicate, inputs = dbaExpr_to_smtExpr dba_predicate inputs in
      let smt_program = make_smt_program smt_cond predicate inputs in
      let smtfile = open_out "smt_in.smt2" in
      Printf.fprintf smtfile "%s" smt_program;
      close_out smtfile;
      ignore (Unix.system "z3 -smt2 smt_in.smt2 > smt_out");
      let smtout = open_in "smt_out" in
      let lexbuf = Lexing.from_channel smtout in
      let result, _ = SMTParserWp.main SMTLexerWp.token lexbuf in
      close_in smtout;
      match result with
      | None -> incr Ai_options.nb_nat_predicate_recovery_tries; check_unsat tl
      | Some SAT ->
        incr Ai_options.nb_nat_predicate_recovery_tries;
        check_unsat tl
      | Some TIMEOUT
      | Some UNKNOWN
      | Some UNSAT ->
        incr Ai_options.nb_nat_predicate_recovery_tries;
        Some dba_predicate
  in
  check_unsat predicates
