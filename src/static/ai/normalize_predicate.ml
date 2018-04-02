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

open Smtlib2
open Smtlib2print

(* Converts a dba expression into SMT formula *)
let rec dbaExpr_to_smtExpr expr inputs : smt_bv_expr * SmtVarSet.t =
  match expr with
  | Dba.ExprVar (name, size, _) ->
    let inputs = SmtVarSet.add (SmtBv (name, size)) inputs in
    SmtBvVar (name, size), inputs
  | Dba.ExprLoad (size, endian, e) ->
    let smt_load, inputs = load_to_smt e size endian inputs in
    smt_load, inputs
  | Dba.ExprRestrict (e, i, j) ->
    let smt_expr, inputs = dbaExpr_to_smtExpr e inputs in
    SmtBvUnary (SmtBvExtract (i, j), smt_expr), inputs
  | Dba.ExprCst (region, bv) ->
    (match region with  (* Do not handle regions *)
     | `Constant -> SmtBvCst bv, inputs
     | _ -> failwith "region not supported")
  | Dba.ExprUnary (op, e) ->
    let smt_e, inputs = dbaExpr_to_smtExpr e inputs in
    SmtBvUnary (Dba_to_smtlib.unary op, smt_e), inputs
  | Dba.ExprBinary(op, e1, e2) -> (
      let expr1, inputs = dbaExpr_to_smtExpr e1 inputs in
      let expr2, inputs = dbaExpr_to_smtExpr e2 inputs in
      match op with
      | Dba.LShift | Dba.RShiftU | Dba.RShiftS ->
        (* Check size of the two bitvectors because DBA accepts different
           but not smtlib *)
        let size1 = size_bvexpr expr1 in
        let size2 = size_bvexpr expr2 in
        if size1 = size2 then
          SmtBvBinary (Dba_to_smtlib.binary op, expr1, expr2), inputs
        else if size1 > size2 then
          let expr2 = SmtBvUnary (SmtBvZeroExtend (size1 - size2), expr2) in
          SmtBvBinary (Dba_to_smtlib.binary op, expr1, expr2), inputs
        else
          failwith "shift x y avec taille(y) > taille(x)"
      | Dba.LeqU
      | Dba.LtU
      | Dba.GeqU
      | Dba.GtU
      | Dba.LeqS
      | Dba.LtS
      | Dba.GeqS
      | Dba.GtS ->
        (* Create formula to return #b1 or #b0 instead of a boolean *)
        SmtBvIte (SmtBvExpr (SmtBvBinary (Dba_to_smtlib.binary op, expr1, expr2)),
                  bvone , bvzero), inputs
      | Dba.LeftRotate ->
        (match expr2 with
         | SmtBvCst value ->
           let value = Bitvector.value_of value in
           SmtBvUnary (SmtBvRotateL (Bigint.int_of_big_int value), expr1 ), inputs
         (* smtlib only takes constant as shift index, so fail if not a SmtBvCst *)
         | _ ->
           Format.printf "%a|%s"
          Dba_printer.Ascii.pp_expr e2 (smtbvexpr_to_string expr2);
           failwith "shift index for rotate left cannot be expr (must be const)")
      | Dba.RightRotate -> (
          match expr2 with
          | SmtBvCst value ->
            let value = Bitvector.value_of value in
            SmtBvUnary (SmtBvRotateR (Bigint.int_of_big_int value), expr1), inputs
          | _ ->
            Format.printf "%a|%s"
              Dba_printer.Ascii.pp_expr e2 (smtbvexpr_to_string expr2);
            failwith "shift index for rotate right cannot be expr (must be const)")
      | _ ->
        SmtBvBinary(Dba_to_smtlib.binary op, expr1, expr2), inputs
    )     (* Normal case *)
  | Dba.ExprExtU (e, size) ->
    let expr, inputs = dbaExpr_to_smtExpr e inputs in
    let s = size_bvexpr expr in
    SmtBvUnary (SmtBvZeroExtend (size - s), expr), inputs
  | Dba.ExprExtS (e, size) ->
    let expr, inputs = dbaExpr_to_smtExpr e inputs in
    let s = size_bvexpr expr in
    SmtBvUnary (SmtBvSignExtend (size - s), expr), inputs
  | Dba.ExprIte(c, e1, e2) ->
    let f_c, inputs = cond_to_f c inputs in
    let smt_e1, inputs = dbaExpr_to_smtExpr e1 inputs in
    let smt_e2, inputs = dbaExpr_to_smtExpr e2 inputs in
    SmtBvIte (SmtBvExpr f_c, smt_e1, smt_e2), inputs
  | Dba.ExprAlternative(e_l, _) ->
    let eval e = dbaExpr_to_smtExpr e inputs in
    let eq _ _ = true in (* FIXME: this predicate implementation is probably false *)
    Dba_utils.eval_alternatives eval eq e_l

and logical_load (addr:smt_bv_expr) size : smt_bv_expr = (* size in BYTES *)
  (* Recursive function to concat each byte read into a single bitvector *)
  let rec concat_select_symb sz =
    let to_add = size - sz in
    let new_addr =
      if to_add = 0
      then addr
      else (smtbv_add_int addr to_add)
    in
    let memory = SmtABvArray ("memory", Machine.Word_size.get (), 8) in
    match sz with
    | 1 -> SmtABvSelect (memory, new_addr)
    | 4 -> SmtABvLoad32 (memory, addr)
    | _ ->
      let op1 = concat_select_symb (sz - 1) in
      let op2 = SmtABvSelect (memory, new_addr) in
      SmtBvBinary (SmtBvConcat, op1, op2)
  in
  concat_select_symb size


(* Converts a load in expression *)
and load_to_smt expr size endianness inputs : smt_bv_expr * SmtVarSet.t =
  match endianness with
  | Dba.BigEndian -> failwith "Big endian is not implemented\n"
  | Dba.LittleEndian ->
    let expr_f, inputs = dbaExpr_to_smtExpr expr inputs in
    logical_load expr_f size, inputs


(* Converts a condition into formula *)
(* TODO: voir pour pour convertir bvcmp X 1 par un =  *)
and cond_to_f c inputs : smt_bv_expr * SmtVarSet.t =
  match c with
  | Dba.CondReif e ->
    let expr, inputs = dbaExpr_to_smtExpr e inputs in
    expr, inputs
  | Dba.CondNot c ->
    let smt_c, inputs = cond_to_f c inputs in
    SmtBvUnary (SmtBvNot, smt_c), inputs
  | Dba.CondAnd (c1, c2) ->
    let f_c1, inputs = cond_to_f c1 inputs in
    let f_c2, inputs = cond_to_f c2 inputs in
    SmtBvBinary (SmtBvAnd, f_c1, f_c2), inputs
  | Dba.CondOr (c1, c2) ->
    let f_c1, inputs = cond_to_f c1 inputs in
    let f_c2, inputs = cond_to_f c2 inputs in
    SmtBvBinary (SmtBvOr, f_c1, f_c2), inputs
  | Dba.True -> bvone, inputs
  | Dba.False -> bvzero, inputs


let print_check_unsat smt_cond predicate =
  let assertion = SmtBvBinary (SmtBvComp, smt_cond, predicate) in
  let assertion = SmtComp (SmtBvExpr assertion, zero) in
  Format.asprintf "(assert %s)\n\n" (smtexpr_to_string assertion) ^
  "(check-sat)\n"


let make_smt_program smt_cond predicate inputs =
  (smt_header ()) ^ "\n\n" ^
  smtvarset_to_string inputs ^ "\n\n" ^
  Format.asprintf "%s\n\n" (smt_functions ()) ^
  print_check_unsat smt_cond predicate


let apply_smt_natural_cond_recovery predicates cond op1 op2 =
  let inputs = SmtVarSet.singleton (SmtABv("memory", Machine.Word_size.get (), 8)) in
  let smt_cond, inputs = cond_to_f cond inputs in
  let _smt_op1, inputs = dbaExpr_to_smtExpr op1 inputs in
  let _smt_op2, inputs = dbaExpr_to_smtExpr op2 inputs in
  let rec check_unsat predicates =
    match predicates with
      [] -> None
    | binop :: tl ->
      let dba_predicate = Dba.ExprBinary (binop, op1, op2) in
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
        None -> incr Options.nb_nat_predicate_recovery_tries; check_unsat tl
      | Some SAT -> incr Options.nb_nat_predicate_recovery_tries; check_unsat tl
      | Some TIMEOUT
      | Some UNKNOWN
      | Some UNSAT -> incr Options.nb_nat_predicate_recovery_tries; Some dba_predicate
  in
  check_unsat predicates
