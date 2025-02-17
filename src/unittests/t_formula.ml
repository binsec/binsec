(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2025                                               *)
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

open OUnit2
open Formula

(** asserts that two formulas are equal *)
let eq_fm expected actual ctxt =
  assert_equal ~ctxt
    ~printer:(fun fm -> Format.asprintf "%a" Formula_pp.pp_formula fm)
    expected actual

(** asserts that two bv terms are equal *)
let eq_bv expected actual ctxt =
  assert_equal ~ctxt
    ~printer:(fun bv -> Format.asprintf "%a" Formula.Printing.p_bvterm bv)
    expected actual

(** asserts there is not exception thrown when generating the term from the description *)
let assert_no_raise desc desc_str _ =
  try ignore (bv_term desc)
  with _ ->
    assert_failure
      (Format.asprintf "failed to create the term associated to %s" desc_str)

let tests = []

(** makes a formula from a list of entries *)
let from_list l = List.fold_left (fun fm entry -> push_front entry fm) empty l

(** makes a formula from a smt string
 * NOTE: Does all sorts of simlifications like splitting (assert (and a b)) in
 * (assert a) (assert b) or simplifying constant operations. *)
let from_string s =
  let smt =
    try
      Parse_utils.read_string ~parser:Smtlib_parser.script
        ~lexer:Smtlib_lexer.token ~string:s
    with Failure s as e ->
      Format.eprintf "Failure: %s" s;
      raise e
  in
  Smtlib_to_formula.script smt

(* actual tests start here *)

(* {{{ Smart constructors *)

let term =
  let a = mk_ax_var (ax_var "a" 2 2) in
  let b = mk_bv_var (bv_var "b" 2) in
  mk_bv_xor (mk_select 2 a b)
    (mk_bv_concat (mk_select 1 a (mk_bv_add_int b 1)) (mk_select 1 a b))

let expected = mk_bv_cst (Bitvector.zeros 4)

let tests =
  ("xor x x  = 0 and coalescing concat(select x, select x+1)"
 >:: eq_bv expected term)
  :: tests

(* }}} *)

let formula = from_string "(declare-fun useless () Bool)"

let tests =
  ("prune useless decl"
  >:: eq_fm empty (Formula_transformation.prune_and_inline formula))
  :: tests

let formula =
  [
    mk_define (mk_bl_def (bl_var "pa") [] mk_bl_true);
    mk_declare (mk_bl_decl (bl_var "x") []);
    mk_define
      (mk_bl_def (bl_var "pa2") []
         (mk_bl_and (mk_bl_var (bl_var "pa")) (mk_bl_var (bl_var "x"))));
    mk_assert (mk_bl_var (bl_var "pa2"));
  ]
  |> from_list

let expected =
  [
    mk_declare (mk_bl_decl (bl_var "x") []); mk_assert (mk_bl_var (bl_var "x"));
  ]
  |> from_list

let tests =
  ("inline true def"
  >:: eq_fm expected (Formula_transformation.prune_and_inline formula))
  :: tests

let formula =
  [
    mk_define (mk_bl_def (bl_var "pa") [] mk_bl_true);
    mk_declare (mk_bl_decl (bl_var "x") []);
    mk_define
      (mk_bl_def (bl_var "pa2") []
         (mk_bl_and (mk_bl_var (bl_var "pa")) (mk_bl_var (bl_var "x"))));
    mk_define
      (mk_bl_def (bl_var "pa3") []
         (mk_bl_or (mk_bl_var (bl_var "pa2")) (mk_bl_var (bl_var "x"))));
    mk_assert (mk_bl_var (bl_var "pa2"));
  ]
  |> from_list

let expected =
  [
    mk_declare (mk_bl_decl (bl_var "x") []); mk_assert (mk_bl_var (bl_var "x"));
  ]
  |> from_list

let tests =
  ("inline true def and prune irrelevant"
  >:: eq_fm expected (Formula_transformation.prune_and_inline formula))
  :: tests

(* test that we do not crash when div/rem/mod with constant 0 in formula *)
let desc op =
  BvBnop
    (op, mk_bv_cst (Bitvector.of_int32 4l), mk_bv_cst (Bitvector.of_int32 0l))

let tests =
  List.map
    (fun (op, opstr) ->
      let term_str = Format.asprintf "4 %s 0" opstr in
      "no div_by_zero exception when creating " ^ term_str ^ " term"
      >:: assert_no_raise (desc op) term_str)
    [
      (BvUdiv, "udiv");
      (BvSdiv, "sdiv");
      (BvSmod, "smod");
      (BvSrem, "srem");
      (BvUrem, "urem");
    ]
  @ tests

let term =
  let v = mk_bv_var (bv_var "v" 32) in
  let cst1 = mk_bv_cst (Bitvector.zeros 2) in
  let cst2 = mk_bv_cst (Bitvector.zeros 3) in
  mk_bv_concat (mk_bv_concat v cst1) cst2

let expected =
  let v = mk_bv_var (bv_var "v" 32) in
  let cst = mk_bv_cst (Bitvector.zeros 5) in
  mk_bv_concat v cst

let tests =
  ("(e ++ bv1) ++ bv2 = e ++ (bv1 ++ bv2)" >:: eq_bv expected term) :: tests

(* test that we do not crash when div/rem/mod with constant 0 in formula *)
let desc op =
  BvBnop
    (op, mk_bv_cst (Bitvector.of_int32 4l), mk_bv_cst (Bitvector.of_int32 0l))

let tests =
  List.map
    (fun (op, opstr) ->
      let term_str = Format.asprintf "4 %s 0" opstr in
      "no div_by_zero exception when creating " ^ term_str ^ " term"
      >:: assert_no_raise (desc op) term_str)
    [
      (BvUdiv, "udiv");
      (BvSdiv, "sdiv");
      (BvSmod, "smod");
      (BvSrem, "srem");
      (BvUrem, "urem");
    ]
  @ tests

let term =
  let v = mk_bv_var (bv_var "v" 32) in
  let cst1 = mk_bv_cst (Bitvector.of_int ~size:2 2) in
  let cst2 = mk_bv_cst (Bitvector.of_int ~size:2 3) in
  mk_bv_concat (mk_bv_concat v cst1) cst2

let expected =
  let v = mk_bv_var (bv_var "v" 32) in
  let cst = mk_bv_cst (Bitvector.of_int ~size:4 11) in
  mk_bv_concat v cst

let tests =
  ("(e ++ bv1) ++ bv2 = e ++ (bv1 ++ bv2)" >:: eq_bv expected term) :: tests

(* run the tests *)
let () = run_test_tt_main ("formula simplifications" >::: tests)
