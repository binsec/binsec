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

(* run the tests *)
let () = run_test_tt_main ("formula simplifications" >::: tests)
