let id fm =
  fm
  |> Binsec_smtlib.Formula.To_smtlib.formula ~theory:"QF_ABV"
  |> Binsec.Smtlib_to_formula.script

let id' scr : Binsec_smtlib.Lang.script =
  let fm = Binsec.Smtlib_to_formula.script scr in
  {
    script_commands =
      Binsec_smtlib.Lang.Utils.(mk_command (CmdSetLogic (mk_symbol "QF_ABV")))
      :: Binsec_smtlib.Formula.fold_backward
           (fun entry acc -> Binsec_smtlib.Formula.To_smtlib.entry entry :: acc)
           fm [];
    script_loc = Binsec_smtlib.Lang.Location.dummy_loc;
  }

open OUnit2

let formula_txt0 =
  {|
(set-logic QF_ABV)
(declare-fun mem () (Array (_ BitVec 8) (_ BitVec 8)))
(declare-fun addr1 () (_ BitVec 8))
(declare-fun addr2 () (_ BitVec 8))
(declare-fun val () (_ BitVec 8))
(define-fun mem_write () (Array (_ BitVec 8) (_ BitVec 8))
  (store mem addr1 val))
(assert (not (= addr1 addr2)))
(assert (not (= (select mem_write addr2) (select mem addr2))))
|}

let formula_scr0 =
  Binsec_kernel.Parse_utils.read_string ~parser:Binsec_smtlib.Lang.Parser.script
    ~lexer:Binsec_smtlib.Lang.Lexer.token ~string:formula_txt0

let formula_ast0 = Binsec.Smtlib_to_formula.script formula_scr0

let test_script_identity scr =
  assert_equal scr (id' scr)
    ~printer:(Format.asprintf "%a" Binsec_smtlib.Lang.Printer.pp)

let test_formula_identity fm =
  assert_equal fm (id fm)
    ~printer:(Format.asprintf "%a" Binsec_smtlib.Formula.pp_formula)

let () =
  run_test_tt_main
    ("formula bijection"
    >::: [
           ("formula to formula" >:: fun _ -> test_formula_identity formula_ast0);
           ("script to script" >:: fun _ -> test_script_identity formula_scr0);
         ])
