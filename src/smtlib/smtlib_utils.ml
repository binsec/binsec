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

open Smtlib

let symbol_of_svar (sv : sorted_var) =
  match sv.sorted_var_desc with SortedVar (sy, _) -> sy

let sort_of_svar (sv : sorted_var) =
  match sv.sorted_var_desc with SortedVar (_, so) -> so

let symbol_from_id (id : Smtlib.identifier) =
  match id.id_desc with IdSymbol sy -> sy | IdUnderscore (sy, _) -> sy

let symbols_of_sort (sort : Smtlib.sort) : Smtlib.symbol list =
  let rec aux symbols sort =
    match sort.sort_desc with
    | SortIdentifier id -> symbol_from_id id :: symbols
    | SortFun (id, sorts) ->
        List.fold_left aux (symbol_from_id id :: symbols) sorts
  in
  List.rev (aux [] sort)

let string_of_symbol (symbol : Smtlib.symbol) : string =
  match symbol.symbol_desc with SimpleSymbol s | QuotedSymbol s -> s

let symbol_of_vbinding (vb : var_binding) =
  match vb.var_binding_desc with VarBinding (sy, _) -> sy

let id_from_qid (qid : Smtlib.qual_identifier) : identifier =
  match qid.qual_identifier_desc with
  | QualIdentifierAs (id, _) | QualIdentifierIdentifier id -> id

let get_logic (s : Smtlib.script) =
  let rec aux (cmds : Smtlib.commands) =
    match cmds with
    | [] -> ""
    | cmd :: cmds -> (
        match cmd.command_desc with
        | CmdSetLogic symb -> (
            match symb.symbol_desc with
            | SimpleSymbol logic_name -> logic_name
            | QuotedSymbol _ -> assert false)
        | CmdAssert _ | CmdCheckSat | CmdCheckSatAssuming _ | CmdComment _
        | CmdDeclareConst (_, _)
        | CmdDeclareFun (_, _, _, _)
        | CmdDeclareSort (_, _)
        | CmdDefineFun _ | CmdDefineFunRec _
        | CmdDefineSort (_, _, _)
        | CmdEcho _ | CmdExit | CmdGetAssertions | CmdGetAssignment
        | CmdGetInfo _ | CmdGetModel | CmdGetOption _ | CmdGetProof
        | CmdGetUnsatAssumptions | CmdGetUnsatCore | CmdGetValue _
        | CmdMetaInfo _ | CmdPop _ | CmdPush _ | CmdReset | CmdResetAssertions
        | CmdSetInfo _ | CmdSetOption _ ->
            aux cmds)
  in
  aux s.script_commands

let rec is_constant_term (t : Smtlib.term) : bool =
  match t.term_desc with
  | TermSpecConstant _ -> true
  | TermAnnotatedTerm (t, _) -> is_constant_term t
  | TermLetTerm _ | TermQualIdentifier _ | TermQualIdentifierTerms _
  | TermForallTerm _ | TermLambdaTerm _ | TermExistsTerm _ ->
      false

let is_variable_term (t : Smtlib.term) : bool = not (is_constant_term t)

let mk_symbol (s : string) =
  { symbol_desc = SimpleSymbol s; symbol_loc = Location.dummy_loc }

let mk_localized_symbol (s : string) (symbol_loc : Location.t) : Smtlib.symbol =
  { symbol_desc = SimpleSymbol s; symbol_loc }

let mk_idx_num i = IdxNum (string_of_int i)

let mk_id_symbol symbol =
  let id_desc = Smtlib.IdSymbol symbol in
  let id_loc = Location.dummy_loc in
  { id_desc; id_loc }

let mk_id_underscore symbol indexes =
  let id_desc = Smtlib.IdUnderscore (symbol, indexes) in
  let id_loc = Location.dummy_loc in
  { id_desc; id_loc }

let mk_qual_identifier_identifier identifier =
  let qual_identifier_desc = Smtlib.QualIdentifierIdentifier identifier in
  let qual_identifier_loc = Location.dummy_loc in
  { qual_identifier_desc; qual_identifier_loc }

let mk_sorted_var symbol sort =
  let sorted_var_desc = Smtlib.SortedVar (symbol, sort) in
  let sorted_var_loc = Location.dummy_loc in
  { sorted_var_desc; sorted_var_loc }

let mk_var_binding symbol term =
  let var_binding_desc = Smtlib.VarBinding (symbol, term) in
  let var_binding_loc = Location.dummy_loc in
  { var_binding_desc; var_binding_loc }

let mk_sort_identifier symbol =
  let sort_desc = Smtlib.SortIdentifier (mk_id_symbol symbol) in
  let sort_loc = Location.dummy_loc in
  { sort_desc; sort_loc }

let mk_sort_fun symbol sorts =
  let sort_desc = Smtlib.SortFun (mk_id_symbol symbol, sorts) in
  let sort_loc = Location.dummy_loc in
  { sort_desc; sort_loc }

let mk_term_spec_constant constant =
  let term_desc = Smtlib.TermSpecConstant constant in
  let term_loc = Location.dummy_loc in
  { term_desc; term_loc }

let mk_term_qual_identifier qual_identifier =
  let term_desc = Smtlib.TermQualIdentifier qual_identifier in
  let term_loc = Location.dummy_loc in
  { term_desc; term_loc }

let mk_term_qual_identifier_terms qual_identifier terms =
  let term_desc = Smtlib.TermQualIdentifierTerms (qual_identifier, terms) in
  let term_loc = Location.dummy_loc in
  { term_desc; term_loc }

let mk_term_let_term var_bindings term =
  let term_desc = Smtlib.TermLetTerm (var_bindings, term) in
  let term_loc = Location.dummy_loc in
  { term_desc; term_loc }

let mk_term_forall_term sorted_vars term =
  let term_desc = Smtlib.TermForallTerm (sorted_vars, term) in
  let term_loc = Location.dummy_loc in
  { term_desc; term_loc }

let mk_term_exists_term sorted_vars term =
  let term_desc = Smtlib.TermExistsTerm (sorted_vars, term) in
  let term_loc = Location.dummy_loc in
  { term_desc; term_loc }

let mk_fun_def symbol sort sorted_vars term =
  let fun_def_desc = Smtlib.FunDef (symbol, None, sorted_vars, sort, term) in
  let fun_def_loc = Location.dummy_loc in
  { fun_def_desc; fun_def_loc }

let mk_cmd_declare_fun symbol sorts sort =
  let command_desc = Smtlib.CmdDeclareFun (symbol, None, sorts, sort) in
  let command_loc = Location.dummy_loc in
  { command_desc; command_loc }

let mk_cmd_define_fun fun_def =
  let command_desc = Smtlib.CmdDefineFun fun_def in
  let command_loc = Location.dummy_loc in
  { command_desc; command_loc }

let mk_command (cmd : Smtlib.command_desc) : Smtlib.command =
  { command_desc = cmd; command_loc = Location.dummy_loc }

let test_model_parsing () =
  let open Smtlib_options in
  if Model_from_file.is_set () then (
    let filename = Model_from_file.get () in
    Logger.debug "Parsing SMT model from %s" filename;
    let parser = Smtlib_parser.model in
    let lexer = Smtlib_lexer.token in
    let premodel = Parse_utils.read_file ~parser ~lexer ~filename in
    Logger.debug "@[Parsed model@\n%a@]" Smtlib_pp.pp_model premodel;
    let model = Smt_model.extract premodel in
    Logger.result "%a" Smt_model.pp model)

let _ = Cli.Boot.enlist ~name:"SMT model parser test" ~f:test_model_parsing
