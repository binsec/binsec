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

(* Pretty printer for the AST *)
open Format
open Lexing
open Location
open Smtlib

let pp_loc ppf loc =
  let b = loc.loc_start and e = loc.loc_end in
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  fprintf ppf "L%d, C%d-%d" l fc lc

let pp_list pp_f ppf elts =
  let rec pp_list_aux ppf = function
    | [] -> ()
    | [ e ] -> fprintf ppf "%a" pp_f e
    | e :: es -> fprintf ppf "%a@ %a" pp_f e pp_list_aux es
  in
  pp_list_aux ppf elts

let rec pp_list_k pp_f_k k ppf = function
  | [] -> k ()
  | [ e ] -> pp_f_k k ppf e
  | e :: es ->
      pp_f_k
        (fun () ->
          pp_print_space ppf ();
          pp_list_k pp_f_k k ppf es)
        ppf e

let pp_numeral ppf n = fprintf ppf "%s" n

let pp_symbol ppf symb =
  match symb.symbol_desc with
  | SimpleSymbol s -> fprintf ppf "%s" s
  | QuotedSymbol s -> fprintf ppf "|%s|" s

let pp_symbols ppf symbs = fprintf ppf "%a" (pp_list pp_symbol) symbs
let pp_keyword ppf kwd = fprintf ppf ":%s" kwd

let pp_spec_constant ppf = function
  | CstBool b -> fprintf ppf "%b" b
  | CstNumeral n -> fprintf ppf "%s" n
  | CstBinary str -> fprintf ppf "#b%s" str
  | CstDecimal str -> fprintf ppf "%s" str
  | CstDecimalSize (str, size) -> fprintf ppf "(_ bv%s %s)" str size
  | CstHexadecimal str -> fprintf ppf "#x%s" str
  | CstString str -> fprintf ppf "\"%s\"" str

let pp_idx ppf idx =
  match idx with
  | IdxNum n -> pp_numeral ppf n
  | IdxSymbol symb -> pp_symbol ppf symb

let pp_indexes ppf indexes = pp_list pp_idx ppf indexes

let pp_identifier ppf identifier =
  match identifier.id_desc with
  | IdSymbol symb -> pp_symbol ppf symb
  | IdUnderscore (symb, indexes) ->
      fprintf ppf "@[<hov 2>(_@ %a@ %a)@]" pp_symbol symb pp_indexes indexes

let rec pp_sort ppf sort =
  match sort.sort_desc with
  | SortIdentifier id -> pp_identifier ppf id
  | SortFun (id, sorts) ->
      fprintf ppf "@[<hov 2>(%a@ %a)@]" pp_identifier id pp_sorts sorts

and pp_sorts ppf sorts = pp_list pp_sort ppf sorts

let pp_qual_identifier ppf qualid =
  match qualid.qual_identifier_desc with
  | QualIdentifierIdentifier id -> fprintf ppf "%a" pp_identifier id
  | QualIdentifierAs (id, sort) ->
      fprintf ppf "@[<hov 2>(as@ %a@ %a)@]" pp_identifier id pp_sort sort

let pp_sorted_var ppf svar =
  match svar.sorted_var_desc with
  | SortedVar (symb, sort) ->
      fprintf ppf "@[<hov 2>(%a@ %a)@]" pp_symbol symb pp_sort sort

let pp_sorted_vars ppf svars = fprintf ppf "%a" (pp_list pp_sorted_var) svars

let rec pp_sexpr ppf sexpr =
  match sexpr.sexpr_desc with
  | SexprConstant sc -> pp_spec_constant ppf sc
  | SexprSymbol symb -> pp_symbol ppf symb
  | SexprKeyword kwd -> pp_keyword ppf kwd
  | SexprParens sexprs -> fprintf ppf "@[<hov 2>(%a)@]" pp_sexprs sexprs

and pp_sexprs ppf sexprs = pp_list pp_sexpr ppf sexprs

let pp_attribute_value ppf attr_value =
  match attr_value.attr_value_desc with
  | AttrValSymbol symb -> pp_symbol ppf symb
  | AttrValSpecConstant sc -> pp_spec_constant ppf sc
  | AttrValSexpr sexprs -> fprintf ppf "@[<hov 2>(%a)@]" pp_sexprs sexprs

let pp_attribute ppf attribute =
  match attribute.attribute_desc with
  | AttrKeyword kwd -> pp_keyword ppf kwd
  | AttrKeywordValue (kwd, value) ->
      fprintf ppf "%a %a" pp_keyword kwd pp_attribute_value value

let pp_attributes ppf attributes = pp_list pp_attribute ppf attributes

let rec pp_term_k k ppf term =
  match term.term_desc with
  | TermSpecConstant sc ->
      pp_spec_constant ppf sc;
      k ()
  | TermQualIdentifier qualid ->
      pp_qual_identifier ppf qualid;
      k ()
  | TermQualIdentifierTerms (qualid, terms) ->
      fprintf ppf "@[<hov 0>(%a@ " pp_qual_identifier qualid;
      pp_terms_k
        (fun () ->
          fprintf ppf ")@]";
          k ())
        ppf terms
  | TermLetTerm (varbindings, term) ->
      fprintf ppf "@[<hov 0>(let@ (@[<v 0>";
      pp_var_bindings_k
        (fun () ->
          fprintf ppf "@])@ ";
          pp_term_k
            (fun () ->
              fprintf ppf ")@]";
              k ())
            ppf term)
        ppf varbindings
  | TermForallTerm (svars, term) ->
      fprintf ppf "@[<hov 1>(forall@ (@[<hov>%a@])@ " pp_sorted_vars svars;
      pp_term_k
        (fun () ->
          fprintf ppf ")@]";
          k ())
        ppf term
  | TermExistsTerm (svars, term) ->
      fprintf ppf "@[<hov 1>(exists@ (@[<hov>%a@])@ " pp_sorted_vars svars;
      pp_term_k
        (fun () ->
          fprintf ppf ")@]";
          k ())
        ppf term
  | TermAnnotatedTerm (term, attrs) ->
      fprintf ppf "@[<hov 1>(!@ ";
      pp_term_k
        (fun () ->
          fprintf ppf " %a)@]" pp_attributes attrs;
          k ())
        ppf term
  | TermLambdaTerm (svars, term) ->
      fprintf ppf "@[<hov 1>(lambda@ (@[<hov>%a@])@ " pp_sorted_vars svars;
      pp_term_k
        (fun () ->
          fprintf ppf ")@]";
          k ())
        ppf term

and pp_terms_k k ppf terms = pp_list_k pp_term_k k ppf terms

and pp_var_binding_k k ppf vbinding =
  match vbinding.var_binding_desc with
  | VarBinding (symb, term) ->
      fprintf ppf "@[<hov 1>(%a@ " pp_symbol symb;
      pp_term_k
        (fun () ->
          fprintf ppf ")@]";
          k ())
        ppf term

and pp_var_bindings_k k ppf vbindings =
  pp_list_k pp_var_binding_k k ppf vbindings

let pp_term = pp_term_k (fun () -> ())
let pp_terms = pp_terms_k (fun () -> ())

let pp_opt_type_parameters ppf optsorts =
  match optsorts with
  | Some sorts -> fprintf ppf "par@ (%a)@ " pp_sorts sorts
  | None -> ()

let pp_fun_def ppf fun_def =
  match fun_def.fun_def_desc with
  | FunDef (symb, optsorts, svars, sort, term) ->
      fprintf ppf "%a@ %a(%a)@ %a@ %a" pp_symbol symb pp_opt_type_parameters
        optsorts pp_sorted_vars svars pp_sort sort pp_term term

let pp_fun_rec_def ppf fun_rec_def =
  match fun_rec_def.fun_rec_def_desc with
  | FunRecDef (symb, optsorts, svars, sort, term) ->
      fprintf ppf "(%a@ %a(%a)@ %a@ %a)" pp_symbol symb pp_opt_type_parameters
        optsorts pp_sorted_vars svars pp_sort sort pp_term term

let pp_fun_rec_defs ppf fun_rec_defs =
  fprintf ppf "@[<v 0>";
  List.iter (fun frdef -> pp_fun_rec_def ppf frdef) fun_rec_defs;
  fprintf ppf "@]"

let pp_info_flag ppf info_flag =
  match info_flag.info_flag_desc with
  | InfoFlagKeyword kwd -> pp_keyword ppf kwd

let pp_smt_option ppf smt_option =
  match smt_option.smt_option_desc with
  | OptionAttribute attr -> pp_attribute ppf attr

let pp_command ppf cmd =
  match cmd.command_desc with
  | CmdAssert t -> fprintf ppf "@[<hov 0>(assert@ %a)@]" pp_term t
  | CmdCheckSat -> fprintf ppf "(check-sat)"
  | CmdCheckSatAssuming symbs ->
      fprintf ppf "(check-sat-assuming@ (%a))" pp_symbols symbs
  | CmdComment str -> fprintf ppf ";; %s@\n" str
  | CmdDeclareConst (symb, sort) ->
      fprintf ppf "@[<hov 2>(declare-const@ %a@ %a)@]" pp_symbol symb pp_sort
        sort
  | CmdDeclareFun (symb, _, sorts, sort) ->
      fprintf ppf "@[<hov 2>(declare-fun@ %a@ (%a)@ %a)@]" pp_symbol symb
        pp_sorts sorts pp_sort sort
  | CmdDefineFun fundef ->
      fprintf ppf "@[<hov 2>(define-fun@ %a)@]" pp_fun_def fundef
  | CmdDefineFunRec funrecdefs ->
      fprintf ppf "@[<hov 2>(define-fun@ (%a))@]" pp_fun_rec_defs funrecdefs
  | CmdDefineSort (symb, symbs, sort) ->
      fprintf ppf "(define-sort@ %a@ (%a)@ %a)" pp_symbol symb pp_symbols symbs
        pp_sort sort
  | CmdDeclareSort (symb, num) ->
      fprintf ppf "(declare-sort@ %a@ %a)" pp_symbol symb pp_numeral num
  | CmdGetAssertions -> fprintf ppf "(get-assertions)"
  | CmdGetAssignment -> fprintf ppf "(get-assignment)"
  | CmdExit -> fprintf ppf "(exit)"
  | CmdEcho s -> fprintf ppf "(echo@ \"%s\")" s
  | CmdGetModel -> fprintf ppf "(get-model)"
  | CmdGetProof -> fprintf ppf "(get-proof)"
  | CmdGetUnsatCore -> fprintf ppf "(get-unsat-core)"
  | CmdGetInfo infoflag -> fprintf ppf "(get-info@ %a)" pp_info_flag infoflag
  | CmdGetOption kwd -> fprintf ppf "(get-option@ %a)" pp_keyword kwd
  | CmdGetUnsatAssumptions -> fprintf ppf "(get-unsat-assumptions)"
  | CmdGetValue terms -> fprintf ppf "(get-value@ (%a))" pp_terms terms
  | CmdMetaInfo attr -> fprintf ppf "(meta-info@ %a)" pp_attribute attr
  | CmdPop (Some n) -> fprintf ppf "(pop@ %a)" pp_numeral n
  | CmdPush (Some n) -> fprintf ppf "(push@ %a)" pp_numeral n
  | CmdPop None -> fprintf ppf "(pop)"
  | CmdPush None -> fprintf ppf "(push)"
  | CmdReset -> fprintf ppf "(reset)"
  | CmdResetAssertions -> fprintf ppf "(reset-assertions)"
  | CmdSetInfo attr -> fprintf ppf "(set-info@ %a)" pp_attribute attr
  | CmdSetLogic symb -> fprintf ppf "(set-logic@ %a)" pp_symbol symb
  | CmdSetOption smt_option ->
      fprintf ppf "(set-option@ %a)" pp_smt_option smt_option

let pp_command_list ppf l =
  (* All commands are delimited by (). This is printed here. *)
  List.iter (fun cmd -> Format.fprintf ppf "@[<hov 2>%a@]@ " pp_command cmd) l

let pp_commands ppf cmds = Format.fprintf ppf "@[<v 0>%a@]" pp_command_list cmds

let pp ppf (s : script) =
  Format.set_max_indent 25;
  pp_commands ppf s.script_commands

let pp_tofile filename program =
  let oc = open_out_bin filename in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf "%a@." pp program;
  close_out oc

let pp_model ppf model =
  Format.fprintf ppf "@[(@[<v 1>model@ %a@])@]" pp_commands model.model_commands
