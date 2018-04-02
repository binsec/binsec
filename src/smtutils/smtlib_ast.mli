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

(** AST for SMT-LIB *)
type numeral = string ;;

type constant =
  | CstNumeral of numeral (* Better to use a string than an int here as
                           * int_of_string can fail *)
  | CstDecimal of string
  | CstHexadecimal of string
  | CstBinary of string
  | CstString of string
  | CstBool of bool
;;

type symbol_desc =
  | SimpleSymbol of string
  | QuotedSymbol of string
;;

type symbol = {
  symbol_desc : symbol_desc;
  symbol_loc  : Locations.t;
}
;;

type symbols = symbol list ;;

type keyword = string ;;

type sexpr_desc =
  | SexprConstant of constant
  | SexprSymbol of symbol
  | SexprKeyword of keyword
  | SexprParens of sexprs

and sexpr = {
  sexpr_desc: sexpr_desc;
  sexpr_loc: Locations.t
}

and sexprs = sexpr list
;;

type index =
  | IdxNum of numeral
  | IdxSymbol of symbol
;;

type indexes = index list ;;

type info_flag_desc =
  | InfoFlagKeyword of keyword
;;

type info_flag = {
  info_flag_desc : info_flag_desc;
  info_flag_loc  : Locations.t;
}
;;

type id_desc =
  | IdSymbol of symbol
  | IdUnderscore of symbol * indexes
;;

type identifier = {
  id_desc : id_desc;
  id_loc  : Locations.t;
}
;;

type attr_value_desc =
  | AttrValSpecConstant of constant
  | AttrValSymbol of symbol
  | AttrValSexpr of sexprs
;;

type attr_value = {
  attr_value_desc : attr_value_desc;
  attr_value_loc  : Locations.t;
}
;;

type attribute_desc =
  | AttrKeyword of keyword
  | AttrKeywordValue of keyword * attr_value
;;

type attribute = {
  attribute_desc : attribute_desc;
  attribute_loc  : Locations.t;
}
;;

type attributes = attribute list ;;

type smt_option_desc =
  | OptionAttribute of attribute
;;

type smt_option = {
  smt_option_desc : smt_option_desc;
  smt_option_loc  : Locations.t;
}
;;

type sort = {
  sort_desc : sort_desc;
  sort_loc  : Locations.t;
}

and sorts = sort list

and sort_desc =
  | SortIdentifier of identifier
  | SortFun of identifier * sorts (* SMT-LIB: there must be at least one sort *)
;;

type qual_identifier_desc =
  | QualIdentifierIdentifier of identifier
  | QualIdentifierAs of identifier * sort
;;

type qual_identifier = {
  qual_identifier_desc : qual_identifier_desc;
  qual_identifier_loc  : Locations.t;
}
;;

type sorted_var_desc = SortedVar of symbol * sort ;;

type sorted_var = {
  sorted_var_desc : sorted_var_desc;
  sorted_var_loc  : Locations.t;
}
;;

type sorted_vars = sorted_var list ;;

type var_binding_desc = VarBinding of symbol * term

and var_binding = {
  var_binding_desc : var_binding_desc;
  var_binding_loc  : Locations.t
}

and var_bindings = var_binding list

and term_desc =
  | TermSpecConstant of constant
  | TermQualIdentifier of qual_identifier
  | TermQualIdentifierTerms of qual_identifier * terms
  | TermLetTerm of var_bindings * term
  | TermForallTerm of sorted_vars * term
  | TermExistsTerm of sorted_vars * term
  | TermAnnotatedTerm of term * attributes

and terms = term list

and term = {
  term_desc : term_desc;
  term_loc  : Locations.t;
}
;;

(* [sorted_vars] can be empty *)
type fun_def_desc =
  | FunDef of symbol * sorts option * sorted_vars * sort * term
;;

type fun_def = {
  fun_def_desc : fun_def_desc;
  fun_def_loc  : Locations.t;
  (* fun_is_rec   : boolean; *)
}
;;

type fun_rec_def_desc =
  | FunRecDef of symbol * sorts option * sorted_vars * sort * term
;;

type fun_rec_def = {
  fun_rec_def_desc : fun_rec_def_desc;
  fun_rec_def_loc  : Locations.t;
  (* fun_is_rec   : boolean; *)
}
;;

type fun_rec_defs = fun_rec_def list ;;

type command_desc =
  | CmdAssert of term
  | CmdCheckSat
  | CmdCheckSatAssuming of symbols
  | CmdDeclareConst of symbol * sort
  | CmdDeclareFun of symbol * sorts option * sorts * sort
  | CmdDeclareSort of symbol * numeral
  | CmdDefineFun of fun_def
  | CmdDefineFunRec of fun_rec_defs
  | CmdDefineSort of symbol * symbols * sort (* symbols can be empty *)
  | CmdEcho of string
  | CmdExit
  | CmdGetAssertions
  | CmdGetAssignment
  | CmdGetInfo of info_flag
  | CmdGetModel
  | CmdGetOption of keyword
  | CmdGetProof
  | CmdGetUnsatAssumptions
  | CmdGetUnsatCore
  | CmdGetValue of terms (* at least one term *)
  | CmdMetaInfo of attribute
  | CmdPop of numeral option
  | CmdPush of numeral option
  | CmdReset
  | CmdResetAssertions
  | CmdSetInfo of attribute
  | CmdSetLogic of symbol
  | CmdSetOption of smt_option

type command = {
  command_desc : command_desc;
  command_loc  : Locations.t;
}
;;

type commands = command list ;;

type script = {
  script_commands : commands;
  script_loc      : Locations.t;
}
;;


type model = {
  model_commands : commands;
  model_loc : Locations.t ;
}
