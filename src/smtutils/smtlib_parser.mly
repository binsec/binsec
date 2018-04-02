/**************************************************************************/
/*  This file is part of Binsec.                                          */
/*                                                                        */
/*  Copyright (C) 2016-2017                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

%{
    open Smtlib_ast
    open Locations ;;

    (* Helper construction functions.
       File locations is handled in production rules.
       *)
    let mk_sexpr sexpr_desc sexpr_loc = { sexpr_desc; sexpr_loc; } ;;
    let mk_identifier id_desc id_loc = { id_desc; id_loc; } ;;

    let mk_sort sort_desc sort_loc = { sort_desc; sort_loc; } ;;

    let mk_command command_desc command_loc =
      { command_desc; command_loc; }
    ;;

    let mk_fun_def fun_def_desc fun_def_loc =
      { fun_def_desc; fun_def_loc; }
    ;;

    let mk_fun_rec_def fun_rec_def_desc fun_rec_def_loc =
      { fun_rec_def_desc; fun_rec_def_loc; }
    ;;

    let mk_sorted_var sorted_var_desc sorted_var_loc =
      { sorted_var_desc; sorted_var_loc; }
    ;;

    let mk_qual_identifier qual_identifier_desc qual_identifier_loc =
      { qual_identifier_desc; qual_identifier_loc; }
    ;;

    let mk_var_binding var_binding_desc var_binding_loc =
      { var_binding_desc; var_binding_loc; }
    ;;

    let mk_term term_desc term_loc = { term_desc; term_loc; } ;;

    let mk_smt_option smt_option_desc smt_option_loc = {
        smt_option_desc; smt_option_loc ;
      }
    ;;

    let mk_script script_commands script_loc =
      { script_commands; script_loc; }
    ;;

    let mk_model model_commands model_loc = { model_commands; model_loc; }
    let mk_attribute attribute_desc attribute_loc =
      { attribute_desc; attribute_loc; }
    ;;

    let mk_attr_value attr_value_desc attr_value_loc =
      { attr_value_desc; attr_value_loc; }
    ;;

    let mk_info_flag info_flag_desc info_flag_loc =
      { info_flag_desc; info_flag_loc; }
    ;;

    let mk_symbol symbol_desc symbol_loc = { symbol_desc; symbol_loc; } ;;
%}

%start script

/* general */
%token BANG
%token UNDERSCORE
%token AS
%token EXISTS
%token FORALL
%token LET

/* commands */
%token SETLOGIC
%token SETOPTION
%token SETINFO
%token DECLARESORT
%token DEFINESORT
%token DECLAREFUN
%token DECLARECONST
%token DEFINEFUN
%token DEFINEFUNREC
%token PAR
/* %token LAMBDA */
%token PUSH
%token POP
%token ASSERT
%token CHECKSAT
%token GETASSERTIONS
%token GETPROOF
%token GETUNSATCORE
%token GETVALUE
%token GETASSIGNMENT
%token GETUNSATASSUMPTIONS
%token GETOPTION
%token GETINFO
%token GETMODEL
%token EXIT
%token ECHO
%token RESET
%token RESETASSERTIONS
%token METAINFO
%token MODEL

/* Other tokens */
%token LPAREN
%token RPAREN
%token EOF

%token <Smtlib_ast.numeral> NUMERAL
%token <string> DECIMAL
%token <string> HEXADECIMAL
%token <string> BINARY
%token <string> STRING
%token <bool>   BOOL
%token <string> KEYWORD
%token <string> SYMBOL
%token <string> QUOTEDSYMBOL

%type  <Smtlib_ast.script> script
%type  <Smtlib_ast.model>  model

%start script
%start model
%%

script:
| commands=delimited(LPAREN,command,RPAREN)*; EOF
  { let loc = mk_loc $startpos $endpos in
    mk_script commands loc }
;

model :
| commands=delimited(LPAREN,
     preceded(MODEL, list(delimited(LPAREN,command,RPAREN))),
     RPAREN); EOF
  { let loc = mk_loc $startpos $endpos in
    mk_model commands loc
   }        
;

%inline command:
| ASSERT t=term;
  { let loc = mk_loc $startpos $endpos in mk_command (CmdAssert t) loc }
| CHECKSAT
  { let loc = mk_loc $startpos $endpos in mk_command CmdCheckSat loc }
| DECLARECONST symb=symbol; so=sort;
  { let loc = mk_loc $startpos $endpos in
    mk_command (CmdDeclareConst(symb, so)) loc }
| DECLAREFUN symb=symbol; polys=option(poly_parameters);
  LPAREN sorts=sort* RPAREN so=sort;
  { let loc = mk_loc $startpos $endpos in
    mk_command (CmdDeclareFun (symb, polys, sorts, so)) loc }
| DECLARESORT symb=symbol; num=NUMERAL;
  { let loc = mk_loc $startpos $endpos in
    mk_command (CmdDeclareSort(symb, num)) loc }
| DEFINEFUN fdef=fun_nonrec_def; 
 { let loc = mk_loc $startpos $endpos in
   mk_command (CmdDefineFun fdef) loc }
| DEFINEFUNREC LPAREN frdefs=fun_rec_def+; RPAREN
 { let loc = mk_loc $startpos $endpos in
   mk_command (CmdDefineFunRec frdefs) loc }
| DEFINESORT symb=symbol; LPAREN symbs=symbol+ RPAREN so=sort;
  { let loc = mk_loc $startpos $endpos in
    mk_command (CmdDefineSort (symb, symbs, so)) loc }
| ECHO s=STRING;
  { let loc = mk_loc $startpos $endpos in
    mk_command (CmdEcho s) loc }
| EXIT
  { let loc = mk_loc $startpos $endpos in
    mk_command CmdExit loc }
| GETASSERTIONS
    { let loc = mk_loc $startpos $endpos in
      mk_command CmdGetAssertions loc }
| GETASSIGNMENT
    { let loc = mk_loc $startpos $endpos in
      mk_command CmdGetAssignment loc }
| GETINFO iflag=info_flag;
  { let loc = mk_loc $startpos $endpos in
    mk_command (CmdGetInfo iflag) loc }
| GETMODEL
    { let loc = mk_loc $startpos $endpos in
      mk_command CmdGetModel loc }
| GETOPTION kwd=KEYWORD;
  { let loc = mk_loc $startpos $endpos in
    mk_command (CmdGetOption kwd) loc }
| RESET
    { let loc = mk_loc $startpos $endpos in
      mk_command CmdReset loc }
| RESETASSERTIONS
    { let loc = mk_loc $startpos $endpos in
      mk_command CmdResetAssertions loc }
| GETPROOF
    { let loc = mk_loc $startpos $endpos in
      mk_command CmdGetProof loc }
| GETUNSATCORE
    { let loc = mk_loc $startpos $endpos in
      mk_command CmdGetUnsatCore loc }
| GETUNSATASSUMPTIONS
    { let loc = mk_loc $startpos $endpos in
      mk_command CmdGetUnsatAssumptions loc }
| GETVALUE LPAREN ts=term+; RPAREN;
  { let loc = mk_loc $startpos $endpos in
    mk_command (CmdGetValue ts) loc }
| METAINFO attr=attribute;
  { let loc = mk_loc $startpos $endpos in
    mk_command (CmdMetaInfo attr) loc }
| POP num=option(NUMERAL);
  { let loc = mk_loc $startpos $endpos in
    mk_command (CmdPop num) loc }
| PUSH num=option(NUMERAL);
  { let loc = mk_loc $startpos $endpos in
    mk_command (CmdPush num) loc }
| SETINFO attr=attribute;
  { let loc = mk_loc $startpos $endpos in
    mk_command (CmdSetInfo attr) loc }
| SETLOGIC symb=symbol;
  { let loc = mk_loc $startpos $endpos in
    mk_command (CmdSetLogic symb) loc }
| SETOPTION sopt=smt_option;
  { let loc = mk_loc $startpos $endpos in
    mk_command (CmdSetOption sopt) loc }
;

fun_def:
| symb=symbol; polys=option(poly_parameters);
  LPAREN svars=sorted_var*; RPAREN so=sort; t=term;
  { (symb, polys, svars, so, t) }
  ;

fun_nonrec_def:
| fd=fun_def;
  { let loc = mk_loc $startpos $endpos in
    let s, ps, svs, so, t = fd in
    mk_fun_def (FunDef (s, ps, svs, so, t)) loc }
;

fun_rec_def:
| LPAREN fd=fun_def RPAREN
 { let s, ps, svs, so, t = fd in
   let loc = mk_loc $startpos $endpos in
   mk_fun_rec_def (FunRecDef (s, ps, svs, so, t)) loc }
 ;

poly_parameters:
| PAR LPAREN sorts=sort+; RPAREN { sorts }
;

sorted_var:
| LPAREN symb=symbol; so=sort; RPAREN
  { let loc = mk_loc $startpos $endpos in
    mk_sorted_var (SortedVar (symb, so)) loc }
;

sort:
| id=identifier;
  { let loc = mk_loc $startpos $endpos in mk_sort (SortIdentifier id) loc }
| LPAREN id=identifier; sorts=sort+; RPAREN
  { let loc = mk_loc $startpos $endpos in mk_sort (SortFun (id, sorts)) loc }
;

index:
| NUMERAL { IdxNum $1 }
| SYMBOL
  { let loc = mk_loc $startpos $endpos in
    IdxSymbol (mk_symbol (SimpleSymbol $1) loc) }
;

identifier:
| symb=symbol
  { let loc = mk_loc $startpos $endpos in mk_identifier (IdSymbol symb) loc }
| LPAREN UNDERSCORE symb=symbol; indexes=index+; RPAREN
  { let loc = mk_loc $startpos $endpos in
    mk_identifier (IdUnderscore (symb, indexes)) loc }
;

symbol:
| SYMBOL
  { let loc = mk_loc $startpos $endpos in mk_symbol (SimpleSymbol $1) loc }
| QUOTEDSYMBOL
  { let loc = mk_loc $startpos $endpos in mk_symbol (QuotedSymbol $1) loc }
;

term:
| spec_constant
 { let loc = mk_loc $startpos $endpos in
   mk_term (TermSpecConstant $1) loc }
| qual_identifier
 { let loc = mk_loc $startpos $endpos in
   mk_term (TermQualIdentifier $1) loc }
| LPAREN qualid=qual_identifier; ts=term+; RPAREN
 { let loc = mk_loc $startpos $endpos in
   mk_term (TermQualIdentifierTerms(qualid, ts)) loc }
| LPAREN LET LPAREN vbindings=var_binding+; RPAREN t=term; RPAREN
 { let loc = mk_loc $startpos $endpos in
   mk_term (TermLetTerm (vbindings, t)) loc }
| LPAREN FORALL LPAREN svars=sorted_var+; RPAREN t=term; RPAREN
 { let loc = mk_loc $startpos $endpos in
   mk_term (TermForallTerm (svars, t)) loc}
| LPAREN EXISTS  LPAREN svars=sorted_var+; RPAREN t=term; RPAREN
 { let loc = mk_loc $startpos $endpos in
   mk_term (TermExistsTerm (svars, t)) loc }
| LPAREN BANG t=term; attrs=attribute+ RPAREN
 { let loc = mk_loc $startpos $endpos in
   mk_term (TermAnnotatedTerm(t, attrs)) loc }
;

qual_identifier:
| id=identifier;
  { let loc = mk_loc $startpos $endpos in
    mk_qual_identifier (QualIdentifierIdentifier id) loc }
| LPAREN AS id=identifier; so=sort; RPAREN
  { let loc = mk_loc $startpos $endpos in
    mk_qual_identifier (QualIdentifierAs (id, so)) loc }
;

var_binding:
| LPAREN symb=symbol; t=term; RPAREN
  { let loc = mk_loc $startpos $endpos in
    mk_var_binding (VarBinding (symb, t)) loc }
;

spec_constant:
| BINARY  { CstBinary $1 }
| DECIMAL { CstDecimal $1 }
| NUMERAL { CstNumeral $1 }
| STRING  { CstString $1 }
| BOOL    { CstBool $1 }
| HEXADECIMAL { CstHexadecimal $1 }
;

attribute_value:
| sc=spec_constant;
  { let loc = mk_loc $startpos $endpos in
    mk_attr_value (AttrValSpecConstant sc) loc }
| symb=symbol;
  { let loc = mk_loc $startpos $endpos in
    mk_attr_value (AttrValSymbol symb) loc }
| LPAREN sexprs=sexpr*; RPAREN
 { let loc = mk_loc $startpos $endpos in
   mk_attr_value (AttrValSexpr sexprs) loc }
;

sexpr:
| sc=spec_constant;
  { let loc = mk_loc $startpos $endpos in
    mk_sexpr (SexprConstant sc) loc }
| symb=symbol;
  { let loc = mk_loc $startpos $endpos in
    mk_sexpr (SexprSymbol symb) loc }
| kwd=KEYWORD;
  { let loc = mk_loc $startpos $endpos in
    mk_sexpr (SexprKeyword kwd) loc }
| LPAREN sexprs=sexpr*; RPAREN
 { let loc = mk_loc $startpos $endpos in
   mk_sexpr (SexprParens sexprs) loc }
;

attribute:
| kwd=KEYWORD;
  { let loc = mk_loc $startpos $endpos in
    mk_attribute (AttrKeyword kwd) loc }
| kwd=KEYWORD; attrval=attribute_value;
  { let loc = mk_loc $startpos $endpos in
    mk_attribute (AttrKeywordValue (kwd, attrval)) loc }
;

smt_option:
| attr=attribute
  { let loc = mk_loc $startpos $endpos in
    mk_smt_option (OptionAttribute attr) loc }
;

info_flag:
| kwd=KEYWORD
  { let loc = mk_loc $startpos $endpos in
    mk_info_flag (InfoFlagKeyword kwd) loc }
;