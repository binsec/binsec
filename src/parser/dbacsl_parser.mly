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
  open Dba

  let is_same_size_op op =
    match op with
    | Dba.Concat | Dba.LShift | Dba.RShiftU
    | Dba.RShiftS  | Dba.LeftRotate | Dba.RightRotate -> false
    | _ -> true


  let infer_binary_op_size left op right =
    if is_same_size_op op then
      let sz1 = Parse_helpers.expr_size left in
      let sz2 = Parse_helpers.expr_size right in
      if sz1 = 0 || sz2 = 0 then
        if sz1 = 0 then
          Dba.ExprBinary(op, Parse_helpers.patch_expr_size left sz2, right)
        else
          Dba.ExprBinary(op, left, Parse_helpers.patch_expr_size right sz1)
      else
        if sz1 <> sz2 then
          match left, right with
          | (_,Dba.ExprCst(`Constant, bv)) -> Dba.ExprBinary(op, left,
          Dba.ExprCst(`Constant, Bitvector.create (Bitvector.value_of bv) sz1))
          | (Dba.ExprCst(`Constant, bv),_) -> Dba.ExprBinary(op,
          Dba.ExprCst(`Constant, Bitvector.create (Bitvector.value_of bv) sz2), right)
          | _ -> Printf.printf "Cannot infer size (mismatch remaining"; Dba.ExprBinary(op, left, right)
        else
          Dba.ExprBinary(op, left, right)
    else
      Dba.ExprBinary (op, left, right)

%}


%token UMINUS PLUS MINUS MULTU MULTS DIVU DIVS /*POW*/ MODU MODS /*NONDET*/ STORELOAD
%token AND OR XOR NOT
%token CONCAT /*COLON SEMICOLON*/ LSHIFT RSHIFTU RSHIFTS LROTATE RROTATE EXTU EXTS INFER SUPER
%token EQQ NEQ LEU LES LTU LTS GEU GES GTU GTS
%token LBRACE RBRACE LPAR RPAR LBRACKET RBRACKET COMMA /*NEXT*/
%token /*ASSIGN*/ TRUE FALSE IFJUMP ELSE /*ARRAY*/
%token EOF
/*%token INTERVALSEP DCOLON TOACTION WILDCARD SUBTERM DEFAULT SYMB CONC PROP PROPC PROPS AND2 OR2 TAINTCHECK NOT2 TAINTI TAINTP*/
%token TERM_TOKEN ASSUME ASSERT EQQ2 NEQ2

%token <string> INT
%token <string> IDENT
%token <string * int> HEXA
/*%token <string> STRING*/

%left  EQQ NEQ LEU LES LTU LTS GEU GES GTU GTS
%left CONCAT
%left LSHIFT RSHIFTU RSHIFTS LROTATE RROTATE /*EXTU EXTS*/
%left PLUS MINUS
%left MULTU MULTS DIVU DIVS MODU MODS
%left AND OR XOR
/*%left POW*/


%left UMINUS
%nonassoc NOT

%start term
%type <Dba.instruction> term

%%

term:
  | TERM_TOKEN ASSUME cond EOF { Dba.IkAssume($3, 0) }
  | TERM_TOKEN ASSUME LPAR cond RPAR EOF { Dba.IkAssume($4, 0) }
  | TERM_TOKEN ASSERT cond EOF { Dba.IkAssert ($3, 0) }
  | TERM_TOKEN ASSERT LPAR cond RPAR EOF { Dba.IkAssert($4, 0) }
  | cond EOF { Dba.IkAssert($1, 0) }

expr:
  | INT INFER INT SUPER {
    let size = int_of_string $3 in
    let bigint = (Bigint.big_int_of_string $1) in
    let bv = Bitvector.create bigint size in
    Dba.ExprCst (`Constant, bv)
  }
  | INT {
    let bigint = (Bigint.big_int_of_string $1) in
    Dba.ExprCst (`Constant, (Bitvector.create bigint 0))
  }
  | HEXA {
    let s, size = $1 in
    let bigint = Bigint.big_int_of_string s in
    let bv = Bitvector.create bigint size in
    Dba.ExprCst (`Constant, bv)
  }
  | IDENT {
    let var = Parse_helpers.expr_of_name $1 in
    var
  }
  | STORELOAD LBRACKET expr COMMA INT RBRACKET {
    let size = int_of_string $5 in
    Dba.ExprLoad (size/8, LittleEndian, $3)
  }
  | STORELOAD LBRACKET expr RBRACKET {
    Dba.ExprLoad (0, LittleEndian, $3)
  }
  | NOT expr %prec NOT { Dba.ExprUnary (Dba.Not, $2) }
  | MINUS expr %prec UMINUS { Dba.ExprUnary (Dba.UMinus, $2) }
  | LBRACE expr COMMA INT COMMA INT RBRACE {
    let off1 = int_of_string $4 in
    let off2 = int_of_string $6 in
    Dba.ExprRestrict ($2, off1, off2)
  }
  | EXTU expr INT {
    let size = int_of_string $3 in
    Dba.ExprExtU ($2, size)
  }
  | EXTS expr INT {
    let size = int_of_string $3 in
    Dba.ExprExtS ($2, size)
  }
  | IFJUMP cond expr ELSE expr { Dba.ExprIte ($2, $3, $5) }
  | LPAR expr RPAR { $2 }
  | expr bin_op expr {
    infer_binary_op_size $1 $2 $3
  }
  | LPAR expr bin_op expr RPAR {
    infer_binary_op_size $2 $3 $4
  }

bin_op :
 | MODU { Dba.ModU }
 | MODS { Dba.ModS }
 | OR  { Dba.Or }
 | AND { Dba.And }
 | XOR { Dba.Xor }
 | PLUS { Dba.Plus }
 | MINUS { Dba.Minus }
 | MULTU { Dba.MultU }
 | MULTS { Dba.MultS }
 | DIVU { Dba.DivU }
 | DIVS { Dba.DivS }

 | CONCAT { Dba.Concat }

 | EQQ { Dba.Eq }
 | EQQ2 { Dba.Eq }
 | NEQ { Dba.Diff }
 | NEQ2 { Dba.Diff }
 | LEU { Dba.LeqU }
 | LTU { Dba.LtU }
 | GEU { Dba.GeqU }
 | GTU { Dba.GtU }
 | LES { Dba.LeqS }
 | LTS { Dba.LtS }
 | GES { Dba.GeqS }
 | GTS { Dba.GtS }

 | LSHIFT  { Dba.LShift }
 | RSHIFTU  { Dba.RShiftU }
 | RSHIFTS   { Dba.RShiftS }
 | LROTATE  { Dba.LeftRotate }
 | RROTATE  { Dba.RightRotate }

cond :
 | TRUE { Dba.True }
 | FALSE { Dba.False }
 | expr { Dba.CondReif $1 }
