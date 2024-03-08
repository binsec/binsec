/**************************************************************************/
/*  This file is part of BINSEC.                                          */
/*                                                                        */
/*  Copyright (C) 2016-2024                                               */
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
    let open Dba.Binary_op in
    match op with
    | Concat | LShift | RShiftU
    | RShiftS  | LeftRotate | RightRotate -> false
    | _ -> true


  let infer_binary_op_size left op right =
    let open Dba.Expr in
    let default () = binary op left right in
    if is_same_size_op op then
      let sz1 = Dba.Expr.size_of left in
      let sz2 = Dba.Expr.size_of right in
      if sz1 = 0 || sz2 = 0 then
        if sz1 = 0 then
          binary op (Parse_helpers.patch_expr_size left sz2) right
        else
          binary op left (Parse_helpers.patch_expr_size right sz1)
      else
        if sz1 <> sz2 then
          match left, right with
          | (_,Dba.Expr.Cst bv) ->
             binary op left
                    (constant (Bitvector.create (Bitvector.value_of bv) sz1))
          | (Dba.Expr.Cst bv,_) ->
             binary op
                    (constant (Bitvector.create (Bitvector.value_of bv) sz2)) right
          | _ -> Printf.printf "Cannot infer size (mismatch remaining"; default ()
        else default ()
    else default ()

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
%type <Dba.Instr.t> term

%%

term:
  | TERM_TOKEN ASSUME c=cond; EOF
  | TERM_TOKEN ASSUME LPAR c=cond; RPAR EOF { Dba.Instr.assume c 0 }
  | TERM_TOKEN ASSERT c=cond; EOF
  | TERM_TOKEN ASSERT LPAR c=cond; RPAR EOF
  | c=cond EOF { Dba.Instr._assert c 0 }
;

expr:
  | INT INFER INT SUPER {
    let size = int_of_string $3 in
    let bigint = (Z.of_string $1) in
    let bv = Bitvector.create bigint size in
    Dba.Expr.constant bv
  }
  | INT {
    let bigint = Z.of_string $1 in
    Expr.constant (Bitvector.create bigint 0)
  }
  | HEXA {
    let s, size = $1 in
    let bigint = Z.of_string s in
    let bv = Bitvector.create bigint size in
    Expr.constant bv
  }
  | IDENT { Parse_helpers.expr_of_name $1 }
  | STORELOAD LBRACKET expr COMMA INT RBRACKET {
    let size =
      int_of_string $5
      |> Size.Bit.create
      |> Size.Byte.of_bitsize in
    Dba.Expr.load size Machine.LittleEndian $3
  }
  | STORELOAD LBRACKET expr RBRACKET {
    Dba.Expr.load (Size.Byte.create 0) Machine.LittleEndian $3
  }
  | NOT expr %prec NOT         { Dba.Expr.lognot $2 }
  | MINUS e=expr; %prec UMINUS { Dba.Expr.uminus e }
  | LBRACE expr COMMA INT COMMA INT RBRACE {
    let off1 = int_of_string $4 in
    let off2 = int_of_string $6 in
    Dba.Expr.restrict off1 off2 $2
  }
  | EXTU expr INT {
    let size = int_of_string $3 in
    Dba.Expr.uext size $2
  }
  | EXTS expr INT {
    let size = int_of_string $3 in
    Dba.Expr.sext size $2
  }
  | IFJUMP cond expr ELSE expr { Dba.Expr.ite $2 $3 $5 }
  | LPAR expr RPAR { $2 }
  | expr bin_op expr {
    infer_binary_op_size $1 $2 $3
  }
  | LPAR expr bin_op expr RPAR {
    infer_binary_op_size $2 $3 $4
  }

bin_op :
 | MODU { Dba.Binary_op.ModU }
 | MODS { Dba.Binary_op.ModS }
 | OR  { Dba.Binary_op.Or }
 | AND { Dba.Binary_op.And }
 | XOR { Dba.Binary_op.Xor }
 | PLUS { Dba.Binary_op.Plus }
 | MINUS { Dba.Binary_op.Minus }
 | MULTU { Dba.Binary_op.Mult }
 | MULTS { Dba.Binary_op.Mult }
 | DIVU { Dba.Binary_op.DivU }
 | DIVS { Dba.Binary_op.DivS }

 | CONCAT { Dba.Binary_op.Concat }

 | EQQ { Dba.Binary_op.Eq }
 | EQQ2 { Dba.Binary_op.Eq }
 | NEQ { Dba.Binary_op.Diff }
 | NEQ2 { Dba.Binary_op.Diff }
 | LEU { Dba.Binary_op.LeqU }
 | LTU { Dba.Binary_op.LtU }
 | GEU { Dba.Binary_op.GeqU }
 | GTU { Dba.Binary_op.GtU }
 | LES { Dba.Binary_op.LeqS }
 | LTS { Dba.Binary_op.LtS }
 | GES { Dba.Binary_op.GeqS }
 | GTS { Dba.Binary_op.GtS }

 | LSHIFT  { Dba.Binary_op.LShift }
 | RSHIFTU  { Dba.Binary_op.RShiftU }
 | RSHIFTS   { Dba.Binary_op.RShiftS }
 | LROTATE  { Dba.Binary_op.LeftRotate }
 | RROTATE  { Dba.Binary_op.RightRotate }

cond :
 | TRUE    { Dba.Expr._true }
 | FALSE   { Dba.Expr._false }
 | e=expr  { e }
