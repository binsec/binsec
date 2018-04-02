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
  open Policy_type

  let infer_size_variable (name:string): int =
    match name with
    | "eax" | "ebx" | "ecx" | "edx" | "edi"
    | "esi" | "ebp" | "esp" -> 32
    | "btemp" -> 8
    | "stemp" -> 16
    | "temp" -> 32
    | "dtemp" -> 64
    |  _ -> failwith "Unknown variable"

  let infer_variable_lhs (name:string): lhs =
    let first_char = String.get name 0 in
    if first_char = '_' || first_char =  '?' ||  first_char  = '!' then
      if first_char = '_' then
        Dba.LhsVar("*", 0, None)
      else
        Dba.LhsVar(name,0, None)
    else
      match name with
      | "al" -> Dba.LhsVarRestrict ("al", 32, 0, 7)
      | "ah" -> Dba.LhsVarRestrict ("al", 32, 8, 15)
      | "ax" -> Dba.LhsVarRestrict ("al", 32, 0, 15)
      | "eax" -> Dba.LhsVar ("eax", 32, None)
      | "bl" -> Dba.LhsVarRestrict ("bl", 32, 0, 7)
      | "bh" -> Dba.LhsVarRestrict ("bh", 32, 8, 15)
      | "bx" -> Dba.LhsVarRestrict ("bx", 32, 0, 15)
      | "ebx" -> Dba.LhsVar ("ebx", 32, None)
      | "cl" -> Dba.LhsVarRestrict ("cl", 32, 0, 7)
      | "ch" -> Dba.LhsVarRestrict ("ch", 32, 8, 15)
      | "cx" -> Dba.LhsVarRestrict ("cx", 32, 0, 15)
      | "ecx" -> Dba.LhsVar ("ecx", 32, None)
      | "dl" -> Dba.LhsVarRestrict ("dl", 32, 0, 7)
      | "dh" -> Dba.LhsVarRestrict ("dh", 32, 8, 15)
      | "dx" -> Dba.LhsVarRestrict ("dx", 32, 0, 15)
      | "edx" -> Dba.LhsVar ("edx", 32, None)
      | "di" -> Dba.LhsVarRestrict ("di", 32, 0, 15)
      | "edi" -> Dba.LhsVar ("edi", 32, None)
      | "si" -> Dba.LhsVarRestrict ("si", 32, 0, 15)
      | "esi" -> Dba.LhsVar ("esi", 32, None)
      | "bp" -> Dba.LhsVarRestrict ("bp", 32, 0, 15)
      | "ebp" -> Dba.LhsVar ("ebp", 32, None)
      | "sp" -> Dba.LhsVarRestrict ("sp", 32, 0, 15)
      | "esp" -> Dba.LhsVar ("esp", 32, None)
      | "btemp" -> Dba.LhsVar ("btemp", 8, None)
      | "stemp" -> Dba.LhsVar ("stemp", 16, None)
      | "temp" -> Dba.LhsVar ("temp", 32, None)
      | "dtemp" -> Dba.LhsVar ("dtemp", 64, None)
      | _ -> (Logger.error "Unknown LHS variable"; raise Parsing.Parse_error)

%}


%token UMINUS PLUS MINUS MULTU MULTS DIVU DIVS /*POW*/ MODU MODS
 /*NONDET*/ STORELOAD
%token AND OR XOR NOT
%token CONCAT /*COLON*/ /*SEMICOLON*/ LSHIFT RSHIFTU RSHIFTS LROTATE RROTATE EXTU EXTS
INFER SUPER
%token EQQ NEQ LEU LES LTU LTS GEU GES GTU GTS
%token LBRACE RBRACE LPAR RPAR LBRACKET RBRACKET COMMA NEXT
%token ASSIGN TRUE FALSE IFJUMP ELSE /*ARRAY*/
%token EOF
%token INTERVALSEP DCOLON TOACTION WILDCARD SUBTERM DEFAULT SYMB CONC PROP PROPC PROPS AND2 OR2 TAINTCHECK NOT2 TAINTI TAINTP
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

%start  policy
%type <Policy_type.policy> policy

%%

policy:
  | EOF { [] }
  | rules EOF { $1 }

rules:
  | rule rules { $1::$2 }
  | rule { [$1] }

rule:
  | location_p DCOLON instruction_p DCOLON expression_p DCOLON sigma_p TOACTION action { {loc_p=$1; inst_p=$3; exp_p=$5; sigma_p=$7; action=$9,""} }
  | DEFAULT TOACTION action { {loc_p=LocWildcard; inst_p=InstWildcard; exp_p=ExpWildcard; sigma_p=SigmaWildcard; action=$3,""} }

action:
  | SYMB { Symb }
  | CONC { Conc }
  | PROP { KeepOrSymb }
  | PROPC { KeepOrConc }
  | PROPS { KeepOrSymb }

location_p:
  | WILDCARD { LocWildcard }
  | addrlist { LocSet($1) }
  | addr INTERVALSEP addr { LocInterval($1,$3) }
  | LBRACKET addr INTERVALSEP addr RBRACKET { LocInterval($2,$4) }

addrlist:
 | addr COMMA addrlist { $1 :: $3 }
 | addr { [$1] }

addr:
  | HEXA { Int64.of_string (fst $1)}

instruction_p:
  | WILDCARD { InstWildcard }
  | inst     { InstPattern($1) }

expression_p:
  | WILDCARD { ExpWildcard }
  | expr { ExpDba $1 }
  | expr SUBTERM expr { ExpSubTerm($1,$3) }

sigma_p:
  | WILDCARD { SigmaWildcard }
  | NOT2 LPAR sigma_p RPAR { SigmaUnary(Policy_type.Not,$3) }
  | sigma_p OR2 sigma_p { SigmaBinary(Policy_type.Or,$1,$3) }
  | sigma_p AND2 sigma_p { SigmaBinary(Policy_type.And,$1,$3) }
  | LPAR sigma_p OR2 sigma_p RPAR { SigmaBinary(Policy_type.Or,$2,$4) }
  | LPAR sigma_p AND2 sigma_p RPAR { SigmaBinary(Policy_type.And,$2,$4) }
  | TAINTCHECK LPAR taint COMMA expr RPAR { TaintCheck($3, $5) }

taint:
  | TAINTP { Taint_types.TaintP }
  | TAINTI { Taint_types.TaintI }


inst:
  | lhs ASSIGN expr {
  let size = Parse_helpers.expr_size $3 in
  let sizelhs = match $1 with Dba.LhsVar(_,sz,_) | Dba.LhsVarRestrict(_,sz,_,_) | Dba.LhsStore(sz,_,_) -> sz in
  Logger.debug ~level:2 "Lhs:%d | Expr (infered):%d" sizelhs size;
  let lhs =
    if size != 0 then
    match $1 with
    | Dba.LhsVar(_n,sz,_) ->
      Logger.debug ~level:2 "Var Size:%d" sz;
      if sz <> size && sz <> 0 then raise Parsing.Parse_error else $1
    | Dba.LhsVarRestrict(_,sz,l,h) ->
      Logger.debug ~level:2 "VarR Size:%d" sz;
      if size <> h -l + 1 && sz <> 0 then raise Parsing.Parse_error else $1
    | Dba.LhsStore(sz,en,e) ->
      Logger.debug ~level:2 "LhsStore Size:%d" sz;
      if sz = 0 then Dba.LhsStore(size,en,e) else $1
    else $1
  in
  let exp =
    if size = 0 && sizelhs != 0 then
    match $1 with
    | Dba.LhsVar(_n,sz,_) -> Parse_helpers.patch_expr_size $3 sz
    | Dba.LhsVarRestrict(_n,_sz,l,h) -> Parse_helpers.patch_expr_size $3 (h-l+1)
    | Dba.LhsStore(sz,_en,_e) -> if sz != 0 then Parse_helpers.patch_expr_size $3 sz else $3
    else $3
  in
  Dba.IkAssign (lhs, exp, 0)
  }
  | NEXT expr { Dba.IkDJump ($2, None) }
  | IFJUMP cond NEXT expr ELSE NEXT expr {
    Dba.IkIf ($2, JOuter (Dba_types.Caddress.block_start @@ Bitvector.zeros 32), 0)
  }
(*  | IFJUMP cond NEXT address ELSE NEXT INT {
   Dba.IkIf ($2, JOuter $4, (int_of_string $7))
  }
  | IFJUMP cond NEXT INT ELSE NEXT INT {
   Dba.IkIf ($2, JInner (int_of_string $4), (int_of_string $7))
  }
*)

lhs :
  | IDENT {
    let var = infer_variable_lhs $1 in
    var
  }
  | IDENT LBRACE INT COMMA INT RBRACE {
    let off1 = int_of_string $3 in
    let off2 = int_of_string $5 in
    let size = infer_size_variable $1 in
    Dba.LhsVarRestrict ($1, size, off1, off2)
  }
  | STORELOAD LBRACKET expr COMMA INT RBRACKET {
    let size = int_of_string $5 in Dba.LhsStore (size, LittleEndian, $3)
  }
  | STORELOAD LBRACKET expr RBRACKET {
    Dba.LhsStore (0, LittleEndian, $3)
  }


expr:
  | INT INFER INT SUPER {
    let size = int_of_string $3 in
    let bigint = (Bigint.big_int_of_string $1) in
    let bv = Bitvector.create bigint size in
    Dba.ExprCst (`Constant, bv)
  }
  | INT {
    let bigint = (Bigint.big_int_of_string $1) in
    Dba.ExprCst (`Constant, Bitvector.create bigint 0)
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
    Dba.ExprLoad (size, LittleEndian, $3)
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
    let sz1 = Parse_helpers.expr_size $1 in
    let sz2 = Parse_helpers.expr_size $3 in
    if (sz1 = 0 && sz2 != 0) || (sz1 != 0 && sz2 = 0) then
      match $2 with
      | Dba.ModU | Dba.ModS | Dba.Or | Dba.And | Dba.Xor | Dba.Plus
      | Dba.Minus | Dba.MultU | Dba.MultS | Dba.DivU | Dba.DivS ->
        if sz1 = 0 then
          Dba.ExprBinary($2, Parse_helpers.patch_expr_size $1 sz2, $3)
        else
          Dba.ExprBinary($2, $1, Parse_helpers.patch_expr_size $3 sz1)
      | _ -> Dba.ExprBinary ($2, $1, $3)
    else
      Dba.ExprBinary ($2, $1, $3)
  }
  | LPAR expr bin_op expr RPAR {
    let sz1 = Parse_helpers.expr_size $2 in
    let sz2 = Parse_helpers.expr_size $4 in
    if (sz1 = 0 && sz2 != 0) || (sz1 != 0 && sz2 = 0) then
      match $3 with
      | Dba.ModU | Dba.ModS | Dba.Or | Dba.And | Dba.Xor | Dba.Plus
      | Dba.Minus | Dba.MultU | Dba.MultS | Dba.DivU | Dba.DivS ->
        if sz1 = 0 then
          Dba.ExprBinary($3, Parse_helpers.patch_expr_size $2 sz2, $4)
        else
          Dba.ExprBinary($3, $2, Parse_helpers.patch_expr_size $4 sz1)
      | _ -> Dba.ExprBinary ($3, $2, $4)
    else
      Dba.ExprBinary ($3, $2, $4)
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
 | NEQ { Dba.Diff }
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
