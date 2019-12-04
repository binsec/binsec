/**************************************************************************/
/*  This file is part of BINSEC.                                          */
/*                                                                        */
/*  Copyright (C) 2016-2019                                               */
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

  let infer_variable_lhs (name:string): LValue.t =
    let open Dba.LValue in
    let open Size.Bit in
    let first_char = String.get name 0 in
    if first_char = '_' || first_char =  '?' ||  first_char  = '!' then
      let name = if first_char = '_' then "*" else name in
      var name ~bitsize:(Size.Bit.create 0)
    else
      let reg name = var name ~bitsize:bits32 in
      let restrict = _restrict in
      match name with
      | "al" -> restrict "eax" bits32 0 7
      | "ah" -> restrict "eax" bits32 8 15
      | "ax" -> restrict "eax" bits32 0 15
      | "eax" -> reg "eax"
      | "bl" -> restrict "ebx" bits32 0 7
      | "bh" -> restrict "ebx" bits32 8 15
      | "bx" -> restrict "ebx" bits32 0 15
      | "ebx" -> reg "ebx"
      | "cl" -> restrict "ecx" bits32 0 7
      | "ch" -> restrict "ecx" bits32 8 15
      | "cx" -> restrict "ecx" bits32 0 15
      | "ecx" -> reg "ecx"
      | "dl" -> restrict "edx" bits32 0 7
      | "dh" -> restrict "edx" bits32 8 15
      | "dx" -> restrict "edx" bits32 0 15
      | "edx" -> reg "edx"
      | "di"  -> restrict "edi" bits32 0 15
      | "edi" -> reg "edi"
      | "si" -> restrict "esi" bits32 0 15
      | "esi" -> reg "esi"
      | "bp"  -> restrict "ebp" bits32 0 15
      | "ebp" -> reg "ebp"
      | "sp"  -> restrict "esp" bits32 0 15
      | "esp" -> reg "esp"
      | "btemp" -> var "btemp" ~bitsize:bits8
      | "stemp" -> var "stemp" ~bitsize:bits16
      | "temp" ->  var "temp"  ~bitsize:bits32
      | "dtemp" -> var "dtemp" ~bitsize:bits64
      | _ -> Kernel_options.Logger.error "Unknown LHS variable";
             raise Parsing.Parse_error

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
  | lv=lhs; ASSIGN e=expr; {
  let size = Dba.Expr.size_of e in
  let sizelhs =
    match lv with
    | Dba.LValue.Var { size = sz; _}
    | Dba.LValue.Restrict({Dba.size = sz; _},_)
    | Dba.LValue.Store(sz,_,_) -> sz in
  Kernel_options.Logger.debug ~level:2 "Lhs:%d | Expr (infered):%d" sizelhs size;
  let lhs =
    if size <> 0 then
    match lv with
    | Dba.LValue.Var { size = sz; _} ->
      Kernel_options.Logger.debug ~level:2 "Var Size:%d" sz;
      if sz <> size && sz <> 0 then raise Parsing.Parse_error else lv
    | Dba.LValue.Restrict({Dba.size = sz; _}, {Interval.lo=l; Interval.hi=h}) ->
      Kernel_options.Logger.debug ~level:2 "VarR Size:%d" sz;
      if size <> h - l + 1 && sz <> 0 then raise Parsing.Parse_error else lv
    | Dba.LValue.Store(sz,en,e) ->
       Kernel_options.Logger.debug ~level:2 "LhsStore Size:%d" sz;
       if sz = 0 then
         let bytes = Size.Byte.create size in
         Dba.LValue.store bytes en e
       else lv
    else lv
  in
  let exp =
    if size = 0 && sizelhs <> 0 then
    match lv with
    | Dba.LValue.Var { size; _} -> Parse_helpers.patch_expr_size e size
    | Dba.LValue.Restrict(_, {Interval.lo=l; Interval.hi=h}) ->
    Parse_helpers.patch_expr_size e (h-l+1)
    | Dba.LValue.Store(sz,_en,_e) ->
       if sz <> 0 then Parse_helpers.patch_expr_size e sz else e
    else e
  in Dba.Instr.assign lhs exp 0
  }
  | NEXT expr { Dba.Instr.dynamic_jump $2 }
  | IFJUMP c=cond; NEXT expr ELSE NEXT expr {
    Dba.Instr.ite c
    (Dba.Jump_target.outer (Dba_types.Caddress.block_start @@ Virtual_address.create 0)) 0
  }
(*  | IFJUMP cond NEXT address ELSE NEXT INT {
   Dba.Instr.If ($2, JOuter $4, (int_of_string $7))
  }
  | IFJUMP cond NEXT INT ELSE NEXT INT {
   Dba.Instr.If ($2, JInner (int_of_string $4), (int_of_string $7))
  }
*)

lhs :
  | v=IDENT { infer_variable_lhs v }
  | id=IDENT; LBRACE lo=INT; COMMA hi=INT; RBRACE
  {
    let off1 = int_of_string lo in
    let off2 = int_of_string hi in
    let size = infer_size_variable id |> Size.Bit.create in
    Dba.LValue._restrict id size off1 off2
  }
  | STORELOAD LBRACKET e=expr; sz=ioption(preceded(COMMA, INT)); RBRACKET
  {
    let bytesize =
      match sz with
      | None -> Size.Byte.create 0
      | Some v -> Size.Byte.of_string v
   in Dba.LValue.store bytesize Machine.LittleEndian e
  }
;

expr:
  | INT INFER INT SUPER {
    let size = int_of_string $3 in
    let bigint = Bigint.big_int_of_string $1 in
    let bv = Bitvector.create bigint size in
    Dba.Expr.constant bv
  }
  | INT {
    let bigint = Bigint.big_int_of_string $1 in
    Dba.Expr.constant (Bitvector.create bigint 0)
  }
  | HEXA {
    let s, size = $1 in
    let bigint = Bigint.big_int_of_string s in
    let bv = Bitvector.create bigint size in
    Dba.Expr.constant bv
  }
  | IDENT {
    let var = Parse_helpers.expr_of_name $1 in
    var
  }
  | STORELOAD LBRACKET expr COMMA INT RBRACKET {
    let size = int_of_string $5 |> Size.Byte.create in
    Dba.Expr.load size Machine.LittleEndian $3
  }
  | STORELOAD LBRACKET expr RBRACKET {
    Dba.Expr.load (Size.Byte.create 0) Machine.LittleEndian $3
  }
  | NOT expr %prec NOT      { Dba.Expr.lognot $2 }
  | MINUS expr %prec UMINUS { Dba.Expr.uminus $2 }
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
  | e1=expr; bop=bin_op; e2=expr;
  | LPAR e1=expr; bop=bin_op; e2=expr RPAR {
    let sz1 = Dba.Expr.size_of e1 in
    let sz2 = Dba.Expr.size_of e2 in
    if sz1 = 0 && sz2 <> 0 || sz1 <> 0 && sz2 = 0 then
      match bop with
      | Dba.Binary_op.ModU | Dba.Binary_op.ModS | Dba.Binary_op.Or
        | Dba.Binary_op.And | Dba.Binary_op.Xor | Dba.Binary_op.Plus
        | Dba.Binary_op.Minus | Dba.Binary_op.Mult
        | Dba.Binary_op.DivU | Dba.Binary_op.DivS ->
        if sz1 = 0 then
          Dba.Expr.binary bop (Parse_helpers.patch_expr_size e1 sz2) e2
        else
          Dba.Expr.binary bop e1 (Parse_helpers.patch_expr_size e2 sz1)
      | _ -> Dba.Expr.binary bop e1 e2
    else Dba.Expr.binary bop e1 e2
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
 | NEQ { Dba.Binary_op.Diff }
 | LEU { Dba.Binary_op.LeqU }
 | LTU { Dba.Binary_op.LtU }
 | GEU { Dba.Binary_op.GeqU }
 | GTU { Dba.Binary_op.GtU }
 | LES { Dba.Binary_op.LeqS }
 | LTS { Dba.Binary_op.LtS }
 | GES { Dba.Binary_op.GeqS }
 | GTS { Dba.Binary_op.GtS }
 | LSHIFT   { Dba.Binary_op.LShift }
 | RSHIFTU  { Dba.Binary_op.RShiftU }
 | RSHIFTS  { Dba.Binary_op.RShiftS }
 | LROTATE  { Dba.Binary_op.LeftRotate }
 | RROTATE  { Dba.Binary_op.RightRotate }

cond :
 | TRUE    { Dba.Expr.one }
 | FALSE   { Dba.Expr.zero }
 | e=expr  { e }
