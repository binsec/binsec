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

%token PLUS MINUS MUL UDIV SDIV UMOD SMOD
%token EQUAL DIFF ULE ULT UGE UGT SLE SLT SGE SGT
%token NOT AND OR XOR LSL LSR ASR ROL ROR CONCAT
%token <int> ZEXT SEXT
%token LAND LOR

%token <string option> LMEM
%token RMEM LARROW RARROW
%token LPAR RPAR QMARK COLON COMMA

%token CASE IS ANY


%token <Z.t>                            INT
%token <Bitvector.t>                    CONST
%token <string * int>                   IDENT
%token <string * Dba.Var.Tag.attribute> SYMBOL
%token <int Interval.t>                 RANGE

%left LOR
%left LAND
%nonassoc DIFF
%right CONCAT
%left LSL LSR ASR ROL ROR
%left PLUS MINUS
%left MUL UDIV SDIV UMOD SMOD AND OR XOR
%nonassoc NOT ZEXT SEXT
%nonassoc RANGE

%%

%public
let var := id=IDENT;
    { let name, size = id in
      Env.lookup name size }

%public
let load :=
  | array=LMEM; ~=address; e=ioption(preceded(COMMA, endianness));
    s=ioption(preceded(COMMA, INT)); RMEM;
    { let endianness = match e with
	| None -> Env.endianness
	| Some e -> e in
      let size = match s with
	| None ->  1
	| Some s -> (Z.to_int s) in
      LValue.store (Size.Byte.create size) endianness address ?array }

%public
let loc :=
  | ~=var;
    { var }
  | ~=load;
    { load }

%public
let expr :=
  | ~=logic;
    { logic }
  | ~=ite;
    { ite }
  | ~=switch;
    { switch }

let core :=
  | ~=wrap;
    { wrap }
  | ~=loc;
    { Expr (LValue.to_expr loc) }
  | value=INT;
    { Int value }
  | value=CONST;
    { Expr (Expr.constant value) }
  | symbol=SYMBOL;
    { let name, attr = symbol in
      Expr (Env.lookup_symbol name attr) }
  | ~=core; r=RANGE;
    { let hi = r.Interval.hi and lo = r.Interval.lo in
      assert (hi >= lo && lo >= 0); (* TODO *)
      restrict lo hi core }
  | ~=unop; ~=core;
    { unop core }
  | a=core; ~=binop; b=core;
    { binop a b }

let wrap := LPAR; ~=expr; RPAR; { expr }

let chain :=
  | hd=core; tl=nonempty_list(pair(cmpop, core));
    { List.fold_left
	(fun (e, a) (op, b) ->
	  logand e @@ op a b, b)
	(Expr Expr.one, hd) tl |> fst }

let logic :=
  | ~=core;
    { core }
  | ~=chain;
    { chain }
  | a=bool; ~=logop; b=bool;
    { logop (Expr a) (Expr b) }

let ite :=
  | test=bool; QMARK; branch=logic; COLON; default=expr;
    { ite test branch default }

let switch :=
  | CASE; expr=logic; IS;
    cases=rev(nonempty_list(separated_pair(const, COLON, expr)));
    ANY; COLON; default=logic;
    { let default = match default with
	| Int x ->
	   let size =
	     List.fold_left
	       (fun s -> function
		 | _, Int _ -> s
		 | _, Expr y -> Expr.size_of y)
	       (-1) cases in
	   if size = -1 then
	     Logger.fatal "unable to infer size for the switch case body";
	   Expr.constant (Bitvector.create x size)
	| Expr y -> y in
      List.fold_left
	(fun e (k, c) ->
	  ite (to_bool (equal expr k)) c e) (Expr default) cases }

%public
let bool :=
  | ~=logic;
    { to_bool logic }

%public
let const ==
  | value=INT;   { Int value }
  | value=CONST; { Expr (Expr.constant value) }

%public
let address :=
  | ~=expr;
    { to_expr Env.wordsize expr }

let unop ==
  | NOT;                 { lognot }
  | MINUS;               { neg }
  | size=ZEXT;           { uext size }
  | size=SEXT;           { sext size }

let binop ==
  | PLUS;   { add }
  | MINUS;  { sub }
  | MUL;    { mul }
  | UDIV;   { udiv }
  | SDIV;   { sdiv }
  | UMOD;   { umod }
  | SMOD;   { smod }
  | AND;    { logand }
  | OR;     { logor }
  | XOR;    { logxor }
  | LSL;    { shift_left }
  | LSR;    { shift_right }
  | ASR;    { shift_right_signed }
  | ROL;    { rotate_left }
  | ROR;    { rotate_right }
  | CONCAT; { append }
  | DIFF;   { diff }

let cmpop ==
  | EQUAL;  { equal }
  | UGE;    { uge }
  | UGT;    { ugt }
  | ULE;    { ule }
  | ULT;    { ult }
  | SGE;    { sge }
  | SGT;    { sgt }
  | SLE;    { sle }
  | SLT;    { slt }

let logop ==
  | LAND;   { logand }
  | LOR;    { logor }

let endianness ==
  | RARROW; { Machine.BigEndian }
  | LARROW; { Machine.LittleEndian }
