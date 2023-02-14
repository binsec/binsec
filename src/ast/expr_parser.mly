/**************************************************************************/
/*  This file is part of BINSEC.                                          */
/*                                                                        */
/*  Copyright (C) 2016-2022                                               */
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
    type ('a, 'b) either =
      | Left of 'a
      | Right of 'b
%}

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

%%

%public
let var := id=IDENT;
    { let name, size = id in
      Env.lookup name size }

%public
let load :=
  | array=LMEM; ~=addr; e=ioption(preceded(COMMA, endianness));
    s=ioption(preceded(COMMA, INT)); RMEM;
    { let endianness = match e with
	| None -> Env.endianness
	| Some e -> e in
      let size = match s with
	| None ->  1
	| Some s -> (Z.to_int s) in
      LValue.store (Size.Byte.create size) endianness addr ?array }

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
    { LValue.to_expr loc }
  | value=CONST;
    { Expr.constant value }
  | symbol=SYMBOL;
    { let name, attr = symbol in
      Env.lookup_symbol name attr }

let extra :=
  | ~=core;
    { core }
  | ~=core; r=RANGE;
    { let hi = r.Interval.hi and lo = r.Interval.lo in
      assert (hi >= lo && lo >= 0); (* TODO *)
      Expr.restrict lo hi core }
  | ~=unop; ~=extra;
    { unop extra }
  | a=extra; ~=binop; b=extra;
    { binop a b }

let sugar :=
  | ~=extra;
    { extra }
  | v=INT; ~=binop; ~=sugar;
    { let size = Expr.size_of sugar in
      let value = Expr.constant @@ Bitvector.create v size in
      binop value sugar }
  | ~=extra; ~=binop; v=INT;
    { let size = Expr.size_of extra in
      let value = Expr.constant @@ Bitvector.create v size in
      binop extra value }

let isugar ==
  | ~=sugar;   { Right sugar }
  | value=INT; { Left value }

let wrap := LPAR; ~=expr; RPAR; { expr }

let chain :=
  | hd=isugar; tl=nonempty_list(pair(cmpop, isugar));
    { List.fold_left
	(fun (e, a) (op, b) ->
	  let x, y = match a, b with
	    | Left _, Left _ -> assert false
	    | Left x, Right y ->
	       Expr.constant
	       @@ Bitvector.create x (Expr.size_of y), y
	    | Right x, Left y ->
	       x, Expr.constant
		  @@ Bitvector.create y (Expr.size_of x)
	    | Right x, Right y -> x, y in
	  Expr.logand e @@ op x y, b)
	(Expr.one, hd) tl |> fst }

let logic :=
  | ~=sugar;
    { sugar }
  | ~=chain;
    { chain }
  | a=bool; ~=logop; b=bool;
    { logop a b }

let ilogic ==
  | ~=logic;   { Right logic }
  | value=INT; { Left value }

%public
let iexpr ==
  | ~=expr;    { Right expr }
  | value=INT; { Left value }

let ite :=
  | test=bool; QMARK; branch=ilogic; COLON; default=iexpr;
    { let branch, default = match branch, default with
	| Left _,  Left _  -> assert false (* TODO *)
	| Left x,  Right y ->
	   Expr.constant (Bitvector.create x (Expr.size_of y)), y
	| Right x, Left y  ->
	   x, Expr.constant (Bitvector.create y (Expr.size_of x))
	| Right x, Right y -> x, y in
      Expr.ite test branch default }

let switch :=
  | CASE; expr=logic; IS;
    cases=rev(nonempty_list(separated_pair(iconst, COLON, iexpr)));
    ANY; COLON; default=ilogic;
    { let size = Expr.size_of expr in
      let default, size' = match default with
	| Left x ->
	   let size' =
	     List.fold_left
	       (fun s -> function
		 | _, Left _ -> s
		 | _, Right y -> Expr.size_of y)
	     (-1) cases in
	   assert (size' <> -1); (* TODO *)
	   Expr.constant (Bitvector.create x size'), size'
	| Right y -> y, Expr.size_of y in
      List.fold_left
	(fun e (k, c) ->
	  let k = match k with
	    | Left x -> Expr.constant (Bitvector.create x size)
	    | Right y -> Expr.constant y in
	  let c = match c with
	    | Left x -> Expr.constant (Bitvector.create x size')
	    | Right y -> y in
	  Expr.ite (Expr.equal expr k) c e) default cases }

%public
let bool :=
  | ~=logic;
    { assert (Expr.size_of logic = 1); (* TODO *) logic }
  | value=INT;
    { match Z.to_int value with
      | 0 -> Expr.zero
      | 1 -> Expr.one
      | _ -> assert false (* TODO *) }

%public
let iconst ==
  | value=INT;   { Left value }
  | value=CONST; { Right value }

let addr :=
  | ~=expr;
    { assert (Expr.size_of expr = Env.wordsize); (* TODO *) expr }
  | value=INT;
    { Expr.constant @@ Bitvector.create value Env.wordsize }

let unop ==
  | NOT;                 { Expr.lognot }
  | MINUS;               { Expr.neg }
  | size=ZEXT;           { Expr.uext size }
  | size=SEXT;           { Expr.sext size }

let binop ==
  | PLUS;   { Expr.add }
  | MINUS;  { Expr.sub }
  | MUL;    { Expr.mul }
  | UDIV;   { Expr.udiv }
  | SDIV;   { Expr.sdiv }
  | UMOD;   { Expr.umod }
  | SMOD;   { Expr.smod }
  | AND;    { Expr.logand }
  | OR;     { Expr.logor }
  | XOR;    { Expr.logxor }
  | LSL;    { Expr.shift_left }
  | LSR;    { Expr.shift_right }
  | ASR;    { Expr.shift_right_signed }
  | ROL;    { Expr.rotate_left }
  | ROR;    { Expr.rotate_right }
  | CONCAT; { Expr.append }
  | DIFF;   { Expr.diff }

let cmpop ==
  | EQUAL;  { Expr.equal }
  | UGE;    { Expr.uge }
  | UGT;    { Expr.ugt }
  | ULE;    { Expr.ule }
  | ULT;    { Expr.ult }
  | SGE;    { Expr.sge }
  | SGT;    { Expr.sgt }
  | SLE;    { Expr.sle }
  | SLT;    { Expr.slt }

let logop ==
  | LAND;   { Expr.logand }
  | LOR;    { Expr.logor }

let endianness ==
  | RARROW; { Machine.BigEndian }
  | LARROW; { Machine.LittleEndian }
