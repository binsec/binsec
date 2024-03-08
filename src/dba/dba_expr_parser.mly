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
    type ('a, 'b) either =
      | Left of 'a
      | Right of 'b
%}

%token PLUS MINUS MUL UDIV SDIV UMOD SMOD
%token EQUAL DIFF ULE ULT UGE UGT SLE SLT SGE SGT
%token NOT AND OR XOR LSL LSR ASR ROL ROR CONCAT
%token <int> ZEXT SEXT
%token LAND LOR

%token LMEM RMEM LARROW RARROW
%token LPAR RPAR QMARK COLON COMMA

%token CASE IS ANY

%token <Z.t>                           INT
%token <Bitvector.t>                   CONST
%token <string * int>                  IDENT
%token <string * Dba.VarTag.attribute> SYMBOL
%token <int Interval.t>                RANGE

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
      E.lookup name size }

%public
let load :=
  | LMEM; ~=addr; e=ioption(preceded(COMMA, endianness));
    s=ioption(preceded(COMMA, INT)); RMEM;
    { let endianness = match e with
	| None -> E.endianness
	| Some e -> e in
      let size = match s with
	| None -> Size.Byte.create 1
	| Some s -> Size.Byte.create (Z.to_int s) in
      Dba.LValue.store size endianness addr }

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
    { Dba.LValue.to_expr loc }
  | value=CONST;
    { Dba.Expr.constant value }
  | symbol=SYMBOL;
    { let name, attr = symbol in
      E.lookup_symbol name attr }

let extra :=
  | ~=core;
    { core }
  | ~=core; r=RANGE;
    { let hi = r.Interval.hi and lo = r.Interval.lo in
      assert (hi >= lo && lo >= 0); (* TODO *)
      Dba.Expr.restrict lo hi core }
  | ~=unop; ~=extra;
    { Dba.Expr.unary unop extra }
  | a=extra; ~=binop; b=extra;
    { Dba.Expr.binary binop a b }

let sugar :=
  | ~=extra;
    { extra }
  | v=INT; ~=binop; ~=sugar;
    { let size = Dba.Expr.size_of sugar in
      let value = Dba.Expr.constant @@ Bitvector.create v size in
      Dba.Expr.binary binop value sugar }
  | ~=extra; ~=binop; v=INT;
    { let size = Dba.Expr.size_of extra in
      let value = Dba.Expr.constant @@ Bitvector.create v size in
      Dba.Expr.binary binop extra value }

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
	       Dba.Expr.constant
	       @@ Bitvector.create x (Dba.Expr.size_of y), y
	    | Right x, Left y ->
	       x, Dba.Expr.constant
		  @@ Bitvector.create y (Dba.Expr.size_of x)
	    | Right x, Right y -> x, y in
	  Dba.Expr.logand e @@ Dba.Expr.binary op x y, b)
	(Dba.Expr.one, hd) tl |> fst }

let logic :=
  | ~=sugar;
    { sugar }
  | ~=chain;
    { chain }
  | a=bool; ~=logop; b=bool;
    { Dba.Expr.binary logop a b }

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
	   Dba.Expr.constant (Bitvector.create x (Dba.Expr.size_of y)), y
	| Right x, Left y  ->
	   x, Dba.Expr.constant (Bitvector.create y (Dba.Expr.size_of x))
	| Right x, Right y -> x, y in
      Dba.Expr.ite test branch default }

let switch :=
  | CASE; expr=logic; IS;
    cases=rev(nonempty_list(separated_pair(iconst, COLON, iexpr)));
    ANY; COLON; default=ilogic;
    { let size = Dba.Expr.size_of expr in
      let default, size' = match default with
	| Left x ->
	   let size' =
	     List.fold_left
	       (fun s -> function
		 | _, Left _ -> s
		 | _, Right y -> Dba.Expr.size_of y)
	     (-1) cases in
	   assert (size' <> -1); (* TODO *)
	   Dba.Expr.constant (Bitvector.create x size'), size'
	| Right y -> y, Dba.Expr.size_of y in
      List.fold_left
	(fun e (k, c) ->
	  let k = match k with
	    | Left x -> Dba.Expr.constant (Bitvector.create x size)
	    | Right y -> Dba.Expr.constant y in
	  let c = match c with
	    | Left x -> Dba.Expr.constant (Bitvector.create x size')
	    | Right y -> y in
	  Dba.Expr.ite (Dba.Expr.equal expr k) c e) default cases }

%public
let bool :=
  | ~=logic;
    { assert (Dba.Expr.size_of logic = 1); (* TODO *) logic }
  | value=INT;
    { match Z.to_int value with
      | 0 -> Dba.Expr.zero
      | 1 -> Dba.Expr.one
      | _ -> assert false (* TODO *) }

%public
let iconst ==
  | value=INT;   { Left value }
  | value=CONST; { Right value }

let addr :=
  | ~=expr;
    { assert (Dba.Expr.size_of expr = E.wordsize); (* TODO *) expr }
  | value=INT;
    { Dba.Expr.constant @@ Bitvector.create value E.wordsize }

let unop ==
  | NOT;                 { Dba.Unary_op.Not }
  | MINUS;               { Dba.Unary_op.UMinus }
  | size=ZEXT;           { Dba.Unary_op.Uext size }
  | size=SEXT;           { Dba.Unary_op.Sext size }

let binop ==
  | PLUS;   { Dba.Binary_op.Plus }
  | MINUS;  { Dba.Binary_op.Minus }
  | MUL;    { Dba.Binary_op.Mult }
  | UDIV;   { Dba.Binary_op.DivU }
  | SDIV;   { Dba.Binary_op.DivS }
  | UMOD;   { Dba.Binary_op.ModU }
  | SMOD;   { Dba.Binary_op.ModS }
  | AND;    { Dba.Binary_op.And }
  | OR;     { Dba.Binary_op.Or }
  | XOR;    { Dba.Binary_op.Xor }
  | LSL;    { Dba.Binary_op.LShift }
  | LSR;    { Dba.Binary_op.RShiftU }
  | ASR;    { Dba.Binary_op.RShiftS }
  | ROL;    { Dba.Binary_op.LeftRotate }
  | ROR;    { Dba.Binary_op.RightRotate }
  | CONCAT; { Dba.Binary_op.Concat }
  | DIFF;   { Dba.Binary_op.Diff }

let cmpop ==
  | EQUAL;  { Dba.Binary_op.Eq }
  | UGE;    { Dba.Binary_op.GeqU }
  | UGT;    { Dba.Binary_op.GtU }
  | ULE;    { Dba.Binary_op.LeqU }
  | ULT;    { Dba.Binary_op.LtU }
  | SGE;    { Dba.Binary_op.GeqS }
  | SGT;    { Dba.Binary_op.GtS }
  | SLE;    { Dba.Binary_op.LeqS }
  | SLT;    { Dba.Binary_op.LtS }

let logop ==
  | LAND;   { Dba.Binary_op.And }
  | LOR;    { Dba.Binary_op.Or }

let endianness ==
  | RARROW; { Machine.BigEndian }
  | LARROW; { Machine.LittleEndian }
