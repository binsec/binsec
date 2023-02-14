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


%token STARTING FROM FILE CORE LOAD SECTION SECTIONS IMPORT
%token REACH CUT ENUMERATE
%token TIMES SUCH THAT PRINT FORMULA MODEL STREAM BIN DEC HEXA ASCII CSTRING
%token REPLACE BY WITH TAND
%token EOF

%start <Script.t list> script

%%

let script :=
  | statements=nonempty_list(terminated(statement, option(SEMICOLON))); EOF;
    { statements }

let statement :=
  | ~=pragma;
    { pragma }
  | ~=init;
    { init }
  | ~=goal;
    { goal }
  | ~=stub;
    { stub }

let init :=
  | ~=instr;
    { Init instr }

let goal :=
  | REACH; loc=address; times=option(terminated(INT, TIMES));
    guard=option(preceded(pair(SUCH, THAT), bool));
    actions=loption(preceded(THEN, separated_nonempty_list(TAND, action)));
    { let n = Option.fold ~none:1 ~some:Z.to_int times in
      Directive (loc, Reach (n, guard, actions)) }
  | REACH; MUL; loc=address; guard=option(preceded(pair(SUCH, THAT), bool));
    actions=loption(preceded(THEN, separated_nonempty_list(TAND, action)));
    { Directive (loc, Reach (-1, guard, actions)) }
  | CUT; AT; loc=address; guard=option(preceded(IF,bool));
    { Directive (loc, Cut guard) }
  | AT; loc=address; ~=directive;
    { Directive (loc, directive) }

let directive :=
  | ENUMERATE; enum=expr; times=option(delimited(LPAR, INT, RPAR));
    { Enumerate (Option.fold ~none:1 ~some:Z.to_int times, enum) }
  | ENUMERATE; MUL; enum=expr;
    { Enumerate (max (1 lsl Expr.size_of enum) max_int, enum) }
  | ASSUME; test=bool;
    { Assume test }
  | ASSERT; test=bool;
    { Assert test }


let address ==
  | ~=iexpr;
    { match iexpr with
      | Left i -> Expr.constant (Bitvector.create i Env.wordsize)
      | Right e -> e }

let stub :=
  | REPLACE; locs=separated_nonempty_list(COMMA, address); BY; ~=chunk; END;
    { Stub (locs, chunk) }
  | ABORT; AT; locs=separated_nonempty_list(COMMA, address);
    { Stub (locs, [ Instr.dynamic_assert Expr.zero ]) }
  | HALT; AT; locs=separated_nonempty_list(COMMA, address);
    { Stub (locs, [ Instr.halt ]) }

let pragma :=
  | STARTING; FROM; ~=address;
    stmts=loption(delimited(WITH,flatten(nonempty_list(stmt)), END));
    { Start_from (address, stmts) }
  | STARTING; FROM; CORE;
    stmts=loption(delimited(WITH,flatten(nonempty_list(stmt)), END));
    { Start_from_core stmts }
  | LOAD; SECTION; ~=section; FROM; FILE;
    { Load_sections [ section ] }
  | LOAD; SECTIONS; sections=separated_nonempty_list(COMMA, section);
    FROM; FILE;
    { Load_sections sections }
  | ~=load; FROM; FILE;
    { Load_data (LValue.to_expr load) }
  | IMPORT; symbols=separated_nonempty_list(COMMA, SYMBOL); FROM; file=ident;
    { Import_symbols (symbols, file) }


let action :=
  | PRINT; FORMULA;
    { Types.Output.Formula }
  | PRINT; FORMULA; FOR; slice=separated_nonempty_list(COMMA, identifiable);
    { Types.Output.Slice slice }
  | PRINT; MODEL;
    { Types.Output.Model }
  | PRINT; ~=format; ~=expr;
    { Types.Output.Value (format, expr) }
  | PRINT; ASCII; STREAM; id=IDENT;
    { Types.Output.Stream (fst id) }
  | PRINT; CSTRING; id=IDENT;
    { Types.Output.String (fst id) }

let format :=
  |
    { Types.Output.Hex }
  | BIN;
    { Types.Output.Bin }
  | DEC;
    { Types.Output.Dec }
  | HEXA;
    { Types.Output.Hex }
  | ASCII;
    { Types.Output.Ascii }


let ident ==
  | id=IDENT;
    { fst id }
  | value=CONST;
    { Bitvector.to_asciistring value }

let identifiable :=
  | nammed=separated_pair(expr, AS, ident);
    { let expr, name = nammed in
      expr, name }
  | id=IDENT;
    { let name, size = id in
      LValue.to_expr (Env.lookup name size), name }

let section :=
  | ~=ident;
    { ident }
  | label=LABEL;
    { "." ^ label }
