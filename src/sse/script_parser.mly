/**************************************************************************/
/*  This file is part of BINSEC.                                          */
/*                                                                        */
/*  Copyright (C) 2016-2021                                               */
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

%token STARTING FROM FILE LOAD SECTION SECTIONS
%token REACH CUT ENUMERATE
%token TIMES SUCH THAT PRINT FORMULA MODEL STREAM BIN DEC HEXA ASCII
%token REPLACE BY TAND
%token EOF

%start <Sse_types.Script.t list> script

%%

let script := statements=list(terminated(statement, option(SEMICOLON))); EOF;
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
  | a=assignment;
    { Sse_types.Script.Init
	(Parse_helpers.Initialization.from_assignment (a 0)) }
  | a=assignment; AS; id=IDENT;
    { match a 0 with
      | Dba.Instr.Assign (lval, _, _)
      | Dba.Instr.Nondet (lval, _, _) as a ->
	 let size = Dba.LValue.size_of lval in
	 let identifier, _ = id in
	 ignore (E.lookup identifier size);
	 Sse_types.Script.Init
	   (Parse_helpers.Initialization.from_assignment ~identifier a)
      | _ -> assert false }
  | ~=load; FROM; FILE;
    { Sse_types.Script.Init (Parse_helpers.Initialization.from_store load) }
  | ASSUME; ~=bool;
    { Sse_types.Script.Init (Parse_helpers.Initialization.assume bool) }

let goal :=
  | REACH; loc=address; times=option(terminated(INT, TIMES));
    guard=option(preceded(pair(SUCH, THAT), bool));
    actions=option(preceded(THEN, separated_nonempty_list(TAND, action)));
    { let n = Option.fold ~none:1 ~some:Z.to_int times in
      Sse_types.Script.Goal (Directive.reach ~n ?guard ?actions ~loc ()) }
  | REACH; MUL; loc=address; guard=option(preceded(pair(SUCH, THAT), bool));
    actions=option(preceded(THEN, separated_nonempty_list(TAND, action)));
    { Sse_types.Script.Goal (Directive.reach_all ?guard ?actions ~loc ()) }
  | CUT; AT; loc=address; guard=option(preceded(IF,bool));
    { Sse_types.Script.Goal (Directive.cut ?guard ~loc ()) }
  | AT; loc=address; ~=directive;
    { Sse_types.Script.Goal (directive ~loc ()) }

let address ==
  | ~=iexpr;
    { match iexpr with
      | Left i -> Dba.Expr.constant (Bitvector.create i E.wordsize)
      | Right e -> e }

let directive :=
  | ENUMERATE; e=expr; times=option(delimited(LPAR, INT, RPAR));
    { let n = Option.fold ~none:1 ~some:Z.to_int times in
      Directive.enumerate ~n e }
  | ENUMERATE; MUL; e=expr;
    { Directive.enumerate_all e }
  | ASSUME; e=bool;
    { Directive.assume e }
  | ASSERT; e=bool;
    { Directive.dynamic_assert e }

let stub :=
  | REPLACE; locs=separated_nonempty_list(COMMA, address); BY; ~=chunk;
    { Sse_types.Script.Stub (locs, chunk) }
  | ABORT; AT; locs=separated_nonempty_list(COMMA, address);
    { Sse_types.Script.Stub (locs, Dhunk.singleton
				     (Dba.Instr._assert Dba.Expr.zero 0)) }
  | HALT; AT; locs=separated_nonempty_list(COMMA, address);
    { Sse_types.Script.Stub (locs, Dhunk.singleton (Dba.Instr.stop None)) }

let pragma :=
  | STARTING; FROM; ~=address;
    { Sse_types.Script.Pragma (Sse_types.Pragma.Start_from address) }
  | LOAD; SECTION; ~=section; FROM; FILE;
    { Sse_types.Script.Pragma (Sse_types.Pragma.Load_sections [ section ]) }
  | LOAD; SECTIONS; sections=separated_nonempty_list(COMMA, section);
    FROM; FILE;
    { Sse_types.Script.Pragma (Sse_types.Pragma.Load_sections sections) }

let action :=
  | PRINT; FORMULA;
    { Directive.Action.Print_formula None }
  | PRINT; FORMULA; FOR; slice=separated_nonempty_list(COMMA, identifiable);
    { Directive.Action.Print_formula (Some slice) }
  | PRINT; MODEL;
    { Directive.Action.Print_model }
  | PRINT; ~=format; ~=expr;
    { Directive.Action.Print_value (format, expr) }
  | PRINT; ASCII; STREAM; id=IDENT;
    { Directive.Action.Print_stream (fst id) }

let format :=
  |
    { Directive.Action.Hex }
  | BIN;
    { Directive.Action.Bin }
  | DEC;
    { Directive.Action.Dec }
  | HEXA;
    { Directive.Action.Hex }
  | ASCII;
    { Directive.Action.Ascii }


let ident ==
  | id=IDENT;
    { fst id }

let identifiable :=
  | nammed=separated_pair(expr, AS, ident);
    { let expr, name = nammed in
      expr, name }
  | id=IDENT;
    { let name, size = id in
      Dba.LValue.to_expr (E.lookup name size), name }

let section :=
  | ~=ident;
    { ident }
  | label=LABEL;
    { "." ^ label }
