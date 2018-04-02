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

%token SATT UNSATT LPAREN RPAREN EOF ERROR ERROR_MSG
%token <string> IDENT
%token <string> NUM


%start main
%type <Smtlib2.smt_result option * (string * string) list list> main
%%

verdict:
| SATT   { Some Smtlib2.SAT   }
| UNSATT { Some Smtlib2.UNSAT }
;

main:
| v=verdict; m=main; { v, snd m }
| args=delimited(LPAREN, args, RPAREN);  m=main;
  { None, args @ (snd m)}
| EOF   { (None, []) }
| error { Logger.error "SMTParserWp: Unexpected token in main"; None, [] }
;

args :
 | ERROR ERROR_MSG       { [] }
 | values=list(value);   { [values] }
;

memory_ident :
 | LPAREN id1=IDENT; id2=IDENT; n=NUM; RPAREN
  { Format.sprintf "%s %s %s" id1 id2 n }
;

value:
| id=IDENT; n=NUM;                { id, n }
| id=memory_ident; n=NUM;         { id, n }
| LPAREN v=value; RPAREN          { v }
;
