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

%token ASSERT ASSUME ASSIGN GOTO JUMP HALT
%token UNDEF NONDET
%token AT IF THEN ELSE

%%

%public
let assignment :=
  | ~=loc; ASSIGN; ~=expr;
    { Dba.Instr.assign loc expr }
  | ~=loc; ASSIGN; value=INT;
    { Dba.Instr.assign
	loc (Dba.Expr.constant
	     @@ Bitvector.create value (Dba.LValue.size_of loc)) }
  | ~=loc; ASSIGN; UNDEF;
    { Dba.Instr.undefined loc }
  | ~=loc; ASSIGN; NONDET;
    { Dba.Instr.non_deterministic loc }

%public
let fallthrough :=
  | ~=assignment;
    { assignment }
  | ASSERT; ~=bool;
    { Dba.Instr._assert bool }
  | ASSUME; ~=bool;
    { Dba.Instr.assume bool }

%public
let terminator :=
  | JUMP; AT; ~=iexpr;
    { let addr = match iexpr with
	| Right e -> e
	| Left i -> Dba.Expr.constant
		    @@ Bitvector.create i E.wordsize in
      Dba.Instr.dynamic_jump addr }
  | HALT;
    { Dba.Instr.stop None }
