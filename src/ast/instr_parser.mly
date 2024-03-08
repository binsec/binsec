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
    { Instr.assign loc (to_expr (LValue.size_of loc) expr) }
  | ~=loc; ASSIGN; UNDEF;
    { Instr.undef loc }
  | ~=loc; ASSIGN; NONDET;
    { Instr.nondet loc }

%public
let fallthrough :=
  | ~=assignment;
    { assignment }
  | ASSERT; ~=bool;
    { Instr.dynamic_assert bool }
  | ASSUME; ~=bool;
    { Instr.assume bool }

%public
let terminator :=
  | JUMP; AT; ~=expr;
    { let addr = to_expr Env.wordsize expr in
      Instr.dynamic_jump addr }
  | HALT;
    { Instr.halt }
