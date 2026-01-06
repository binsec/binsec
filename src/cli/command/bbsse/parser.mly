/**************************************************************************/
/*  This file is part of BINSEC.                                          */
/*                                                                        */
/*  Copyright (C) 2016-2026                                               */
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

%token EXPECT IS SKIP PROCESS CALL AT
%token CLEAR OPAQUE BRANCH FALLTHROUGH UNREACHABLE
%token <Z.t> ADDRESS
%token EOF

%start directives
%type <Types.Directive.t  list> directives


%%

let directives := ~=list(directive); EOF; <>

let directive :=
  | EXPECT; ~=address; IS; ~=status;
    { Types.Directive.ExpectAt (address, status) }
  | SKIP; ~=address;
    { Types.Directive.SkipJump address }
  | PROCESS; CALL; AT; ~=address;
    { Types.Directive.ProcessCall address }

let address == addr=ADDRESS;
    { Virtual_address.of_bigint addr }

let status ==
  | OPAQUE; BRANCH;
    { Types.Status.Opaque true }
  | OPAQUE; FALLTHROUGH;
    { Types.Status.Opaque false }
  | UNREACHABLE;
    { Types.Status.Unreachable }
  | CLEAR;
    { Types.Status.Clear }
