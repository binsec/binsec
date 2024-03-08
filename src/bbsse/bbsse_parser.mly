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

%token EXPECT IS SKIP PROCESS CALL AT
%token CLEAR OPAQUE BRANCH FALLTHROUGH UNREACHABLE
%token <Z.t> ADDRESS
%token EOF

%start directives
%type <Bbsse_types.Directive.t  list> directives


%%

let directives := ~=list(directive); EOF; <>

let directive :=
  | EXPECT; ~=address; IS; ~=status;
    { Bbsse_types.Directive.ExpectAt (address, status) }
  | SKIP; ~=address;
    { Bbsse_types.Directive.SkipJump address }
  | PROCESS; CALL; AT; ~=address;
    { Bbsse_types.Directive.ProcessCall address }

let address == addr=ADDRESS;
    { Virtual_address.of_bigint addr }

let status ==
  | OPAQUE; BRANCH;
    { Bbsse_types.Status.Opaque true }
  | OPAQUE; FALLTHROUGH;
    { Bbsse_types.Status.Opaque false }
  | UNREACHABLE;
    { Bbsse_types.Status.Unreachable }
  | CLEAR;
    { Bbsse_types.Status.Clear }
