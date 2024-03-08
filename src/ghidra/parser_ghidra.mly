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

%token DATE INFO FILE SUPER LPAR DOT RPAR
%token ADDRESS OPCODE SIZE MNEMONIC KIND SUCCESSORS
%token <Z.t> NUMERIC
%token <string> STRING
%token EOF

%start instructions
%type <(Virtual_address.t * int * string * string * Virtual_address.t list) list> instructions

%start instruction
%type <Virtual_address.t * int * string * string * Virtual_address.t list> instruction

%%

let instructions := ~=list(instruction); EOF; <>

let instruction :=
  addr=log(kv(ADDRESS, NUMERIC));
  log(kv(OPCODE, NUMERIC));
  size=log(kv(SIZE, NUMERIC));
  mnemonic=log(kv(MNEMONIC, STRING));
  kind = log(kv(KIND, STRING));
  successors=log(kv(SUCCESSORS, list(NUMERIC)));
    {
      Virtual_address.of_bigint addr,
      Z.to_int size,
      String.lowercase_ascii mnemonic,
      kind,
      List.map Virtual_address.of_bigint successors
    }

let kv (key, value) := LPAR; key; DOT; ~=value; RPAR; <>

let log (info) := DATE; INFO; FILE; SUPER; info
