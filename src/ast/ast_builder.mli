(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

type t

val make:
  Disasm_types.pmap ->
  Dba_types.Caddress.Set.t Dba_types.Caddress.Map.t ->
  t

val cfg_opcode_of_ast:
  t ->
  Dba_types.Caddress.Map.key option ->
  Dba_types.Caddress.Set.t ->
  (Dba.address * Region_bitvector.t) list Dba_types.Caddress.Map.t ->
  Cfgraph.G.t


val cfg_dba_of_ast:
  t ->
  Dba_types.Caddress.Map.key option ->
  Dba_types.Caddress.Set.t ->
  (Dba.address * Region_bitvector.t) list Dba_types.Caddress.Map.t ->
  Cfgraph.G.t
