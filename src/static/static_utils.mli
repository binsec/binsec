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

(** Common functions for static analyses *)
val update_instr_map:
   Dba.address ->
   Dba_types.Caddress.Map.key ->
   Disasm_types.pmap ->
   Dba_types.Caddress.Set.t ->
   'a Dba_types.Caddress.Map.t ->
   'a * 'a Dba_types.Caddress.Map.t ->
   Dba_types.Caddress.Set.t Dba_types.Caddress.Map.t ->
   Dba_types.Caddress.Set.t Dba_types.Caddress.Map.t ->
   (Dba.instruction * Disasm_types.GenericInstruction.t option) *
   Disasm_types.pmap
   * 'a Dba_types.Caddress.Map.t *
   Dba_types.Caddress.Set.t Dba_types.Caddress.Map.t
