(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

(** Backward analysis for better precision  *)
val backward_refine_elements :
  Dba_types.AddressStack.Map.key ->
  ('a * 'b * 'c * Dba_types.AddressStack.Set.t) Dba_types.AddressStack.Map.t ->
  (Dba.Instr.t * 'd) Dba_types.Caddress.Map.t ->
  ('a ->
   int Basic_types.String.Map.t ->
   Formula.VarSet.t -> Formula.bl_term list * Formula.VarSet.t) ->
  int Dba_types.Caddress.Map.t ->
  Region_bitvector.t list *
  (Formula.def list * Formula.bl_term list *
   Formula.VarSet.t * string * int Basic_types.String.Map.t)
