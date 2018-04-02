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

(** Generic abstract interpretation definitions *)
open Dba_types

module type AbstractAnalysis =
sig
  type t
  type equalities
  type predsSet = Dba_types.AddressStack.Set.t
  type flagsType = High_level_predicate.t
  type statesMap = (t * flagsType * equalities * predsSet) AddressStack.Map.t
  type conditionsList = Smt_bitvectors.smtBvExprAlt list
  type djmpsMap = Caddress.Set.t AddressStack.Map.t
  type stopList = Caddress.Set.t
  type insert_instrs = Dba.instruction list Caddress.Map.t
  type replace_instrs = Dba_types.instruction_sequence Caddress.Map.t
  type localThresholdsType = (int array * int array * int array * int array)
  type globalThresholdsType = (int array * int array * int array * int array) Caddress.Map.t
  type thresholdsType = localThresholdsType * globalThresholdsType
  type wideningType = int * int Caddress.Map.t

  val analyse:
    Dba.address ->
    Dba.instruction list ->
    Disasm_types.pmap ->
    insert_instrs ->
    replace_instrs ->
    stopList ->
    thresholdsType ->
    wideningType ->
    statesMap * conditionsList * Disasm_types.pmap  * djmpsMap * Caddress.Set.t

end


module Make (State: Ai_sigs.AbstractDomain): AbstractAnalysis

val run :
   ?dba_file:string option -> configuration_file:string option -> unit -> unit
