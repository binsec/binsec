(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2021                                               *)
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

module Path_generator : sig
  type t

  val init :
    Ghidra_cfg.vertex ->
    ((Virtual_address.t * int) Stack.t
    * bool Stack.t Stack.t
    * Bitvector.t Stack.t Stack.t)
    * ((Virtual_address.t * int) Stack.t
      * bool Stack.t Stack.t
      * Bitvector.t Stack.t Stack.t)
      option

  val reinit :
    Ghidra_cfg.vertex ->
    Ghidra_cfg.t ->
    Ghidra_cfg.vertex Ghidra_cfg.Vtbl.t ->
    string Ghidra_cfg.Vtbl.t ->
    ((Virtual_address.t * int) Stack.t
    * bool Stack.t Stack.t
    * Bitvector.t Stack.t Stack.t)
    * ((Virtual_address.t * int) Stack.t
      * bool Stack.t Stack.t
      * Bitvector.t Stack.t Stack.t)
      option
end
