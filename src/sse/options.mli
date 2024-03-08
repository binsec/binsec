(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
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

(** Definition of command-line & programmatic options for SSE *)

include Cli.S

module Engine : sig
  type t = ..

  include Cli.GENERIC_OPT with type t := t

  val register : string -> t -> (unit -> (module Types.STATE_FACTORY)) -> unit
  val get_factory : unit -> (module Types.STATE_FACTORY)
end

module AlternativeEngine : Cli.BOOLEAN
module LegacyEngine : Cli.BOOLEAN
module MaxDepth : Cli.INTEGER
module TransientEnum : Cli.INTEGER
module JumpEnumDepth : Cli.INTEGER
module QMerge : Cli.INTEGER
module KillFlagsAtReturn : Cli.BOOLEAN
module Randomize : Cli.BOOLEAN
module ScriptFiles : Cli.STRING_LIST

type warnerror = Error | Warn | Quiet

module MissingSymbol : Cli.GENERIC with type t := warnerror
module Timeout : Cli.INTEGER_OPT
module Monitor : Cli.BOOLEAN

type search_heuristics = Dfs | Bfs | Nurs

module Search_heuristics : Cli.GENERIC with type t = search_heuristics

module Seed : Cli.INTEGER_OPT
(** Seed for the random number generator *)
