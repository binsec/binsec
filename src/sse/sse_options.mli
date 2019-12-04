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

(** Definition of command-line & programmatic options for SSE *)

include Cli.S

module MaxDepth : Cli.INTEGER

module JumpEnumDepth : Cli.INTEGER

module Randomize : Cli.BOOLEAN

module KeepGoing : Cli.BOOLEAN

module SMT_dir : Cli.STRING_OPT

module AddressTraceFile : Cli.STRING_OPT

module AvoidAddresses : Cli.INTEGER_SET

module GoalAddresses : Cli.INTEGER_SET

module LoadSections : Cli.STRING_SET

module LoadROSections : Cli.BOOLEAN

module MemoryFile : Cli.STRING

module Comment : Cli.BOOLEAN

module Timeout : Cli.FLOAT

module Address_counter : sig
  type t  = private {
    address : Virtual_address.t;
    counter : int;
    }

  val check_and_decr : t -> t option
end

module Visit_address_counter :
  Cli.CHECKABLE with type t = Address_counter.t list

type search_heuristics =
  | Dfs
  | Bfs
  | Nurs

module Search_heuristics : Cli.GENERIC with type t = search_heuristics

module Solver_call_frequency : Cli.INTEGER
(** Define the frequency -- in terms of number of conditionals -- with which we
    call the solver *)

module Seed : Cli.INTEGER_OPT
(** Seed for the random number generator *)


module Directives : Cli.GENERIC with type t = Directive.t list

module Dot_filename_out : Cli.STRING_OPT

module SMT_log_directory : Cli.STRING
