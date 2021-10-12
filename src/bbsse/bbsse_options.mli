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

(** Definition of command-line & programmatic options for BBSSE *)

include Cli.S

module MaxCondition : Cli.INTEGER_OPT

module MaxConditionCycle : Cli.INTEGER_OPT

module ProcessAllJumps : Cli.BOOLEAN

module GenGroundTruth : Cli.BOOLEAN

module FindJumps : Cli.BOOLEAN

module OPFile : Cli.STRING_OPT

module IgnoreAddr : Cli.STRING_OPT

module MaxDepth : Cli.INTEGER

module Address_counter : sig
  type t = private { address : Virtual_address.t; counter : int }

  val check_and_decr : t -> t option

  val init : Virtual_address.t -> int -> t
end

module Visit_address_counter :
  Cli.CHECKABLE with type t = Address_counter.t list
