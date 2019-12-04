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

include Cli.S

(** Options for simulation *)

module StepByStep : Cli.BOOLEAN

module FuzzerIterations : Cli.INTEGER

type strategy =
  | Branch_if
  | Branch_else
  | Fail

module Conditional_strategy : Cli.GENERIC with type t = strategy

type semantic_mode =
  | Flat
  | Region
  | Region_load_store
  | Logic
  | Rewrite

module Semantic_mode : Cli.GENERIC with type t = semantic_mode

(* module SemanticsMode : sig
 *   val to_string : unit -> string
 *   val arg : string * Arg.spec * string
 *
 *   val flat_or_not_basic : unit -> bool
 *   val flat_or_basic_and_full : unit -> bool
 *   val basic : unit -> bool
 *   val basic_affine : unit -> bool
 *   val flat : unit -> bool
 * end *)
