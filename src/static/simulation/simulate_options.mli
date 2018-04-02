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

(** Options for simulation *)

module StepByStep : Parameters.Boolean

module FuzzerIterations : Parameters.Integer

module ConditionalStrategy : sig
  type t = private
    | Normal
    | Else

  val set : string -> unit
  val get : unit -> t option
  val cli_handler: Arg.spec
end


module SemanticsMode : sig
  val to_string : unit -> string
  val arg : string * Arg.spec * string

  val flat_or_not_basic : unit -> bool
  val flat_or_basic_and_full : unit -> bool
  val basic : unit -> bool
  val basic_affine : unit -> bool
  val flat : unit -> bool
end
