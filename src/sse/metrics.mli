(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

type status =
  | Halt
  | Cut
  | Merged
  | Stashed
  | Unsatisfiable_assumption
  | Assertion_failure
  | Max_depth
  | Enumeration_limit
  | Unresolved_formula
  | Non_executable_code
  | Error of string

val pp_status : Format.formatter -> status -> unit

module type TIMER = sig
  type t

  val get : t -> float
  val start : t -> unit
  val stop : t -> unit
end

module type ASPECT = sig
  type t

  val get : t -> int
  val incr : t -> unit

  module Timer : TIMER with type t := unit

  val pp : Format.formatter -> unit -> unit
  val to_toml : unit -> Toml.Types.table
end

module type QUERY = sig
  module Preprocess : ASPECT with type t := Binsec_smtlib.Solver.status
  module Solver : ASPECT with type t := Binsec_smtlib.Solver.status

  val reset : unit -> unit
  val pp : Format.formatter -> unit -> unit
  val to_toml : unit -> Toml.Types.table
end

module Query () : QUERY

module type EXPLORATION = sig
  module Paths : sig
    type t = Total | Pending | Completed | Discontinued

    val get : t -> int
    val status : status -> int
    val incr : unit -> unit
    val resume : unit -> unit
    val signal : status -> unit
  end

  module Topology : sig
    type t = Branch | Jump | Assert

    val get : t -> int
    val incr : t -> unit
  end

  module Max_depth : sig
    val get : unit -> int
    val update : int -> unit
  end

  module Addresses : sig
    val unique : unit -> int
    val register : Virtual_address.t -> unit
  end

  module Instructions : sig
    val get : unit -> int
    val incr : int -> unit
  end

  module Timer : TIMER with type t := unit

  val reset : unit -> unit
  val pp : Format.formatter -> unit -> unit
  val to_toml : unit -> Toml.Types.table
end

module Exploration () : EXPLORATION
