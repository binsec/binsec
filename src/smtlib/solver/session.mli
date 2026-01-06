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

type status = Binsec_smtlib_bindings.status = Sat | Unsat | Unknown

val pp_status : Format.formatter -> status -> unit

module type S = sig
  type t
  type arg

  val open_session : arg -> t
  val put : t -> (Format.formatter -> 'a -> unit) -> 'a -> unit
  val comment : t -> (Format.formatter -> 'a -> unit) -> 'a -> unit
  val check_sat : t -> timeout:float -> status

  val check_sat_assuming :
    t -> timeout:float -> (Format.formatter -> 'a -> unit) -> 'a -> status

  val get_value :
    t ->
    (Format.formatter -> 'a -> unit) ->
    'a ->
    (Smtlib.term * Smtlib.term) list

  val get_model : t -> Smtlib.model
  val close_session : t -> unit
  val pp : Format.formatter -> t -> unit
end

module Spawn : S with type arg = Solver.t

module Dump : S with type arg = string
(** [!module-Dump] is a dummy session that outputs its input stream to a file.

    [!val-check_sat] and [!val-check_sat_assuming] always return [!constructor-Unknown].

    [!val-get_value] and [!val-get_model] return dummy results.
*)

(** [!module-Carbon_copy] performs the same as the [Main] session while outputting its input stream to a file.
*)
module Carbon_copy (Main : S) : S with type arg = Main.arg * string
