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

module Command : sig
  type t = string array

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

type t = Boolector | Bitwuzla | Z3 | CVC4 | Yices

val pp : Format.formatter -> t -> unit
val is_boolector : t -> bool
val is_yices : t -> bool

val ping : t -> bool
(** [ping solver] return true if the command was found in the path. *)

val name_of : t -> string
(** {2 Accessors} *)

val command : ?incremental:bool -> ?options:string -> int -> t -> Command.t
val timeout_s : int -> t -> int
