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

type 'a key = 'a Types.key

module type S = sig
  type t

  val id : t -> int
  val get : 'a key -> t -> 'a
  val set : 'a key -> 'a -> t -> unit
  val register_key : ?merge:('a -> 'a -> 'a option) -> 'a -> 'a key
  val register_at_fork : (t -> t -> unit) -> unit
  val register_at_end : (t -> Types.status -> unit) -> unit
end

module Make () : sig
  include S

  val empty : unit -> t
  val fork : t -> t
  val merge : t -> t -> t option
  val terminate : t -> Types.status -> unit
end
