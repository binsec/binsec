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

(** {2 Virtual addresses}

    A virtual address is a simple location information corresponding to a
    physical (virtual) address of the underlying machine.
*)

exception Non_canonical_form

type t = private int

val zero : t
val create : int -> t
val to_int : t -> int
val of_int64 : int64 -> t
val of_bitvector : Bitvector.t -> t
val to_int64 : t -> int64
val of_bigint : Z.t -> t
val to_bigint : t -> Z.t
val of_string : string -> t
val to_string : t -> string
val add_int : int -> t -> t
val succ : t -> t
val pred : t -> t
val diff : t -> t -> int

include Sigs.PRINTABLE with type t := t
include Sigs.Collection with type t := t

val pp_set : Format.formatter -> Set.t -> unit
