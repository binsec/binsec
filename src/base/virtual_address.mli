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

(** {2 Virtual addresses}

    A virtual address is a simple location information corresponding to a
    physical (virtual) address of the underlying machine.
*)

type t = private int
val create : int -> t
val to_int : t -> int
val equal: t -> t -> bool

val of_int64 : int64 -> t
val of_bitvector : Bitvector.t -> t

val to_int64 : t -> int64

val of_bigint : Bigint.t -> t
val to_bigint : t -> Bigint.t

val of_string : string -> t

val add_int : int -> t -> t
val succ : t -> t
val pred : t -> t

include Sigs.PRINTABLE with type t := t
include Sigs.Collection with type t := t

val pp_set : Format.formatter -> Set.t -> unit
