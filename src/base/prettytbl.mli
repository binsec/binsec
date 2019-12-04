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

type alignment = L | C | R

module Column : sig
  type t

  val default : t

  val make: ?min_length:int -> ?max_length:int
            -> ?left_border:string -> ?right_border:string
            -> ?align:alignment -> unit -> t
  (** [make ~min_lenght ~max_length ~left_border ~right_border ~align ()] *)
end

type t

val make: Column.t array -> t
(** [make columns] create a new table of [Array.length columns] columns *)

val append: t -> string array -> unit
(** [append tbl row] append a new row [row] to the table [tbl]. [row] should
    have the length equal to the column array used to create [tbl] *)

include Sigs.PRINTABLE with type t := t
