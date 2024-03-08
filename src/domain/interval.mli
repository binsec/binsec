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

type t = private { min : Z.t; max : Z.t; stride : int }
(** Represents the unsigned fixed-width integer interval
    between [min] and [max], with [stride] fixed bits. *)

include Common.S with type t := t

val zeros : int -> t
val zero : t
val ones : int -> t
val one : t
val create : size:int -> min:Z.t -> max:Z.t -> stride:int -> t
val mem : Z.t -> size:int -> t -> bool
val iter : (Z.t -> unit) -> size:int -> t -> unit
val fold : (Z.t -> 'a -> 'a) -> 'a -> size:int -> t -> 'a
val for_all : (Z.t -> bool) -> size:int -> t -> bool
val sum : size:int -> t -> Z.t
