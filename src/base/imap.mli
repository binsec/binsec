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

(* TODO: poor man interval map -- sorted list *)

type 'a t

val empty : 'a t

val add : base:Z.t -> int -> 'a -> 'a t -> 'a t
(** [add ~base size value t]
    add a new pair (\[[base] .. [base] + [size]\[, [value]) in [t]
*)

val mem : Z.t -> 'a t -> bool
(** [mem index t]
    check if [index] is in [t].
*)

val find : Z.t -> 'a t -> 'a
(** [find index t]
    lookup [index] in [t].

    @raise Not_found if [index] is not in [t].
*)

val iter : (Z.t * Z.t -> 'a -> unit) -> 'a t -> unit
val fold : (Z.t * Z.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val map : ('a -> 'b) -> 'a t -> 'b t
