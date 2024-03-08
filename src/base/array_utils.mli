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

(** Extra functions over arrays *)

val find : ('a -> bool) -> 'a array -> 'a
(** [find p a] returns the first element of the array [a] that satisfies the predicate [p].
    @raise Not_found if there is no value that satisfies [p] in the array [a]
*)

val findi : ('a -> bool) -> 'a array -> int
(** [find p a] returns the index of the first element of the array [a] that
               satisfies the predicate [p].
    @raise Not_found if there is no value that satisfies [p] in the array [a]
*)

val find_opt : ('a -> bool) -> 'a array -> 'a option
(** [find p a] returns the first element of the array [a] that satisfies
    the predicate [p] or None.
*)

val fold_lefti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b array -> 'a
(** Same as Array.fold_left, but the function is applied with the index
    of the element as first argument, and the element itself as third argument
*)

val fold_righti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b array -> 'a
(** Same as Array.fold_right, but the function is applied with the index
    of the element as first argument, and the element itself as third argument
*)
