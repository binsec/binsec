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

(** Extra functions over lists *)

val is_empty : 'a list -> bool
(** [is_empty l] returns [true] if [l = []] *)

val take : int -> 'a list -> 'a list
(** [take n l] returns up to the [n] first elements of list [l] *)

val take_while : (int -> 'a -> bool) -> 'a list -> 'a list

val drop : int -> 'a list -> 'a list
(** [drop n l] removes the [n] first elements of list [l]
    if n is greater than the length of the list, returns [].
    @assumes n >= 0
*)

val last : 'a list -> 'a
(** [last l] returns the last element of list l.
    Raise [Failure "last"] if the list is empty
*)

val drop_last : 'a list -> 'a list
(** [drop_last l] returns list [l] without its last element.
    Raise [Failure "drop_last"] if the list is empty
*)

val rev_flatten : 'a list list -> 'a list
(** [rev_flatten l] reverses and flatten the list of list [l] at the same time.
    It is the same as doing [List.flatten l |> List.rev] but tail-recursive and
    more efficient.
 **)

val flat_map : ('a -> 'b list) -> 'a list -> 'b list
(** [flat_map f l] is like [List.map f l |> List.flatten] but tail-recusrive
    and more efficient *)

val hd_hd : 'a list -> 'a * 'a
(** [hd_hd l] gets the two first elements of a list.
    @raise Failure "hd_hd" if [l] is empty or the singleton list
*)

val pop : 'a list -> 'a * 'a list
(** [pop l] returns the head and tail of the list.contents
    @raise Failure "pop" if [l] is empty
*)

val make : int -> 'a -> 'a list
(** [make n x] returns a list of x of length n.
    @assumes n >= 0
*)

val map_if : ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list
(** [map_if p f l] behaves like [map f l] but applied only on elements of
    [l] verifying [p].

    Tail recursive.
*)

val filter_map : ('b -> bool) -> ('a -> 'b) -> 'a list -> 'b list
(** [filter_map p f l] behaves like [map f l] but applied only on elements [e]
    of [l] verifying [p (f e)].

    Tail recursive.
*)

val eq_length : 'a list -> 'b list -> bool
(** [eq_lenght l1 l2] returns [true] if [l1] and [l2] have the same length,
 ** independently of what their cells contain *)

val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int
(** [compare f l1 l2] compares [l1] and [l2] according to lexicographic order
    using [f] as the order on elements. *)
