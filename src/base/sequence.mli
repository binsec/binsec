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

type 'a t
(** an ordered list from which we can push and pop at both sides *)

val empty : 'a t
val length : 'a t -> int

val append : 'a t -> 'a t -> 'a t
(** append a b is the equivalent of b @ a with lists *)

(*

         forward

     +--------------->
back                    front
     <---------------+

         backward

*)

val push_front : 'a -> 'a t -> 'a t
(** appends an element at the front of the sequence *)

val push_back : 'a -> 'a t -> 'a t
(** appends an element at the back of the sequence *)

val peek_front : 'a t -> 'a option
(** returns the element at the front of the sequence *)

val peek_back : 'a t -> 'a option
(** returns the element at the back of the sequence *)

val pop_front : 'a t -> 'a t option
(** removes the element at the front of the sequence *)

val pop_back : 'a t -> 'a t option
(** removes the element at the back of the sequence *)

val map_forward : ('a -> 'b) -> 'a t -> 'b t
(** map, with guaranteed side effect from back to front *)

val map_backward : ('a -> 'b) -> 'a t -> 'b t
(** map, with guaranteed side effect from front to back *)

val iter_forward : ('a -> unit) -> 'a t -> unit
(** iter, with guaranteed side effect from back to front *)

val iter_backward : ('a -> unit) -> 'a t -> unit
(** iter, with guaranteed side effect from front to back *)

val fold_forward : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** fold from back to front *)

val fold_backward : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** fold from front to back *)

val to_seq_forward : 'a t -> 'a Seq.t
(** creates a Seq.t which iterates from back to front
 * Not intended to be particularly performant *)

val to_seq_backward : 'a t -> 'a Seq.t
(** creates a Seq.t which iterates from front to back
 * Not intended to be particularly performant *)
