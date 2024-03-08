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

(** Worklist / Work queue implementation *)

(** {1 Module type} *)
module type S = sig
  type elt
  type t

  val empty : t
  val singleton : elt -> t
  val is_empty : t -> bool
  val length : t -> int
  val add : elt -> t -> t
  val remove : t -> t
  val pop : t -> elt * t
  val peek : t -> elt
  val merge : t -> t -> t
  val iter : (elt -> unit) -> t -> unit
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val map : (elt -> elt) -> t -> t
end

(** {2 Functors} *)

(** Priority queues over comparable types *)
module Make : functor (X : Sigs.COMPARABLE) -> S with type elt = X.t

(** Priority queues with generated comparison function.
      Added benefit: insertion at the front/rear can be guaranteed.
      Culprit: does not behave as a set (i.e. the same element of [X.t] w.r.t to
      [=] can be inserted twice in the queue).
*)
module CMake : functor (X : Sigs.ANY) -> sig
  include S with type elt = X.t

  val cons : elt -> t -> t
  (** [cons e h] inserts [e] at the front of the worklist [h] *)

  val snoc : elt -> t -> t
  (** [snoc e h] inserts [e] at the rear of the worklist [h] *)
end
