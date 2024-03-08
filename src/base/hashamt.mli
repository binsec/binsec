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

(** Implementation of Hash-Array Mapped Tries *)

(** HAMT is a very efficient persistent data structures for dictionaries *)

module type S = sig
  type key
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val singleton : key -> 'a -> 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val find : key -> 'a t -> 'a
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val join : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val cardinal : 'a t -> int
  val bindings : 'a t -> (key * 'a) list
end

module Make (H : Hashtbl.HashedType) : S with type key = H.t
