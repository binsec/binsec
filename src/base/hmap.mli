(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

module type S = sig
  type key
  type !'a t

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t

  val union_eq : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  (** [union_eq f m m'] is equivalent to
        [merge
          (fun k d d' ->
            match d, d' with
            | None, _ -> d' | _, None -> d
            | Some a, Some a' -> if a == a' then d else Some (f k a a'))
          m m']
  *)

  val union_map_eq :
    (key -> 'a -> 'a -> 'a) -> (key -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  (** [union_map_eq f g m m'] is equivalent to
        [merge
          (fun k d d' ->
            match d, d' with
            | None, None -> assert false
            | None, Some a' -> Some (g k a')
            | Some a, None -> Some (g k a)
            | Some a, Some a' -> if a == a' then d else Some (f k a a'))
          m m']
  *)

  val freeze : 'a t -> unit
  val bindings : 'a t -> (key * 'a) list
  val choose : 'a t -> key * 'a
  val mem : key -> 'a t -> bool
  val find : key -> 'a t -> 'a
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val is_empty : 'a t -> bool
end

module Make (H : sig
  type t

  val hash : t -> int
  val compare : t -> t -> int
end) : S with type key = H.t
