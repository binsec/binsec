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

module type Value = sig
  type t

  val equal : t -> t -> bool
  val len : t -> int
  val crop : lo:int -> hi:int -> t -> t
  val concat : t -> t -> t
end

module type S = sig
  type t
  type v

  val empty : t
  val is_empty : t -> bool
  val is_empty_between : Z.t -> Z.t -> t -> bool
  val singleton : Z.t -> v -> t
  val store : Z.t -> v -> t -> t
  val select : (Z.t -> int -> v) -> Z.t -> int -> t -> v
  val iter : (Z.t -> v -> unit) -> t -> unit
  val rev_iter : (Z.t -> v -> unit) -> t -> unit
  val fold : (Z.t -> v -> 'a -> 'a) -> 'a -> t -> 'a
  val rev_fold : (Z.t -> v -> 'a -> 'a) -> 'a -> t -> 'a
  val map : (Z.t -> v -> v) -> t -> t
  val merge : (Z.t -> v option -> v option -> v option) -> t -> t -> t
  val choose : t -> Z.t * v
  val bindings : t -> (Z.t * v) list
end

module Make (E : Value) : S with type v := E.t
