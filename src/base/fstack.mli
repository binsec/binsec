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

(** Simple functional stack *)

module type Typed = sig
  type t
end

module Make (X : Typed) : sig
  type elem = X.t
  type t

  val empty : t
  val singleton : elem -> t
  val push : elem -> t -> t

  val pop : t -> elem * t
  val top : t -> elem

  val is_empty : t -> bool
  val length : t -> int

  val iter : (elem -> unit) -> t -> unit
  val fold : ('a -> elem -> 'a) -> 'a -> t -> 'a
end
