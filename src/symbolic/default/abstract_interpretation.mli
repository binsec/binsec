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

open Types

module type CONTEXT = sig
  type 'a t
  (** context *)

  val domain : 'a t -> (module Domains.S with type t = 'a)
  val add_dependency : 'a t -> parent:Expr.t -> Expr.t -> unit
  val find_dependency : 'a t -> Expr.t -> BvSet.t
  val add_value : 'a t -> Expr.t -> 'a -> unit
  val find_value : 'a t -> Expr.t -> 'a
end

module type S = sig
  type 'a t

  val domain : 'a t -> (module Domains.S with type t = 'a)
  val eval : 'a t -> Expr.t -> 'a
  val refine : 'a t -> Expr.t -> 'a -> unit
end

module Make (C : CONTEXT) : S with type 'a t := 'a C.t
