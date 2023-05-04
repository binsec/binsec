(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2023                                               *)
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

module Bv = Bitvector
module BiMap = Basic_types.BigInt.Map

module rec Expr : (Term.S with type a := string and type b := Memory.t)

and Memory : sig
  type t =
    | Root
    | Symbol of string
    | Layer of { id : int; over : t; addr : Expr.t; store : Store.t }
    | Overlay of { id : int; over : t; addr : Expr.t; store : Store.t }

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val hash : t -> int

  val source : addr:Expr.t -> len:int -> Loader_buf.t -> t -> t

  val write : addr:Expr.t -> Expr.t -> Expr.endianness -> t -> t

  val read : addr:Expr.t -> int -> Expr.endianness -> t -> Expr.t * t

  val merge : Expr.t -> t -> t -> t
end

and Store : sig
  type t

  val iter : (Z.t -> Expr.t -> unit) -> t -> unit

  val fold : (Z.t -> Expr.t -> 'a -> 'a) -> 'a -> t -> 'a
end

module BvTbl : Hashtbl.S with type key = Expr.t

module AxTbl : Hashtbl.S with type key = Memory.t

module BiTbl : Hashtbl.S with type key = Z.t

module StTbl : Hashtbl.S with type key = string

module Model : sig
  type t = Bv.t BvTbl.t * char BiTbl.t * char BiTbl.t StTbl.t

  val empty : unit -> t

  val eval : t -> Expr.t -> Bv.t

  val pp :
    Format.formatter -> Expr.t list Basic_types.String.Map.t -> int -> t -> unit
end
