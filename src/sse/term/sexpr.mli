(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2022                                               *)
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

module rec Expr : (Term.S with type a := unit and type b := Memory.t)

and Memory : sig
  type t =
    | Unknown
    | Source of {
        id : int;
        over : t;
        addr : Bv.t;
        orig : Loader_buf.t;
        len : int;
      }
    | Layer of {
        id : int;
        over : t;
        addr : Expr.t;
        bytes : Expr.t BiMap.t;
        pop : int;
      }

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val hash : t -> int

  val source : addr:Bv.t -> len:int -> Loader_buf.t -> t -> t

  val write : addr:Expr.t -> Expr.t -> Expr.endianness -> t -> t

  val read : addr:Expr.t -> int -> Expr.endianness -> t -> Expr.t
end

module BvTbl : Hashtbl.S with type key = Expr.t

module AxTbl : Hashtbl.S with type key = Memory.t
