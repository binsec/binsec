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

(** Generic signatures used throughout BINSEC *)

module type ANY = sig
  type t
end

module type PRINTABLE = sig
  type t

  val pp : Format.formatter -> t -> unit
end

module type STR_INJECTIBLE = sig
  type t

  val of_string : string -> t
end

module type STRINGIFIABLE = sig
  include STR_INJECTIBLE

  val to_string : t -> string
end

module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type COMPARABLE_EXT = sig
  include COMPARABLE

  val equal : t -> t -> bool
end

module type HASHABLE_AUTO = sig
  include COMPARABLE

  val hash : t -> int
end

module type HASHABLE = sig
  include COMPARABLE

  val equal : t -> t -> bool
  val hash : t -> int
end

module type ITERABLE = sig
  include COMPARABLE

  val succ : t -> t
  val pred : t -> t
end

module type EQ = sig
  type t

  val equal : t -> t -> bool
end

module type COMPARISON = sig
  type t
  type boolean

  val equal : t -> t -> boolean
  val diff : t -> t -> boolean
  val ule : t -> t -> boolean
  val uge : t -> t -> boolean
  val ult : t -> t -> boolean
  val ugt : t -> t -> boolean
  val sle : t -> t -> boolean
  val sge : t -> t -> boolean
  val slt : t -> t -> boolean
  val sgt : t -> t -> boolean
end

module type ARITHMETIC = sig
  type t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val neg : t -> t

  (* Unsigned operations *)
  val udiv : t -> t -> t
  val umod : t -> t -> t
  val urem : t -> t -> t

  (* Signed operations *)
  val sdiv : t -> t -> t
  val smod : t -> t -> t
  val srem : t -> t -> t
end

module type LOGICAL = sig
  type t

  val logand : t -> t -> t
  val logor : t -> t -> t
  val lognot : t -> t
end

module type EXTENDED_LOGICAL = sig
  include LOGICAL

  val logxor : t -> t -> t
end

module type SHIFT_ROT = sig
  type t
  type index

  val shift_left : t -> index -> t
  val shift_right : t -> index -> t
  val shift_right_signed : t -> index -> t
  val rotate_left : t -> index -> t
  val rotate_right : t -> index -> t
end

module type BITWISE = sig
  include EXTENDED_LOGICAL
  include SHIFT_ROT with type t := t and type index := int
end
