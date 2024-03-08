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

(** {2 Size} *)

module type Size = sig
  type t = private Natural.t

  val create : int -> t
  val of_string : string -> t
  val of_int32 : int32 -> t
  val to_int : t -> int
  val eq : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val pp_hex : Format.formatter -> t -> unit
  val add : t -> t -> t
  val sub : t -> t -> t
  val div : t -> t -> t
  val mul : t -> t -> t
  val pred : t -> t
  val is_zero : t -> bool
end

module Bit : sig
  include Size

  val bits1 : t
  val bits2 : t
  val bits4 : t
  val bits8 : t
  val bits16 : t
  val bits32 : t
  val bits64 : t
  val bits80 : t
  val bits128 : t
  val bits256 : t
  val bits512 : t
end

module Byte : sig
  include Size

  val to_bitsize : t -> Bit.t
  val of_bitsize : Bit.t -> t
  val unsafe_of_bits : int -> t
  val one : t
  val two : t
  val four : t
  val eight : t
  val fifteen : t
  val sixteen : t
  val thirtytwo : t
end
