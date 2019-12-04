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


module CommonSize = struct
  include Natural

  let pp_hex ppf t = Format.fprintf ppf "%x" (to_int t)

  let of_string s = int_of_string s |> create
  let of_int32 n = Int32.to_int n |> create
end


module Bit = struct
  include CommonSize

  let bits1 = create 1
  let bits8 = create 8
  let bits16 = create 16
  let bits32 = create 32
  let bits64 = create 64
  let bits128 = create 128
end


module Byte = struct
  include CommonSize

  module Cst = Basic_types.Constants

  let b = Cst.bytesize

  let to_bitsize n = mul n b

  let of_bitsize n =
    assert(to_int n mod (b:>int) = 0);
    div n b

  let unsafe_of_bits n = of_bitsize (create n)
end
