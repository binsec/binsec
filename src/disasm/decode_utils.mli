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

val int64_to_littleendian_bin : Int64.t -> int -> string
val hex_string_to_bin : string -> string
val string_to_hex : ?with_space:bool -> string -> string
val little_string_to_big_string : ?with_space:bool -> string -> string
val string_to_big_int : string -> Z.t
val decode_hex_opcode : ?addr:Int64.t -> string -> string * Dhunk.t
val decode_bin_opcode : ?addr:int64 -> string -> string * Dhunk.t
val decode_opcode : ?addr:Int64.t -> Binstream.t -> string * Dhunk.t
val int64_to_char : int64 -> char
