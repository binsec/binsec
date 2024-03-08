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

(** Abstract description of machines *)

(** Abstract representation of hardware architecture *)

type bitwidth = [ `x16 | `x32 | `x64 | `x128 ]
type endianness = LittleEndian | BigEndian

type isa = private
  | Unknown
  | ARM of { rev : [ `v7 | `v8 ]; endianness : endianness }
  | PPC of { bits : [ `x32 | `x64 ]; endianness : endianness }
  | RISCV of { bits : [ `x32 | `x64 | `x128 ] }
  | X86 of { bits : [ `x16 | `x32 | `x64 ] }
  | Z80

module ISA : sig
  include Sigs.PRINTABLE with type t = isa

  val endianness : t -> endianness
  val bits : t -> bitwidth
  val stack_register : t -> string
  val to_string : isa -> string
end

(** Word size of the machine in bits *)
module Bitwidth : sig
  include Sigs.PRINTABLE with type t = bitwidth

  val bitsize : t -> Size.Bit.t
  val bytesize : t -> Size.Byte.t
  val pp_print_hex : t -> Format.formatter -> int -> unit
end

module Endianness : Sigs.PRINTABLE with type t = endianness

type t = isa

val amd64 : t
val armv7 : endianness -> t
val armv8 : endianness -> t
val ppc64 : endianness -> t
val riscv : [ `x32 | `x64 | `x128 ] -> t
val x86 : t
val z80 : t
val unknown : t

include Sigs.PRINTABLE with type t := t
