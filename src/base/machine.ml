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

type bitwidth = [ `x16 | `x32 | `x64 | `x128 ]
type endianness = LittleEndian | BigEndian

type isa =
  | Unknown
  | ARM of { rev : [ `v7 | `v8 ]; endianness : endianness }
  | PPC of { bits : [ `x32 | `x64 ]; endianness : endianness }
  | RISCV of { bits : [ `x32 | `x64 | `x128 ] }
  | X86 of { bits : [ `x16 | `x32 | `x64 ] }
  | Z80

let unknown_msg =
  "Machine ISA set to unknown. Aborting. Did you forget to set an -isa switch \
   on the command line ?"

module ISA = struct
  type t = isa

  let endianness = function
    | Unknown -> failwith unknown_msg
    | ARM { endianness; _ } -> endianness
    | PPC { endianness; _ } -> endianness
    | RISCV _ -> LittleEndian
    | X86 _ -> LittleEndian
    | Z80 -> LittleEndian

  let bits = function
    | Unknown -> failwith unknown_msg
    | ARM { rev = `v7; _ } -> `x32
    | ARM { rev = `v8; _ } -> `x64
    | PPC { bits; _ } -> (bits :> bitwidth)
    | RISCV { bits; _ } -> (bits :> bitwidth)
    | X86 { bits; _ } -> (bits :> bitwidth)
    | Z80 -> `x16

  let stack_register = function
    | Unknown -> failwith unknown_msg
    | ARM _ -> "sp"
    | PPC _ -> "r1"
    | RISCV _ -> "x2"
    | X86 _ -> "esp"
    | Z80 -> "sp"

  let to_string = function
    | Unknown -> "unknown"
    | ARM { rev = `v7; _ } -> "armv7"
    | ARM { rev = `v8; _ } -> "armv8"
    | PPC _ -> "powerpc"
    | RISCV _ -> "risk-v"
    | X86 _ -> "x86"
    | Z80 -> "Z80"

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

(** Word size of the machine in bits *)
module Bitwidth = struct
  type t = bitwidth

  let bitsize = function
    | `x16 -> Size.Bit.bits16
    | `x32 -> Size.Bit.bits32
    | `x64 -> Size.Bit.bits64
    | `x128 -> Size.Bit.bits128

  let bytesize t = Size.Byte.of_bitsize (bitsize t)
  let pp ppf t = Size.Bit.pp ppf (bitsize t)

  let pp_print_hex t ppf x =
    match t with
    | `x16 -> Format.fprintf ppf "%04x" x
    | `x32 -> Format.fprintf ppf "%08x" x
    | `x64 -> Format.fprintf ppf "%016x" x
    | `x128 -> Format.fprintf ppf "%032x" x
end

module Endianness = struct
  type t = endianness

  let pp ppf = function
    | LittleEndian -> Format.fprintf ppf "little endian"
    | BigEndian -> Format.fprintf ppf "big endian"
end

type t = isa

let amd64 = X86 { bits = `x64 }
let armv7 endianness = ARM { rev = `v7; endianness }
let armv8 endianness = ARM { rev = `v8; endianness }
let ppc64 endianness = PPC { bits = `x64; endianness }
let riscv bits = RISCV { bits }
let x86 = X86 { bits = `x32 }
let z80 = Z80
let unknown = Unknown
let pp = ISA.pp
