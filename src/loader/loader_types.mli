(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
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

type arch =
  | Unknown
  | AMD64
  | ARM
  | ARM64
  | IA64
  | MIPS
  | PPC
  | PPC64
  | SPARC
  | XCORE
  | X86

val pp_arch : Format.formatter -> arch -> unit
val print_arch : arch -> string

type endian = LittleEndian | BigEndian

val pp_endian : Format.formatter -> endian -> unit
val print_endian : endian -> string

type 'a map = {
  raw  : 'a;
  virt : 'a;
}

(** Some aliases to make more explicit the nature of values being read. As a
    first approximation, all values are expected to fit in OCaml integers. *)
type u8   = int
type u16  = int
type u32  = int
type u64  = int (* Bye bye 32 bits. 63 bits ought to be enough for anyone. *)
