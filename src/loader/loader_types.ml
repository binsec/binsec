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

let pp_arch ppf = function
  | Unknown -> Format.fprintf ppf "unknown"
  | AMD64   -> Format.fprintf ppf "AMD_64"
  | ARM     -> Format.fprintf ppf "ARM"
  | ARM64   -> Format.fprintf ppf "ARM_64"
  | IA64    -> Format.fprintf ppf "Itanium"
  | MIPS    -> Format.fprintf ppf "MIPS"
  | PPC     -> Format.fprintf ppf "PowerPC"
  | PPC64   -> Format.fprintf ppf "PowerPC_64"
  | SPARC   -> Format.fprintf ppf "SPARC"
  | XCORE   -> Format.fprintf ppf "xCORE"
  | X86     -> Format.fprintf ppf "x86"

  
let print_arch a =
  pp_arch Format.str_formatter a;
  Format.flush_str_formatter ()

type endian = LittleEndian | BigEndian

let pp_endian ppf = function
  | LittleEndian -> Format.fprintf ppf "LittleEndian"
  | BigEndian    -> Format.fprintf ppf "BigEndian"

let print_endian e =
  pp_endian Format.str_formatter e;
  Format.flush_str_formatter ()

type 'a map = {
  raw  : 'a;
  virt : 'a;
}

type u8   = int
type u16  = int
type u32  = int
type u64  = int

