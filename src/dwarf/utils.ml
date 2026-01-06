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

open Basic_types.Integers

let read_address format cursor =
  match format with
  | `x32 -> Virtual_address.of_uint32 (Reader.Read.u32 cursor)
  | `x64 -> Virtual_address.of_uint64 (Reader.Read.u64 cursor)

let read ?(signed = false) format cursor =
  match (format, signed) with
  | `x32, true -> Int32.to_int64 (Reader.Read.i32 cursor)
  | `x32, false -> Uint32.to_int64 (Reader.Read.u32 cursor)
  | `x64, true -> Reader.Read.i64 cursor
  | `x64, false -> Uint64.to_int64 (Reader.Read.u64 cursor)

let read_addr isa cursor =
  match Machine.ISA.bits isa with
  | `x32 -> Virtual_address.of_uint32 (Reader.Read.u32 cursor)
  | `x64 -> Virtual_address.of_uint64 (Reader.Read.u64 cursor)
  | _ -> Logger.fatal "unexpected architecture bit size"

let is_max_addr isa addr =
  match Machine.ISA.bits isa with
  | `x32 -> Virtual_address.to_int32 addr = 0xffffffffl
  | `x64 -> Virtual_address.to_int64 addr = 0xffffffffffffffffL
  | _ -> Logger.fatal "unexpected architecture bit size"

let addr_to_string isa addr =
  match Machine.ISA.bits isa with
  | `x32 -> Format.asprintf "%a" (Virtual_address.pp_print `x32) addr
  | `x64 -> Format.asprintf "%a" (Virtual_address.pp_print `x64) addr
  | _ -> Logger.fatal "unexpected architecture bit size"
