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

type section_flag = Read | Write | Exec
type ('a, 'b) map = { raw : 'a; virt : 'b }

open Basic_types.Integers

type u8 = uint8
type u16 = uint16
type u32 = uint32
type u64 = uint64
type s8 = int8
type s16 = int16
type s32 = int32
type s64 = int64

type buffer =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

exception Invalid_format of string

let invalid_format msg = raise (Invalid_format msg)
let assert_format b msg = if not b then invalid_format msg
let ensure t count msg = if not (Reader.ensure t count) then invalid_format msg
