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

type section_flag = Read | Write | Exec

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

type s8   = int
type s16  = int
type s32  = int
type s64  = int (* Bye bye 32 bits. 63 bits ought to be enough for anyone. *)
