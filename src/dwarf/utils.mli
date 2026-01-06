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

val read_address : [ `x32 | `x64 ] -> int Reader.t -> Virtual_address.t
(** [read_address] reads an address of either 32 or 64 bits
    according to the given wordsize. *)

val read : ?signed:bool -> [ `x32 | `x64 ] -> int Reader.t -> int64
(** [read] reads a value of either 32 or 64 bits
    according to the given wordsize. *)

val read_addr : Machine.t -> int Reader.t -> Virtual_address.t
(** [read_addr] reads an address of either 32 or 64 bits
    according to the given architecture. *)

val is_max_addr : Machine.t -> Virtual_address.t -> bool
(** [is_max_addr isa addr] returns true if [addr] is the largest
    valid address for the given architecture. *)

val addr_to_string : Machine.t -> Virtual_address.t -> string
(** [addr_to_string isa addr] returns the string representation of
    a 32 or 64 bits address according to the given architecture. *)
