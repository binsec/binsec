(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
(*    VERIMAG                                                             *)
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
(**************************************************************************)

type read_map = (int,string) Hashtbl.t * int

type symb_file = read_map * string list
type symb_files = (int64, symb_file) Hashtbl.t

val init : unit -> symb_files

val get_aliases: symb_files -> (string * string) list

val get_vals :  symb_files -> int64 -> (int * string) list

val update_file: string -> (int * int) list -> unit

val add_read : symb_files -> int64 -> string list -> unit

val add_mmap: symb_files -> int64 -> string list -> unit
