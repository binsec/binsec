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

type t = private int

external seed : int -> t = "%identity" [@@noalloc]

external fold_int : t -> int -> t
  = "cstubs_hash_fold_int" "cstubs_hash_fold_int_untagged"
[@@noalloc] [@@untagged]

external fold_string : (t[@untagged]) -> string -> (t[@untagged])
  = "cstubs_hash_fold_string" "cstubs_hash_fold_string_untagged"
[@@noalloc]

val return : t -> int [@@inline]
