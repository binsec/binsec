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

(** {1 Mnemonic } *)

type t = private
  | Unknown  (** Opcodes which do not have a valid mnemonic translation *)
  | Unsupported of string option
      (** Opcodes which have a valid mnemonic but do not have a handled mnemonic translation *)
  | Supported of string

val supported : 'a -> (Format.formatter -> 'a -> unit) -> t
val unsupported : ?mnemonic_hint:string -> unit -> t
val unknown : t
val pp : Format.formatter -> t -> unit
val to_string : t -> string
