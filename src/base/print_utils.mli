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

(** Extra pretty-printing functions *)

val pp_list :
  ?pre:string -> ?post:string -> ?sep:string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit

val pp_as_string : ('a -> string) -> Format.formatter -> 'a -> unit

val pp_opt_as_string : ('a -> string) -> Format.formatter -> 'a option -> unit

val pp_opt :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit

val string_from_pp : (Format.formatter -> 'a -> unit) -> 'a -> string

val pp_dba_prelude : ?flat_memory:bool -> Format.formatter -> unit -> unit 

val pp_byte : ?prefixed:bool -> Format.formatter -> int -> unit
(** [pp_byte prefixed ppf by] prints as hexadecimal byte [by] into [ppf]
    If [prefixed] is [true] (default) it also prepends "0x".
    This function assumes that [by] is between 0 ad 255.
 *)

val pp_to_file :
  filename:string -> (Format.formatter -> 'a -> unit) -> 'a -> unit 
