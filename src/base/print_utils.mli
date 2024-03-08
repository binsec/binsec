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

(** Extra pretty-printing functions *)

type sformat = (unit, Format.formatter, unit) format
type 'a formatter = Format.formatter -> 'a -> unit

val pp_list :
  ?pre:sformat ->
  ?post:sformat ->
  ?sep:sformat ->
  'a formatter ->
  'a list formatter
(** [pp_list ~pre ~post ~sep pp_e ppf l] pretty-prints list [l] between opening
    formatting indication [pre] and closing formatting indication [post] using
    pretty-printer [pp_e] to print its elements, separating by formatting
    indication [sep].

    Default values are:
    - the empty string formatting indication for [pre] and [post]
    - ";" for [sep]
*)

val pp_as_string : ('a -> string) -> 'a formatter
(** [pp_as_string f ppf v] *)

val pp_opt_as_string : ('a -> string) -> 'a option formatter
val pp_opt : 'a formatter -> 'a option formatter

val string_from_pp : 'a formatter -> 'a -> string
(** [string_from_pp pp v] renders value [v] as string according to its
    pretty-printer [pp] *)

val pp_dba_prelude : ?flat_memory:bool -> unit formatter

val pp_byte : ?prefixed:bool -> int formatter
(** [pp_byte prefixed ppf by] prints as hexadecimal byte [by] into [ppf]
    If [prefixed] is [true] (default) it also prepends "0x".
    This function assumes that [by] is between 0 ad 255.
*)

val pp_to_file : filename:string -> 'a formatter -> 'a -> unit
