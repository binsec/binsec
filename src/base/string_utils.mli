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

(** Extra functions over strings *)

val start_with : prefix:string -> string -> bool
(** [start_with ~prefix str] check if [str] starts with [prefix] *)

val replace_chars : (char -> string) -> string -> string
(** [replace_chars f s] creates a new string where all characters [c] from [s]
    have been replaced by the result of [f c]
*)

val reverse : string -> string
(** [reverse s] creates a new reversed version of [s] *)

val filter : (char -> bool) -> string -> string
(** [filter p s] creates a copy of [s] containing the characters of [s]
    such that [p c = true],
*)

val fold : ('a -> char -> 'a) -> 'a -> string -> 'a
(** [fold f acc s] computes (f .... (f (f acc s.[1]) s.[2]) .... s.[n]) where
    [n] is String.length s - 1
 **)

val for_all : (char -> bool) -> string -> bool
val exists : (char -> bool) -> string -> bool

val lfindi : string -> (char -> bool) -> int option
(** [lfindi s p] retrieves the first index of a character verifying predicate
    [p]. [None] otherwise
*)

val remove_char : char -> string -> string
(** [remove_char c s] creates a copy of [s] without any occurrence of [c] *)

val remove_newline : string -> string
(** [remove_newline s] creates a copy of [s] without the newline character '\n'
*)

val lchop : int -> string -> string
(** [lchop n s] removes the first [n] characters of string [s].
    Does nothing if the [s] is empty.
    Returns the empty string if [n] >= [String.length s]
    [n] must be positive.
*)

val left : int -> string -> string
(** [left n s] returns the [n] leftmost characters of [s].
    [n] must be positive and smaller than the length of [s].
*)

val right : int -> string -> string
(** [right n s] returns the [n] rightmost characters of [s].
    [n] must be positive and smaller than the length of [s].
*)

val size_of_hexstring : string -> int
(** [size_of_hexstring s] computes the size in bits of hexadecimal string [s].
    Unsafe function: it does not fully check that [s] is a valid hexstring.
*)

val contains : string -> string -> bool
(** [contains subs s] return [true] if [subs] is a substring of [s]
*)

val split : sep:string -> string -> string list
(** [split ~sep s] splits the string [s] into substrings, taking the string
    [sep] as delimiter and returns the list of substrings.
*)

val cli_split : string -> string list
(** [cli_split s] is [split ~sep:"," s] *)

val char_codes : string -> int array

(** {3 Character functions}*)

val is_char_printable : char -> bool
(** [is_char_printable c] returns [true] if [c] is a terminal-printable ASCII
    character
*)

val is_hex_char : char -> bool
(** [is_hex_char c] returns [true] if [c] is a character representing a hex
    number, i.e., 'a'-'z', 'A'-'Z', '0'-'9'.  *)

val to_hex : string -> string
(** [to_hex s] return the hexadecimal representation of stored bytes *)

val pp_hex : Format.formatter -> string -> unit
(** [pp_hex s] print the hexadecimal representation of stored bytes *)
