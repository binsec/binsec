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

(** Representation of binary streams *)

(** {2 Types} *)


type t

(** {3 Constructors} *)

val empty : t


val of_nibbles : string -> t
(** [of_nibbles s] converts a string [s] of hexadecimal characters.

    @assumes each character is in the range [0-9a-f].
*)

val of_bytes : string -> t
(** [of_bytes s] converts a byte stream [s].
    Each character stands for its own byte.
*)

val of_list : int list -> t
(** [of_list l] converts a list of integers.

    @assumes: each integer n is in the range 0 <= n <= 255.
*)

(** {3 Operations} *)

val append_int    : int -> t -> t
val prepend_int   : int -> t -> t

val append_int64  : int64 -> t -> t
val prepend_int64 : int64 -> t -> t

val append_char : char -> t -> t
val prepend_char : char -> t -> t

val rev: t -> t

(** {3 Iterators }*)

val iter : (int -> unit) -> t -> unit
val map  : (int -> int) -> t -> t
val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a

(** {3 Accessors} *)

val length : t -> int

val get_byte : t -> int -> int option

val get_byte_exn : t -> int -> int
(** [get_byte b n] retrieves byte number [n] from [b]

    The byte sequence is 0-indexed.

    @raise Invalid_argument if the index is not between [0] and [length b - 1]
*)


val to_string : t -> string

(** {3 Printers} *)

include Sigs.PRINTABLE with type t := t
