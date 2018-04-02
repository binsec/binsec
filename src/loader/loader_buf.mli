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

(** Thanks to def-lkb for the general idea: https://github.com/def-lkb/owee *)

open Loader_types

(** Minimal support for error reporting. *)
exception Invalid_format of string
val invalid_format : string -> 'a
val assert_format : bool -> string -> unit

module type S =
sig
  type t

  (** Size of the buffer. *)
  val dim : t -> int

  (** A mutable cursor, pointing to an arbitrary position of a buffer. *)
  type cursor = private {
    buffer: t;
    endian: endian;
    mutable position: int;
  }

  val cursor  : ?at:int -> endian -> t -> cursor
  val seek    : cursor -> int -> unit
  val ensure  : cursor -> int -> string -> unit
  val advance : cursor -> int -> unit
  val at_end  : cursor -> bool

  module Peek : sig
    val u8  : cursor -> u8
    val u16 : cursor -> u16
    val u32 : cursor -> u32
    val u64 : cursor -> u64

    (** [fixed_string t len] peeks a string of exactly [len] bytes from [t] *)
    val fixed_string : cursor -> int -> string

    (** [zero_string msg t ?maxlen ()] peeks a zero-terminated string from [t],
      * stopping at the first zero or when [maxlen] is reached, if it was
      * provided. *)
    val zero_string : string -> cursor -> ?maxlen:int -> unit -> string
  end

  module Read : sig
    val u8  : cursor -> u8
    val u16 : cursor -> u16
    val u32 : cursor -> u32
    val u64 : cursor -> u64

    (** [fixed_string t len] reads a string of exactly [len] bytes from [t] *)
    val fixed_string : cursor -> int -> string

    (** [zero_string msg t ?maxlen ()] reads a zero-terminated string from [t],
      * stopping at the first zero or when [maxlen] is reached, if it was
      * provided. *)
    val zero_string : string -> cursor -> ?maxlen:int -> unit -> string
  end
end

module type Bufferable =
sig
  type t

  val get : t -> int -> int
  val dim : t -> int
end

module Make (B: Bufferable) : S with type t = B.t

include S with type t = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** [sub t len] returns a fresh cursor pointing to the beginning of a sub-buffer
  * of size [len] starting from [t], and advances [t] by [len]. *)
val sub : cursor -> int -> cursor

