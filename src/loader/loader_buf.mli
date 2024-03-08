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

(** Generic representation of loader buffers *)

(** Thanks to def-lkb for the general idea: https://github.com/def-lkb/owee *)

open Loader_types

exception Invalid_format of string
(** Minimal support for error reporting. *)

val invalid_format : string -> 'a
val assert_format : bool -> string -> unit

module type S = sig
  type t

  val dim : t -> int
  (** Size of the buffer. *)

  type cursor = private {
    buffer : t;
    endian : Machine.endianness;
    mutable position : int;
  }
  (** A mutable cursor, pointing to an arbitrary position of a buffer. *)

  val cursor : ?at:int -> Machine.endianness -> t -> cursor
  val seek : cursor -> int -> unit
  val ensure : cursor -> int -> string -> unit
  val advance : cursor -> int -> unit
  val at_end : cursor -> bool

  module Peek : sig
    val u8 : cursor -> u8
    val u16 : cursor -> u16
    val u32 : cursor -> u32
    val u64 : cursor -> u64
    val s8 : cursor -> s8
    val s16 : cursor -> s16
    val s32 : cursor -> s32
    val s64 : cursor -> s64
    val uleb128 : cursor -> u64
    val sleb128 : cursor -> s64

    val bytes : cursor -> int -> string
    (** [bytes t len] peeks a string of exactly [len] bytes from [t] *)

    val fixed_string : cursor -> int -> string
    (** [fixed_string t len] peeks a string of maximum [len] bytes from [t] *)

    val zero_string : string -> cursor -> ?maxlen:int -> unit -> string
    (** [zero_string msg t ?maxlen ()] peeks a zero-terminated string from [t],
      * stopping at the first zero or when [maxlen] is reached, if it was
      * provided. *)
  end

  module Read : sig
    val u8 : cursor -> u8
    val u16 : cursor -> u16
    val u32 : cursor -> u32
    val u64 : cursor -> u64
    val s8 : cursor -> s8
    val s16 : cursor -> s16
    val s32 : cursor -> s32
    val s64 : cursor -> s64
    val uleb128 : cursor -> u64
    val sleb128 : cursor -> s64

    val bytes : cursor -> int -> string
    (** [bytes t len] reads a string of exactly [len] bytes from [t] *)

    val fixed_string : cursor -> int -> string
    (** [fixed_string t len] reads a string of maximum [len] bytes from [t],
        then advance to exactly [len] bytes. *)

    val zero_string : string -> cursor -> ?maxlen:int -> unit -> string
    (** [zero_string msg t ?maxlen ()] reads a zero-terminated string from [t],
      * stopping at the first zero or when [maxlen] is reached, if it was
      * provided. *)
  end
end

module type Bufferable = sig
  type t

  val get : t -> int -> int
  val dim : t -> int
end

module Make (B : Bufferable) : S with type t = B.t

module type W = sig
  include S

  module Write : sig
    val u8 : cursor -> u8 -> unit
    val u16 : cursor -> u16 -> unit
    val u32 : cursor -> u32 -> unit
    val u64 : cursor -> u64 -> unit
    val s8 : cursor -> s8 -> unit
    val s16 : cursor -> s16 -> unit
    val s32 : cursor -> s32 -> unit
    val s64 : cursor -> s64 -> unit
  end
end

module type Writable = sig
  include Bufferable

  val set : t -> int -> u8 -> unit
end

module Wake (W : Writable) : W with type t = W.t

include
  W
    with type t =
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val sub : cursor -> int -> cursor
(** [sub t len] returns a fresh cursor pointing to the beginning of a sub-buffer
  * of size [len] starting from [t], and advances [t] by [len]. *)

val read : ?signed:bool -> [ `x32 | `x64 ] -> cursor -> int
