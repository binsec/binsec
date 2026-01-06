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

(** Basic stream reader *)

open Basic_types.Integers

type nonrec uint8 = uint8
type nonrec uint16 = uint16
type nonrec uint32 = uint32
type nonrec uint64 = uint64
type nonrec int8 = int8
type nonrec int16 = int16
type nonrec int32 = int32
type nonrec int64 = int64
type endianness = Basic_types.endianness = LittleEndian | BigEndian
type 'a t

(** {2 Constructors} *)

val create :
  offset:('a -> int -> 'a) ->
  get:('b -> 'a -> char) ->
  ?endianness:endianness ->
  start:'a ->
  ?pos:'a ->
  stop:'a ->
  'b ->
  'a t

val of_bigarray :
  ?pos:int ->
  ?endianness:endianness ->
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int t

val of_zero_extend_bigarray :
  ?pos:int ->
  ?endianness:endianness ->
  dim:int ->
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int t

val of_nibbles : ?pos:int -> ?endianness:endianness -> string -> int t
val of_bytes : ?pos:int -> ?endianness:endianness -> string -> int t
val of_binstream : ?pos:int -> ?endianness:endianness -> Binstream.t -> int t

val rebase :
  offset:('a -> int -> 'a) -> distance:('a -> 'a -> int) -> 'a -> int t -> 'a t

val sub : 'a t -> int -> int t

(** {2 Generic manipulation functions} *)

val get_pos : int t -> int
(** [get_pos r] gets the delta between the start and the current position. *)

val set_pos : 'a t -> int -> unit
(** [set_pos r n] moves the cursor to the [n]th position from the start. *)

val move : 'a t -> 'a -> unit
(** [move r p] moves the cursor to position [p]. *)

val advance : 'a t -> int -> unit
(** [advance r n] moves the cursor by [n] bytes. *)

val rewind : 'a t -> int -> unit
(** [rewind r n] moves back the cursor by [n] bytes. *)

val get_endianness : 'a t -> endianness
(** [get_endianness r] gets the current reader endianness r *)

val set_endianness : 'a t -> endianness -> unit
(** [set_endianness e r] sets reader to report value w.r.t to endianness r *)

val dim : int t -> int
val at_end : 'a t -> bool
val ensure : 'a t -> int -> bool

(** {2 Read functions } *)

module type ACCESS = sig
  val u8 : 'a t -> uint8
  val u16 : 'a t -> uint16
  val u32 : 'a t -> uint32
  val u64 : 'a t -> uint64
  val i8 : 'a t -> int8
  val i16 : 'a t -> int16
  val i32 : 'a t -> int32
  val i64 : 'a t -> int64
  val uleb128 : 'a t -> Z.t
  val sleb128 : 'a t -> Z.t
  val bv8 : 'a t -> Bitvector.t
  val bv16 : 'a t -> Bitvector.t
  val bv32 : 'a t -> Bitvector.t
  val bv64 : 'a t -> Bitvector.t
  val read : 'a t -> int -> Bitvector.t

  val bytes : 'a t -> int -> string
  (** [bytes t len] gets a string of exactly [len] bytes from [t] *)

  val fixed_string : 'a t -> int -> string
  (** [fixed_string t len] gets a string of maximum [len] bytes from [t] *)

  val zero_string : string -> 'a t -> ?maxlen:int -> unit -> string
  (** [zero_string msg t ?maxlen ()] gets a zero-terminated string from [t],
      stopping at the first zero or when [maxlen] is reached, if it was
      provided. *)

  val sub : 'a t -> int -> 'a t
  (** [sub t len] returns a fresh cursor pointing to the beginning of a sub-buffer
      of size [len] starting from [t], and advances [t] by [len]. *)
end

module Peek : ACCESS
module Read : ACCESS
