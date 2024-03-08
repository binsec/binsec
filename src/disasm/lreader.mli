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

(** Basic stream reader *)

type byte = int
type t

(** {7 Constructors} *)

val create :
  ?endianness:Machine.endianness -> ?at:int -> ('a -> int -> byte) -> 'a -> t

val of_img : ?endianness:Machine.endianness -> ?at:int -> Loader.Img.t -> t

val of_zero_extend_buffer :
  ?endianness:Machine.endianness -> ?at:int -> Loader_buf.t -> t

val of_nibbles : ?endianness:Machine.endianness -> ?at:int -> string -> t
val of_bytes : ?endianness:Machine.endianness -> ?at:int -> string -> t
val of_binstream : ?endianness:Machine.endianness -> ?at:int -> Binstream.t -> t

(** {7 Pretty-printer} *)

val pp : Format.formatter -> t -> unit

(** {7 Generic manipulation functions} *)

val get_pos : t -> int

val rewind : t -> int -> unit
(** [rewind r n] moves back the cursor [n] bytes. *)

val advance : t -> int -> unit
(** [advance r n] moves the cursor the cursor [n] bytes. *)

val set_endianness : t -> Machine.endianness -> unit
(** [set_endianness e r] sets reader to report value w.r.t to endianness r *)

(** {6 Read functions } *)

module type Accessor = sig
  type t

  (* read an unsigned int *)
  val u8 : t -> int

  (* read a signed int *)
  val i8 : t -> int
  val u16 : t -> int
  val i16 : t -> int
  val i32 : t -> int32
  val i64 : t -> int64
  val bv8 : t -> Bitvector.t
  val bv16 : t -> Bitvector.t
  val bv32 : t -> Bitvector.t
  val bv64 : t -> Bitvector.t
  val read : t -> int -> Bitvector.t
end

module Read : Accessor with type t := t
(** Accessor functions of module [Read]
    read n=1, 2, 4, or 8 bytes and advance n bytes as well.
*)

module Peek : Accessor with type t := t
(** [Peek] is like [Read] but does not advance *)

val get_slice : t -> lo:int -> hi:int -> bytes
(** [get_slice t ~lo ~hi] returns the  bytes contained in the
    interval from \[lo, hi\].
    @raise Invalid_argument if lo > hi
*)
