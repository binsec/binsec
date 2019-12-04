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

(** Basic stream reader *)

type bytestream

type t

(** {7 Constructors} *)

val of_img : ?endianness:Machine.endianness -> ?cursor:int -> Loader.Img.t -> t

val of_nibbles : ?endianness:Machine.endianness -> ?cursor:int -> ?base:int -> string -> t

val of_bytes : ?endianness:Machine.endianness -> ?cursor:int -> ?base:int -> string -> t

val of_binstream : ?endianness:Machine.endianness -> ?cursor:int -> ?base:int -> Binstream.t -> t

(** {7 Pretty-printer} *)

val pp : Format.formatter -> t -> unit

(** {7 Generic manipulation functions} *)

val get_virtual_cursor : t -> int

val rewind : t -> int -> unit
(** [rewind r n] moves back the cursor [n] bytes. *)

val advance : t -> int -> unit
(** [advance r n] moves the cursor the cursor [n] bytes. *)

val set_endianness: t -> Machine.endianness -> unit
(** [set_endianness e r] sets reader to report value w.r.t to endianness r *)

(** {6 Read functions } *)

module type Accessor = sig
  type t
  (* read an unsigned int *)
  val u8  : t -> int
  val u16 : t -> int
  val u32 : t -> int
  val u64 : t -> int
  (* read a signed int *)
  val i8  : t -> int
  val i16 : t -> int
  val i32 : t -> int
  (* u64 is incorrect because the sign bit is ignored; no i64 *)
  val bv8: t -> Bitvector.t
  val bv16: t -> Bitvector.t
  val bv32: t -> Bitvector.t
  val bv64: t -> Bitvector.t
end

module Read : Accessor with type t := t
(** Accessor functions of module [Read]
    read n=1, 2, 4, or 8 bytes and advance n bytes as well.
*)

(** [Peek] is like [Read] but does not advance *)
module Peek : sig
  include Accessor with type t:=t

  val peek : t -> int -> Bitvector.t
  (** [peek loader n] peeks at the next [n] bytes of loader [loader] *)
end


val get_slice : t -> int -> int -> int list
(** [get_slice addr_start addr_end] returns the list of bytes contained in the
    interval from [addr_start] included to [addr_end] excluded.
    @assert addr_start < addr_end
*)
