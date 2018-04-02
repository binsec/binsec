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

(** Basic stream reader *)
type bytestream

type t

(* {7 Constructors }*)
val of_img : ?cursor:int -> Loader.Img.t -> t

val of_nibbles : ?cursor:int -> ?base:int -> string -> t
val of_bytes : ?cursor:int -> ?base:int -> string -> t

val of_binstream : ?cursor:int -> ?base:int -> Basic_types.Binstream.t -> t

(* {7 Pretty-printer} *)
val pp : Format.formatter -> t -> unit

(* {7 Generic manipulation functions} *)
val set_virtual_cursor : t -> int -> unit
val get_virtual_cursor : t -> int

val rewind : t -> int -> unit
(** [rewind r n] moves back the cursor [n] bytes. *)

val advance : t -> int -> unit
(** [advance r n] moves the cursor the cursor [n] bytes. *)

(** {6 Read functions } *)

module type Accessor = sig
  type t
  val u8  : t -> int
  val u16 : t -> int
  val u32 : t -> int
  val u64 : t -> int
end

module Read : Accessor with type t:=t
(** Accessor functions of module [Read]
    read n=1, 2, 4, or 8 bytes and advance n bytes as well.
*)

module Peek : Accessor with type t:=t
(** [Peek] is like [Read] but does not advance *)

val get_slice : t -> int -> int -> int list
(* [get_slice addr_start addr_end] returns the list of bytes contained in the
   interval [addr_start, addr_end[.
   @assert addr_start < addr_end
*)
