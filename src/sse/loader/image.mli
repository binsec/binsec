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

type buffer =
  | Zero
  | Data of { offset : int; len : int; value : Loader_types.buffer }

val crop_buffer : lo:Z.t -> hi:Z.t -> buffer -> buffer
(** [crop_buffer ~lo ~hi buf] creates a new buffer view containing
    the [buf] bytes from [lo] to [hi]. *)

val content_reader :
  Virtual_address.t ->
  Z.t ->
  ?endianness:Machine.endianness ->
  buffer Zmap.t ->
  Virtual_address.t Reader.t
(** [content_reader addr size ~endianness content] returns a new
    {!module-Reader} of [size] bytes of [content] starting from [addr].

    Sequential accesses are optimized with a cache.

    @raise Not_found if the reader tries to access a non-mapped address. *)

type protection = R | RW | RX | RWX
type symbol = { base : Virtual_address.t; name : string; origin : string }

type section = {
  base : Virtual_address.t;
  name : string;
  origin : string;
  symbols : symbol Zmap.t;
}

type t = private {
  content : buffer Zmap.t;  (** set of initialized data *)
  protection : protection Zmap.t;  (** set of mappings *)
  symbols :
    (Z.t * string) list Dba.Var.Tag.Attribute.Map.t Basic_types.String.Map.t;
      (** set of symbol attributes*)
  layout : section Zmap.t;  (** reverse memory paving *)
}

val load : fs:(string -> Loader_types.buffer) -> string -> Loader.Img.t -> t
(** [load ~fs filename img] builds a process image for the file [filename]
    with the content, permission and symbols from the loader image [img].

    It uses the virtual file system [fs] to access extra file contents
    (e.g. for separated debug or coredump). *)

val layout_with_cache :
  t -> (Virtual_address.t -> section) * (Virtual_address.t -> symbol)
(** [layout_with_cache image] returns two mapping functions.
    The first one maps an address to a {!type-section} while
    the second maps an address to the {!type-symbol} it belongs.

    Sequential accesses are optimized with a cache.

    @raise Not_found if the address is not mapped. *)
