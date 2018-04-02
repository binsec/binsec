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

open Loader_types

module type S =
sig

  module Section :
  sig
    type t
    type header

    val name : t -> string
    val flag : t -> int
    val pos  : t -> int map
    val size : t -> int map

    val header : t -> header
  end

  module Symbol :
  sig
    type t
    type header

    val name  : t -> string
    val value : t -> int

    val header : t -> header
  end

  module Img :
  sig
    type t
    type header

    val arch   : t -> arch
    val entry  : t -> int
    val endian : t -> endian
    val sections : t -> Section.t array
    val symbols  : t -> Symbol.t  array

    val header : t -> header
  end

  val check_magic : Loader_buf.t -> bool

  val load : Loader_buf.t -> Img.t
  val load_file_descr : Unix.file_descr -> Img.t
  val load_file : string -> Img.t

  val read_offset  : Img.t -> int -> int
  val read_address : Img.t -> int -> int

  val write_address : Img.t -> int -> int list -> unit

  module Offset  : Loader_buf.S with type t = Img.t
  module Address : Loader_buf.S with type t = Img.t
end
