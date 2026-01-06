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

open Loader_types

module type S = sig
  module Section : sig
    type t
    type header

    val name : t -> string
    val pos : t -> (int, Virtual_address.t) map
    val size : t -> (int, Z.t) map
    val header : t -> header
    val has_flag : section_flag -> t -> bool
  end

  module Symbol : sig
    type t
    type header

    val name : t -> string
    val value : t -> Virtual_address.t
    val header : t -> header
  end

  module Img : sig
    type t
    type header

    val arch : t -> Machine.t
    val entry : t -> Virtual_address.t
    val sections : t -> Section.t array
    val symbols : t -> Symbol.t array
    val header : t -> header
    val cursor : ?at:int -> t -> int Reader.t
    val content : t -> Section.t -> buffer
    val buffer : t -> buffer

    include Sigs.PRINTABLE with type t := t
  end

  val check_magic : buffer -> bool
  val load : buffer -> Img.t
  val load_file_descr : Unix.file_descr -> Img.t
  val load_file : string -> Img.t
  val read_offset : Img.t -> int -> u8
  val read_address : Img.t -> Virtual_address.t -> u8
end
