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

open Loader_types

module Section = struct
  type t = Loader_buf.t

  let has_flag f _ = match f with Read | Exec -> true | Write -> false

  type header = unit

  let header _ = ()

  let size t =
    let raw = Bigarray.Array1.dim t in
    { raw; virt = raw }

  let pos _ = { raw = 0; virt = 0 }
  let flag _ = 0b110
  let name _ = ".raw"
end

module Symbol = struct
  type t = unit
  type header = unit

  let name _ = assert false
  let value _ = assert false
  let header _ = assert false
end

module Img = struct
  type header = unit
  type t = { content : Section.t; sections : Section.t array; arch : Machine.t }

  let sections { sections; _ } = sections
  let symbols _ = [||]
  let arch t = t.arch
  let header _ = ()
  let entry _ = 0

  let cursor ?(at = 0) t =
    Loader_buf.cursor ~at Machine.LittleEndian (Array.get t.sections 0)

  let content _ buf = buf
  let pp ppf _ = Format.pp_print_string ppf "Raw image"
end

let check_magic _ = assert false

let read_address img i =
  try Bigarray.Array1.get img.Img.content i
  with Invalid_argument _ -> raise Not_found

let read_offset = read_address

let load content =
  {
    Img.arch = Kernel_options.Machine.get ();
    content;
    sections = [| content |];
  }

let load_file_descr file_descr =
  load
    Bigarray.(
      array1_of_genarray
        (Unix.map_file file_descr Int8_unsigned C_layout false [| -1 |]))

let load_file path =
  let file_descr = Unix.openfile path [ Unix.O_RDONLY ] 0 in
  let img = load_file_descr file_descr in
  Unix.close file_descr;
  img

module Offset = Loader_buf.Make (struct
  type t = Img.t

  let get t i = read_address t i
  let dim t = Bigarray.Array1.dim t.Img.content
end)

module Address = Offset
