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

open Basic_types.Integers
open Loader_types

module Section = struct
  type t = buffer

  let has_flag f _ = match f with Read | Exec -> true | Write -> false

  type header = unit

  let header _ = ()

  let size t =
    let raw = Bigarray.Array1.dim t in
    { raw; virt = Z.of_int raw }

  let pos _ = { raw = 0; virt = Virtual_address.create 0 }
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
  let entry _ = Virtual_address.create 0
  let cursor ?at t = Reader.of_bigarray ?pos:at (Array.get t.sections 0)
  let content _ buf = buf
  let buffer { content; _ } = content
  let pp ppf _ = Format.pp_print_string ppf "Raw image"
end

let check_magic _ = assert false

let read_offset img i =
  try Int.unsafe_to_uint8 (Bigarray.Array1.get img.Img.content i)
  with Invalid_argument _ -> raise Not_found

let read_address img a = read_offset img (Virtual_address.to_int a)

let load content =
  { Img.arch = Machine.unknown; content; sections = [| content |] }

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
