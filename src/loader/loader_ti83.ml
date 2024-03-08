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

open Loader_buf
open Loader_types

type header = { name : string; comment : string }

module Section = struct
  type t = { offset : int; size : int }
  type header = unit

  let name _ = "RAM"
  let flag _ = 0
  let pos { offset; _ } = { virt = 0x9d93; raw = offset }
  let size { size; _ } = { virt = size; raw = size }
  let header _ = ()
  let has_flag _ _ = true
end

module Symbol = struct
  type t = { name : string; value : int }
  type header = unit

  let name { name; _ } = name
  let value { value; _ } = value
  let header _ = ()
end

module Img = struct
  type t = { header : header; ram : Section.t; buf : Loader_buf.t }
  type nonrec header = header

  let arch _ = Machine.z80
  let entry _ = 0x9d95
  let sections { ram; _ } = [| ram |]
  let symbols _ = [||]
  let header { header; _ } = header

  let cursor ?(at = 0) { buf; _ } =
    Loader_buf.cursor ~at Machine.LittleEndian buf

  let content { buf; _ } Section.{ offset; size; _ } =
    Bigarray.Array1.sub buf offset size

  let pp ppf { header = { name; comment }; _ } =
    Format.fprintf ppf "@[<v>TI-83 Plus compiled assembly program %s@ %S@]" name
      comment
end

let check_magic buf =
  (not (dim buf < 11))
  && buf.{0} = Char.code '*'
  && buf.{1} = Char.code '*'
  && buf.{2} = Char.code 'T'
  && buf.{3} = Char.code 'I'
  && buf.{4} = Char.code '8'
  && buf.{5} = Char.code '3'
  && buf.{6} = Char.code 'F'
  && buf.{7} = Char.code '*'
  && buf.{8} = 0x1a
  && buf.{9} = 0x0a
  && buf.{10} = 0x00

let load buf =
  let cursor = Loader_buf.cursor ~at:0x0b Machine.LittleEndian buf in
  let comment = Loader_buf.Peek.zero_string "" cursor ~maxlen:42 () in
  Loader_buf.advance cursor 0x2a;
  ignore (Loader_buf.Read.u16 cursor);
  if Loader_buf.Read.u16 cursor <> 0x0d then invalid_arg "Invalid format";
  ignore (Loader_buf.Read.u16 cursor);
  if Loader_buf.Read.u8 cursor <> 0x06 then invalid_arg "Invalid format";
  let name = Loader_buf.Peek.zero_string "" cursor ~maxlen:10 () in
  Loader_buf.advance cursor 0x0a;
  ignore (Loader_buf.Read.u16 cursor);
  let size = Loader_buf.Read.u16 cursor in
  let offset = cursor.position in
  if Loader_buf.Peek.u8 cursor <> 0xbb then invalid_arg "Invalid format";
  Img.{ header = { name; comment }; ram = { offset; size }; buf }

let load_file_descr file_descr =
  let buffer =
    Bigarray.(
      array1_of_genarray
        (Unix.map_file file_descr Int8_unsigned C_layout false [| -1 |]))
  in
  load buffer

let load_file path =
  let file_descr = Unix.openfile path [ Unix.O_RDONLY ] 0 in
  let img = load_file_descr file_descr in
  Unix.close file_descr;
  img

let read_offset Img.{ buf; _ } offset = buf.{offset}

let read_address Img.{ ram = { offset; size; _ }; buf; _ } addr =
  if addr < 0x9d93 || 0x9d93 + size <= addr then
    invalid_arg (Format.sprintf "Unreachable virtual address %04x" addr)
  else buf.{addr - 0x9d93 + offset}

module Offset = Loader_buf.Make (struct
  type t = Img.t

  let get t i = read_offset t i
  let dim i = Bigarray.Array1.dim i.Img.buf
end)

module Address = Loader_buf.Make (struct
  type t = Img.t

  let get t i = read_address t i
  let dim _ = max_int
end)
