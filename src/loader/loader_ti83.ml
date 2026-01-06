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

type header = { name : string; comment : string }

module Section = struct
  type t = { offset : int; size : int }
  type header = unit

  let name _ = "RAM"
  let pos { offset; _ } = { virt = Virtual_address.create 0x9d93; raw = offset }
  let size { size; _ } = { virt = Z.of_int size; raw = size }
  let header _ = ()
  let has_flag _ _ = true
end

module Symbol = struct
  type t = unit
  type header = unit

  let name _ = assert false
  let value _ = assert false
  let header _ = ()
end

module Img = struct
  type t = { header : header; ram : Section.t; buf : buffer }
  type nonrec header = header

  let arch _ = Machine.z80
  let entry _ = Virtual_address.create 0x9d95
  let sections { ram; _ } = [| ram |]
  let symbols _ = [||]
  let header { header; _ } = header
  let cursor ?at { buf; _ } = Reader.of_bigarray ?pos:at buf

  let content { buf; _ } Section.{ offset; size; _ } =
    Bigarray.Array1.sub buf offset size

  let buffer { buf; _ } = buf

  let pp ppf { header = { name; comment }; _ } =
    Format.fprintf ppf "@[<v>TI-83 Plus compiled assembly program %s@ %S@]" name
      comment
end

let check_magic buf =
  (not (Bigarray.Array1.dim buf < 11))
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
  let cursor = Reader.of_bigarray ~pos:0x0b buf in
  let comment = Reader.Peek.zero_string "" cursor ~maxlen:42 () in
  Reader.advance cursor 0x2a;
  ignore (Reader.Read.u16 cursor);
  if Uint16.to_int (Reader.Read.u16 cursor) <> 0x0d then
    invalid_arg "Invalid format";
  ignore (Reader.Read.u16 cursor);
  if Uint8.to_int (Reader.Read.u8 cursor) <> 0x06 then
    invalid_arg "Invalid format";
  let name = Reader.Peek.zero_string "" cursor ~maxlen:10 () in
  Reader.advance cursor 0x0a;
  ignore (Reader.Read.u16 cursor);
  let size = Uint16.to_int (Reader.Read.u16 cursor) in
  let offset = Reader.get_pos cursor in
  if Uint8.to_int (Reader.Peek.u8 cursor) <> 0xbb then
    invalid_arg "Invalid format";
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

let read_offset Img.{ buf; _ } offset = Int.unsafe_to_uint8 buf.{offset}

let read_address Img.{ ram = { offset; size; _ }; buf; _ } addr =
  let addr = Virtual_address.to_int addr in
  if addr < 0x9d93 || 0x9d93 + size <= addr then
    invalid_arg (Format.sprintf "Unreachable virtual address %04x" addr)
  else Int.unsafe_to_uint8 buf.{addr - 0x9d93 + offset}
