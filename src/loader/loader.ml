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

open Loader_buf

type ('a,'b,'c) t_pack = ELF of 'a | PE of 'b | Dump of 'c
type ('a,'b,'c) header_pack = ELF_header of 'a | PE_header of 'b | Dump_header of 'c

module Section =
struct

  type t = (Loader_elf.Section.t,Loader_pe.Section.t,Loader_dump.Section.t) t_pack
  type header = (Loader_elf.Section.header,Loader_pe.Section.header,Loader_dump.Section.header) header_pack

  let name = function
    | ELF elf -> Loader_elf.Section.name elf
    | PE pe -> Loader_pe.Section.name pe
    | Dump d -> Loader_dump.Section.name d

  let flag = function
    | ELF elf -> Loader_elf.Section.flag elf
    | PE pe -> Loader_pe.Section.flag pe
    | Dump d -> Loader_dump.Section.flag d

  let pos = function
    | ELF elf -> Loader_elf.Section.pos elf
    | PE pe -> Loader_pe.Section.pos pe
    | Dump d -> Loader_dump.Section.pos d

  let size = function
    | ELF elf -> Loader_elf.Section.size elf
    | PE pe -> Loader_pe.Section.size pe
    | Dump d -> Loader_dump.Section.size d

  let header = function
    | ELF elf -> ELF_header (Loader_elf.Section.header elf)
    | PE pe -> PE_header (Loader_pe.Section.header pe)
    | Dump d -> Dump_header (Loader_dump.Section.header d)

  let has_flag f = function
    | ELF elf -> Loader_elf.Section.has_flag f elf
    | PE pe -> Loader_pe.Section.has_flag f pe
    | Dump d -> Loader_dump.Section.has_flag f d

end

module Symbol =
struct

  type t = (Loader_elf.Symbol.t,Loader_pe.Symbol.t,Loader_dump.Symbol.t) t_pack
  type header = (Loader_elf.Symbol.header,Loader_pe.Symbol.header,Loader_dump.Symbol.header) header_pack

  let name = function
    | ELF elf -> Loader_elf.Symbol.name elf
    | PE pe -> Loader_pe.Symbol.name pe
    | Dump d -> Loader_dump.Symbol.name d

  let value = function
    | ELF elf -> Loader_elf.Symbol.value elf
    | PE pe -> Loader_pe.Symbol.value pe
    | Dump d -> Loader_dump.Symbol.value d

  let header = function
    | ELF elf -> ELF_header (Loader_elf.Symbol.header elf)
    | PE pe -> PE_header (Loader_pe.Symbol.header pe)
    | Dump d -> Dump_header (Loader_dump.Symbol.header d)

end

module Img =
struct

  type t = (Loader_elf.Img.t,Loader_pe.Img.t,Loader_dump.Img.t) t_pack
  type header = (Loader_elf.Img.header,Loader_pe.Img.header,Loader_dump.Img.header) header_pack

  let arch = function
    | ELF elf -> Loader_elf.Img.arch elf
    | PE pe -> Loader_pe.Img.arch pe
    | Dump dump -> Loader_dump.Img.arch dump

  let entry = function
    | ELF elf -> Loader_elf.Img.entry elf
    | PE pe -> Loader_pe.Img.entry pe
    | Dump dump -> Loader_dump.Img.entry dump

  let sections = function
    | ELF elf -> Array.map (fun s -> ELF s) (Loader_elf.Img.sections elf)
    | PE pe -> Array.map (fun s -> PE s) (Loader_pe.Img.sections pe)
    | Dump dump -> Array.map (fun s -> Dump s) (Loader_dump.Img.sections dump)


  let symbols = function
    | ELF elf -> Array.map (fun s -> ELF s) (Loader_elf.Img.symbols elf)
    | PE pe -> Array.map (fun s -> PE s) (Loader_pe.Img.symbols pe)
    | Dump dump -> Array.map (fun s -> Dump s) (Loader_dump.Img.symbols dump)

  let header = function
    | ELF elf -> ELF_header (Loader_elf.Img.header elf)
    | PE pe -> PE_header (Loader_pe.Img.header pe)
    | Dump dump -> Dump_header (Loader_dump.Img.header dump)

  let cursor ?at = function
    | ELF elf -> Loader_elf.Img.cursor ?at elf
    | PE pe -> Loader_pe.Img.cursor ?at pe
    | Dump dump -> Loader_dump.Img.cursor ?at dump

  let pp ppf = function
    | ELF elf -> Loader_elf.Img.pp ppf elf
    | PE pe -> Loader_pe.Img.pp ppf pe
    | Dump dump -> Loader_dump.Img.pp ppf dump

end

let check_magic t =
  Loader_elf.check_magic t || Loader_pe.check_magic t

let load buffer =
  if Loader_elf.check_magic buffer then
    ELF (Loader_elf.load buffer)
  else if Loader_pe.check_magic buffer then
    PE (Loader_pe.load buffer)
  else invalid_format "Unknown image file"

let load_file_descr file_descr =
  let buffer =
    Bigarray.(Array1.map_file file_descr Int8_unsigned C_layout false (-1))
  in load buffer

let load_file path =
  let file_descr = Unix.openfile path [Unix.O_RDONLY] 0 in
  let img = load_file_descr file_descr in
  Unix.close file_descr;
  img

let read_offset img offset =
  match img with
  | ELF elf -> Loader_elf.read_offset elf offset
  | PE pe -> Loader_pe.read_offset pe offset
  | Dump d -> Loader_dump.read_offset d offset

let read_address img addr =
  match img with
  | ELF elf -> Loader_elf.read_address elf addr
  | PE pe -> Loader_pe.read_address pe addr
  | Dump d -> Loader_dump.read_address d addr


module Offset = Loader_buf.Make
    (struct
      type t = Img.t
      let get t i = read_offset t i
      let dim = function
        | ELF elf -> Loader_elf.Offset.dim elf
        | PE pe -> Loader_pe.Offset.dim pe
        | Dump d -> Loader_dump.Offset.dim d
    end)

module Address = Loader_buf.Make
    (struct
      type t = Img.t
      let get t i = read_address t i
      let dim = function
        | ELF elf -> Loader_elf.Address.dim elf
        | PE pe -> Loader_pe.Address.dim pe
        | Dump d -> Loader_dump.Offset.dim d
    end)
