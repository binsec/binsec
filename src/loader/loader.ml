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

open Loader_buf

type ('a,'b) t_pack = ELF of 'a | PE of 'b
type ('a,'b) header_pack = ELF_header of 'a | PE_header of 'b

module Section =
struct

  type t = (Loader_elf.Section.t,Loader_pe.Section.t) t_pack
  type header = (Loader_elf.Section.header,Loader_pe.Section.header) header_pack

  let name = function
    | ELF elf -> Loader_elf.Section.name elf
    | PE pe -> Loader_pe.Section.name pe

  let flag = function
    | ELF elf -> Loader_elf.Section.flag elf
    | PE pe -> Loader_pe.Section.flag pe

  let pos = function
    | ELF elf -> Loader_elf.Section.pos elf
    | PE pe -> Loader_pe.Section.pos pe

  let size = function
    | ELF elf -> Loader_elf.Section.size elf
    | PE pe -> Loader_pe.Section.size pe

  let header = function
    | ELF elf -> ELF_header (Loader_elf.Section.header elf)
    | PE pe -> PE_header (Loader_pe.Section.header pe)

end

module Symbol =
struct

  type t = (Loader_elf.Symbol.t,Loader_pe.Symbol.t) t_pack
  type header = (Loader_elf.Symbol.header,Loader_pe.Symbol.header) header_pack

  let name = function
    | ELF elf -> Loader_elf.Symbol.name elf
    | PE pe -> Loader_pe.Symbol.name pe

  let value = function
    | ELF elf -> Loader_elf.Symbol.value elf
    | PE pe -> Loader_pe.Symbol.value pe

  let header = function
    | ELF elf -> ELF_header (Loader_elf.Symbol.header elf)
    | PE pe -> PE_header (Loader_pe.Symbol.header pe)

end

module Img =
struct

  type t = (Loader_elf.Img.t,Loader_pe.Img.t) t_pack
  type header = (Loader_elf.Img.header,Loader_pe.Img.header) header_pack

  let arch = function
    | ELF elf -> Loader_elf.Img.arch elf
    | PE pe -> Loader_pe.Img.arch pe

  let entry = function
    | ELF elf -> Loader_elf.Img.entry elf
    | PE pe -> Loader_pe.Img.entry pe

  let endian = function
    | ELF elf -> Loader_elf.Img.endian elf
    | PE pe -> Loader_pe.Img.endian pe

  let sections = function
    | ELF elf -> Array.map (fun s -> ELF s) (Loader_elf.Img.sections elf)
    | PE pe -> Array.map (fun s -> PE s) (Loader_pe.Img.sections pe)

  let symbols = function
    | ELF elf -> Array.map (fun s -> ELF s) (Loader_elf.Img.symbols elf)
    | PE pe -> Array.map (fun s -> PE s) (Loader_pe.Img.symbols pe)

  let header = function
    | ELF elf -> ELF_header (Loader_elf.Img.header elf)
    | PE pe -> PE_header (Loader_pe.Img.header pe)

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

let read_address img addr =
  match img with
  | ELF elf -> Loader_elf.read_address elf addr
  | PE pe -> Loader_pe.read_address pe addr

let write_address img addr v =
  match img with
  | ELF elf -> Loader_elf.write_address elf addr v
  | PE pe -> Loader_pe.write_address pe addr v


module Offset = Loader_buf.Make
    (struct
      type t = Img.t
      let get t i = read_offset t i
      let dim = function
        | ELF elf -> Loader_elf.Offset.dim elf
        | PE pe -> Loader_pe.Offset.dim pe
    end)

module Address = Loader_buf.Make
    (struct
      type t = Img.t
      let get t i = read_address t i
      let dim = function
        | ELF elf -> Loader_elf.Address.dim elf
        | PE pe -> Loader_pe.Address.dim pe
    end)

