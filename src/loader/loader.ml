(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2022                                               *)
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

type ('a, 'b, 'c, 'd) t_pack = ELF of 'a | PE of 'b | Dump of 'c | TI83 of 'd

module Section = struct
  type t =
    ( Loader_elf.Section.t,
      Loader_pe.Section.t,
      Loader_dump.Section.t,
      Loader_ti83.Section.t )
    t_pack

  type header =
    ( Loader_elf.Section.header,
      Loader_pe.Section.header,
      Loader_dump.Section.header,
      Loader_ti83.Section.header )
    t_pack

  let name = function
    | ELF elf -> Loader_elf.Section.name elf
    | PE pe -> Loader_pe.Section.name pe
    | Dump d -> Loader_dump.Section.name d
    | TI83 ti -> Loader_ti83.Section.name ti

  let flag = function
    | ELF elf -> Loader_elf.Section.flag elf
    | PE pe -> Loader_pe.Section.flag pe
    | Dump d -> Loader_dump.Section.flag d
    | TI83 ti -> Loader_ti83.Section.flag ti

  let pos = function
    | ELF elf -> Loader_elf.Section.pos elf
    | PE pe -> Loader_pe.Section.pos pe
    | Dump d -> Loader_dump.Section.pos d
    | TI83 ti -> Loader_ti83.Section.pos ti

  let size = function
    | ELF elf -> Loader_elf.Section.size elf
    | PE pe -> Loader_pe.Section.size pe
    | Dump d -> Loader_dump.Section.size d
    | TI83 ti -> Loader_ti83.Section.size ti

  let header = function
    | ELF elf -> ELF (Loader_elf.Section.header elf)
    | PE pe -> PE (Loader_pe.Section.header pe)
    | Dump d -> Dump (Loader_dump.Section.header d)
    | TI83 ti -> TI83 (Loader_ti83.Section.header ti)

  let has_flag f = function
    | ELF elf -> Loader_elf.Section.has_flag f elf
    | PE pe -> Loader_pe.Section.has_flag f pe
    | Dump d -> Loader_dump.Section.has_flag f d
    | TI83 ti -> Loader_ti83.Section.has_flag f ti
end

module Symbol = struct
  type t =
    ( Loader_elf.Symbol.t,
      Loader_pe.Symbol.t,
      Loader_dump.Symbol.t,
      Loader_ti83.Symbol.t )
    t_pack

  type header =
    ( Loader_elf.Symbol.header,
      Loader_pe.Symbol.header,
      Loader_dump.Symbol.header,
      Loader_ti83.Symbol.header )
    t_pack

  let name = function
    | ELF elf -> Loader_elf.Symbol.name elf
    | PE pe -> Loader_pe.Symbol.name pe
    | Dump d -> Loader_dump.Symbol.name d
    | TI83 ti -> Loader_ti83.Symbol.name ti

  let value = function
    | ELF elf -> Loader_elf.Symbol.value elf
    | PE pe -> Loader_pe.Symbol.value pe
    | Dump d -> Loader_dump.Symbol.value d
    | TI83 ti -> Loader_ti83.Symbol.value ti

  let header = function
    | ELF elf -> ELF (Loader_elf.Symbol.header elf)
    | PE pe -> PE (Loader_pe.Symbol.header pe)
    | Dump d -> Dump (Loader_dump.Symbol.header d)
    | TI83 ti -> TI83 (Loader_ti83.Symbol.header ti)
end

module Img = struct
  type t =
    ( Loader_elf.Img.t,
      Loader_pe.Img.t,
      Loader_dump.Img.t,
      Loader_ti83.Img.t )
    t_pack

  type header =
    ( Loader_elf.Img.header,
      Loader_pe.Img.header,
      Loader_dump.Img.header,
      Loader_ti83.Img.header )
    t_pack

  let arch = function
    | ELF elf -> Loader_elf.Img.arch elf
    | PE pe -> Loader_pe.Img.arch pe
    | Dump dump -> Loader_dump.Img.arch dump
    | TI83 ti -> Loader_ti83.Img.arch ti

  let entry = function
    | ELF elf -> Loader_elf.Img.entry elf
    | PE pe -> Loader_pe.Img.entry pe
    | Dump dump -> Loader_dump.Img.entry dump
    | TI83 ti -> Loader_ti83.Img.entry ti

  let sections = function
    | ELF elf -> Array.map (fun s -> ELF s) (Loader_elf.Img.sections elf)
    | PE pe -> Array.map (fun s -> PE s) (Loader_pe.Img.sections pe)
    | Dump dump -> Array.map (fun s -> Dump s) (Loader_dump.Img.sections dump)
    | TI83 ti -> Array.map (fun s -> TI83 s) (Loader_ti83.Img.sections ti)

  let symbols = function
    | ELF elf -> Array.map (fun s -> ELF s) (Loader_elf.Img.symbols elf)
    | PE pe -> Array.map (fun s -> PE s) (Loader_pe.Img.symbols pe)
    | Dump dump -> Array.map (fun s -> Dump s) (Loader_dump.Img.symbols dump)
    | TI83 ti -> Array.map (fun s -> TI83 s) (Loader_ti83.Img.symbols ti)

  let header = function
    | ELF elf -> ELF (Loader_elf.Img.header elf)
    | PE pe -> PE (Loader_pe.Img.header pe)
    | Dump dump -> Dump (Loader_dump.Img.header dump)
    | TI83 ti -> TI83 (Loader_ti83.Img.header ti)

  let cursor ?at = function
    | ELF elf -> Loader_elf.Img.cursor ?at elf
    | PE pe -> Loader_pe.Img.cursor ?at pe
    | Dump dump -> Loader_dump.Img.cursor ?at dump
    | TI83 ti -> Loader_ti83.Img.cursor ?at ti

  let content t s =
    match (t, s) with
    | ELF t, ELF s -> Loader_elf.Img.content t s
    | PE t, PE s -> Loader_pe.Img.content t s
    | Dump t, Dump s -> Loader_dump.Img.content t s
    | TI83 t, TI83 s -> Loader_ti83.Img.content t s
    | _ -> assert false

  let pp ppf = function
    | ELF elf -> Loader_elf.Img.pp ppf elf
    | PE pe -> Loader_pe.Img.pp ppf pe
    | Dump dump -> Loader_dump.Img.pp ppf dump
    | TI83 ti -> Loader_ti83.Img.pp ppf ti
end

let check_magic t =
  Loader_elf.check_magic t || Loader_pe.check_magic t
  || Loader_ti83.check_magic t

let load buffer =
  if Loader_elf.check_magic buffer then ELF (Loader_elf.load buffer)
  else if Loader_pe.check_magic buffer then PE (Loader_pe.load buffer)
  else if Loader_ti83.check_magic buffer then TI83 (Loader_ti83.load buffer)
  else invalid_format "Unknown image file"

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

let read_offset img offset =
  match img with
  | ELF elf -> Loader_elf.read_offset elf offset
  | PE pe -> Loader_pe.read_offset pe offset
  | Dump d -> Loader_dump.read_offset d offset
  | TI83 ti -> Loader_ti83.read_offset ti offset

let read_address img addr =
  match img with
  | ELF elf -> Loader_elf.read_address elf addr
  | PE pe -> Loader_pe.read_address pe addr
  | Dump d -> Loader_dump.read_address d addr
  | TI83 ti -> Loader_ti83.read_address ti addr

module Offset = Loader_buf.Make (struct
  type t = Img.t

  let get t i = read_offset t i

  let dim = function
    | ELF elf -> Loader_elf.Offset.dim elf
    | PE pe -> Loader_pe.Offset.dim pe
    | Dump d -> Loader_dump.Offset.dim d
    | TI83 ti -> Loader_ti83.Offset.dim ti
end)

module Address = Loader_buf.Make (struct
  type t = Img.t

  let get t i = read_address t i

  let dim = function
    | ELF elf -> Loader_elf.Address.dim elf
    | PE pe -> Loader_pe.Address.dim pe
    | Dump d -> Loader_dump.Offset.dim d
    | TI83 ti -> Loader_ti83.Offset.dim ti
end)

module View = struct
  type t = {
    base : Virtual_address.t;
    size : int;
    buffer :
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
  }

  let create vaddr img section =
    let { Loader_types.virt; _ } = Section.pos section in
    let base = Virtual_address.add_int virt vaddr in
    let { Loader_types.virt = size; _ } = Section.size section in
    let buffer = Img.content img section in
    { base; size; buffer }

  let zero_extend_get buffer offset =
    if Bigarray.Array1.dim buffer < offset then 0
    else Bigarray.Array1.get buffer offset

  let unsafe_get { base; buffer; _ } addr =
    zero_extend_get buffer (Virtual_address.diff addr base)

  let get { base; size; buffer } addr =
    let offset = Virtual_address.diff addr base in
    if offset < 0 || size < offset then raise Not_found
    else zero_extend_get buffer offset
end
