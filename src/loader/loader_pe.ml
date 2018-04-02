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
open Loader_types

let read_magic t =
  not (dim t.buffer < 0x40) &&
  Read.u8 t = Char.code 'M' &&
  Read.u8 t = Char.code 'Z' &&
  (seek t 0x3c; seek t (Read.u32 t);
   not (t.position + 4 > dim t.buffer) &&
   Read.u8 t = Char.code 'P' &&
   Read.u8 t = Char.code 'E' &&
   Read.u8 t = 0x0 &&
   Read.u8 t = 0x0)

let check_magic buffer =
  let t = cursor LittleEndian buffer in
  read_magic t

let init_cursor buffer =
  let t = cursor LittleEndian buffer in
  if not (read_magic t) then invalid_format "No PE magic number";
  t

(* File header *)
type file_header = {
  machine                 : u16;
  number_of_sections      : u16;
  time_date_stamp         : u32;
  pointer_to_symbol_table : u32;
  number_of_symbols       : u32;
  size_of_optional_header : u16;
  characteristics         : u16;
}

let arch = function
  | 0x014c -> X86
  | 0x01c0 -> ARM
  | 0x01c2 -> ARM
  | 0x01c4 -> ARM
  | 0x01f0 -> PPC
  | 0x01f1 -> PPC
  | 0x0200 -> IA64
  | 0x0166 -> MIPS
  | 0x0169 -> MIPS
  | 0x0266 -> MIPS
  | 0x0366 -> MIPS
  | 0x0466 -> MIPS
  | 0x8664 -> AMD64
  | 0xaa64 -> ARM64
  | _ -> Unknown

let read_file_header t =
  ensure t 20 "File header truncated";
  let machine                 = Read.u16 t in
  let number_of_sections      = Read.u16 t in
  let time_date_stamp         = Read.u32 t in
  let pointer_to_symbol_table = Read.u32 t in
  let number_of_symbols       = Read.u32 t in
  let size_of_optional_header = Read.u16 t in
  let characteristics         = Read.u16 t in
  { machine; number_of_sections; time_date_stamp;
    pointer_to_symbol_table; number_of_symbols;
    size_of_optional_header; characteristics }

(* Optional header *)
type standard_fields = {
  magic                      : u16;
  size_of_code               : u32;
  size_of_initialized_data   : u32;
  size_of_uninitialized_data : u32;
  address_of_entry_point     : u32;
  base_of_code               : u32;
  base_of_data        : u32 option;
}

type windows_fields = {
  image_base              : u64;
  section_alignement      : u32;
  file_alignement         : u32;
  size_of_image           : u32;
  size_of_headers         : u32;
  checksum                : u32;
  subsystem               : u16;
  dll_characteristics     : u16;
  size_of_stack_reserve   : u64;
  size_of_stack_commit    : u64;
  size_of_heap_reserve    : u64;
  size_of_heap_commit     : u64;
  number_of_rva_and_sizes : u32;
}

type data_directory = {
  virtual_address : u32;
  size            : u32;
}

type data_directories = {
  export_directory       : data_directory;
  import_directory       : data_directory;
  resource_directory     : data_directory;
  exception_directory    : data_directory;
  security_directory     : data_directory;
  basereloc_directory    : data_directory;
  debug_directory        : data_directory;
  globalptr_directory    : data_directory;
  tls_directory          : data_directory;
  load_config_directory  : data_directory;
  bound_import_directory : data_directory;
  iat_directory          : data_directory;
  delay_import_directory : data_directory;
  clr_header_directory   : data_directory;
}

type optional_header = {
  standard_fields  : standard_fields;
  windows_fields   : windows_fields;
  data_directories : data_directories;
}

type program = file_header * optional_header

let read_standard_fields32 t magic =
  ensure t 26 "Standard fields truncated";
  let _major_linker_version      = Read.u8 t in
  let _minor_linker_version      = Read.u8 t in
  let size_of_code               = Read.u32 t in
  let size_of_initialized_data   = Read.u32 t in
  let size_of_uninitialized_data = Read.u32 t in
  let address_of_entry_point     = Read.u32 t in
  let base_of_code               = Read.u32 t in
  let base_of_data               = Some (Read.u32 t) in
  { magic; size_of_code;
    size_of_initialized_data; size_of_uninitialized_data;
    address_of_entry_point; base_of_code; base_of_data }

let read_standard_fields64 t magic =
  ensure t 22 "Standard fields truncated";
  let _major_linker_version      = Read.u8 t in
  let _minor_linker_version      = Read.u8 t in
  let size_of_code               = Read.u32 t in
  let size_of_initialized_data   = Read.u32 t in
  let size_of_uninitialized_data = Read.u32 t in
  let address_of_entry_point     = Read.u32 t in
  let base_of_code               = Read.u32 t in
  let base_of_data               = None in
  { magic; size_of_code;
    size_of_initialized_data; size_of_uninitialized_data;
    address_of_entry_point; base_of_code; base_of_data }

let read_standard_fields t =
  ensure t 2 "PE magic number truncated";
  let magic = Read.u16 t in
  match magic with
  | 0x10b -> read_standard_fields32 t magic
  | 0x20b -> read_standard_fields64 t magic
  | _ -> invalid_format "Invalid PE image file"

let read_windows_fields32 t =
  ensure t 68 "Windows fields truncated";
  let image_base               = Read.u32 t in
  let section_alignement       = Read.u32 t in
  let file_alignement          = Read.u32 t in
  let _major_os_version        = Read.u16 t in
  let _minor_os_version        = Read.u16 t in
  let _major_image_version     = Read.u16 t in
  let _minor_image_version     = Read.u16 t in
  let _major_subsystem_version = Read.u16 t in
  let _minor_subsystem_version = Read.u16 t in
  if not (Read.u32 t = 0) then
    invalid_format "Invalid Win32 version value";
  let size_of_image            = Read.u32 t in
  let size_of_headers          = Read.u32 t in
  let checksum                 = Read.u32 t in
  let subsystem                = Read.u16 t in
  let dll_characteristics      = Read.u16 t in
  let size_of_stack_reserve    = Read.u32 t in
  let size_of_stack_commit     = Read.u32 t in
  let size_of_heap_reserve     = Read.u32 t in
  let size_of_heap_commit      = Read.u32 t in
  if not (Read.u32 t = 0) then
    invalid_format "Invalid loader flags";
  let number_of_rva_and_sizes  = Read.u32 t in
  { image_base; section_alignement; file_alignement;
    size_of_image; size_of_headers; number_of_rva_and_sizes;
    checksum; subsystem; dll_characteristics;
    size_of_stack_reserve; size_of_stack_commit;
    size_of_heap_reserve; size_of_heap_commit }

let read_windows_fields64 t =
  ensure t 88 "Windows fields truncated";
  let image_base               = Read.u64 t in
  let section_alignement       = Read.u32 t in
  let file_alignement          = Read.u32 t in
  let _major_os_version        = Read.u16 t in
  let _minor_os_version        = Read.u16 t in
  let _major_image_version     = Read.u16 t in
  let _minor_image_version     = Read.u16 t in
  let _major_subsystem_version = Read.u16 t in
  let _minor_subsystem_version = Read.u16 t in
  if not (Read.u32 t = 0) then
    invalid_format "Invalid Win32 version value";
  let size_of_image            = Read.u32 t in
  let size_of_headers          = Read.u32 t in
  let checksum                 = Read.u32 t in
  let subsystem                = Read.u16 t in
  let dll_characteristics      = Read.u16 t in
  let size_of_stack_reserve    = Read.u64 t in
  let size_of_stack_commit     = Read.u64 t in
  let size_of_heap_reserve     = Read.u64 t in
  let size_of_heap_commit      = Read.u64 t in
  if not (Read.u32 t = 0) then
    invalid_format "Invalid loader flags";
  let number_of_rva_and_sizes  = Read.u32 t in
  { image_base; section_alignement; file_alignement;
    size_of_image; size_of_headers; number_of_rva_and_sizes;
    checksum; subsystem; dll_characteristics;
    size_of_stack_reserve; size_of_stack_commit;
    size_of_heap_reserve; size_of_heap_commit }

let read_windows_fields standard t =
  match standard.magic with
  | 0x10b -> read_windows_fields32 t
  | 0x20b -> read_windows_fields64 t
  | _ -> invalid_format "Invalid PE image file"

let read_data_directory t =
  ensure t 8 "Data directory truncated";
  let virtual_address = Read.u32 t in
  let size            = Read.u32 t in
  { virtual_address; size }

let read_data_directories t =
  ensure t 96 "Data directories truncated";
  let export_directory       = read_data_directory t in
  let import_directory       = read_data_directory t in
  let resource_directory     = read_data_directory t in
  let exception_directory    = read_data_directory t in
  let security_directory     = read_data_directory t in
  let basereloc_directory    = read_data_directory t in
  let debug_directory        = read_data_directory t in
  if not (Read.u64 t = 0) then
    invalid_format "Invalid data directories";
  let globalptr_directory    = read_data_directory t in
  if not (globalptr_directory.size = 0) then
    invalid_format "Invalid data directories";
  let tls_directory          = read_data_directory t in
  let load_config_directory  = read_data_directory t in
  let bound_import_directory = read_data_directory t in
  let iat_directory          = read_data_directory t in
  let delay_import_directory = read_data_directory t in
  let clr_header_directory   = read_data_directory t in
  if not (Read.u64 t = 0) then
    invalid_format "Invalid data directories";
  { export_directory; import_directory; resource_directory;
    exception_directory; security_directory; basereloc_directory;
    debug_directory; globalptr_directory; tls_directory;
    load_config_directory; bound_import_directory; iat_directory;
    delay_import_directory; clr_header_directory }

let read_optional_header t =
  let standard_fields = read_standard_fields t in
  let windows_fields = read_windows_fields standard_fields t in
  let data_directories = read_data_directories t in
  { standard_fields; windows_fields; data_directories }

let rebase o i = o.windows_fields.image_base + i

(* Section header *)
type section = {
  section_name     : string;
  virtual_size        : u32;
  virtual_address     : u32;
  size_of_raw_data    : u32;
  pointer_to_raw_data : u32;
  characteristics     : u32;
}

let read_section_name t =
  let position = t.position in
  let name = Read.fixed_string t 8 in
  seek t (position + 8);
  name

let read_section t file optional n =
  seek t (optional + file.size_of_optional_header + n * 40);
  (* file header + optional header + nbr * section header *)
  ensure t 40 "Section header truncated";
  let section_name = read_section_name t in
  let virtual_size = Read.u32 t in
  let virtual_address = Read.u32 t in
  let size_of_raw_data = Read.u32 t in
  let pointer_to_raw_data = Read.u32 t in
  let _pointer_to_relocations = Read.u32 t in
  let _pointer_to_linenumbers = Read.u32 t in
  let _number_of_relocations = Read.u16 t in
  let _number_of_linenumbers = Read.u16 t in
  let characteristics = Read.u32 t in
  { section_name; virtual_size; virtual_address;
    size_of_raw_data; pointer_to_raw_data; characteristics }

let read_sections t file optional =
  Array.init file.number_of_sections (read_section t file optional)

exception Found of section
let find_section sections f =
  try
    Array.iter
      (fun section -> if f section then raise (Found section))
      sections;
    None
  with Found section ->
    Some section

let in_section optional (section:section) addr =
  addr >= rebase optional section.virtual_address &&
  addr < rebase optional section.virtual_address + section.virtual_size

let in_section_opt optional section_opt addr =
  match section_opt with
  | None -> false
  | Some section -> in_section optional section addr

let _find_section_by_name sections name =
  find_section sections (fun s -> s.section_name = name)

let find_section_by_addr optional sections addr =
  find_section sections (fun s -> in_section optional s addr)

(* Symbol header *)
type symbol = {
  symbol_name         : string;
  value                 : u32;
  section_number        : u16;
  storage_class         : u8;
  number_of_aux_symbols : u8;
}

let read_symbol_name t strtab strsize =
  let position = t.position in
  let name =
    if Read.u32 t = 0 then (
      let n = Read.u32 t in
      seek t (strtab + n);
      Read.zero_string "Unterminated symbol name" t
        ~maxlen:(strsize - n) ())
    else (
      seek t position;
      Read.fixed_string t 8)
  in
  seek t (position + 8);
  name

let read_symbol t file strtab strsize n =
  seek t (file.pointer_to_symbol_table + n * 18);
  ensure t 18 "Symbol header truncated";
  let symbol_name = read_symbol_name t strtab strsize in
  let value                 = Read.u32 t in
  let section_number        = Read.u16 t in
  let storage_class         = Read.u8 t in
  let number_of_aux_symbols = Read.u8 t in
  { symbol_name; value; section_number;
    storage_class; number_of_aux_symbols }

let read_symbols t file =
  let strtab = file.pointer_to_symbol_table + 18 * file.number_of_symbols in
  let strsize = seek t strtab; Read.u32 t in
  seek t file.pointer_to_symbol_table;
  Array.init file.number_of_symbols (read_symbol t file strtab strsize)

module Section =
struct

  type t = optional_header * section
  type header = section

  let name (_,s) = s.section_name
  let flag (_,s:t) = s.characteristics
  let pos (o,(s:section)) = { raw = s.pointer_to_raw_data; virt = rebase o s.virtual_address }
  let size (_,s) = { raw = s.size_of_raw_data; virt = s.virtual_size }

  let header (_,s) = s

end

module Symbol =
struct

  type t = symbol
  type header = symbol

  let name s = s.symbol_name
  let value s = s.value

  let header s = s

end

module Img =
struct

  type t = program * section array * symbol array * Loader_buf.t
  type header = program

  let arch  ((f,_),_,_,_) = arch f.machine
  let entry ((_,o),_,_,_) = rebase o o.standard_fields.address_of_entry_point
  let endian _ = LittleEndian
  let sections ((_,o),s,_,_) = Array.map (fun s -> o,s) s
  let symbols (_,_,s,_) = Array.copy s

  let header (h,_,_,_) = h

end

let load buffer =
  let t = init_cursor buffer in
  let file_header = read_file_header t in
  let position = t.position in
  let optional_header = read_optional_header t in
  let sections = read_sections t file_header position in
  let symbols = read_symbols t file_header in
  (file_header, optional_header), sections, symbols, buffer

let load_file_descr file_descr =
  let buffer =
    Bigarray.(Array1.map_file file_descr Int8_unsigned C_layout false (-1))
  in load buffer

let load_file path =
  let file_descr = Unix.openfile path [Unix.O_RDONLY] 0 in
  let img = load_file_descr file_descr in
  Unix.close file_descr;
  img

let read_offset (_,_,_,b) offset = b.{offset}

let cache = ref None
let find_section_by_addr_with_cache optional sections addr =
  if not (in_section_opt optional (!cache) addr)
  then cache := find_section_by_addr optional sections addr;
  !cache

let read_address ((_,o),s,_,b) addr =
  match find_section_by_addr_with_cache o s addr with
  | None ->
     let msg = Format.sprintf "Unreachable virtual address %x" addr in
     invalid_arg msg
  | Some (s:section) ->
    let offset = addr - (rebase o s.virtual_address) in
    if offset >= s.size_of_raw_data then 0
    else b.{offset + s.pointer_to_raw_data}

let write_address _ _ _ = assert false
                        
module Offset = Loader_buf.Make
    (struct
      type t = Img.t
      let get t i = read_offset t i
      let dim (_,_,_,b) = Bigarray.Array1.dim b
    end)

module Address = Loader_buf.Make
    (struct
      type t = Img.t
      let get t i = read_address t i
      let dim _ = max_int
    end)

