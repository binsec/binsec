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
open Reader
open Loader_types

let read_magic t =
  (not (dim t < 0x40))
  && Uint8.to_char (Read.u8 t) = 'M'
  && Uint8.to_char (Read.u8 t) = 'Z'
  &&
  (set_pos t 0x3c;
   set_pos t (Uint32.to_int (Read.u32 t));
   (not (get_pos t + 4 > dim t))
   && Uint8.to_char (Read.u8 t) = 'P'
   && Uint8.to_char (Read.u8 t) = 'E'
   && Uint8.to_char (Read.u8 t) = '\x00'
   && Uint8.to_char (Read.u8 t) = '\x00')

let check_magic buffer =
  let t = of_bigarray buffer in
  read_magic t

let init_cursor buffer =
  let t = of_bigarray buffer in
  if not (read_magic t) then invalid_format "No PE magic number";
  t

(* File header *)
type file_header = {
  machine : u16;
  number_of_sections : int;
  time_date_stamp : u32;
  pointer_to_symbol_table : int;
  number_of_symbols : int;
  size_of_optional_header : int;
  characteristics : u16;
}

let arch = function
  | 0x014c -> Machine.x86
  | 0x01c0 -> Machine.armv7 Machine.LittleEndian
  (* | 0x01c2 -> Machine.armv7 *)
  (* Thumb *)
  (* | 0x01c4 -> Machine.armv7 *)
  (* Thumb-2 *)
  (* | 0x01f0 -> Machine.PowerPC
   * | 0x01f1 -> Machine.PowerPC
   * | 0x0200 -> Machine.IA64
   * | 0x0166 -> Machine.MIPS
   * | 0x0169 -> Machine.MIPS
   * | 0x0266 -> Machine.MIPS
   * | 0x0366 -> Machine.MIPS
   * | 0x0466 -> Machine.MIPS *)
  | 0x8664 -> Machine.amd64
  (* | 0xaa64 -> Machine.ARM64 *)
  | _ -> Machine.unknown

let read_file_header t =
  ensure t 20 "File header truncated";
  let machine = Read.u16 t in
  let number_of_sections = Uint16.to_int (Read.u16 t) in
  let time_date_stamp = Read.u32 t in
  let pointer_to_symbol_table = Uint32.to_int (Read.u32 t) in
  let number_of_symbols = Uint32.to_int (Read.u32 t) in
  let size_of_optional_header = Uint16.to_int (Read.u16 t) in
  let characteristics = Read.u16 t in
  {
    machine;
    number_of_sections;
    time_date_stamp;
    pointer_to_symbol_table;
    number_of_symbols;
    size_of_optional_header;
    characteristics;
  }

(* Optional header *)
type standard_fields = {
  magic : u16;
  size_of_code : u32;
  size_of_initialized_data : u32;
  size_of_uninitialized_data : u32;
  address_of_entry_point : Virtual_address.t;
  base_of_code : u32;
  base_of_data : u32 option;
}

type windows_fields = {
  image_base : Virtual_address.t;
  section_alignement : u32;
  file_alignement : u32;
  size_of_image : u32;
  size_of_headers : u32;
  checksum : u32;
  subsystem : u16;
  dll_characteristics : u16;
  size_of_stack_reserve : u64;
  size_of_stack_commit : u64;
  size_of_heap_reserve : u64;
  size_of_heap_commit : u64;
  number_of_rva_and_sizes : u32;
}

type data_directory = { virtual_address : u32; size : u32 }

type data_directories = {
  export_directory : data_directory;
  import_directory : data_directory;
  resource_directory : data_directory;
  exception_directory : data_directory;
  security_directory : data_directory;
  basereloc_directory : data_directory;
  debug_directory : data_directory;
  globalptr_directory : data_directory;
  tls_directory : data_directory;
  load_config_directory : data_directory;
  bound_import_directory : data_directory;
  iat_directory : data_directory;
  delay_import_directory : data_directory;
  clr_header_directory : data_directory;
}

type optional_header = {
  standard_fields : standard_fields;
  windows_fields : windows_fields;
  data_directories : data_directories;
}

type program = file_header * optional_header

let read_standard_fields32 t magic =
  ensure t 26 "Standard fields truncated";
  let _major_linker_version = Read.u8 t in
  let _minor_linker_version = Read.u8 t in
  let size_of_code = Read.u32 t in
  let size_of_initialized_data = Read.u32 t in
  let size_of_uninitialized_data = Read.u32 t in
  let address_of_entry_point = Virtual_address.of_uint32 (Read.u32 t) in
  let base_of_code = Read.u32 t in
  let base_of_data = Some (Read.u32 t) in
  {
    magic;
    size_of_code;
    size_of_initialized_data;
    size_of_uninitialized_data;
    address_of_entry_point;
    base_of_code;
    base_of_data;
  }

let read_standard_fields64 t magic =
  ensure t 22 "Standard fields truncated";
  let _major_linker_version = Read.u8 t in
  let _minor_linker_version = Read.u8 t in
  let size_of_code = Read.u32 t in
  let size_of_initialized_data = Read.u32 t in
  let size_of_uninitialized_data = Read.u32 t in
  let address_of_entry_point = Virtual_address.of_uint32 (Read.u32 t) in
  let base_of_code = Read.u32 t in
  let base_of_data = None in
  {
    magic;
    size_of_code;
    size_of_initialized_data;
    size_of_uninitialized_data;
    address_of_entry_point;
    base_of_code;
    base_of_data;
  }

let read_standard_fields t =
  ensure t 2 "PE magic number truncated";
  let magic = Read.u16 t in
  match Uint16.to_int magic with
  | 0x10b -> read_standard_fields32 t magic
  | 0x20b -> read_standard_fields64 t magic
  | _ -> invalid_format "Invalid PE image file"

let read_windows_fields32 t =
  ensure t 68 "Windows fields truncated";
  let image_base = Virtual_address.of_uint32 (Read.u32 t) in
  let section_alignement = Read.u32 t in
  let file_alignement = Read.u32 t in
  let _major_os_version = Read.u16 t in
  let _minor_os_version = Read.u16 t in
  let _major_image_version = Read.u16 t in
  let _minor_image_version = Read.u16 t in
  let _major_subsystem_version = Read.u16 t in
  let _minor_subsystem_version = Read.u16 t in
  if not (Read.i32 t = 0l) then invalid_format "Invalid Win32 version value";
  let size_of_image = Read.u32 t in
  let size_of_headers = Read.u32 t in
  let checksum = Read.u32 t in
  let subsystem = Read.u16 t in
  let dll_characteristics = Read.u16 t in
  let size_of_stack_reserve = Uint32.to_uint64 (Read.u32 t) in
  let size_of_stack_commit = Uint32.to_uint64 (Read.u32 t) in
  let size_of_heap_reserve = Uint32.to_uint64 (Read.u32 t) in
  let size_of_heap_commit = Uint32.to_uint64 (Read.u32 t) in
  if not (Read.i32 t = 0l) then invalid_format "Invalid loader flags";
  let number_of_rva_and_sizes = Read.u32 t in
  {
    image_base;
    section_alignement;
    file_alignement;
    size_of_image;
    size_of_headers;
    number_of_rva_and_sizes;
    checksum;
    subsystem;
    dll_characteristics;
    size_of_stack_reserve;
    size_of_stack_commit;
    size_of_heap_reserve;
    size_of_heap_commit;
  }

let read_windows_fields64 t =
  ensure t 88 "Windows fields truncated";
  let image_base = Virtual_address.of_uint64 (Read.u64 t) in
  let section_alignement = Read.u32 t in
  let file_alignement = Read.u32 t in
  let _major_os_version = Read.u16 t in
  let _minor_os_version = Read.u16 t in
  let _major_image_version = Read.u16 t in
  let _minor_image_version = Read.u16 t in
  let _major_subsystem_version = Read.u16 t in
  let _minor_subsystem_version = Read.u16 t in
  if not (Read.i32 t = 0l) then invalid_format "Invalid Win32 version value";
  let size_of_image = Read.u32 t in
  let size_of_headers = Read.u32 t in
  let checksum = Read.u32 t in
  let subsystem = Read.u16 t in
  let dll_characteristics = Read.u16 t in
  let size_of_stack_reserve = Read.u64 t in
  let size_of_stack_commit = Read.u64 t in
  let size_of_heap_reserve = Read.u64 t in
  let size_of_heap_commit = Read.u64 t in
  if not (Read.i32 t = 0l) then invalid_format "Invalid loader flags";
  let number_of_rva_and_sizes = Read.u32 t in
  {
    image_base;
    section_alignement;
    file_alignement;
    size_of_image;
    size_of_headers;
    number_of_rva_and_sizes;
    checksum;
    subsystem;
    dll_characteristics;
    size_of_stack_reserve;
    size_of_stack_commit;
    size_of_heap_reserve;
    size_of_heap_commit;
  }

let read_windows_fields standard t =
  match Uint16.to_int standard.magic with
  | 0x10b -> read_windows_fields32 t
  | 0x20b -> read_windows_fields64 t
  | _ -> invalid_format "Invalid PE image file"

let read_data_directory t =
  ensure t 8 "Data directory truncated";
  let virtual_address = Read.u32 t in
  let size = Read.u32 t in
  { virtual_address; size }

let read_data_directories t =
  ensure t 96 "Data directories truncated";
  let export_directory = read_data_directory t in
  let import_directory = read_data_directory t in
  let resource_directory = read_data_directory t in
  let exception_directory = read_data_directory t in
  let security_directory = read_data_directory t in
  let basereloc_directory = read_data_directory t in
  let debug_directory = read_data_directory t in
  if not (Read.i64 t = 0L) then invalid_format "Invalid data directories";
  let globalptr_directory = read_data_directory t in
  if not (Uint32.to_int32 globalptr_directory.size = 0l) then
    invalid_format "Invalid data directories";
  let tls_directory = read_data_directory t in
  let load_config_directory = read_data_directory t in
  let bound_import_directory = read_data_directory t in
  let iat_directory = read_data_directory t in
  let delay_import_directory = read_data_directory t in
  let clr_header_directory = read_data_directory t in
  if not (Read.i64 t = 0L) then invalid_format "Invalid data directories";
  {
    export_directory;
    import_directory;
    resource_directory;
    exception_directory;
    security_directory;
    basereloc_directory;
    debug_directory;
    globalptr_directory;
    tls_directory;
    load_config_directory;
    bound_import_directory;
    iat_directory;
    delay_import_directory;
    clr_header_directory;
  }

let read_optional_header t =
  let standard_fields = read_standard_fields t in
  let windows_fields = read_windows_fields standard_fields t in
  let data_directories = read_data_directories t in
  { standard_fields; windows_fields; data_directories }

let rebase o i = Virtual_address.add o.windows_fields.image_base i

(* Section header *)
type section = {
  section_name : string;
  virtual_size : u32;
  virtual_address : Virtual_address.t;
  size_of_raw_data : int;
  pointer_to_raw_data : int;
  characteristics : u32;
}

let read_section_name t =
  let name = Peek.fixed_string t 8 in
  advance t 8;
  name

let read_section t file optional n =
  set_pos t (optional + file.size_of_optional_header + (n * 40));
  (* file header + optional header + nbr * section header *)
  ensure t 40 "Section header truncated";
  let section_name = read_section_name t in
  let virtual_size = Read.u32 t in
  let virtual_address = Virtual_address.of_uint32 (Read.u32 t) in
  let size_of_raw_data = Uint32.to_int (Read.u32 t) in
  let pointer_to_raw_data = Uint32.to_int (Read.u32 t) in
  let _pointer_to_relocations = Read.u32 t in
  let _pointer_to_linenumbers = Read.u32 t in
  let _number_of_relocations = Read.u16 t in
  let _number_of_linenumbers = Read.u16 t in
  let characteristics = Read.u32 t in
  {
    section_name;
    virtual_size;
    virtual_address;
    size_of_raw_data;
    pointer_to_raw_data;
    characteristics;
  }

let read_sections t file optional =
  Array.init file.number_of_sections (read_section t file optional)

exception Found of section

let find_section sections f =
  try
    Array.iter (fun section -> if f section then raise (Found section)) sections;
    None
  with Found section -> Some section

let in_section optional (section : section) addr =
  addr >= rebase optional section.virtual_address
  && addr
     < Virtual_address.add_bigint
         (Uint32.to_bigint section.virtual_size)
         (rebase optional section.virtual_address)

let in_section_opt optional section_opt addr =
  match section_opt with
  | None -> false
  | Some section -> in_section optional section addr

let find_section_by_addr optional sections addr =
  find_section sections (fun s -> in_section optional s addr)

(* Symbol header *)
type symbol = {
  symbol_name : string;
  value : Virtual_address.t;
  section_number : u16;
  storage_class : u8;
  number_of_aux_symbols : u8;
}

let read_symbol_name t strtab strsize =
  let position = get_pos t in
  let name =
    if Read.i32 t = 0l then (
      let n = Uint32.to_int (Read.u32 t) in
      set_pos t (strtab + n);
      Read.zero_string "Unterminated symbol name" t ~maxlen:(strsize - n) ())
    else (
      set_pos t position;
      Read.fixed_string t 8)
  in
  set_pos t (position + 8);
  name

let read_symbol t file strtab strsize n =
  set_pos t (file.pointer_to_symbol_table + (n * 18));
  ensure t 18 "Symbol header truncated";
  let symbol_name = read_symbol_name t strtab strsize in
  let value = Virtual_address.of_uint32 (Read.u32 t) in
  let section_number = Read.u16 t in
  let storage_class = Read.u8 t in
  let number_of_aux_symbols = Read.u8 t in
  { symbol_name; value; section_number; storage_class; number_of_aux_symbols }

let read_symbols t file =
  let strtab = file.pointer_to_symbol_table + (18 * file.number_of_symbols) in
  let strsize =
    set_pos t strtab;
    Uint32.to_int (Read.u32 t)
  in
  set_pos t file.pointer_to_symbol_table;
  Array.init file.number_of_symbols (read_symbol t file strtab strsize)

module Section = struct
  type t = optional_header * section
  type header = section

  let name (_, s) = s.section_name
  let flag ((_, s) : t) = Uint32.to_int32 s.characteristics

  let pos (o, (s : section)) =
    { raw = s.pointer_to_raw_data; virt = rebase o s.virtual_address }

  let size (_, s) =
    { raw = s.size_of_raw_data; virt = Uint32.to_bigint s.virtual_size }

  let header (_, s) = s

  let has_flag f s =
    let mask =
      match f with
      | Write -> 0x80000000l
      | Read -> 0x40000000l
      | Exec -> 0x20000000l
    in
    Int32.logand (flag s) mask = mask
end

module Symbol = struct
  type t = symbol
  type header = symbol

  let name s = s.symbol_name
  let value s = s.value
  let header s = s
end

let pp_symbol ppf symbol =
  Format.fprintf ppf "@[<h>%s %s@]"
    (Z.format "%-8x" (Virtual_address.to_bigint (Symbol.value symbol)))
    (Symbol.name symbol)

let pp_symbols ppf symbols =
  let nsymbols = Array.length symbols in
  if nsymbols <> 0 then
    Format.fprintf ppf "@[<v 2># Symbols (%d) @ %a@]" nsymbols
      (fun ppf a ->
        Array.iter (fun sy -> Format.fprintf ppf "%a@ " pp_symbol sy) a)
      symbols

let pp_section i ppf section =
  let aux fmt section (f, s) =
    Format.fprintf fmt "%s" (if Section.has_flag f section then s else "-")
  in
  let pp_flags fmt section =
    List.iter (aux fmt section)
      Loader_types.[ (Read, "r"); (Write, "w"); (Exec, "x") ]
  in
  let pp_imap ppf m =
    Format.fprintf ppf "@[<h>%8x %s@]" m.Loader_types.raw
      (Z.format "%8x" m.Loader_types.virt)
  in
  let pos = Section.pos section in
  Format.fprintf ppf "@[<h>%2d %-20s %8lx %8x %s %a %a@]" i
    (Section.name section) (Section.flag section) pos.raw
    (Z.format "%8x" (Virtual_address.to_bigint pos.virt))
    pp_imap (Section.size section) pp_flags section

let pp_sections ppf sections =
  let nsections = Array.length sections in
  if nsections <> 0 then
    Format.fprintf ppf "@[<v 2># Sections (%d)@ %a@]" nsections
      (fun ppf a ->
        Array.iteri (fun i sy -> Format.fprintf ppf "%a@ " (pp_section i) sy) a)
      sections

let pp_arch ppf arch = Format.fprintf ppf "@[Machine: %a@]" Machine.pp arch

let pp_ep ppf ep =
  Format.fprintf ppf "@[Entry point address: %a@]" Virtual_address.pp ep

module Img = struct
  type t = program * section array * symbol array * buffer
  type header = program

  let arch ((f, _), _, _, _) = arch (Uint16.to_int f.machine)

  let entry ((_, o), _, _, _) =
    rebase o o.standard_fields.address_of_entry_point

  let sections ((_, o), s, _, _) = Array.map (fun s -> (o, s)) s
  let symbols (_, _, s, _) = Array.copy s
  let header (h, _, _, _) = h
  let cursor ?at (_, _, _, b) = Reader.of_bigarray ?pos:at b

  let content (_, _, _, b) (_, s) =
    Bigarray.Array1.sub b s.pointer_to_raw_data s.size_of_raw_data

  let buffer (_, _, _, b) = b

  let pp_header ppf img =
    Format.fprintf ppf "@[<v 2># Header@ %a@ %a@]" pp_arch (arch img) pp_ep
      (entry img)

  let pp ppf img =
    Format.fprintf ppf "@[<v 0>%a@ %a@ %a@]" pp_header img pp_symbols
      (symbols img) pp_sections (sections img)
end

let load buffer =
  let t = init_cursor buffer in
  let file_header = read_file_header t in
  let position = get_pos t in
  let optional_header = read_optional_header t in
  let sections = read_sections t file_header position in
  let symbols = read_symbols t file_header in
  ((file_header, optional_header), sections, symbols, buffer)

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

let read_offset (_, _, _, b) offset = Int.unsafe_to_uint8 b.{offset}
let cache = ref None

let find_section_by_addr_with_cache optional sections addr =
  if not (in_section_opt optional !cache addr) then
    cache := find_section_by_addr optional sections addr;
  !cache

let read_address ((_, o), s, _, b) addr =
  match find_section_by_addr_with_cache o s addr with
  | None ->
      let msg =
        Format.asprintf "Unreachable virtual address %a" Virtual_address.pp addr
      in
      invalid_arg msg
  | Some (s : section) ->
      let offset = Virtual_address.diff addr (rebase o s.virtual_address) in
      if offset >= s.size_of_raw_data then Int.unsafe_to_uint8 0
      else Int.unsafe_to_uint8 b.{offset + s.pointer_to_raw_data}
