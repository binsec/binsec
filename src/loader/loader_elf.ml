(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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

let check_magic buffer =
  not (dim buffer < 4) &&
  buffer.{0} = 0x7f &&
  buffer.{1} = Char.code 'E' &&
  buffer.{2} = Char.code 'L' &&
  buffer.{3} = Char.code 'F'

type identification = {
  elf_class      : u8;
  elf_data       : u8;
  elf_version    : u8;
  elf_osabi      : u8;
  elf_abiversion : u8;
}

let endian = function
  | 1 -> LittleEndian
  | 2 -> BigEndian
  | _ -> invalid_format "Unknown ELF data"

let read_identification buffer =
  if (dim buffer < 16) then invalid_format "Identification truncated";
  let elf_class      = buffer.{4} in
  let elf_data       = buffer.{5} in
  let elf_version    = buffer.{6} in
  let elf_osabi      = buffer.{7} in
  let elf_abiversion = buffer.{8} in
  if not (buffer.{9}  = 0 &&
          buffer.{10} = 0 &&
          buffer.{11} = 0 &&
          buffer.{12} = 0 &&
          buffer.{13} = 0 &&
          buffer.{14} = 0 &&
          buffer.{15} = 0)
  then
    invalid_format "Invalid padding after identification";
  { elf_class; elf_data; elf_version;
    elf_osabi; elf_abiversion }

let init_cursor buffer =
  if not (check_magic buffer) then invalid_format "No ELF magic number";
  let ident = read_identification buffer in
  cursor ~at:16 (endian ident.elf_data) buffer, ident

(* Program header *)
type program = {
  e_ident     : identification;
  e_type      : u16;
  e_machine   : u16;
  e_version   : u32;
  e_entry     : u64;
  e_phoff     : u64;
  e_shoff     : u64;
  e_flags     : u32;
  e_ehsize    : u16;
  e_phentsize : u16;
  e_phnum     : u16;
  e_shentsize : u16;
  e_shnum     : u16;
  e_shstrndx  : u16;
}

let arch = function
  | 0x02 -> SPARC
  | 0x03 -> X86
  | 0x08 -> MIPS
  | 0x0a -> MIPS
  | 0x12 -> SPARC
  | 0x14 -> PPC
  | 0x15 -> PPC64
  | 0x28 -> ARM
  | 0x2b -> SPARC
  | 0x32 -> IA64
  | 0x33 -> MIPS
  | 0x3e -> AMD64
  | 0xb7 -> ARM64
  | 0xcb -> XCORE
  | _ -> Unknown

let read_header32 t e_ident =
  ensure t 36 "Program header truncated";
  let e_type      = Read.u16 t in
  let e_machine   = Read.u16 t in
  let e_version   = Read.u32 t in
  let e_entry     = Read.u32 t in
  let e_phoff     = Read.u32 t in
  let e_shoff     = Read.u32 t in
  let e_flags     = Read.u32 t in
  let e_ehsize    = Read.u16 t in
  let e_phentsize = Read.u16 t in
  let e_phnum     = Read.u16 t in
  let e_shentsize = Read.u16 t in
  let e_shnum     = Read.u16 t in
  let e_shstrndx  = Read.u16 t in
  { e_type; e_machine; e_version; e_entry;
    e_phoff; e_shoff; e_flags; e_ehsize;
    e_phentsize; e_phnum; e_shentsize;
    e_shnum; e_shstrndx; e_ident }

let read_header64 t e_ident =
  ensure t 48 "Program header truncated";
  let e_type      = Read.u16 t in
  let e_machine   = Read.u16 t in
  let e_version   = Read.u32 t in
  let e_entry     = Read.u64 t in
  let e_phoff     = Read.u64 t in
  let e_shoff     = Read.u64 t in
  let e_flags     = Read.u32 t in
  let e_ehsize    = Read.u16 t in
  let e_phentsize = Read.u16 t in
  let e_phnum     = Read.u16 t in
  let e_shentsize = Read.u16 t in
  let e_shnum     = Read.u16 t in
  let e_shstrndx  = Read.u16 t in
  { e_type; e_machine; e_version; e_entry;
    e_phoff; e_shoff; e_flags; e_ehsize;
    e_phentsize; e_phnum; e_shentsize;
    e_shnum; e_shstrndx; e_ident }

let read_header t e_ident =
  match e_ident.elf_class with
  | 1 -> read_header32 t e_ident
  | 2 -> read_header64 t e_ident
  | _ -> invalid_format "Invalid ELF class"

(* ELF program header *)
type program_header = {
  p_type    : u32;
  p_flags   : u32;
  p_offset  : u64;
  p_vaddr   : u64;
  p_paddr   : u64;
  p_filesz  : u64;
  p_memsz   : u64;
  p_align   : u64;
}

let read_program_header32 t =
  ensure t 32 "Program header truncated";
  let p_type   = Read.u32 t in
  let p_offset = Read.u32 t in
  let p_vaddr  = Read.u32 t in
  let p_paddr  = Read.u32 t in
  let p_filesz = Read.u32 t in
  let p_memsz  = Read.u32 t in
  let p_flags  = Read.u32 t in
  let p_align  = Read.u32 t in
  {p_type; p_flags; p_offset; p_vaddr;
   p_paddr; p_filesz; p_memsz; p_align }

let read_program_header64 t =
  ensure t 56 "Program header truncated";
  let p_type   = Read.u32 t in
  let p_flags  = Read.u32 t in
  let p_offset = Read.u64 t in
  let p_vaddr  = Read.u64 t in
  let p_paddr  = Read.u64 t in
  let p_filesz = Read.u64 t in
  let p_memsz  = Read.u64 t in
  let p_align  = Read.u64 t in
  {p_type; p_flags; p_offset; p_vaddr;
   p_paddr; p_filesz; p_memsz; p_align }

let read_program_header t header n =
  seek t (header.e_phoff + n * header.e_phentsize);
  match header.e_ident.elf_class with
  | 1 -> read_program_header32 t
  | 2 -> read_program_header64 t
  | _ -> invalid_format "Invalid ELF class"

let read_program_headers t header =
  Array.init header.e_phnum (read_program_header t header)


(* Section header *)
type section = {
  sh_name      : u32;
  sh_type      : u32;
  sh_flags     : u64;
  sh_addr      : u64;
  sh_offset    : u64;
  sh_size      : u64;
  sh_link      : u32;
  sh_info      : u32;
  sh_addralign : u64;
  sh_entsize   : u64;
  sh_name_str : string;
}

let read_section32 t =
  ensure t 40 "Section header truncated";
  let sh_name      = Read.u32 t in
  let sh_type      = Read.u32 t in
  let sh_flags     = Read.u32 t in
  let sh_addr      = Read.u32 t in
  let sh_offset    = Read.u32 t in
  let sh_size      = Read.u32 t in
  let sh_link      = Read.u32 t in
  let sh_info      = Read.u32 t in
  let sh_addralign = Read.u32 t in
  let sh_entsize   = Read.u32 t in
  { sh_name; sh_type; sh_flags; sh_addr;
    sh_offset; sh_size; sh_link; sh_info;
    sh_addralign; sh_entsize; sh_name_str = "" }

let read_section64 t =
  ensure t 64 "Section header truncated";
  let sh_name      = Read.u32 t in
  let sh_type      = Read.u32 t in
  let sh_flags     = Read.u64 t in
  let sh_addr      = Read.u64 t in
  let sh_offset    = Read.u64 t in
  let sh_size      = Read.u64 t in
  let sh_link      = Read.u32 t in
  let sh_info      = Read.u32 t in
  let sh_addralign = Read.u64 t in
  let sh_entsize   = Read.u64 t in
  { sh_name; sh_type; sh_flags; sh_addr;
    sh_offset; sh_size; sh_link; sh_info;
    sh_addralign; sh_entsize; sh_name_str = "" }

let read_section t header n =
  seek t (header.e_shoff + n * header.e_shentsize);
  match header.e_ident.elf_class with
  | 1 -> read_section32 t
  | 2 -> read_section64 t
  | _ -> invalid_format "Invalid ELF class"

let read_section_name t shstrndx shdr =
  let n = shdr.sh_name in
  seek t (shstrndx.sh_offset + n);
  Read.zero_string "Unterminated section name" t
    ~maxlen:(shstrndx.sh_size - n) ()

let read_sections t header =
  let sections = Array.init header.e_shnum (read_section t header) in
  let shstrndx = sections.(header.e_shstrndx) in
  Array.map
    (fun s -> {s with sh_name_str = read_section_name t shstrndx s})
    sections

exception Found of section
let find_section sections f =
  try
    Array.iter
      (fun section -> if f section then raise (Found section))
      sections;
    None
  with Found section ->
    Some section

let in_section section addr =
  addr >= section.sh_addr &&
  addr < section.sh_addr + section.sh_size

let in_section_opt section_opt addr =
  match section_opt with
  | None -> false
  | Some section -> in_section section addr

let find_section_by_name sections name =
  find_section sections (fun s -> s.sh_name_str = name)

let find_section_by_addr sections addr =
  find_section sections (fun s -> in_section s addr)

(* Symbol header *)
type symbol = {
  st_name  : u32;
  st_info  : u8 ;
  st_other : u8 ;
  st_shndx : u16;
  st_value : u64;
  st_size  : u64;
  st_name_str : string;
}

let read_symbol32 t =
  ensure t 16 "Symbol header truncated";
  let st_name  = Read.u32 t in
  let st_value = Read.u32 t in
  let st_size  = Read.u32 t in
  let st_info  = Read.u8  t in
  let st_other = Read.u8  t in
  let st_shndx = Read.u16 t in
  { st_name; st_info; st_other; st_shndx;
    st_value; st_size; st_name_str = "" }

let read_symbol64 t =
  ensure t 24 "Symbol header truncated";
  let st_name  = Read.u32 t in
  let st_info  = Read.u8  t in
  let st_other = Read.u8  t in
  let st_shndx = Read.u16 t in
  let st_value = Read.u64 t in
  let st_size  = Read.u64 t in
  { st_name; st_info; st_other; st_shndx;
    st_value; st_size; st_name_str = "" }

let read_symbol t header section n =
  seek t (section.sh_offset + n * section.sh_entsize);
  match header.e_ident.elf_class with
  | 1 -> read_symbol32 t
  | 2 -> read_symbol64 t
  | _ -> invalid_format "Invalid ELF class"

let read_symbol_name t strtab symb =
  let n = symb.st_name in
  seek t (strtab.sh_offset + n);
  Read.zero_string "Unterminated symbol name" t
    ~maxlen:(strtab.sh_size - n) ()

let read_symbols t ~sym ~str header sections =
  match
    find_section_by_name sections sym,
    find_section_by_name sections str
  with
  | Some symtab, Some strtab when symtab.sh_entsize > 0 ->
    let num = symtab.sh_size / symtab.sh_entsize in
    let symbols = Array.init num (read_symbol t header symtab) in
    Array.map
      (fun s -> {s with st_name_str = read_symbol_name t strtab s})
      symbols
  | _, _ -> [||]

let read_symbols t header sections =
  Array.append
    (read_symbols t ~sym:".symtab" ~str:".strtab" header sections)
    (read_symbols t ~sym:".dynsym" ~str:".dynstr" header sections)

module Section =
struct

  type t = section
  type header = section

  let name s = s.sh_name_str
  let flag s = s.sh_flags
  let pos s = { raw = s.sh_offset; virt = s.sh_addr }
  let size s = { raw = s.sh_size; virt = s.sh_size }

  let header s = s
  let has_flag f s =
    let mask =
      match f with
      | Write -> 1
      | Read -> 2
      | Exec -> 4
    in
    (flag s) land mask = mask

end

module Symbol =
struct

  type t = symbol
  type header = symbol

  let name s = s.st_name_str
  let value s = s.st_value

  let header s = s

end

module Img =
struct

  type t = {program:program;sections:section array;
            symbols:symbol array;buf:Loader_buf.t;
            phdrs:program_header array}
  type header = program

  let arch   i = arch i.program.e_machine
  let entry  i = i.program.e_entry
  let endian i = endian i.program.e_ident.elf_data
  let sections i = Array.copy i.sections
  let symbols  i = Array.copy i.symbols

  let header i = i.program

end

let load buffer =
  let t, e_ident = init_cursor buffer in
  let header = read_header t e_ident in
  let sections = read_sections t header in
  let phdrs = read_program_headers t header in
  let symbols = read_symbols t header sections in
  {Img.program=header; sections; symbols; buf=buffer; phdrs}

let load_file_descr file_descr =
  let buffer =
    Bigarray.(Array1.map_file file_descr Int8_unsigned C_layout false (-1))
  in load buffer

let load_file path =
  let file_descr = Unix.openfile path [Unix.O_RDONLY] 0 in
  let img = load_file_descr file_descr in
  Unix.close file_descr;
  img

let read_offset i offset = i.Img.buf.{offset}

let cache = ref None
let find_section_by_addr_with_cache sections addr =
  if not (in_section_opt (!cache) addr)
  then cache := find_section_by_addr sections addr;
  !cache

let read_address i addr =
  match find_section_by_addr_with_cache i.Img.sections addr with
  | None ->
    let msg = Format.sprintf "Unreachable virtual address %x" addr in
    invalid_arg msg
  | Some s -> i.Img.buf.{addr - s.sh_addr + s.sh_offset}

let write_address img addr bytes =
  match find_section_by_addr img.Img.sections addr with
  | None ->
    let msg = Format.sprintf "Unreachable virtual address %x" addr in
    invalid_arg msg
  | Some s ->
    let base = addr - s.sh_addr + s.sh_offset in
    List.iteri (fun i by -> img.Img.buf.{base + i} <- by) bytes


module Offset = Loader_buf.Make
    (struct
      type t = Img.t
      let get t i = read_offset t i
      let dim i = Bigarray.Array1.dim i.Img.buf
    end)

module Address = Loader_buf.Make
    (struct
      type t = Img.t
      let get t i = read_address t i
      let dim _ = max_int
    end)

let program_headers i = i.Img.phdrs
