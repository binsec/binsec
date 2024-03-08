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

module E_class = struct
  type t = [ `x32 | `x64 ]

  let of_u8 = function
    | 1 -> `x32
    | 2 -> `x64
    | _ -> invalid_format "Invalid elf class"

  let pp ppf = function
    | `x32 -> Format.fprintf ppf "ELF32"
    | `x64 -> Format.fprintf ppf "ELF64"
end

let check_magic buffer =
  (not (dim buffer < 4))
  && buffer.{0} = 0x7f
  && buffer.{1} = Char.code 'E'
  && buffer.{2} = Char.code 'L'
  && buffer.{3} = Char.code 'F'

module E_ident = struct
  type t = {
    kind : E_class.t;
    data : Machine.endianness;
    version : u8;
    osabi : u8;
    abiversion : u8;
  }

  let endian = function
    | 1 -> Machine.LittleEndian
    | 2 -> Machine.BigEndian
    | _ -> invalid_format "Unknown ELF data"

  let read buffer =
    if dim buffer < 16 then invalid_format "Identification truncated";
    let kind = E_class.of_u8 buffer.{4} in
    let data = endian buffer.{5} in
    let version = buffer.{6} in
    let osabi = buffer.{7} in
    let abiversion = buffer.{8} in
    if
      not
        (buffer.{9} = 0
        && buffer.{10} = 0
        && buffer.{11} = 0
        && buffer.{12} = 0
        && buffer.{13} = 0
        && buffer.{14} = 0
        && buffer.{15} = 0)
    then invalid_format "Invalid padding after identification";
    { kind; data; version; osabi; abiversion }

  let init_cursor buffer =
    if not (check_magic buffer) then invalid_format "No ELF magic number";
    let ident = read buffer in
    (cursor ~at:16 ident.data buffer, ident)
end

module Ehdr = struct
  module ET = struct
    type t = NONE | REL | EXEC | DYN | CORE | OS of int | PROC of int

    let of_u16 = function
      | 0 -> NONE
      | 1 -> REL
      | 2 -> EXEC
      | 3 -> DYN
      | 4 -> CORE
      | t when 0xfe00 <= t && t < 0xff00 -> OS t
      | t when 0xff00 <= t && t <= 0xffff -> PROC t
      | _ -> raise @@ Invalid_argument "Not a valid type"

    let ppvx vformat ppf = function
      | NONE -> Format.fprintf ppf "NONE"
      | REL -> Format.fprintf ppf "REL"
      | EXEC -> Format.fprintf ppf "EXEC"
      | DYN -> Format.fprintf ppf "DYN"
      | CORE -> Format.fprintf ppf "CORE"
      | OS t -> vformat ppf t
      | PROC t -> Format.fprintf ppf "PROC(%04x)" t

    let pp = ppvx (fun ppf -> Format.fprintf ppf "OS(%04x)")
  end

  (* Program header *)
  type t = {
    ident : E_ident.t;
    kind : ET.t;
    machine : Machine.t;
    version : u32;
    entry : u64;
    phoff : u64;
    shoff : u64;
    flags : u32;
    ehsize : u16;
    phentsize : u16;
    phnum : u16;
    shentsize : u16;
    shnum : u16;
    shstrndx : u16;
  }

  let arch endianness (mode : [ `x64 | `x32 ]) = function
    (* | 0x02 -> Machine.SPARC *)
    | 0x03 -> Machine.x86
    (* | 0x08 -> Machine.MIPS
     * | 0x0a -> Machine.MIPS
     * | 0x12 -> Machine.SPARC
     * | 0x14 -> Machine.PowerPC *)
    | 0x15 -> Machine.ppc64 endianness
    | 0x28 -> Machine.armv7 endianness
    (* | 0x2b -> Machine.SPARC
     * | 0x32 -> Machine.IA64
     * | 0x33 -> Machine.MIPS *)
    | 0x3e -> Machine.amd64
    | 0xb7 -> Machine.armv8 endianness
    (* | 0xcb -> Machine.XCORE *)
    | 0xf3 -> Machine.riscv (mode :> [ `x128 | `x64 | `x32 ])
    | _ -> Machine.unknown

  let read_32 t ident =
    ensure t 36 "Program header truncated";
    let kind = ET.of_u16 (Read.u16 t) in
    let machine = arch ident.E_ident.data ident.E_ident.kind (Read.u16 t) in
    let version = Read.u32 t in
    let entry = Read.u32 t in
    let phoff = Read.u32 t in
    let shoff = Read.u32 t in
    let flags = Read.u32 t in
    let ehsize = Read.u16 t in
    let phentsize = Read.u16 t in
    let phnum = Read.u16 t in
    let shentsize = Read.u16 t in
    let shnum = Read.u16 t in
    let shstrndx = Read.u16 t in
    {
      kind;
      machine;
      version;
      entry;
      phoff;
      shoff;
      flags;
      ehsize;
      phentsize;
      phnum;
      shentsize;
      shnum;
      shstrndx;
      ident;
    }

  let read_64 t ident =
    ensure t 48 "Program header truncated";
    let kind = ET.of_u16 (Read.u16 t) in
    let machine = arch ident.E_ident.data ident.E_ident.kind (Read.u16 t) in
    let version = Read.u32 t in
    let entry = Read.u64 t in
    let phoff = Read.u64 t in
    let shoff = Read.u64 t in
    let flags = Read.u32 t in
    let ehsize = Read.u16 t in
    let phentsize = Read.u16 t in
    let phnum = Read.u16 t in
    let shentsize = Read.u16 t in
    let shnum = Read.u16 t in
    let shstrndx = Read.u16 t in
    {
      kind;
      machine;
      version;
      entry;
      phoff;
      shoff;
      flags;
      ehsize;
      phentsize;
      phnum;
      shentsize;
      shnum;
      shstrndx;
      ident;
    }

  let read t ident =
    match ident.E_ident.kind with
    | `x32 -> read_32 t ident
    | `x64 -> read_64 t ident

  let ppvx =
    let columns = [| Prettytbl.Column.default; Prettytbl.Column.default |] in
    fun vrows ppf h ->
      let t = Prettytbl.make columns in
      Prettytbl.append t
        [| "Class:"; Format.asprintf "%a" E_class.pp h.ident.E_ident.kind |];
      (* 2's complement as long as endianness has successfully been read 1 | 2 *)
      Prettytbl.append t
        [|
          "Data:";
          Format.asprintf "2's complement, %a" Machine.Endianness.pp
            h.ident.E_ident.data;
        |];
      Prettytbl.append t [| "Type:"; Format.asprintf "%a" ET.pp h.kind |];
      Array.iter (fun vrow -> Prettytbl.append t vrow) vrows;
      Prettytbl.append t
        [| "Machine:"; Format.asprintf "%a" Machine.ISA.pp h.machine |];
      if h.kind <> ET.REL then
        Prettytbl.append t
          [| "Entry point address:"; Printf.sprintf "%#x" h.entry |];
      Format.fprintf ppf "@[<v 2>ELF Header:@\n";
      Prettytbl.pp ppf t;
      Format.pp_close_box ppf ()

  let pp = ppvx [||]
end

module Shdr = struct
  module SHT = struct
    type t =
      | NULL
      | PROGBITS
      | SYMTAB
      | STRTAB
      | RELA
      | HASH
      | DYNAMIC
      | NOTE
      | NOBITS
      | REL
      | SHLIB
      | DYNSYM
      | INIT_ARRAY
      | FINI_ARRAY
      | PREINIT_ARRAY
      | GROUP
      | SYMTAB_SHNDX
      | RELR
      | OS of int
      | PROC of int
      | USER of int

    let of_u32 = function
      | 0 -> NULL
      | 1 -> PROGBITS
      | 2 -> SYMTAB
      | 3 -> STRTAB
      | 4 -> RELA
      | 5 -> HASH
      | 6 -> DYNAMIC
      | 7 -> NOTE
      | 8 -> NOBITS
      | 9 -> REL
      | 10 -> SHLIB
      | 11 -> DYNSYM
      | 14 -> INIT_ARRAY
      | 15 -> FINI_ARRAY
      | 16 -> PREINIT_ARRAY
      | 17 -> GROUP
      | 18 -> SYMTAB_SHNDX
      | 19 -> RELR
      | t when 0x60000000 <= t && t < 0x70000000 -> OS t
      | t when 0x70000000 <= t && t < 0x80000000 -> PROC t
      | t when 0x80000000 <= t && t <= 0xffffffff -> USER t
      | _ -> raise @@ Invalid_argument "Not a valid section type"

    let ppvx vformat ppf = function
      | NULL -> Format.fprintf ppf "NULL"
      | PROGBITS -> Format.fprintf ppf "PROGBITS"
      | SYMTAB -> Format.fprintf ppf "SYMTAB"
      | STRTAB -> Format.fprintf ppf "STRTAB"
      | RELA -> Format.fprintf ppf "RELA"
      | HASH -> Format.fprintf ppf "HASH"
      | DYNAMIC -> Format.fprintf ppf "DYNAMIC"
      | NOTE -> Format.fprintf ppf "NOTE"
      | NOBITS -> Format.fprintf ppf "NOBITS"
      | REL -> Format.fprintf ppf "REL"
      | SHLIB -> Format.fprintf ppf "SHLIB"
      | DYNSYM -> Format.fprintf ppf "DYNSYM"
      | INIT_ARRAY -> Format.fprintf ppf "INIT_ARRAY"
      | FINI_ARRAY -> Format.fprintf ppf "FINI_ARRAY"
      | PREINIT_ARRAY -> Format.fprintf ppf "PREINIT_ARRAY"
      | GROUP -> Format.fprintf ppf "GROUP"
      | SYMTAB_SHNDX -> Format.fprintf ppf "SYMTAB_SHNDX"
      | RELR -> Format.fprintf ppf "RELR"
      | OS t -> vformat ppf t
      | PROC t -> Format.fprintf ppf "PROC(%08x)" t
      | USER t -> Format.fprintf ppf "USER(%08x)" t

    let pp = ppvx (fun ppf -> Format.fprintf ppf "OS(%08x)")
  end

  module SHF = struct
    type t =
      | WRITE
      | ALLOC
      | EXECINSTR
      | MERGE
      | STRINGS
      | INFO_LINK
      | LINK_ORDER
      | OS_NONCONFORMING
      | GROUP
      | TLS
      | OS
      | PROC

    let is f = function
      | WRITE -> f land 0x1 > 0
      | ALLOC -> f land 0x2 > 0
      | EXECINSTR -> f land 0x4 > 0
      | MERGE -> f land 0x10 > 0
      | STRINGS -> f land 0x20 > 0
      | INFO_LINK -> f land 0x40 > 0
      | LINK_ORDER -> f land 0x80 > 0
      | OS_NONCONFORMING -> f land 0x100 > 0
      | GROUP -> f land 0x200 > 0
      | TLS -> f land 0x400 > 0
      | OS -> f land 0x0ff00000 > 0
      | PROC -> f land 0xf0000000 > 0

    let repr = function
      | WRITE -> 'W'
      | ALLOC -> 'A'
      | EXECINSTR -> 'X'
      | MERGE -> 'M'
      | STRINGS -> 'S'
      | INFO_LINK -> 'I'
      | LINK_ORDER -> 'L'
      | OS_NONCONFORMING -> 'O'
      | GROUP -> 'G'
      | TLS -> 'T'
      | OS -> 'o'
      | PROC -> 'p'

    let ifpp t ppf f = if is f t then Format.fprintf ppf "%c" @@ repr t

    let pp ppf f =
      ifpp WRITE ppf f;
      ifpp ALLOC ppf f;
      ifpp EXECINSTR ppf f;
      ifpp MERGE ppf f;
      ifpp STRINGS ppf f;
      ifpp INFO_LINK ppf f;
      ifpp LINK_ORDER ppf f;
      ifpp OS_NONCONFORMING ppf f;
      ifpp GROUP ppf f;
      ifpp TLS ppf f
  end

  (* Section header *)
  type t = {
    idx : int;
    name : string;
    kind : SHT.t;
    flags : u64;
    addr : u64;
    offset : u64;
    size : u64;
    link : u32;
    info : u32;
    addralign : u64;
    entsize : u64;
  }

  let dummy =
    {
      idx = 0;
      name = "";
      kind = SHT.NULL;
      flags = 0;
      addr = 0;
      offset = 0;
      size = 0;
      link = 0;
      info = 0;
      addralign = 0;
      entsize = 0;
    }

  let read_32 t =
    ensure t 40 "Section header truncated";
    let idx = Read.u32 t in
    let kind = SHT.of_u32 (Read.u32 t) in
    let flags = Read.u32 t in
    let addr = Read.u32 t in
    let offset = Read.u32 t in
    let size = Read.u32 t in
    let link = Read.u32 t in
    let info = Read.u32 t in
    let addralign = Read.u32 t in
    let entsize = Read.u32 t in
    {
      idx;
      name = "";
      kind;
      flags;
      addr;
      offset;
      size;
      link;
      info;
      addralign;
      entsize;
    }

  let read_64 t =
    ensure t 64 "Section header truncated";
    let idx = Read.u32 t in
    let kind = SHT.of_u32 (Read.u32 t) in
    let flags = Read.u64 t in
    let addr = Read.u64 t in
    let offset = Read.u64 t in
    let size = Read.u64 t in
    let link = Read.u32 t in
    let info = Read.u32 t in
    let addralign = Read.u64 t in
    let entsize = Read.u64 t in
    {
      idx;
      name = "";
      kind;
      flags;
      addr;
      offset;
      size;
      link;
      info;
      addralign;
      entsize;
    }

  let read t header n =
    seek t Ehdr.(header.shoff + (n * header.shentsize));
    match header.Ehdr.ident.E_ident.kind with
    | `x32 -> read_32 t
    | `x64 -> read_64 t

  let with_name t shstrndx shdr =
    let n = shdr.idx in
    seek t (shstrndx.offset + n);
    Read.zero_string "Unterminated section name" t ~maxlen:(shstrndx.size - n)
      ()

  let read_all t header =
    let sections = Array.init header.Ehdr.shnum (read t header) in
    let shstrndx = sections.(header.Ehdr.shstrndx) in
    Array.iteri
      (fun i s ->
        sections.(i) <- { s with idx = i; name = with_name t shstrndx s })
      sections;
    sections

  let contains addr section =
    (* [Improvement] Maybe there is a better, more generic way to handle the
       problem below than checking for the SHF_ALLOC flag. But it fixes the
       behavior of the loader/disassembly on .o files. *)
    SHF.(is section.flags ALLOC)
    && addr >= section.addr
    && addr < section.addr + section.size

  let find_by_name sections name =
    Array_utils.find_opt (fun s -> s.name = name) sections

  let pretty_formats, pretty_names =
    ( [|
        Prettytbl.(
          Column.make ~max_length:2 ~left_border:"[" ~right_border:"]" ~align:R
            ());
        Prettytbl.(Column.make ~max_length:16 ());
        Prettytbl.(Column.make ~max_length:10 ());
        Prettytbl.(Column.make ~max_length:16 ());
        Prettytbl.(Column.make ~max_length:6 ());
        Prettytbl.(Column.make ~max_length:6 ());
        Prettytbl.(Column.make ~max_length:2 ());
        Prettytbl.(Column.make ~max_length:3 ~align:R ());
        Prettytbl.(Column.make ~max_length:2 ~align:R ());
        Prettytbl.(Column.make ~max_length:3 ~align:R ());
        Prettytbl.(Column.make ~max_length:2 ~align:R ());
      |],
      [|
        "Nr";
        "Name";
        "Type";
        "Addr";
        "Off";
        "Size";
        "ES";
        "Flg";
        "Lk";
        "Inf";
        "Al";
      |] )

  let ppvx_all aformat vformat ppf sections =
    let t = Prettytbl.make pretty_formats in
    Prettytbl.append t pretty_names;
    Array.iter
      (fun section ->
        Prettytbl.append t
          [|
            string_of_int section.idx;
            section.name;
            Format.asprintf "%a" (SHT.ppvx vformat) section.kind;
            Format.asprintf "%a"
              Machine.(Bitwidth.pp_print_hex (aformat :> bitwidth))
              section.addr;
            Printf.sprintf "%06x" section.offset;
            Printf.sprintf "%06x" section.size;
            Printf.sprintf "%02x" section.entsize;
            Format.asprintf "%a" SHF.pp section.flags;
            string_of_int section.link;
            string_of_int section.info;
            string_of_int section.addralign;
          |])
      sections;
    Format.fprintf ppf "@[<v 2>Section Headers:@\n";
    Prettytbl.pp ppf t;
    Format.fprintf ppf
      "@]@\n\
       @[<hov 2>Key to Flags:@\n\
       W (write),@ A (alloc),@ X (execute),@ M (merge),@ S (strings),@ I \
       (info),@ L (link order),@ G (group),@ T (TLS),@ O (extra OS processing \
       required)@]"

  module SHN = struct
    type section = t
    type t = UNDEF | SEC of section | PROC of int | OS of int | ABS | COMMON
    (* | XINDEX *)

    let of_u16 sections = function
      | 0x0000 -> UNDEF
      | n when 0x0000 < n && n < 0xff00 -> SEC sections.(n)
      | n when 0xff00 <= n && n < 0xff20 -> PROC n
      | n when 0xff20 <= n && n < 0xff40 -> OS n
      | 0xfff1 -> ABS
      | 0xfff2 -> COMMON
      | _ -> raise @@ Invalid_argument "Not a valid section indexes"

    let pp ppf = function
      | UNDEF -> Format.fprintf ppf "UND"
      | SEC s -> Format.fprintf ppf "%s" s.name
      | PROC n -> Format.fprintf ppf "PROC(%d)" n
      | OS n -> Format.fprintf ppf "OS(%d)" n
      | ABS -> Format.fprintf ppf "ABS"
      | COMMON -> Format.fprintf ppf "COM"
  end
end

module Sym = struct
  module STT = struct
    type t =
      | NOTYPE
      | OBJECT
      | FUNC
      | SECTION
      | FILE
      | COMMON
      | TLS
      | OS of int
      | PROC of int

    let of_u8 i =
      match i land 0xf with
      | 0 -> NOTYPE
      | 1 -> OBJECT
      | 2 -> FUNC
      | 3 -> SECTION
      | 4 -> FILE
      | 6 -> TLS
      | t when 10 <= t && t < 13 -> OS t
      | t when 13 <= t && t <= 15 -> PROC t
      | _ -> raise @@ Invalid_argument "Not a valid symbol type"

    let ppvx vformat ppf = function
      | NOTYPE -> Format.fprintf ppf "NOTYPE"
      | OBJECT -> Format.fprintf ppf "OBJECT"
      | FUNC -> Format.fprintf ppf "FUNC"
      | SECTION -> Format.fprintf ppf "SECTION"
      | FILE -> Format.fprintf ppf "FILE"
      | COMMON -> Format.fprintf ppf "COMMON"
      | TLS -> Format.fprintf ppf "TLS"
      | OS t -> vformat ppf t
      | PROC t -> Format.fprintf ppf "PROC(%x)" t

    let pp = ppvx (fun ppf -> Format.fprintf ppf "OS(%x)")
  end

  module STB = struct
    type t = LOCAL | GLOBAL | WEAK | OS of int | PROC of int

    let of_u8 i =
      match i lsr 4 with
      | 0 -> LOCAL
      | 1 -> GLOBAL
      | 2 -> WEAK
      | b when 10 <= b && b < 13 -> OS b
      | b when 13 <= b && b <= 15 -> PROC b
      | _ -> raise @@ Invalid_argument "Not a valid symbol binding"

    let pp ppf = function
      | LOCAL -> Format.fprintf ppf "LOCAL"
      | GLOBAL -> Format.fprintf ppf "GLOBAL"
      | WEAK -> Format.fprintf ppf "WEAK"
      | OS b -> Format.fprintf ppf "OS(%x)" b
      | PROC b -> Format.fprintf ppf "PROC(%x)" b
  end

  (* Symbol header *)
  type t = {
    name : string;
    kind : STT.t;
    bind : STB.t;
    other : u8;
    sh : Shdr.SHN.t;
    value : u64;
    size : u64;
  }

  let read_name t strtab idx =
    seek t (strtab.Shdr.offset + idx);
    Read.zero_string "Unterminated symbol name" t
      ~maxlen:(strtab.Shdr.size - idx) ()

  let read_32 t sections strtab =
    ensure t 16 "Symbol header truncated";
    let idx = Read.u32 t in
    let value = Read.u32 t in
    let size = Read.u32 t in
    let info = Read.u8 t in
    let kind = STT.of_u8 info in
    let bind = STB.of_u8 info in
    let other = Read.u8 t in
    let sh = Shdr.SHN.of_u16 sections (Read.u16 t) in
    let name = read_name t strtab idx in
    { name; kind; bind; other; sh; value; size }

  let read_64 t sections strtab =
    ensure t 24 "Symbol header truncated";
    let idx = Read.u32 t in
    let info = Read.u8 t in
    let kind = STT.of_u8 info in
    let bind = STB.of_u8 info in
    let other = Read.u8 t in
    let sh = Shdr.SHN.of_u16 sections (Read.u16 t) in
    let value = Read.u64 t in
    let size = Read.u64 t in
    let name = read_name t strtab idx in
    { name; kind; bind; other; sh; value; size }

  let read t header sections symtab strtab n =
    seek t Shdr.(symtab.offset + (n * symtab.entsize));
    match header.Ehdr.ident.E_ident.kind with
    | `x32 -> read_32 t sections strtab
    | `x64 -> read_64 t sections strtab

  let read_all t header sections =
    Array.map
      (fun section ->
        match section.Shdr.kind with
        | (Shdr.SHT.SYMTAB | Shdr.SHT.DYNSYM) when section.Shdr.entsize <> 0 ->
            Array.init
              (section.Shdr.size / section.Shdr.entsize)
              (read t header sections section sections.(section.Shdr.link))
        | _ -> [||])
      sections

  let pretty_formats, pretty_names =
    ( [|
        Prettytbl.(Column.make ~max_length:3 ~right_border:":" ~align:R ());
        Prettytbl.(Column.make ~max_length:16 ~align:R ());
        Prettytbl.(Column.make ~max_length:10 ~align:R ());
        Prettytbl.(Column.make ~max_length:8 ());
        Prettytbl.(Column.make ~max_length:8 ());
        Prettytbl.(Column.make ~max_length:16 ());
        Prettytbl.(Column.make ~max_length:20 ());
      |],
      [| "Num"; "Value"; "Size"; "Type"; "Bind"; "Section"; "Name" |] )

  let ppvx_all iformat vformat ppf symbols =
    let t = Prettytbl.make pretty_formats in
    Prettytbl.append t pretty_names;
    Array.iteri
      (fun i symbol ->
        Prettytbl.append t
          [|
            string_of_int i;
            Format.asprintf "%a"
              Machine.(Bitwidth.pp_print_hex (iformat :> bitwidth))
              symbol.value;
            string_of_int symbol.size;
            Format.asprintf "%a" (STT.ppvx vformat) symbol.kind;
            Format.asprintf "%a" STB.pp symbol.bind;
            Format.asprintf "%a" Shdr.SHN.pp symbol.sh;
            symbol.name;
          |])
      symbols;
    Prettytbl.pp ppf t;
    Format.pp_close_box ppf ()
end

module Phdr = struct
  (* ELF program header *)
  type t = {
    kind : u32;
    flags : u32;
    offset : u64;
    vaddr : u64;
    paddr : u64;
    filesz : u64;
    memsz : u64;
    align : u64;
  }

  let dummy =
    {
      kind = 0;
      flags = 0;
      offset = 0;
      vaddr = 0;
      paddr = 0;
      filesz = 0;
      memsz = 0;
      align = 0;
    }

  let read_32 t =
    ensure t 32 "Program header truncated";
    let kind = Read.u32 t in
    let offset = Read.u32 t in
    let vaddr = Read.u32 t in
    let paddr = Read.u32 t in
    let filesz = Read.u32 t in
    let memsz = Read.u32 t in
    let flags = Read.u32 t in
    let align = Read.u32 t in
    { kind; flags; offset; vaddr; paddr; filesz; memsz; align }

  let read_64 t =
    ensure t 56 "Program header truncated";
    let kind = Read.u32 t in
    let flags = Read.u32 t in
    let offset = Read.u64 t in
    let vaddr = Read.u64 t in
    let paddr = Read.u64 t in
    let filesz = Read.u64 t in
    let memsz = Read.u64 t in
    let align = Read.u64 t in
    { kind; flags; offset; vaddr; paddr; filesz; memsz; align }

  let read t header n =
    seek t Ehdr.(header.phoff + (n * header.phentsize));
    match header.Ehdr.ident.E_ident.kind with
    | `x32 -> read_32 t
    | `x64 -> read_64 t

  let read_all t header = Array.init header.Ehdr.phnum (read t header)

  let contains addr pheader =
    addr >= pheader.vaddr && addr < pheader.vaddr + pheader.memsz
end

module Section = struct
  type t = Shdr.t
  type header = t

  let name s = s.Shdr.name
  let flag s = s.Shdr.flags
  let pos s = { raw = s.Shdr.offset; virt = s.Shdr.addr }

  let size s =
    let raw = if Shdr.(s.kind = SHT.NOBITS) then 0 else s.Shdr.size in
    { raw; virt = s.Shdr.size }

  let header s = s

  let has_flag f s =
    let mask =
      match f with
      | Write -> Shdr.SHF.WRITE
      | Read -> Shdr.SHF.ALLOC
      | Exec -> Shdr.SHF.EXECINSTR
    in
    Shdr.SHF.is (flag s) mask
end

module Symbol = struct
  type t = Sym.t
  type header = t

  let name s = s.Sym.name
  let value s = s.Sym.value
  let header s = s
end

module Note = struct
  type t = { name : string; kind : int; offset : int; size : int }

  let read t =
    let namesz = Read.u32 t in
    let size = Read.u32 t in
    let kind = Read.u32 t in
    let name = Read.fixed_string t namesz in
    seek t ((t.position + 3) land -4);
    let offset = t.position in
    advance t ((size + 3) land -4);
    { name; kind; offset; size }

  let rec append_all notes bound cursor =
    if cursor.position >= bound then notes
    else append_all (read cursor :: notes) bound cursor
end

module rec Vendor : sig
  type t = Unknown | GNU of string

  include Sigs.PRINTABLE with type t := t

  val pretty_rows : t -> string array array

  module Section : sig
    val ppt : t -> Format.formatter -> int -> unit
  end

  module Symbol : sig
    val ppt : t -> Format.formatter -> int -> unit
  end

  val read :
    Ehdr.t ->
    Shdr.t array ->
    Sym.t array array ->
    Loader_buf.t ->
    Ehdr.t * t * Shdr.t array * Sym.t array array * Loader_buf.t
end = struct
  type t = Unknown | GNU of string

  let pp ppf = function
    | Unknown -> Format.fprintf ppf "Unknown"
    | GNU _ -> Format.fprintf ppf "GNU"

  let pretty_rows = function
    | Unknown -> [||]
    | GNU kernel ->
        [| [| "OS/ABI:"; "Linux - GNU" |]; [| "Kernel Version:"; kernel |] |]

  module Common = struct
    let ppt ppf x = Format.fprintf ppf "OS(%x)" x
    let ppt8 ppf x = Format.fprintf ppf "OS(%08x)" x
  end

  module GNU = struct
    module Section = struct
      let ppt ppf = function
        | 0x6ffffff6 -> Format.fprintf ppf "GNU_HASH"
        | 0x6ffffffd -> Format.fprintf ppf "VERDEF"
        | 0x6ffffffe -> Format.fprintf ppf "VERNEED"
        | 0x6fffffff -> Format.fprintf ppf "VERSYM"
        | t when 0x60000000 <= t && t < 0x70000000 -> Common.ppt8 ppf t
        | _ -> raise @@ Invalid_argument "Not a vendor specific type"
    end

    module Symbol = struct
      let ppt ppf = function
        | 10 -> Format.fprintf ppf "IFUNC"
        | t when 10 <= t && t < 13 -> Common.ppt ppf t
        | _ -> raise @@ Invalid_argument "Not a vendor specific type"
    end
  end

  module Section = struct
    let ppt = function Unknown -> Common.ppt8 | GNU _ -> GNU.Section.ppt
  end

  module Symbol = struct
    let ppt = function Unknown -> Common.ppt | GNU _ -> GNU.Symbol.ppt
  end

  let read header sections symbols buf =
    match Shdr.find_by_name sections ".note.ABI-tag" with
    | None -> (header, Unknown, sections, symbols, buf)
    | Some note ->
        let cursor =
          cursor ~at:note.Shdr.offset header.Ehdr.ident.E_ident.data buf
        in
        let namesz = Read.u32 cursor in
        let descsz = Read.u32 cursor in
        let kind = Read.u32 cursor in
        if kind = 1 then
          let name =
            Read.zero_string "Inconsistent note format" cursor ~maxlen:namesz ()
          in
          if name = "GNU" && descsz = 16 then (
            let padding = ((namesz lxor 0b11) + 1) land 0b11 in
            advance cursor padding;
            if Read.u32 cursor = 0 then
              let version = Read.u32 cursor in
              let major = Read.u32 cursor in
              let minor = Read.u32 cursor in
              let kernel = Printf.sprintf "%d.%d.%d" version major minor in
              (header, GNU kernel, sections, symbols, buf)
            else (header, Unknown, sections, symbols, buf))
          else (header, Unknown, sections, symbols, buf)
        else (header, Unknown, sections, symbols, buf)
end

and Img : sig
  type t = {
    header : Ehdr.t;
    vendor : Vendor.t;
    sections : Shdr.t array;
    mutable last_section : Shdr.t;
    symtabs : Sym.t array array;
    buf : Loader_buf.t;
    phdrs : Phdr.t array;
    mutable last_phdr : Phdr.t;
    notes : Note.t array;
  }

  type header = Ehdr.t

  val arch : t -> Machine.t
  val entry : t -> int
  val endian : t -> Machine.endianness
  val sections : t -> Section.t array
  val symbols : t -> Symbol.t array
  val notes : t -> Note.t array
  val header : t -> header
  val cursor : ?at:int -> t -> Loader_buf.cursor
  val content : t -> Section.t -> Loader_buf.t

  include Sigs.PRINTABLE with type t := t
end = struct
  type t = {
    header : Ehdr.t;
    vendor : Vendor.t;
    sections : Shdr.t array;
    mutable last_section : Shdr.t;
    symtabs : Sym.t array array;
    buf : Loader_buf.t;
    phdrs : Phdr.t array;
    mutable last_phdr : Phdr.t;
    notes : Note.t array;
  }

  type header = Ehdr.t

  let arch i = i.header.Ehdr.machine
  let entry i = i.header.Ehdr.entry
  let endian i = i.header.Ehdr.ident.E_ident.data
  let sections i = Array.copy i.sections
  let symbols i = Array.concat @@ Array.to_list i.symtabs
  let notes i = i.notes
  let header i = i.header

  let cursor ?(at = 0) i =
    Loader_buf.cursor ~at i.header.Ehdr.ident.E_ident.data i.buf

  let content i (s : Shdr.t) =
    if s.kind = Shdr.SHT.NOBITS then Bigarray.Array1.sub i.buf 0 0
    else Bigarray.Array1.sub i.buf s.Shdr.offset s.size

  let pp ppf t =
    let e_class = t.header.Ehdr.ident.E_ident.kind in
    let vrows = Vendor.pretty_rows t.vendor in
    Format.fprintf ppf "@[<v>%a@ @ %a@ @ " (Ehdr.ppvx vrows) t.header
      (Shdr.ppvx_all e_class (Vendor.Section.ppt t.vendor))
      t.sections;
    Array.iteri
      (fun i symbols ->
        if Array.length symbols <> 0 then (
          Format.fprintf ppf "@[<v 2>Symbol table '%s' contains %d entries:@\n"
            t.sections.(i).Shdr.name (Array.length symbols);
          (Sym.ppvx_all e_class (Vendor.Symbol.ppt t.vendor)) ppf symbols;
          Format.fprintf ppf "@]@ @ "))
      t.symtabs;
    Format.pp_close_box ppf ()
end

let alloc img =
  if Ehdr.(img.Img.header.kind = ET.REL) then
    if img.Img.header.Ehdr.entry <> 0 then
      Elf_options.Logger.fatal "Unexpected entry point %a for a relocable file"
        Machine.(
          Bitwidth.pp_print_hex
            (img.Img.header.Ehdr.ident.E_ident.kind :> bitwidth))
        img.Img.header.Ehdr.entry;
  let sections = img.Img.sections and symtabs = img.Img.symtabs in
  let common_idx =
    try
      Array_utils.findi
        (fun s ->
          Shdr.(s.kind = SHT.NOBITS)
          && Section.has_flag Read s && Section.has_flag Write s)
        sections
    with Not_found ->
      failwith
        "Unable to performe symbol allocations: candidate for COMMON section \
         not found"
  in
  let common = sections.(common_idx) in
  let common_oldsize = common.Shdr.size in
  let common_newsize =
    Array.fold_left
      (fun size symbols ->
        Array_utils.fold_lefti
          (fun i size sym ->
            if Shdr.(sym.Sym.sh = SHN.COMMON) then (
              let padding = size mod sym.Sym.value in
              let size =
                if padding = 0 then size else size + sym.Sym.value - padding
              in
              symbols.(i) <-
                { sym with Sym.sh = Shdr.SHN.SEC common; value = size };
              size + sym.Sym.size)
            else size)
          size symbols)
      common_oldsize symtabs
  in
  sections.(common_idx) <- { common with Shdr.size = common_newsize };
  ignore
  @@ Array_utils.fold_lefti
       (fun i addr sec ->
         if Shdr.(SHF.is sec.flags SHF.ALLOC) then (
           let padding = addr mod sec.Shdr.addralign in
           let addr =
             if padding = 0 then addr else addr + sec.Shdr.addralign - padding
           in
           sections.(i) <- { sec with Shdr.addr };
           addr + sec.Shdr.size)
         else addr)
       0 sections;
  Array.iter
    (fun symbols ->
      Array.iteri
        (fun i sym ->
          match sym.Sym.sh with
          | Shdr.SHN.SEC section ->
              let section = sections.(section.Shdr.idx) in
              symbols.(i) <-
                {
                  sym with
                  Sym.value = sym.Sym.value + section.Shdr.addr;
                  sh = Shdr.SHN.SEC section;
                }
          | _ -> ())
        symbols)
    symtabs

module Rel = struct
  type t = { offset : int; kind : int; symbol : Sym.t; addend : int option }

  let read32 t symbols =
    ensure t 8 "Relocation entry truncated";
    let offset = Read.u32 t in
    let info = Read.u32 t in
    let kind = info land 0xff in
    let symbol_idx = info lsr 8 in
    { offset; kind; symbol = Array.get symbols symbol_idx; addend = None }

  let read64 t symbols =
    ensure t 16 "Relocation entry truncated";
    let offset = Read.u64 t in
    let info = Read.u64 t in
    let kind = info land 0xffffffff in
    let symbol_idx = info lsr 32 in
    { offset; kind; symbol = Array.get symbols symbol_idx; addend = None }

  let reada32 t symbols =
    ensure t 12 "Relocation entry truncated";
    { (read32 t symbols) with addend = Some (Read.s32 t) }

  let reada64 t symbols =
    ensure t 24 "Relocation entry truncated";
    { (read64 t symbols) with addend = Some (Read.s64 t) }

  let read (img : Img.t) (section : Shdr.t) =
    let cursor = cursor ~at:section.offset img.header.ident.data img.buf in
    let read =
      match (img.header.ident.kind, section.kind) with
      | `x32, REL -> read32
      | `x32, RELA -> reada32
      | `x64, REL -> read64
      | `x64, RELA -> reada64
      | _ -> invalid_format "Invalid ELF class"
    in
    let symbols = Array.get img.symtabs section.link in
    if Array.length symbols = 0 then [||]
    else
      Array.init (section.size / section.entsize) (fun _ -> read cursor symbols)
end

module R_386 = struct
  type t =
    | NONE
    | A32
    | PC32
    | GOT32
    | PLT32
    | COPY
    | GLOB_DAT
    | JUMP_SLOT
    | RELATIVE
    | GOTOFF
    | GOTPC
    | TLS_TPOFF
    | TLS_IE
    | TLS_GOTIE
    | TLS_LE
    | TLS_GD
    | TLS_LDM
    | A16
    | PC16
    | A8
    | PC8
    | TLS_GD_32
    | TLS_GD_PUSH
    | TLS_GD_CALL
    | TLS_GD_POP
    | TLS_LDM_32
    | TLS_LDM_PUSH
    | TLS_LDM_CALL
    | TLS_LDM_POP
    | TLS_LDO_32
    | TLS_IE_32
    | TLS_LE_32
    | TLS_DTPMOD32
    | TLS_DTPOFF32
    | TLS_TPOFF32
    | SIZE32
    | TLS_GOTDESC
    | TLS_DESC_CALL
    | TLS_DESC
    | IRELATIVE

  let of_u8 = function
    | 0x00 -> NONE
    | 0x01 -> A32
    | 0x02 -> PC32
    | 0x03 -> GOT32
    | 0x04 -> PLT32
    | 0x05 -> COPY
    | 0x06 -> GLOB_DAT
    | 0x07 -> JUMP_SLOT
    | 0x08 -> RELATIVE
    | 0x09 -> GOTOFF
    | 0x0a -> GOTPC
    | 0x0e -> TLS_TPOFF
    | 0x0f -> TLS_IE
    | 0x10 -> TLS_GOTIE
    | 0x11 -> TLS_LE
    | 0x12 -> TLS_GD
    | 0x13 -> TLS_LDM
    | 0x14 -> A16
    | 0x15 -> PC16
    | 0x16 -> A8
    | 0x17 -> PC8
    | 0x18 -> TLS_GD_32
    | 0x19 -> TLS_GD_PUSH
    | 0x1a -> TLS_GD_CALL
    | 0x1b -> TLS_GD_POP
    | 0x1c -> TLS_LDM_32
    | 0x1d -> TLS_LDM_PUSH
    | 0x1e -> TLS_LDM_CALL
    | 0x1f -> TLS_LDM_POP
    | 0x20 -> TLS_LDO_32
    | 0x21 -> TLS_IE_32
    | 0x22 -> TLS_LE_32
    | 0x23 -> TLS_DTPMOD32
    | 0x24 -> TLS_DTPOFF32
    | 0x25 -> TLS_TPOFF32
    | 0x26 -> SIZE32
    | 0x27 -> TLS_GOTDESC
    | 0x28 -> TLS_DESC_CALL
    | 0x29 -> TLS_DESC
    | 0x2a -> IRELATIVE
    | x -> raise @@ Invalid_argument (Printf.sprintf "0x%02x" x)

  let to_string = function
    | NONE -> "R_386_NONE"
    | A32 -> "R_386_32"
    | PC32 -> "R_386_PC32"
    | GOT32 -> "R_386_GOT32"
    | PLT32 -> "R_386_PLT32"
    | COPY -> "R_386_COPY"
    | GLOB_DAT -> "R_386_GLOB_DAT"
    | JUMP_SLOT -> "R_386_JUMP_SLOT"
    | RELATIVE -> "R_386_RELATIVE"
    | GOTOFF -> "R_386_GOTOFF"
    | GOTPC -> "R_386_GOTPC"
    | TLS_TPOFF -> "R_386_TLS_TPOFF"
    | TLS_IE -> "R_386_TLS_IE"
    | TLS_GOTIE -> "R_386_TLS_GOTIE"
    | TLS_LE -> "R_386_TLS_LE"
    | TLS_GD -> "R_386_TLS_GD"
    | TLS_LDM -> "R_386_TLS_LDM"
    | A16 -> "R_386_16"
    | PC16 -> "R_386_PC16"
    | A8 -> "R_386_8"
    | PC8 -> "R_386_PC8"
    | TLS_GD_32 -> "R_386_TLS_GD_32"
    | TLS_GD_PUSH -> "R_386_TLS_GD_PUSH"
    | TLS_GD_CALL -> "R_386_TLS_GD_CALL"
    | TLS_GD_POP -> "R_386_TLS_GD_POP"
    | TLS_LDM_32 -> "R_386_TLS_LDM_32"
    | TLS_LDM_PUSH -> "R_386_TLS_LDM_PUSH"
    | TLS_LDM_CALL -> "R_386_TLS_LDM_CALL"
    | TLS_LDM_POP -> "R_386_TLS_LDM_POP"
    | TLS_LDO_32 -> "R_386_TLS_LDO_32"
    | TLS_IE_32 -> "R_386_TLS_IE_32"
    | TLS_LE_32 -> "R_386_TLS_LE_32"
    | TLS_DTPMOD32 -> "R_386_TLS_DTPMOD32"
    | TLS_DTPOFF32 -> "R_386_TLS_DTPOFF32"
    | TLS_TPOFF32 -> "R_386_TLS_TPOFF32"
    | SIZE32 -> "R_386_SIZE32"
    | TLS_GOTDESC -> "R_386_TLS_GOTDESC"
    | TLS_DESC_CALL -> "R_386_TLS_DESC_CALL"
    | TLS_DESC -> "R_386_TLS_DESC"
    | IRELATIVE -> "R_386_IRELATIVE"

  let pp ppf t = Format.fprintf ppf "%s" (to_string t)

  let apply img section (rel : Rel.t) =
    let r_offset = section.Shdr.offset + rel.offset in
    let r_symbol = rel.symbol in
    let r_symbol_val = r_symbol.Sym.value in
    let r_type = of_u8 rel.kind in
    let cursor = cursor ~at:r_offset (Img.endian img) img.Img.buf in
    match (r_type, rel.addend) with
    | NONE, _ -> ()
    | A32, Some r_addend -> Write.u32 cursor @@ (r_symbol_val + r_addend)
    | A32, None ->
        let r_addend = Peek.s32 cursor in
        Write.u32 cursor @@ (r_symbol_val + r_addend)
    | PC32, Some r_addend ->
        Write.u32 cursor @@ (r_symbol_val + r_addend - r_offset)
    | PC32, None ->
        let r_addend = Peek.s32 cursor in
        Write.u32 cursor @@ (r_symbol_val + r_addend - rel.offset)
    | t, _ ->
        Elf_options.Logger.warning "non supported %a, relocation is ignored" pp
          t
end

let r_apply = function
  | Machine.X86 { bits = `x32 } -> R_386.apply
  | isa ->
      Elf_options.Logger.warning "Relocation for %a is not supported" Machine.pp
        isa;
      fun _ _ _ -> ()

let reloc img =
  Array.iter
    (fun section ->
      match section.Shdr.kind with
      | (Shdr.SHT.REL | Shdr.SHT.RELA) when section.Shdr.link = 0 ->
          Elf_options.Logger.warning
            "non supported relocations without symbols (Lk=0) in section %s"
            section.Shdr.name
      | Shdr.SHT.REL | Shdr.SHT.RELA ->
          Array.iter
            (r_apply (Img.arch img) img img.Img.sections.(section.Shdr.info))
            (Rel.read img section)
      | _ -> ())
    img.Img.sections

let load buf =
  let t, e_ident = E_ident.init_cursor buf in
  let header = Ehdr.read t e_ident in
  let sections = Shdr.read_all t header in
  let last_section =
    if Array.length sections = 0 then Shdr.dummy else Array.get sections 0
  in
  let phdrs = Phdr.read_all t header in
  let last_phdr =
    if Array.length phdrs = 0 then Phdr.dummy else Array.get phdrs 0
  in
  let symtabs = Sym.read_all t header sections in
  let header, vendor, sections, symtabs, buf =
    Vendor.read header sections symtabs buf
  in
  let notes =
    Array.fold_left
      (fun notes s ->
        if s.Shdr.kind = Shdr.SHT.NOTE then (
          seek t s.Shdr.offset;
          Note.append_all notes (s.Shdr.offset + s.Shdr.size) t)
        else notes)
      [] sections
  in
  let notes = Array.of_list notes in
  let img =
    {
      Img.header;
      vendor;
      sections;
      last_section;
      symtabs;
      buf;
      phdrs;
      last_phdr;
      notes;
    }
  in
  if Elf_options.Alloc.get () then alloc img;
  if Elf_options.Reloc.get () then reloc img;
  img

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

let read_offset i offset = i.Img.buf.{offset}

let find_section_by_addr_with_cache (i : Img.t) addr =
  if Shdr.contains addr i.last_section then i.last_section
  else
    let s = Array_utils.find (Shdr.contains addr) i.sections in
    i.last_section <- s;
    s

let find_programme_header_by_addr_with_cache (i : Img.t) addr =
  if Phdr.contains addr i.last_phdr then i.last_phdr
  else
    let p = Array_utils.find (Phdr.contains addr) i.phdrs in
    i.last_phdr <- p;
    p

let read_address i addr =
  try
    if Array.length i.Img.phdrs > 0 then
      let h = find_programme_header_by_addr_with_cache i addr in
      let offset = addr - h.Phdr.vaddr in
      if offset > h.Phdr.filesz then 0 else i.Img.buf.{h.Phdr.offset + offset}
    else
      let s = find_section_by_addr_with_cache i addr in
      if s.Shdr.kind = Shdr.SHT.NOBITS then 0
      else i.Img.buf.{addr - s.Shdr.addr + s.Shdr.offset}
  with Not_found ->
    let msg = Format.sprintf "Unreachable virtual address %x" addr in
    invalid_arg msg

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

let program_headers i = i.Img.phdrs
let notes = Img.notes

type fmap = {
  addresses : Virtual_address.t Interval.t;
  offset : int;
  mutable name : string;
}

let fmap addresses offset name = { addresses; offset; name }

let files (i : Img.t) =
  let read =
    match i.header.ident.kind with
    | `x32 -> Loader_buf.Read.u32
    | `x64 -> Loader_buf.Read.u64
  in
  Array.fold_left
    (fun result -> function
      | { Note.name = "CORE"; kind = 0x46494c45; offset = at; _ } ->
          let cursor = Img.cursor ~at i in
          let n = read cursor in
          let ps = read cursor in
          let files =
            Array.init n (fun _ ->
                let lo = Virtual_address.create (read cursor) in
                let hi = Virtual_address.create (read cursor) in
                let offset = ps * read cursor in
                { addresses = { Interval.lo; hi }; offset; name = "" })
          in
          Array.iter
            (fun fmap ->
              fmap.name <- Loader_buf.Read.zero_string "files" cursor ())
            files;
          files
      | _ -> result)
    [||] i.notes

module Utils = struct
  let is_ifunc (img : Img.t) (sym : Sym.t) =
    match img.vendor with
    | GNU _ -> ( match sym.kind with OS 10 -> true | _ -> false)
    | _ -> false
end
