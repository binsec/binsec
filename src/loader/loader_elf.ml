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
  (not (Bigarray.Array1.dim buffer < 4))
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
    if Bigarray.Array1.dim buffer < 16 then
      invalid_format "Identification truncated";
    let kind = E_class.of_u8 buffer.{4} in
    let data = endian buffer.{5} in
    let version = Int.unsafe_to_uint8 buffer.{6} in
    let osabi = Int.unsafe_to_uint8 buffer.{7} in
    let abiversion = Int.unsafe_to_uint8 buffer.{8} in
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
    (of_bigarray ~pos:16 ~endianness:ident.data buffer, ident)
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
    entry : Virtual_address.t;
    phoff : int;
    shoff : int;
    flags : u32;
    ehsize : u16;
    phentsize : int;
    phnum : int;
    shentsize : int;
    shnum : int;
    shstrndx : int;
  }

  let arch endianness (mode : [ `x64 | `x32 ]) entry = function
    (* | 0x02 -> Machine.SPARC *)
    | 0x03 -> Machine.x86
    (* | 0x08 -> Machine.MIPS
     * | 0x0a -> Machine.MIPS *)
    | 0x12 -> Machine.sparcv8
    (* | 0x14 -> Machine.PowerPC *)
    | 0x15 -> Machine.ppc64 endianness
    | 0x28 ->
        Machine.armv7 endianness
          ~thumb:(if Virtual_address.modi entry 2 = 0 then False else True)
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
    let kind = ET.of_u16 (Uint16.to_int (Read.u16 t)) in
    let machine = Uint16.to_int (Read.u16 t) in
    let version = Read.u32 t in
    let entry = Virtual_address.of_uint32 (Read.u32 t) in
    let machine = arch ident.E_ident.data ident.E_ident.kind entry machine in
    let phoff = Uint32.to_int (Read.u32 t) in
    let shoff = Uint32.to_int (Read.u32 t) in
    let flags = Read.u32 t in
    let ehsize = Read.u16 t in
    let phentsize = Uint16.to_int (Read.u16 t) in
    let phnum = Uint16.to_int (Read.u16 t) in
    let shentsize = Uint16.to_int (Read.u16 t) in
    let shnum = Uint16.to_int (Read.u16 t) in
    let shstrndx = Uint16.to_int (Read.u16 t) in
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
    let kind = ET.of_u16 (Uint16.to_int (Read.u16 t)) in
    let machine = Uint16.to_int (Read.u16 t) in
    let version = Read.u32 t in
    let entry = Virtual_address.of_uint64 (Read.u64 t) in
    let machine = arch ident.E_ident.data ident.E_ident.kind entry machine in
    let phoff = Uint64.to_int (Read.u64 t) in
    let shoff = Uint64.to_int (Read.u64 t) in
    let flags = Read.u32 t in
    let ehsize = Read.u16 t in
    let phentsize = Uint16.to_int (Read.u16 t) in
    let phnum = Uint16.to_int (Read.u16 t) in
    let shentsize = Uint16.to_int (Read.u16 t) in
    let shnum = Uint16.to_int (Read.u16 t) in
    let shstrndx = Uint16.to_int (Read.u16 t) in
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
          [|
            "Entry point address:";
            Format.asprintf "%a" Virtual_address.pp h.entry;
          |];
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
      | OS of int32
      | PROC of int32
      | USER of int32

    let of_u32 = function
      | 0l -> NULL
      | 1l -> PROGBITS
      | 2l -> SYMTAB
      | 3l -> STRTAB
      | 4l -> RELA
      | 5l -> HASH
      | 6l -> DYNAMIC
      | 7l -> NOTE
      | 8l -> NOBITS
      | 9l -> REL
      | 10l -> SHLIB
      | 11l -> DYNSYM
      | 14l -> INIT_ARRAY
      | 15l -> FINI_ARRAY
      | 16l -> PREINIT_ARRAY
      | 17l -> GROUP
      | 18l -> SYMTAB_SHNDX
      | 19l -> RELR
      | t when 0x60000000l <= t && t < 0x70000000l -> OS t
      | t when 0x70000000l <= t (* < 0x80000000l *) -> PROC t
      | t when t < 0l (* 0x80000000l <= t <= 0xffffffffl *) -> USER t
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
      | PROC t -> Format.fprintf ppf "PROC(%08lx)" t
      | USER t -> Format.fprintf ppf "USER(%08lx)" t

    let pp = ppvx (fun ppf -> Format.fprintf ppf "OS(%08lx)")
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

    let is f f' =
      let f = Uint64.to_int64 f in
      match f' with
      | WRITE -> Int64.logand f 0x1L <> 0L
      | ALLOC -> Int64.logand f 0x2L <> 0L
      | EXECINSTR -> Int64.logand f 0x4L > 0L
      | MERGE -> Int64.logand f 0x10L > 0L
      | STRINGS -> Int64.logand f 0x20L > 0L
      | INFO_LINK -> Int64.logand f 0x40L > 0L
      | LINK_ORDER -> Int64.logand f 0x80L > 0L
      | OS_NONCONFORMING -> Int64.logand f 0x100L > 0L
      | GROUP -> Int64.logand f 0x200L > 0L
      | TLS -> Int64.logand f 0x400L > 0L
      | OS -> Int64.logand f 0x0ff00000L > 0L
      | PROC -> Int64.logand f 0xf0000000L > 0L

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
    addr : Virtual_address.t;
    offset : int;
    size : int;
    link : int;
    info : u32;
    addralign : u64;
    entsize : int;
  }

  let dummy =
    {
      idx = 0;
      name = "";
      kind = SHT.NULL;
      flags = Int64.to_uint64 0L;
      addr = Virtual_address.zero;
      offset = 0;
      size = 0;
      link = 0;
      info = Int32.to_uint32 0l;
      addralign = Int64.to_uint64 0L;
      entsize = 0;
    }

  let read_32 t =
    ensure t 40 "Section header truncated";
    let idx = Uint32.to_int (Read.u32 t) in
    let kind = SHT.of_u32 (Uint32.to_int32 (Read.u32 t)) in
    let flags = Uint32.to_uint64 (Read.u32 t) in
    let addr = Virtual_address.of_uint32 (Read.u32 t) in
    let offset = Uint32.to_int (Read.u32 t) in
    let size = Uint32.to_int (Read.u32 t) in
    let link = Uint32.to_int (Read.u32 t) in
    let info = Read.u32 t in
    let addralign = Uint32.to_uint64 (Read.u32 t) in
    let entsize = Uint32.to_int (Read.u32 t) in
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
    let idx = Uint32.to_int (Read.u32 t) in
    let kind = SHT.of_u32 (Uint32.to_int32 (Read.u32 t)) in
    let flags = Read.u64 t in
    let addr = Virtual_address.of_uint64 (Read.u64 t) in
    let offset = Uint64.to_int (Read.u64 t) in
    let size = Uint64.to_int (Read.u64 t) in
    let link = Uint32.to_int (Read.u32 t) in
    let info = Read.u32 t in
    let addralign = Read.u64 t in
    let entsize = Uint64.to_int (Read.u64 t) in
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
    set_pos t Ehdr.(header.shoff + (n * header.shentsize));
    match header.Ehdr.ident.E_ident.kind with
    | `x32 -> read_32 t
    | `x64 -> read_64 t

  let with_name t shstrndx shdr =
    let n = shdr.idx in
    set_pos t (shstrndx.offset + n);
    Read.zero_string "Unterminated section name" t ~maxlen:(shstrndx.size - n)
      ()

  let read_all t header =
    if header.Ehdr.shnum = 0 then [||]
    else
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
    && addr < Virtual_address.add_int section.size section.addr

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

  let ppvx_all (aformat : E_class.t) vformat ppf sections =
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
              (Virtual_address.pp_print (aformat :> [ `x16 | `x32 | `x64 ]))
              section.addr;
            Printf.sprintf "%06x" section.offset;
            Printf.sprintf "%06x" section.size;
            Printf.sprintf "%02x" section.entsize;
            Format.asprintf "%a" SHF.pp section.flags;
            string_of_int section.link;
            Z.to_string (Uint32.to_bigint section.info);
            Z.to_string (Uint64.to_bigint section.addralign);
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
      | n when 0x0000 < n && n < 0xff00 -> SEC (sections n)
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
    value : Virtual_address.t;
    size : int;
  }

  let read_name t (strtab : Shdr.t) idx =
    set_pos t (strtab.offset + idx);
    Read.zero_string "Unterminated symbol name" t ~maxlen:(strtab.size - idx) ()

  let read_32 t sections names =
    ensure t 16 "Symbol header truncated";
    let idx = Uint32.to_int (Read.u32 t) in
    let value = Virtual_address.of_uint32 (Read.u32 t) in
    let size = Uint32.to_int (Read.u32 t) in
    let info = Uint8.to_int (Read.u8 t) in
    let kind = STT.of_u8 info in
    let bind = STB.of_u8 info in
    let other = Read.u8 t in
    let sh = Shdr.SHN.of_u16 sections (Uint16.to_int (Read.u16 t)) in
    { name = names idx; kind; bind; other; sh; value; size }

  let read_64 t sections names =
    ensure t 24 "Symbol header truncated";
    let idx = Uint32.to_int (Read.u32 t) in
    let info = Uint8.to_int (Read.u8 t) in
    let kind = STT.of_u8 info in
    let bind = STB.of_u8 info in
    let other = Read.u8 t in
    let sh = Shdr.SHN.of_u16 sections (Uint16.to_int (Read.u16 t)) in
    let value = Virtual_address.of_uint64 (Read.u64 t) in
    let size = Uint64.to_int (Read.u64 t) in
    { name = names idx; kind; bind; other; sh; value; size }

  let read t header sections symtab strtab n =
    set_pos t Shdr.(symtab.offset + (n * symtab.entsize));
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
              (read t header (Array.get sections) section
                 (read_name t sections.(section.Shdr.link)))
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

  let ppvx_all (iformat : E_class.t) vformat ppf symbols =
    let t = Prettytbl.make pretty_formats in
    Prettytbl.append t pretty_names;
    Array.iteri
      (fun i symbol ->
        Prettytbl.append t
          [|
            string_of_int i;
            Format.asprintf "%a"
              (Virtual_address.pp_print (iformat :> [ `x16 | `x32 | `x64 ]))
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
  module PHT = struct
    type t =
      | NULL
      | LOAD
      | DYNAMIC
      | INTERP
      | NOTE
      | SHLIB
      | PHDR
      | TLS
      | OS of int32
      | PROC of int32

    let of_u32 = function
      | 0l -> NULL
      | 1l -> LOAD
      | 2l -> DYNAMIC
      | 3l -> INTERP
      | 4l -> NOTE
      | 5l -> SHLIB
      | 6l -> PHDR
      | 7l -> TLS
      | t when 0x60000000l <= t && t < 0x70000000l -> OS t
      | t when 0x70000000l <= t (* < 0x80000000l *) -> PROC t
      | _ -> raise @@ Invalid_argument "Not a valid segment type"

    let ppvx vformat ppf = function
      | NULL -> Format.fprintf ppf "NULL"
      | LOAD -> Format.fprintf ppf "LOAD"
      | DYNAMIC -> Format.fprintf ppf "DYNAMIC"
      | INTERP -> Format.fprintf ppf "INTERP"
      | NOTE -> Format.fprintf ppf "NOTE"
      | SHLIB -> Format.fprintf ppf "SHLIB"
      | PHDR -> Format.fprintf ppf "PHDR"
      | TLS -> Format.fprintf ppf "TLS"
      | OS t -> vformat ppf t
      | PROC t -> Format.fprintf ppf "PROC(%08lx)" t

    let pp = ppvx (fun ppf -> Format.fprintf ppf "OS(%08lx)")
  end

  module PHF = struct
    type t = X | W | R | OS of int32 | PROC of int32

    let is f f' =
      let f = Uint32.to_int32 f in
      match f' with
      | X -> Int32.logand f 0x1l <> 0l
      | W -> Int32.logand f 0x2l <> 0l
      | R -> Int32.logand f 0x4l > 0l
      | OS _ -> Int32.logand f 0x0ff00000l > 0l
      | PROC _ -> Int32.logand f 0xf0000000l > 0l

    let repr = function
      | X -> 'E'
      | W -> 'W'
      | R -> 'R'
      | OS _ -> 'o'
      | PROC _ -> 'p'

    let ifpp t ppf f = Format.pp_print_char ppf (if is f t then repr t else ' ')

    let pp ppf f =
      ifpp R ppf f;
      ifpp W ppf f;
      ifpp X ppf f
  end

  (* ELF program header *)
  type t = {
    kind : PHT.t;
    flags : u32;
    offset : int;
    vaddr : Virtual_address.t;
    paddr : u64;
    filesz : int;
    memsz : u64;
    align : u64;
  }

  let dummy =
    {
      kind = NULL;
      flags = Int32.to_uint32 0l;
      offset = 0;
      vaddr = Virtual_address.zero;
      paddr = Int64.to_uint64 0L;
      filesz = 0;
      memsz = Int64.to_uint64 0L;
      align = Int64.to_uint64 0L;
    }

  let read_32 t =
    ensure t 32 "Program header truncated";
    let kind = PHT.of_u32 (Uint32.to_int32 (Read.u32 t)) in
    let offset = Uint32.to_int (Read.u32 t) in
    let vaddr = Virtual_address.of_uint32 (Read.u32 t) in
    let paddr = Uint32.to_uint64 (Read.u32 t) in
    let filesz = Uint32.to_int (Read.u32 t) in
    let memsz = Uint32.to_uint64 (Read.u32 t) in
    let flags = Read.u32 t in
    let align = Uint32.to_uint64 (Read.u32 t) in
    { kind; flags; offset; vaddr; paddr; filesz; memsz; align }

  let read_64 t =
    ensure t 56 "Program header truncated";
    let kind = PHT.of_u32 (Uint32.to_int32 (Read.u32 t)) in
    let flags = Read.u32 t in
    let offset = Uint64.to_int (Read.u64 t) in
    let vaddr = Virtual_address.of_uint64 (Read.u64 t) in
    let paddr = Read.u64 t in
    let filesz = Uint64.to_int (Read.u64 t) in
    let memsz = Read.u64 t in
    let align = Read.u64 t in
    { kind; flags; offset; vaddr; paddr; filesz; memsz; align }

  let read t header n =
    set_pos t Ehdr.(header.phoff + (n * header.phentsize));
    match header.Ehdr.ident.E_ident.kind with
    | `x32 -> read_32 t
    | `x64 -> read_64 t

  let read_all t header = Array.init header.Ehdr.phnum (read t header)

  let contains addr pheader =
    addr >= pheader.vaddr
    && addr
       < Virtual_address.add_bigint
           (Uint64.to_bigint pheader.memsz)
           pheader.vaddr

  let pretty_formats32, pretty_formats64, pretty_names =
    ( [|
        Prettytbl.(Column.make ~max_length:16 ());
        Prettytbl.(Column.make ~max_length:8 ());
        Prettytbl.(Column.make ~max_length:10 ());
        Prettytbl.(Column.make ~max_length:10 ());
        Prettytbl.(Column.make ~max_length:8 ());
        Prettytbl.(Column.make ~max_length:8 ());
        Prettytbl.(Column.make ~max_length:3 ~align:R ());
        Prettytbl.(Column.make ~max_length:6 ~align:R ());
      |],
      [|
        Prettytbl.(Column.make ~max_length:16 ());
        Prettytbl.(Column.make ~max_length:8 ());
        Prettytbl.(Column.make ~max_length:18 ());
        Prettytbl.(Column.make ~max_length:18 ());
        Prettytbl.(Column.make ~max_length:8 ());
        Prettytbl.(Column.make ~max_length:8 ());
        Prettytbl.(Column.make ~max_length:3 ());
        Prettytbl.(Column.make ~max_length:6 ());
      |],
      [|
        "Type";
        "Offset";
        "VirtAddr";
        "PhysAddr";
        "FileSiz";
        "MemSiz";
        "Flg";
        "Align";
      |] )

  let ppvx_all (aformat : E_class.t) vformat ppf segments =
    let t =
      Prettytbl.make
        (match aformat with
        | `x32 -> pretty_formats32
        | `x64 -> pretty_formats64)
    in
    Prettytbl.append t pretty_names;
    Array.iter
      (fun segment ->
        Prettytbl.append t
          [|
            Format.asprintf "%a" (PHT.ppvx vformat) segment.kind;
            Printf.sprintf "%#08x" segment.offset;
            Format.asprintf "0x%a"
              (Virtual_address.pp_print (aformat :> [ `x16 | `x32 | `x64 ]))
              segment.vaddr;
            Format.asprintf "0x%a"
              (Virtual_address.pp_print (aformat :> [ `x16 | `x32 | `x64 ]))
              (Virtual_address.of_uint64 segment.paddr);
            Printf.sprintf "%#08x" segment.filesz;
            Printf.sprintf "%#08Lx" (Uint64.to_int64 segment.memsz);
            Format.asprintf "%a" PHF.pp segment.flags;
            Printf.sprintf "%#Lx" (Uint64.to_int64 segment.align);
          |])
      segments;
    Format.fprintf ppf "@[<v 2>Program Headers:@ ";
    Prettytbl.pp ppf t;
    Format.pp_close_box ppf ()
end

module Section = struct
  type t = Shdr.t
  type header = t

  let name s = s.Shdr.name
  let flag s = s.Shdr.flags
  let pos s = { raw = s.Shdr.offset; virt = s.Shdr.addr }

  let size s =
    let raw = if Shdr.(s.kind = SHT.NOBITS) then 0 else s.Shdr.size in
    { raw; virt = Z.of_int s.Shdr.size }

  let header : t -> header = Fun.id

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
    let base = get_pos t in
    let namesz = Uint32.to_int (Read.u32 t) in
    let size = Uint32.to_int (Read.u32 t) in
    let kind = Uint32.to_int (Read.u32 t) in
    let name = Read.fixed_string t namesz in
    set_pos t (base + ((get_pos t - base + 3) land -4));
    let offset = get_pos t in
    advance t ((size + 3) land -4);
    { name; kind; offset; size }

  let rec append_all notes bound cursor =
    if get_pos cursor >= bound then notes
    else append_all (read cursor :: notes) bound cursor
end

module rec Vendor : sig
  type t = Unknown | GNU of string

  include Sigs.PRINTABLE with type t := t

  val pretty_rows : t -> string array array

  module Section : sig
    val ppt : t -> Format.formatter -> int32 -> unit
  end

  module Segment : sig
    val ppt : t -> Format.formatter -> int32 -> unit
  end

  module Symbol : sig
    val ppt : t -> Format.formatter -> int -> unit
  end

  val read :
    Ehdr.t ->
    Shdr.t array ->
    Sym.t array array ->
    buffer ->
    Ehdr.t * t * Shdr.t array * Sym.t array array * buffer
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
    let ppt8 ppf x = Format.fprintf ppf "OS(%08lx)" x
  end

  module GNU = struct
    module Section = struct
      let ppt ppf = function
        | 0x6ffffff6l -> Format.fprintf ppf "GNU_HASH"
        | 0x6ffffffdl -> Format.fprintf ppf "VERDEF"
        | 0x6ffffffel -> Format.fprintf ppf "VERNEED"
        | 0x6fffffffl -> Format.fprintf ppf "VERSYM"
        | t when 0x60000000l <= t && t < 0x70000000l ->
            Format.fprintf ppf "OS(%08lx)" t
        | _ -> raise @@ Invalid_argument "Not a vendor specific type"
    end

    module Segment = struct
      let ppt ppf = function
        | 0x6474e550l -> Format.fprintf ppf "GNU_EH_FRAME"
        | 0x6474e551l -> Format.fprintf ppf "GNU_STACK"
        | 0x6474e552l -> Format.fprintf ppf "GNU_RELRO"
        | 0x6474e553l -> Format.fprintf ppf "GNU_PROPERTY"
        | t when 0x60000000l <= t && t < 0x70000000l ->
            Format.fprintf ppf "OS(%08lx)" t
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

  module Segment = struct
    let ppt = function Unknown -> Common.ppt8 | GNU _ -> GNU.Segment.ppt
  end

  module Symbol = struct
    let ppt = function Unknown -> Common.ppt | GNU _ -> GNU.Symbol.ppt
  end

  let read header sections symbols buf =
    match Shdr.find_by_name sections ".note.ABI-tag" with
    | None -> (header, Unknown, sections, symbols, buf)
    | Some note ->
        let cursor =
          of_bigarray ~pos:note.Shdr.offset
            ~endianness:header.Ehdr.ident.E_ident.data buf
        in
        let namesz = Uint32.to_int (Read.u32 cursor) in
        let descsz = Uint32.to_int (Read.u32 cursor) in
        let kind = Uint32.to_int (Read.u32 cursor) in
        if kind = 1 then
          let name =
            Read.zero_string "Inconsistent note format" cursor ~maxlen:namesz ()
          in
          if name = "GNU" && descsz = 16 then (
            let padding = ((namesz lxor 0b11) + 1) land 0b11 in
            advance cursor padding;
            if Read.i32 cursor = 0l then
              let version = Uint32.to_int (Read.u32 cursor) in
              let major = Uint32.to_int (Read.u32 cursor) in
              let minor = Uint32.to_int (Read.u32 cursor) in
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
    buf : buffer;
    phdrs : Phdr.t array;
    mutable last_phdr : Phdr.t;
    notes : Note.t array;
  }

  type header = Ehdr.t

  val arch : t -> Machine.t
  val entry : t -> Virtual_address.t

  (* val endian : t -> Machine.endianness *)
  val sections : t -> Section.t array
  val symbols : t -> Symbol.t array
  val notes : t -> Note.t array
  val header : t -> header
  val cursor : ?at:int -> t -> int Reader.t
  val content : t -> Section.t -> buffer
  val buffer : t -> buffer

  include Sigs.PRINTABLE with type t := t
end = struct
  type t = {
    header : Ehdr.t;
    vendor : Vendor.t;
    sections : Shdr.t array;
    mutable last_section : Shdr.t;
    symtabs : Sym.t array array;
    buf : buffer;
    phdrs : Phdr.t array;
    mutable last_phdr : Phdr.t;
    notes : Note.t array;
  }

  type header = Ehdr.t

  let arch i = i.header.Ehdr.machine
  let entry i = i.header.Ehdr.entry

  (* let endian i = i.header.Ehdr.ident.E_ident.data *)
  let sections i = Array.copy i.sections
  let symbols i = Array.concat @@ Array.to_list i.symtabs
  let notes i = i.notes
  let header i = i.header

  let cursor ?at i =
    of_bigarray ?pos:at ~endianness:i.header.Ehdr.ident.E_ident.data i.buf

  let content i (s : Shdr.t) =
    if s.kind = Shdr.SHT.NOBITS then Bigarray.Array1.sub i.buf 0 0
    else Bigarray.Array1.sub i.buf s.Shdr.offset s.size

  let buffer { buf; _ } = buf

  let pp ppf t =
    let e_class = t.header.Ehdr.ident.E_ident.kind in
    let vrows = Vendor.pretty_rows t.vendor in
    Format.fprintf ppf "@[<v>%a@ @ %a@ @ " (Ehdr.ppvx vrows) t.header
      (Shdr.ppvx_all e_class (Vendor.Section.ppt t.vendor))
      t.sections;
    if Array.length t.phdrs <> 0 then (
      Phdr.ppvx_all e_class (Vendor.Segment.ppt t.vendor) ppf t.phdrs;
      Format.pp_print_space ppf ();
      Format.pp_print_space ppf ());
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

module Rel = struct
  type t = {
    offset : Virtual_address.t;
    kind : int;
    symbol : Sym.t;
    addend : Z.t option;
  }

  let read32 t symbols =
    ensure t 8 "Relocation entry truncated";
    let offset = Virtual_address.of_uint32 (Read.u32 t) in
    let info = Uint32.to_int (Read.u32 t) in
    let kind = info land 0xff in
    let symbol_idx = info lsr 8 in
    { offset; kind; symbol = symbols symbol_idx; addend = None }

  let read64 t symbols =
    ensure t 16 "Relocation entry truncated";
    let offset = Virtual_address.of_uint64 (Read.u64 t) in
    let info = Uint64.to_int64 (Read.u64 t) in
    let kind = Int64.to_int (Int64.logand info 0xffffffffL) in
    let symbol_idx = Int64.to_int (Int64.shift_right_logical info 32) in
    { offset; kind; symbol = symbols symbol_idx; addend = None }

  let reada32 t symbols =
    ensure t 12 "Relocation entry truncated";
    { (read32 t symbols) with addend = Some (Int32.to_bigint (Read.i32 t)) }

  let reada64 t symbols =
    ensure t 24 "Relocation entry truncated";
    { (read64 t symbols) with addend = Some (Int64.to_bigint (Read.i64 t)) }

  let read (img : Img.t) (section : Shdr.t) =
    let cursor =
      of_bigarray ~pos:section.offset ~endianness:img.header.ident.data img.buf
    in
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
      Array.init (section.size / section.entsize) (fun _ ->
          read cursor (Array.get symbols))
end

module Dynamic = struct
  module DT = struct
    type t =
      | NULL
      | NEEDED
      | PLTRELSZ
      | PLTGOT
      | HASH
      | STRTAB
      | SYMTAB
      | RELA
      | RELASZ
      | RELAENT
      | STRSZ
      | SYMENT
      | INIT
      | FINI
      | SONAME
      | RPATH
      | SYMBOLIC
      | REL
      | RELSZ
      | RELENT
      | PLTREL
      | DEBUG
      | TEXTREL
      | JMPREL
      | BIND_NOW
      | INIT_ARRAY
      | FINI_ARRAY
      | INIT_ARRAYSZ
      | FINI_ARRAYSZ
      | RUNPATH
      | FLAGS
      | ENCODING
      | PREINIT_ARRAY
      | PREINIT_ARRAYSZ
      | Os of int32
      | VALRNGLO
      | CHECKSUM
      | PLTPADSZ
      | MOVEENT
      | MOVESZ
      | FEATURE_1
      | POSFLAG_1
      | SYMINSZ
      | SYMINENT
      | GNU_HASH
      | VALRNGHI
      | ADDRRNGLO
      | CONFIG
      | DEPAUDIT
      | AUDIT
      | PLTPAD
      | MOVETAB
      | SYMINFO
      | ADDRRNGHI
      | VERSYM
      | RELACOUNT
      | RELCOUNT
      | FLAGS_1
      | VERDEF
      | VERDEFNUM
      | VERNEED
      | VERNEEDNUM
      | Proc of int32
      | Unknown of int32

    let of_int32 : int32 -> t = function
      | 0l -> NULL
      | 1l -> NEEDED
      | 2l -> PLTRELSZ
      | 3l -> PLTGOT
      | 4l -> HASH
      | 5l -> STRTAB
      | 6l -> SYMTAB
      | 7l -> RELA
      | 8l -> RELASZ
      | 9l -> RELAENT
      | 10l -> STRSZ
      | 11l -> SYMENT
      | 12l -> INIT
      | 13l -> FINI
      | 14l -> SONAME
      | 15l -> RPATH
      | 16l -> SYMBOLIC
      | 17l -> REL
      | 18l -> RELSZ
      | 19l -> RELENT
      | 20l -> PLTREL
      | 21l -> DEBUG
      | 22l -> TEXTREL
      | 23l -> JMPREL
      | 24l -> BIND_NOW
      | 25l -> INIT_ARRAY
      | 26l -> FINI_ARRAY
      | 27l -> INIT_ARRAYSZ
      | 28l -> FINI_ARRAYSZ
      | 29l -> RUNPATH
      | 30l -> FLAGS
      | 32l -> PREINIT_ARRAY
      | 33l -> PREINIT_ARRAYSZ
      | x when 0x6000000dl <= x && x < 0x6ffff000l -> Os x
      | 0x6ffffd00l -> VALRNGLO
      | 0x6ffffdf8l -> CHECKSUM
      | 0x6ffffdf9l -> PLTPADSZ
      | 0x6ffffdfal -> MOVEENT
      | 0x6ffffdfbl -> MOVESZ
      | 0x6ffffdfcl -> FEATURE_1
      | 0x6ffffdfdl -> POSFLAG_1
      | 0x6ffffdfel -> SYMINSZ
      | 0x6ffffdffl -> SYMINENT
      | 0x6ffffe00l -> ADDRRNGLO
      | 0x6ffffef5l -> GNU_HASH
      | 0x6ffffefal -> CONFIG
      | 0x6ffffefbl -> DEPAUDIT
      | 0x6ffffefcl -> AUDIT
      | 0x6ffffefdl -> PLTPAD
      | 0x6ffffefel -> MOVETAB
      | 0x6ffffeffl -> SYMINFO
      | 0x6ffffff0l -> VERSYM
      | 0x6ffffff9l -> RELACOUNT
      | 0x6ffffffal -> RELCOUNT
      | 0x6ffffffbl -> FLAGS_1
      | 0x6ffffffcl -> VERDEF
      | 0x6ffffffdl -> VERDEFNUM
      | 0x6ffffffel -> VERNEED
      | 0x6fffffffl -> VERNEEDNUM
      | x when 0x70000000l < x (* && x < 0x7fffffffl *) -> Proc x
      | x -> Unknown x

    module Map = Map.Make (struct
      type nonrec t = t

      let compare = compare
    end)

    let pp ppf = function
      | NULL -> Format.pp_print_string ppf "DT_NULL"
      | NEEDED -> Format.pp_print_string ppf "DT_NEEDED"
      | PLTRELSZ -> Format.pp_print_string ppf "DT_PLTRELSZ"
      | PLTGOT -> Format.pp_print_string ppf "DT_PLTGOT"
      | HASH -> Format.pp_print_string ppf "DT_HASH"
      | STRTAB -> Format.pp_print_string ppf "DT_STRTAB"
      | SYMTAB -> Format.pp_print_string ppf "DT_SYMTAB"
      | RELA -> Format.pp_print_string ppf "DT_RELA"
      | RELASZ -> Format.pp_print_string ppf "DT_RELASZ"
      | RELAENT -> Format.pp_print_string ppf "DT_RELAENT"
      | STRSZ -> Format.pp_print_string ppf "DT_STRSZ"
      | SYMENT -> Format.pp_print_string ppf "DT_SYMENT"
      | INIT -> Format.pp_print_string ppf "DT_INIT"
      | FINI -> Format.pp_print_string ppf "DT_FINI"
      | SONAME -> Format.pp_print_string ppf "DT_SONAME"
      | RPATH -> Format.pp_print_string ppf "DT_RPATH"
      | SYMBOLIC -> Format.pp_print_string ppf "DT_SYMBOLIC"
      | REL -> Format.pp_print_string ppf "DT_REL"
      | RELSZ -> Format.pp_print_string ppf "DT_RELSZ"
      | RELENT -> Format.pp_print_string ppf "DT_RELENT"
      | PLTREL -> Format.pp_print_string ppf "DT_PLTREL"
      | DEBUG -> Format.pp_print_string ppf "DT_DEBUG"
      | TEXTREL -> Format.pp_print_string ppf "DT_TEXTREL"
      | JMPREL -> Format.pp_print_string ppf "DT_JMPREL"
      | BIND_NOW -> Format.pp_print_string ppf "DT_BIND_NOW"
      | INIT_ARRAY -> Format.pp_print_string ppf "DT_INIT_ARRAY"
      | FINI_ARRAY -> Format.pp_print_string ppf "DT_FINI_ARRAY"
      | INIT_ARRAYSZ -> Format.pp_print_string ppf "DT_INIT_ARRAYSZ"
      | FINI_ARRAYSZ -> Format.pp_print_string ppf "DT_FINI_ARRAYSZ"
      | RUNPATH -> Format.pp_print_string ppf "DT_RUNPATH"
      | FLAGS -> Format.pp_print_string ppf "DT_FLAGS"
      | ENCODING -> Format.pp_print_string ppf "DT_ENCODING"
      | PREINIT_ARRAY -> Format.pp_print_string ppf "DT_PREINIT_ARRAY"
      | PREINIT_ARRAYSZ -> Format.pp_print_string ppf "DT_PREINIT_ARRAYSZ"
      | Os x -> Format.fprintf ppf "DT_OS(%lx)" x
      | VALRNGLO -> Format.pp_print_string ppf "DT_VALRNGLO"
      | CHECKSUM -> Format.pp_print_string ppf "DT_CHECKSUM"
      | PLTPADSZ -> Format.pp_print_string ppf "DT_PLTPADSZ"
      | MOVEENT -> Format.pp_print_string ppf "DT_MOVEENT"
      | MOVESZ -> Format.pp_print_string ppf "DT_MOVESZ"
      | FEATURE_1 -> Format.pp_print_string ppf "DT_FEATURE_1"
      | POSFLAG_1 -> Format.pp_print_string ppf "DT_POSFLAG_1"
      | SYMINSZ -> Format.pp_print_string ppf "DT_SYMINSZ"
      | SYMINENT -> Format.pp_print_string ppf "DT_SYMINENT"
      | GNU_HASH -> Format.pp_print_string ppf "DT_GNU_HASH"
      | VALRNGHI -> Format.pp_print_string ppf "DT_VALRNGHI"
      | ADDRRNGLO -> Format.pp_print_string ppf "DT_ADDRRNGLO"
      | CONFIG -> Format.pp_print_string ppf "DT_CONFIG"
      | DEPAUDIT -> Format.pp_print_string ppf "DT_DEPAUDIT"
      | AUDIT -> Format.pp_print_string ppf "DT_AUDIT"
      | PLTPAD -> Format.pp_print_string ppf "DT_PLTPAD"
      | MOVETAB -> Format.pp_print_string ppf "DT_MOVETAB"
      | SYMINFO -> Format.pp_print_string ppf "DT_SYMINFO"
      | ADDRRNGHI -> Format.pp_print_string ppf "DT_ADDRRNGHI"
      | VERSYM -> Format.pp_print_string ppf "DT_VERSYM"
      | RELACOUNT -> Format.pp_print_string ppf "DT_RELACOUNT"
      | RELCOUNT -> Format.pp_print_string ppf "DT_RELCOUNT"
      | FLAGS_1 -> Format.pp_print_string ppf "DT_FLAGS_1"
      | VERDEF -> Format.pp_print_string ppf "DT_VERDEF"
      | VERDEFNUM -> Format.pp_print_string ppf "DT_VERDEFNUM"
      | VERNEED -> Format.pp_print_string ppf "DT_VERNEED"
      | VERNEEDNUM -> Format.pp_print_string ppf "DT_VERNEEDNUM"
      | Proc x -> Format.fprintf ppf "DT_PROC(%lx)" x
      | Unknown x -> Format.fprintf ppf "Unknown(%lx)" x
  end

  type t = DT.t * uint64

  let read32 t =
    ensure t 8 "Dynamic entry truncated";
    let kind = DT.of_int32 (Uint32.to_int32 (Read.u32 t)) in
    let value = Uint32.to_uint64 (Read.u32 t) in
    (kind, value)

  let read64 t =
    ensure t 16 "Dynamic entry truncated";
    let kind = DT.of_int32 (Uint64.to_int32 (Read.u64 t)) in
    let value = Read.u64 t in
    (kind, value)

  let read : Img.t -> Phdr.t -> uint64 DT.Map.t =
    let rec loop :
        (int Reader.t -> t) ->
        int Reader.t ->
        uint64 DT.Map.t ->
        uint64 DT.Map.t =
     fun read cursor entries ->
      match read cursor with
      | NULL, _ -> entries
      | key, value ->
          Loader_logger.debug "(%a, %Lx)" DT.pp key (Uint64.to_int64 value);
          loop read cursor (DT.Map.add key value entries)
    in
    fun img segment ->
      let cursor =
        of_bigarray ~pos:segment.offset ~endianness:img.header.ident.data
          img.buf
      in
      let read =
        match img.header.ident.kind with `x32 -> read32 | `x64 -> read64
      in
      loop read cursor DT.Map.empty
end

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
          set_pos t s.Shdr.offset;
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

let read_offset i offset = Int.unsafe_to_uint8 i.Img.buf.{offset}

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
      let offset = Virtual_address.diff addr h.Phdr.vaddr in
      if offset > h.Phdr.filesz then Int.unsafe_to_uint8 0
      else Int.unsafe_to_uint8 i.Img.buf.{h.Phdr.offset + offset}
    else
      let s = find_section_by_addr_with_cache i addr in
      if s.Shdr.kind = Shdr.SHT.NOBITS then Int.unsafe_to_uint8 0
      else
        Int.unsafe_to_uint8
          i.Img.buf.{Virtual_address.diff addr s.Shdr.addr + s.Shdr.offset}
  with Not_found ->
    let msg =
      Format.asprintf "Unreachable virtual address %a" Virtual_address.pp addr
    in
    invalid_arg msg

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
    | `x32 -> fun r -> Uint32.to_int (Read.u32 r)
    | `x64 -> fun r -> Uint64.to_int (Read.u64 r)
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
            (fun fmap -> fmap.name <- Read.zero_string "files" cursor ())
            files;
          files
      | _ -> result)
    [||] i.notes

module Utils = struct
  let is_ifunc (img : Img.t) (sym : Sym.t) =
    match img.vendor with
    | GNU _ -> ( match sym.kind with OS 10 -> true | _ -> false)
    | _ -> false

  let generic_map_synthetic_symtab :
      Rel.t array ->
      plt:Shdr.t ->
      plt0_size:int ->
      pltn_size:int ->
      (Virtual_address.t * string) list =
   fun rel_plt ~plt:{ addr = base; _ } ~plt0_size ~pltn_size ->
    Array_utils.fold_righti
      (fun i symtab ({ symbol = { name; _ }; addend; _ } : Rel.t) ->
        if Option.fold ~none:true ~some:(Z.equal Z.zero) addend then
          (Virtual_address.add_int (plt0_size + (pltn_size * i)) base, name)
          :: symtab
        else symtab)
      [] rel_plt

  let x86_map_synthetic_symtab image =
    match
      Array.fold_left
        (fun ((rela_opt, plt_opt) as r) sec ->
          match Section.header sec with
          | { name; _ } as header -> (
              match name with
              | ".rela.plt" -> (Some header, plt_opt)
              | ".plt" when plt_opt = None -> (rela_opt, Some header)
              | ".plt.sec" -> (rela_opt, Some header)
              | _ -> r))
        (None, None) (Img.sections image)
    with
    | None, _ | _, None -> []
    | Some rela_plt, Some ({ name = ".plt.sec"; _ } as plt) ->
        generic_map_synthetic_symtab (Rel.read image rela_plt) ~plt ~plt0_size:0
          ~pltn_size:16
    | Some rela_plt, Some plt ->
        generic_map_synthetic_symtab (Rel.read image rela_plt) ~plt
          ~plt0_size:16 ~pltn_size:16

  let generic_map_synthetic_symtab image ~plt0_size ~pltn_size =
    match
      Array.fold_left
        (fun ((rel_opt, plt_opt) as r) sec ->
          match Section.header sec with
          | { name; _ } as header -> (
              match name with
              | ".rela.plt" | ".rel.plt" -> (Some header, plt_opt)
              | ".plt" when plt_opt = None -> (rel_opt, Some header)
              | _ -> r))
        (None, None) (Img.sections image)
    with
    | None, _ | _, None -> []
    | Some rel_plt, Some plt ->
        generic_map_synthetic_symtab (Rel.read image rel_plt) ~plt ~plt0_size
          ~pltn_size

  let synthetic_symtab : Img.t -> (Virtual_address.t * string) list =
   fun img ->
    match Img.header img with
    | { Ehdr.kind = EXEC | DYN; machine; _ } -> (
        match machine with
        | X86 _ -> x86_map_synthetic_symtab img
        | ARM { rev = `v8; _ } | RISCV _ ->
            generic_map_synthetic_symtab img ~plt0_size:32 ~pltn_size:16
        | _ -> [])
    | _ -> []

  let is_jump_slot : Machine.isa -> int -> bool =
   fun isa kind ->
    match (isa, kind) with
    | ARM { rev = `v7 _; _ }, 22
    | ARM { rev = `v8; _ }, 1026
    | PPC _, 21
    | RISCV _, 5
    | SPARC _, 21
    | X86 _, 7 ->
        true
    | _ -> false

  let jmprel : Img.t -> Rel.t list =
    let get_entry : Dynamic.DT.t -> uint64 Dynamic.DT.Map.t -> uint64 =
      Dynamic.DT.Map.find
    in
    let offset addr int = Virtual_address.add_int int addr
    and get img addr = Uint8.to_char (read_address img addr) in
    let rec loop :
        ('a Reader.t -> Rel.t) ->
        Virtual_address.t Reader.t ->
        Rel.t list ->
        Rel.t list =
     fun read cursor relocs ->
      if Reader.at_end cursor then List.rev relocs
      else loop read cursor (read cursor :: relocs)
    in
    fun img ->
      let endianness =
        match Img.header img with { ident = { data; _ }; _ } -> data
      in
      let phs = program_headers img in
      match
        Array.find_opt
          (function { Phdr.kind = DYNAMIC; _ } -> true | _ -> false)
          phs
      with
      | None -> []
      | Some dynamic -> (
          let entries = Dynamic.read img dynamic in
          match
            ( get_entry JMPREL entries,
              get_entry PLTREL entries,
              get_entry PLTRELSZ entries,
              get_entry STRTAB entries,
              get_entry STRSZ entries,
              get_entry SYMTAB entries,
              get_entry SYMENT entries )
          with
          | exception Not_found -> []
          | jmprel, pltrel, pltrelsz, strtab, strsz, symtab, syment -> (
              match
                match
                  ( img.header.ident.kind,
                    Dynamic.DT.of_int32 (Uint64.to_int32 pltrel) )
                with
                | `x32, REL -> Some (Rel.read32, Sym.read_32)
                | `x32, RELA -> Some (Rel.reada32, Sym.read_32)
                | `x64, REL -> Some (Rel.read64, Sym.read_64)
                | `x64, RELA -> Some (Rel.reada64, Sym.read_64)
                | _ | (exception Invalid_argument _) -> None
              with
              | Some (rel_read, sym_read) ->
                  let rel_reader =
                    let start = Virtual_address.of_uint64 jmprel in
                    let stop =
                      Virtual_address.add start
                        (Virtual_address.of_uint64 pltrelsz)
                    in
                    Reader.create ~offset ~get ~endianness ~start ~stop img
                  and sym_reader =
                    Reader.create ~offset ~get ~endianness
                      ~start:(Virtual_address.of_uint64 symtab)
                      ~stop:
                        (Virtual_address.of_uint64
                           (Int64.to_uint64 0xffffffffffffffffL))
                      img
                  and str_reader =
                    let start = Virtual_address.of_uint64 strtab in
                    let stop =
                      Virtual_address.add start
                        (Virtual_address.of_uint64 strsz)
                    in
                    Reader.create ~offset ~get ~start ~stop img
                  in
                  let sym_name idx =
                    set_pos str_reader idx;
                    Read.zero_string "Unterminated symbol name" str_reader ()
                  in
                  let syment = Uint64.to_int syment in
                  let sym idx =
                    set_pos sym_reader (idx * syment);
                    sym_read sym_reader (Fun.const Shdr.dummy) sym_name
                  in
                  let read cursor = rel_read cursor sym in
                  loop read rel_reader []
              | None -> []))
end
