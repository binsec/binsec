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

open Loader_types

module E_class : sig
  type t = [ `x32 | `x64 ]

  include Sigs.PRINTABLE with type t := t
end

module E_ident : sig
  type t = private {
    kind : E_class.t;
    data : Machine.endianness;
    version : u8;
    osabi : u8;
    abiversion : u8;
  }
end

module Ehdr : sig
  module ET : sig
    type t = NONE | REL | EXEC | DYN | CORE | OS of int | PROC of int

    include Sigs.PRINTABLE with type t := t
  end

  (* Main header. *)
  type t = private {
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

  include Sigs.PRINTABLE with type t := t
end

module Shdr : sig
  module SHT : sig
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

    include Sigs.PRINTABLE with type t := t
  end

  module SHF : sig
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

    val is : u64 -> t -> bool

    include Sigs.PRINTABLE with type t := u64
  end

  (* ELF section header. *)
  type t = private {
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

  module SHN : sig
    type section = t
    type t = UNDEF | SEC of section | PROC of int | OS of int | ABS | COMMON

    include Sigs.PRINTABLE with type t := t
  end
end

module Sym : sig
  module STT : sig
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

    include Sigs.PRINTABLE with type t := t
  end

  module STB : sig
    type t = LOCAL | GLOBAL | WEAK | OS of int | PROC of int

    include Sigs.PRINTABLE with type t := t
  end

  type t = private {
    name : string;
    kind : STT.t;
    bind : STB.t;
    other : u8;
    sh : Shdr.SHN.t;
    value : Virtual_address.t;
    size : int;
  }
end

module Phdr : sig
  module PHT : sig
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

    include Sigs.PRINTABLE with type t := t
  end

  module PHF : sig
    type t = X | W | R | OS of int32 | PROC of int32

    val is : u32 -> t -> bool

    include Sigs.PRINTABLE with type t := u32
  end

  (* ELF program header *)
  type t = private {
    kind : PHT.t;
    flags : u32;
    offset : int;
    vaddr : Virtual_address.t;
    paddr : u64;
    filesz : int;
    memsz : u64;
    align : u64;
  }
end

include
  Loader_sigs.S
    with type Section.header = Shdr.t
     and type Symbol.header = Sym.t
     and type Img.header = Ehdr.t

val program_headers : Img.t -> Phdr.t array

module Rel : sig
  type t = {
    offset : Virtual_address.t;
    kind : int;
    symbol : Sym.t;
    addend : Z.t option;
  }

  val read : Img.t -> Shdr.t -> t array
end

module Dynamic : sig
  module DT : sig
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

    module Map : Map.S with type key = t
    include Sigs.PRINTABLE with type t := t
  end

  val read : Img.t -> Phdr.t -> u64 DT.Map.t
end

module Note : sig
  type t = { name : string; kind : int; offset : int; size : int }

  val read : int Reader.t -> t
end

val notes : Img.t -> Note.t array

type fmap = private {
  addresses : Virtual_address.t Interval.t;
  offset : int;
  mutable name : string;
}

val fmap : Virtual_address.t Interval.t -> int -> string -> fmap
val files : Img.t -> fmap array

module Utils : sig
  val is_ifunc : Img.t -> Symbol.t -> bool
  val synthetic_symtab : Img.t -> (Virtual_address.t * string) list
  val is_jump_slot : Machine.isa -> int -> bool
  val jmprel : Img.t -> Rel.t list
end
