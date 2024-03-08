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

(** Kernel general command-line options. *)

include Cli.Options (struct
  let shortname = "" (* This is the only one :-) *)
  let name = "Kernel"
end)

module Config_file = Builder.String_option (struct
  let name = "config"
  let doc = "Use this configuration file"
end)

module Dba_config = Builder.String_option (struct
  let name = "dba-config"
  let doc = "Set dba configuration file name"
end)

module Dba_file = Builder.String_option (struct
  let name = "dba-file"
  let doc = "Set DBA file "
end)

module Describe_binary = Builder.False (struct
  let name = "describe"
  let doc = "Display a description of the binary and exits"
end)

(** Server options *)

module Experimental = Builder.False (struct
  let name = "X"
  let doc = "Only for developmental purposes"
end)

module ExecFile = Builder.String_option (struct
  let name = "file"
  let doc = "Set binary file"
end)

module Entry_point = Builder.String_option (struct
  let name = "entrypoint"
  let doc = "Set entry point"
end)

module Decoder = Builder.String (struct
  let name = "decoder"
  let default = "unisim-armsec"
  let doc = "External decoder command"
end)

module Version = Builder.False (struct
  let name = "version"
  let doc = "Print the version identifier and exit"
end)

module Machine = struct
  (** Abstract representation of hardware architecture *)
  include Builder.Variant_choice_assoc (struct
    type t = Machine.isa

    let name = "isa"
    let doc = Format.asprintf " Set isa [set by loader]"

    let assoc_map =
      [
        ("x86", Machine.x86);
        ("amd64", Machine.amd64);
        ("arm32", Machine.(armv7 LittleEndian));
        ("aarch64", Machine.(armv8 LittleEndian));
        ("ppc64", Machine.ppc64 BigEndian);
        ("riscv", Machine.riscv `x32);
        ("riscv64", Machine.riscv `x64);
        ("z80", Machine.z80);
        ("unknown", Machine.unknown);
      ]

    let default = Machine.unknown
  end)

  let pp ppf () = Machine.ISA.pp ppf (get ())
  let isa = get
  let endianness () = Machine.ISA.endianness (get ())

  let word_size () =
    Size.Bit.to_int Machine.(Bitwidth.bitsize (ISA.bits (get ())))

  let bits () = Machine.ISA.bits (get ())
  let stack_register () = Machine.ISA.stack_register (get ())
end
