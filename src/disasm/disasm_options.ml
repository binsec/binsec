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

(** Generic options for disassembly *)

include Cli.Make (struct
  let name = "disassembly"
  let shortname = "disasm"
end)

type disassembly_mode =
  | Recursive
  | Linear
  | Linear_byte_wise
  | Extended_linear

module Disassembly_mode = struct
  include Builder.Variant_choice_assoc (struct
    type t = disassembly_mode

    let assoc_map =
      [
        ("rec", Recursive);
        ("linear", Linear);
        ("bytelinear", Linear_byte_wise);
        ("extlinear", Extended_linear);
      ]

    let default = Linear
    let name = "mode"
    let doc = " Set disassembly mode"
  end)
end

module DbaOutputFile = struct
  include Builder.String (struct
    let name = "o-dba"
    let default = "out.dba"
    let doc = Format.sprintf " Set DBA instructions output file"
  end)
end

module OpcodeOutputFile = Builder.String_option (struct
  let name = "dump"
  let doc = " Set opcodes output file [stdout]"
end)

module NoLoaderMode = Builder.False (struct
  let name = "no-loader"
  let doc = "Do not use loader and start at 0x0"
end)

module ShowInstructionCount = Builder.False (struct
  let name = "show-instruction-count"
  let doc = "Show a summary of encountered instructions"
end)

module Sections = Builder.String_set (struct
  let name = "sections"
  let doc = "Disassemble given comma separated list of sections"
end)

module Functions = Builder.String_set (struct
  let name = "functions"
  let doc = "Disassemble given comma separated list of functions"
end)

module Decode_instruction = Builder.String_option (struct
  let name = "decode"
  let doc = "Decode hexadecimal opcode"
end)

module Decode_replacement = Builder.String_option (struct
  let name = "decode-replacement"

  let doc =
    "Replace instructions with a specific dba blocks. Syntax: (0xaddress -> \
     dhunk)*"
end)

module CFG_graph = Builder.False (struct
  let name = "cfgraph"
  let doc = "Print control-flow graph"
end)

module Disasm_at = Builder.String (struct
  let default = "0"
  let name = "at"
  let doc = "Use this address as base for opcode decoding"
end)

module Cache_decoder = Builder.False (struct
  let name = "cache-decoder"

  let doc =
    "Cache accesses to decoder queries. This option is useful for externally \
     provided decoders. Warning: this may be RAM-intensive and assumes code \
     under disassembly is not dynamically created"
end)
