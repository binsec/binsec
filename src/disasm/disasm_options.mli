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

include Cli.S

(** Command-line options specific to disassembly *)

module DbaOutputFile : Cli.STRING
module OpcodeOutputFile : Cli.STRING_OPT

module NoLoaderMode : Cli.BOOLEAN
(** Default to [false]. Loader is activated by default *)

module ShowInstructionCount : Cli.BOOLEAN
module Sections : Cli.STRING_SET
module Functions : Cli.STRING_SET

type disassembly_mode =
  | Recursive
  | Linear
  | Linear_byte_wise
  | Extended_linear

module Disassembly_mode : Cli.GENERIC with type t = disassembly_mode
module Decode_instruction : Cli.STRING_OPT
module Decode_replacement : Cli.STRING_OPT
module CFG_graph : Cli.BOOLEAN
module Disasm_at : Cli.STRING
module Cache_decoder : Cli.BOOLEAN
