(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

(** Command-line and API options for example plugin *)
include Cli.S


(** The maximal number of mnemonics to display. Defaults to [5], *)
module Limit : Cli.INTEGER

(** User-provided set of prefixes. Prefixes are discarded when counting
 ** mnemonics so as to consider <prefix> m1 and m1 as two occurrences of the
 ** same mnemonic.
 ** A typical example is [rep] in the X86 ISA.
 *)
module Asm_prefixes : Cli.STRING_SET
