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

module Logger : Logger.S

(** General command-line options (globals vars) *)

module ExecFile : Cli.STRING_OPT
(** Executable file (or unnamed argument) *)

module Config_file : Cli.STRING_OPT
(** User-provided configuration file *)

module Machine : sig
  include Cli.GENERIC with type t := Machine.t

  val isa : unit -> Machine.isa
  val endianness : unit -> Machine.endianness
  val bits : unit -> Machine.bitwidth
  val word_size : unit -> int
  val stack_register : unit -> string

  include Sigs.PRINTABLE with type t := unit
end

module Decoder : Cli.STRING
(** Use external decoder
    This is for example needed for arm support.
*)

(** {2 Static disassembly / Analysis } *)

module Dba_file : Cli.STRING_OPT
module Dba_config : Cli.STRING_OPT

(** DBA start address *)

module Entry_point : Cli.STRING_OPT
module Describe_binary : Cli.BOOLEAN

(** {2 Tests} *)

module Experimental : Cli.BOOLEAN
(** {b Experimental purposes only} *)

module Version : Cli.BOOLEAN
