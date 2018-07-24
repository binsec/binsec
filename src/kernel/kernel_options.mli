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

(** General command-line options (globals vars) *)

module ExecFile : Cli.STRING_OPT
(** Executable file (or unnamed argument) *)

module Config_file : Cli.STRING_OPT
(** User-provided configuration file *)

(** Use external decoder
    This is for example needed for arm support.
*)
module Decoder : Cli.STRING

(** {2 Static disassembly / Analysis } *)

module Dba_file : Cli.STRING_OPT

module Dba_config : Cli.STRING_OPT

(** DBA start address *)

module Entry_point : Cli.STRING_OPT

module Describe_binary: Cli.BOOLEAN

(** {2 Tests} *)

(** {b Experimental purposes only} *)
module Experimental : Cli.BOOLEAN

module Machine : sig
  module ISA : sig
    include Cli.GENERIC with type t = Machine.isa
    val pp : Format.formatter -> Machine.isa -> unit
  end
end
include Cli.S

module Share_directories : sig
  include Cli.STRING_LIST
  val find_file : filename:string -> string
end
