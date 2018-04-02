(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
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

(** Command-line options specific to disassembly *)

val simplification_cli_handler : Arg.spec
(** Sets variables below according to command line *)

val simpl: bool ref
val simpl_fun : bool ref
val simpl_sequence : bool ref
val simpl_inline_calls : bool ref
val simpl_no_summaries : bool ref

module DisassemblyMode : sig
  type t = private
    | Recursive | Linear | Linear_byte_wise | ExtendedLinear


  val set : string -> unit
  val get : unit -> t
  (** Defaults to [Recursive] *)

  val set_recursive         : unit -> unit
  val set_linear            : unit -> unit
  val set_byte_wise_linear  : unit -> unit
  val set_extended_linear   : unit -> unit
  val cli_handler : Arg.spec
end


module DbaOutputFile : Parameters.String

module OpcodeOutputFile : Parameters.OptionalString

module NoLoaderMode : Parameters.Boolean
(** Default to [false]. Loader is activated by default *)

module IgnoreUnhandledInstructions : Parameters.Boolean
(** Defaults to [true] **)

module ProtectedMode : Parameters.Boolean

module ShowInstructionCount : Parameters.Boolean
module Sections : Parameters.StringSet
     
module ArmDecoder : Parameters.String

val is_ignored_segment : X86Types.segment_reg -> bool
val mark_ignored_segments : string -> unit

val set_file : string -> unit
val get_file : unit -> string
(* Set & get executable file under disassembly *)

module Logger : Logger.S
