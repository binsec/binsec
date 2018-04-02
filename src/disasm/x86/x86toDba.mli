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

(** Lifter from X86 to DBA *)

exception InstructionUnhandled of string

(** {2 Access to internal statistics} *)

val handled_instructions : unit -> int * int
(** insertions / unique insertions *)

val unknown_instructions : unit -> int * int
(** insertions / unique insertions *)

val native_instructions_decoded : unit -> int
(** Number of decoded instructions.
    This is always equal to
    [fst (handled_instructions ()) + fst (unknown_instructions ())]
 *)

val pp_unknown_instructions : Format.formatter -> unit -> unit

val decode: Dba_types.Virtual_address.t -> X86Instruction.t * Dba_types.Block.t

val decode_string: Basic_types.Binstream.t -> int64 -> X86Instruction.t * Dba_types.Block.t
