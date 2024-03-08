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
val decode : Lreader.t -> Virtual_address.t -> X86Instruction.t * Dhunk.t

val decode_binstream :
  ?base_addr:Virtual_address.t -> Binstream.t -> X86Instruction.t * Dhunk.t
(** [decode_binstream base_addr bstream] decodes a binary stream whose address
    is supposed to be [base_addr] into an instruction and its DBA hunk encoding.

    - [base_addr] defaults to 0
*)
