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

val decode_32 :
  Lreader.t -> Virtual_address.t -> Instruction.Generic.t * Dhunk.t
(** [decode_32 r addr] decodes what is at address [addr] in reader [r]
    using ISA RISC-V32I with standard extensions C and M *)

val decode_64 :
  Lreader.t -> Virtual_address.t -> Instruction.Generic.t * Dhunk.t
(** [decode_64 r addr] decodes what is at address [addr] in reader [r]
    using ISA RISC-V64I with standard extensions C and M *)
