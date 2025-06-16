(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2025                                               *)
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

type supported_modes = Both | Thumb | Arm

module SupportedModes : Cli.GENERIC with type t = supported_modes

val decode : Lreader.t -> Virtual_address.t -> Instruction.Generic.t * Dhunk.t
(** [decode r addr] decodes what is at address [addr] in reader [r].
 *)

val cached_decode :
  Lreader.t -> Virtual_address.t -> Instruction.Generic.t * Dhunk.t
(** Use
    [let decode = cached_decode reader in
     decode addr1;
     ...
     decode addrn; ]

   if you want to use a cached decoder, adapted for a new reader.

   The cached decoder assumes that the code will not change dynamically.
*)
