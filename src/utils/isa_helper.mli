(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2022                                               *)
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

val get_defs : unit -> (string * Dba.LValue.t) list
(** [get_defs ()]
    returns the list of known entities for the current architecture
    (see {!val:Kernel_options.Machine.isa}).

    Meaningfull for x86 only for now.
*)

val core : Loader_elf.Img.t -> Virtual_address.t * (Dba.Var.t * Dba.Expr.t) list
(** [core img]
    read and translate the content of [NT_PRSTATUS] note into
    entrypoint and initialisation values.

    Meaningfull for x86 only for now.
*)

val max_instruction_len : unit -> Size.Byte.t
(** [max_instruction_len ()]
    returns the size of the longest valid instruction for
     the current architecture (see {!val:Kernel_options.Machine.isa}).
*)
