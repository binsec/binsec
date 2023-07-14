(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2023                                               *)
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

    Not complete yet.
*)

val get_arg : int -> Dba.Expr.t
(** [get_arg n]
    return the standard location of the [n]th argument of a function.
*)

val get_ret : ?syscall:bool -> unit -> Dba.LValue.t
(** [get_ret ()]
    returns the standard location of the function return value.

    If [syscall] is [true], it returns the location of the syscall return value.
    (Meaningfull for x86 only for now)
*)

val make_return : ?value:Dba.Expr.t -> unit -> Dhunk.t
(** [make_return ~value ()]
    returns the standard instruction sequence of a function return.

    If [value] is given, the return value is set accordingly.
*)

val get_stack_pointer : unit -> Dba.Var.t * Bitvector.t
(** [get_a_stack_pointer ()]
    returns the stack pointer and a possible initialization for it.
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
