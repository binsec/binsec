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

(** Pretty-printers for X86 *)

val pp_address : Format.formatter -> X86Types.address -> unit

val pp_bytes : int -> Format.formatter -> int -> unit
(** [pp_bytes n ppf v] prints the first [n] bytes of [v] into [ppf].
    [n] must be between 0 (excluded) and 4 (included) as [v] represents a X86
    word (32 bits).
*)

val pp_byte :  Format.formatter -> int -> unit
(** [pp_byte ppf v] is [pp_bytes 1 ppf v] *)

val pp_word :  Format.formatter -> int -> unit
(** [pp_word ppf v] is [pp_bytes 4 ppf v] *)

val pp_instr :  Format.formatter -> X86Types.instruction_kind -> unit
