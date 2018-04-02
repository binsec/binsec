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

module type Gettable_settable = sig
  type t
  val set : t -> unit
  val get : unit -> t
  val pp : Format.formatter -> t -> unit
  val pp_current : Format.formatter -> unit -> unit
end

(** Abstract representation of hardware architecture *)

type isa =
  | X86 (** x86 32 bits *)
  | AMD64 (** aka x86-64 *)
  | PowerPC (** Power PC 32 bits *)
  | ARM (** ARM 32 bits *)
  | Unknown


type endianness =
  | LittleEndian
  | BigEndian


(** *)
module ISA : Gettable_settable with type t = isa

module Endianness : Gettable_settable with type t = endianness

(** Word size of the machine in bits *)
module Word_size : Gettable_settable with type t = int


(** {2 General setters} *)

val set_x86 : unit -> unit
(** X86 and LittleEndian *)

val set_amd64 : unit -> unit
(** AMD64 and LittleEndian *)

val set_powerpc : unit -> unit
(** PowerPC and BigEndian *)

val is_unknown : unit -> bool
val set_unknown : unit -> unit
(** No architecture and LittleEndian. Anything can happen. *)

val set_arm : endianness -> unit
(** ARM with chosen endianness. *)

val set_arm_little : unit -> unit
val set_arm_big : unit -> unit

val pp : Format.formatter -> unit -> unit
(** [pp ppf arch] pretty-prints an arch value into [ppf] *)
