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

(** Canonical representation of an instruction *)

(** {2 Type} *)

type t = private {
  address : Virtual_address.t;
  size : Size.Byte.t;
  opcode : Binstream.t;
  mnemonic : Mnemonic.t;
  dba_block : Dhunk.t;
}

val address : t -> Virtual_address.t
(** {2 Accessors} *)

val size : t -> Size.Byte.t
val opcode : t -> Binstream.t
val mnemonic : t -> Mnemonic.t
val hunk : t -> Dhunk.t

(** {3 Basics} *)

module type Basic = sig
  type mnemonic
  type t = private { size : Size.Byte.t; opcode : string; mnemonic : mnemonic }

  val create : int -> string -> mnemonic -> t
  val pp_opcode : Format.formatter -> t -> unit
  val pp_mnemonic : Format.formatter -> t -> unit
end

module Make (P : Sigs.PRINTABLE) : Basic with type mnemonic = P.t

(** {3 Generic disassembled instruction type} *)

module Generic : sig
  include Basic with type mnemonic = Mnemonic.t
end

(** {2 Constructors} *)

val empty : Dba_types.Caddress.t -> t

val create :
  Virtual_address.t -> Size.Byte.t -> Binstream.t -> Mnemonic.t -> Dhunk.t -> t

val unsupported :
  Virtual_address.t -> Size.Byte.t -> Binstream.t -> Mnemonic.t -> t
(** Create an instruction without supporting DBA semantics.

   This function should not be used when the menmonic is supported.
   It will fail in that case.
 *)

val of_generic_instruction : Virtual_address.t -> Generic.t -> Dhunk.t -> t
val of_dba_block : ?mnemonic:Mnemonic.t -> Virtual_address.t -> Dhunk.t -> t
val to_generic_instruction : t -> Generic.t
val set_dba_block : t -> Dhunk.t -> t
val set_mnemonic : Mnemonic.t -> t -> t
val is_decoded : t -> bool
val stop : Virtual_address.t -> t

(** {2 Other accessors} *)

val get_caddress : t -> Dba_types.Caddress.t
val start : t -> int

include Sigs.PRINTABLE with type t := t
