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

(* Type declarations for disassembly *)

module type BasicInstruction = sig
  type mnemonic

  type t = private {
    size : Basic_types.ByteSize.t;
    opcode : string;
    mnemonic : mnemonic;
  }

  val create : int -> string -> mnemonic ->  t
  val pp_opcode : Format.formatter -> t -> unit
  val pp_mnemonic : Format.formatter -> t -> unit
end

(** {6 Mnemonic } *)
module Mnemonic : sig
  type t = private
    | Bad        (** Opcodes which do not have a valid mnemonic translation *)
    | Unhandled  (** Opcodes which have a valid mnemonic but do not have a handled mnemonic translation *)
    | Handled of string


  val handled : 'a -> (Format.formatter -> 'a -> unit) -> t
  val unhandled : t
  val bad : t
  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

(** {7 Functor} *)
module Make(P:Sigs.Printable): BasicInstruction with type mnemonic = P.t


(* {7 Generic disassembled instruction type} *)
module GenericInstruction : sig
  include BasicInstruction with type mnemonic = Mnemonic.t
end

(* {6 DBA-decoded instruction *} *)
module Instruction : sig
  type t = private {
    address:  Dba_types.Virtual_address.t;
    size : Basic_types.ByteSize.t;
    opcode : Basic_types.Binstream.t;
    mnemonic : Mnemonic.t;
    dba_block : Dba_types.Block.t;
  }

  (** {7 Constructors }*)

  val empty : Dba_types.Caddress.t -> t

  val create :
    Dba_types.Virtual_address.t -> Basic_types.ByteSize.t -> Basic_types.Binstream.t ->
    Mnemonic.t -> Dba_types.Block.t -> t

  val of_generic_instruction :
     Dba_types.Virtual_address.t -> GenericInstruction.t -> Dba_types.Block.t -> t

  val of_dba_block : Dba_types.Virtual_address.t -> Dba_types.Block.t -> t

  val to_generic_instruction : t -> GenericInstruction.t

  val set_dba_block : t -> Dba_types.Block.t -> t

  val is_decoded : t -> bool

  val stop : Dba_types.Virtual_address.t -> t

  (** {7 Other accessors} *)
  val get_caddress : t -> Dba_types.Caddress.t
end


type pmap = (Dba.instruction * GenericInstruction.t option) Dba_types.Caddress.Map.t

val add_chained_instr:
  (Dba_types.Caddress.Map.key * Dba.instruction) list -> pmap -> pmap



module Program : sig
  type t = Instruction.t Dba_types.Virtual_address.Map.t

  val to_pmap : t -> pmap
end
