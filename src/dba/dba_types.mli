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

(** Modules & types related to DBA types *)

open Sigs
module Logger : Logger.S

type instruction_sequence = (Dba.address * Dba.Instr.t) list

module Call_stack : COMPARABLE with type t = (Dba.address * Dba.address) list

(** {2 Dba address / Code address } *)

(** A DBA instruction is uniquely located at an address + label/id

    Such a location is named "code address" (or [Caddress])
*)
module Caddress : sig
  type t = Dba.address

  include Sigs.Collection with type t := t

  val default_init : t ref
  val create : Virtual_address.t -> int -> t

  val block_start : Virtual_address.t -> t
  (** [block_start bv] i [create bv 0] *)

  val block_start_of_int : int -> t
  val rebase : t -> Virtual_address.t -> t
  val reid : t -> int -> t

  val pp_base : Format.formatter -> t -> unit
  (** [pp_base caddr] only print the base address of the DBA code address as
      hexadecimal, thus ignoring the [id] part
  *)

  val add_int : t -> int -> t
  (** [add_int addr n] Increment the current address from the value of [n] *)

  val add_id : t -> int -> t
  val base_value : t -> Virtual_address.t
  val of_virtual_address : Virtual_address.t -> t
  val to_virtual_address : t -> Virtual_address.t

  include HASHABLE with type t := t
end

module AddressStack : sig
  type t = Dba.address * Call_stack.t * int

  val pp : Format.formatter -> t -> unit

  include Collection with type t := t
end

(** {2 DBA AST modules} *)

module Var : Sigs.Collection with type t = Dba.Var.t

module Expr : sig
  type t = Dba.Expr.t

  include PRINTABLE with type t := t

  val var : string -> Size.Bit.t -> Dba.Var.Tag.t -> t
  (** {6 Constructors } *)

  val flag : ?bits:Size.Bit.t -> string -> t
  (** [flag ~bits flagname] constructs a variable named [flagname] tagged as a
      flag.
      [bits] defaults to [Size.Bit.bits1].
  *)

  val temporary : string -> Size.Bit.t -> t
  (** [temporary name nbits] constructs a variable named [name] of size [nbits]
      flagged as a temporary *)

  val sext : t -> Size.Bit.t -> t
  val uext : t -> Size.Bit.t -> t
  val bool_false : t

  val bool_true : t
  (** Encoding of booleans as DBA expressions *)

  val temp : Size.Bit.t -> t
  (** [temp n] creates an expression representing a temporary of size [n] with name
      [Format.sprintf "temp%d" n]. *)

  val of_lvalue : Dba.LValue.t -> t
  (** {6 Operations } *)

  val is_symbolic : t -> bool
  (** {6 Predicates } *)

  val is_zero : t -> bool
  (** [is_zero e] is [true] if [e]'s value is equal to 0 whatever its length is *)

  val is_one : t -> bool
  (** [is_one e] is [true] if [e]'s value is equal to 1 whatever its length is *)

  val is_max : t -> bool
  (** [is_max e] is [true] if [e] is a constant representing the maximum value for
   ** its size *)
end

module LValue : sig
  type t = Dba.LValue.t

  module Map : Map.S with type key = t

  val name_of : t -> string option
  (** [name_of lval] returns [Some name] if the lvalue is named, [None]
      otherwise *)

  val bitsize : t -> Size.Bit.t
  val unsafe_bitsize : t -> int
  val is_temporary : t -> bool
  val is_flag : t -> bool
end

module Jump_target : sig
  val outer_jumps : 'a Dba.Jump_target.t -> Virtual_address.Set.t
end

type ('a, 'b) defuse = { defs : 'a; uses : 'b }

module Instruction : sig
  type t = Dba.Instr.t

  val set_successor : t -> int -> t
  (** {7 Modificators} *)

  val reset_successor : src_id:int -> dst_id:int -> t -> t

  val reloc :
    ?outer:(Dba.id Dba.Jump_target.t -> Dba.id Dba.Jump_target.t) ->
    ?inner:(Dba.id -> Dba.id) ->
    t ->
    t

  val successors : t -> Dba.id Dba.jump_target list
  (** {7 Properties and computations} *)

  val variables :
    t -> (Basic_types.String.Set.t, Basic_types.String.Set.t) defuse
  (** [variables t] returns a couple [defined * used] representing sets of
      variable names defined and used at instruction [t] *)

  val temporaries :
    t -> (Basic_types.String.Set.t, Basic_types.String.Set.t) defuse
  (** [temporaries t] returns a couple [defined * used] representing sets of
      temporaries being defined and used *)

  val outer_jumps : t -> Virtual_address.Set.t
  (** [outer_jumps t] returns the set of virtual addresses this instruction may
      jump to.

      This is a conservative, syntactic computation.
      Whenever an instruction can jump to a virtual address, this corresponds to
      a jump outside a DBA block. This function is used in [Dhunk.outer_jumps].
  *)

  val is_call : t -> bool
  (** [is_call t] returns [true] if the instruction is a function call.

      A DBA function call is encoded a jump (static or dynamic) with a [Call]
      tag which stores the return address.
  *)

  val is_return : t -> bool

  val generic_reset_successors :
    p:(Dba.id -> bool) -> f:(Dba.id -> Dba.id) -> Dba.Instr.t -> Dba.Instr.t
  (** [generic_reset_successors ~p ~f i]
        applies the transformation [f] on the successor index of [i]
        if predicate [p] is [true]
    *)
end

module Declarations : sig
  type t = (Dba.size * Dba.Var.Tag.t) Basic_types.String.Map.t
  (** A DBA declaration has a name, a size in bits and some optional tags *)

  val of_list : (string * Dba.size * Dba.Var.Tag.t) list -> t
end

module Statement : sig
  type t = private { location : Caddress.t; instruction : Dba.Instr.t }

  include PRINTABLE with type t := t

  val create : Caddress.t -> Dba.Instr.t -> t
  val location : t -> Caddress.t
  val instruction : t -> Dba.Instr.t
  val set_instruction : t -> Dba.Instr.t -> t
  val set_location : t -> Caddress.t -> t
end

val malloc_id : int ref

type dbainstrmap = Dba.Instr.t Caddress.Map.t
type read_perm = Read of bool
type write_perm = Write of bool
type exec_perm = Exec of bool
type permissions = Dba.Expr.t * (read_perm * write_perm * exec_perm)

type program = {
  start_address : Dba.address;
  declarations : Declarations.t;
  initializations : Dba.Instr.t list;
  instructions : dbainstrmap;
}
