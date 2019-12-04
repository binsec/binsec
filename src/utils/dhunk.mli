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

(** DBA hunks, aka dhunk *)

(** {4 DBA blocks} *)

type t
(** A DBA block represents a set of DBA instructions with explicit links to
    the next one. The first instruction of the block always has [id] 0.
    Typically, DBA a block is the translation of one binary/assembly
    instruction.
*)


module Node : sig
  type t
  val id : t -> int
  val inst : t -> Dba.Instr.t option
end


val empty : t
(** The one and only empty DBA hunk. Get it here! *)

val stop : t
(** A simple dhunk of one instruction stopping with an [OK] [Dba.state] *)

val is_empty : t -> bool

val init : int -> (int -> Dba.Instr.t) -> t
val singleton : Dba.Instr.t -> t
val length : t -> int

val node : t -> int -> Node.t option
val inst : t -> int -> Dba.Instr.t option

val start : t -> Node.t
(** [start b] is the first index of block [b] *)

val copy : t -> t
val iter  : f:(Dba.Instr.t -> unit) -> t -> unit

val iteri : f:(int -> Dba.Instr.t -> unit) -> t -> unit

val of_list : Dba.Instr.t list -> t
(** [of_list l] assumes the list is sorted in increasing order inside the
    block *)

val of_labelled_list : (int * Dba.Instr.t) list -> t
(** [of_list l] assumes the list is sorted in increasing order inside the
    block, i.e. the labels are contiguous starting from 0. *)


val mapi : f:(int -> Dba.Instr.t -> Dba.Instr.t) -> t -> t

val flatten : t -> (int * Dba.Instr.t) list

val to_list : t -> Dba.Instr.t list
val fold : ('a -> Dba.Instr.t -> 'a) -> 'a -> t -> 'a
val for_all : (Dba.Instr.t -> bool) -> t -> bool

val export_and_view : ?cmd:string -> t -> unit
(** [view dh] Visualize dot-rendered DBA hunk [dh] using [cmd].

    Default value for [cmd] is firefox.
*)

val pred : t -> Node.t -> Node.t list
val succ : t -> Node.t -> Node.t list

include Sigs.PRINTABLE with type t:=t

(** {7 Dhunk properties}*)

module Check : sig
  val has_inbound_inner_jumps : t -> bool
  (** [has_inbound_inner_jumps dh] checks a hunk only has well-behaved inner
      jumps, i.e. to an index that is defined inside this hunk.
  *)

  val no_undeclared_variables : Dba_types.Declarations.t -> t -> bool
  (** [no_undeclared_variables decls dh] checks that the hunk [dh] only uses
      well-declared variables w.r.t. to [decls]
  *)

  val no_temporary_leak : t -> bool
  (** [no_temporary_leak b] checks the invariant that a block must always
      (re)define a temporary before using it. This guarantees that no
      assumption is made on the block sequences and that no "leaked"
      information from another block is used inside a block.

      @return [true] if that is the case.
  *)
end

module Simplify : sig
  val remove_gotos : t -> unit
  (** [remove_gotos d] removes static jump instruction which have 1 predecessor
      and 1 successor
  *)

  val run : t -> unit
  (** [run d] does all simplifications for dhunk [d]*)
end

val to_stmts :
  t -> Virtual_address.t -> Dba_types.Statement.t list

val outer_jumps : t -> Virtual_address.Set.t
(** [outer_jumps b] computes the set of jumps to external addresses in hunk
    [b].
    Due to dynamic jumps, this represents a syntactic under-approximation of
    the possible jumps from this block.
*)

val callees : t -> Virtual_address.Set.t
(** [callees b] computes the set of addresses this block may call *)

val is_return : t -> bool
(** [is_return d] check if dhunk [d] encodes a return *)

val has_indirect_jump : t -> bool
(** [has_indirect_jump d] returns [true] if the hunk contains an indirect jump
    instruction
*)

type conditional = {
    condition : Dba.Expr.t;
    consequent : Virtual_address.t;
    alternative : Virtual_address.t;
}

val conditional : t -> conditional option
