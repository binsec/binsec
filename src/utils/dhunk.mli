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

(** DBA hunks, aka dhunk *)

(** {4 DBA blocks} *)

type t
(** A DBA block represents a set of DBA instructions with explicit links to
    the next one. The first instruction of the block always has [id] 0.
    Typically, DBA a block is the translation of one binary/assembly
    instruction.
*)

val empty : t
(** The one and only empty DBA hunk. Get it here! *)

val stop : t
(** A simple dhunk of one instruction stopping with an [OK] [Dba.state] *)

val is_empty : t -> bool
val init : int -> (int -> Dba.Instr.t) -> t
val singleton : Dba.Instr.t -> t

val goto : Virtual_address.t -> t
(** A simple dhunk of one instruction jumping to the virtual address. *)

val length : t -> int
val inst : t -> int -> Dba.Instr.t option
val inst_exn : t -> int -> Dba.Instr.t

val start : t -> int
(** [start b] is the first index of block [b] *)

val exits : t -> int list
(** [exits b] are the instruction indexes which end the block [b] *)

val copy : t -> t
val iter : f:(Dba.Instr.t -> unit) -> t -> unit
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

val unlink : t -> int -> unit
(** [unlink dh i] skips the [i]th instruction.
    Its predecessors go to its successor.

    @raise [Invalid_argument] if [inst dh i] has not exactly one successor. *)

val export_and_view : ?cmd:string -> t -> unit
(** [view dh] Visualize dot-rendered DBA hunk [dh] using [cmd].

    Default value for [cmd] is firefox.
*)

val pred : t -> int -> int list
val succ : t -> int -> int list

val optimize : ?inplace:bool -> t -> t
(** [optimize dh] Performs some "compiler" optimizations
    and return the simplified block.

    @param inplace Directly modify the block without making a copy.
                   Default: [false].
*)

include Sigs.PRINTABLE with type t := t

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

val to_stmts : t -> Virtual_address.t -> Dba_types.Statement.t list

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
val constant_propagation : t -> t
val dead_code_elimination : t -> t
