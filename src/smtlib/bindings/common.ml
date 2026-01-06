(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

type status = Sat | Unsat | Unknown

module type TERM = sig
  module rec Bl : sig
    type t

    val const : string -> t
    (** [const name]
        creates the (first-order) boolean constant named [name] . *)

    val top : t
    (** The value [true]. *)

    val bot : t
    (** The value [false]. *)

    val lognot : t -> t
    (** [lognot x y]
        creates the boolean [not] operation of [x]. *)

    val logand : t -> t -> t
    (** [logand x y]
        creates the boolean [and] operation between [x] and [y]. *)

    val logor : t -> t -> t
    (** [logor x y]
        creates the boolean [or] operation between [x] and [y]. *)

    val logxor : t -> t -> t
    (** [logxor x y]
        creates the boolean [xor] operation between [x] and [y]. *)

    val ite : t -> t -> t -> t
    (** [ite x y z]
        creates the boolean [ite] operation between [y] and [z]
        according to [x]. *)

    val equal : t -> t -> t
    (** [equal x y ]
        creates the boolean [=] operation between [x] and [y]. *)

    val diff : t -> t -> t
    (** [diff x y ]
        creates the boolean [<>] operation between [x] and [y]. *)

    val implies : t -> t -> t
    (** [implies x y]
        creates the boolean [=>] operation between [x] and [y]. *)

    val to_bv : t -> Bv.t
    (** [to_bv x]
        cast the boolean [x] as a 1-bit bitvector. *)
  end

  and Bv : sig
    type t

    val const : int -> string -> t
    (** [const size name]
        creates the (first-order) constant of [size] bits named [name] . *)

    val value : int -> Z.t -> t
    (** [value sz bv]
        creates a constant from the bitvector value [bv] of size [sz]. *)

    include
      Binsec_base.Sigs.COMPARISON with type t := t and type boolean := Bl.t

    include Binsec_base.Sigs.ARITHMETIC with type t := t
    include Binsec_base.Sigs.EXTENDED_LOGICAL with type t := t

    val lognand : t -> t -> t
    (** [lognand x y]
        creates the [nand] bitvector operation between [x] and [y]. *)

    val lognor : t -> t -> t
    (** [lognor x y]
        creates the [nor] bitvector operation between [x] and [y]. *)

    val logxnor : t -> t -> t
    (** [logxnor x y]
        creates the [xnor] bitvector operation between [x] and [y]. *)

    include Binsec_base.Sigs.SHIFT_ROT with type t := t and type index := t

    val rotate_lefti : t -> int -> t
    (** [rotate_lefti x i]
        same as [rotate_left] but with a constant index. *)

    val rotate_righti : t -> int -> t
    (** [rotate_righti x i]
        same as [rotate_right] but with a constant index. *)

    val append : t -> t -> t
    (** [append x y]
        creates the concatenation of the two bitvectors [x] and [y]. *)

    val extract : hi:int -> lo:int -> t -> t
    (** [extract ~hi ~lo x]
        extracts the sub-bitvector of [x] starting from bit [lo]
        and ending at bit [hi] (included). *)

    val uext : int -> t -> t
    (** [uext n x]
        zero-extends the bitvector [x] by adding [n] bits. *)

    val sext : int -> t -> t
    (** [sext n x]
        sign extends the bitvector [x] copying [n] times
        the most significant bit. *)

    val ite : Bl.t -> t -> t -> t
    (** [ite x y z]
        creates the bitvector [ite] operation between [y] and [z]
        according to [x]. *)

    val succ : t -> t
    (** [succ x]
        creates the bitvector [x + 1] operation. *)

    val to_bl : t -> Bl.t
    (** [to_bl x]
        cast the 1-bit bitvector [x] as a boolean. *)
  end

  and Ax : sig
    type t
    type sort

    val sort : idx:int -> int -> sort
    (** [sort ~idx elm]
        creates a new array kind that maps [idx]-bit bitvector indexes
        to [elm]-bit bitvector values. *)

    val const : sort -> string -> t
    (** [const sort name]
        creates the (first-order) constant array named [name]. *)

    val store : t -> Bv.t -> Bv.t -> t
    (** [store a i x]
        creates the array [store] operation of the byte [x] at index [i]
        in the array [a]. *)

    val select : t -> Bv.t -> Bv.t
    (** [select a i]
        creates the array [select] operation of one byte at index [i]
        in the array [a]. *)

    val equal : t -> t -> Bl.t
    (** [equal x y ]
        creates the array [=] operation between [x] and [y]. *)

    val diff : t -> t -> Bl.t
    (** [diff x y ]
        creates the array [<>] operation between [x] and [y]. *)

    val ite : Bl.t -> t -> t -> t
    (** [ite x y z]
        creates the array [ite] operation between [y] and [z]
        according to [x]. *)
  end
end

module type S = sig
  (** An incremental solver instance. *)

  include TERM

  val assert_formula : Bl.t -> unit
  (** [assert_formula bl] assert the boolean entry in the solver instance.

      @param bl The boolean entry to assert.
  *)

  val push : unit -> unit
  (** [push ()]
      creates a backup point of the current solver context.
  *)

  val pop : unit -> unit
  (** [pop ()]
      discard all the assertions since the last backup point,
      restoring the solver context in the same state as before the [push ()].
      Invalid uses may fail in an unpredictable fashion.
  *)

  val check_sat : ?timeout:float -> unit -> status
  (** [check_sat ()] checks if the current formula is satisfiable.

      @return Sat, Unsat or Unknown.
  *)

  val check_sat_assuming : ?timeout:float -> Bl.t -> status
  (** [check_sat_assuming e] checks if the current formula is satisfiable
      with the assumtion [e].

      @return Sat, Unsat or Unknown.
  *)

  val get_bv_value : Bv.t -> Z.t
  (** [get_bv_value expr] returns the assignment of the
      expression [expr] if [check_sat] returned [Sat].
      Invalid uses may fail in an unpredictable fashion.

      @param expr The expression to get the assignment.

      @return The bitvector assignment of [expr].
  *)

  val fold_ax_values : (Z.t -> Z.t -> 'a -> 'a) -> Ax.t -> 'a -> 'a
  (** [fold_ax_values f ax v] iter through the assignment of the
      array [ax] if [check_sat] returned [Sat].
      Invalid uses may fail in an unpredictable fashion.

      @param ax The expression to get the assignment.
  *)

  val close : unit -> unit
  (** [close ()] will destroy the solver instance and release its ressources.
      Calling any function on this instance afterward is invalid
      and may fail in an unpredictable fashion.
  *)
end

(** A solver instance factory. *)
module type OPEN = functor () -> S

type ('bl, 'bv, 'ax) t =
  (module S with type Bl.t = 'bl and type Bv.t = 'bv and type Ax.t = 'ax)
