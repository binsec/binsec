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

(** Interface with SMT solvers *)

(** This module provides basic functions to solve
    SMT formulas, either by providing the file name
    or directly by interacting with the SMT solver via
    theirs incremental mode. *)

type 'a command =
  | PutEntry : Formula.entry -> unit command
  | CheckSat : float -> Formula.status command
  | GetModel : Formula_model.t command
  | GetBvValue : Formula.bv_term list -> Bitvector.t list command
  | GetAxValue : Formula.ax_term -> (Bitvector.t * Bitvector.t) array command

module Command : sig
  type 'a t = 'a command

  val pp : Format.formatter -> 'a t -> unit
  val check_sat : float -> Formula.status t
  val get_model : Formula_model.t t
  val get_bv_value : Formula.bv_term list -> Bitvector.t list t
  val get_ax_value : Formula.ax_term -> (Bitvector.t * Bitvector.t) array t
  val put_entry : Formula.entry -> unit t
end

module Make (S : Session.S) : sig
  type t

  val pp : Format.formatter -> t -> unit
  val create : theory:string -> S.arg -> t
  val destroy : t -> unit
  val run : t -> 'a command -> 'a
  val put_entry : t -> Formula.entry -> unit

  (* run check-sat only if necessary (ie no assert has been issued since last
   * check-sat *)
  val check_sat : timeout:float -> t -> Formula.status

  (* run get-model only if necessary; you still have to make sure that the
   * current formula is satisfiable *)
  val get_model : t -> Formula_model.t
  val get_bv_value : t -> Formula.bv_term list -> Bitvector.t list
  val get_ax_value : t -> Formula.ax_term -> (Bitvector.t * Bitvector.t) array
end
