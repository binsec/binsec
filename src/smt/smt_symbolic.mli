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

(** Symbolic state *)

module State : sig
  type t

  val initializations : t -> int Bitvector.Collection.Map.t
  val create : unit -> t
  val assign : ?wild:bool -> string -> Formula.sort -> Formula.term -> t -> t

  val havoc : ?wild:bool -> string -> Formula.sort -> t -> t
  (** assign variable with a fresh symbolic value *)

  val declare : ?wild:bool -> string -> Formula.sort -> t -> t
  (** if [wild] is set, then the variable is appended to uncontroled *)

  val constrain : Formula.bl_term -> t -> t
  (** [constrain c s] adds constraint [c] to state [s] *)

  val comment : string -> t -> t
  (** [comment cmt s] *)

  val formula : t -> Formula.formula
  val memory_term : Formula.ax_term -> string * Formula.sort * Formula.term
  val get_memory : t -> Formula.ax_term
  val get_path_constraint : t -> Formula.bl_term

  val get_bv : string -> Size.Bit.t -> t -> Formula.bv_term * t
  (** automatically declares missing variables, thus returns [t] *)

  val init_mem_at : addr:Bitvector.t -> size:int -> t -> t
  val uncontrolled : t -> Formula.VarSet.t
  val pp : Format.formatter -> t -> unit
end
