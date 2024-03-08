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

module type Solver = sig
  type t

  val open_session : unit -> t
  (** [open_session ()] creates a new incremental solver instance.

      @return A new stateful solver instance.
  *)

  val put : t -> Formula.entry -> unit
  (** [put solver entry] sends the entry to the solver.

      @param solver The solver instance.
      @param entry The formula entry to send.
  *)

  val check_sat : t -> Formula.status
  (** [check_sat solver] checks if the current formula is satisfiable.

      @param solver The solver instance.

      @return SAT, UNSAT or UNKNOWN.
  *)

  val get_bv_value : t -> Formula.bv_term -> Bitvector.t
  (** [get_bv_value solver expr] returns the assignment of the
      expression [expr] if [check_sat] returned SAT.
      Invalid uses may fail in an unpredictable fashion.

      @param solver The solver instance that returned SAT.
      @param expr The expression to get the assignment.

      @return The bitvector assignment of [expr].
  *)

  val get_ax_values : t -> Formula.ax_term -> (Bitvector.t * Bitvector.t) array
  (** [get_ax_values solver expr] returns the assignment of the
      array [expr] if [check_sat] returned SAT.
      Invalid uses may fail in an unpredictable fashion.

      @param solver The solver instance that returned SAT.
      @param expr The expression to get the assignment.

      @return The pair address/value assignments of [expr].
  *)

  val close_session : t -> unit
  (** [close_session solver] will destroy the solver instance
      and release its ressources.
      Calling any function on this instance afterward is invalid
      and may fail in an unpredictable fashion.

      @param solver The solver instance to destroy.
  *)

  val check_sat_and_close : t -> Formula.status
  (** [check_sat_and_close solver] is the same as caching the result
      of [check_sat] before calling [close_session].

      @param solver The solver instance.
  *)

  val query_stat : unit -> int
  (** [query_stat ()] returns the cumulated number of [check_sat] issued.

      @return The total number of satisfiability queries.
  *)

  val time_stat : unit -> float
  (** [time_stat ()] return the cumulated elapsed time with an open solver
      session.

      @return The total number of second passed with an open solver session.
  *)
end
