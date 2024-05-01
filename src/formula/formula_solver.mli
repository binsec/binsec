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

(** Interface with SMT solvers *)

(** This module provides basic functions to solve
    SMT formulas, either by providing the file name
    or directly by interacting with the SMT solver via
    theirs incremental mode. *)

module Command : sig
  type command =
    | PutEntry of Formula.entry
    | CheckSat
    | GetModel
    | GetValue of Formula.term

  val pp_command : Format.formatter -> command -> unit
  val check_sat : command
  val get_model : command
  val get_value : Formula.term -> command
  val put_entry : Formula.entry -> command
end

type solver_session = private {
  id : int;
  solver : Prover.t;
  pid : Subprocess.t;
  stdin : out_channel;
  stdout : in_channel;
  stderr : in_channel;
  (* a file to dump the session *)
  dump : out_channel option;
  (* everything printed to this formatter will be formatted to both stdin and
   * dump *)
  combined : Format.formatter;
  incremental : bool;
}
(** channels when interacting with solvers *)

val start_interactive :
  ?file:string -> ?timeout:int -> Prover.t -> solver_session
(** [start_interactive ~file ~timeout solver] starts an interactive session
    with the solver [solver] and the given [~timeout] (default is none).
    [~file] provides a debug output file where every command sent to the solver
    are also copied. *)

val stop_interactive : solver_session -> unit
(** stop the interactive session by closing the process *)

val solve : ?timeout:int -> string -> Prover.t -> Formula.status
(** [solve ~timeout file solver] solve the formula in [file]
    with the given [~timeout] and the given [solver]. Only,
    the SMT status is returned *)

val solve_incremental :
  ?term:Formula.bl_term -> solver_session -> Formula.status
(** [solve_incremental ~expr ~debug session solver] solve
    the current formula fed to the solver. An optional
    smt_expr [~expr] can be provided which would be added
    first. *)

val solve_model :
  ?timeout:int -> string -> Prover.t -> Formula.status * Smt_model.t
(** same as [solve] but also returns the model generated *)

val solve_model_time :
  ?timeout:int ->
  ?get_model:bool ->
  file:string ->
  Prover.t ->
  Formula.status * Smt_model.t * float
(** same as [solve_model] but also returns the computation time *)

val solve_incremental_model :
  ?term:Formula.bl_term -> solver_session -> Formula.status * Smt_model.t
(** same as [solve_incremental_model] but also returns the model generated *)

val solve_incremental_model_time :
  ?term:Formula.bl_term ->
  ?get_model:bool ->
  solver_session ->
  Formula.status * Smt_model.t * float
(** same as [solve_incremental_model] but also returns the
    computation time *)

val solve_incremental_value :
  ?term:Formula.bl_term ->
  Formula.bv_term ->
  solver_session ->
  Formula.status * Smtlib.term option
(** same as [solve_incremental_model] but uses the smtlib2
    [get-value] rather than [get-model] *)

val push : solver_session -> unit
(** send the [push] command to the solver *)

val pop : solver_session -> unit
(** send the [pop] command to the solver *)

(** returns a formatter st everything printed to the formatter will be sent to
 * the solver **)

val get_formatter_of_session : solver_session -> Format.formatter

module Session : sig
  type t

  type output =
    | Nil
    | Model of Smt_model.t
    | Sat of Formula.status
    | Values of (Smtlib.term * Smtlib.term) list

  val pp : Format.formatter -> t -> unit
  val create : ?file:string -> ?timeout:int -> Prover.t -> t
  val destroy : t -> unit
  val run : t -> Command.command -> output
  val put_entry : t -> Formula.entry -> unit

  (* run check-sat only if necessary (ie no assert has been issued since last
   * check-sat *)
  val check_sat : t -> Formula.status

  (* run get-model only if necessary; you still have to make sure that the
   * current formula is satisfiable *)
  val get_model : t -> Smt_model.t
  val get_value : t -> Formula.term -> (Smtlib.term * Smtlib.term) list
end
