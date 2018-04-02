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

(** Interface with SMT solvers *)

(** This module provides basic functions to solve
    SMT formulas, either by providing the file name
    or directly by interacting with the SMT solver via
    theirs incremental mode. *)

(** channels when interacting with solvers *)
type solver_session = private {
  stdin: out_channel;
  stdout: in_channel;
  stderr: in_channel;
}

(** default session constructor *)
val default_session : unit -> solver_session

(** check that the given solver is boolector *)
val is_boolector : [> `boolector ] -> bool

(** [start_interactive ~file ~timeout solver] starts an interactive session
    with the solver [solver] and the given [~timeout] (default is none).
    [~file] provides a debug output file where every command sent to the solver
    are also copied. *)
val start_interactive : ?file:string -> ?timeout:int -> Common_piqi.solver_t -> solver_session

(** stop the interactive session by closing the process *)
val stop_interactive : solver_session -> unit 

(** [solve ~timeout file solver] solve the formula in [file]
    with the given [~timeout] and the given [solver]. Only,
    the SMT status is returned *)
val solve:
  ?timeout:int ->
  string -> Common_piqi.solver_t -> Smtlib2.smt_result 

(** [solve_incremental ~expr ~debug session solver] solve
    the current formula fed to the solver. An optional 
    smt_expr [~expr] can be provided which would be added
    first. *)
val solve_incremental :
  ?f:Smtlib2.smt_expr option ->
  ?file:string -> solver_session -> Common_piqi.solver_t -> Smtlib2.smt_result

(** same as [solve] but also returns the model generated *)
val solve_model :
  ?timeout:int ->
  string -> Common_piqi.solver_t -> Smtlib2.smt_result * Smt_model.t

(** same as [solve_model] but also returns the computation time *)
val solve_model_time :
  ?timeout:int ->
  ?get_model:bool ->
  string -> Common_piqi.solver_t -> Smtlib2.smt_result * Smt_model.t * float

(** same as [solve_incremental_model] but also returns the model generated *)
val solve_incremental_model :
  ?f:Smtlib2.smt_expr option ->
  ?file:string -> solver_session ->
  Common_piqi.solver_t -> Smtlib2.smt_result * Smt_model.t

(** same as [solve_incremental_model] but also returns the
    computation time *)
val solve_incremental_model_time :
  ?f:Smtlib2.smt_expr option ->
  ?file:string ->
  ?get_model:bool ->
  solver_session ->
  Common_piqi.solver_t -> Smtlib2.smt_result * Smt_model.t * float

(** same as [solve_incremental_model] but uses the smtlib2
    [get-value] rather than [get-model] *)
val solve_incremental_value :
  ?f:Smtlib2.smt_expr option -> 
  solver_session ->
  Common_piqi.solver_t ->
  ?file:string ->
  Smtlib2.smt_expr ->
  Smtlib2.smt_result * Smt_model.t

(** send the [push] command to the solver *)
val push : ?file:string -> solver_session -> unit

(** send the [pop] command to the solver *)
val pop : ?file:string -> solver_session -> unit
