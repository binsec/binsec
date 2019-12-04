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

include Cli.S

type domain =
    | TaintedKset
    | Kset
    | Interval

(** Command line options for abstract interpretation *)
module Domain : Cli.GENERIC with type t = domain

module FailSoftMode : Cli.BOOLEAN

module X86FlagPatterns : Cli.BOOLEAN

module KSetSize : Cli.INTEGER

(** Specific variables for statistics computations *)
val finalsize : int ref
val initsize : int ref
val ftemps: int ref
val itemps: int ref
val fflags: int ref
val iflags: int ref

val nb_refined_lhs: int ref
val nb_equalities_names: int ref
val nb_equalities_classes: int ref
val nb_equalities_refinement: int ref

val nb_nat_predicate_recovery_tries : int ref
val nb_recovered_nat_predicates : int ref
val nb_failed_nat_predicate_recoveries : int ref
val nb_conditional_cache_uses : int ref

val time_nat_flag_recovery: float ref
val time_analysis: float ref
val time_redundant_evals: float ref
val time_equalities: float ref
val time_simpl : float ref
val time_disas : float ref
