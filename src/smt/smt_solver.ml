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

let solvers =
  let open Formula_options in
  [ Bitwuzla; Boolector; Z3; CVC4; Yices ]

let map =
  let open Formula_options in
  let open Smt_options in
  function
  | Auto | Bitwuzla_native -> assert false
  | Bitwuzla_smtlib -> Bitwuzla
  | Boolector_smtlib -> Boolector
  | Z3_smtlib -> Z3
  | CVC4_smtlib -> CVC4
  | Yices_smtlib -> Yices

let get_solver () =
  let open Formula_options in
  let open Smt_options in
  match SMTSolver.get () with
  | (Auto | Bitwuzla_native) when Smt_bitwuzla.available ->
      (module Smt_bitwuzla : Smt_sig.Solver)
  | Auto -> (
      try
        let solver = List.find Prover.ping solvers in
        Logger.info "Found %a in the path." Prover.pp solver;
        Solver.set solver;
        (module Smt_external.Solver : Smt_sig.Solver)
      with Not_found -> Logger.fatal "No SMT solver found.")
  | Bitwuzla_native ->
      Logger.fatal "Native bitwuzla binding is required but not available."
  | solver when Prover.ping (map solver) ->
      Solver.set (map solver);
      (module Smt_external.Solver : Smt_sig.Solver)
  | solver ->
      Logger.fatal "%a is required but not available in path." Prover.pp
        (map solver)
