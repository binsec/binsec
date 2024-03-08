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

(** Definition of command-line & programmatic options for SSE *)

include Cli.Cli_sig

type solver =
  | Auto  (** try to use the best SMT solver available; in order *)
  | Bitwuzla_native  (** bitwuzla native ocaml binding *)
  | Bitwuzla_smtlib  (** bitwuzla external process *)
  | Boolector_smtlib  (** boolector external process *)
  | Z3_smtlib  (** z3 external process *)
  | CVC4_smtlib  (** cvc4 external process *)
  | Yices_smtlib  (** yices external process *)

module SMTSolver : Cli.GENERIC with type t = solver
module KeepGoing : Cli.BOOLEAN
module SMT_dir : Cli.STRING_OPT
module SMT_log_directory : Cli.STRING
