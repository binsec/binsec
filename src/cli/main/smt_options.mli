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

(** Definition of command-line & programmatic options for SSE *)

include Cli.Cli_sig

type solver =
  | Auto  (** try to use the best SMT solver available; in order *)
  | Bitwuzla_builtin  (** bitwuzla native ocaml binding (cxx) *)
  | Bitwuzla_legacy  (** bitwuzla native ocaml binding (c) *)
  | Bitwuzla_smtlib  (** bitwuzla external process *)
  | Boolector_smtlib  (** boolector external process *)
  | Z3_builtin  (** z3 native ocaml binding *)
  | Z3_smtlib  (** z3 external process *)
  | CVC4_smtlib  (** cvc4 external process *)
  | Yices_smtlib  (** yices external process *)

val backend : solver -> Smtlib.Solver.backend

module Solver : Cli.GENERIC with type t = solver
module Timeout : Cli.FLOAT_OPT
module Multichecks : Cli.BOOLEAN
module DumpDir : Cli.STRING_OPT
module Theory : Cli.STRING

val fresh_file : unit -> string option
