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

include Cli.Options (struct
  let shortname = "smt"
  let name = "Static Symbolic Execution"
end)

type solver =
  | Auto (* try to use the best SMT solver available; in order *)
  | Bitwuzla_native (* bitwuzla native ocaml binding *)
  | Bitwuzla_smtlib (* bitwuzla external process *)
  | Boolector_smtlib (* boolector external process *)
  | Z3_smtlib (* z3 external process *)
  | CVC4_smtlib (* cvc4 external process *)
  | Yices_smtlib
(* yices external process *)

module SMTSolver = Builder.Variant_choice_assoc (struct
  type t = solver

  let assoc_map =
    [
      ("auto", Auto);
      ("bitwuzla", Bitwuzla_smtlib);
      ("bitwuzla:native", Bitwuzla_native);
      ("bitwuzla:smtlib", Bitwuzla_smtlib);
      ("boolector", Boolector_smtlib);
      ("boolector:smtlib", Boolector_smtlib);
      ("z3", Z3_smtlib);
      ("z3:smtlib", Z3_smtlib);
      ("cvc4", CVC4_smtlib);
      ("cvc4:smtlib", CVC4_smtlib);
      ("yices", Yices_smtlib);
      ("yices:smtlib", Yices_smtlib);
    ]

  let default = Auto
  let name = "solver"
  let doc = "Manually set the SMT solver to use."
end)

module KeepGoing = Builder.False (struct
  let name = "keep-going"
  let doc = "Ignore errors returned by the SMT solver. Default is to abort."
end)

module SMT_dir = Builder.String_option (struct
  let name = "dir"
  let doc = "set directory to cache smt scripts"
end)

module SMT_log_directory = Builder.String (struct
  let name = "dump-dir"
  let doc = "Set directory where unsolved SMT scripts are dumped"
  let default = "binsec_smtdump"
end)
