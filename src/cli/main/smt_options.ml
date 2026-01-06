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

include
  Cli.Options_from_logger
    (Smtlib.Logger)
    (struct
      let shortname = "smt"
      let name = "Static Symbolic Execution"
    end)

type solver =
  | Auto (* try to use the best SMT solver available; in order *)
  | Bitwuzla_builtin (* bitwuzla native ocaml binding (cxx) *)
  | Bitwuzla_legacy (* bitwuzla native ocaml binding (c) *)
  | Bitwuzla_smtlib (* bitwuzla external process *)
  | Boolector_smtlib (* boolector external process *)
  | Z3_builtin (* z3 native ocaml binding *)
  | Z3_smtlib (* z3 external process *)
  | CVC4_smtlib (* cvc4 external process *)
  | Yices_smtlib
(* yices external process *)

let text_backend : Smtlib.Solver.t -> Smtlib.Solver.backend =
 fun solver ->
  if Smtlib.Solver.ping solver then (
    Logger.debug "Found %a in the path." Smtlib.Solver.pp solver;
    Text { session = (module Smtlib.Solver.Session.Spawn); arg = solver })
  else
    Logger.fatal "%a is required but not available in path." Smtlib.Solver.pp
      solver

let backend : solver -> Smtlib.Solver.backend = function
  | Auto -> Smtlib.Solver.default_backend ()
  | Bitwuzla_builtin when Option.is_some Smtlib.Bindings.bitwuzla_cxx ->
      Logger.debug "Use native Bitwuzla binding (cxx).";
      Binding
        {
          factory = Option.get Smtlib.Bindings.bitwuzla_cxx;
          complete_fold_ax_values = true;
        }
  | (Bitwuzla_builtin | Bitwuzla_legacy)
    when Option.is_some Smtlib.Bindings.bitwuzla_c ->
      Logger.debug "Use native Bitwuzla binding (c).";
      Binding
        {
          factory = Option.get Smtlib.Bindings.bitwuzla_c;
          complete_fold_ax_values = true;
        }
  | Z3_builtin when Option.is_some Smtlib.Bindings.z3 ->
      Logger.debug "Use native z3 binding.";
      Binding
        {
          factory = Option.get Smtlib.Bindings.z3;
          complete_fold_ax_values = false;
        }
  | Bitwuzla_builtin | Bitwuzla_legacy ->
      Logger.fatal "Native bitwuzla binding is required but not available."
  | Z3_builtin ->
      Logger.fatal "Native z3 binding is required but not available."
  | Bitwuzla_smtlib -> text_backend Bitwuzla
  | Boolector_smtlib -> text_backend Boolector
  | Z3_smtlib -> text_backend Z3
  | CVC4_smtlib -> text_backend CVC4
  | Yices_smtlib -> text_backend Yices

module Solver = Builder.Variant_choice_assoc (struct
  type t = solver

  let assoc_map =
    [
      ("auto", Auto);
      ("bitwuzla", Bitwuzla_smtlib);
      ("bitwuzla:native", Bitwuzla_legacy);
      ("bitwuzla:legacy", Bitwuzla_legacy);
      ("bitwuzla:builtin", Bitwuzla_builtin);
      ("bitwuzla:smtlib", Bitwuzla_smtlib);
      ("boolector", Boolector_smtlib);
      ("boolector:smtlib", Boolector_smtlib);
      ("z3", Z3_smtlib);
      ("z3:builtin", Z3_builtin);
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

module Timeout = Builder.Float_option (struct
  let name = "timeout"
  let doc = "set the per query timeout"
end)

module Multichecks = Builder.False (struct
  let name = "multi-checks"
  let doc = "enable forward incremental solving"
end)

module DumpDir = Builder.String_option (struct
  let name = "dir"
  let doc = "set directory to cache smt scripts"
end)

module Theory = Builder.String (struct
  let name = "theory"
  let doc = "Select the formula theory"
  let default = "QF_ABV"
end)

module Utils = struct
  let sse_dirname = "binsec_sse"

  let mk_file ~dir =
    let n = ref 0 in
    fun () ->
      incr n;
      let temp_dir = dir () in
      if not (Sys.file_exists temp_dir) then (
        Logger.debug ~level:6 "Creating directory %s" temp_dir;
        Unix.mkdir temp_dir 0o700);
      let filename =
        Filename.concat temp_dir @@ Printf.sprintf "sse_%d.smt2" !n
      in
      Logger.debug ~level:5 "Creating temporary %s" filename;
      filename

  let temp_file =
    let dir () =
      let tmpdir = DumpDir.get () in
      Filename.concat tmpdir sse_dirname
    in
    mk_file ~dir
end

let fresh_file () =
  if DumpDir.is_set () then (
    let filename = Utils.temp_file () in
    Logger.debug ~level:3 "@[<h>Using SMT script file %s@]" filename;
    Some filename)
  else None
