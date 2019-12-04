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

open Formula_options

let transform ~filename =
  let cst = OptimAll.get () || OptimCst.get () in
  let itv = OptimAll.get () || OptimItv.get () in
  let prn = OptimAll.get () || OptimPrn.get () in
  let rbs = OptimAll.get () || OptimRbs.get () in
  let row = OptimAll.get () || OptimRow.get () in
  let ssa = OptimAll.get () || OptimSsa.get () in
  let lst =
    let i = OptimLst.get () in
    if i = 0 then None else Some i
  in
  let smt_script =
    Parse_utils.read_file
      ~parser:Smtlib_parser.script ~lexer:Smtlib_lexer.token ~filename
  in
  Smtlib_to_formula.script smt_script
  (* itv & keep are set to their default values *)
  |> Formula_transformation.optimize
    ~is_controlled:(fun _ -> false)
    ~keep:Formula.VarSet.empty ?lst ~cst ~itv ~prn ~rbs ~row ~ssa
  |> Formula_to_smtlib.formula,
  match lst with
  | None -> "smt_simpl_out.smt2"
  | Some n -> Printf.sprintf "smt_simpl_out_%08i.smt2" n


let maybe_smt_transform_only () =
  if Formula_options.is_enabled () && Kernel_options.ExecFile.is_set () then begin
    let filename = Kernel_options.ExecFile.get () in
    if File_utils.has_suffix ~suffixes:[".smt"; ".smt2"] filename then
      transform ~filename
      |> (fun (script, file) -> Smtlib_pp.pp_tofile file script; exit 0)
    else begin
      Logger.error "Bad filename extension: %s" filename;
      exit 1;
    end
  end


let _ =
  Cli.Boot.enlist ~name:"transform" ~f:maybe_smt_transform_only
