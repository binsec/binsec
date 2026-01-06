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

include Binsec_base
include Binsec_kernel
include Binsec_unix
include Binsec_cli
module Kernel_core = Kernel_core
module Kernel_functions = Kernel_functions
module Kernel_options = Kernel_options
module Formula_options = Formula_options

module Formula_transformation = struct
  include Binsec_smtlib.Formula.Transformation

  let optimize_from_options ?(keep = Smtlib.Formula.VarSet.empty) ?is_controlled
      fm =
    let open Formula_options in
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
    optimize ~keep ?lst ~cst ~itv ~prn ~rbs ~row ~ssa ?is_controlled fm
end

module Smt_options = Smt_options
module Smtlib_to_formula = Smtlib_to_formula

module Dba_options =
  Cli.Options_from_logger
    (Dba_logger)
    (struct
      let name = "DBA"
      let shortname = "dba"
    end)

module Loader_options =
  Cli.Options_from_logger
    (Loader_logger)
    (struct
      let name = "Loader"
      let shortname = "loader"
    end)

module Isa_options =
  Cli.Options_from_logger
    (Isa_logger)
    (struct
      let name = "ISA"
      let shortname = "isa"
    end)
