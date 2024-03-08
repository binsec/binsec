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

include Cli.Make (struct
  let shortname = "fml"
  let name = "Formulas"
end)

module OptimAll = Builder.No (struct
  let name = "optim-all"

  let doc =
    "Do not force all the optimizations (each optimization can still be set \
     individually)"
end)

module OptimCst = Builder.False (struct
  let name = "optim-cst"
  let doc = "Enable constant propagation"
end)

module OptimItv = Builder.False (struct
  let name = "optim-itv"
  let doc = "Enable intervals in read-over-write"
end)

module OptimPrn = Builder.False (struct
  let name = "optim-prn"
  let doc = "Enable pruning and inlining"
end)

module OptimRbs = Builder.False (struct
  let name = "optim-rbs"
  let doc = "Enable rebasing in read-over-write"
end)

module OptimRow = Builder.False (struct
  let name = "optim-row"
  let doc = "Enable read-over-write"
end)

module OptimSsa = Builder.False (struct
  let name = "optim-ssa"
  let doc = "Enable static single assignment"
end)

module OptimLst = Builder.Integer (struct
  let name = "optim-lst"
  let doc = "Switch to list-like memory representation in read-over-write"
  let default = 0
end)

module Flatten_memory = Builder.False (struct
  let name = "flat-mem"
  let doc = "Remove memory reads if indexes are constant"
end)

type solver = Boolector | Bitwuzla | Z3 | CVC4 | Yices

module Solver = struct
  include Builder.Variant_choice_assoc (struct
    type t = solver

    let assoc_map =
      [
        ("z3", Z3);
        ("cvc4", CVC4);
        ("yices", Yices);
        ("boolector", Boolector);
        ("bitwuzla", Bitwuzla);
      ]

    let default = Z3
    let name = "solver"
    let doc = " Set solver to use"
  end)

  module Timeout = Builder.Integer (struct
    let name = "solver-timeout"
    let doc = "Timeout for solver queries"
    let default = 5
  end)

  module Options = Builder.String_option (struct
    let name = "solver-options"
    let doc = "Use these options to launch the solver (ignore default options)"
  end)
end

module No_stitching = Builder.False (struct
  let name = "no-stitching"
  let doc = "Do not try to stitch together continuous stores/select"
end)
