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

include Cli.Make(
struct
  let name = "DBA simplifications"
  let shortname = "dba-simp"
end
)

module Display_statistics =
  Builder.False(
  struct
    let name = "show-stats"
    let doc =
      "Display statistical information regarding DBA simplifications"
  end
  )

type pmap =
  (Dba.Instr.t * Instruction.Generic.t option)
    Dba_types.Caddress.Map.t


type specifics =
  | All
  | NoInline
  | NoSummaries

type simplification =
  | No_simplification
  | Program
  | Function of specifics
  | Sequence of specifics

module Simplification = Builder.Variant_choice_assoc(
struct
  type t = simplification

  let assoc_map = [
      "prog", Program;
      "fun", Function All;
      "seq", Sequence All;
      "fun-no-inline", Function NoInline;
      "seq-no-inline", Function NoInline;
      "fun-no-sum", Function NoSummaries;
      "seq-no-sum", Function NoSummaries;
      "none", No_simplification;
    ]

  let default = No_simplification
  let name = "simplify"
  let doc = " Activate DBA simplification on given level"
end
)
