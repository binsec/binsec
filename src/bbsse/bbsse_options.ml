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
  let shortname = "bbsse"
  let name = "Backward Bounded Static Symbolic Execution"
end)

module MaxBB = Builder.Integer_list (struct
  let name = "max-basic-blocks"
  let doc = "Set the maximal number of basic blocks to process backward"
end)

module Consolidate = Builder.False (struct
  let name = "consolidate"
  let doc = "Use previous opaque predicate knowledge to cut paths early"
end)

module FindJumpsBetween = Builder.Integer_list (struct
  let name = "find-jumps"
  let doc = "Automatically find conditional jumps between these two addresses"
end)

module FindAllJumps = Builder.False (struct
  let name = "process-all-jumps"
  let doc = "Automatically find all conditional jumps in the executable"
end)

module CallsToProceed = Builder.Integer_set (struct
  let name = "calls-to-proceed"

  let doc =
    "List of call site that will not be automatically skipped by the analysis"
end)

module Directives = Builder.String_option (struct
  let name = "directives"
  let doc = "Path to a script file"
end)
