(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2023                                               *)
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
  let shortname = "sse"

  let name = "Static Symbolic Execution"
end)

module AlternativeEngine = Builder.False (struct
  let name = "alternative-engine"

  let doc = "Enable the experimental engine"
end)

module LegacyEngine = Builder.False (struct
  let name = "legacy-engine"

  let doc =
    "Use the legacy engine. Some features are not or hardly supported (e.g. \
     core dump, custom arrays, etc.)."
end)

module MaxDepth = Builder.Integer (struct
  let name = "depth"

  let default = 1000

  let doc = "Set exploration maximal depth"
end)

module TransientEnum = Builder.Integer (struct
  let name = "self-written-enum"

  let default = 0

  let doc = "Set maximum number of forks for symbolic instruction opcodes"
end)

module JumpEnumDepth = Builder.Integer (struct
  let name = "jump-enum"

  let default = 3

  let doc = "Set maximum number of jump targets to retrieve for dynamic jumps"
end)

module QMerge = Builder.Integer (struct
  let name = "qmerge"

  let default = 0

  let doc = "Set maximum look ahead depth for quick merging"
end)

module Randomize = Builder.False (struct
  let name = "randomize"

  let doc = "randomize path selection"
end)

module ScriptFiles = Builder.String_list (struct
  let name = "script"

  let doc = "set file containing initializations, directives and stubs"
end)

module Timeout = Builder.Integer_option (struct
  let name = "timeout"

  let doc = "Sets a timeout in second for symbolic execution"
end)

type search_heuristics = Dfs | Bfs | Nurs

module Search_heuristics = Builder.Variant_choice_assoc (struct
  type t = search_heuristics

  let name = "heuristics"

  let doc = "Use the following search heuristics"

  let default = Dfs

  let assoc_map = [ ("dfs", Dfs); ("bfs", Bfs); ("nurs", Nurs) ]
end)

module Seed = Builder.Integer_option (struct
  let name = "seed"

  let doc = "Give a specific seed for random number generators"
end)
