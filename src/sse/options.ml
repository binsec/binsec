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
  let shortname = "sse"
  let name = "Static Symbolic Execution"
end)

module Engine = struct
  type t = ..

  module Htbl = Hashtbl.Make (struct
    type nonrec t = t

    let equal = ( = )
    let hash = Hashtbl.hash
  end)

  let engines = Basic_types.String.Htbl.create 4
  let factories = Htbl.create 4

  module Opt = Builder.Any_opt (struct
    type nonrec t = t

    let name = "engine"
    let doc = "Use the following symbolic engine"

    let of_string name =
      try Basic_types.String.Htbl.find engines name
      with Not_found ->
        if List.mem name (Plugins.Plugins.Plugins.list ()) then (
          Plugins.Plugins.Plugins.load name;
          Basic_types.String.Htbl.find engines name)
        else raise Not_found
  end)

  let set = Opt.set
  let get = Opt.get
  let is_set = Opt.is_set
  let is_default = Opt.is_default
  let get_opt = Opt.get_opt

  let register name engine factory =
    if Basic_types.String.Htbl.mem engines name then
      Logger.fatal "Engine name %S has already been taken." name;
    (match Htbl.find factories engine with
    | name', factory' when factory != factory' ->
        Logger.fatal "Trying to overwrite engine %S with %S" name' name
    | (exception Not_found) | _ -> ());
    Basic_types.String.Htbl.add engines name engine;
    Htbl.add factories engine (name, factory)

  let get_factory () =
    match Htbl.find factories (get ()) with
    | _, f -> f ()
    | exception Not_found ->
        Logger.fatal "Trying to load an unregistered engine"
end

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

module KillFlagsAtReturn = Builder.No (struct
  let name = "kill-flags-at-return"
  let doc = "Conservatively always consider flags alive at function return"
end)

module Randomize = Builder.False (struct
  let name = "randomize"
  let doc = "randomize path selection"
end)

module ScriptFiles = Builder.String_list (struct
  let name = "script"
  let doc = "set file containing initializations, directives and stubs"
end)

type warnerror = Error | Warn | Quiet

module MissingSymbol = Builder.Variant_choice_assoc (struct
  type t = warnerror

  let name = "missing-symbol"

  let doc =
    "Select how to handle function replacement when the symbol is not resolved \
     from the binary"

  let assoc_map = [ ("error", Error); ("warn", Warn); ("quiet", Quiet) ]
  let default = Error
end)

module Timeout = Builder.Integer_option (struct
  let name = "timeout"
  let doc = "Sets a timeout in second for symbolic execution"
end)

module Monitor = Builder.No (struct
  let name = "screen"
  let doc = "Disable the monitor screen (curses)"
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
