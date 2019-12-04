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
  let shortname = "dse"
  let name = "Dynamic Symbolic Execution"
end
)


module RandomSeed =
  Builder.False(
  struct
    let name = "random"
    let doc = "Use random seed"
  end
  )

type strategy =
  | Dot
  | Dfs
  | Bfs
  | Random
  | Uaf
  | Uaf_Strcmp

module Strategy =
  Builder.Variant_choice (
  struct
    let name = "strategy"
    type t = strategy

    let assoc = [
      Dot, "dot";
      Dfs, "dfs";
      Bfs, "bfs";
      Random, "random";
      Uaf, "uaf";
      Uaf_Strcmp, "uaf-strcmp";
    ]

    let to_string v = List.assoc v assoc

    let of_string s =
      let rec loop l =
        match l with
        | (v, name) :: l ->
          if String.compare name s = 0 then v else loop l
        | [] -> assert false
      in loop assoc

    let default = Dfs
    let choices = List.map snd assoc
    let doc = "Exploration strategy"
  end
  )

module Binary =
  Builder.String (
  struct
    let name = "bin"
    let default = "/tmp/bin"
    let doc = "Binary to explore"
  end
  )

module DSE_args =
  Builder.String (
  struct
    let name = "args"
    let doc = "Argv"
    let default = "aa"
  end
  )

module Max_trace_length =
  Builder.Integer(
  struct
    let name = "length"
    let default = 5000
    let doc = "Set trace length limit"
  end
  )

module Config_seed_file =
  Builder.String (
  struct
    let name = "config"
    let default = "/tmp/bin.json"
    let doc = "Bootstrap json configuration file"
  end
  )

module Exploration_timeout =
  Builder.Float (
  struct
    let name = "timeout"
    let default = -1.0
    let doc = "Timeout for exploration"
  end
  )
