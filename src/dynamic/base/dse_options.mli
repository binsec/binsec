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

(** DSE *)
(** {2 DSE along multiple paths with exploration strategy } *)

include Cli.S

module RandomSeed : Cli.BOOLEAN

type strategy =
  | Dot
  | Dfs
  | Bfs
  | Random
  | Uaf
  | Uaf_Strcmp

module Strategy : Cli.GENERIC with type t = strategy
module Binary : Cli.STRING
module DSE_args : Cli.STRING

module Config_seed_file : Cli.STRING
module Max_trace_length : Cli.INTEGER
module Exploration_timeout : Cli.FLOAT
