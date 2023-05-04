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

open Options
open Types

let get_worklist () =
  match Search_heuristics.get () with
  | Dfs -> (module Heuristic.Dfs : WORKLIST)
  | Bfs -> (module Heuristic.Bfs : WORKLIST)
  | Nurs ->
      let seed =
        match Seed.get_opt () with
        | Some s -> s
        | None ->
            let v = Utils.random_max_int () in
            Logger.info "Random search seed is %d" v;
            Seed.set v;
            v
      in
      Random.init seed;
      (module Heuristic.Nurs : WORKLIST)

let get_state () =
  if AlternativeEngine.get () then
    Logger.warning
      "'-sse-alternative-engine' is now the default behavior. Use \
       '-sse-legacy-engine' to restore the previous one.";
  if LegacyEngine.get () then
    (module Sse_symbolic.State ((val Smt_solver.get_solver ())) : STATE_FACTORY)
  else (module Senv.State ((val Senv.get_solver_factory ())) : STATE_FACTORY)

let run () =
  if is_enabled () && Kernel_options.ExecFile.is_set () then
    let module S = Exec.Start ((val get_state ())) ((val get_worklist ())) in
    ()

let _ = Cli.Boot.enlist ~name:"SSE" ~f:run
