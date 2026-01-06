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

open Types

module Flags : PLUGIN = struct
  let name = "sse-kill-flags-at-return"
  let fields _ = []

  let extensions :
      type a.
      (module Types.ENGINE with type Path.t = a) -> a Types.extension list =
   fun engine ->
    let module Engine = (val engine) in
    let module Isa = (val Isa_helper.get Engine.isa) in
    let shordlived_flags = Isa.get_shortlived_flags () in
    if shordlived_flags = [] then []
    else
      [
        Instrumentation_routine
          (fun graph ->
            Revision.iter_exits
              (fun vertex ->
                if Revision.is_new_vertex graph vertex then
                  match Revision.node graph vertex with
                  | Terminator
                      {
                        kind =
                          Goto { tag = Return; _ } | Jump { tag = Return; _ };
                        _;
                      } ->
                      List.iter
                        (fun var ->
                          Revision.insert_before graph vertex (Forget var))
                        shordlived_flags
                  | _ -> ())
              graph);
      ]
end
