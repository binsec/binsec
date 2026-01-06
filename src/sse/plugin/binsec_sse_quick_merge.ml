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

type ('value, 'model, 'state, 'path, 'a) field_id +=
  | Sibling : ('value, 'model, 'state, 'path, 'path option) field_id
  | Waiting : ('value, 'model, 'state, 'path, bool) field_id

type Ir.builtin += Join of Dba.Expr.t

module Plugin : PLUGIN = struct
  let name = "quick-merge"

  let fields _ =
    [
      Field { id = Sibling; default = None; copy = None; merge = None };
      Field { id = Waiting; default = false; copy = None; merge = None };
    ]

  let extensions :
      type a. (module ENGINE with type Path.t = a) -> a extension list =
   fun engine ->
    let module Engine = (val engine) in
    let module Path = Engine.Path in
    let sibling = Engine.lookup Sibling and waiting = Engine.lookup Waiting in
    let release path =
      Path.set path sibling None;
      if Path.get path waiting then (
        Logger.info "wake up";
        Path.set path waiting false;
        Engine.resume path Return)
    in
    [
      Builtin_printer
        (fun ppf -> function
          | Join e ->
              Format.fprintf ppf "join if %a is symbolic"
                Dba_printer.Ascii.pp_expr e;
              true
          | _ -> false);
      Instrumentation_routine
        (fun graph ->
          Revision.iter_new_vertex
            (fun vertex ->
              match Revision.node graph vertex with
              | Fallthrough { kind = Assume exp | Assert exp; _ }
              | Branch { test = exp; _ }
              | Terminator { kind = Jump { target = exp; _ }; _ } ->
                  Revision.insert_before graph vertex (Builtin (Join exp))
              | _ -> ())
            graph);
      Fork_callback
        (fun path0 path1 ->
          Option.iter release (Path.get path0 sibling);
          Path.set path0 sibling (Some path1);
          Path.set path1 sibling (Some path0));
      Builtin_resolver
        (function
        | Join exp ->
            Call
              (fun path0 ->
                match Path.get path0 sibling with
                | Some path1 when Path.is_symbolic path0 exp ->
                    Path.set path0 sibling None;
                    if Path.get path1 waiting then (
                      Path.set path1 waiting false;
                      Merge ([ path1 ], Return))
                    else (
                      Path.set path0 waiting true;
                      Signal Stashed)
                | None | Some _ -> Return)
        | _ -> Unknown);
      Signal_callback
        (fun path0 status ->
          match Path.get path0 sibling with
          | Some path1 when status <> Stashed || not (Path.get path0 waiting) ->
              release path1
          | None | Some _ -> ());
    ]
end
