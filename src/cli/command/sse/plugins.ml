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
module StrTbl = Basic_types.String.Htbl

let plugins = StrTbl.create 8

let register ~is_enabled (plugin : unit -> (module PLUGIN)) =
  let module P = (val plugin ()) in
  if StrTbl.mem plugins P.name then
    Options.Logger.fatal "plugin name %s has already been registered" P.name;
  StrTbl.add plugins P.name (is_enabled, plugin)

let list () =
  StrTbl.fold
    (fun _ (is_enabled, plugin) plugins ->
      if is_enabled () then plugin () :: plugins else plugins)
    plugins []
