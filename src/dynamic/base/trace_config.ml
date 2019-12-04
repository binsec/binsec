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

(** Trace format either a file or network stream *)
type trace_format =
  | Chunked of Pervasives.in_channel * bool
  | Stream of string

(** Configuration sent to DSE analyses with a configuration,
    filename, [trace_format] *)
type t = {
  mutable configuration: Config_piqi.Configuration.t;
  mutable trace_file: string;
  mutable trace_input: trace_format;
}

(** Hold the current DSE configuration *)
let default = {
  configuration = Config_piqi.default_configuration ();
  trace_file = "";
  trace_input = Chunked (Pervasives.stdin, false);
}


let is_remote c =
  match c.trace_input with
  | Chunked _ -> false
  | Stream _ -> true
