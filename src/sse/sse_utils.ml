(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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

let sse_dirname = "binsec_sse"

let temp_file =
  let n = ref 0 in
  fun () ->
    incr n;
    let tmpdir = Sse_options.SmtDir.get () in
    let temp_dir = Filename.concat tmpdir sse_dirname in
    if not (Sys.file_exists temp_dir) then (
      Logger.debug ~level:6 "Creating directory %s" temp_dir;
      Unix.mkdir temp_dir 0o700
    );
    let filename =
      Filename.concat temp_dir @@ Printf.sprintf "sse_%d.smt2" !n in
    Logger.debug ~level:5 "Creating temporary %s" filename;
    filename


let mk_var_name basename idx =
  Format.sprintf "%s_%d" basename idx
