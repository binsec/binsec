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

type t =
  {
    units : Dwarf_cunit.t list;
    frame : Dwarf_frame.t;
    lines : Dwarf_lines.t;
  }

let load img : t =
  let units = Dwarf_cunit.load img in
  let frame = Dwarf_frame.load img in
  let lines = Dwarf_lines.load img in
  {units; frame; lines}

let pp ppf debug : unit =
  Format.fprintf ppf "@[<v 2>Contents of the .debug_info section:@ ";
  List.iter
    (fun unit -> Format.pp_print_space ppf (); Dwarf_cunit.pp ppf unit)
    debug.units;
  Format.fprintf ppf "@]@ @[<v>Contents of the frame section:@ @ ";
  Dwarf_frame.pp ppf debug.frame;
  Format.fprintf ppf
    "@]@ @[<v>Decoded dump of debug contents of section .debug_line:@ @ ";
  Dwarf_lines.pp ppf debug.lines;
  Format.pp_close_box ppf ()

let run () =
  if Kernel_options.ExecFile.is_set ()
     && Dwarf_options.is_enabled () then
    Dwarf_options.Logger.result "@\n%a" pp
     (load (Kernel_functions.get_img ()))

let _ =
  Cli.Boot.enlist ~name:"g" ~f:run
