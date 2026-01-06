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

module Logger = Logger
module Expr = Expr
module Cunit = Cunit
module Frame = Frame
module Lines = Lines
module Loclist = Loclist
module Int = Basic_types.Integers.Int

type t = {
  isa : Machine.isa;
  units : Cunit.t list;
  frame : Frame.t;
  lines : Lines.t;
  loc : Loclist.t Int.Map.t;
}

let load img : t =
  let isa = Loader.Img.arch img in
  let units = Cunit.load img in
  let frame = Frame.load img in
  let lines = Lines.load img in
  let loc = Loclist.load img in
  { isa; units; frame; lines; loc }

let pp ppf debug : unit =
  Format.fprintf ppf "@[<v 2>Contents of the .debug_info section:@ ";
  List.iter
    (fun unit ->
      Format.pp_print_space ppf ();
      Cunit.pp ppf unit)
    debug.units;
  Format.fprintf ppf "@]@ @[<v>Contents of the frame section:@ @ ";
  Frame.pp ppf debug.frame;
  Format.fprintf ppf
    "@]@ @[<v>Decoded dump of debug contents of section .debug_line:@ @ ";
  Lines.pp ppf debug.lines;
  (match Int.Map.find 0 debug.loc with
  | exception Not_found -> ()
  | loclist ->
      Format.fprintf ppf "@]@ @[<v>Contents of the .debug_loc section:@ @ ";
      Loclist.pp ppf (debug.isa, loclist));
  Format.pp_close_box ppf ()
