(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
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

type t = { loc_start : Lexing.position; loc_end : Lexing.position }

let mk_loc loc_start loc_end = { loc_start; loc_end }

let in_file name =
  let loc =
    {
      Lexing.pos_fname = name;
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = -1;
    }
  in
  { loc_start = loc; loc_end = loc }

let none = in_file "_none_"
let dummy_loc = { loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos }

let pp_lines ppf loc =
  let lstart = loc.loc_start.Lexing.pos_lnum
  and lend = loc.loc_end.Lexing.pos_lnum in
  if lstart = lend then Format.fprintf ppf "%d" lstart
  else Format.fprintf ppf "%d-%d" lstart lend
