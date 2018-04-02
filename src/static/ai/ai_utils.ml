(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
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

open Dba_types

let unrolled_loops_at_address addr unrolled_loops =
  try Caddress.Map.find addr unrolled_loops
  with Not_found -> Caddress.Set.empty

let unroll_current_loop addr loop unrolled_loops limit =
  if Caddress.Set.cardinal unrolled_loops = 1
  then
    if Caddress.Set.mem addr unrolled_loops
    then min (loop + 1) limit, 0
    else 0, min (loop + 1) limit
  else loop, loop
