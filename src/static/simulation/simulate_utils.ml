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

let mk_undef_value, get_nb_undef_builds =
  let n = ref 0 in
  (fun (size:int) -> incr n; `Undef size),
  (fun () -> !n)

let incr_undef_loads, get_undef_loads =
  let n = ref 0 in
  (fun () -> incr n),
  (fun () -> !n)


let mk_sup from size =
  let open Bigint in
  let big_size = big_int_of_int size in
  sub_big_int (add_big_int from big_size) unit_big_int
