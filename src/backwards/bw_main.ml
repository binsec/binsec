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

open Bw_options

let run_opaque_predicates () =
  if is_enabled () && Opaque_predicates.get () then
    Opaque.Check.all ()
;;

let run_opaque_addresses () =
  if is_enabled () && Opaque_addresses.is_set () then
    Opaque.Check.subset ()
;;

let _ =
  Cli.Boot.enlist ~name:"opaque predicates" ~f:run_opaque_predicates;
  Cli.Boot.enlist ~name:"opaque predicates (specified addresses)"
    ~f:run_opaque_addresses;
;;
