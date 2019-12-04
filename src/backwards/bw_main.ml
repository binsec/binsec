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

open Bw_options

let run_opaque_predicates () =
  if is_enabled () && Opaque_predicates.get () then
    ignore @@ Opaque.Check.all ()
;;

let run_opaque_addresses () =
  if is_enabled () && Opaque_addresses.is_set () then
    ignore @@ Opaque.Check.subset ()
;;

let run_opaque_sections () =
  if is_enabled () && Opaque_sections.is_set () then
    let sections = Basic_types.String.Set.of_list @@ Opaque_sections.get () in
    ignore @@ Opaque.Check.sections sections
;;

let _ =
  Cli.Boot.enlist ~name:"opaque predicates" ~f:run_opaque_predicates;
  Cli.Boot.enlist ~name:"opaque predicates (specified addresses)"
    ~f:run_opaque_addresses;
  Cli.Boot.enlist ~name:"opaque predicates (sections)" ~f:run_opaque_sections;
;;
