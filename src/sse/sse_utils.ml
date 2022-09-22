(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2022                                               *)
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

open Sse_options

let mk_var_name basename idx = Format.sprintf "%s_%d" basename idx

let string_to_vaddr sloc acc =
  let img = Kernel_functions.get_img () in
  match Loader_utils.Binary_loc.(to_virtual_address ~img (of_string sloc)) with
  | Some vaddr -> Virtual_address.Set.add vaddr acc
  | None -> Logger.fatal "Unable to parse the address %s" sloc

let get_goal_addresses () =
  Basic_types.String.Set.fold string_to_vaddr (GoalAddresses.get ())
    Virtual_address.Set.empty

let get_avoid_addresses () =
  Basic_types.String.Set.fold string_to_vaddr (AvoidAddresses.get ())
    Virtual_address.Set.empty
