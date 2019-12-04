(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
(*    VERIMAG                                                             *)
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
(**************************************************************************)

open Config_piqi

module SymbolicInput :
sig
  type input_id_t
  type input_entry_t
  type input_entries_t

  val init                         : unit -> input_entries_t
  val concat                       : input_entries_t -> input_entries_t -> input_entries_t
  val length                       : input_entries_t -> int

  val get_input_entry_address      : input_entry_t -> int64
  val get_input_entry_iteration    : input_entry_t -> int
  val get_input_entry_name         : input_entry_t -> string
  val get_all_names                : input_entries_t -> string list
  val get_value_of_input           : input_entries_t -> string -> int64
  val get_input_entry_value        : input_entry_t -> int64

  val get_input_entry_register     : input_entry_t -> string option
  val get_input_entry_mem          : input_entry_t -> (int64 * int) option

  val add_inputs_mem               : input_entries_t -> int64 -> int -> int64 -> int -> bool -> input_entries_t
  val parse_inputs                 : Common.action -> Config_piqi.input_t list -> input_entries_t
  val parse_config_json_patch      : string -> input_entries_t
  val parse_config_json_conc       : string -> input_entries_t

  val export_inputs                : Common.action -> input_entries_t -> Config_piqi.input_t list
  (*  val update_config_json           : string -> ?new_file:string -> ?conc:input_entries_t -> ?patch:input_entries_t -> unit*)

  val find_input_from_name         : input_entries_t -> string -> input_entry_t
  val find_input_from_position     : input_entries_t -> int64 * int -> input_entry_t
  val find_all_input_from_position : input_entries_t -> int64 * int -> bool -> input_entries_t
  val remove_all_input_from_position : input_entries_t -> int64 * int -> bool -> input_entries_t

  val modify_input_entry_value     : input_entry_t -> int64 -> input_entry_t
  val create_mem_values            : int64 -> string -> (int64 * int64) list

  val to_list                      : input_entries_t -> input_entry_t list
  val to_list_order_decr           : input_entries_t -> input_entry_t list

  val update                       : input_entries_t -> Smt_model.t -> input_entries_t
end
