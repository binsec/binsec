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

exception Assert_failure of Dba_types.Statement.t
exception Assume_failure of string
exception Assignment_size_conflict of string
exception Regions_conflict of string
exception Bad_region of string
exception Bad_bound of string
exception Undeclared_variable of string
exception Uninitialized_variable of string
exception Bad_address_size
exception Bad_exp_size
exception Bad_condition of string
exception Operands_size_conflict of string
exception Unbound_region_element of string
exception Alternative_conflict_values
exception Unknown_value of string
exception Bad_concat of string
exception Freed_variable_access
exception Invalid_address of string
exception Invalid_free_address
exception Invalid_free_region
exception Read_permission_denied
exception Write_permission_denied
exception Exec_permission_denied
exception Size_error of string
exception Nonhomogeneous_sizes
exception Invalid_top_argument of string
exception Enumerate_Top
exception Stop_Unsupported of string
exception Empty_env
exception Div_by_zero


exception Mismatched_instruction_size of Dba.instruction
exception Mismatched_address_size of Dba.address

let invalid_address msg =
  raise (Invalid_address msg)

let not_yet_implemented msg =
  let msg = Format.sprintf "not_yet_implemented: %s" msg in
  failwith msg

let mismatched_instruction_size instruction =
  raise (Mismatched_instruction_size instruction)

let mismatched_address_size address =
  raise (Mismatched_address_size address)

let assert_failure a instr =
  raise (Assert_failure (Dba_types.Statement.create a instr))
