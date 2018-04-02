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

(** Display messages related to Abstract Interpretation *)

type message =
  | Join of Dba.address * string * string * string
  | Widen of Dba.address * string * string * string
  | MError of Dba_types.AddressStack.Map.key * string
  | Info of Dba_types.AddressStack.Map.key * string
  | Elements of Region_bitvector.t list
  | DelayedWidenings of Dba_types.AddressStack.Map.key * int
  | Widenings of Dba_types.AddressStack.Map.key
  | Unrollings of Dba_types.AddressStack.Map.key
  | Joins of Dba_types.AddressStack.Map.key
  | Equalities of string
  | Assign of string * Dba.expr * string
  | Guard of Dba.cond * string * string
  | Call of Dba.address * Dba.address
  | RemoveEqualities of Dba.lhs * string
  | Cond of Dba.cond * string
  | CondNat of Dba.address * Dba.cond * string
  | CondReplacement of Dba.cond * Dba.expr * Dba.expr * Dba.expr * Dba.expr * string
  | Predicates of Dba.binary_op list
  | Djmps of Dba_types.Caddress.Set.t Dba_types.AddressStack.Map.t
  | Stats_flags
  | Stats_equalities


val display : message -> unit

type 'a getset =
  { set : 'a -> unit;
    get : unit -> 'a;
    add : 'a -> unit;
  }

val analysis_time : float getset

val pp_results : unit -> unit

val save_evaluation_counts : unit -> unit
val restore_evaluation_counts : unit -> unit
val set_location_count : int -> unit

val increase_evaluation_count : unit -> unit
val increase_function_count : unit -> unit
val increase_lhs_evaluation_count : unit -> unit
val increase_lhseq_evaluation_count : unit -> unit

val equality_use : Dba_types.AddressStack.t -> Dba.lhs -> Dba.lhs -> unit
val get_equality_uses : unit -> int

val add_call : Bigint.t -> unit
