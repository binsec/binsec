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

(** Abstract representation of configuration data for static analyses *)

type instruction_kinds = Dba.Instr.t list

type widening_delay = int

module BoundThreshold : sig
  type t = private
    { los : int array;
      his : int array; }

  val mk_from_list : int list -> int list -> t
end


module WideningThreshold : sig
  type t = private
    { signed : BoundThreshold.t;
      unsigned : BoundThreshold.t; }

  val mk : BoundThreshold.t -> BoundThreshold.t -> t

  val flatten_to_arrays_tuple :
    t -> int array * int array * int array * int array
end


val default_global_widening_thresholds : WideningThreshold.t

val default_global_widening_delay : int

(** {2 Configuration definition} *)
type t = private {
  entry_points : Virtual_address.Set.t;
  jumps : Dba.addresses Dba_types.Caddress.Map.t;
  allowed_jumpzones : (Dba.address * Dba.address) list;
  stops : Dba_types.Caddress.Set.t ;
  prepend_stubs : instruction_kinds Dba_types.Caddress.Map.t;
  substitute_stubs : Dba_types.instruction_sequence Dba_types.Caddress.Map.t;
  linear_addresses :
    (Virtual_address.t * Virtual_address.t
    ) list;
  global_widening : WideningThreshold.t * widening_delay;
  local_widening_thresholds : WideningThreshold.t Dba_types.Caddress.Map.t;
  local_widening_delays : widening_delay Dba_types.Caddress.Map.t;
}


(** {3 Constructors and modificators } *)
val default : t
val empty : t


val set_entry_points : Virtual_address.Set.t -> t -> t
val has_entry_points : t -> bool

val set_jumps : Dba.addresses Dba_types.Caddress.Map.t -> t -> t

val set_stops : Dba_types.Caddress.Set.t -> t -> t

val set_prepend_stubs : instruction_kinds Dba_types.Caddress.Map.t -> t -> t

val set_substitute_stubs : Dba_types.instruction_sequence Dba_types.Caddress.Map.t -> t -> t

val set_allowed_jumpzones :
  (Dba.address * Dba.address) list -> t -> t

val set_linear_addresses :
  (Dba.address * Dba.address) list -> t -> t

val set_global_widening_delay : widening_delay -> t -> t

val set_global_widening_thresholds : WideningThreshold.t -> t -> t
