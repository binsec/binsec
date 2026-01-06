(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

open Types

type t
type access = Select of string * int | Store of string * int

val create :
  ?debug:(name:string -> label:string -> string) -> next_id:Suid.t -> unit -> t

val copy : t -> t
val visit_bl : t -> Expr.t -> unit
val visit_bv : t -> Expr.t -> unit
val visit_ax : t -> Memory.t -> unit
val pp_print_defs : Format.formatter -> t -> unit
val pp_flush_bl : t -> Format.formatter -> Expr.t -> string
val pp_flush_bv : t -> Format.formatter -> Expr.t -> string
val pp_flush_defs : Format.formatter -> t -> unit
val pp_print_bl : t -> Format.formatter -> Expr.t -> unit
val pp_print_bv : t -> Format.formatter -> Expr.t -> unit
val pp_print_ax : t -> Format.formatter -> Memory.t -> unit
val iter_free_variables : (string -> Expr.t -> unit) -> t -> unit
val iter_free_arrays : (string -> Memory.symbol -> unit) -> t -> unit
val fold_array_accesses : ('a -> access -> 'a) -> t -> Memory.t -> 'a -> 'a
val array_accesses_count : t -> Memory.t -> int
val pp_bv : Format.formatter -> Z.t -> int -> unit
