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

module Printer : sig
  type t

  val create :
    ?word_size:int ->
    ?debug:(name:string -> label:string -> string) ->
    next_id:Suid.t ->
    unit ->
    t

  val visit_bl : t -> Sexpr.Expr.t -> unit

  val visit_bv : t -> Sexpr.Expr.t -> unit

  val visit_ax : t -> Sexpr.Memory.t -> unit

  val pp_print_decls : Format.formatter -> t -> unit

  val pp_print_defs : Format.formatter -> t -> unit

  val pp_print_bl : t -> Format.formatter -> Sexpr.Expr.t -> unit

  val pp_print_bv : t -> Format.formatter -> Sexpr.Expr.t -> unit

  val pp_print_ax : t -> Format.formatter -> Sexpr.Memory.t -> unit
end

module Solver () : Solver_sig.S
