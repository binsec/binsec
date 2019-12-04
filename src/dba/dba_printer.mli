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

(** Pretty-printing modules & functions for DBA*)

module type DbaPrinter = sig
  val pp_code_address : Format.formatter -> Dba.address -> unit
  val pp_tag : Format.formatter -> Dba.tag -> unit
  val pp_binary_op : Format.formatter -> Dba.Binary_op.t -> unit
  val pp_unary_op : Format.formatter -> Dba.Unary_op.t -> unit
  val pp_bl_term: Format.formatter -> Dba.Expr.t -> unit
  val pp_expr: Format.formatter -> Dba.Expr.t -> unit
  val pp_instruction : Format.formatter -> Dba.Instr.t -> unit
  val pp_lhs :  Format.formatter -> Dba.LValue.t -> unit
  val pp_region : Format.formatter -> Dba.region -> unit
  (* Print the instruction, but prints the explicit goto only if it
     does not go to (current_id + 1). *)
  val pp_instruction_maybe_goto : current_id:int -> Format.formatter -> Dba.Instr.t -> unit
end

module type Renderer = sig
  val binary_ops : (Dba.Binary_op.t * string) list
  val unary_ops : (Dba.Unary_op.t * string) list
  val endiannesses : (Machine.endianness * string) list
  val string_of_digit_char : char -> string
  val left_right_parentheses : string * string
end

module Make : functor(R: Renderer) -> DbaPrinter

(* EIC-prefixed modules consider that the code is endianness-independent.
   Therefore, they do not print any information regarding endianness.
*)
module Ascii : DbaPrinter
module EICAscii : DbaPrinter
module Unicode : DbaPrinter
module EICUnicode : DbaPrinter
