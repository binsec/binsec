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

(** Pretty-printing modules & functions for DBA*)

module type DbaPrinter = sig
  val pp_code_address : Format.formatter -> Dba.address -> unit
  val pp_tag : Format.formatter -> Dba.tag -> unit
  val pp_binary_op : Format.formatter -> Dba.binary_op -> unit
  val pp_unary_op : Format.formatter -> Dba.unary_op -> unit
  val pp_cond: Format.formatter -> Dba.cond -> unit
  val pp_expr: Format.formatter -> Dba.expr -> unit
  val pp_instruction : Format.formatter -> Dba.instruction -> unit
  val pp_lhs :  Format.formatter -> Dba.lhs -> unit
  val pp_region : Format.formatter -> Dba.region -> unit
end

module type Renderer = sig
  val binary_ops : (Dba.binary_op * string) list
  val unary_ops : (Dba.unary_op * string) list
  val endiannesses : (Dba.endianness * string) list
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
