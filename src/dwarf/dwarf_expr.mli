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

val map : int -> Dba.Expr.t
(** [map n] according to the DWARF Register Number Mapping *)

type t

val load : [ `x32 | `x64 ] -> Loader_buf.cursor -> t
(** [load cursor] read a DWARF expression at the current cursor position *)

val loc : ?cfa:Dba.Expr.t -> t -> Dba.Expr.t
(** [loc ~cfa expr] interpret the expression expr
    according to the Canonical Frame Address *)

include Sigs.PRINTABLE with type t := t
