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

val map : Machine.isa -> int -> Dba.Expr.t
(** [map isa n] according to the DWARF Register Number Mapping *)

type t

val load : Machine.isa -> [ `x32 | `x64 ] -> int -> int Reader.t -> t
(** [load isa blocksize cursor] read a DWARF expression at the current cursor position *)

val loc : Machine.isa -> ?cfa:Dba.Expr.t -> t -> int -> Dba.Expr.t
(** [loc isa ~cfa expr bitsize] interpret the expression expr
    according to the Canonical Frame Address *)

val cfa : Machine.isa -> t -> Dba.Expr.t
(** [cfa isa expr] interpret the expression expr as a Canonical Frame Address *)

include Sigs.PRINTABLE with type t := t
