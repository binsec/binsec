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
module StMap = Basic_types.String.Map

val constant : Smtlib.Lang.constant -> Expr.t
(** [constant cst] parses the SMT-LIB constant [cst].

    @raise Invalid_argument
      if the SMT-LIB expression does not fit the supported fragment. *)

val expr : Expr.t StMap.t * Memory.t StMap.t -> Smtlib.Lang.term -> Expr.t
(** [expr env term] parses the SMT-LIB expression [term] using the variable
    mapping [env].

    @raise Invalid_argument
      if the SMT-LIB expression does not fit the supported fragment.
    @raise Not_found if a variable is not mapped in [env]. *)

val array : Expr.t StMap.t * Memory.t StMap.t -> Smtlib.Lang.term -> Memory.t
(** [array env term] parses the SMT-LIB expression [term] using the variable
    mapping [env].

    @raise Invalid_argument
      if the SMT-LIB expression does not fit the supported fragment.
    @raise Not_found if a variable is not mapped in [env]. *)
