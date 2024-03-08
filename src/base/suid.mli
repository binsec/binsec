(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
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

type t = private string
(** This module is intended to generate valid SMTLib2 short identifiers
    Are valid any non-empty sequence of letters, digits
                  and the characters ~ ! @ $ % ^ & * _ - + < > . ? /
                  that does not start with a digit
    To never clash with reserved keywords, generated identifiers
    always start with one of the special characters *)

external to_string : t -> string = "%identity"
val pp : Format.formatter -> t -> unit
val zero : t
val incr : t -> t
val compare : t -> t -> int
