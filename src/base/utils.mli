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

(** Generic utilities *)

(** {2 Option types }*)
val get_opt_or_default : 'a -> 'a option -> 'a
(** [get_opt_or_default default vopt] returns [default] if [vopt] is None, the
    contents of [vopt] otherwise *)

val unsafe_get_opt : 'a option -> 'a
(** [unsafe_get_opt vopt] retrieves the contents of [vopt].
    Raise [Assert_failure] if [vopt] is [None].
*)

val is_none : 'a option -> bool
(** [is_none vopt] tests if [vopt] is indeed [None]. *)

(** {2 Timing } *)

val time: (unit -> 'a) -> float * 'a
(** [time f] times the execution of function f and returns both the time taken
    and the result *)

