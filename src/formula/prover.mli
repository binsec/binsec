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

type executable = string ;;
type arguments  = string array ;;

module Command : sig
  type t = private {
     executable : executable;
     arguments  : arguments;
  } ;;

  val to_string : t -> string ;;
end

type t = Formula_options.solver ;;

val pp : Format.formatter -> t -> unit ;;

val is_boolector : t -> bool ;;
val is_yices : t -> bool ;;

(** {2 Accessors} *)
val name_of : t -> string ;;

val command : ?incremental:bool -> int -> t -> Command.t ;;

val command_string : ?incremental:bool -> int -> t -> string ;;

val timeout_s : int -> t -> int ;;
