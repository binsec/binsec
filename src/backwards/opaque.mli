(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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

module Opacity_status : sig
  type t
  val is_clear : t -> bool
  val pp : Format.formatter -> t -> unit
end

module Check : sig

  val vertex : cfg:Instr_cfg.t -> Instr_cfg.V.t -> unit ;;

  val vertices : cfg:Instr_cfg.t -> Instr_cfg.V.t list -> unit ;;

  val addresses : cfg:Instr_cfg.t -> Virtual_address.t list -> unit ;;

  val graph : cfg:Instr_cfg.t -> unit ;;

  val file : filename:string -> unit ;;

  val subset : unit -> unit ;;
  val all : unit -> unit ;;
end
