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

module Opacity_status : sig
  type t
  val is_clear : t -> bool
  val pp : Format.formatter -> t -> unit
end

module Check : sig

  type t = Opacity_status.t Virtual_address.Map.t ;;

  val vertex : cfg:Instr_cfg.t -> Instr_cfg.V.t -> Opacity_status.t option ;;

  val vertices : cfg:Instr_cfg.t -> Instr_cfg.V.t list -> t ;;

  val addresses : cfg:Instr_cfg.t -> Virtual_address.t list -> t ;;

  val graph : cfg:Instr_cfg.t -> t ;;

  val file : filename:string -> t ;;

  val subset : unit -> t ;;

  val sections : Basic_types.String.Set.t ->  t ;;

  val all : unit -> t ;;
end
