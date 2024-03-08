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

module Node : sig
  module T : sig
    type t = Entrypoint | Text | Plt

    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
  end

  type t

  val nid : t -> Graph.Dot_ast.id
  val func : t -> Ida_cfg.Function.t
  val typ : t -> T.t
  val create : ?nid:Graph.Dot_ast.id -> Ida_cfg.Function.t -> T.t -> t
  val pp : Format.formatter -> t -> unit
  val pp_short : Format.formatter -> t -> unit
  val pp_list : Format.formatter -> t list -> unit
  val equal : t -> t -> bool
end

module Edge : sig
  type t

  val src : t -> Node.t
  val dst : t -> Node.t
  val create : Node.t -> Node.t -> t
  val pp : Format.formatter -> t -> unit
  val pp_list : Format.formatter -> t list -> unit
end

include
  Cfg.S with type addr = Node.t and type inst = Node.t and type symb = Node.t

module Parse : sig
  val build_cg : cg_file:string -> t
end
