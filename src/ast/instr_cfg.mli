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

module type S = sig
  include Cfg.S

  val ordered_iter_vertex :
    compare:(vertex -> vertex -> int) -> (vertex -> unit) -> t -> unit

  val iter_vertex_by_address : (vertex -> unit) -> t -> unit

  val output_graph :
    out_channel -> t -> entry:vertex -> Virtual_address.t list -> unit

  val dump : filename:string -> t -> unit
end

module Make (H : Hashtbl.HashedType) :
  S
    with type addr = Virtual_address.t
     and type inst = Instruction.t
     and type symb = H.t

include
  S
    with type addr = Virtual_address.t
     and type inst = Instruction.t
     and type symb = string
