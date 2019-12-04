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

module VA = Virtual_address

val to_vaddr : string -> VA.t
val strip_enclosing_chars : string -> string
val parse_calls : string -> (VA.t * VA.t * VA.t) list
val clean_mnemonic : string -> string
val to_supported : VA.t -> Mnemonic.t -> Mnemonic.t
val read_list : string -> string list

module Dot : sig
  val pp_id : Format.formatter -> Graph.Dot_ast.id -> unit
  val pp_a : Format.formatter ->
    Graph.Dot_ast.id * Graph.Dot_ast.id option -> unit
  val pp_attr : Format.formatter ->
    (Graph.Dot_ast.id * Graph.Dot_ast.id option) list -> unit
  val pp_attrs : Format.formatter ->
    (Graph.Dot_ast.id * Graph.Dot_ast.id option) list list -> unit
  val pp_node_id : Format.formatter -> Graph.Dot_ast.id * 'a -> unit
  val pp_node : Format.formatter -> Graph.Dot_ast.node -> unit
  val pp_stmt : Format.formatter -> Graph.Dot_ast.stmt -> unit
end
