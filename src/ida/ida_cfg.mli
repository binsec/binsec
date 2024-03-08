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

module VA = Virtual_address
module H = VA.Htbl

module Function : sig
  type t = private Name of string | Address of VA.t

  val name : string -> t
  val addr : VA.t -> t
  val pp : Format.formatter -> t -> unit
  val pp_list : Format.formatter -> t list -> unit
  val same : t -> t -> bool
  val to_string : t -> string
  val compare : t -> t -> int
end

module S : sig
  type t

  val block : t -> VA.t
  val func : t -> Function.t
  val create : VA.t -> Function.t -> t
end

module C : sig
  include
    Cfg.S
      with type addr = VA.t
       and type inst = Instruction.t
       and type symb = S.t
end

module F : sig
  type t

  val create : name:string -> edges:(VA.t * VA.t) list -> eps:VA.Set.t -> t
  val of_list : name:string -> VA.t list -> t
  val eps : t -> VA.Set.t
  val blocks : t -> VA.t list H.t
  val edges : t -> (VA.t * VA.t) list
  val name : t -> string
  val calls : t -> (VA.t * VA.t) list H.t
  val pp_blocks : Format.formatter -> t -> unit
  val pp_edges : Format.formatter -> t -> unit
  val pp : Format.formatter -> t -> unit
  val all_leaders : t -> VA.t list
  val all_insts : t -> VA.t list
  val set_name : t -> string -> unit
  val set_eps : t -> VA.Set.t -> unit
  val set_blocks : t -> VA.t -> VA.t list -> unit
  val set_edges : t -> (VA.t * VA.t) list -> unit
  val set_calls : t -> VA.t -> (VA.t * VA.t) list -> unit
  val build_cfg : t -> C.t
end

module G : sig
  type t

  val graph : t -> C.t
  val ep : t -> VA.t option
  val funcs : t -> F.t H.t
  val add_function : t -> F.t -> unit
  val calls : t -> (VA.t * VA.t) list H.t
  val create : ?ep:VA.t -> unit -> t
  val pp : Format.formatter -> t -> unit
  val all_leaders : t -> VA.Set.t
  val add_calls : t -> caller:VA.t -> callee:VA.t -> return:VA.t -> unit
  val remove_edge : t -> C.V.t -> C.V.t -> unit
  val disassemble_vertex : t -> C.V.t -> unit
  val succ : t -> C.V.t -> C.V.t list
  val pred : t -> C.V.t -> C.V.t list
  val mem_vertex_a : t -> VA.t -> C.V.t option
  val ret_nodes : t -> F.t -> VA.t list
end

val do_cfg : simple:bool -> ida_file:string -> G.t
