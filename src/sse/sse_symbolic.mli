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

val memory_name : string
val memory_type : Formula.sort
val path_constraint_name : string

(* the part of the symbolic store which is shared among every branch
 * inside mutability *)
module Store : sig
  module M = Basic_types.String.Map
  type infos = int * Formula.sort (* index, type *)
  type t

  val create : unit -> t
  val add_entry : t -> Formula.entry -> unit
end

(* the part of the symbolic store which is not shared *)
module State : sig
  module M = Basic_types.String.Map
  module S = Basic_types.Int64.Map
  type t = private {
    store : Store.t;
    initialisation : int S.t;
    var_index : int M.t;
  }
  val create : Store.t -> t
  val assign : t -> Store.M.key -> Formula.sort -> Formula.term -> t
  val declare : t -> M.key -> Formula.sort -> t

  val get_memory : t -> Formula.ax_term
  val get_path_constraint : t -> Formula.bl_term
  val get_bv : t -> M.key -> Size.Bit.t -> Formula.bv_term
  val merge : t -> t -> t
  val init_mem_at : addr:int64 -> size:int -> t -> t
  val get_entries : t -> Formula.formula
  val get_path_variables : t -> Formula.VarSet.t

  val has_empty_vinfos : t -> bool
  val copy_store :  t -> t

  val pp : Format.formatter -> t -> unit
end
