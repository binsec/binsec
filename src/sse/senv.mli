(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2021                                               *)
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

module Query_stats : sig
  val pp : Format.formatter -> unit -> unit
end

module State (S : Smt_solver.Solver) : sig
  type t
  (** Symbolic state *)

  val empty : unit -> t

  val assume : Dba.Expr.t -> t -> t option

  val split_on :
    Dba.Expr.t ->
    ?n:int ->
    ?except:Bitvector.t list ->
    t ->
    (Bitvector.t * t) list

  val fresh : string -> int -> t -> t

  val assign : string -> Dba.Expr.t -> t -> t

  val write : addr:Dba.Expr.t -> Dba.Expr.t -> Machine.endianness -> t -> t

  val load_from : addr:Bitvector.t -> int -> t -> t

  val pp : Format.formatter -> t -> unit

  val pp_smt :
    ?slice:(Dba.Expr.t * string) list -> Format.formatter -> t -> unit

  val as_ascii : string -> t -> string
end
