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

open Sexpr

module type S = sig
  val visit_formula : Expr.t list -> unit
  val iter_free_variables : (string -> Expr.t -> unit) -> unit
  val iter_free_arrays : (string -> Memory.t -> unit) -> unit
  val assert_formula : Expr.t -> unit
  val assert_distinct : Expr.t -> Expr.t -> unit
  val check_sat : ?timeout:float -> unit -> Libsolver.status
  val check_sat_assuming : ?timeout:float -> Expr.t -> Libsolver.status
  val get_value : Expr.t -> Z.t
  val fold_array_values : (Z.t -> Z.t -> 'a -> 'a) -> Memory.t -> 'a -> 'a
  val push : unit -> unit
  val pop : unit -> unit
  val close : unit -> unit
end

module type OPEN = functor () -> S

type lazy_memory = {
  addr_space : int;
  content : (Z.t * Loader_buf.t) Imap.t;
  mutable lemmas : Expr.t list;
}

type result = Sat of Model.t | Unsat | Unknown

module type GET_MODEL = sig
  val check_sat : ?timeout:float -> lazy_memory -> Expr.t list -> result

  val fold_values :
    ?timeout:float ->
    lazy_memory ->
    Expr.t list ->
    Expr.t ->
    n:int ->
    except:Bv.t list ->
    (Bv.t -> Model.t -> 'a -> 'a) ->
    'a ->
    'a
end

module type GET_MODEL_WITH_STATS = functor (QS : Types.QUERY_STATISTICS) ->
  GET_MODEL

module Once (Session : OPEN) : GET_MODEL_WITH_STATS
module MultiChecks (Session : OPEN) : GET_MODEL_WITH_STATS
