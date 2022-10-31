(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2022                                               *)
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

module I = Basic_types.Int
module S = Basic_types.String
module A = Fiber.A
module Var = Fiber.Var
module Expr = Fiber.Expr

exception Unknown

exception Non_unique

exception Non_mergeable

type 'a test = True of 'a | False of 'a | Both of { t : 'a; f : 'a }

type target = Slice of (Expr.t * string) list | Env of string I.Htbl.t

module type STATE = sig
  type t
  (** Symbolic state *)

  val empty : unit -> t

  val assume : Expr.t -> t -> t option

  val test : Expr.t -> t -> t test

  val get_value : ?check_unique:bool -> Expr.t -> t -> Bitvector.t

  val split_on :
    Expr.t -> ?n:int -> ?except:Bitvector.t list -> t -> (Bitvector.t * t) list

  val fresh : Var.t -> t -> t

  val assign : Var.t -> Expr.t -> t -> t

  val write : addr:Expr.t -> Expr.t -> Machine.endianness -> t -> t

  val store : string -> addr:Expr.t -> Expr.t -> Machine.endianness -> t -> t

  val memcpy : addr:Bitvector.t -> int -> Loader_buf.t -> t -> t

  val merge : t -> t -> t

  val pp : Format.formatter -> t -> unit

  val pp_smt : target -> Format.formatter -> t -> unit

  val as_ascii : name:string -> t -> string

  val as_c_string : name:string -> t -> string
end

module type EXPLORATION_STATISTICS = sig
  val get_paths : unit -> int

  val get_completed_paths : unit -> int

  val get_unknown_paths : unit -> int

  val get_pending_paths : unit -> int

  val get_total_asserts : unit -> int

  val get_failed_asserts : unit -> int

  val get_branches : unit -> int

  val get_max_depth : unit -> int

  val get_instructions : unit -> int

  val get_unique_insts : unit -> int

  val get_time : unit -> float

  val reset : unit -> unit

  val add_path : unit -> unit

  val terminate_path : unit -> unit

  val interrupt_path : unit -> unit

  val add_assert : unit -> unit

  val add_failed_assert : unit -> unit

  val add_branch : unit -> unit

  val update_depth : int -> unit

  val add_instruction : unit -> unit

  val add_unique_inst : unit -> unit

  val pp : Format.formatter -> unit -> unit

  val to_toml : unit -> Toml.Types.table
end

module type QUERY_STATISTICS = sig
  module Preprocess : sig
    val get_sat : unit -> int

    val get_unsat : unit -> int

    val get_const : unit -> int

    val incr_sat : unit -> unit

    val incr_unsat : unit -> unit

    val incr_const : unit -> unit

    val pp : Format.formatter -> unit -> unit

    val to_toml : unit -> Toml.Types.table
  end

  module Solver : sig
    val get_sat : unit -> int

    val get_unsat : unit -> int

    val get_err : unit -> int

    val get_time : unit -> float

    val incr_sat : unit -> unit

    val incr_unsat : unit -> unit

    val incr_err : unit -> unit

    val start_timer : unit -> unit

    val stop_timer : unit -> unit

    val pp : Format.formatter -> unit -> unit

    val to_toml : unit -> Toml.Types.table
  end

  val reset : unit -> unit

  val pp : Format.formatter -> unit -> unit
end

module type STATE_FACTORY = functor (QS : QUERY_STATISTICS) -> STATE

module type WORKLIST = sig
  type 'a t

  val push : 'a -> 'a t -> 'a t

  val pop : 'a t -> 'a * 'a t

  val singleton : 'a -> 'a t

  val length : 'a t -> int

  val is_empty : 'a t -> bool

  val empty : 'a t
end
