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

module S = Basic_types.String
module I = Basic_types.Int

module A = struct
  type t = string option

  let equal t t' =
    match (t, t') with
    | None, None -> true
    | Some name, Some name' -> String.equal name name'
    | (None | Some _), (None | Some _) -> false

  let compare t t' =
    match (t, t') with
    | None, None -> 0
    | Some name, Some name' -> String.compare name name'
    | None, Some _ -> -1
    | Some _, None -> 1

  let hash = function None -> 129913994 | Some name -> Hashtbl.hash name

  let default = None
end

module Expr = Dba.Expr
module Var = Dba.Var

module Output = struct
  type format = Bin | Dec | Hex | Ascii

  type t =
    | Model
    | Formula
    | Slice of (Expr.t * string) list
    | Value of format * Expr.t
    | Stream of string
    | String of string
end

exception Unknown

exception Undef of Var.t

exception Uninterp of string

exception Non_unique

exception Non_mergeable

type 'a test = True of 'a | False of 'a | Both of { t : 'a; f : 'a }

type target = (Expr.t * string) list option

type size = Term.size

and 'a interval = 'a Term.interval

and unary = Term.unary

and binary = Term.binary

and 'a operator = 'a Term.operator =
  | Not : unary operator
  | Sext : size -> unary operator
  | Uext : size -> unary operator
  | Restrict : int interval -> unary operator
  | Plus : binary operator
  | Minus : _ operator
  | Mul : binary operator
  | Udiv : binary operator (* Corresponds to *)
  | Umod : binary operator (* the truncated division *)
  | Sdiv : binary operator (* of C99 and most *)
  | Smod : binary operator (* processors *)
  | Or : binary operator
  | And : binary operator
  | Xor : binary operator
  | Concat : binary operator
  | Lsl : binary operator
  | Lsr : binary operator
  | Asr : binary operator
  | Rol : binary operator
  | Ror : binary operator
  | Eq : binary operator
  | Diff : binary operator
  | Ule : binary operator
  | Ult : binary operator
  | Uge : binary operator
  | Ugt : binary operator
  | Sle : binary operator
  | Slt : binary operator
  | Sge : binary operator
  | Sgt : binary operator

module type VALUE = sig
  type t
  (** Symbolic value *)

  type state

  val constant : Bitvector.t -> t

  val lookup : Var.t -> state -> t

  val read : addr:t -> int -> Machine.endianness -> state -> t * state

  val select :
    string -> addr:t -> int -> Machine.endianness -> state -> t * state

  val unary : unary operator -> t -> t

  val binary : binary operator -> t -> t -> t

  val ite : t -> t -> t -> t

  val eval : Expr.t -> state -> t
end

module type STATE = sig
  type t
  (** Symbolic state *)

  module Value : VALUE with type state := t

  val empty : unit -> t

  val assume : Value.t -> t -> t option

  val test : Value.t -> t -> t test

  val get_value : Value.t -> t -> Bitvector.t

  val enumerate :
    Value.t -> ?n:int -> ?except:Bitvector.t list -> t -> (Bitvector.t * t) list

  val fresh : Var.t -> t -> t

  val alloc : array:string -> t -> t

  val assign : Var.t -> Value.t -> t -> t

  val write : addr:Value.t -> Value.t -> Machine.endianness -> t -> t

  val store : string -> addr:Value.t -> Value.t -> Machine.endianness -> t -> t

  val memcpy : addr:Bitvector.t -> int -> Loader_buf.t -> t -> t

  val merge : t -> t -> t

  val pp : Format.formatter -> t -> unit

  val pp_smt : target -> Format.formatter -> t -> unit

  val as_ascii : name:string -> t -> string

  val as_c_string : name:string -> t -> string

  val to_formula : t -> Formula.formula
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

  val add_instructions : int -> unit

  val register_address : Virtual_address.t -> unit

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
