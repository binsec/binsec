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

exception Unknown

type 'a test = True of 'a | False of 'a | Both of { t : 'a; f : 'a }

module type STATE = sig
  type t
  (** Symbolic state *)

  val empty : unit -> t

  val assume : Dba.Expr.t -> t -> t option

  val test : Dba.Expr.t -> t -> t test

  val split_on :
    Dba.Expr.t ->
    ?n:int ->
    ?except:Bitvector.t list ->
    t ->
    (Bitvector.t * t) list

  val fresh : string -> int -> t -> t

  val assign : string -> Dba.Expr.t -> t -> t

  val write : addr:Dba.Expr.t -> Dba.Expr.t -> Machine.endianness -> t -> t

  val memcpy : addr:Bitvector.t -> int -> Loader_buf.t -> t -> t

  val pp : Format.formatter -> t -> unit

  val pp_smt :
    ?slice:(Dba.Expr.t * string) list -> Format.formatter -> t -> unit

  val as_ascii : string -> t -> string
end

module type EXPLORATION_STATISTICS = sig
  val get_paths : unit -> int

  val get_completed_paths : unit -> int

  val get_unknown_paths : unit -> int

  val get_total_asserts : unit -> int

  val get_failed_asserts : unit -> int

  val get_branches : unit -> int

  val get_max_depth : unit -> int

  val get_instructions : unit -> int

  val get_unique_insts : unit -> int

  val get_time : unit -> float
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
end

module type STATE_FACTORY = functor (QS : QUERY_STATISTICS) -> STATE

module Pragma : sig
  type t =
    | Start_from of Dba.Expr.t * Dhunk.t
    | Start_from_core of Dhunk.t
    | Load_sections of string list
    | Reach_all
end

module Script : sig
  type t =
    | Init of Parse_helpers.Initialization.t
    | Goal of Directive.t
    | Stub of Dba.Expr.t list * Dhunk.t
    | Pragma of Pragma.t
end

module C :
  Instr_cfg.S
    with type addr = Virtual_address.t
     and type inst = Instruction.t
     and type symb = Basic_types.Int.t

module Path_state (S : STATE) : sig
  type t

  val create :
    ?depth:int ->
    ?address_counters:Sse_options.Address_counter.t Virtual_address.Map.t ->
    ?block_index:int ->
    S.t ->
    Instruction.t ->
    t

  val branch : t -> t

  (** {2 Accessors} *)

  val dba_instruction : t -> Dba.Instr.t

  val current_statement : t -> Dba_types.Statement.t

  val virtual_address : t -> Virtual_address.t

  val location : t -> Dba_types.Caddress.t

  val symbolic_state : t -> S.t

  val block_index : t -> int

  val id : t -> int

  val depth : t -> int

  val solver_calls : t -> int

  val paths_created : unit -> int

  val is_depth_ok : t -> bool

  val inst : t -> Instruction.t

  val next_address : t -> Virtual_address.t option

  val counter : Virtual_address.t -> t -> Sse_options.Address_counter.t option

  (** {2 Modifiers} *)

  val set_counter : Virtual_address.t -> Sse_options.Address_counter.t -> t -> t

  val set_block_index : int -> t -> t

  val set_instruction : Instruction.t -> t -> t
  (** increase depth and extend path *)

  val set_symbolic_state : S.t -> t -> t

  val incr_solver_calls : t -> t

  val reset_solver_calls : t -> t

  val set_address_counters :
    Sse_options.Address_counter.t Virtual_address.Map.t -> t -> t

  val set_next_address : Virtual_address.t -> t -> t

  (** {2 Printers} *)

  val pp_loc : Format.formatter -> t -> unit

  val pp_path : Format.formatter -> t -> unit
end

module type WORKLIST = sig
  type elt

  type t

  val push : elt -> t -> t

  val pop : t -> elt * t

  val singleton : elt -> t

  val length : t -> int

  val is_empty : t -> bool

  val empty : t
end

module type WORKLIST_FACTORY = functor (E : Sigs.ANY) ->
  WORKLIST with type elt := E.t

module Dfs : WORKLIST_FACTORY

module Bfs : WORKLIST_FACTORY

module Nurs : WORKLIST_FACTORY
