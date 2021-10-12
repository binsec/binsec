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

module Pragma : sig
  type t = Start_from of Dba.Expr.t | Load_sections of string list
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

module Path_state : sig
  type t

  val create :
    ?depth:int ->
    ?address_counters:Sse_options.Address_counter.t Virtual_address.Map.t ->
    ?block_index:int ->
    Senv.t ->
    Instruction.t ->
    t

  val branch : t -> t

  (** {2 Accessors} *)

  val dba_instruction : t -> Dba.Instr.t

  val current_statement : t -> Dba_types.Statement.t

  val virtual_address : t -> Virtual_address.t

  val location : t -> Dba_types.Caddress.t

  val symbolic_state : t -> Senv.t

  val block_index : t -> int

  val id : t -> int

  val depth : t -> int

  val solver_calls : t -> int

  val paths_created : unit -> int

  val may_lead_to_goal : t -> bool

  val inst : t -> Instruction.t

  val next_address : t -> Virtual_address.t option

  val counter : Virtual_address.t -> t -> Sse_options.Address_counter.t option

  (** {2 Modifiers} *)

  val set_counter : Virtual_address.t -> Sse_options.Address_counter.t -> t -> t

  val set_block_index : int -> t -> t

  val set_instruction : Instruction.t -> t -> t
  (** increase depth and extend path *)

  val set_symbolic_state : Senv.t -> t -> t

  val incr_solver_calls : t -> t

  val reset_solver_calls : t -> t

  val set_address_counters :
    Sse_options.Address_counter.t Virtual_address.Map.t -> t -> t

  val set_next_address : Virtual_address.t -> t -> t

  val with_init_mem_at : addr:Bitvector.t -> size:int -> t -> t

  (** {2 Printers} *)

  val pp_loc : Format.formatter -> t -> unit

  val pp_path : Format.formatter -> t -> unit
end

module type WORKLIST = sig
  type t

  val push : Path_state.t -> t -> t

  val pop : t -> Path_state.t * t

  val singleton : Path_state.t -> t

  val length : t -> int

  val is_empty : t -> bool

  val empty : t
end

module Dfs : WORKLIST

module Bfs : WORKLIST

module Nurs : WORKLIST
(** Non uniformed randomized search heuristics *)
