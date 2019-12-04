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

module Extra : sig type t end

module C : Instr_cfg.S with type addr = Virtual_address.t
                        and type inst = Instruction.t
                        and type symb = Extra.t


module Path_state : sig
  type t

  val create :
    ?depth:int ->
    ?address_counters:
      Sse_options.Address_counter.t Virtual_address.Map.t ->
    ?cfg:C.t ->
    ?block_index:int ->
    Sse_symbolic.State.t -> Instruction.t ->
    t

  val branch : t -> t

  (** {2 Accessors} *)

  val dba_instruction : t -> Dba.Instr.t
  val current_statement : t -> Dba_types.Statement.t
  val virtual_address : t -> Virtual_address.t
  val location : t -> Dba_types.Caddress.t
  val symbolic_state : t -> Sse_symbolic.State.t
  val block_index : t -> int
  val id : t -> int
  val solver_calls : t -> int
  val paths_created : unit -> int
  val may_lead_to_goal : t -> bool
  val cfg : t -> C.t
  val entrypoint : t -> Virtual_address.t
  val inst : t -> Instruction.t

  val counter : Virtual_address.t -> t -> Sse_options.Address_counter.t option

  (** {2 Modifiers} *)

  val set_counter :
    Virtual_address.t  -> Sse_options.Address_counter.t -> t -> t

  val set_block_index : int -> t -> t
  val set_symbolic_state : Sse_symbolic.State.t -> t -> t

  val incr_solver_calls : t -> t
  val reset_solver_calls : t -> t

  val set_address_counters :
    Sse_options.Address_counter.t Virtual_address.Map.t -> t -> t

  val goto_vaddr : Virtual_address.t -> t -> t
  val goto : Dba_types.Caddress.t -> t -> t

  val add_assertion : Formula.bl_term -> t -> t

  val with_init_mem_at: addr:Bitvector.t -> size:int -> t -> t
  val address_belongs_to_init: addr:Bitvector.t -> t -> bool
  val prepare_solver_in_state :
    ?keep:Formula.VarSet.t -> t -> Solver.Session.t -> unit

  (** {2 Printers} *)

  val pp_loc : Format.formatter -> t -> unit
  val pp_path : t -> unit

  val dump_cfg : filename:string -> t -> unit
end

module type GLOBAL_ENV = sig
  type t

  (** {2 Accessors} *)

  val wl_size : t -> int
  (** [wl_size e] returns the size of the current worklist
      for environment [e].
   *)

  module Directives : sig
    val at : Virtual_address.t -> t -> Directive.t Queue.t option
    (** [at va e] returns the user-defined goals for this address.
        When no goals are defined [] is returned.
   *)

    val has : t -> bool
    (** [has e] is [true] if there are still some goals to deal with. *)

    val update : Virtual_address.t -> Directive.t Queue.t -> t -> unit
    (** [update va a e] replaces the action linked to [va] in [e] by [a].
     *)

    val remove : Virtual_address.t -> t -> unit

    module Enumeration : sig
      val record: Virtual_address.t -> Dba.Expr.t -> Bitvector.t list -> t -> unit
      val count : Virtual_address.t -> Dba.Expr.t -> t -> int
      val get   : Virtual_address.t -> Dba.Expr.t -> t -> Bitvector.t list
    end
  end


  (** {2 Constructors} *)

  val from_address :
    initialize_fun:(Path_state.t -> Path_state.t) ->
    entrypoint:Virtual_address.t -> t

  (** {2 Modifiers} *)

  module Path : sig
    exception Empty_worklist

    val choose : t -> t * Path_state.t
    (** [choose_path e] pops a new path [p] from environment [e],
      and returns both the path and the environment without this path.

      @raise Empty_worklist once worklist has been emptied *)

    val add : Path_state.t -> t -> t
    (** [add_path p e] register path [p] in the worlist
        of environment [e].
     *)
  end

end

module Dfs_global : GLOBAL_ENV

module Bfs_global : GLOBAL_ENV

module Nurs_global : GLOBAL_ENV
(** Non uniformed randomized search heuristics *)
