(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
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

(** General command-line options (globals vars) *)

(** {2 General options and types} *)

(** Sub components *)
type mode = private
  | Analyzer
  | Simulator
  | Disasm
  | Trace
  | Test
  | NoMode
  | Serve
  | DSE

val check_options : unit -> unit
(** Check that a file has been set when a mode type needs it *)

(** Parse the command line *)
val parse_command_line : unit -> unit

(** Hold the command select *)
val command: mode ref

(** Executable file (or unnamed argument) *)
module ExecFile : sig
  val set : string -> unit
  val get : unit -> string
  val is_set : unit -> bool
end
  

(** {2 Static disassembly / Analysis } *)

val get_dba_filename : unit -> string option

val get_dba_configuration_filename : unit -> string option

(** DBA start address *)

val get_entry_point : unit -> Bigint.t option

val get_describe_binary : unit -> bool
(* Should we display a description of the contents of this binary ? *)

(** {3 Disassembly options} *)

(** Option set to print cfgraph *)
val cfgraph: bool ref

(** Option set to only disassemble an opcode *)
val decode: bool ref

(** Option set to only disassemble an opcode to LLVM *)
val decode_llvm: bool ref


(** Opaque predicate information file (for assisted disassembly) *)
val opaque_predicates_file: string ref

(** Call stack tampering information file (for assisted disassembly *)
val violated_call_ret_file: string ref

(** Output debug file *)
val debug_file: string ref

(** Output CFG file *)
val cfg_file: string ref

(** {3 Others stats global vars} *)

val display_statistics : bool ref
val finalsize : int ref
val initsize : int ref
val ftemps: int ref
val itemps: int ref
val fflags: int ref
val iflags: int ref

val nb_refined_lhs: int ref
val nb_equalities_names: int ref
val nb_equalities_classes: int ref
val nb_equalities_refinement: int ref

val nb_nat_predicate_recovery_tries : int ref
val nb_recovered_nat_predicates : int ref
val nb_failed_nat_predicate_recoveries : int ref
val nb_conditional_cache_uses : int ref

val time_nat_flag_recovery: float ref
val time_analysis: float ref
val time_redundant_evals: float ref
val time_equalities: float ref
val time_simpl : float ref
val time_disas : float ref



(** {2 Server} *)

(** Number of parallel workers *)
val nb_workers: int ref

(** External port *)
val port : int ref

(** Internal port server<->workers *)
val backend_port: int ref

(** IP address to listen on *)
val ip_address: string ref




(** {2 Dynamic analysis} *)

(** {3 Configuration} *)

(** Trace format either a file or network stream *)
type trace_format =
  | Chunked of Pervasives.in_channel * bool (** File input channel, either to load it entirely or not *)
  | Stream of string                    (** Socket to read trace from *)

(** Configuration sent to DSE analyses with a configuration, filename, [trace_format] *)
type trace_analysis_config = {
  mutable configuration: Config_piqi.Configuration.t;
  mutable trace_file: string;
  mutable trace_input: trace_format;
}

(** Hold the current DSE configuration *)
val config: trace_analysis_config

val summary_only: bool ref

(** {3 Formula generation options} *)

(** Either to apply the backward pruning phase when generating
    a formula (default [true]) *)
val pruning: bool ref

(** policy filename when provided on the command line *)
val pol_name : string ref

(** Enable lifting low-level condition predicates to higher
    level predicates on conditional jumps *)
val natural_cond_enabled: bool ref

(** Remove the array theory from formula generated. {b Warning:
    only works when providing a full concrete memory addressing
    as concretization policy} *)
val flatten_memory: bool ref


(** {3 Path exploration options} *)

val nth_to_invert: int ref
val nth_alloc: int ref 
val nth_free: int ref 
val nth_use: int ref 
val check_init: bool ref

val bin_to_explore: string ref
val arg_dse: string ref
val config_seed_file: string ref
val strategy: string ref
val trace_limit_length: int ref
val exploration_timeout: float ref
val random_seed: bool ref
val score_file: string ref
val score_alloc: string ref
val score_free: string ref
val score_use: string ref
val l_event: string ref
val dse_trace: string ref
val dse_type: string ref


(** {2 Tests} *)

(** {b Experimental purposes only} *)
val experiment: bool ref

module ShareDirectory : sig
  val find_file : string -> string
end
