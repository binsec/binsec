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

(** Trace type definition *)

open Syscall_piqi
open Libcall_piqi

(** {2 Types} *)

exception Not_found_in_concrete_infos of string
(** @raise Not_found_in_concrete_infos if the runtime concrete info queried
    is not found in current instruction infos *)

module InstrMap = Basic_types.Int.Map
(** Trace instructions are indexed in this map *)

type metadata =
  | Exception of int32 * int64 (** infos about the exception raised during the execution *)
  | Module of string           (** name of the dynamic library loaded *)
  | Layer of int               (** self-modification layer *)

type trace_concrete_infos =
  | NextAddr of int64          (** Next address *)
  | RegRead of string * int64  (** Register value read *)
  | RegWrite of string * int64 (** Register value written *)
  | Libcall of libcall_t       (** Infos about a call to a library *)
  | Syscall of syscall_t       (** Syscall info *)
  | MemLoad of int64 * string  (** Load in memory: address and data (size read is length data) *)
  | MemStore of int64 * string (** Memory write: address, data written *)
  | Not_retrieved              (** concretes infos were not retreived for this instr *)
  | Comment of string          (** Comment (free-to-use) *)
  | Wave of int                (** which self-modification layer this instruction belong to *)

(** Trace instruction type *)
type trace_inst = {
  thread : int32;   (** What thread id the instruction belong to *)
  location : int64; (** location of the instruction (address) *)
  opcode : string;  (** mnemonic of the instruction *)
  opcode_bytes: string; (** opcode bytes *)
  decoded : bool;   (** either the instruction was decoded by the decoder or not *)
  mutable dbainstrs : Dba_types.Statement.t list; (** DBA IR of the instruction *)
  mutable concrete_infos : trace_concrete_infos list (** concrete(runtime) infos of the instruction *)
}

(** Trace type *)
type trace = {
  instrs : trace_inst InstrMap.t; (** instructions indexed by theirs trace offset *)
  complete : bool;                (** either the whole trace have been read (might be useful for huge trace) *)
  metadatas : (metadata list) Basic_types.Int.Map.t; (** Metadatas indexed by the offset in the trace where they appeared *)
  address_size : int;             (** Size of addresses *)
}
(* ----------------------------- *)

type trace_inst_map = trace_inst InstrMap.t
(** map of instructions *)

type metadata_map = (metadata list) Basic_types.Int.Map.t
(** map of metadatas *)

type conc_infos = trace_concrete_infos list
(** list of runtime infos *)

(** {2 Constructors} *)

val empty_inst: trace_inst
(** @return an empty instructions *)

val empty_trace: trace
(** @return an empty trace *)

(** {2 Utility functions to query the concrete infos}

{b All this functions raise {!Not_found_in_concrete_infos} of the
value or the information queried is not found in the infos}
*)

val conc_infos_available: conc_infos -> bool
(** are concrete infos available *)

val get_next_address: conc_infos -> int64
(** next address for the current instruction *)

val get_next_address_bv: conc_infos -> int -> Bitvector.t
(** return the next address as a Bitvector given an address
    size  *)

val get_reg_value: string -> ?read:bool -> conc_infos -> int64
(** return the given register value as read or written using the
    switch [?read] (default is read) *)

val get_reg_write_or_read_value: string -> conc_infos -> int64
(** return the given written register. If not found instead of
    raising an exception return the register as read *)

val get_regread_value_bv: string -> ?read:bool -> conc_infos -> Bitvector.t
(** same as [get_reg_value ~read:true] but return a {!Bitvector.t}*)

val get_regwrite_value_bv: string -> conc_infos -> Bitvector.t
(** same as [get_reg_value ~read:false] but return a {!Bitvector.t}*)

val get_store_addr: conc_infos -> int64
(** get the address of the value stored by the instruction *)

val get_store_content: conc_infos -> string
(** get the content of the value stored by the instruction *)

val get_load_addr: conc_infos -> int64
(** get the load address of the value read by the instruction *)

val get_load_content: int64 -> int -> conc_infos -> string
(** get the load content read by the instruction *)

val get_syscall: conc_infos -> syscall_t
(** return the syscall infos of the instruction (use it with [is_syscall]) *)

val is_syscall: conc_infos -> bool
(** return either or not the instruction is a syscall *)

val is_libcall: conc_infos -> bool
(** return either or not the instruction is call to an external library *)

val get_libcall: conc_infos -> libcall_t
(** return the concrete informations of the library call *)

val get_syscall_name: conc_infos -> string
(** return the syscall name in case the isntruction is a syscall *)

val is_traced: conc_infos -> bool
(** return true if the instruction is a libcall and the call is traced

    Note: Use the negation of this since we are more interesting in doing
    something if the call is not traced
*)

val is_concrete_infos_retrieved: conc_infos -> bool
