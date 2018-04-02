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

open Syscall_piqi
open Libcall_piqi
open Libcall_t

exception Not_found_in_concrete_infos of string

module InstrMap = Basic_types.Int.Map


type metadata =
  | Exception of int32 * int64
  | Module of string
  | Layer of int

type trace_concrete_infos =
  | NextAddr of int64
  | RegRead of string * int64      (* Register value *)
  | RegWrite of string * int64     (* Register value *)
  | Libcall of libcall_t           (* Function name of a call and bool to indicate if was traced *)
  | Syscall of syscall_t           (* Syscall (see above) *)
  | MemLoad of int64 * string      (* Load in memory: address, size, datas *)
  | MemStore of int64 * string     (* Memory write address, size written *)
  | Not_retrieved                  (* concretes infos were not retreived for this instr *)
  | Comment of string
  | Wave of int
  (* --------------------------- *)

type conc_infos = trace_concrete_infos list

(* Instruction and trace types *)
type trace_inst = {
  thread : int32;
  location : int64;
  (* opcode_size : int; *)
  opcode : string;
  opcode_bytes: string;
  decoded : bool;
  mutable dbainstrs : Dba_types.Statement.t list;
  (* next_loc : int64; *)
  mutable concrete_infos : conc_infos
}

type trace = {
  instrs : trace_inst InstrMap.t;   (* instructions *)
  complete : bool;                  (* either the whole trace have been read (might be useful for huge trace) *)
  metadatas : (metadata list) Basic_types.Int.Map.t;
  address_size : int;                  (* Size of addresses into the trace *)
}
(* ----------------------------- *)

type trace_inst_map = trace_inst InstrMap.t
type metadata_map = (metadata list) Basic_types.Int.Map.t

(* ------ Utilitarian functions ----- *)
let empty_inst  =  {
  thread = Int32.zero;
  location = Int64.zero;
  decoded = false;
  opcode = "";
  opcode_bytes = "";
  dbainstrs = [];
  concrete_infos = []}

let empty_trace = {
  instrs = InstrMap.empty;
  complete = false;
  metadatas = Basic_types.Int.Map.empty;
  address_size = 0
}

let rec conc_infos_available (infos:conc_infos): bool =
  match infos with
  | [] -> true
  | a::q -> (match a with | Not_retrieved -> false | _ -> conc_infos_available q)

let rec get_next_address (infos:conc_infos): int64 =
  match infos with
  |  [] -> raise (Not_found_in_concrete_infos "NextAddr")
  |  i::q ->
    begin
      match i with
      | NextAddr(v) -> v
      | _ -> get_next_address q
    end

let get_next_address_bv (infos:conc_infos) (addr_size:int) : Bitvector.t =
  let value = get_next_address infos in
  Bitvector.create (Bigint.big_int_of_int64 value) addr_size

let rec get_reg_value (name:string) ?(read=true) (infos:conc_infos)  : int64 =
  match infos with
  | [] -> raise (Not_found_in_concrete_infos ("Regvalue:"^name))
  | RegRead (s,v) :: _ when s = name && read -> v
  | RegWrite(s,v) :: _ when s = name && not read -> v
  | _ :: q -> get_reg_value name ~read:read q

let get_reg_write_or_read_value (name:string) (infos:conc_infos)  : int64 =
  try
    (* Logger.debug "Try to get write value for register\n"; *)
    get_reg_value name ~read:false infos
  with
  | Not_found_in_concrete_infos _ ->
    (* Logger.debug "Write not found take read\n"; *)
    get_reg_value name ~read:true infos

let  get_regread_value_bv (name:string) ?(read=true) (infos:conc_infos) : Bitvector.t =
  let _,_,high = X86Util.reg_to_extract name in
  Bitvector.create (Bigint.big_int_of_int64 (get_reg_value name ~read:read infos)) (high+1)

let get_regwrite_value_bv (name:string) (infos:conc_infos) : Bitvector.t =
  let _,_,high = X86Util.reg_to_extract name in
  Bitvector.create (Bigint.big_int_of_int64 (get_reg_value name ~read:false infos)) (high+1)


let rec get_store_addr (infos:conc_infos): int64 =
  match infos with
  | [] -> raise (Not_found_in_concrete_infos "Store addr")
  | MemStore(addr, _) :: _ -> addr
  | _ :: q -> get_store_addr q

let rec get_store_content (infos:conc_infos): string =
  match infos with
  | [] -> raise (Not_found_in_concrete_infos "Store content")
  | MemStore(_, data) :: _ -> data
  | _ :: q -> get_store_content q

let rec get_load_addr (infos:conc_infos): int64 =
  (* Now only called in case of segment register *)
  match infos with
  | [] -> raise (Not_found_in_concrete_infos "Load addr")
  | MemLoad(addr, _) :: _ -> addr
  | _ :: q -> get_load_addr q

let rec get_load_content (address:int64) (size:int) (infos:conc_infos): string =
  match infos with
  | [] -> raise (Not_found_in_concrete_infos "Load addr")
  | MemLoad(addr, data)::q ->
    let supp_bound = (Int64.add address (Int64.of_int size)) in
    let curr_bound = (Int64.add addr (Int64.of_int (String.length data))) in
    if(address = addr && supp_bound = curr_bound) then
      data
    else if(address >= addr && supp_bound <= curr_bound) then
      let offset = Int64.to_int (Int64.sub address addr) in
      String.sub data offset size
    else
      get_load_content address size q
  | _ :: q -> get_load_content address size q

let rec get_syscall (infos:conc_infos): syscall_t =
  match infos with
  | [] -> raise (Not_found_in_concrete_infos "Syscall")
  | Syscall sys :: _  -> sys
  | _ :: q -> get_syscall q

let rec is_syscall (infos:conc_infos): bool =
  match infos with
  | [] -> false
  | Syscall _ :: _ -> true
  | _ :: q -> is_syscall q

let rec is_libcall (infos:conc_infos): bool =
  match infos with
  | [] -> false
  | Libcall _ :: _ -> true
  | _ :: q -> is_libcall q

let rec get_libcall (infos:conc_infos): libcall_t =
  match infos with
  | [] -> raise (Not_found_in_concrete_infos "libcall")
  | Libcall c :: _ -> c
  | _ :: q -> get_libcall q

let rec get_syscall_name (infos:conc_infos): string =
  match infos with
  | [] -> raise (Not_found_in_concrete_infos "Syscall")
  | Syscall _ :: _ ->
    begin "SYSCALL"
    (*       match sys with
             | Stub -> "stub"
             | Read(_) -> "read"
             | Generic(_,name) -> name *)
    end
  | _ :: q -> get_syscall_name q

let rec is_traced (infos:conc_infos): bool =
  match infos with
  | [] -> true
  | Libcall c :: _ -> c.is_traced
  | _ :: q -> is_traced q

let is_concrete_infos_retrieved (infos:conc_infos): bool =
  let rec aux infs = 
    match infs with
    | [] -> true
    | Not_retrieved :: _ -> false
    | _ :: q -> aux q
  in
  match infos with
  | [] -> false
  | _ -> aux infos
(* ---------------------------------- *)
