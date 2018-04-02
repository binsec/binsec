(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
(*    VERIMAG                                                             *)
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
(**************************************************************************)

open Trace_type
open Call_convention
open Libcall_piqi
open Common_piqi
open Libc_stubs
open Windows_stubs

let the = Utils.unsafe_get_opt

let apply_libcall_policy
    (policy:libcall_pol list) (tr_index:int) (instr:trace_inst)
    (callcvt:call_convention_t) (default:action) (env:Path_pred_env.t): unit =
  let cvt =
    match callcvt with
    | `cdecl -> (module Cdecl : CallConvention)
    | `stdcall -> (module Stdcall: CallConvention)
    | `fastcall -> failwith "not implemented fastcall"
    | `thiscall -> failwith "not implemented thiscall"
    | `unknown_cvt -> failwith "Invalid call convention 'unknown'"
  in
  let module CVT = (val cvt: CallConvention) in
  let rec get_pol ident (l:libcall_pol list) =
    match l with
    | [] -> None
    | a :: _ when a.Libcall_pol.ident = ident -> Some a
    | _ :: tl -> get_pol ident tl
  in
  let prefix = "_"^(string_of_int tr_index) in
  let data = get_libcall instr.concrete_infos in
  let pol = get_pol data.Libcall_t.ident policy in
  let infos = instr.concrete_infos in
  match pol with
  | None ->
    if not(data.Libcall_t.is_traced) then begin
      Logger.warning ~level:1
        "No libcall policy found for the call: %s (apply default)"
        data.Libcall_t.func_name;
      CVT.default_stub prefix env
    end
    else Logger.debug ~level:2 "Generic traced (so do nothing)"

  | Some pol ->
    Logger.debug ~level:1 "Start applying policy:%s" pol.Libcall_pol.name;
    env.Path_pred_env.formula <- Formula.add_comment env.Path_pred_env.formula pol.Libcall_pol.name;
    begin
      let open Libcall_t in
      match data.Libcall_t.ident with
      | `invalid     -> failwith "Invalid pol selected"
      | `generic     -> Generic_call.apply_policy (the pol.Libcall_pol.generic) (the data.generic) cvt prefix infos default env
      | `printf      -> Printf_call.apply_policy (the pol.Libcall_pol.printf) (the data.printf) cvt prefix infos default env
      | `strcpy      -> Strcpy_call.apply_policy (the pol.Libcall_pol.strcpy) (the data.strcpy) cvt prefix infos default env
      | `fstat       -> Fstat_call.apply_policy (the pol.Libcall_pol.fstat) (the data.fstat) cvt prefix infos default env
      | `fxstat64    -> Fxstat64_call.apply_policy (the pol.Libcall_pol.fxstat64) (the data.fxstat64) cvt prefix infos default env
      | `realloc     -> Realloc_call.apply_policy (the pol.Libcall_pol.realloc) (the data.realloc) cvt prefix infos default env
      | `mmap        -> Mmap_call.apply_policy (the pol.Libcall_pol.mmap) (the data.mmap) cvt prefix infos default env
      | `qsort       -> Qsort_call.apply_policy (the pol.Libcall_pol.qsort) (the data.qsort) cvt prefix infos default env
      | `bsearch     -> Bsearch_call.apply_policy (the pol.Libcall_pol.bsearch) (the data.bsearch) cvt prefix infos default env
      | `strncpy     -> Strncpy_call.apply_policy (the pol.Libcall_pol.strncpy) (the data.strncpy) cvt prefix infos default env
      | `atoi        -> Atoi_call.apply_policy (the pol.Libcall_pol.atoi) (the data.atoi) cvt prefix infos default env
      | `malloc      -> Malloc_call.apply_policy (the pol.Libcall_pol.malloc) (the data.malloc) cvt prefix infos default env
      | `read        -> Read_call.apply_policy (the pol.Libcall_pol.read) (the data.read) cvt prefix infos default env
      | `fread       -> Fread_call.apply_policy (the pol.Libcall_pol.fread) (the data.fread) cvt prefix infos default env
      | `open_stub   -> Open_stub_call.apply_policy (the pol.Libcall_pol.open_stub) (the data.open_stub) cvt prefix infos default env
      | `lseek       -> Lseek_call.apply_policy (the pol.Libcall_pol.lseek) (the data.lseek) cvt prefix infos default env
      | `strchr      -> Strchr_call.apply_policy (the pol.Libcall_pol.strchr) (the data.strchr) cvt prefix infos default env
      | `strrchr     -> Strrchr_call.apply_policy (the pol.Libcall_pol.strchr) (the data.strchr) cvt prefix infos default env
      | `strcmp      -> Strcmp_call.apply_policy (the pol.Libcall_pol.strcmp) (the data.strcmp) cvt prefix infos default env
      | `strncmp     -> Strncmp_call.apply_policy (the pol.Libcall_pol.strncmp) (the data.strncmp) cvt prefix infos default env
      | `memcmp     -> Memcmp_call.apply_policy (the pol.Libcall_pol.memcmp) (the data.memcmp) cvt prefix infos default env
      | `free        -> Free_call.apply_policy (the pol.Libcall_pol.free) (the data.free) cvt prefix infos default env
      | `ctype_b_loc ->
        Ctype_b_loc_call.apply_policy (the pol.Libcall_pol.ctype_b_loc) (the data.ctype_b_loc) cvt prefix infos default env
      | `fscanf      -> Fscanf_call.apply_policy (the pol.Libcall_pol.fscanf) (the data.fscanf) cvt prefix infos default env
      | `exit        -> ()
      (* | `ignore -> Ignore_call.apply_policy (the pol.Libcall_pol.ignore) (the data.ignore) cvt prefix infos default env *)

      | `getmodulehandle ->
        GetModuleHandle_call.apply_policy (the pol.Libcall_pol.getmodulehandle) (the data.getmodulehandle) cvt prefix infos default env
      | `getprocaddress ->
        GetProcAddress_call.apply_policy (the pol.Libcall_pol.getprocaddress) (the data.getprocaddress) cvt prefix infos default env
      | `getmainargs ->
        Getmainargs_call.apply_policy (the pol.Libcall_pol.getmainargs) (the data.getmainargs) cvt prefix infos default env
      | `gethostname ->
        Gethostname_call.apply_policy (the pol.Libcall_pol.gethostname) (the data.gethostname) cvt prefix infos default env
      | `memcpy ->
        Memcpy_call.apply_policy (the pol.Libcall_pol.memcpy) (the data.memcpy) cvt prefix infos default env
      | `memset ->
        Memset_call.apply_policy (the pol.Libcall_pol.memset) (the data.memset) cvt prefix infos default env
      | `fgetc ->
        Fgetc_call.apply_policy (the pol.Libcall_pol.fgetc) (the data.fgetc) cvt prefix infos default env
    end


let libcall_to_string (data:libcall_t): string =
  let open Libcall_t in
  match data.ident with
  | `invalid         -> "[invalid]"
  | `generic         -> (Format.sprintf "Call:%s[0x%Lx] " data.func_name data.func_addr) ^(match data.generic with None -> "" | Some x -> (Generic_call.to_string x))
  | `printf          -> Printf_call.to_string (the data.printf)
  | `strcpy          -> Strcpy_call.to_string (the data.strcpy)
  | `strncpy         -> Strncpy_call.to_string (the data.strncpy)
  | `fstat           -> Fstat_call.to_string (the data.fstat)
  | `fxstat64        -> Fxstat64_call.to_string (the data.fxstat64)
  | `realloc         -> Realloc_call.to_string (the data.realloc)
  | `mmap            -> Mmap_call.to_string (the data.mmap)
  | `qsort           -> Qsort_call.to_string (the data.qsort)
  | `bsearch         -> Bsearch_call.to_string (the data.bsearch)
  | `atoi            -> Atoi_call.to_string (the data.atoi)
  | `malloc          -> Malloc_call.to_string (the data.malloc)
  | `read            -> Read_call.to_string (the data.read)
  | `fread           -> Fread_call.to_string (the data.fread)
  | `open_stub       -> Open_stub_call.to_string (the data.open_stub)
  | `lseek           -> Lseek_call.to_string (the data.lseek)
  | `strchr          -> Strchr_call.to_string (the data.strchr)
  | `strrchr         -> Strrchr_call.to_string (the data.strchr)
  | `strcmp          -> Strcmp_call.to_string (the data.strcmp)
  | `strncmp         -> Strncmp_call.to_string (the data.strncmp)
  | `memcmp          -> Memcmp_call.to_string (the data.memcmp)
  | `free            -> Free_call.to_string (the data.free)
  | `ctype_b_loc     -> Ctype_b_loc_call.to_string (the data.ctype_b_loc)
  | `fscanf          -> Fscanf_call.to_string (the data.fscanf)
  | `exit            -> "Call to exit"
  (* | `ignore       -> Ignore_call.to_string (the data.ignore) *)
  | `getmodulehandle -> GetModuleHandle_call.to_string (the data.getmodulehandle)
  | `getprocaddress  -> GetProcAddress_call.to_string (the data.getprocaddress)
  | `getmainargs     -> Getmainargs_call.to_string (the data.getmainargs)
  | `gethostname     -> Gethostname_call.to_string (the data.gethostname)
  | `memcpy          -> Memcpy_call.to_string (the data.memcpy)
  | `memset          -> Memset_call.to_string (the data.memset)
  | `fgetc           -> Fgetc_call.to_string (the data.fgetc)

let check_libcall_policy_consistency (policy:libcall_pol list) (default:action)
  : bool =
  let open Libcall_pol in
  let aux acc pol =
    let res =
    match pol.ident with
    | `invalid -> Logger.warning "invalid policy ident"; false
    | `printf -> Printf_call.check_consistency (the pol.printf) default
    | `strcpy -> Strcpy_call.check_consistency (the pol.strcpy) default
    | `strncpy -> Strncpy_call.check_consistency (the pol.strncpy) default
    | `fstat -> Fstat_call.check_consistency (the pol.fstat) default
    | `fxstat64 -> Fxstat64_call.check_consistency (the pol.fxstat64) default
    | `realloc -> Realloc_call.check_consistency (the pol.realloc) default
    | `mmap -> Mmap_call.check_consistency (the pol.mmap) default
    | `qsort -> Qsort_call.check_consistency (the pol.qsort) default
    | `bsearch -> Bsearch_call.check_consistency (the pol.bsearch) default
    | `atoi -> Atoi_call.check_consistency (the pol.atoi) default
    | `malloc -> Malloc_call.check_consistency (the pol.malloc) default
    | `read -> Read_call.check_consistency (the pol.read) default
    | `fread -> Fread_call.check_consistency (the pol.fread) default
    | `open_stub -> Open_stub_call.check_consistency (the pol.open_stub) default
    | `lseek -> Lseek_call.check_consistency (the pol.lseek) default
    | `strchr -> Strchr_call.check_consistency (the pol.strchr) default
    | `strrchr -> Strrchr_call.check_consistency (the pol.strchr) default
    | `strcmp -> Strcmp_call.check_consistency (the pol.strcmp) default
    | `strncmp -> Strncmp_call.check_consistency (the pol.strncmp) default
    | `memcmp -> Memcmp_call.check_consistency (the pol.memcmp) default
    | `free-> Free_call.check_consistency (the pol.free) default
    | `ctype_b_loc-> Ctype_b_loc_call.check_consistency (the pol.ctype_b_loc) default
    | `fscanf -> Fscanf_call.check_consistency (the pol.fscanf) default
    | `exit -> true
    (* | `ignore -> Ignore_call.check_consistency (the pol.ignore) default *)
    | `generic -> Generic_call.check_consistency (the pol.generic) default
    | `getmodulehandle -> GetModuleHandle_call.check_consistency (the pol.getmodulehandle) default
    | `getprocaddress -> GetProcAddress_call.check_consistency (the pol.getprocaddress) default
    | `getmainargs -> Getmainargs_call.check_consistency (the pol.getmainargs) default
    | `gethostname -> Gethostname_call.check_consistency (the pol.gethostname) default
    | `memcpy -> Memcpy_call.check_consistency (the pol.memcpy) default
    | `memset -> Memset_call.check_consistency (the pol.memset) default
    | `fgetc -> Fgetc_call.check_consistency (the pol.fgetc) default
    in res && acc
  in
  List.fold_left aux true policy

let serialize_stack_params (data:libcall_t): string =
  let open Libcall_t in
  match data.ident with
    | `invalid -> ""
    | `generic -> ""
    | `printf -> Printf_call.serialize_stack_params (the data.printf)
    | `strcpy -> Strcpy_call.serialize_stack_params (the data.strcpy)
    | `strncpy -> Strncpy_call.serialize_stack_params (the data.strncpy)
    | `atoi -> Atoi_call.serialize_stack_params (the data.atoi)
    | `malloc -> Malloc_call.serialize_stack_params (the data.malloc)
    | `getmodulehandle -> GetModuleHandle_call.serialize_stack_params (the data.getmodulehandle)
    | `getprocaddress -> GetProcAddress_call.serialize_stack_params (the data.getprocaddress)
    | `getmainargs -> Getmainargs_call.serialize_stack_params (the data.getmainargs)
    | `gethostname -> Gethostname_call.serialize_stack_params (the data.gethostname)
    | `free -> Free_call.serialize_stack_params (the data.free)
    | `memcpy -> Memcpy_call.serialize_stack_params (the data.memcpy)
    | `memset -> Memset_call.serialize_stack_params (the data.memset)
    | `fgetc -> Fgetc_call.serialize_stack_params (the data.fgetc)
    |  `exit | `read | `fread |`fstat | `fxstat64
    | `strcmp | `strncmp | `memcmp | `mmap | `qsort | `bsearch | `fscanf
    | `open_stub
    | `realloc | `strchr | `strrchr | `ctype_b_loc | `lseek -> assert false

let apply_default_stub (tr_index:int) (callcvt:call_convention_t) (env:Path_pred_env.t): unit =
  let prefix = "_"^(string_of_int tr_index) in
  match callcvt with
  | `cdecl -> Cdecl.default_stub prefix env
  | `stdcall -> Stdcall.default_stub prefix env
  | `fastcall -> failwith "not implemented fastcall"
  | `thiscall -> failwith "not implemented thiscall"
  | `unknown_cvt -> failwith "invalid call convention 'unknown'"
