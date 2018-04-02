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

open Libcall_types
open Libcall_utils
open Call_convention
open Trace_type

open Common_piqi
open Libcall_piqi



module GetModuleHandle_call:
  LibCall with type data_t = getmodulehandle_t
           and type pol_t = getmodulehandle_pol =
struct

  type pol_t = getmodulehandle_pol
  type data_t = getmodulehandle_t

  let check_consistency (pol:getmodulehandle_pol) (default:action): bool =
    let open Getmodulehandle_pol in
    let open Memory_pol in
    check_consistency_memory_t
      pol.module_name.addr pol.module_name.value "GetModuleHandleA" default

  let apply_policy
      (pol:getmodulehandle_pol) (data:getmodulehandle_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (default:action) (env:Path_pred_env.t) =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_param_pointer 0 (uniq_prefix^"_getmodulehandle_modulename")
      pol.Getmodulehandle_pol.module_name data.Getmodulehandle_t.module_name infos default env |> ignore;
    CVT.set_ret (uniq_prefix^"_getmodulehandle_hmodule")
      data.Getmodulehandle_t.ret pol.Getmodulehandle_pol.ret infos default env |> ignore;
    CVT.set_epilog 1 (uniq_prefix^"_getmodulehandle") infos env

  let to_string (data:getmodulehandle_t): string =
    let open Getmodulehandle_t in
    let open Memory_t in
    Printf.sprintf "GetModuleHandleA(@[%Lx]=\"%s\"):%Lx" data.module_name.addr (String.escaped data.module_name.value) data.ret

  let serialize_stack_params (data:getmodulehandle_t): string =
    Libcall_utils.serialize_int64_list
      [data.Getmodulehandle_t.module_name.Memory_t.addr]
end;;


module GetProcAddress_call:
  LibCall with type data_t = getprocaddress_t
           and type pol_t = getprocaddress_pol
= struct

  type pol_t = getprocaddress_pol
  type data_t = getprocaddress_t

  let check_consistency (pol:getprocaddress_pol) (default:action): bool =
    let open Getprocaddress_pol in
    let open Memory_pol in
    check_consistency_memory_t
      pol.proc_name.addr pol.proc_name.value "GetProcAddress" default

  let apply_policy
      (pol:getprocaddress_pol) (data:getprocaddress_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_param 0 (uniq_prefix^"_getprocaddress_hmodule")
      data.Getprocaddress_t.hmodule pol.Getprocaddress_pol.hmodule infos default env |> ignore;
    CVT.set_param_pointer 1 (uniq_prefix^"_getprocaddress_procname")
      pol.Getprocaddress_pol.proc_name data.Getprocaddress_t.proc_name infos default env |> ignore;
    CVT.set_ret (uniq_prefix^"_getmodulehandle_proc_addr")
      data.Getprocaddress_t.ret pol.Getprocaddress_pol.ret infos default env;
    CVT.set_epilog 2 (uniq_prefix^"_getprocaddress") infos env

  let to_string (data:getprocaddress_t): string =
    let open Getprocaddress_t in
    let open Memory_t in
    Format.sprintf
      "GetProcAddress(%Lx, %@[%Lx]=\"%s\"):%Lx"
      data.hmodule data.proc_name.addr
      (String.escaped data.proc_name.value) data.ret

  let serialize_stack_params (data:getprocaddress_t): string =
    let open Getprocaddress_t in
    let open Memory_t in
    Libcall_utils.serialize_int64_list [data.hmodule; data.proc_name.addr]
end


module Getmainargs_call:
  LibCall with type data_t = getmainargs_t
           and type pol_t = getmainargs_pol
=
struct
  type pol_t = getmainargs_pol
  type data_t = getmainargs_t

  let check_consistency (pol:getmainargs_pol) (default:action): bool =
    let open Getmainargs_pol in
    let open Memory_pol in
    check_consistency_memory_t pol.argc.addr pol.argc.value "getmainargs" default &&
    check_consistency_memory_t pol.argv.addr pol.argv.value "getmainargs" default &&
    check_consistency_memory_t pol.env.addr pol.env.value "getmainargs" default &&
    check_consistency_memory_t pol.startinfo.addr pol.startinfo.value "getmainargs" default

  let apply_policy
      (pol:getmainargs_pol) (data:getmainargs_t) (cvt:(module CallConvention))
      (uniq_p:string) (infos:conc_infos) (default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_param_pointer 0 (uniq_p^"_getmainargs_argc")
      pol.Getmainargs_pol.argc data.Getmainargs_t.argc infos default env |> ignore;
    CVT.set_param_pointer 1 (uniq_p^"_getmainargs_argv")
      pol.Getmainargs_pol.argv data.Getmainargs_t.argv infos default env |> ignore;
    CVT.set_param_pointer 2 (uniq_p^"_getmainargs_env")
      pol.Getmainargs_pol.env  data.Getmainargs_t.env  infos default env |> ignore;
    CVT.set_param 3 (uniq_p^"_getmainargs_dowildcard")
      data.Getmainargs_t.dowildcard pol.Getmainargs_pol.dowildcard infos default env |> ignore;
    CVT.set_param_pointer 4 (uniq_p^"_getmainargs_startinfo")
      pol.Getmainargs_pol.startinfo data.Getmainargs_t.startinfo infos default env |> ignore;
    CVT.set_ret (uniq_p^"_getmainargs_ret")
      data.Getmainargs_t.ret pol.Getmainargs_pol.ret infos default env;
    (* 5 parameters but looks like they are not popped from stack*)
    CVT.set_epilog 0 (uniq_p^"_getmainargs") infos env

  let to_string (data:getmainargs_t): string =
    let open Getmainargs_t in
    let open Memory_t in
    let open Decode_utils in
    Format.sprintf
      "_getmainargs(*argc:%Lx='%s', *argv:%Lx'%s', *env:%Lx'%s', \
       dowildcard:%Ld, *startinfo:%Lx'%s'):%Lx"
      data.argc.addr (string_to_hex data.argc.value)
      data.argv.addr (string_to_hex data.argv.value)
      data.env.addr (string_to_hex data.env.value)
      data.dowildcard data.startinfo.addr
      (string_to_hex data.startinfo.value) data.ret

  let serialize_stack_params (data:getmainargs_t): string =
    let open Getmainargs_t in
    let open Memory_t in
    Libcall_utils.serialize_int64_list
      [data.argc.addr; data.argv.addr; data.env.addr;
       data.dowildcard; data.startinfo.addr]
end


module Gethostname_call:
  LibCall with type data_t = gethostname_t
           and type pol_t = gethostname_pol
= struct
  type pol_t = gethostname_pol
  type data_t = gethostname_t

  let check_consistency (pol:gethostname_pol) (default:action): bool =
    let open Gethostname_pol in
    let open Memory_pol in
    check_consistency_memory_t pol.name.addr pol.name.value "gethostname" default

  let apply_policy
      (pol:gethostname_pol) (data:gethostname_t) (cvt:(module CallConvention))
      (uniq_p:string) (infos:conc_infos) (default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_param_pointer 0 (uniq_p^"_gethostname_name")
      pol.Gethostname_pol.name data.Gethostname_t.name infos default env
    |> ignore;
    CVT.set_param 1 (uniq_p^"_gethostname_namelen")
      data.Gethostname_t.namelen pol.Gethostname_pol.namelen infos default env
    |> ignore;
    CVT.set_ret (uniq_p^"_gethostname_ret")
      data.Gethostname_t.ret pol.Gethostname_pol.ret infos default env;
    CVT.set_epilog 2 (uniq_p^"_gethostname") infos env

  let to_string (data:gethostname_t): string =
    let open Gethostname_t in
    let open Memory_t in
    Format.sprintf
      "gethostname(*name:%Lx='%s', namelen=%Ld):%Ld"
      data.name.addr data.name.value data.namelen data.ret

  let serialize_stack_params (data:gethostname_t): string =
    let open Gethostname_t in
    let open Memory_t in
    Libcall_utils.serialize_int64_list [data.name.addr; data.namelen]
end
