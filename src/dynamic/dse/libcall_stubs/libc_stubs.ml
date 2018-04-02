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

(** Atoi stub  *)

open Libcall_types
open Libcall_utils
open Call_convention
open Trace_type
open Formula_utils
open Path_pred_env
open Common_piqi
open Libcall_piqi

open Smtlib2
open Formula
open Formula_type

let bv_create64 value64 size =
  let value = Bigint.big_int_of_int64 value64 in
  Bitvector.create value size

(** Atoi stub  *)
module Atoi_call : LibCall with type data_t = atoi_t and type pol_t = atoi_pol = struct
  type data_t = atoi_t
  type pol_t = atoi_pol

  let check_consistency (pol:atoi_pol) (default:action): bool =
    let open Atoi_pol in
    check_consistency_memory_t pol.src.Memory_pol.addr pol.src.Memory_pol.value "Atoi" default

  let apply_policy (pol:atoi_pol) (data:atoi_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_param_pointer 0 (uniq_prefix^"_atoi_src")
      pol.Atoi_pol.src data.Atoi_t.src infos default env |> ignore;
    CVT.set_ret (uniq_prefix^"_atoi_ret")
      data.Atoi_t.ret pol.Atoi_pol.ret infos default env |> ignore;
    CVT.set_epilog 1 (uniq_prefix^"_atoi") infos env

  let to_string (data:atoi_t): string =
    let open Atoi_t in
    let open Memory_t in
    Format.sprintf "atoi(@[%Lx]=\"%s\"):%Ld" data.src.Memory_t.addr
      (String.escaped data.src.value) data.ret

  let serialize_stack_params (data:atoi_t): string =
    Libcall_utils.serialize_int64_list [data.Atoi_t.src.Memory_t.addr]
end

module Printf_call:
  LibCall with type data_t = printf_t and type pol_t = printf_pol =
struct
  type data_t = printf_t
  type pol_t = printf_pol

  let check_consistency  (pol:printf_pol) (default:action): bool =
    let open Printf_pol in
    let open Common.Memory_pol in
    check_consistency_memory_t pol.format.addr pol.format.value "Format" default

  let apply_policy
      (pol:printf_pol) (data:printf_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (default:action) (env:Path_pred_env.t): unit =
    let open Printf_pol in
    let module CVT = (val cvt: CallConvention) in
    CVT.set_param_pointer 0 (uniq_prefix^"_printf_format")
      pol.format data.Printf_t.format infos default env |> ignore;
    CVT.set_ret (uniq_prefix^"_printf_ret") data.Printf_t.ret pol.ret infos
      default env |> ignore;
    (* normally n-1 but format takes count one so n *)
    let nb_args =
      Str.split (Str.regexp "%") data.Printf_t.format.Memory_t.value
      |> List.length in
    CVT.set_epilog nb_args (uniq_prefix^"_printf") infos env

  let to_string (data:printf_t): string =
    let open Printf_t in
    let open Memory_t in
    Format.sprintf "printf(@[%Lx]=\"%s\",...):%Ld" data.format.addr
      (String.escaped data.format.value) data.ret

  let serialize_stack_params (data:printf_t): string =
    let open Printf_t in
    let open Common.Memory_t in
    Libcall_utils.serialize_int64_list [data.format.addr]
end


module Malloc_call: LibCall with type data_t = malloc_t and type pol_t = malloc_pol = struct
  type data_t = malloc_t
  type pol_t  = malloc_pol

  let check_consistency _ _ = true

  let apply_policy
      (pol:malloc_pol) (data:malloc_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_param 0 (uniq_prefix^"_malloc_n") data.Malloc_t.size pol.Malloc_pol.size infos default env |> ignore;
    CVT.set_ret (uniq_prefix^"_malloc_ret") data.Malloc_t.ret pol.Malloc_pol.ret infos default env;
    CVT.set_epilog 1 (uniq_prefix^"_malloc") infos env

  let to_string (data:malloc_t): string =
    Format.sprintf "malloc(%Ld):%Lx" data.Malloc_t.size data.Malloc_t.ret

  let serialize_stack_params (data:malloc_t): string =
    Libcall_utils.serialize_int64_list [data.Malloc_t.size]
end

(* TODO *)
(*
module Strcpy_call: LibCall with type data_t = strcpy_t
                             and type pol_t = strcpy_pol =
struct
  type data_t = strcpy_t
  type pol_t  = strcpy_pol

  let check_consistency (pol:strcpy_pol) (default:action): bool =
    let open Strcpy_pol in
    let open Memory_pol in
    check_consistency_memory_t pol.src.addr pol.src.value "Strcpy(src)" default
    && check_consistency_memory_t pol.dst.addr pol.dst.value "Strcpy(dst)" default
    &&
    match pol.src.value, pol.dst.value with
    | `symb, `conc ->
      Logger.warning "Strcpy inconsistency, keep src string symbolic but \
                      concretizing dst meaningless";
      false
    | _ -> true


  let apply_policy
      (pol:strcpy_pol) (data:strcpy_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_param_pointer 0 (uniq_prefix^"_strcpy_src") pol.Strcpy_pol.src data.Strcpy_t.src infos default env |> ignore;
    CVT.set_param_pointer 1 (uniq_prefix^"_strcpy_dst") pol.Strcpy_pol.dst data.Strcpy_t.dst infos default env |> ignore;
    CVT.set_ret (uniq_prefix^"_strcpy_ret") data.Strcpy_t.ret pol.Strcpy_pol.ret infos default env;
    CVT.set_epilog 2 (uniq_prefix^"_strcpy") infos env


  let to_string (data:strcpy_t): string =
    let open Strcpy_t in
    let open Memory_t in
    Format.sprintf "strcpy(@[%Lx]=\"%s\",@[%Lx]):%Ld" data.src.addr
      (String.escaped data.src.value) data.dst.addr data.ret


  let serialize_stack_params (data:strcpy_t): string =
    let open Strcpy_t in
    let open Memory_t in
    Libcall_utils.serialize_int64_list [data.dst.addr; data.src.addr]
end
*)

(* TODO *)
module Generic_call : LibCall with type data_t = generic_t and type pol_t = generic_pol = struct
  type data_t = generic_t
  type pol_t  = generic_pol

  let check_consistency _ _  = true

  let apply_policy
      (pol:generic_pol) (data:generic_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_epilog 0 "" infos env;
    CVT.set_ret (uniq_prefix^"_ret")
         data.Generic_t.ret pol.Generic_pol.ret infos default env

  let to_string (data:generic_t) : string =
    Format.sprintf "ret %Lx" data.Generic_t.ret

  let serialize_stack_params (_data:generic_t): string =
    Libcall_utils.serialize_int64_list []
end


module Mmap_call : LibCall with type data_t = mmap_t and type pol_t = mmap_pol = struct
  type pol_t = mmap_pol
  type data_t = mmap_t

  let check_consistency (pol:mmap_pol) (_:action): bool =
    let open Mmap_pol in
    pol.ret.Memory_pol.addr = `conc

  let apply_policy
      (pol:mmap_pol) (data:mmap_t) (cvt:(module CallConvention))
      (_uniq_prefix:string) (infos:conc_infos) (_default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_epilog 0 "" infos env;
    match pol.Mmap_pol.ret.Memory_pol.value with
    | `conc ->
       begin
        let size_max = data.Mmap_t.length in
        let addr_size = env.Path_pred_env.addr_size in
        let rec create_expr buf_addr vals off size_max =
          if Int64.compare off size_max < 0 then begin
            let addr = Bigint.big_int_of_int64 (Int64.add buf_addr off) in
            let addr = SmtBvCst(Bitvector.create addr addr_size) in
            let load_int =
              SmtBvCst(Bitvector.create (Bigint.big_int_of_int (Char.code (String.get vals (Int64.to_int off)))) 8) in
            let load_char = SmtBvUnary(SmtBvExtract(0,7),load_int) in
            env.formula <- store_memory env.formula (SmtABvStore(env.formula.memory,addr,load_char));
            let off = Int64.add off 1L in
            create_expr buf_addr vals off size_max
          end
        in
        create_expr
          data.Mmap_t.ret.Memory_t.addr data.Mmap_t.ret.Memory_t.value 0L size_max
       end
    | `patch | `ignore | `default
    | `logic
    | `symb
      -> ();
    let value = Bigint.big_int_of_int64 data.Mmap_t.ret.Memory_t.addr in
    let size = env.Path_pred_env.addr_size in
    let fexpr = SmtBvCst (Bitvector.create value size) in
    replace_register "eax" fexpr env

  let to_string (data:mmap_t) : string =
    let open Mmap_t in
    Format.sprintf
      "Mmap fd %Lx, size %Ld, new buf %Lx"
      data.fd data.length data.ret.Memory_t.addr

  let serialize_stack_params (data:mmap_t): string =
    let open Mmap_t in
    Libcall_utils.serialize_int64_list [data.addr;data.length;data.prot;data.flags;data.fd;data.offset]
end


module Read_call: LibCall with type data_t = read_t and type pol_t = read_pol = struct
  type pol_t = read_pol
  type data_t = read_t

  let check_consistency _ _ = true

  let apply_policy
      (pol:read_pol) (data:read_t) (cvt:(module CallConvention))
      (_uniq_prefix:string) (infos:conc_infos) (_default:action) env =
    let module CVT = (val cvt: CallConvention) in
    match pol.Read_pol.buf.Memory_pol.value with
    | `conc ->
       begin
        let size_max = data.Read_t.count in
        let addr_size = env.Path_pred_env.addr_size in
        let rec create_expr buf_addr vals off size_max =
          if Int64.compare off size_max < 0 then begin
            let addr = Bigint.big_int_of_int64 (Int64.add buf_addr off) in
            let addr = SmtBvCst(Bitvector.create addr addr_size) in
            let load_int =
              SmtBvCst(Bitvector.create (Bigint.big_int_of_int (Char.code (String.get vals (Int64.to_int off)))) 8) in
            let load_char = SmtBvUnary(SmtBvExtract(0,7),load_int) in
            env.formula <- store_memory env.formula (SmtABvStore(env.formula.memory,addr,load_char));
            let off = Int64.add off 1L in
            create_expr buf_addr vals off size_max
          end
        in
        let addr_ret = SmtBvCst(bv_create64 data.Read_t.ret addr_size) in
        replace_register "eax" addr_ret env;
        create_expr
          data.Read_t.buf.Memory_t.addr data.Read_t.buf.Memory_t.value 0L size_max
       end
    | `patch | `ignore | `default
     | `logic | `symb -> ();
    CVT.set_epilog 0 "" infos env

  let to_string (data:read_t): string =
    let open Read_t in
    Format.sprintf
      "Read : fd = %Lx, buf = %Lx , Size = %Lx, Real Size = %Lx"
      data.fd data.buf.Memory_t.addr data.count data.ret

  let serialize_stack_params (data:read_t): string =
    let open Read_t in
    Libcall_utils.serialize_int64_list [data.fd;data.buf.Memory_t.addr; data.count]

end

module Fread_call: LibCall with type data_t = fread_t and type pol_t = fread_pol = struct
  type pol_t = fread_pol
  type data_t = fread_t

  let check_consistency _ _ = true

  let apply_policy
      (pol:fread_pol) (data:fread_t) (cvt:(module CallConvention))
      (_uniq_prefix:string) (infos:conc_infos) (_default:action) env =
    let module CVT = (val cvt: CallConvention) in
    match pol.Fread_pol.ptr.Memory_pol.value with
    | `conc ->
       begin
        let size_max = Int64.mul data.Fread_t.size  data.Fread_t.nmemb in
        let addr_size = env.Path_pred_env.addr_size in
        let rec create_expr buf_addr vals off size_max =
          if Int64.compare off size_max < 0 then begin
            let addr = Bigint.big_int_of_int64 (Int64.add buf_addr off) in
            let addr = SmtBvCst(Bitvector.create addr addr_size) in
            let load_int =
              SmtBvCst(Bitvector.create (Bigint.big_int_of_int (Char.code (String.get vals (Int64.to_int off)))) 8) in
            let load_char = SmtBvUnary(SmtBvExtract(0,7),load_int) in
            env.formula <- store_memory env.formula (SmtABvStore(env.formula.memory,addr,load_char));
            let off = Int64.add off 1L in
            create_expr buf_addr vals off size_max
          end
        in
        let addr_ret = SmtBvCst(bv_create64 data.Fread_t.ret addr_size) in
        replace_register "eax" addr_ret env;
        create_expr
          data.Fread_t.ptr.Memory_t.addr data.Fread_t.ptr.Memory_t.value 0L size_max
       end
    | `patch | `ignore | `default
     | `logic | `symb -> ();

    CVT.set_epilog 0 "" infos env

  let to_string (data:fread_t): string =
    let open Fread_t in
    Format.sprintf
      "Fread : ptr = %Lx, size = %Lx , nmemb = %Lx, File %Lx, Real Size = %Lx"
      data.ptr.Memory_t.addr data.size data.nmemb data.stream data.ret

  let serialize_stack_params (data:fread_t): string =
    let open Fread_t in
    Libcall_utils.serialize_int64_list [data.ptr.Memory_t.addr;data.size;data.nmemb;data.stream]

end


module Open_stub_call: LibCall with type data_t = open_t and type pol_t = open_pol = struct
  type pol_t = open_pol
  type data_t = open_t

  let check_consistency _ _ = true

  let apply_policy
      (_pol:open_pol) (data:open_t) (cvt:(module CallConvention))
      (_uniq_prefix:string) (infos:conc_infos) (_default:action) env =
    let module CVT = (val cvt: CallConvention) in
    let open Open_t in
    FdInput.add_fd data.ret data.pathname.Memory_t.value;
    CVT.set_epilog 0 "" infos env

  let to_string (data:open_t): string =
    let open Open_t in
    Format.sprintf
      "Open : file = %s, flags = %Lx , mode = %Lx, ret = %Lx"
      data.pathname.Memory_t.value data.flags data.mode data.ret

  let serialize_stack_params (data:open_t): string =
    let open Open_t in
    Libcall_utils.serialize_int64_list
      [data.pathname.Memory_t.addr; data.flags; data.mode]

end

module Lseek_call: LibCall with type data_t = lseek_t and type pol_t = lseek_pol = struct
  type pol_t = lseek_pol
  type data_t = lseek_t

  let check_consistency _ _ = true

  let apply_policy
      (pol:lseek_pol) (data:lseek_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (default:action) env =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_ret (uniq_prefix^"_ret")
         data.Lseek_t.ret pol.Lseek_pol.ret infos default env;
    CVT.set_epilog 0 "" infos env

  let to_string (data:lseek_t): string =
    let open Lseek_t in
    Format.sprintf
      "Lseek : fd = %Lx, offset = %Lx , whence = %Lx, ret = %Lx" data.fd data.offset data.whence data.ret

  let serialize_stack_params (data:lseek_t): string =
    let open Lseek_t in
    Libcall_utils.serialize_int64_list
      [data.fd; data.offset; data.whence]

end





module Fscanf_call: LibCall with type data_t = fscanf_t and type pol_t = fscanf_pol = struct
  type pol_t = fscanf_pol
  type data_t = fscanf_t

  let check_consistency _ _ =    true

  let apply_policy
      (_pol:fscanf_pol) (_data:fscanf_t) (cvt:(module CallConvention))
      (_uniq_prefix:string) (infos:conc_infos) (_default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_epilog 0 "" infos env


  let to_string (_data:fscanf_t): string = Format.sprintf "Fscanf !"

  let serialize_stack_params (_data:fscanf_t): string =
    Libcall_utils.serialize_int64_list []

end

module Free_call: LibCall with type data_t = free_t and type pol_t = free_pol = struct
  type pol_t = free_pol
  type data_t = free_t

  let check_consistency _ _ = true

  let apply_policy
      (pol:free_pol) (data:free_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_param 0 (uniq_prefix^"_free_ptr")
      data.Free_t.ptr pol.Free_pol.ptr infos default env |> ignore;
    CVT.set_epilog 1 (uniq_prefix^"_free") infos env

  let to_string (data:free_t): string = Format.sprintf "free(%Lx)" data.Free_t.ptr

  let serialize_stack_params (data:free_t): string =
    Libcall_utils.serialize_int64_list [data.Free_t.ptr]
end


module Strchr_call: LibCall with type data_t = strchr_t and type pol_t = strchr_pol = struct
  type pol_t = strchr_pol
  type data_t = strchr_t

  let check_consistency (pol:strchr_pol) (_:action): bool =
    let open Strchr_pol in
    pol.sc.Memory_pol.addr = `logic && pol.sc.Memory_pol.value = `logic &&
    pol.c.Memory_pol.addr = `logic && pol.c.Memory_pol.value = `logic &&
    pol.ret = `logic

  let apply_policy
      (_pol:strchr_pol) (data:strchr_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (_default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_epilog 0 "" infos env;
    let s_alias = (uniq_prefix^"_strchr_s_alias") in
    let s_alias = CVT.add_alias 0 s_alias infos env in
    let c_alias = (uniq_prefix^"_strchr_c_alias") in
    let c_alias = CVT.add_alias 1 c_alias infos env in
    let mem_name = "memory" ^ (string_of_int ((get_varindex env.formula "memory"
                                              )-1)) in
    (* add possible padding *)
    let size_max = Basic_types.Int64.max data.Strchr_t.size_max 10L in
    let size = env.Path_pred_env.addr_size in
    let rec create_expr  s off char_c size_max =
      let addr =
        SmtBvBinary (
          SmtBvAdd, s,
          SmtBvCst(Bitvector.create (Bigint.big_int_of_int64 off) size)) in
      let load_int =  SmtABvLoad32(SmtABvArray(mem_name,0,size),addr) in
      let load_char = SmtBvExpr(SmtBvUnary(SmtBvExtract(0,7),load_int)) in
      let cond = SmtComp(load_char,char_c) in
      if Int64.compare off size_max = 0
      then SmtBvIte(cond, addr, SmtBvCst(Bitvector.zeros size))
      else
        let is_null = SmtComp(load_char,SmtBvExpr(SmtBvCst(Bitvector.zeros 8))) in
        let off = Int64.succ off in
        let fexpr = create_expr s off char_c size_max in
        SmtBvIte(is_null,
                 SmtBvCst(Bitvector.zeros size),
                 SmtBvIte(cond, addr, fexpr))
    in
    (*let char_c = SmtBvExpr(SmtBvCst(Big_int.big_int_of_int64 c,8)) in*)
    let char_c = SmtBvExpr(SmtBvUnary(SmtBvExtract(0,7),c_alias)) in
    let fexpr = create_expr s_alias 0L char_c size_max in
    replace_register "eax" fexpr env

  let to_string (data:strchr_t): string =
    let open Strchr_t in
    Format.sprintf "Strchr s : %Lx c %Lx size_max = %Lx"
      data.s data.c data.size_max


  let serialize_stack_params (data:strchr_t): string =
    let open Strchr_t in
    Libcall_utils.serialize_int64_list [data.s; data.c]
end



module Strrchr_call:
  LibCall with type data_t = strchr_t and type pol_t = strchr_pol =
struct
  type pol_t = strchr_pol
  type data_t = strchr_t

  let check_consistency (pol:strchr_pol) (_:action): bool =
    let open Strchr_pol in
    pol.sc.Memory_pol.addr = `logic && pol.sc.Memory_pol.value = `logic &&
    pol.c.Memory_pol.addr = `logic && pol.c.Memory_pol.value = `logic &&
    pol.ret = `logic

  let apply_policy
      (_pol:strchr_pol) (data:strchr_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (_default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_epilog 0 "" infos env;
    let s_alias = (uniq_prefix^"_strchr_s_alias") in
    let s_alias = CVT.add_alias 0 s_alias infos env in
    let c_alias = (uniq_prefix^"_strchr_c_alias") in
    let c_alias = CVT.add_alias 1 c_alias infos env in
    let mem_name = "memory" ^ (string_of_int ((get_varindex env.formula "memory")-1)) in
    let size_max = Basic_types.Int64.max 10L data.Strchr_t.size_max in
    let size = env.Path_pred_env.addr_size in

    let rec create_expr s off char_c =
      let addr =
        SmtBvBinary(
          SmtBvAdd, s,
          SmtBvCst(Bitvector.create (Bigint.big_int_of_int64 off) size)) in
      let load_int =  SmtABvLoad32(SmtABvArray(mem_name,0, size),addr) in
      let load_char = SmtBvExpr(SmtBvUnary(SmtBvExtract(0,7),load_int)) in
      let cond = SmtComp(load_char,char_c) in
      if Int64.compare off 0L =0
      then SmtBvIte(cond,addr,SmtBvCst (Bitvector.zeros size))
      else
        let is_null =
          SmtComp(load_char,
                  SmtBvExpr(SmtBvCst (Bitvector.zeros 8))) in
        let off = Int64.succ off in
        let fexpr = create_expr s off char_c in
        SmtBvIte(is_null,SmtBvCst (Bitvector.zeros size),SmtBvIte(cond, addr, fexpr))
    in
    let char_c = SmtBvExpr(SmtBvUnary(SmtBvExtract(0,7),c_alias)) in
    let fexpr = create_expr s_alias size_max char_c in
    replace_register "eax" fexpr env


  let to_string (data:strchr_t): string =
    let open Strchr_t in
    Format.sprintf "Strrchr s : %Lx c %Lx size_max = %Lx"
      data.s data.c data.size_max


  let serialize_stack_params (data:strchr_t): string =
    let open Strchr_t in
    Libcall_utils.serialize_int64_list [data.s; data.c]
end


module Strcmp_call:
  LibCall with type data_t = strcmp_t and type pol_t = strcmp_pol =
struct
  type pol_t = strcmp_pol
  type data_t = strcmp_t

  let check_consistency (pol:strcmp_pol) (_default:action): bool =
    let open Strcmp_pol in
    pol.src.Memory_pol.addr = `logic && pol.src.Memory_pol.value = `logic &&
    pol.dst.Memory_pol.addr = `logic && pol.dst.Memory_pol.value = `logic &&
    pol.ret = `logic

  let apply_policy
      (_pol:strcmp_pol) (data:strcmp_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (_default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_epilog 0 "" infos env;
    let s1_alias = uniq_prefix^"_strcmp_s1_alias" in
    let s1_alias = CVT.add_alias 0 s1_alias infos env in
    let s2_alias = uniq_prefix^"_strcmp_s2_alias" in
    let s2_alias = CVT.add_alias 1 s2_alias infos env in
    let mem_name = "memory" ^ (string_of_int ((get_varindex env.formula "memory"
                                              )-1)) in
    let s1_size = Int64.succ data.Strcmp_t.size_max_s1 in
    let s2_size = Int64.succ data.Strcmp_t.size_max_s2 in
    let size_max =  Basic_types.Int64.max s1_size s2_size in
    let addr_size = env.Path_pred_env.addr_size in
    let is_inf c1 c2 =
      let inf = SmtBvExpr(SmtBvBinary(SmtBvSlt,c1,c2)) in
      let one = Bitvector.succ (Bitvector.zeros addr_size) in
      let smt_one = SmtBvCst one in
      SmtBvIte(inf, SmtBvUnary(SmtBvNeg, smt_one ), smt_one)
    in
    let is_cmp_end c =
      SmtComp(SmtBvExpr(SmtBvCst (Bitvector.zeros 8)), SmtBvExpr c) in
    let bv_zeros = Bitvector.zeros addr_size in
    let smt_zeros = SmtBvCst bv_zeros in

    let rec create_expr s1 s2 off size_max =
      let bv = Bitvector.create (Bigint.big_int_of_int64 off) addr_size in
      let addr1 = SmtBvBinary(SmtBvAdd, s1, SmtBvCst bv) in
      let addr2 = SmtBvBinary(SmtBvAdd, s2, SmtBvCst bv) in
      let load_int_a1 =  SmtABvLoad32(SmtABvArray(mem_name,addr_size,addr_size),addr1) in
      let load_char_a1 = SmtBvUnary(SmtBvExtract(0,7),load_int_a1) in
      let load_int_a2 =  SmtABvLoad32(SmtABvArray(mem_name,addr_size,addr_size),addr2) in
      let load_char_a2 = SmtBvUnary(SmtBvExtract(0,7),load_int_a2) in
      if Int64.compare off size_max >=0 then
        SmtBvIte(
          SmtComp (SmtBvExpr(load_char_a1), SmtBvExpr(load_char_a2)),
          smt_zeros,
          is_inf load_char_a1 load_char_a2)
      else
        let off = Int64.succ off in
        let next_cmp = create_expr s1 s2 off size_max in
        let not_end =
          SmtBvIte(
            SmtComp(SmtBvExpr(load_char_a1), SmtBvExpr(load_char_a2)),
            next_cmp,
            is_inf load_char_a1 load_char_a2) in
        let is_end = is_cmp_end  in
        let if_end_then_true c =
          SmtBvIte(is_end c, smt_zeros, SmtBvCst(Bitvector.succ bv_zeros)) in
        SmtBvIte(is_end load_char_a1, if_end_then_true load_char_a2, not_end)
    in
    let fexpr = create_expr s1_alias s2_alias 0L size_max in
    replace_register "eax" fexpr env

  let to_string (data:strcmp_t): string =
    let open Strcmp_t in
    Format.sprintf "Strcmp s : %Lx c %Lx size_max %Lx %Lx, ret %Lx "
      data.s1 data.s2 data.size_max_s1 data.size_max_s2 data.ret

  let serialize_stack_params (data:strcmp_t): string =
    let open Strcmp_t in
    Libcall_utils.serialize_int64_list [data.s1;data.s2]
end

module Strncmp_call:
  LibCall with type data_t = strncmp_t and type pol_t = strncmp_pol =
struct
  type pol_t = strncmp_pol
  type data_t = strncmp_t

  let check_consistency (pol:strncmp_pol) (_default:action): bool =
    let open Strncmp_pol in
(*    pol.src.Memory_pol.addr = `logic && pol.src.Memory_pol.value = `logic &&
    pol.dst.Memory_pol.addr = `logic && pol.dst.Memory_pol.value = `logic && *)
    pol.ret = `logic

  let apply_policy
      (_pol:strncmp_pol) (data:strncmp_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (_default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_epilog 0 "" infos env;
    let s1_alias = uniq_prefix^"_strcmp_s1_alias" in
    let s1_alias = CVT.add_alias 0 s1_alias infos env in
    let s2_alias = uniq_prefix^"_strcmp_s2_alias" in
    let s2_alias = CVT.add_alias 1 s2_alias infos env in
    let mem_name = "memory" ^ (string_of_int ((get_varindex env.formula "memory"
                                              )-1)) in
    let size_max =  data.Strncmp_t.n in
    let addr_size = env.Path_pred_env.addr_size in
    let is_inf c1 c2 =
      let inf = SmtBvExpr(SmtBvBinary(SmtBvSlt,c1,c2)) in
      let one = Bitvector.succ (Bitvector.zeros addr_size) in
      let smt_one = SmtBvCst one in
      SmtBvIte(inf, SmtBvUnary(SmtBvNeg, smt_one ), smt_one)
    in
    let is_cmp_end c =
      SmtComp(SmtBvExpr(SmtBvCst (Bitvector.zeros 8)), SmtBvExpr c) in
    let bv_zeros = Bitvector.zeros addr_size in
    let smt_zeros = SmtBvCst bv_zeros in

    let rec create_expr s1 s2 off size_max =
      let bv = Bitvector.create (Bigint.big_int_of_int64 off) addr_size in
      let addr1 = SmtBvBinary(SmtBvAdd, s1, SmtBvCst bv) in
      let addr2 = SmtBvBinary(SmtBvAdd, s2, SmtBvCst bv) in
      let load_int_a1 =  SmtABvLoad32(SmtABvArray(mem_name,addr_size,addr_size),addr1) in
      let load_char_a1 = SmtBvUnary(SmtBvExtract(0,7),load_int_a1) in
      let load_int_a2 =  SmtABvLoad32(SmtABvArray(mem_name,addr_size,addr_size),addr2) in
      let load_char_a2 = SmtBvUnary(SmtBvExtract(0,7),load_int_a2) in
      if Int64.compare (Int64.succ off) size_max >=0 then
        SmtBvIte(
          SmtComp (SmtBvExpr(load_char_a1), SmtBvExpr(load_char_a2)),
          smt_zeros,
          is_inf load_char_a1 load_char_a2)
      else
        let off = Int64.succ off in
        let next_cmp = create_expr s1 s2 off size_max in
        let not_end =
          SmtBvIte(
            SmtComp(SmtBvExpr(load_char_a1), SmtBvExpr(load_char_a2)),
            next_cmp,
            is_inf load_char_a1 load_char_a2) in
        let is_end = is_cmp_end  in
        let if_end_then_true c =
          SmtBvIte(is_end c, smt_zeros, SmtBvCst(Bitvector.succ bv_zeros)) in
        SmtBvIte(is_end load_char_a1, if_end_then_true load_char_a2, not_end)
    in
    let fexpr = create_expr s1_alias s2_alias 0L size_max in
    replace_register "eax" fexpr env

  let to_string (data:strncmp_t): string =
    let open Strncmp_t in
    Format.sprintf "Strcmp s : %Lx %Lx %Lx, ret %Lx "
      data.s1 data.s2 data.n data.ret

  let serialize_stack_params (data:strncmp_t): string =
    let open Strncmp_t in
    Libcall_utils.serialize_int64_list [data.s1;data.s2;data.n]
end

module Memcmp_call:
  LibCall with type data_t = memcmp_t and type pol_t = memcmp_pol =
struct
  type pol_t = memcmp_pol
  type data_t = memcmp_t

  let check_consistency (pol:memcmp_pol) (_default:action): bool =
    let open Memcmp_pol in
(*    pol.src.Memory_pol.addr = `logic && pol.src.Memory_pol.value = `logic &&
    pol.dst.Memory_pol.addr = `logic && pol.dst.Memory_pol.value = `logic && *)
    pol.ret = `logic

  let apply_policy
      (_pol:memcmp_pol) (data:memcmp_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (_default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_epilog 0 "" infos env;
    let s1_alias = uniq_prefix^"_strcmp_s1_alias" in
    let s1_alias = CVT.add_alias 0 s1_alias infos env in
    let s2_alias = uniq_prefix^"_strcmp_s2_alias" in
    let s2_alias = CVT.add_alias 1 s2_alias infos env in
    let mem_name = "memory" ^ (string_of_int ((get_varindex env.formula "memory"
                                              )-1)) in
    let size_max =  data.Memcmp_t.n in
    let addr_size = env.Path_pred_env.addr_size in
    let is_inf c1 c2 =
      let inf = SmtBvExpr(SmtBvBinary(SmtBvSlt,c1,c2)) in
      let one = Bitvector.succ (Bitvector.zeros addr_size) in
      let smt_one = SmtBvCst one in
      SmtBvIte(inf, SmtBvUnary(SmtBvNeg, smt_one ), smt_one)
    in
    let bv_zeros = Bitvector.zeros addr_size in
    let smt_zeros = SmtBvCst bv_zeros in

    let rec create_expr s1 s2 off size_max =
      let bv = Bitvector.create (Bigint.big_int_of_int64 off) addr_size in
      let addr1 = SmtBvBinary(SmtBvAdd, s1, SmtBvCst bv) in
      let addr2 = SmtBvBinary(SmtBvAdd, s2, SmtBvCst bv) in
      let load_int_a1 =  SmtABvLoad32(SmtABvArray(mem_name,addr_size,addr_size),addr1) in
      let load_char_a1 = SmtBvUnary(SmtBvExtract(0,7),load_int_a1) in
      let load_int_a2 =  SmtABvLoad32(SmtABvArray(mem_name,addr_size,addr_size),addr2) in
      let load_char_a2 = SmtBvUnary(SmtBvExtract(0,7),load_int_a2) in
      if Int64.compare off size_max >=0 then
        SmtBvIte(
          SmtComp (SmtBvExpr(load_char_a1), SmtBvExpr(load_char_a2)),
          smt_zeros,
          is_inf load_char_a1 load_char_a2)
      else
        let off = Int64.succ off in
        let next_cmp = create_expr s1 s2 off size_max in
(*        let not_end =*)
          SmtBvIte(
            SmtComp(SmtBvExpr(load_char_a1), SmtBvExpr(load_char_a2)),
            next_cmp,
            is_inf load_char_a1 load_char_a2) (*in
        let is_end = is_cmp_end  in
        let if_end_then_true c =
          SmtBvIte(is_end c, smt_zeros, SmtBvCst(Bitvector.succ bv_zeros)) in
        SmtBvIte(is_end load_char_a1, if_end_then_true load_char_a2, not_end)*)
    in
    let fexpr = create_expr s1_alias s2_alias 0L size_max in
    replace_register "eax" fexpr env

  let to_string (data:memcmp_t): string =
    let open Memcmp_t in
    Format.sprintf "Memcmp s : %Lx %Lx %Lx, ret %Lx "
      data.s1 data.s2 data.n data.ret

  let serialize_stack_params (data:memcmp_t): string =
    let open Memcmp_t in
    Libcall_utils.serialize_int64_list [data.s1;data.s2;data.n]
end



(* TODO *)
module Strncpy_call:
  LibCall with type data_t = strncpy_t and type pol_t = strncpy_pol =
struct
  type pol_t = strncpy_pol
  type data_t = strncpy_t

  let check_consistency _ _ = true

  let apply_policy
      (_pol:strncpy_pol) (data:strncpy_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (_default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_epilog 0 "" infos env;
    let src_alias = (uniq_prefix^"_strncpy_src_alias") in
    let src_alias = CVT.add_alias 1 src_alias infos env in
    let dst_alias = (uniq_prefix^"_strncpy_dst_alias") in
    let dst_alias = CVT.add_alias 0 dst_alias infos env in
    let size_max = data.Strncpy_t.n in
    let addr_size = env.Path_pred_env.addr_size in
    let rec create_expr  dst_ori src_ori off size_max =
      if Int64.compare off size_max >=0 then
        let dst = SmtBvBinary(SmtBvAdd,dst_ori,SmtBvCst(Bitvector.create (Bigint.big_int_of_int64 off) addr_size)) in
        env.formula <- store_memory env.formula (SmtABvStore(env.formula.memory,dst,SmtBvCst(Bitvector.create (Bigint.big_int_of_int 0) 8)))
      else begin
        let mem_name = "memory" ^ (string_of_int ((get_varindex env.formula "memory")-1)) in
        let dst = SmtBvBinary(SmtBvAdd,dst_ori,SmtBvCst(Bitvector.create (Bigint.big_int_of_int64 off) addr_size)) in
        let src = SmtBvBinary(SmtBvAdd,src_ori,SmtBvCst(Bitvector.create (Bigint.big_int_of_int64 off) addr_size)) in
        let load_int_src =  SmtABvLoad32(SmtABvArray(mem_name,addr_size,addr_size),src) in
        let load_char_src = SmtBvUnary(SmtBvExtract(0,7),load_int_src) in
        let load_int_dst =  SmtABvLoad32(SmtABvArray(mem_name,addr_size,addr_size),dst) in
        let load_char_dst = SmtBvUnary(SmtBvExtract(0,7),load_int_dst) in
        let new_char  =
          SmtBvIte(SmtComp(SmtBvExpr(load_char_src),
                           SmtBvExpr(SmtBvCst (Bitvector.zeros 8))),
                   load_char_dst,load_char_src)  in
        env.formula <- store_memory env.formula (SmtABvStore(env.formula.memory,dst,new_char));
        let off = Int64.add off 1L in
        create_expr dst_ori src_ori off size_max
      end
    in
    let fexpr =
      SmtBvCst(Bitvector.create (Bigint.big_int_of_int64 data.Strncpy_t.ret) addr_size) in
    replace_register "eax" fexpr env;
    create_expr dst_alias src_alias 0L size_max

  let to_string (data:strncpy_t): string =
    let open Strncpy_t in
    Format.sprintf "StrNcpy dst %Lx src %Lx sz max %Lx" data.dst data.src data.n


  let serialize_stack_params (data:strncpy_t): string =
    let open Strncpy_t in
    Libcall_utils.serialize_int64_list [data.dst; data.src; data.n]
end

(* TODO *)
module Strcpy_call:
  LibCall with type data_t = strcpy_t and type pol_t = strcpy_pol =
struct
  type pol_t = strcpy_pol
  type data_t = strcpy_t

  let check_consistency _ _ = true

  let apply_policy
      (_pol:strcpy_pol) (data:strcpy_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (_default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_epilog 0 "" infos env;
    let src_alias = (uniq_prefix^"_strcpy_src_alias") in (*SmtBvVar((uniq_prefix^"_strcpy_src_addr"),8) in*)
    let src_alias = CVT.add_alias 1 src_alias infos env in
    let dst_alias = (uniq_prefix^"_strcpy_dst_alias") in (*SmtBvVar((uniq_prefix^"_strcpy_src_addr"),8) in*)
    let dst_alias = CVT.add_alias 0 dst_alias infos env in
    let size_max =
      assert ((String.length data.Strcpy_t.src.Memory_t.value) >= 0);
      Basic_types.Int64.max (Int64.of_int (String.length data.Strcpy_t.src.Memory_t.value)) 1L in
    let addr_size = env.Path_pred_env.addr_size in
    let rec create_expr dst_ori src_ori off size_max =
      let smt_bv =
        SmtBvCst(Bitvector.create (Bigint.big_int_of_int64 off) addr_size) in
      let dst = SmtBvBinary(SmtBvAdd, dst_ori, smt_bv) in
      if Int64.compare off size_max >=0 then
        env.formula <-
          store_memory env.formula
            (SmtABvStore(env.formula.memory,dst,SmtBvCst(Bitvector.zeros 8)))
      else begin
        let mem_name = "memory" ^ (string_of_int ((get_varindex env.formula "memory")-1)) in
        (*let dst = Big_int.big_int_of_int64 (Int64.add dst off) in
            let dst = SmtBvCst(dst,addr_size) in*)
        (*let src = Big_int.big_int_of_int64 (Int64.add src off) in
          let src = SmtBvCst(src,addr_size) in*)
        let src = SmtBvBinary(SmtBvAdd, src_ori, smt_bv) in
        let load_int_src =  SmtABvLoad32(SmtABvArray(mem_name,addr_size,addr_size),src) in
        let load_char_src = SmtBvUnary(SmtBvExtract(0,7),load_int_src) in
        let load_int_dst =  SmtABvLoad32(SmtABvArray(mem_name,addr_size,addr_size),dst) in
        let load_char_dst = SmtBvUnary(SmtBvExtract(0,7),load_int_dst) in
        let new_char  =
          SmtBvIte(
            SmtComp(SmtBvExpr(load_char_src),
                    SmtBvExpr(SmtBvCst(Bitvector.zeros 8))),
            load_char_dst, load_char_src) in
        env.formula <-
          store_memory env.formula (SmtABvStore(env.formula.memory,dst, new_char));
        (*Formula.add_constraint env.formula cond in*)
        let off = Int64.succ off in
        create_expr dst_ori src_ori off size_max
      end
    in
    let fexpr =
      SmtBvCst
        (Bitvector.create (Bigint.big_int_of_int64 data.Strcpy_t.ret) addr_size) in
    replace_register "eax" fexpr env;
    create_expr dst_alias src_alias 0L size_max

  let to_string (data:strcpy_t): string =
    let open Strcpy_t in
    Format.sprintf "Strcpy dst %Lx src %Lx sz max %x"
      data.dst.Memory_t.addr data.src.Memory_t.addr
      (String.length data.src.Memory_t.value)


  let serialize_stack_params (data:strcpy_t): string =
    let open Strcpy_t in
    Libcall_utils.serialize_int64_list
      [data.dst.Memory_t.addr; data.src.Memory_t.addr]
end

module Ctype_b_loc_call:
  LibCall with type data_t = ctype_b_loc_t
           and type pol_t = ctype_b_loc_pol =
struct
  type pol_t = ctype_b_loc_pol
  type data_t = ctype_b_loc_t

  let check_consistency (pol:ctype_b_loc_pol) (_default:action): bool =
    let open Ctype_b_loc_pol in
    pol.table.Memory_pol.addr = `conc && pol.table.Memory_pol.value = `conc &&
    pol.ret = `logic

  let apply_policy
      (_pol:ctype_b_loc_pol) (data:ctype_b_loc_t) (cvt:(module CallConvention))
      _uniq_prefix (infos:conc_infos) (_default:action) (env:Path_pred_env.t) =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_epilog 0 "" infos env;
    let table = data.Ctype_b_loc_t.table.Memory_t.value in
    let table_addr = Int64.sub data.Ctype_b_loc_t.table.Memory_t.addr 0x100L in
    let size_max = Int64.of_int (String.length table) in
    let addr_size = env.Path_pred_env.addr_size in
    let rec create_expr table_addr table off size_max =
      if Int64.compare off size_max < 0 then
        let addr = Bigint.big_int_of_int64 (Int64.add table_addr off) in
        let addr = SmtBvCst (Bitvector.create addr addr_size) in
        let load_int =
          SmtBvCst(Bitvector.create
                     (Bigint.big_int_of_int
                        (Char.code (String.get table (Int64.to_int off)))) 8) in
        let load_char = SmtBvUnary(SmtBvExtract(0,7),load_int) in
        env.formula <-
          store_memory env.formula
            (SmtABvStore(env.formula.memory, addr, load_char));
        let off = Int64.add off 1L in
        create_expr table_addr table off size_max
    in
    let addr_ret =
      SmtBvCst(bv_create64 data.Ctype_b_loc_t.ret addr_size) in
    let value =
      SmtBvCst(bv_create64 data.Ctype_b_loc_t.table.Memory_t.addr addr_size) in
    env.formula <-
      store_memory env.formula (SmtABvStore32(env.formula.memory,addr_ret,value));
    replace_register "eax" addr_ret env;
    create_expr table_addr table 0L size_max

  let to_string (data:ctype_b_loc_t): string =
    let open Ctype_b_loc_t in
    Format.sprintf "Ctype_b_loc ret %Lx , @ret %Lx"
      data.ret data.table.Memory_t.addr


  let serialize_stack_params (_data:ctype_b_loc_t): string =
    Libcall_utils.serialize_int64_list []
end


(* TODO *)
module Memcpy_call:
  LibCall with type data_t = memcpy_t and type pol_t = memcpy_pol =
struct
  type pol_t = memcpy_pol
  type data_t = memcpy_t

  let check_consistency (pol:memcpy_pol) (default:action): bool =
    let open Memcpy_pol in
    let open Memory_pol in
    check_consistency_memory_t pol.dest.addr pol.dest.value "memcpy(dest)" default
    &&
    check_consistency_memory_t pol.src.addr pol.src.value "memcpy(src)" default
    &&
    ( pol.src.value = pol.dest.value
      || (Logger.warning "should apply same action src,dest memcpy content";
          false))
    &&
    (pol.dest.addr = pol.ret
     || (Logger.warning "memcpy: should apply same action *dest and ret";
         false))

  let apply_policy
      (pol:memcpy_pol) (data:memcpy_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (default:action) (env:Path_pred_env.t) =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_param 2 (uniq_prefix^"_memcpy_size")
      data.Memcpy_t.size pol.Memcpy_pol.size infos default env |> ignore;
    let dstaddr =
      CVT.set_param_pointer 0 (uniq_prefix^"_memcpy_dest")
        pol.Memcpy_pol.dest data.Memcpy_t.dest infos default env in
    let srcaddr =
      CVT.set_param_pointer 1 (uniq_prefix^"_memcpy_src")
        pol.Memcpy_pol.src data.Memcpy_t.src infos default env in
    let addr_size = env.Path_pred_env.addr_size in
    begin
      match srcaddr, dstaddr with
      | Some _, Some _ ->
        begin
          match pol.Memcpy_pol.src.Memory_pol.value,
                pol.Memcpy_pol.dest.Memory_pol.value with
          | `logic, `logic ->
            (*   let i = ref 0 in
                 let size = Int64.to_int data.size in
                 while !i < size do  (* Implicit concretization of size *)
                 let cst_i = Dba.ExprCst(`Constant, Bitvector.create (Bigint.big_int_of_int !i) env.addr_size) in
                 let expr_addr = Dba.ExprBinary(Dba.Plus, dst_addr, cst_i) in
                 let expr_content = Dba.ExprLoad(1, Dba.LittleEndian, Dba.ExprBinary(Dba.Plus, src_addr, cst_i)) in
                 logicalize_memory expr_addr expr_content env;  (* TODO: Do it 4 bytes by 4 bytes to make less store *)
                 i := !i +1
                 done*)
            let src_alias = (uniq_prefix^"_memcpy_src_alias") in (*SmtBvVar((uniq_prefix^"_strcpy_src_addr"),8) in*)
            let src_alias = CVT.add_alias 1 src_alias infos env in
            let dst_alias = (uniq_prefix^"_memcpy_dst_alias") in (*SmtBvVar((uniq_prefix^"_strcpy_src_addr"),8) in*)
            let dst_alias = CVT.add_alias 0 dst_alias infos env in
            if pol.Memcpy_pol.src.Memory_pol.addr = `conc then
              let cond =
                SmtComp(
                  SmtBvExpr(src_alias),
                  SmtBvExpr(SmtBvCst(bv_create64 data.Memcpy_t.src.Memory_t.addr 32))) in
              env.formula <- Formula.add_constraint env.formula cond;
              let size_max = data.Memcpy_t.size in
              let rec create_expr dst_ori src_ori off size_max =
                if((Int64.compare off size_max)>=0) then ()
                else begin
                  let mem_name =
                    "memory" ^ (string_of_int ((get_varindex env.formula
                                                  "memory")-1)) in
                  let smt_bv = SmtBvCst (bv_create64 off addr_size) in
                  let dst = SmtBvBinary(SmtBvAdd, dst_ori, smt_bv) in
                  let src = SmtBvBinary(SmtBvAdd, src_ori, smt_bv) in
                  let load_int_src =
                    SmtABvLoad32(SmtABvArray(mem_name, addr_size, addr_size), src) in
                  let load_char_src = SmtBvUnary(SmtBvExtract(0,7),load_int_src) in
                  env.formula <-
                    store_memory env.formula (SmtABvStore(env.formula.memory, dst,load_char_src)) ;
                  (*Formula.add_constraint env.formula cond in*)
                  let off = Int64.succ off in
                  create_expr dst_ori src_ori off size_max
                end
              in
              let fexpr = SmtBvCst(bv_create64 data.Memcpy_t.ret addr_size) in
              replace_register "eax" fexpr env;
              create_expr dst_alias src_alias 0L size_max
          | _, _ -> ()
        end
      | _ -> ()
    end;
    (match pol.Memcpy_pol.ret, dstaddr with
     | `logic, Some dst_addr -> (* dest pointer is returned in eax *)
       logicalize_register "eax" dst_addr env
     | _ ->
       CVT.set_ret (uniq_prefix^"_memcpy_ret")
         data.Memcpy_t.ret pol.Memcpy_pol.ret infos default env);
    CVT.set_epilog 3 (uniq_prefix^"_memcpy") infos env

  let to_string (data:memcpy_t): string =
    let open Memcpy_t in
    let open Memory_t in
    Format.sprintf
      "memcpy(dest:@@[%Lx](%d),src:@@[%Lx](%d), size:%d):%Lx, string %s"
      data.dest.addr (String.length data.dest.value)
      data.src.addr (String.length data.src.value)
      (Int64.to_int data.size) data.ret data.src.value

  let serialize_stack_params (data:memcpy_t): string =
    let open Memcpy_t in
    let open Memory_t in
    Libcall_utils.serialize_int64_list [data.dest.addr; data.src.addr; data.size]
end


module Realloc_call:
  LibCall with type data_t = realloc_t and type pol_t = realloc_pol =
struct
  type pol_t = realloc_pol
  type data_t = realloc_t

  let check_consistency (pol:realloc_pol) (_default:action): bool =
    let open Realloc_pol in
    pol.ret=`conc

  let apply_policy
      (_pol:realloc_pol) (data:realloc_t) (cvt:(module CallConvention))
      (_uniq_prefix:string) (infos:conc_infos) (_default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_epilog 0 "" infos env;
    let addr_size = env.Path_pred_env.addr_size in
    let fexpr = SmtBvCst(bv_create64 data.Realloc_t.ret addr_size) in
    replace_register "eax" fexpr env;
    if data.Realloc_t.ret <> data.Realloc_t.ptr && data.Realloc_t.ptr <> 0L then
      let src_alias = SmtBvCst(bv_create64 data.Realloc_t.ptr addr_size) in
      let dst_alias = fexpr in
      let size_max = data.Realloc_t.size in
      let rec create_expr dst_ori src_ori off size_max =
        if Int64.compare off size_max < 0 then begin
          let mem_name =
            "memory" ^ (string_of_int ((get_varindex env.formula "memory")-1))
          in
          let smt_bv = SmtBvCst (bv_create64 off addr_size) in
          let dst = SmtBvBinary(SmtBvAdd, dst_ori, smt_bv)  in
          let src = SmtBvBinary(SmtBvAdd, src_ori, smt_bv)  in
          let load_int_src = SmtABvLoad32(SmtABvArray(mem_name,addr_size,addr_size),src) in
          let load_char_src = SmtBvUnary(SmtBvExtract(0,7),load_int_src) in
          env.formula <-
            store_memory env.formula
              (SmtABvStore(env.formula.memory,dst, load_char_src));
          let off = Int64.succ off in
          create_expr dst_ori src_ori off size_max
        end
      in create_expr dst_alias src_alias 0L size_max


  let to_string (data:realloc_t): string =
    let open Realloc_t in
    Format.sprintf "realloc (%Lx,%Lx) -> %Lx" data.ptr data.size data.ret

  let serialize_stack_params (data:realloc_t): string =
    let open Realloc_t in
    Libcall_utils.serialize_int64_list [data.ptr; data.size]
end

(* TODO *)
module Memset_call: LibCall with type data_t = memset_t and type pol_t = memset_pol = struct
  type pol_t = memset_pol
  type data_t = memset_t

  let check_consistency (pol:memset_pol) (default:action): bool =
    let open Memset_pol in
    let open Memory_pol in
    check_consistency_memory_t pol.s.addr pol.s.value "memset(s)" default

  let apply_policy
      (pol:memset_pol) (data:memset_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    let dstaddr =
      CVT.set_param_pointer 0 (uniq_prefix^"_memset_s")
        pol.Memset_pol.s data.Memset_t.s infos default env in
    let char_val =
      CVT.set_param 1 (uniq_prefix^"_memset_c")
        data.Memset_t.c pol.Memset_pol.c infos default env in
    CVT.set_param 2 (uniq_prefix^"_memset_size")
      data.Memset_t.size pol.Memset_pol.size infos default env |> ignore;
    Logger.debug "Start dealing with the content";
    begin
      match char_val, dstaddr with
      | Some c, Some dst_addr ->
        begin
          match pol.Memset_pol.s.Memory_pol.value with
          | `logic ->
            let i = ref 0 in
            let size = Int64.to_int data.Memset_t.size in
            while !i < size do
              (* Implicit concretization of size *)
              let bigv = Bigint.big_int_of_int !i in
              let cst_i =
                Dba_types.Expr.constant
                  (Bitvector.create bigv env.Path_pred_env.addr_size) in
              let expr_addr = Dba.ExprBinary(Dba.Plus, dst_addr, cst_i) in
              (* Because it is a char *)
              let expr_content = Dba.ExprRestrict(c, 0, 7) in
              logicalize_memory expr_addr expr_content env;
              incr i
            done
          | _ -> ()
        end
      | _ -> ()
    end;
    (match pol.Memset_pol.ret, dstaddr with
     | `logic, Some dst_addr -> (* dest pointer is returned in eax *)
       logicalize_register "eax" dst_addr env
     | _ ->
       CVT.set_ret (uniq_prefix^"_memset_ret")
         data.Memset_t.ret pol.Memset_pol.ret infos default env);
    CVT.set_epilog 3 (uniq_prefix^"_memset") infos env

  let to_string (data:memset_t): string =
    let open Memset_t in
    let open Memory_t in
    Format.sprintf
      "memset(s:@[%Lx](%d),c:'%s', size:%d):%Lx"
      data.s.addr (String.length data.s.value)
      (Decode_utils.int64_to_char data.c |> Char.escaped)
      (Int64.to_int data.size) data.ret

  let serialize_stack_params (data:memset_t): string =
    let open Memset_t in
    let open Memory_t in
    Libcall_utils.serialize_int64_list [data.s.addr; data.c; data.size]

end

module Fgetc_call:
  LibCall with type data_t = fgetc_t and type pol_t = fgetc_pol =
struct
  type pol_t = fgetc_pol
  type data_t = fgetc_t

  let check_consistency _ _ = true

  let apply_policy
      (pol:fgetc_pol) (data:fgetc_t) (cvt:(module CallConvention))
      (uniq_prefix:string) (infos:conc_infos) (default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_param 0 (uniq_prefix^"_fgetc_stream")
      data.Fgetc_t.stream pol.Fgetc_pol.stream infos default env |> ignore;
    CVT.set_ret (uniq_prefix^"_fgetc_ret")
      data.Fgetc_t.ret pol.Fgetc_pol.ret infos default env;
    CVT.set_epilog 1 (uniq_prefix^"_fgetc") infos env

  let to_string (data:fgetc_t): string =
    let open Fgetc_t in
    Format.sprintf "fgetc(%Lx):'%s'"
      data.stream
      (Decode_utils.int64_to_char data.ret |> Char.escaped)

  let serialize_stack_params (data:fgetc_t): string =
    Libcall_utils.serialize_int64_list [data.Fgetc_t.stream]
end

module Fstat_call:
  LibCall with type data_t = fstat_t and type pol_t = fstat_pol =
struct
  type pol_t = fstat_pol
  type data_t = fstat_t

  let check_consistency _ _ = true

  let apply_policy
      (_pol:fstat_pol) (data:fstat_t) (cvt:(module CallConvention))
      (_uniq_prefix:string) (infos:conc_infos) (_default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_epilog 0 "" infos env;
    (* sizeof struct stat, in int32 = 88 *)
    let size_max = 88L in
    let addr_size = env.Path_pred_env.addr_size in
    let rec create_expr buf_addr vals off size_max =
      if Int64.compare off size_max < 0 then begin
        let addr = Bigint.big_int_of_int64 (Int64.add buf_addr off) in
        let addr = SmtBvCst(Bitvector.create addr addr_size) in
        let load_int =
          let v =
            Bigint.big_int_of_int
              (Char.code (String.get vals (Int64.to_int off))) in
          SmtBvCst(Bitvector.create v 8) in
        let load_char = SmtBvUnary(SmtBvExtract(0,7),load_int) in
        env.formula <-
          store_memory env.formula
            (SmtABvStore(env.formula.memory, addr, load_char));
        let off = Int64.succ off in
        create_expr buf_addr vals off size_max
      end
    in
    let addr_ret = SmtBvCst(bv_create64 data.Fstat_t.ret addr_size) in
    replace_register "eax" addr_ret env;
    create_expr
      data.Fstat_t.buf.Memory_t.addr data.Fstat_t.buf.Memory_t.value 0L size_max


  let to_string (data:fstat_t): string =
    let open Fstat_t in
    let open Memory_t in
    Format.sprintf "fstat: fd %Lx, buf %Lx, ret %Ld"
      data.fd data.buf.addr data.ret

  let serialize_stack_params (data:fstat_t): string =
    let open Fstat_t in
    let open Memory_t in
    Libcall_utils.serialize_int64_list [data.fd; data.buf.addr; data.ret]
end




module Fxstat64_call:
  LibCall with type data_t = fxstat64_t and type pol_t = fxstat64_pol =
struct
  type pol_t = fxstat64_pol
  type data_t = fxstat64_t

  let check_consistency _ _ = true

  let apply_policy
      (_pol:fxstat64_pol) (data:fxstat64_t) (cvt:(module CallConvention))
      (_uniq_prefix:string) (infos:conc_infos) (_default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_epilog 0 "" infos env;
    (* sizeof struct stat, in int32 = 88 *)
    let size_max = 88L in
    let addr_size = env.Path_pred_env.addr_size in
    let rec create_expr buf_addr vals off size_max =
      if Int64.compare off size_max < 0 then begin
        let addr = Bigint.big_int_of_int64 (Int64.add buf_addr off) in
        let addr = SmtBvCst(Bitvector.create addr addr_size) in
        let load_int =
          let v =
            Bigint.big_int_of_int
              (Char.code (String.get vals (Int64.to_int off))) in
          SmtBvCst(Bitvector.create v 8) in
        let load_char = SmtBvUnary(SmtBvExtract(0,7),load_int) in
        env.formula <-
          store_memory env.formula
            (SmtABvStore(env.formula.memory, addr, load_char));
        let off = Int64.succ off in
        create_expr buf_addr vals off size_max
      end
    in
    let addr_ret = SmtBvCst(bv_create64 data.Fxstat64_t.ret addr_size) in
    replace_register "eax" addr_ret env;
    create_expr
      data.Fxstat64_t.buf.Memory_t.addr data.Fxstat64_t.buf.Memory_t.value 0L size_max


  let to_string (data:fxstat64_t): string =
    let open Fxstat64_t in
    let open Memory_t in
    Format.sprintf "fxstat64: vers %Lx fd %Lx, buf %Lx, ret %Ld"
      data.vers data.fd data.buf.addr data.ret

  let serialize_stack_params (data:fxstat64_t): string =
    let open Fxstat64_t in
    let open Memory_t in
    Libcall_utils.serialize_int64_list [data.vers;data.fd; data.buf.addr; data.ret]
end


module Qsort_call:
  LibCall with type data_t = qsort_t and type pol_t = qsort_pol =
struct
  type pol_t = qsort_pol
  type data_t = qsort_t

  let check_consistency _ _ = true

  let apply_policy
      (_pol:qsort_pol) (data:qsort_t) (cvt:(module CallConvention))
      (_uniq_prefix:string) (infos:conc_infos) (_default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_epilog 0 "" infos env;
    let size_max = Int64.mul data.Qsort_t.size data.Qsort_t.nmemb in
    let addr_size = env.Path_pred_env.addr_size in
    let rec create_expr buf_addr vals off size_max =
      if Int64.compare off size_max < 0 then begin
        let addr = Bigint.big_int_of_int64 (Int64.add buf_addr off) in
        let addr = SmtBvCst(Bitvector.create addr addr_size) in
        let load_int =
          let v =
            Bigint.big_int_of_int
              (Char.code (String.get vals (Int64.to_int off))) in
          SmtBvCst(Bitvector.create v 8) in
        let load_char = SmtBvUnary(SmtBvExtract(0, 7),load_int) in
        env.formula <-
          store_memory env.formula
            (SmtABvStore(env.formula.memory, addr, load_char));
        let off = Int64.succ off in
        create_expr buf_addr vals off size_max
      end
    in create_expr data.Qsort_t.base.Memory_t.addr data.Qsort_t.base.Memory_t.value 0L size_max

  let to_string (data:qsort_t): string =
    let open Qsort_t in
    Format.sprintf "Qsort: base %Lx, nmemb %Lx, size %Ld, compare %Lx"
      data.Qsort_t.base.Memory_t.addr data.nmemb data.size data.compare

  let serialize_stack_params (data:qsort_t): string =
    let open Qsort_t in
    Libcall_utils.serialize_int64_list
      [data.Qsort_t.base.Memory_t.addr; data.nmemb; data.size; data.compare]
end


module Bsearch_call:
  LibCall with type data_t = bsearch_t and type pol_t = bsearch_pol =
struct
  type pol_t = bsearch_pol
  type data_t = bsearch_t

  let check_consistency _ _ = true

  let apply_policy
      (_pol:bsearch_pol) (data:bsearch_t) (cvt:(module CallConvention))
      (_uniq_prefix:string) (infos:conc_infos) (_default:action) (env:Path_pred_env.t): unit =
    let module CVT = (val cvt: CallConvention) in
    CVT.set_epilog 0 "" infos env;
    let size_max = data.Bsearch_t.size in
    let addr_size = env.Path_pred_env.addr_size in
    let rec create_expr buf_addr vals off size_max =
      if Int64.compare off size_max < 0 then begin
        let addr = Bigint.big_int_of_int64 (Int64.add buf_addr off) in
        let addr = SmtBvCst(Bitvector.create addr addr_size) in
        let load_int =
          SmtBvCst(Bitvector.create (Bigint.big_int_of_int (Char.code (String.get vals (Int64.to_int off)))) 8) in
        let load_char = SmtBvUnary(SmtBvExtract(0,7),load_int) in
        env.formula <- store_memory env.formula (SmtABvStore(env.formula.memory,addr,load_char));
        let off = Int64.add off 1L in
        create_expr buf_addr vals off size_max
      end
    in
    let addr_ret = SmtBvCst(bv_create64 data.Bsearch_t.ret.Memory_t.addr addr_size) in
    replace_register "eax" addr_ret env;
    create_expr
      data.Bsearch_t.ret.Memory_t.addr data.Bsearch_t.ret.Memory_t.value 0L size_max

  let to_string (data:bsearch_t): string =
    let open Bsearch_t in
    let open Memory_t in
    Format.sprintf
      "Beasrch, key %Lx,  base %Lx, nmemb %Lx, size %Ld, compare %Lx"
      data.key.addr data.Bsearch_t.base.addr data.nmemb data.size data.compare

  let serialize_stack_params (data:bsearch_t): string =
    let open Bsearch_t in
    let open Memory_t in
    Libcall_utils.serialize_int64_list
      [data.key.addr; data.Bsearch_t.base.addr;
       data.nmemb; data.size; data.compare]
end
