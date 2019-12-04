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

open Formula
open Common_piqi
open Memory_t
open Path_predicate_env
open Path_predicate_utils
open Trace_type
open Decode_utils
open Path_predicate_formula

module type CallConvention =
sig
  val get_param: int -> Path_predicate_env.t -> Dba.Expr.t
  val set_param:
    int -> string -> int64 -> Common_piqi.action ->
    conc_infos -> Common_piqi.action -> Path_predicate_env.t -> Dba.Expr.t option
  val set_param_pointer:
    int -> string -> memory_pol -> memory_t ->
    conc_infos -> Common_piqi.action -> Path_predicate_env.t -> Dba.Expr.t option
  val set_ret: string -> int64 -> ?supp:int64 option -> Common_piqi.action -> conc_infos -> Common_piqi.action -> Path_predicate_env.t -> unit
  val set_epilog: int -> string -> conc_infos -> Path_predicate_env.t -> unit
  val add_alias: int -> string -> conc_infos -> Path_predicate_env.t -> bv_term
  val default_stub: string -> Path_predicate_env.t -> unit
end

let bytesize (env:Path_predicate_env.t) =  env.formula.addr_size / 8

module Cdecl: CallConvention = struct

  let esp = Dba.Expr.var "esp" 32

  let get_param (index:int) (env:Path_predicate_env.t): Dba.Expr.t =
    let bytesize = bytesize env in
    let offset = Bigint.big_int_of_int ((index + 1) * bytesize) in
    let expr_addr =
      let bv_offset = Bitvector.create offset env.formula.addr_size in
      Dba.Expr.add esp (Dba.Expr.constant bv_offset) in
    let bysz = Size.Byte.create bytesize in
    Dba.Expr.load bysz (Machine.LittleEndian) expr_addr

  let rec set_param index (prefix:string) (value:int64)
      (action:Common_piqi.action) (infos:conc_infos)
      (default:Common_piqi.action) (env:Path_predicate_env.t) =
    let bytesize = bytesize env in
    let offset = Bigint.big_int_of_int (bytesize + index * bytesize) in
    let expr_addr =
      let bv_offset = Bitvector.create offset env.formula.addr_size in
      Dba.Expr.add esp (Dba.Expr.constant bv_offset) in
    match action with
    | `default -> set_param index prefix value default infos default env
    | `patch -> failwith "Invalid action patch for libcall policy"
    | `conc ->
      let encoded = int64_to_littleendian_bin value env.formula.addr_size in
      concretize_memory expr_addr encoded env;
      Some (Dba.Expr.constant (Bitvector.create (Bigint.big_int_of_int64 value) (Kernel_options.Machine.word_size ())))
    | `symb ->
      symbolize_memory expr_addr prefix (env.formula.addr_size/8) env;
      Some (Dba.Expr.var prefix env.formula.addr_size)
    | `ignore -> None
    | `logic -> Some expr_addr (* Print a warning ? (logic stuff should be handled in the stub itself) *)

  let rec set_param_pointer
      (index:int) (prefix:string) (pol:memory_pol) (data:memory_t)
      (infos:conc_infos) (default:Common_piqi.action) (env:Path_predicate_env.t)
    : Dba.Expr.t option =
    let offset = Bigint.big_int_of_int ((env.formula.addr_size/8)+((index*env.formula.addr_size)/8)) in   (* call_site + (nth*align) *)

    let expr_addr =
      let bv_offset = Bitvector.create offset env.formula.addr_size in
      Dba.Expr.add esp (Dba.Expr.constant bv_offset) in

    let rec aux_deref expr_addr_deref pol_value data_value =
      match pol_value with
      | `default -> aux_deref expr_addr_deref default data_value
      | `patch -> failwith "invalid action path for pointer value"
      | `ignore -> () (* Does nothing *)
      | `logic -> () (* Print warning ? shouldn't reach here *)
      | `conc ->
        concretize_memory expr_addr_deref data_value env
      | `symb ->
        symbolize_memory expr_addr_deref (prefix^"_value") (String.length data_value) env (* Important: We are implicitly concretizing the length of data ! *)
    in
    match pol.Memory_pol.addr with
    | `default -> set_param_pointer index prefix {pol with Memory_pol.addr = default} data infos default env
    | `patch -> failwith "Invalid action patch for libcall policy"
    | `conc ->
      let encoded = int64_to_littleendian_bin data.addr env.formula.addr_size in
      let _:unit = concretize_memory expr_addr encoded env in
      let expr_addr_deref =
        let bv =
          Bitvector.create (Bigint.big_int_of_int64 data.addr) env.formula.addr_size in
        Dba.Expr.constant bv in
      aux_deref expr_addr_deref pol.Memory_pol.value data.value;
      Some(expr_addr_deref)
    | `symb ->
      let name = prefix^"_addr" in
      symbolize_memory expr_addr name (env.formula.addr_size/8) env;
      Some (Dba.Expr.var name env.formula.addr_size)
    | `logic ->
       let bysz = Size.Byte.unsafe_of_bits env.formula.addr_size in
      let expr_addr_deref =
        Dba.Expr.load bysz Machine.LittleEndian expr_addr in (* [esp + X] *)
      aux_deref expr_addr_deref pol.Memory_pol.value data.Memory_t.value;
      Some expr_addr_deref
    | `ignore -> None


  let rec set_ret
      (prefix:string) (value:int64) ?(supp=None) (action:Common_piqi.action)
      (infos:conc_infos) (default:Common_piqi.action) (env:Path_predicate_env.t): unit =
    match action with
    | `default -> set_ret prefix value ~supp default infos default env
    | `patch -> failwith "Invalid action patch for libcall policy"
    | `conc ->
      let fexpr = mk_bv_cst (Bitvector.create (Bigint.big_int_of_int64 value) env.formula.addr_size) in
      symbolize_and_then_concretize_register "eax" fexpr prefix env;
      (* (match supp with | None -> () | Some _ -> ()) (* TODO:Do something *) *)
    | `symb ->
      symbolize_register "eax" prefix env (* Symbolise the whole eax even if size is smaller *)
    | `ignore -> ()
    | `logic -> ()


  let set_epilog (_:int) (prefix:string) (_infos:conc_infos) (env:Path_predicate_env.t): unit =
    List.iter
      (fun i -> symbolize_register i prefix env)
      ["ecx"; "edx"; "CF"; "DF"; "ZF"; "OF"; "SF"; "AF"; "PF"]; (* eax not here because already threated by set_ret normally *)
    let f_expr =
      mk_bv_add (mk_bv_var (bv_var "token" env.formula.addr_size))
        (mk_bv_cst (Bitvector.create (Bigint.big_int_of_int (env.formula.addr_size/8)) env.formula.addr_size))
    in
    replace_register "esp" f_expr env (* simulate the ret that has normally been done *)

  let default_stub (prefix:string) (env:Path_predicate_env.t): unit =
    List.iter (fun i -> symbolize_register i prefix env) ["eax";"ecx"; "edx"; "CF"; "DF"; "ZF"; "OF"; "SF"; "AF"; "PF"];
    let f_expr =
      mk_bv_add (mk_bv_var (bv_var "token" env.formula.addr_size))
        (mk_bv_cst (Bitvector.create (Bigint.big_int_of_int (env.formula.addr_size/8)) env.formula.addr_size))
    in
    replace_register "esp" f_expr env (* simulate the ret that has normally been done *)

  let add_alias (index:int) (alias:string) (infos:conc_infos) (env:Path_predicate_env.t) =
    let mem_name = "memory" ^ (string_of_int ((get_varindex env.formula "memory")-1)) in
    let offset = (env.formula.addr_size/8)+(index*(env.formula.addr_size/8)) in   (* call_site + (nth*align) *)
    let esp_value = get_reg_write_or_read_value "esp" infos in
    let addr =
      mk_bv_cst
        (Bitvector.create
           (Bigint.big_int_of_int64 (Int64.add esp_value (Int64.of_int offset)))
           env.formula.addr_size)
    in
    let value = mk_select 4 (mk_ax_var (ax_var mem_name env.formula.addr_size env.formula.addr_size)) addr in
    let alias_var = mk_bv_var (bv_var alias env.formula.addr_size) in
    let new_f = add_symbolic_input env.formula alias env.formula.addr_size in
    env.formula <- new_f;
    env.formula <-
      add_constraint env.formula (mk_bv_equal alias_var value);
    alias_var

end;;

module Stdcall: CallConvention = struct
  include Cdecl

  let set_epilog (nb_param:int) (prefix:string) (infos:conc_infos) env =
    List.iter (fun i -> symbolize_register i prefix env)
      ["ecx"; "edx"; "CF"; "DF"; "ZF"; "OF"; "SF"; "AF"; "PF"];
    (* eax not here because already threated by set_ret normally *)
    let sz = env.formula.addr_size/8 in
    let esp_value = Int64.add (get_reg_value "esp" infos) (Int64.of_int (nb_param*sz)) in
    let f_val = mk_bv_cst (Bitvector.create (Bigint.big_int_of_int64 esp_value) env.formula.addr_size) in
    symbolize_and_then_concretize_register "esp" f_val prefix env

  let default_stub (prefix:string) (env:Path_predicate_env.t): unit =
    List.iter (fun i -> symbolize_register i prefix env) ["eax";"ecx"; "edx";"esp"; "CF"; "DF"; "ZF"; "OF"; "SF"; "AF"; "PF"]
end;;
