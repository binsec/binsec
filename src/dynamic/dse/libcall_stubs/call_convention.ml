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

open Smtlib2
open Common_piqi
open Memory_t
open Path_pred_env
open Formula_utils
open Trace_type
open Decode_utils
open Formula

module type CallConvention =
sig
  val get_param: int -> Path_pred_env.t -> Dba.expr
  val set_param: int -> string -> int64 -> Common_piqi.action -> conc_infos -> Common_piqi.action -> Path_pred_env.t -> Dba.expr option
  val set_param_pointer: int -> string -> memory_pol -> memory_t -> conc_infos -> Common_piqi.action -> Path_pred_env.t -> Dba.expr option
  val set_ret: string -> int64 -> ?supp:int64 option -> Common_piqi.action -> conc_infos -> Common_piqi.action -> Path_pred_env.t -> unit
  val set_epilog: int -> string -> conc_infos -> Path_pred_env.t -> unit
  val add_alias: int -> string -> conc_infos -> Path_pred_env.t -> smt_bv_expr
  val default_stub: string -> Path_pred_env.t -> unit
end

let bytesize (env:Path_pred_env.t) =  env.addr_size / 8

module Cdecl: CallConvention = struct
  let get_param (index:int) (env:Path_pred_env.t): Dba.expr =
    let bytesize = bytesize env in
    let offset = Bigint.big_int_of_int (bytesize + index * bytesize) in
    let expr_addr =
      Dba.ExprBinary(Dba.Plus,
                     Dba.ExprVar("esp", Machine.Word_size.get (), None),
                     Dba_types.Expr.constant
                       (Bitvector.create offset env.addr_size))
    in Dba.ExprLoad(bytesize, Dba.LittleEndian, expr_addr)

  let rec set_param (index:int) (prefix:string) (value:int64) (action:Common_piqi.action
                                                ) (infos:conc_infos
                                                  ) (default:Common_piqi.action
                                                    ) (env:Path_pred_env.t)
    : Dba.expr option =
    let bytesize = bytesize env in
    let offset = Bigint.big_int_of_int (bytesize + index * bytesize) in
    let expr_addr = Dba.ExprBinary(Dba.Plus,
                                   Dba.ExprVar("esp",32,None),
                                   Dba.ExprCst(`Constant, Bitvector.create offset env.addr_size)) in
    match action with
    | `default -> set_param index prefix value default infos default env
    | `patch -> failwith "Invalid action patch for libcall policy"
    | `conc ->
      let encoded = int64_to_littleendian_bin value env.addr_size in
      concretize_memory expr_addr encoded env;
      Some(Dba_types.Expr.constant (Bitvector.create (Bigint.big_int_of_int64 value) (Machine.Word_size.get ())))
    | `symb ->
      symbolize_memory expr_addr prefix (env.addr_size/8) env;
      Some (Dba.ExprVar(prefix, env.addr_size, None))
    | `ignore -> None
    | `logic -> Some(expr_addr) (* Print a warning ? (logic stuff should be handled in the stub itself) *)

  let rec set_param_pointer (index:int) (prefix:string) (pol:memory_pol) (data:memory_t) (infos:conc_infos) (default:Common_piqi.action) (env:Path_pred_env.t): Dba.expr option =
    let offset = Bigint.big_int_of_int ((env.addr_size/8)+((index*env.addr_size)/8)) in   (* call_site + (nth*align) *)
    let expr_addr = Dba.ExprBinary(Dba.Plus, Dba.ExprVar("esp",32,None), Dba.ExprCst(`Constant, Bitvector.create offset env.addr_size)) in
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
      let encoded = int64_to_littleendian_bin data.addr env.addr_size in
      let _:unit = concretize_memory expr_addr encoded env in
      let expr_addr_deref = Dba.ExprCst(`Constant, Bitvector.create (Bigint.big_int_of_int64 data.addr) env.addr_size) in
      aux_deref expr_addr_deref pol.Memory_pol.value data.value;
      Some(expr_addr_deref)
    | `symb ->
      let name = prefix^"_addr" in
      symbolize_memory expr_addr name (env.addr_size/8) env;
      Some(Dba.ExprVar(name, env.addr_size, None))
    | `logic ->
      let expr_addr_deref = Dba.ExprLoad(env.addr_size/8, Dba.LittleEndian, expr_addr) in (* [esp + X] *)
      aux_deref expr_addr_deref pol.Memory_pol.value data.Memory_t.value;
      Some(expr_addr_deref)
    | `ignore -> None


  let rec set_ret (prefix:string) (value:int64) ?(supp=None) (action:Common_piqi.action) (infos:conc_infos) (default:Common_piqi.action) (env:Path_pred_env.t): unit =
    match action with
    | `default -> set_ret prefix value ~supp default infos default env
    | `patch -> failwith "Invalid action patch for libcall policy"
    | `conc ->
      let fexpr = SmtBvCst(Bitvector.create (Bigint.big_int_of_int64 value) env.addr_size) in
      symbolize_and_then_concretize_register "eax" fexpr prefix env;
      (* (match supp with | None -> () | Some _ -> ()) (* TODO:Do something *) *)
    | `symb ->
      symbolize_register "eax" prefix env (* Symbolise the whole eax even if size is smaller *)
    | `ignore -> ()
    | `logic -> ()


  let set_epilog (_:int) (prefix:string) (_infos:conc_infos) (env:Path_pred_env.t): unit =
    List.iter (fun i -> symbolize_register i prefix env) ["ecx"; "edx"; "CF"; "DF"; "ZF"; "OF"; "SF"; "AF"; "PF"]; (* eax not here because already threated by set_ret normally *)
    let f_expr =
      SmtBvBinary(SmtBvAdd, SmtBvToken,
                  SmtBvCst(Bitvector.create (Bigint.big_int_of_int (env.addr_size/8)) env.addr_size)) in
    replace_register "esp" f_expr env (* simulate the ret that has normally been done *)

  let default_stub (prefix:string) (env:Path_pred_env.t): unit =
    List.iter (fun i -> symbolize_register i prefix env) ["eax";"ecx"; "edx"; "CF"; "DF"; "ZF"; "OF"; "SF"; "AF"; "PF"];
    let f_expr =
      SmtBvBinary(SmtBvAdd, SmtBvToken,
                  SmtBvCst(Bitvector.create (Bigint.big_int_of_int (env.addr_size/8)) env.addr_size)) in
    replace_register "esp" f_expr env (* simulate the ret that has normally been done *)

  let add_alias (index:int) (alias:string) (infos:conc_infos) (env:Path_pred_env.t) =
    let mem_name = "memory" ^ (string_of_int ((get_varindex env.formula "memory")-1)) in
    let offset = (env.addr_size/8)+(index*(env.addr_size/8)) in   (* call_site + (nth*align) *)
    let esp_value = get_reg_write_or_read_value "esp" infos in
    let addr = SmtBvCst(Bitvector.create (Bigint.big_int_of_int64 (Int64.add esp_value (Int64.of_int offset))) env.addr_size) in
    let value =  SmtABvLoad32(SmtABvArray(mem_name,env.addr_size,env.addr_size),addr) in
    let alias_var = SmtBvVar(alias,env.addr_size) in
    let new_f = add_symbolic_input env.formula alias env.addr_size in
    env.formula <- new_f;
    env.formula <-
      add_constraint env.formula (SmtComp(SmtBvExpr alias_var, SmtBvExpr value));
    alias_var



end;;

module Stdcall: CallConvention = struct
  let get_param (index:int) (env:Path_pred_env.t): Dba.expr =
    Cdecl.get_param index env

  let set_param (index:int) (prefix:string) (value:int64) (action:Common_piqi.action) (infos:conc_infos) (default:Common_piqi.action) (env:Path_pred_env.t): Dba.expr option =
    Cdecl.set_param index prefix value action infos default env

  let set_param_pointer (index:int) (prefix:string) (pol:memory_pol) (data:memory_t) (infos:conc_infos) (default:Common_piqi.action) (env:Path_pred_env.t): Dba.expr option =
    Cdecl.set_param_pointer index prefix pol data infos default env

  let set_ret (prefix:string) (value:int64) ?(supp=None) (action:Common_piqi.action) (infos:conc_infos) (default:Common_piqi.action) (env:Path_pred_env.t): unit =
    Cdecl.set_ret prefix value ~supp action infos default env

  let set_epilog (nb_param:int) (prefix:string) (infos:conc_infos) (env:Path_pred_env.t): unit =
    List.iter (fun i -> symbolize_register i prefix env) ["ecx"; "edx"; "CF"; "DF"; "ZF"; "OF"; "SF"; "AF"; "PF"]; (* eax not here because already threated by set_ret normally *)
    let sz = env.addr_size/8 in
    let esp_value = Int64.add (get_reg_value "esp" infos) (Int64.of_int (nb_param*sz)) in
    let f_val = SmtBvCst(Bitvector.create (Bigint.big_int_of_int64 esp_value) env.addr_size) in
    symbolize_and_then_concretize_register "esp" f_val prefix env

  let default_stub (prefix:string) (env:Path_pred_env.t): unit =
    List.iter (fun i -> symbolize_register i prefix env) ["eax";"ecx"; "edx";"esp"; "CF"; "DF"; "ZF"; "OF"; "SF"; "AF"; "PF"]

  let add_alias (index:int) (alias:string)  (infos:conc_infos) (env:Path_pred_env.t) =
    Cdecl.add_alias index alias infos env

end;;
