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
open Formula_utils
open Formula_transformation
open Formula_pp
open Path_predicate_env
open Path_predicate_formula
open Decode_utils
open Dse_options

module Pp = Dba_printer.EICUnicode

let concretize_value (name:string) (size:int) (low:int) (high:int) (f_expr:bv_term) (env:Path_predicate_env.t): unit =
  Logger.debug ~level:2 "Will concretize value %s[%d]{%d,%d}" name size low high;
  let new_f, var_f = get_var_or_create env.Path_predicate_env.formula name size low high in
  (* let new_f = change_variable new_f name size low high var_f in *)
  (* let new_f, var_f = get_var_or_create new_f name size low high in *)
  let cst = mk_bv_equal var_f f_expr in
  let new_f = add_constraint new_f cst in
  env.Path_predicate_env.formula <- new_f

let concretize_register (name:string) (f_expr:bv_term) (env:Path_predicate_env.t): unit =
  let fullname, low, high = X86Util.reg_to_extract name in
  let fullname, l, h = X86Util.reg_to_extract fullname in
  concretize_value fullname (h-l+1) low high f_expr env

let replace_value (name:string) (size:int) (low:int) (high:int) (f_expr:bv_term) (env:Path_predicate_env.t): unit =
  Logger.debug ~level:2 "Will logicalize value %s[%d]{%d,%d}" name size low high;
  let new_f, var_f = get_var_or_create env.Path_predicate_env.formula name size low high in
  let final_f =
    replace_bv_term
      (mk_bv_def (bv_var "token" (bv_size var_f)) [] var_f)
      f_expr
  in
  let new_f = change_variable new_f name size low high final_f in
  env.Path_predicate_env.formula <- new_f

let replace_register (name:string) (f_expr:bv_term) (env:Path_predicate_env.t): unit =
  let fullname, low, high = X86Util.reg_to_extract name in
  let fullname, l, h = X86Util.reg_to_extract fullname in
  replace_value fullname (h-l+1) low high f_expr env

let logicalize_register (name:string) (expr:Dba.Expr.t) (env:Path_predicate_env.t): unit =
  let f_expr, csts = env.Path_predicate_env.analysis#expr_to_smt expr env in
  env.Path_predicate_env.formula <- List.fold_left (fun acc c -> add_constraint acc c) env.Path_predicate_env.formula csts;
  replace_register name f_expr env

let rec concretize_memory (expr_addr:Dba.Expr.t) (data:string) (env:Path_predicate_env.t): unit =
  let sz = env.Path_predicate_env.formula.addr_size / 8 in
  let aux size data_cst =
    Logger.debug ~level:2 "Stub: concretize memory addr: [%a]==%s"
      Pp.pp_bl_term expr_addr (print_bv_term data_cst);
    let f_select, csts =
      let bysz = Size.Byte.create size in
      env.Path_predicate_env.analysis#expr_to_smt
        (Dba.Expr.load bysz Machine.LittleEndian expr_addr)
        env in
    let f_constraint = mk_bv_equal f_select data_cst in
    env.Path_predicate_env.formula <-
      List.fold_left add_constraint env.Path_predicate_env.formula
        (csts@[f_constraint]);
    let new_addr =
      let bv = Bitvector.create
          (Bigint.big_int_of_int size) env.Path_predicate_env.formula.addr_size in
      Dba.Expr.add expr_addr (Dba.Expr.constant bv) in
    concretize_memory new_addr (String_utils.lchop size data) env
  in
  match String.length data with
  | 0 -> ()
  | x when x >= sz ->
    let chunk = String_utils.left sz data in
    let data_cst =
      mk_bv_cst (Bitvector.create (string_to_big_int chunk) env.Path_predicate_env.formula.addr_size)
    in aux sz data_cst
  | _ -> (* Just do one *)
    let data_cst =
      mk_bv_cst (Bitvector.create (String.get data 0 |> Char.code |> Bigint.big_int_of_int) 8)
    in
    aux 1 data_cst

let replace_memory (addr:int64) (data:string) (env:Path_predicate_env.t): unit =
  let len = String.length data in
  let rec aux f i =
    if i >= len then
      f
    else begin
      let offset = Int64.add addr (Int64.of_int i) in
      Logger.debug ~level:2 "Will store memory[%Lx]=0x%x" offset (Char.code (String.get data i));
      let f_cnt =
        mk_bv_cst (Bitvector.create (String.get data i |> Char.code |> Bigint.big_int_of_int)  8)
      in
      let f_addr =
        mk_bv_cst (Bitvector.create (Bigint.big_int_of_int64 offset)
                     env.Path_predicate_env.formula.addr_size)
      in
      let new_f = store_memory f (mk_store 1 f.memory f_addr f_cnt) in
      aux new_f (i+1)
    end
  in
  env.Path_predicate_env.formula <- aux env.Path_predicate_env.formula 0

let symbolize_value
    (name:string) (size:int) (low:int) (high:int) ?(is_full_name = false)
    (prefix_symbol:string) (env:Path_predicate_env.t): unit =
  Logger.debug ~level:2 "Will symbolise %s[%d]{%d,%d}" name size low high;
  let final_name = if(is_full_name) then prefix_symbol else prefix_symbol^"_"^name in
  let var_f = mk_bv_var (bv_var final_name (high-low+1)) in
  let new_f = add_symbolic_input env.Path_predicate_env.formula final_name (high-low+1) in
  env.Path_predicate_env.formula <- change_variable new_f name size low high var_f


let symbolize_register (name:string) ?(is_full_name = false) (prefix_symbol:string) (env:Path_predicate_env.t): unit =
  let fullname, low, high = X86Util.reg_to_extract name in
  let fullname, l, h = X86Util.reg_to_extract fullname in
  symbolize_value fullname  (h-l+1) low high ~is_full_name:is_full_name prefix_symbol env

let symbolize_memory_one_octet (addr:int64) (prefix_symbol:string)  (env:Path_predicate_env.t): unit =
  let f = env.Path_predicate_env.formula in
  let name = prefix_symbol in
  let offset = addr in
  let f_addr =
    mk_bv_cst (Bitvector.create (Bigint.big_int_of_int64 offset)
                 env.Path_predicate_env.formula.addr_size)
  in
  let var_f = mk_bv_var (bv_var name 8) in
  let new_f = add_symbolic_input f name 8 in
  let new_f = store_memory new_f (mk_store 1 new_f.memory f_addr var_f) in
  env.Path_predicate_env.formula <- new_f

let rec symbolize_memory expr_addr (prefix_symbol:string) ?(i=0) size env
  : unit =
  let sz = env.Path_predicate_env.formula.addr_size/8 in
  let aux in_sz =
    let name =  if in_sz = size then prefix_symbol else prefix_symbol^"_"^(string_of_int i) in
    Logger.debug ~level:2  "Stub: symbolize memory [%a]=%s"
      Pp.pp_bl_term expr_addr name;
    let addr = env.Path_predicate_env.analysis#get_current_dbacodeaddress () in
    let dba_instr =
      let bytes = Size.Byte.create in_sz in
      let bits = Size.Byte.to_bitsize bytes |> Size.Bit.to_int in
      assert ((bytes:>int) <> 0);
      Dba_types.Statement.create
        addr
        (Dba.Instr.assign
           (Dba.LValue.store bytes Machine.LittleEndian expr_addr)
           (Dba.Expr.var name bits)
           addr.Dba.id) in
    env.Path_predicate_env.analysis#exec dba_instr env;
    let new_addr =
      Dba.Expr.add expr_addr
        (Dba.Expr.constant
           (Bitvector.create (Bigint.big_int_of_int in_sz)
              env.Path_predicate_env.formula.addr_size)) in
    symbolize_memory new_addr prefix_symbol ~i:(i+in_sz) size env
  in
  match size-i with
  | 0 -> ()
  | x when x >= sz -> aux sz
  | _ -> aux 1

let logicalize_memory (expr_addr:Dba.Expr.t) (expr_content:Dba.Expr.t) (env:Path_predicate_env.t): unit =
  (* TODO: enforce the size of expr_content to be 8 bits size *)
  let addr = env.Path_predicate_env.analysis#get_current_dbacodeaddress () in
  let in_sz = Dba_utils.computesize_dbaexpr expr_content in
  let dba_instr =
    let bits = Size.Bit.create in_sz in
    let bytes = Size.Byte.of_bitsize bits in
    assert ((bytes:>int) <> 0);
    Dba_types.Statement.create
      addr
      (Dba.Instr.assign
         (Dba.LValue.store bytes Machine.LittleEndian expr_addr)
         expr_content addr.Dba.id) in
  env.Path_predicate_env.analysis#exec dba_instr env

let symbolize_and_then_concretize_register (name:string) (f_expr:bv_term) (prefix_symbol:string) (env:Path_predicate_env.t): unit =
  symbolize_register name prefix_symbol env;
  concretize_register name f_expr env (* TODO:attach this constraint to the variable declaration *)
