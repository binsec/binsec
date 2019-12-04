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

open Region_bitvector
open Concrete_state
open Simulate_options
open Simulate_utils

let perm = ref (Dba_types.Region.Map.empty : Dba_types.permissions list Dba_types.Region.Map.t)
let permis = ref (Dba_types.Rights.empty : Dba.Expr.t Dba_types.Rights.t)

module Env = Static_types.Env

let smt_true =
  let open Smt_bitvectors in
  let open Formula in
  SmtBvComparisonAlt
    (BvEqual,
     SmtBvCstAlt Bitvector.one,
     SmtBvCstAlt Bitvector.one)

let smt_false =
  let open Smt_bitvectors in
  let open Formula in
  SmtBvComparisonAlt
    (BvEqual,
     SmtBvCstAlt Bitvector.zero,
     SmtBvCstAlt Bitvector.one)


let eval_unop = function
  | Dba.Unary_op.UMinus -> neg
  | Dba.Unary_op.Not -> lognot
  | Dba.Unary_op.Sext n -> fun e -> signed_extension e n
  | Dba.Unary_op.Uext n -> fun e -> extension e n
  | Dba.Unary_op.Restrict {Interval.lo; Interval.hi;} ->
    fun e -> restrict e lo hi

let eval_binop = function
  | Dba.Binary_op.Plus        -> Region_bitvector.add
  | Dba.Binary_op.Minus       -> Region_bitvector.sub
  | Dba.Binary_op.Mult        -> Region_bitvector.mul
  | Dba.Binary_op.DivU        -> Region_bitvector.udiv
  | Dba.Binary_op.DivS        -> Region_bitvector.sdiv
  | Dba.Binary_op.ModU        -> Region_bitvector.umod
  | Dba.Binary_op.ModS        -> Region_bitvector.smod
  | Dba.Binary_op.Or          -> Region_bitvector.logor
  | Dba.Binary_op.And         -> Region_bitvector.logand
  | Dba.Binary_op.Xor         -> Region_bitvector.logxor
  | Dba.Binary_op.Concat      -> Region_bitvector.append
  | Dba.Binary_op.LShift      -> Region_bitvector.lshift
  | Dba.Binary_op.RShiftU     -> Region_bitvector.rshiftU
  | Dba.Binary_op.RShiftS     -> Region_bitvector.rshiftS
  | Dba.Binary_op.LeftRotate  -> Region_bitvector.rotate_left
  | Dba.Binary_op.RightRotate -> Region_bitvector.rotate_right
  | Dba.Binary_op.Eq          -> Region_bitvector.eq
  | Dba.Binary_op.Diff        -> Region_bitvector.diff
  | Dba.Binary_op.LeqU        -> Region_bitvector.leqU
  | Dba.Binary_op.LtU         -> Region_bitvector.ltU
  | Dba.Binary_op.GeqU        -> Region_bitvector.geqU
  | Dba.Binary_op.GtU         -> Region_bitvector.gtU
  | Dba.Binary_op.LeqS        -> Region_bitvector.leqS
  | Dba.Binary_op.LtS         -> Region_bitvector.ltS
  | Dba.Binary_op.GeqS        -> Region_bitvector.geqS
  | Dba.Binary_op.GtS         -> Region_bitvector.gtS


let rec eval_expr expr m conds glbs: Region_bitvector.t =
  Logger.debug ~level:3 "Evaluating %a" Dba_printer.Ascii.pp_bl_term expr;
  let rbv =
    match expr with
    | Dba.(Expr.Var {name = v; size; _}) ->
      let big_zero = Bigint.zero_big_int in
      begin
        try read (Static_types.Var (v, size)) big_zero m conds glbs
        with Not_found ->
        try read (Static_types.Var (v, size)) big_zero !m_init conds glbs
        with Not_found -> Simulate_utils.mk_undef_value size
      end

    | Dba.Expr.Load (size, endianness, e) ->
      Logger.debug "Loading ...";
      let append v1 v2 =
        match endianness with
        | Machine.BigEndian -> append v1 v2
        | Machine.LittleEndian -> append v2 v1
      in
      let vexp =
        match eval_expr e m conds glbs with
        | `SymbSmt smb ->
          Region_bitvector.get_value smb
            (Kernel_options.Machine.word_size ()) conds glbs
        | v -> v
      in
      let region = region_of vexp in
      let i = value_of vexp in
      let big_zero = Bigint.zero_big_int in
      let v = `Value (`Constant, bitvector_of vexp) in
      let sub_m = SubEnv.singleton big_zero v in
      let env = Env.add (Static_types.Var
                           ("\\addr", Kernel_options.Machine.word_size ()))
                  sub_m m in
      let read_at i env =
        let r = Static_types.Array region in
        try read r i env conds glbs
        with Not_found ->
        try read r i !m_init conds glbs
        with Not_found -> Region_bitvector.default_get_byte_region_at i
      in
      let ret = read_at i env in
      let limit = mk_sup i size in
      let open Bigint in
      let rec loop index vexp ret =
        Logger.debug "LOOP";
        if le_big_int index limit
        then
          let bv = `Value (`Constant, Bitvector.succ (bitvector_of vexp)) in
          let sub_m = SubEnv.singleton big_zero bv in
          let key = Static_types.Var
                      ("\\addr", Kernel_options.Machine.word_size ()) in
          let m = Env.add key sub_m m in
          loop (succ_big_int index) vexp (append ret (read_at index m))
        else ret
      in
      loop (succ_big_int i) vexp ret


    | Dba.Expr.Cst (r, bv) ->
      let region =
        match Semantic_mode.get () with
        | Flat -> `Constant
        | _ -> r
      in `Value (region, bv)

    | Dba.Expr.Unary (uop,expr) ->
      eval_expr expr m conds glbs |> eval_unop uop

    | Dba.Expr.Binary (bop, expr1, expr2) ->
      let v1 = eval_expr expr1 m conds glbs in
      let v2 = eval_expr expr2 m conds glbs in
      eval_binop bop v1 v2


    | Dba.Expr.Ite (cond, expr1, expr2) ->
      begin
        try
          let v = eval_expr cond m conds glbs in
          let next_e = if Region_bitvector.is_zero v then expr2 else expr1 in
          eval_expr next_e m conds glbs
        with
        | Smt_bitvectors.Assume_condition c ->
          begin match Conditional_strategy.get () with
            | Fail -> raise (Smt_bitvectors.Assume_condition c)
            | Branch_else ->
              let conds = add_smt_cond (Dba.Expr.lognot cond) m conds glbs in
              eval_expr expr2 m conds glbs
            | Branch_if ->
              let conds = add_smt_cond cond m conds glbs in
              eval_expr expr1 m conds glbs
          end
      end
  in
  Logger.debug "RBV: %a" Region_bitvector.pp rbv; rbv

and eval_cond c m conds glbs =
  let v = eval_expr c m conds glbs in
  not (Region_bitvector.is_zero v)

and get_smt_cond c m conds glbs =
  let open Smt_bitvectors in
  let open Formula in
  let open Dba in
  let open Binary_op in
  let bop op e1 e2 =
    let smt_cond1 = get_smt_cond e1 m conds glbs in
    let smt_cond2 = get_smt_cond e2 m conds glbs in
    SmtBvBinaryAlt (op, smt_cond1, smt_cond2) in
  match c with
  | Expr.Cst (_, v) ->
    if Bitvector.is_one v then smt_true
    else smt_false
  | Expr.Unary (Unary_op.Not, e) ->
    SmtBvUnaryAlt (BvNot, get_smt_cond e m conds glbs)
  | Expr.Binary(And, c1, c2) -> bop BvAnd c1 c2
  | Expr.Binary (Or, c1, c2) -> bop BvOr c1 c2
  | _ -> assert false

and add_smt_cond c m conds glbs =
  get_smt_cond c m conds glbs :: conds


(* Checking read of memory permissions here *)
and read x i m conds glbs =
  match x with
  | Static_types.Var _ ->
    SubEnv.find Bigint.zero_big_int (Env.find x m)
  | Static_types.Array r ->
    let c =
      try Dba_types.Rights.find (Dba_types.Rights.R, r) !permis
      with Not_found -> Dba.Expr.one in
    if eval_cond c m conds glbs then SubEnv.find i (Env.find x m)
    else raise Errors.Read_permission_denied

(* Checking write to memory permissions here *)
and write x i rbv m conds glbs =
  match x with
  | Static_types.Var _ ->
    let sub_m  = SubEnv.singleton Bigint.zero_big_int rbv in
    Env.add x sub_m m
  | Static_types.Array r ->
    let c =
      try Dba_types.Rights.find (Dba_types.Rights.W, r) !permis
      with Not_found -> Dba.Expr.one in
    if eval_cond c m conds glbs then
      let sub_m = try Env.find x m with Not_found -> SubEnv.empty in
      let sub_m = SubEnv.add i rbv sub_m in
      Env.add x sub_m m
    else
      raise Errors.Write_permission_denied
