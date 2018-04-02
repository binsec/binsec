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


open Errors
open Region_bitvector
open Concrete_state
open Simulate_options
open Simulate_utils

let perm = ref (Dba_types.Region.Map.empty : Dba_types.permissions list Dba_types.Region.Map.t)
let permis = ref (Dba_types.Rights.empty : Dba.cond Dba_types.Rights.t)

module MallocMap = Dba_types.Region.Map
module Env = Static_types.Env

let eval_unop = function
  | Dba.UMinus -> neg
  | Dba.Not -> lognot

let eval_binop = function
  | Dba.Plus        -> Region_bitvector.add
  | Dba.Minus       -> Region_bitvector.sub
  | Dba.MultU       -> Region_bitvector.umul
  | Dba.MultS       -> Region_bitvector.smul
  | Dba.DivU        -> Region_bitvector.udiv
  | Dba.DivS        -> Region_bitvector.sdiv
  | Dba.ModU        -> Region_bitvector.umod
  | Dba.ModS        -> Region_bitvector.smod
  | Dba.Or          -> Region_bitvector.logor
  | Dba.And         -> Region_bitvector.logand
  | Dba.Xor         -> Region_bitvector.logxor
  | Dba.Concat      -> Region_bitvector.append
  | Dba.LShift      -> Region_bitvector.lshift
  | Dba.RShiftU     -> Region_bitvector.rshiftU
  | Dba.RShiftS     -> Region_bitvector.rshiftS
  | Dba.LeftRotate  -> Region_bitvector.rotate_left
  | Dba.RightRotate -> Region_bitvector.rotate_right
  | Dba.Eq          -> Region_bitvector.eq
  | Dba.Diff        -> Region_bitvector.diff
  | Dba.LeqU        -> Region_bitvector.leqU
  | Dba.LtU         -> Region_bitvector.ltU
  | Dba.GeqU        -> Region_bitvector.geqU
  | Dba.GtU         -> Region_bitvector.gtU
  | Dba.LeqS        -> Region_bitvector.leqS
  | Dba.LtS         -> Region_bitvector.ltS
  | Dba.GeqS        -> Region_bitvector.geqS
  | Dba.GtS         -> Region_bitvector.gtS


let rec eval_expr expr m conds glbs: Region_bitvector.t =
  Logger.debug ~level:3 "Evaluating %a" Dba_printer.Ascii.pp_expr expr;
  let rbv =
  match expr with
  | Dba.ExprVar (v, size, _) ->
    let big_zero = Bigint.zero_big_int in
    begin
      try read (Static_types.Var (v, size)) big_zero m conds glbs
      with Not_found ->
      try read (Static_types.Var (v, size)) big_zero !m_init conds glbs
      with Not_found -> Simulate_utils.mk_undef_value size
    end

  | Dba.ExprLoad (size, endianness, e) ->
    Logger.debug "Loading ...";
    let append v1 v2 =
      match endianness with
      | Dba.BigEndian -> append v1 v2
      | Dba.LittleEndian -> append v2 v1
    in
    let vexp =
      match eval_expr e m conds glbs with
      | `SymbSmt smb ->
        Region_bitvector.get_value smb (Machine.Word_size.get ()) conds glbs
      | v -> v
    in
    let region = region_of vexp in
    let i = value_of vexp in
    let big_zero = Bigint.zero_big_int in
    let v = `Value (`Constant, bitvector_of vexp) in
    let sub_m = SubEnv.singleton big_zero v in
    let env = Env.add (Static_types.Var ("\\addr", Machine.Word_size.get ())) sub_m m in
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
        let key = Static_types.Var ("\\addr", Machine.Word_size.get ()) in
        let m = Env.add key sub_m m in
        loop (succ_big_int index) vexp (append ret (read_at index m))
      else ret
    in
    loop (succ_big_int i) vexp ret


  | Dba.ExprCst (r, bv) ->
    let region =
      if Simulate_options.SemanticsMode.flat () then `Constant else r in
    `Value (region, bv)

  | Dba.ExprUnary (uop,expr) ->
    eval_expr expr m conds glbs |> eval_unop uop

  | Dba.ExprBinary (bop, expr1, expr2) ->
    let v1 = eval_expr expr1 m conds glbs in
    let v2 = eval_expr expr2 m conds glbs in
    eval_binop bop v1 v2

  | Dba.ExprRestrict (expr, offset1, offset2) ->
    let v = eval_expr expr m conds glbs in
    restrict v offset1 offset2

  | Dba.ExprExtU (expr, size) ->
    extension (eval_expr expr m conds glbs) size

  | Dba.ExprExtS (expr, size) ->
    signed_extension (eval_expr expr m conds glbs) size

  | Dba.ExprIte (cond, expr1, expr2) ->
    begin
      try
        let e = if eval_cond cond m conds glbs then expr1 else expr2 in
        eval_expr e m conds glbs
      with
      | Smt_bitvectors.Assume_condition c ->
        begin match ConditionalStrategy.get () with
          | None -> raise (Smt_bitvectors.Assume_condition c)
          | Some ConditionalStrategy.Else ->
            let not_cond = Dba.CondNot cond in
            let conds = add_smt_cond not_cond m conds glbs in
            eval_expr expr2 m conds glbs
          | Some ConditionalStrategy.Normal ->
            let conds = add_smt_cond cond m conds glbs in
            eval_expr expr1 m conds glbs
        end
    end

  | Dba.ExprAlternative (e_list, _tag) ->
    Dba_utils.eval_alternatives (fun e -> eval_expr e m conds glbs) equal e_list
  in
  Logger.debug "RBV: %a" Region_bitvector.pp rbv; rbv

and eval_cond c m conds glbs =
  match c with
  | Dba.CondReif expr ->
    let bv = eval_expr expr m conds glbs in
    (match bv with
     | `Value (`Constant, b) when Bitvector.is_zero b -> false
     | `Value (`Constant, b) when Bitvector.is_one b -> true
     | `Value (`Constant, _) ->
       raise (Bad_condition "size of condition must be 1")
     | `Value (_, b) when Bitvector.size_of b = 1 ->
       raise (Bad_region "region of condition must be Constant")
     | `SymbSmt smb ->
       let e = Region_bitvector.get_expr smb 1 conds glbs in
       eval_cond (Dba.CondReif e) m conds glbs
     | _ -> raise (Bad_condition (Region_bitvector.to_string bv))
    )
  | Dba.CondNot b -> not (eval_cond b m conds glbs)
  | Dba.CondAnd (b1, b2) ->
    (eval_cond b1 m conds glbs) && (eval_cond b2 m conds glbs)
  | Dba.CondOr (b1, b2) ->
    (eval_cond b1 m conds glbs) || (eval_cond b2 m conds glbs)
  | Dba.True -> true
  | Dba.False -> false


and get_smt_cond c m conds glbs =
  let open Smt_bitvectors in
  let open Smtlib2 in
  match c with
  | Dba.CondReif expr ->
    let bv = eval_expr expr m conds glbs in
    begin match bv with
      | `Value (`Constant, b)
        when (Bitvector.size_of b = 1 &&
              Bigint.eq_big_int (Bitvector.value_of b) Bigint.zero_big_int) ->
        (SmtBvBinaryAlt (SmtBvComp,
                         (SmtBvCstAlt(Bitvector.one)),
                         (SmtBvCstAlt(Bitvector.zero))))
      | `Value (`Constant, b)
        when (Bitvector.size_of b = 1 &&
              Bigint.eq_big_int (Bitvector.value_of b) Bigint.unit_big_int) ->
        (SmtBvBinaryAlt (SmtBvComp,
                         (SmtBvCstAlt(Bitvector.one)),
                         (SmtBvCstAlt(Bitvector.one))))
      | `Value (`Constant, _) ->
        raise (Bad_condition ("size of condition must be 1"))
      | `Value (_, b) when Bitvector.size_of b = 1 ->
        raise (Bad_region "region of condition must be Constant")
      | `SymbSmt smb -> smb
      | `Undef sz -> Smt_bitvectors.gen_undef sz
      | _ -> raise (Bad_condition (Region_bitvector.to_string bv))
    end
  | Dba.CondNot cond ->
    let smt_cond = get_smt_cond cond m conds glbs in
    (SmtBvUnaryAlt (SmtBvNot, smt_cond))

  | Dba.CondAnd (cond1, cond2) ->
    let smt_cond1 = get_smt_cond cond1 m conds glbs in
    let smt_cond2 = get_smt_cond cond2 m conds glbs in
    (SmtBvBinaryAlt (SmtBvAnd, smt_cond1, smt_cond2))

  | Dba.CondOr (cond1, cond2) ->
    let smt_cond1 = get_smt_cond cond1 m conds glbs in
    let smt_cond2 = get_smt_cond cond2 m conds glbs in
    (SmtBvBinaryAlt (SmtBvOr, smt_cond1, smt_cond2))

  | Dba.True ->
    (SmtBvBinaryAlt (SmtBvComp,
                     (SmtBvCstAlt(Bitvector.one)),
                     (SmtBvCstAlt(Bitvector.one))))
  | Dba.False ->
    (SmtBvBinaryAlt (SmtBvComp,
                     (SmtBvCstAlt(Bitvector.one)),
                     (SmtBvCstAlt(Bitvector.zero))))

and add_smt_cond c m conds glbs =
   get_smt_cond c m conds glbs :: conds



(* Checking read of memory permissions here *)
and read x i m conds glbs =
  match x with
  | Static_types.Var _ ->
    SubEnv.find Bigint.zero_big_int (Env.find x m)
  | Static_types.Array r ->
    let c =
      try Dba_types.Rights.find (Dba_types.Rights.R, r) !permis with Not_found -> Dba.True in
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
      try Dba_types.Rights.find (Dba_types.Rights.W, r) !permis with Not_found -> Dba.True in
    if eval_cond c m conds glbs then
      let sub_m = try Env.find x m with Not_found -> SubEnv.empty in
      let sub_m = SubEnv.add i rbv sub_m in
      Env.add x sub_m m
    else
      raise Errors.Write_permission_denied
