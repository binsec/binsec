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

open Dba
open Errors
open Format
open Dba_printer.Ascii

module Expr = struct
  exception Fold_const_failed

let rec fold_expr e =
  let open Bitvector in
  match e with
  | ExprVar(_name,_sz,_) -> raise Fold_const_failed
  | ExprLoad(_sz,_endian,_e) -> raise Fold_const_failed
  | ExprCst(`Constant, v) -> v
  | ExprUnary(uop, e1) ->
    begin match uop with
      | Dba.UMinus -> neg (fold_expr e1)
      | Dba.Not -> lognot (fold_expr e1)
    end
  | ExprBinary(bop, e1, e2) ->
    let apply f a b = f a b |> of_bool in
    let int_snd f a b = f a (Bigint.int_of_big_int (Bitvector.value_of b)) in
    let f = match bop with
      | Plus -> add | Minus -> sub | MultU -> umul
      | MultS -> smul | DivU -> udiv | DivS -> sdiv
      | ModU -> srem | ModS -> smod
      | Or -> logor | And -> logand | Xor -> logxor
      | Concat -> append
      | LShift  -> int_snd shift_left
      | RShiftU -> int_snd shift_right
      | RShiftS -> int_snd shift_right_signed
      | LeftRotate  -> int_snd rotate_left
      | RightRotate -> int_snd rotate_right
      | Eq -> apply equal | Diff -> apply diff
      | LeqU -> apply ule | LtU -> apply ult
      | GeqU -> apply uge | GtU -> apply ugt
      | LeqS -> apply sle | LtS -> apply slt
      | GeqS -> apply sge | GtS -> apply sgt
    in
    f (fold_expr e1) (fold_expr e2)
  | ExprRestrict(e1, l, h) ->
    extract (fold_expr e1) l h
  | ExprExtU(e1, sz) ->
    Bitvector.extend (fold_expr e1) sz
  | ExprExtS(e1, sz) ->
    Bitvector.extend_signed (fold_expr e1) sz
  | ExprIte(_c, _e1, _e2) ->
    (* TODO: Implement for cond *)
    raise Fold_const_failed
  | _ -> raise Fold_const_failed

end
let eval_alternatives eval_expr eq alternatives =
  match alternatives with
  | [] -> failwith "eval_alternatives"
  | [e] -> eval_expr e
  | e :: es ->
    let v = eval_expr e in
    if List.map eval_expr es |> List.for_all (eq v) then v
    else raise Errors.Alternative_conflict_values


(* *****  check size (basic typing) in DBAs   *)
let rec computesize_dbaexpr e: int =  (* size: bits *)
  match e with
  | ExprVar(_, sz, _) ->
    if sz > 0 then sz else begin
      Logger.fatal "Negatively sized expression %a" pp_expr e;
      raise Bad_exp_size
    end
  | ExprLoad (size_byte, _, bexpr) as e -> (* read by bytes *)
    let sz = computesize_dbaexpr bexpr in
    if 0 < size_byte then
      (if sz = Machine.Word_size.get () then size_byte * 8
       else raise Bad_exp_size)
    else raise (Bad_bound (asprintf "%a" pp_expr e))
  | ExprCst(_, cval) ->
    let sz = Bitvector.size_of cval in
    let v = Bitvector.value_of cval in
    let sign = Bigint.sign_big_int v in
    let max = Bigint.power_int_positive_int 2 sz in
    let lt_max = Bigint.lt_big_int v max in
    if sz > 0 && (sign = 1 || sign = 0) && lt_max
    then sz
    else raise Bad_exp_size
  | ExprUnary (_, bexpr) -> computesize_dbaexpr bexpr

  | ExprBinary(bop,bexpr1,bexpr2) -> (
      match bop with
      | Plus | Minus | MultU | MultS | DivU
      | DivS | ModU | ModS | Or | And | Xor ->
        let (sz1, sz2) =
          (computesize_dbaexpr bexpr1, computesize_dbaexpr bexpr2) in
        if sz1 = sz2 then sz1
        else
          raise
            (Size_error(asprintf "%a, sizes %d != %d" pp_expr e sz1 sz2))
      | LShift | RShiftU | RShiftS | LeftRotate
      | RightRotate -> computesize_dbaexpr bexpr1
      | Concat ->
        let sz1 = computesize_dbaexpr bexpr1 in
        let sz2 = computesize_dbaexpr bexpr2 in
        sz1 + sz2
      | Eq| Diff | LeqU | LtU | GeqU | GtU
      | LeqS | LtS | GeqS | GtS ->
        let sz1 = computesize_dbaexpr bexpr1 in
        let sz2 = computesize_dbaexpr bexpr2 in
        if (sz1 = sz2) then 1 (* reifie mais check coherence*)
        else
          raise (Size_error(Format.asprintf "%a, sizes %d != %d"
                              pp_expr e sz1 sz2))
    )

  | ExprRestrict(bexpr,i,j) as e ->
    let sz = (computesize_dbaexpr bexpr) in
    if ((i<=j) && (0<=i) && (j<sz)) then j-i+1
    else raise (Bad_bound (asprintf "%a" pp_expr e))

  | ExprExtU(bexpr,n) as e ->
    let n' = computesize_dbaexpr bexpr in
    if (n' <= n) then n
    else raise (Bad_bound (asprintf "%a" pp_expr e))
  | ExprExtS(bexpr,n) as e ->
    let n' = computesize_dbaexpr bexpr in
    if (n' <= n) then n
    else raise (Bad_bound (asprintf "%a" pp_expr e))
  | ExprIte(bcond,be1,be2) ->
    let sz1 = computesize_dbaexpr be1 in
    let sz2 = computesize_dbaexpr be2 in
    if ((checksize_dbacond(bcond)) && (sz1=sz2)) then sz1
    else raise (Size_error("Ite branches size different"))
  | ExprAlternative (e_list, _) ->
    eval_alternatives computesize_dbaexpr (=) e_list

and checksize_dbacond (bcond : cond) : bool =
  match bcond with
    CondReif(bexp) -> let sz = computesize_dbaexpr(bexp) in
    if (sz=1) then true
    else raise (Size_error("Reification cond should be of size=1"))
  | CondNot(bc) -> checksize_dbacond(bc)
  | CondAnd(bc1,bc2) ->
    (checksize_dbacond bc1) && (checksize_dbacond bc2)
  | CondOr(bc1,bc2) ->
    (checksize_dbacond bc1) && (checksize_dbacond bc2)
  | True -> true
  | False -> true

let is_positive = Bigint.le_big_int Bigint.zero_big_int

let checksize_address addr =
  (* Not sure this function is needed anymore. Smart constructors should take
   * care of that *)
  let open Dba_types.Caddress in
  let sz = Bitvector.size_of addr.base in
  let cval = base_value addr in
  sz = Machine.Word_size.get ()
  && addr.id >= 0
  && is_positive cval
  && Bigint.lt_big_int cval (Bigint.power_int_positive_int 2 sz)

let computesize_dbalhs blhs : int  =
  (* taille en bits *) (* plante si erreur *)
  match  blhs with
  | LhsVar(_, size, _) -> size
  | LhsVarRestrict(_name,size,i,j) ->
    if ((i<=j) || (0<=i) || (j<size)) then j-i+1
    else raise (Bad_bound (Format.asprintf "{%d,%d}" i j))
  | LhsStore(size_byte,_endian,bexpr) ->
    if (0 < size_byte) then
      let sz = (computesize_dbaexpr bexpr) in
      if (sz = Machine.Word_size.get ()) then size_byte * 8
      else ((raise Bad_exp_size))
    else
      let msg = Format.asprintf "@[%a, _, %d]" pp_expr bexpr size_byte in
      raise (Bad_bound msg)

let checksize_instruction (binkd : instruction) : bool =
  match binkd with
  | IkAssign(blhs, bexpr, _addr) ->
    let sz1 = computesize_dbalhs blhs
    and sz2 = computesize_dbaexpr bexpr in
    sz1 = sz2
  | IkSJump(JOuter addr,_) -> checksize_address addr
  | IkSJump(JInner _, _) -> true

  | IkDJump(expr,_) ->
    let sz = computesize_dbaexpr(expr) in
    sz = Machine.Word_size.get ()
  | IkIf(bcond, JOuter addr1, _id2) ->
    if (checksize_address addr1)
    then checksize_dbacond(bcond)
    else raise Bad_address_size
  | IkIf(bcond, JInner _id1, _id2) -> checksize_dbacond(bcond)

  | IkStop _ -> true
  | IkPrint(l, _addr) ->
    let f = function Exp x -> ignore (computesize_dbaexpr x)
                   | _ -> ()
    in
    List.iter f l; true
  | IkNondetAssume (lhs_list, bcond, _addr) ->
    let f x = ignore (computesize_dbalhs x) in
    List.iter f lhs_list;
    checksize_dbacond bcond
  | IkNondet (blhs, _, _addr) ->
    let _sz = (computesize_dbalhs blhs) in true
  | IkAssume (bcond, _addr) ->
    checksize_dbacond(bcond)
  | IkAssert (bcond, _addr) ->
    checksize_dbacond(bcond)
  | IkMalloc (lhs, _bexpr, _addr) ->
    let sz = (computesize_dbalhs lhs) in
    (sz = Machine.Word_size.get ())
  | IkFree (bexpr, _addr) ->
    let _sz = (computesize_dbaexpr bexpr) in true
  | IkUndef (blhs, _addr) ->
    let _sz = (computesize_dbalhs blhs) in true


let contains_lhs lhs1 lhs2 =
  let string_of_dbalhs lhs =
    Format.asprintf "%a" Dba_printer.Ascii.pp_lhs lhs in
  let s1 = string_of_dbalhs lhs1 in
  let s2 = string_of_dbalhs lhs2 in
  let re = Str.regexp_string s2 in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false


let globalize_address root_caddress = function
  | JInner offset -> Dba_types.Caddress.reid root_caddress offset
  | JOuter caddress -> caddress


let rec substitute_dba_expr (m_e:expr) (r_e:expr) (e:expr): expr =
  (* Substitue matching_e(m_e) by replacement_e(r_e) in e *)
  if e = m_e then
    r_e
  else
    match e with
    | Dba.ExprLoad (sz,en,expr) -> Dba.ExprLoad(sz, en, substitute_dba_expr m_e r_e expr)
    | Dba.ExprUnary (uop, expr) -> Dba.ExprUnary(uop, substitute_dba_expr m_e r_e expr)
    | Dba.ExprBinary(bop,expr1,expr2) ->
      Dba.ExprBinary(bop, substitute_dba_expr m_e r_e expr1, substitute_dba_expr m_e r_e expr2)
    | Dba.ExprRestrict(expr,i,j) -> Dba.ExprRestrict(substitute_dba_expr m_e r_e expr, i, j)
    | Dba.ExprExtU(expr,n) -> Dba.ExprExtU(substitute_dba_expr m_e r_e expr, n)
    | Dba.ExprExtS(expr,n) -> Dba.ExprExtS(substitute_dba_expr m_e r_e expr, n)
    | Dba.ExprIte(cond,e1,e2) ->
      Dba.ExprIte(substitute_dba_expr_cond m_e r_e cond, substitute_dba_expr m_e r_e e1, substitute_dba_expr m_e r_e e2)
    | Dba.ExprAlternative (e_list, tag_option) ->
      Dba.ExprAlternative (List.map (fun i -> substitute_dba_expr m_e r_e i) e_list, tag_option)
    | _ -> e

and substitute_dba_expr_cond (m_e:expr) (r_e:expr) (c:cond) : cond =
  match c with
  | Dba.CondReif(exp) -> Dba.CondReif(substitute_dba_expr m_e r_e exp)
  | Dba.CondNot(c1) -> Dba.CondNot(substitute_dba_expr_cond m_e r_e c1)
  | Dba.CondAnd(c1, c2) ->
    Dba.CondAnd(substitute_dba_expr_cond m_e r_e c1, substitute_dba_expr_cond m_e r_e c2)
  | Dba.CondOr(c1,c2) ->
    Dba.CondOr(substitute_dba_expr_cond m_e r_e c1, substitute_dba_expr_cond m_e r_e c2)
  | Dba.True -> c
  | Dba.False -> c
