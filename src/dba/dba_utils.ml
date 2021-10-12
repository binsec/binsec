(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2021                                               *)
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
    | Expr.Var _ | Expr.Load _ -> raise Fold_const_failed
    | Expr.Cst v -> v
    | Expr.Unary (uop, e) ->
        let e' = fold_expr e in
        let f =
          match uop with
          | Dba.Unary_op.UMinus -> neg
          | Dba.Unary_op.Not -> lognot
          | Dba.Unary_op.Uext sz -> fun e -> Bitvector.extend e sz
          | Dba.Unary_op.Sext sz -> fun e -> Bitvector.extend_signed e sz
          | Dba.Unary_op.Restrict i -> fun e -> extract e i
        in
        f e'
    | Expr.Binary (bop, e1, e2) ->
        let open Binary_op in
        let apply f a b = f a b |> of_bool in
        let int_snd f a b = f a (Z.to_int (Bitvector.value_of b)) in
        let f =
          match bop with
          | Plus -> add
          | Minus -> sub
          | Mult -> mul
          | DivU -> udiv
          | DivS -> sdiv
          | ModU -> srem
          | ModS -> smod
          | Or -> logor
          | And -> logand
          | Xor -> logxor
          | Concat -> append
          | LShift -> int_snd shift_left
          | RShiftU -> int_snd shift_right
          | RShiftS -> int_snd shift_right_signed
          | LeftRotate -> int_snd rotate_left
          | RightRotate -> int_snd rotate_right
          | Eq -> apply equal
          | Diff -> apply diff
          | LeqU -> apply ule
          | LtU -> apply ult
          | GeqU -> apply uge
          | GtU -> apply ugt
          | LeqS -> apply sle
          | LtS -> apply slt
          | GeqS -> apply sge
          | GtS -> apply sgt
        in
        f (fold_expr e1) (fold_expr e2)
    | Expr.Ite (_c, _e1, _e2) ->
        (* TODO: Implement for cond *)
        raise Fold_const_failed

  let of_vaddr (v : Virtual_address.t) =
    Expr.constant
      (Bitvector.of_int ~size:(Kernel_options.Machine.word_size ()) (v :> int))

  let eval_from_img =
    let get_attr img attr name =
      match attr with
      | Dba.VarTag.Value ->
          Option.get
          @@ Loader_utils.address_of_symbol_or_section_by_name ~name img
      | Dba.VarTag.Size ->
          Option.get @@ Loader_utils.size_of_symbol_or_section_by_name ~name img
      | Dba.VarTag.Last ->
          Int.pred @@ Virtual_address.to_int @@ snd @@ Option.get
          @@ Loader_utils.interval_of_symbol_or_section_by_name ~name img
    in
    let rec fold img = function
      | Dba.Expr.Cst _ as e -> e
      | Dba.Expr.Var { name; size; info = Dba.VarTag.Symbol attr } ->
          Dba.Expr.constant @@ Bitvector.of_int ~size @@ get_attr img attr name
      | Dba.Expr.Var _ -> assert false (* TODO *)
      | Dba.Expr.Load _ -> assert false (* TODO *)
      | Dba.Expr.Unary (op, e) -> Dba.Expr.unary op (fold img e)
      | Dba.Expr.Binary (op, a, b) ->
          Dba.Expr.binary op (fold img a) (fold img b)
      | Dba.Expr.Ite (c, t, e) ->
          Dba.Expr.ite (fold img c) (fold img t) (fold img e)
    in
    fun img e ->
      match fold img e with
      | Dba.Expr.Cst bv -> Virtual_address.of_bitvector bv
      | _ -> assert false
  (* TODO *)

  let complement e ~lo ~hi var =
    let size = var.size and evar = Dba.Expr.v var in
    if lo = 0 then
      Dba.Expr.append (Dba.Expr.restrict (hi + 1) (size - 1) evar) e
    else if hi = size - 1 then
      Dba.Expr.append e (Dba.Expr.restrict 0 (lo - 1) evar)
    else
      Dba.Expr.append
        (Dba.Expr.append (Dba.Expr.restrict (hi + 1) (size - 1) evar) e)
        (Dba.Expr.restrict 0 (lo - 1) evar)
end

let eval_alternatives eval_expr eq alternatives =
  match alternatives with
  | [] -> failwith "eval_alternatives"
  | [ e ] -> eval_expr e
  | e :: es ->
      let v = eval_expr e in
      if List.map eval_expr es |> List.for_all (eq v) then v
      else raise Errors.Alternative_conflict_values

module Logger = Dba_types.Logger

(* *****  check size (basic typing) in DBAs   *)
let rec computesize_dbaexpr e : int =
  (* size: bits *)
  let open! Dba in
  match e with
  | Expr.Var v ->
      if v.size > 0 then v.size
      else
        Logger.fatal ~e:Bad_exp_size "Negatively sized expression %a" pp_bl_term
          e
  | Expr.Load (size_byte, _, bexpr) as e ->
      (* read by bytes *)
      let sz = computesize_dbaexpr bexpr in
      if 0 < size_byte then
        if sz = Kernel_options.Machine.word_size () then size_byte * 8
        else raise Bad_exp_size
      else raise (Bad_bound (asprintf "%a" pp_bl_term e))
  | Expr.Cst cval ->
      let sz = Bitvector.size_of cval in
      let v = Bitvector.value_of cval in
      let sign = Z.sign v in
      let max = Z.pow (Z.of_int 2) sz in
      let lt_max = Z.lt v max in
      if sz > 0 && (sign = 1 || sign = 0) && lt_max then sz
      else raise Bad_exp_size
  | Expr.Unary (uop, bexpr) as e -> (
      let n' = computesize_dbaexpr bexpr in
      match uop with
      | Unary_op.Uext n | Unary_op.Sext n ->
          if n' <= n then n else raise (Bad_bound (asprintf "%a" pp_bl_term e))
      | Unary_op.UMinus | Unary_op.Not -> n'
      | Unary_op.Restrict { Interval.lo; Interval.hi } ->
          if hi <= n' then hi - lo + 1
          else raise (Bad_bound (asprintf "%a" pp_bl_term e)))
  | Expr.Binary (bop, bexpr1, bexpr2) -> (
      let open Binary_op in
      match bop with
      | Plus | Minus | Mult | DivU | DivS | ModU | ModS | Or | And | Xor ->
          let sz1, sz2 =
            (computesize_dbaexpr bexpr1, computesize_dbaexpr bexpr2)
          in
          if sz1 = sz2 then sz1
          else
            raise
              (Size_error (asprintf "%a, sizes %d != %d" pp_bl_term e sz1 sz2))
      | LShift | RShiftU | RShiftS | LeftRotate | RightRotate ->
          computesize_dbaexpr bexpr1
      | Concat ->
          let sz1 = computesize_dbaexpr bexpr1 in
          let sz2 = computesize_dbaexpr bexpr2 in
          sz1 + sz2
      | Eq | Diff | LeqU | LtU | GeqU | GtU | LeqS | LtS | GeqS | GtS ->
          let sz1 = computesize_dbaexpr bexpr1 in
          let sz2 = computesize_dbaexpr bexpr2 in
          if sz1 = sz2 then 1 (* reifie mais check coherence*)
          else
            let msg =
              Format.asprintf "%a, sizes %d != %d" pp_bl_term e sz1 sz2
            in
            raise (Size_error msg))
  | Expr.Ite (bcond, be1, be2) ->
      let sz1 = computesize_dbaexpr be1 in
      let sz2 = computesize_dbaexpr be2 in
      let c_size = computesize_dbaexpr bcond in
      if c_size = 1 && sz1 = sz2 then sz1
      else raise (Size_error "Ite branches size different")

and checksize_dbacond e =
  let sz = computesize_dbaexpr e in
  sz = 1 || failwith "Expressions used in conditional must be of size 1"

let is_positive = Z.leq Z.zero

let checksize_address addr =
  (* Not sure this function is needed anymore. Smart constructors should take
   * care of that *)
  let open Dba_types.Caddress in
  let cval = Virtual_address.to_bigint (base_value addr) in
  addr.id >= 0 && is_positive cval

let computesize_dbalhs blhs : int =
  (* taille en bits *)
  (* plante si erreur *)
  match blhs with
  | LValue.Var v -> v.size
  | LValue.Restrict ({ Dba.size; _ }, { Interval.lo = i; Interval.hi = j }) ->
      if i <= j || 0 <= i || j < size then j - i + 1
      else raise (Bad_bound (Format.asprintf "{%d,%d}" i j))
  | LValue.Store (size_byte, _endian, bexpr) ->
      if 0 < size_byte then
        let sz = computesize_dbaexpr bexpr in
        if sz = Kernel_options.Machine.word_size () then size_byte * 8
        else raise Bad_exp_size
      else
        let msg = Format.asprintf "@[%a, _, %d]" pp_bl_term bexpr size_byte in
        raise (Bad_bound msg)

let valid_condition e = computesize_dbaexpr e = 1 || assert false

let checksize_instruction binkd =
  let open Instr in
  match binkd with
  | Assign (blhs, bexpr, _addr) ->
      let sz1 = computesize_dbalhs blhs and sz2 = computesize_dbaexpr bexpr in
      sz1 = sz2
  | SJump (JOuter addr, _) -> checksize_address addr
  | SJump (JInner _, _) -> true
  | DJump (expr, _) ->
      let sz = computesize_dbaexpr expr in
      sz = Kernel_options.Machine.word_size ()
  | If (bcond, JOuter addr1, _id2) ->
      if checksize_address addr1 then valid_condition bcond
      else raise Bad_address_size
  | If (bcond, JInner _id1, _id2) -> valid_condition bcond
  | Stop _ -> true
  | Print (l, _addr) ->
      let f = function Exp x -> ignore (computesize_dbaexpr x) | _ -> () in
      List.iter f l;
      true
  | NondetAssume (lhs_list, bcond, _addr) ->
      let f x = ignore (computesize_dbalhs x) in
      List.iter f lhs_list;
      valid_condition bcond
  | Nondet (blhs, _, _addr) ->
      let _sz = computesize_dbalhs blhs in
      true
  | Assume (bcond, _addr) -> valid_condition bcond
  | Assert (bcond, _addr) -> valid_condition bcond
  | Malloc (lhs, _bexpr, _addr) ->
      let sz = computesize_dbalhs lhs in
      sz = Kernel_options.Machine.word_size ()
  | Free (bexpr, _addr) ->
      let _sz = computesize_dbaexpr bexpr in
      true
  | Undef (blhs, _addr) ->
      let _sz = computesize_dbalhs blhs in
      true

let contains_lhs lhs1 lhs2 =
  let string_of_dbalhs lhs =
    Format.asprintf "%a" Dba_printer.Ascii.pp_lhs lhs
  in
  let s1 = string_of_dbalhs lhs1 in
  let s2 = string_of_dbalhs lhs2 in
  let re = Str.regexp_string s2 in
  try
    ignore (Str.search_forward re s1 0);
    true
  with Not_found -> false

let globalize_address root_caddress = function
  | JInner offset -> Dba_types.Caddress.reid root_caddress offset
  | JOuter caddress -> caddress

let substitute_dba_expr m_e r_e e =
  (* Substitue matching_e(m_e) by replacement_e(r_e) in e *)
  let open! Dba in
  let rec aux e =
    if e = m_e then r_e
    else
      match e with
      | Expr.Load (sz, en, expr) ->
          let bysz = Size.Byte.create sz in
          Expr.load bysz en (aux expr)
      | Expr.Unary (uop, expr) -> Expr.unary uop (aux expr)
      | Expr.Binary (bop, expr1, expr2) ->
          Expr.binary bop (aux expr1) (aux expr2)
      | Expr.Ite (cond, e1, e2) -> Expr.ite (aux cond) (aux e1) (aux e2)
      | _ -> e
  in
  aux e
