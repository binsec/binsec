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

exception NotSupportedForExport of string
exception NotSupportedForImport of string

let the = Utils.unsafe_get_opt

let generate_bitvector (bv:Bitvector.t): Dba_piqi.bitvector =
  { Dba_piqi.Bitvector.bv = Bigint.int64_of_big_int (Bitvector.value_of bv);
    Dba_piqi.Bitvector.size = Int32.of_int (Bitvector.size_of bv); }

let parse_bitvector (bv:Dba_piqi.bitvector): Bitvector.t =
  let open Dba_piqi.Bitvector in
  Bitvector.create (Bigint.big_int_of_int64 bv.bv) (Int32.to_int bv.size)

let generate_dbacodeaddress (addr:Dba.address): Dba_piqi.dbacodeaddress =
  let bv =  Virtual_address.to_int64 addr.Dba.base in
  { Dba_piqi.Dbacodeaddress.bitvector = bv;
    Dba_piqi.Dbacodeaddress.dbaoffset = Int32.of_int addr.Dba.id;}

let parse_dbacodeaddress addr : Dba.address =
  let open Dba_piqi.Dbacodeaddress in
  let bv = Virtual_address.of_int64 addr.bitvector
  and id = Int32.to_int addr.dbaoffset in
  Dba_types.Caddress.create bv id


let generate_codeaddress addr =
  let open Dba_piqi.Codeaddress in
  match addr with
  | Dba.JInner off ->
    { typeid = `local; offset = Some (Int32.of_int off); address = None}
  | Dba.JOuter addr ->
    { typeid = `non_local;
      offset = None;
      address = Some (generate_dbacodeaddress addr)}

let parse_codeaddress addr =
  let open Dba_piqi.Codeaddress in
  match addr.typeid with
  | `local ->
    Dba.JInner(Int32.to_int (the addr.offset))
  | `non_local ->
    Dba.JOuter(parse_dbacodeaddress (the addr.address))

let generate_tag = function
  | Dba.Return ->
    Dba_piqi.Dbatag.(
      {typeid = `dba_return; address = None})
  | Dba.Call addr ->
    Dba_piqi.Dbatag.({typeid = `dba_call;
                      address = Some (generate_dbacodeaddress addr)})

let parse_tag_option = function
  | None -> None
  | Some t ->
    let open Dba_piqi.Dbatag in
    let v =
      match t.typeid with
      | `dba_call -> Dba.Call (parse_dbacodeaddress (the t.address))
      | `dba_return -> Dba.Return in
    Some v

let generate_dbastate (state:Dba.state option) =
  let open Dba_piqi.Dbastopstate in
  match state with
  | Some Dba.OK -> Some {typeid = `ok; infos = None}
  | Some Dba.KO -> Some {typeid = `ko; infos = None}
  | Some (Dba.Undefined s) -> Some {typeid = `undefined; infos = Some s}
  | Some (Dba.Unsupported s) -> Some {typeid = `unsupported; infos = Some s}
  | None -> None

let parse_dbastate stateopt: Dba.state option =
  match stateopt with
  | None -> None
  | Some st ->
    let open Dba_piqi.Dbastopstate in
    match st.typeid with
    | `ok -> Some Dba.OK
    | `ko -> Some Dba.KO
    | `undefined -> Some (Dba.Undefined (the st.infos))
    | `unsupported -> Some (Dba.Unsupported (the st.infos))

let binaryop_to_piqi (op:Dba.Binary_op.t) =
  match op with
  | Dba.Binary_op.Plus -> `dba_plus | Dba.Binary_op.Minus ->  `dba_minus
  | Dba.Binary_op.Mult -> `dba_mult_u
  (* | Dba.Power -> `dba_power  *)
  | Dba.Binary_op.DivU -> `dba_div_u | Dba.Binary_op.DivS -> `dba_div_s | Dba.Binary_op.ModU -> `dba_mod_u
  | Dba.Binary_op.ModS -> `dba_mod_s | Dba.Binary_op.Or -> `dba_or
  | Dba.Binary_op.And -> `dba_and | Dba.Binary_op.Xor -> `dba_xor
  | Dba.Binary_op.Concat -> `dba_concat
  | Dba.Binary_op.LShift -> `dba_lshift_u | Dba.Binary_op.RShiftU -> `dba_rshift_u
  | Dba.Binary_op.RShiftS -> `dba_rshift_s
  | Dba.Binary_op.LeftRotate -> `dba_left_rotate | Dba.Binary_op.RightRotate -> `dba_right_rotate
  | Dba.Binary_op.Eq -> `dba_eq | Dba.Binary_op.Diff -> `dba_diff
  | Dba.Binary_op.LeqU -> `dba_leq_u | Dba.Binary_op.LtU  -> `dba_lt_u
  | Dba.Binary_op.GeqU -> `dba_geq_u | Dba.Binary_op.GtU -> `dba_gt_u
  | Dba.Binary_op.LeqS -> `dba_leq_s | Dba.Binary_op.LtS -> `dba_lt_s
  | Dba.Binary_op.GeqS -> `dba_geq_s | Dba.Binary_op.GtS -> `dba_gt_s

let piqi_to_binaryop = function
  | `dba_plus -> Dba.Binary_op.Plus  | `dba_minus -> Dba.Binary_op.Minus
  | `dba_mult_u -> Dba.Binary_op.Mult | `dba_mult_s -> Dba.Binary_op.Mult
  | `dba_div_u -> Dba.Binary_op.DivU | `dba_div_s -> Dba.Binary_op.DivS | `dba_mod_u -> Dba.Binary_op.ModU
  | `dba_mod_s -> Dba.Binary_op.ModS | `dba_or -> Dba.Binary_op.Or
  | `dba_and -> Dba.Binary_op.And | `dba_xor -> Dba.Binary_op.Xor
  | `dba_concat -> Dba.Binary_op.Concat
  | `dba_lshift_u -> Dba.Binary_op.LShift | `dba_rshift_u -> Dba.Binary_op.RShiftU
  | `dba_rshift_s -> Dba.Binary_op.RShiftS
  | `dba_left_rotate -> Dba.Binary_op.LeftRotate | `dba_right_rotate -> Dba.Binary_op.RightRotate
  | `dba_eq -> Dba.Binary_op.Eq | `dba_diff -> Dba.Binary_op.Diff
  | `dba_leq_u -> Dba.Binary_op.LeqU | `dba_lt_u -> Dba.Binary_op.LtU
  | `dba_geq_u -> Dba.Binary_op.GeqU | `dba_gt_u -> Dba.Binary_op.GtU
  | `dba_leq_s -> Dba.Binary_op.LeqS | `dba_lt_s -> Dba.Binary_op.LtS
  | `dba_geq_s -> Dba.Binary_op.GeqS | `dba_gt_s -> Dba.Binary_op.GtS

let parse_endian = function
  | `little -> Machine.LittleEndian
  | `big -> Machine.BigEndian

let rec generate_dbaexpr (e:Dba.Expr.t)  =
  let open Dba_piqi.Dbaexpr in
  let expr = Dba_piqi.default_dbaexpr () in
  match e with
  | Dba.Expr.Var {Dba.name = s; Dba.size = sz; _} ->
    {expr with typeid = `dba_expr_var;
               name = Some s;
               size = Some (Int32.of_int sz)}
  | Dba.Expr.Load(sz, endian, e1) ->
    let endian =
      match endian with Machine.LittleEndian -> `little | Machine.BigEndian -> `big in
    {expr with typeid = `dba_load;
               size = Some (Int32.of_int sz);
               endian = Some endian;
               expr1 = Some (generate_dbaexpr e1)}
  | Dba.Expr.Cst(`Constant, bv) ->
    { expr with typeid = `dba_expr_cst;
                bitvector = Some (generate_bitvector bv) }
  | Dba.Expr.Unary(Dba.Unary_op.Uext sz, e1) ->
    {expr with typeid = `dba_expr_ext_u;
               expr1 = Some (generate_dbaexpr e1);
               size = Some (Int32.of_int sz)}
  | Dba.Expr.Unary(Dba.Unary_op.Restrict {Interval.lo; Interval.hi;}, e1) ->
    { expr with typeid = `dba_expr_restrict;
                expr1 = Some (generate_dbaexpr e1);
                low = Some (Int32.of_int lo);
                high = Some (Int32.of_int hi)}

  | Dba.Expr.Unary(Dba.Unary_op.Sext sz, e1) ->
    { expr with typeid = `dba_expr_ext_s;
                expr1 = Some (generate_dbaexpr e1);
                size = Some (Int32.of_int sz)}

  | Dba.Expr.Unary(uop, e1) ->
    let uop =
      match uop with
      | Dba.Unary_op.UMinus -> `dba_unary_minus
      | Dba.Unary_op.Not -> `dba_unary_not
      | _ -> assert false
    in
    {expr with typeid = `dba_expr_unary;
               unaryop = Some uop;
               expr1 = Some (generate_dbaexpr e1)}
  | Dba.Expr.Binary(op, e1, e2) ->
    { expr with typeid = `dba_expr_binary;
                binaryop = Some (binaryop_to_piqi op);
                expr1 = Some (generate_dbaexpr e1);
                expr2 = Some (generate_dbaexpr e2)}
  | Dba.Expr.Ite(c1,e1,e2) ->
    { expr with typeid = `dba_expr_ite;
                cond = Some (generate_dbacond c1);
                expr1 = Some (generate_dbaexpr e1);
                expr2 = Some (generate_dbaexpr e2)}
  | _ -> assert false


and generate_dbacond c  =
  let open Dba_piqi.Dbacond in
  let cond  =  Dba_piqi.default_dbacond () in
  { cond with typeid = `dba_cond_reif; expr = Some (generate_dbaexpr c)}


let rec parse_dbaexpr e =
  let open Dba_piqi.Dbaexpr in
  let open Dba.Expr in
  match e.typeid with
  | `dba_expr_var -> Dba.Expr.var (the e.name) (Int32.to_int (the e.size))
  | `dba_load ->
    load
      (Int32.to_int  (the e.size) |> Size.Byte.create)
      (parse_endian  (the e.endian))
      (parse_dbaexpr (the e.expr1))
  | `dba_expr_cst ->
    constant (parse_bitvector (the e.bitvector))
  | `dba_expr_unary ->
    let uop =
      match the e.unaryop with
      | `dba_unary_minus -> uminus
      | `dba_unary_not -> lognot in
    uop (parse_dbaexpr (the e.expr1))
  | `dba_expr_binary ->
    binary (piqi_to_binaryop (the e.binaryop))
      (parse_dbaexpr (the e.expr1))
      (parse_dbaexpr (the e.expr2))
  | `dba_expr_restrict ->
    restrict
      (Int32.to_int (the e.low))
      (Int32.to_int (the e.high))
      (parse_dbaexpr (the e.expr1))
  | `dba_expr_ext_u ->
    uext (Int32.to_int (the e.size)) (parse_dbaexpr (the e.expr1))
  | `dba_expr_ext_s ->
    sext (Int32.to_int (the e.size)) (parse_dbaexpr (the e.expr1))
  | `dba_expr_ite ->
    ite
      (parse_cond (the e.cond))
      (parse_dbaexpr (the e.expr1))
      (parse_dbaexpr (the e.expr2))
  | `dba_expr_alternative -> raise (NotSupportedForImport "alternative")

and parse_cond c =
  let open Dba_piqi.Dbacond in
  match c.typeid with
  | `dba_cond_reif -> parse_dbaexpr (the c.expr)
  | `dba_cond_not -> Dba.Expr.lognot (parse_cond (the c.cond1))
  | `dba_cond_and ->
    Dba.Expr.logand
      (parse_cond (the c.cond1)) (parse_cond (the c.cond2))
  | `dba_cond_or ->
    Dba.Expr.logor
      (parse_cond (the c.cond1)) (parse_cond (the c.cond2))
  | `dba_true  -> Dba.Expr._true
  | `dba_false -> Dba.Expr._false

let generate_lhs (lhs:Dba.LValue.t) =
  let open Dba_piqi.Dba_lhs in
  let piq_lhs = Dba_piqi.default_dba_lhs () in
  match lhs with
  | Dba.LValue.Var {Dba.name = s; Dba.size = sz; _} ->
    {piq_lhs with typeid=`dba_lhs_var; name=Some s; size=Some (Int32.of_int sz)}
  | Dba.LValue.Restrict(v, {Interval.lo=low; Interval.hi=high}) ->
    {piq_lhs with typeid = `dba_lhs_var_restrict;
                  name = Some v.Dba.name;
                  size = Some (Int32.of_int v.Dba.size);
                  low = Some (Int32.of_int low);
                  high = Some (Int32.of_int high)}
  | Dba.LValue.Store(sz,endian,e) ->
    let indien =
      match endian with Machine.LittleEndian -> `little | Machine.BigEndian -> `big in
    {piq_lhs with typeid = `dba_store;
                  size = Some (Int32.of_int sz);
                  endian = Some indien;
                  expr = Some (generate_dbaexpr e)}

let parse_lhs lhs =
  let open Dba_piqi.Dba_lhs in
  let open Dba.LValue in
  match lhs.typeid with
  | `dba_lhs_var ->
    let bitsize = Size.Bit.of_int32 (the lhs.size) in
    var (the lhs.name) ~bitsize
  | `dba_lhs_var_restrict ->
    let bitsize = Size.Bit.of_int32 (the lhs.size)
    and lo = Int32.to_int (the lhs.low)
    and hi = Int32.to_int (the lhs.high) in
    _restrict (the lhs.name) bitsize lo hi
  | `dba_store ->
    let bytesize = Size.Byte.of_int32 (the lhs.size) in
    store bytesize (parse_endian (the lhs.endian)) (parse_dbaexpr (the lhs.expr))

let generate_instr (instr:Dba_types.Statement.t) =
  let location =
    generate_dbacodeaddress (Dba_types.Statement.location instr) in
  let piq_instr = Dba_piqi.default_dbainstr () in
  let open Dba_piqi.Dbainstr in
  let piq_instr =
    match Dba_types.Statement.instruction instr with
    | Dba.Instr.Assign(lhs,e,off) ->
      { piq_instr with typeid = `dba_ik_assign;
                       lhs = Some (generate_lhs lhs);
                       expr = Some (generate_dbaexpr e);
                       offset = Some (Int32.of_int off)}
    | Dba.Instr.SJump(addr,tagopt) ->
      let tags =
        match tagopt with Some a -> Some (generate_tag a) | None -> None in
      {piq_instr with typeid=`dba_ik_sjump;
                      address=Some (generate_codeaddress addr);
                      tags}
    | Dba.Instr.DJump(e,tagopt) ->
      let tags =
        match tagopt with | Some a -> Some (generate_tag a) | None -> None in
      {piq_instr with typeid=`dba_ik_djump;
                      expr=Some (generate_dbaexpr e);
                      tags}
    | Dba.Instr.If(c,addr,off) ->
      {piq_instr with
       typeid = `dba_ik_if;
       cond = Some (generate_dbacond c);
       address = Some (generate_codeaddress addr);
       offset = Some (Int32.of_int off)}
    | Dba.Instr.Stop state ->
      { piq_instr with typeid = `dba_ik_stop;
                       stopinfos = generate_dbastate state}
    | Dba.Instr.Undef(lhs,off) ->
      {piq_instr with typeid = `dba_ik_undef; lhs = Some (generate_lhs lhs);
                      offset = Some (Int32.of_int off)}
    | Dba.Instr.Assert(_,_)
    | Dba.Instr.Assume(_,_)
    | Dba.Instr.NondetAssume(_,_,_)
    | Dba.Instr.Nondet(_,_,_)
    | Dba.Instr.Malloc(_,_,_)
    | Dba.Instr.Free(_,_)
    | Dba.Instr.Print(_,_) -> raise (NotSupportedForExport "Invalid instr")
  in { piq_instr with location }

let parse_instr inst =
  let open Dba_piqi.Dbainstr in
  let open Dba.Instr in
  let loc = parse_dbacodeaddress inst.location in
  let instkind =
    match inst.typeid with
    | `dba_ik_assign ->
      let lv = parse_lhs (the inst.lhs)
      and e = parse_dbaexpr (the inst.expr)
      and s = Int32.to_int (the inst.offset) in
      Dba.Instr.assign lv e s
    | `dba_ik_sjump ->
      let tag = parse_tag_option inst.tags
      and caddr = parse_codeaddress (the inst.address) in
      static_jump caddr ?tag
    | `dba_ik_djump ->
      let tag = parse_tag_option inst.tags
      and e = parse_dbaexpr (the inst.expr) in
      dynamic_jump e ?tag

    | `dba_ik_if ->
      let c = parse_cond (the inst.cond)
      and ethen = parse_codeaddress (the inst.address)
      and eelse = Int32.to_int (the inst.offset) in
      ite c ethen eelse

    | `dba_ik_stop ->
      stop  (parse_dbastate inst.stopinfos)
    | `dba_ik_undef ->
      undefined (parse_lhs (the inst.lhs)) (Int32.to_int (the inst.offset))
    | `dba_ik_assert
    | `dba_ik_assume
    | `dba_ik_nondet_assume
    | `dba_ik_nondet
    | `dba_ik_malloc
    | `dba_ik_free
    | `dba_ik_print -> raise (NotSupportedForImport "Invalid instruction")
  in Dba_types.Statement.create loc instkind

let generate_dbalist instrs =
  { Dba_piqi.Dba_list.instrs = List.map generate_instr instrs }

let parse_dbalist raw_instrs =
  List.map parse_instr raw_instrs.Dba_piqi.Dba_list.instrs
