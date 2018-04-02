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
  let bv = generate_bitvector addr.Dba.base in
  { Dba_piqi.Dbacodeaddress.bitvector = bv;
    Dba_piqi.Dbacodeaddress.dbaoffset = Int32.of_int addr.Dba.id;}

let parse_dbacodeaddress addr : Dba.address =
  let open Dba_piqi.Dbacodeaddress in
  let bv = parse_bitvector addr.bitvector
  and id = Int32.to_int addr.dbaoffset in
  Dba_types.Caddress.create bv id


let generate_codeaddress (addr:Dba.jump_target) =
  let open Dba_piqi.Codeaddress in
  match addr with
  | Dba.JInner off ->
    { typeid = `local; offset = Some (Int32.of_int off); address = None}
  | Dba.JOuter addr ->
    { typeid = `non_local;
      offset = None;
      address = Some (generate_dbacodeaddress addr)}

let parse_codeaddress addr: Dba.jump_target =
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

let binaryop_to_piqi (op:Dba.binary_op) =
  match op with
  | Dba.Plus -> `dba_plus | Dba.Minus ->  `dba_minus
  | Dba.MultU -> `dba_mult_u | Dba.MultS -> `dba_mult_s
  (* | Dba.Power -> `dba_power  *)
  | Dba.DivU -> `dba_div_u | Dba.DivS -> `dba_div_s | Dba.ModU -> `dba_mod_u
  | Dba.ModS -> `dba_mod_s | Dba.Or -> `dba_or
  | Dba.And -> `dba_and | Dba.Xor -> `dba_xor
  | Dba.Concat -> `dba_concat
  | Dba.LShift -> `dba_lshift_u | Dba.RShiftU -> `dba_rshift_u
  | Dba.RShiftS -> `dba_rshift_s
  | Dba.LeftRotate -> `dba_left_rotate | Dba.RightRotate -> `dba_right_rotate
  | Dba.Eq -> `dba_eq | Dba.Diff -> `dba_diff
  | Dba.LeqU -> `dba_leq_u | Dba.LtU  -> `dba_lt_u
  | Dba.GeqU -> `dba_geq_u | Dba.GtU -> `dba_gt_u
  | Dba.LeqS -> `dba_leq_s | Dba.LtS -> `dba_lt_s
  | Dba.GeqS -> `dba_geq_s | Dba.GtS -> `dba_gt_s

let piqi_to_binaryop = function
  | `dba_plus -> Dba.Plus  | `dba_minus -> Dba.Minus
  | `dba_mult_u -> Dba.MultU | `dba_mult_s -> Dba.MultS
  | `dba_div_u -> Dba.DivU | `dba_div_s -> Dba.DivS | `dba_mod_u -> Dba.ModU
  | `dba_mod_s -> Dba.ModS | `dba_or -> Dba.Or
  | `dba_and -> Dba.And | `dba_xor -> Dba.Xor
  | `dba_concat -> Dba.Concat
  | `dba_lshift_u -> Dba.LShift | `dba_rshift_u -> Dba.RShiftU
  | `dba_rshift_s -> Dba.RShiftS
  | `dba_left_rotate -> Dba.LeftRotate | `dba_right_rotate -> Dba.RightRotate
  | `dba_eq -> Dba.Eq | `dba_diff -> Dba.Diff
  | `dba_leq_u -> Dba.LeqU | `dba_lt_u -> Dba.LtU
  | `dba_geq_u -> Dba.GeqU | `dba_gt_u -> Dba.GtU
  | `dba_leq_s -> Dba.LeqS | `dba_lt_s -> Dba.LtS
  | `dba_geq_s -> Dba.GeqS | `dba_gt_s -> Dba.GtS

let parse_endian = function
  | `little -> Dba.LittleEndian
  | `big -> Dba.BigEndian

let rec generate_dbaexpr (e:Dba.expr)  =
  let open Dba_piqi.Dbaexpr in
  let expr  =  Dba_piqi.default_dbaexpr () in
  match e with
  | Dba.ExprVar(s,sz,_) ->
    {expr with typeid = `dba_expr_var;
               name = Some s;
               size = Some (Int32.of_int sz)}
  | Dba.ExprLoad(sz, endian, e1) ->
    let endian =
      match endian with Dba.LittleEndian -> `little | Dba.BigEndian -> `big in
    {expr with typeid = `dba_load;
               size = Some (Int32.of_int sz);
               endian = Some endian;
               expr1 = Some (generate_dbaexpr e1)}
  | Dba.ExprCst(`Constant, bv) ->
    { expr with typeid = `dba_expr_cst;
                bitvector = Some (generate_bitvector bv) }
  | Dba.ExprUnary(uop, e1) ->
    let uop =
      match uop with Dba.UMinus -> `dba_unary_minus | Dba.Not -> `dba_unary_not in
    {expr with typeid = `dba_expr_unary;
               unaryop = Some uop;
               expr1 = Some (generate_dbaexpr e1)}
  | Dba.ExprBinary(op, e1, e2) ->
    { expr with typeid = `dba_expr_binary;
                binaryop = Some (binaryop_to_piqi op);
                expr1 = Some (generate_dbaexpr e1);
                expr2 = Some (generate_dbaexpr e2)}
  | Dba.ExprRestrict(e1,low,high) ->
    { expr with typeid = `dba_expr_restrict;
                expr1 = Some (generate_dbaexpr e1);
                low = Some (Int32.of_int low);
                high = Some (Int32.of_int high)}
  | Dba.ExprExtU(e1,sz) ->
    {expr with typeid = `dba_expr_ext_u;
               expr1 = Some (generate_dbaexpr e1);
               size = Some (Int32.of_int sz)}
  | Dba.ExprExtS(e1,sz) ->
    { expr with typeid = `dba_expr_ext_s;
                expr1 = Some (generate_dbaexpr e1);
                size = Some (Int32.of_int sz)}
  | Dba.ExprIte(c1,e1,e2) ->
    { expr with typeid = `dba_expr_ite;
                cond = Some (generate_dbacond c1);
                expr1 = Some (generate_dbaexpr e1);
                expr2 = Some (generate_dbaexpr e2)}
  | Dba.ExprAlternative _ ->
    raise (NotSupportedForExport "DbaExprAlternative not implemented")
  | _ -> assert false


and generate_dbacond (c:Dba.cond)  =
  let open Dba_piqi.Dbacond in
  let cond  =  Dba_piqi.default_dbacond () in
  match c with
  | Dba.CondReif(e1) ->
    { cond with typeid = `dba_cond_reif; expr = Some (generate_dbaexpr e1)}
  | Dba.CondNot c1 ->
    {cond with typeid = `dba_cond_not; cond1 = Some (generate_dbacond c1)}
  | Dba.CondAnd (c1,c2) ->
    {cond with typeid = `dba_cond_and;
               cond1 = Some (generate_dbacond c1);
               cond2 = Some (generate_dbacond c2)}
  | Dba.CondOr (c1,c2) ->
    {cond with typeid = `dba_cond_or;
               cond1 = Some (generate_dbacond c1);
               cond2 = Some (generate_dbacond c2)}
  | Dba.True -> {cond with typeid = `dba_true}
  | Dba.False -> {cond with typeid = `dba_false}


let rec parse_dbaexpr e =
  let open Dba_piqi.Dbaexpr in
  match e.typeid with
  | `dba_expr_var -> Dba.ExprVar(the e.name, Int32.to_int (the e.size), None)
  | `dba_load ->
    Dba.ExprLoad(Int32.to_int (the e.size),
                 parse_endian (the e.endian), parse_dbaexpr (the e.expr1))
  | `dba_expr_cst -> Dba.ExprCst(`Constant, parse_bitvector (the e.bitvector))
  | `dba_expr_unary ->
    let uop =
      match the e.unaryop with
      | `dba_unary_minus -> Dba.UMinus
      | `dba_unary_not -> Dba.Not in
    Dba.ExprUnary(uop, parse_dbaexpr (the e.expr1))
  | `dba_expr_binary ->
    Dba.ExprBinary(piqi_to_binaryop (the e.binaryop),
                   parse_dbaexpr (the e.expr1), parse_dbaexpr (the e.expr2))
  | `dba_expr_restrict ->
    Dba.ExprRestrict(parse_dbaexpr (the e.expr1),
                     Int32.to_int (the e.low), Int32.to_int (the e.high))
  | `dba_expr_ext_u ->
    Dba.ExprExtU(parse_dbaexpr (the e.expr1), Int32.to_int (the e.size))
  | `dba_expr_ext_s ->
    Dba.ExprExtS(parse_dbaexpr (the e.expr1), Int32.to_int (the e.size))
  | `dba_expr_ite ->
    Dba.ExprIte(parse_cond (the e.cond),
                parse_dbaexpr (the e.expr1),
                parse_dbaexpr (the e.expr2))
  | `dba_expr_alternative -> raise (NotSupportedForImport("mouarf"))

and parse_cond c: Dba.cond =
  let open Dba_piqi.Dbacond in
  match c.typeid with
  | `dba_cond_reif -> Dba.CondReif(parse_dbaexpr (the c.expr))
  | `dba_cond_not -> Dba.CondNot(parse_cond (the c.cond1))
  | `dba_cond_and ->
    Dba.CondAnd(parse_cond (the c.cond1), parse_cond (the c.cond2))
  | `dba_cond_or ->
    Dba.CondOr(parse_cond (the c.cond1), parse_cond (the c.cond2))
  | `dba_true -> Dba.True
  | `dba_false -> Dba.False

let generate_lhs (lhs:Dba.lhs) =
  let open Dba_piqi.Dba_lhs in
  let piq_lhs = Dba_piqi.default_dba_lhs () in
  match lhs with
  | Dba.LhsVar(s,sz,_) ->
    {piq_lhs with typeid=`dba_lhs_var; name=Some s; size=Some (Int32.of_int sz)}
  | Dba.LhsVarRestrict(s,sz,low,high) ->
    {piq_lhs with typeid = `dba_lhs_var_restrict;
                  name = Some s;
                  size = Some (Int32.of_int sz);
                  low = Some (Int32.of_int low);
                  high = Some (Int32.of_int high)}
  | Dba.LhsStore(sz,endian,e) ->
    let indien =
      match endian with Dba.LittleEndian -> `little | Dba.BigEndian -> `big in
    {piq_lhs with typeid = `dba_store;
                  size = Some (Int32.of_int sz);
                  endian = Some indien;
                  expr = Some (generate_dbaexpr e)}

let parse_lhs lhs =
  let open Dba_piqi.Dba_lhs in
  match lhs.typeid with
  | `dba_lhs_var ->
    Dba.LhsVar(the lhs.name, Int32.to_int (the lhs.size), None)
  | `dba_lhs_var_restrict ->
    Dba.LhsVarRestrict(
      the lhs.name,
      Int32.to_int (the lhs.size),
      Int32.to_int (the lhs.low), Int32.to_int (the lhs.high))
  | `dba_store ->
    Dba.LhsStore(Int32.to_int (the lhs.size),
                 parse_endian (the lhs.endian), parse_dbaexpr (the lhs.expr))

let generate_instr (instr:Dba_types.Statement.t) =
  let location =
    generate_dbacodeaddress (Dba_types.Statement.location instr) in
  let piq_instr = Dba_piqi.default_dbainstr () in
  let open Dba_piqi.Dbainstr in
  let piq_instr =
    match Dba_types.Statement.instruction instr with
    | Dba.IkAssign(lhs,e,off) ->
      { piq_instr with typeid = `dba_ik_assign;
                       lhs = Some (generate_lhs lhs);
                       expr = Some (generate_dbaexpr e);
                       offset = Some (Int32.of_int off)}
    | Dba.IkSJump(addr,tagopt) ->
      let tags =
        match tagopt with Some a -> Some (generate_tag a) | None -> None in
      {piq_instr with typeid=`dba_ik_sjump;
                      address=Some (generate_codeaddress addr);
                      tags}
    | Dba.IkDJump(e,tagopt) ->
      let tags =
        match tagopt with | Some a -> Some (generate_tag a) | None -> None in
      {piq_instr with typeid=`dba_ik_djump;
                      expr=Some (generate_dbaexpr e);
                      tags}
    | Dba.IkIf(c,addr,off) ->
      {piq_instr with
       typeid = `dba_ik_if;
       cond = Some (generate_dbacond c);
       address = Some (generate_codeaddress addr);
       offset = Some (Int32.of_int off)}
    | Dba.IkStop state ->
      { piq_instr with typeid = `dba_ik_stop;
                       stopinfos = generate_dbastate state}
    | Dba.IkUndef(lhs,off) ->
      {piq_instr with typeid = `dba_ik_undef; lhs = Some (generate_lhs lhs);
                      offset = Some (Int32.of_int off)}
    | Dba.IkAssert(_,_)
    | Dba.IkAssume(_,_)
    | Dba.IkNondetAssume(_,_,_)
    | Dba.IkNondet(_,_,_)
    | Dba.IkMalloc(_,_,_)
    | Dba.IkFree(_,_)
    | Dba.IkPrint(_,_) -> raise (NotSupportedForExport "Invalid instr")
  in { piq_instr with location }

let parse_instr inst =
  let open Dba_piqi.Dbainstr in
  let loc = parse_dbacodeaddress inst.location in
  let instkind =
    match inst.typeid with
    | `dba_ik_assign ->
      Dba.IkAssign(parse_lhs (the inst.lhs),
                   parse_dbaexpr(the inst.expr),
                   Int32.to_int (the inst.offset))
    | `dba_ik_sjump ->
      Dba.IkSJump(parse_codeaddress (the inst.address),
                  parse_tag_option inst.tags)
    | `dba_ik_djump ->
      Dba.IkDJump(parse_dbaexpr (the inst.expr), parse_tag_option inst.tags)
    | `dba_ik_if ->
      Dba.IkIf(parse_cond (the inst.cond),
               parse_codeaddress (the inst.address),
               Int32.to_int (the inst.offset))
    | `dba_ik_stop -> Dba.IkStop(parse_dbastate inst.stopinfos)
    | `dba_ik_undef ->
      Dba.IkUndef(parse_lhs (the inst.lhs), Int32.to_int (the inst.offset))
    | `dba_ik_assert
    | `dba_ik_assume
    | `dba_ik_nondet_assume
    | `dba_ik_nondet
    | `dba_ik_malloc
    | `dba_ik_free
    | `dba_ik_print -> raise (NotSupportedForImport("Invalid instr"))
  in Dba_types.Statement.create loc instkind

let generate_dbalist instrs =
  { Dba_piqi.Dba_list.instrs = List.map generate_instr instrs }

let parse_dbalist raw_instrs =
  List.map parse_instr raw_instrs.Dba_piqi.Dba_list.instrs
