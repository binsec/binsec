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

open X86Types

exception InstructionUnhandled of string


type instr_tbl = {
  mutable insertions : int;
  tbl: (string, unit) Hashtbl.t;
}

let create_instr_tbl () = { insertions = 0; tbl = Hashtbl.create 7; }

let add instr_tbl instruction =
  instr_tbl.insertions <- instr_tbl.insertions + 1;
  Hashtbl.replace instr_tbl.tbl instruction ()

type instruction_stats = {
  handled : instr_tbl;
  unknown : instr_tbl;
}

let _add_unknown_instruction,
    _add_handled_instruction,
    handled_instructions,
    unknown_instructions,
    pp_unknown_instructions,
    native_instructions_decoded
     =
     let stats = {
       handled = create_instr_tbl ();
       unknown = create_instr_tbl ();
     }
     in
     (fun s -> add stats.handled s),
     (fun s -> add stats.unknown s),
     (fun () -> stats.handled.insertions, Hashtbl.length stats.handled.tbl),
     (fun () -> stats.unknown.insertions, Hashtbl.length stats.unknown.tbl),
     (fun fmt () ->
        let open Format in
        fprintf fmt "@[<hov 0>";
        Hashtbl.iter (fun k _ -> fprintf fmt "%s;@ " k) stats.unknown.tbl;
        fprintf fmt "@]"),
     (fun () -> stats.handled.insertions + stats.unknown.insertions)


let high_bit_8 = Int64.shift_left Int64.one 7
let higher_bit_8 = Int64.shift_left Int64.one 8
let high_bit_16 = Int64.shift_left Int64.one 15
let higher_bit_16 = Int64.shift_left Int64.one 16
let high_bit = Int64.shift_left Int64.one 31
let higher_bit = Int64.shift_left Int64.one 32
let high_bit_128 = Int64.shift_left Int64.one 127
let higher_bit_128 = Int64.shift_left Int64.one 128


let temp_size size = Format.sprintf "temp%d" size

let cpt_size size = Format.sprintf "cpt%d" size

let size_mode m =
  match m with
  | `M32 -> 32
  | `M16 -> 16
  | `M8 -> 8

let nbytes_mode m =
  match m with
  | `M32 -> 4
  | `M16 -> 2
  | `M8  -> 1


let catenate_expressions = function
  | [] -> assert false
  | e :: es ->
    let concat = Dba_types.Expr.binary Dba.Concat in
    List.fold_left concat e es


let cst_of_int n size =
  let bv = Bitvector.create (Bigint.big_int_of_int n) size in
  Dba_types.Expr.constant bv


let four_32 = cst_of_int 4 32
let maxi_bv n = Dba_types.Expr.constant (Bitvector.max_ubv n)

let cst_of_int64_xmm n =
  let size = 128 in
  let bv =
    if Int64.logand n high_bit_128 = Int64.zero then
      Bitvector.create (Bigint.big_int_of_int64 n) size
    else
      Bitvector.create
        (Bigint.big_int_of_int64 (Int64.neg (Int64.sub higher_bit_128 n)))
        size
  in Dba_types.Expr.constant bv

let cst_of_int64_32 n =
  let size = 32 in
  if Int64.logand n high_bit = Int64.zero then
    let bv = Bitvector.create (Bigint.big_int_of_int64 n) size in
    Dba_types.Expr.constant bv
  else
    let bv = Bitvector.create (Bigint.big_int_of_int64 (Int64.neg (Int64.sub higher_bit n))) size in
    Dba_types.Expr.constant bv

let cst_of_int64_16 n =
  let size = 16 in
  if Int64.logand n high_bit_16 = Int64.zero then
    let bv = Bitvector.create (Bigint.big_int_of_int64 n) size in
    Dba_types.Expr.constant bv
  else
    let bv = Bitvector.create (Bigint.big_int_of_int64 (Int64.neg (Int64.sub higher_bit_16 n))) size in
    Dba_types.Expr.constant bv

let cst_of_int64_8 n =
  let size = 8 in
  if Int64.logand n high_bit_8 = Int64.zero then
    let bv = Bitvector.create (Bigint.big_int_of_int64 n) size in
    Dba_types.Expr.constant bv
  else
    let bv =
      Bitvector.create (Bigint.big_int_of_int64 (Int64.neg (Int64.sub higher_bit_8 n))) size in
    Dba_types.Expr.constant bv

let _is_too_big_shift cst size =
  match cst with
  | Dba.ExprCst (_, bi) ->
    Bigint.ge_big_int (Bitvector.value_of bi) (Bigint.big_int_of_int size)
  | _ -> false

let _addr_of_int n = (Bitvector.create (Bigint.big_int_of_int n) 32, 0)

let strange_bitvector_of_int64 n size =
  let v = if Int64.compare n Int64.zero = -1 then Int64.add higher_bit n else n
  in Bitvector.create (Bigint.big_int_of_int64 v) size

let _strange_cst_of_int64 n size =
  let bv = strange_bitvector_of_int64 n size in
  Dba_types.Expr.constant bv

let strange_addr_of_int64 n =
  let bv = strange_bitvector_of_int64 n 32 in
  Dba.JOuter (Dba_types.Caddress.block_start bv)

let offsets_of_reg8 r =
  let i = X86Util.reg8_to_int r in
  if i < 4 then
    0, 7, X86Util.int_to_reg32 i
  else
    8, 15, X86Util.int_to_reg32 (i - 4)

let offsets_of_reg16 r =
  match r with
  | X86Types.AX -> 0, 15, X86Types.EAX
  | X86Types.CX -> 0, 15, X86Types.ECX
  | X86Types.DX -> 0, 15, X86Types.EDX
  | X86Types.BX -> 0, 15, X86Types.EBX
  | X86Types.SP -> 0, 15, X86Types.ESP
  | X86Types.BP -> 0, 15, X86Types.EBP
  | X86Types.SI -> 0, 15, X86Types.ESI
  | X86Types.DI -> 0, 15, X86Types.EDI

let offsets_of_reg16_32 = function
  | X86Types.EAX -> 0, 15, X86Types.EAX
  | X86Types.ECX -> 0, 15, X86Types.ECX
  | X86Types.EDX -> 0, 15, X86Types.EDX
  | X86Types.EBX -> 0, 15, X86Types.EBX
  | X86Types.ESP -> 0, 15, X86Types.ESP
  | X86Types.EBP -> 0, 15, X86Types.EBP
  | X86Types.ESI -> 0, 15, X86Types.ESI
  | X86Types.EDI -> 0, 15, X86Types.EDI

let offsets_of_reg8_32 r =
  match r with
  | X86Types.EAX -> 0, 7, X86Types.EAX
  | X86Types.ECX -> 0, 7, X86Types.ECX
  | X86Types.EDX -> 0, 7, X86Types.EDX
  | X86Types.EBX -> 0, 7, X86Types.EBX
  | X86Types.ESP -> 8, 15, X86Types.EAX (* CHECK THAT *)
  | X86Types.EBP -> 8, 15, X86Types.ECX
  | X86Types.ESI -> 8, 15, X86Types.EDX
  | X86Types.EDI -> 8, 15, X86Types.EBX


let lhs_of_seg s = Dba.LhsVar (X86Util.segment_reg_to_string s, 16, None)

let lhs_of_reg r mode =
  match mode with
  | `M32 -> Dba.LhsVar (X86Util.reg32_to_string r, 32 , None)
  | `M16 ->
    let off1, off2, r32 = offsets_of_reg16_32 r in
    Dba.LhsVarRestrict (X86Util.reg32_to_string r32, 32, off1, off2)
  | `M8 ->
    let off1, off2, r32 = offsets_of_reg8_32 r in
    Dba.LhsVarRestrict (X86Util.reg32_to_string r32, 32, off1, off2)

let lhs_of_reg32 r = Dba.LhsVar (X86Util.reg32_to_string r, 32, None)

let lhs_of_reg16 r =
  let off1, off2, r32 = offsets_of_reg16 r in
  Dba.LhsVarRestrict (X86Util.reg32_to_string r32, 32, off1, off2)

let lhs_of_reg8 r =
  let off1, off2, r32 = offsets_of_reg8 r in
  Dba.LhsVarRestrict (X86Util.reg32_to_string r32, 32, off1, off2)

let edi_lval = lhs_of_reg32 EDI
let esi_lval = lhs_of_reg32 ESI
let esp_lval = lhs_of_reg32 ESP

let lhs_of_reg_xmm r reg_t =
  match reg_t with
  | XMM -> Dba.LhsVar (X86Util.xmm_reg_to_string r, 128, None)
  | MM  -> Dba.LhsVar (X86Util.mm_reg_to_string (X86Util.xmm_reg_to_mm_reg r), 64, None)

let lhs_of_float_reg r = Dba.LhsVar (X86Util.float_reg_to_string r, 80, None)

let _lhs_of_xmm_reg_restrict r off1 off2 =
  Dba.LhsVarRestrict (X86Util.xmm_reg_to_string r, 128, off1, off2)


let lhs_of_flag f inst = Dba.LhsVar (X86Util.flag_to_string f, 1, Some (Dba.Flag inst))

let undef_flag flag = Predba.undefined (lhs_of_flag flag Dba.FlgUnspecified)

let undef_flags flags = List.map undef_flag flags

let expr_of_seg s = Dba.ExprVar (X86Util.segment_reg_to_string s, 16, None)

let expr_of_reg mode r =
  match mode with
  | `M32 -> Dba.ExprVar (X86Util.reg32_to_string r, 32, None)
  | `M16 ->
    let off1, off2, r32 = offsets_of_reg16_32 r in
    Dba.ExprRestrict (Dba.ExprVar (X86Util.reg32_to_string r32, 32, None), off1, off2)
  | `M8 ->
    let off1, off2, r32 = offsets_of_reg8_32 r in
    Dba.ExprRestrict (Dba.ExprVar (X86Util.reg32_to_string r32, 32, None), off1, off2)

let expr_of_reg32 = expr_of_reg `M32

let expr_of_reg16 r =
  let off1, off2, r32 = offsets_of_reg16 r in
  Dba.ExprRestrict (Dba.ExprVar (X86Util.reg32_to_string r32, 32, None), off1, off2)

let expr_of_reg8 r =
  let off1, off2, r32 = offsets_of_reg8 r in
  Dba.ExprRestrict (Dba.ExprVar (X86Util.reg32_to_string r32, 32, None), off1, off2)

let esp_expr = expr_of_reg32 ESP
and esi_expr = expr_of_reg32 ESI
and edi_expr = expr_of_reg32 EDI


let e_of_reg addrMode r =
  match addrMode with
  | A32 -> Dba.ExprVar (X86Util.reg32_to_string r, 32, None)
  | A16 ->
    let off1, off2, r32 = offsets_of_reg16_32 r in
    let e = Dba.ExprRestrict (Dba.ExprVar (X86Util.reg32_to_string r32, 32, None), off1, off2) in
    Dba.ExprExtU (e, 32)


let expr_of_reg_xmm r xmm =
  match xmm with
  | XMM -> Dba.ExprVar (X86Util.xmm_reg_to_string r, 128, None)
  | MM -> Dba.ExprRestrict (Dba.ExprVar (X86Util.xmm_reg_to_string r, 128, None), 0, 63)

let expr_of_float_reg r = Dba.ExprVar (X86Util.float_reg_to_string r, 80, None)

let expr_of_flag f =
  Dba.ExprVar (X86Util.flag_to_string f, 1, Some (Dba.Flag Dba.FlgUnspecified))

let cf_flag = expr_of_flag CF
let of_flag = expr_of_flag OF
let zf_flag = expr_of_flag ZF


let expr_of_addr addr =
  let open Dba_types in
  let disp = addr.addrDisp in
  let expr_reg = e_of_reg addr.addrMode in
  match (disp = Int64.zero, addr.addrBase, addr.addrIndex) with
  | true, None, None -> Expr.zeros 32
  | true, Some r, None -> expr_reg r
  | true, Some r1, Some (sc, r2) ->
    let bop = Expr.umul (expr_reg r2) (cst_of_int (X86Util.scale_to_size sc) 32) in
    Expr.add (expr_reg r1) bop
  | true, None, Some (Scale1, r) -> expr_reg r
  | true, None, Some (sc, r) ->
    Expr.umul (expr_reg r) (cst_of_int (X86Util.scale_to_size sc) 32)
  | false, _, _ ->
    begin
      match Int64.compare disp Int64.zero > 0, addr.addrBase, addr.addrIndex with
      | false, None, None -> cst_of_int64_32 disp
      | true, None, None  -> cst_of_int64_32 disp
      | false, Some r, None ->
        Expr.sub (expr_reg r) (cst_of_int64_32 (Int64.neg disp))
      | true, Some r, None ->
        Expr.add (expr_reg r) (cst_of_int64_32 disp)
      | false, Some r1, Some (Scale1, r2) ->
        Expr.sub (expr_reg r1)
                  (Expr.add (expr_reg r2) (cst_of_int64_32 (Int64.neg disp)))
      | true, Some r1, Some (Scale1, r2) ->
        Expr.add (expr_reg r1) (Expr.add (expr_reg r2) (cst_of_int64_32 disp))
      | false, Some r1, Some (sc, r2) ->
        let lop =
          Expr.umul (expr_reg r2) (cst_of_int (X86Util.scale_to_size sc) 32) in
        let e = Expr.add lop (cst_of_int64_32 (Int64.neg disp)) in
        Expr.sub (expr_reg r1) e
      | true, Some r1, Some (sc, r2) ->
        let bop =
          Expr.umul (expr_reg r2) (cst_of_int (X86Util.scale_to_size sc) 32) in
        let bop = Expr.add bop (cst_of_int64_32 disp) in
        Expr.add (expr_reg r1) bop
      | false, None, Some (Scale1, r) ->
        Expr.sub (expr_reg r) (cst_of_int64_32 (Int64.neg disp))
      | true, None, Some (Scale1, r) ->
        Expr.add (expr_reg r) (cst_of_int64_32 disp)
      | false, None, Some (sc, r) ->
        let bop =
          Expr.umul (expr_reg r) (cst_of_int (X86Util.scale_to_size sc) 32) in
        Expr.sub bop (cst_of_int64_32 (Int64.neg disp))
      | true, None, Some (sc, r) ->
        let bop =
          Expr.umul (expr_reg r) (cst_of_int (X86Util.scale_to_size sc) 32) in
        Expr.add bop (cst_of_int64_32 disp)
    end


let if_df = Predba.jif (Dba.CondReif (expr_of_flag DF))

let cond_of_cc cc =
  match cc.truth_value, cc.condition with
  | true,  O  -> Dba.CondReif of_flag
  | false, O  -> Dba.CondNot(Dba.CondReif of_flag)
  | true,  B  -> Dba.CondReif cf_flag
  | false, B  -> Dba.CondNot(Dba.CondReif cf_flag)
  | true,  Z  -> Dba.CondReif zf_flag
  | false, Z  -> Dba.CondNot(Dba.CondReif zf_flag)
  | true,  BE -> Dba.CondOr (Dba.CondReif cf_flag, Dba.CondReif zf_flag)
  | false, BE ->
    Dba.CondAnd(Dba.CondNot(Dba.CondReif cf_flag),
                Dba.CondNot(Dba.CondReif zf_flag))
  | true,  S  -> Dba.CondReif(expr_of_flag SF)
  | false, S  -> Dba.CondNot(Dba.CondReif(expr_of_flag SF))
  | true,  P  -> Dba.CondReif (expr_of_flag PF)
  | false, P  -> Dba.CondNot (Dba.CondReif (expr_of_flag PF))
  | true,  L  -> Dba.CondReif(Dba_types.Expr.diff (expr_of_flag SF)  of_flag)
  | false, L  -> Dba.CondReif(Dba_types.Expr.eq (expr_of_flag SF) of_flag)
  | true,  LE ->
    Dba.CondOr(Dba.CondReif zf_flag,
               Dba.CondReif (Dba_types.Expr.diff (expr_of_flag SF) of_flag))
  | false, LE ->
    Dba.CondAnd(
      Dba.CondNot(Dba.CondReif zf_flag),
      Dba.CondReif(Dba_types.Expr.eq (expr_of_flag SF) of_flag))


let retrieve_sreg a =
  let rec is_stack_access e sreg =
    match e with
    | Dba.ExprVar (name, _, _) ->
      if name = "ebp" || name = "esp" then SS else DS
    | Dba.ExprBinary (_, e1, e2) ->
      let sreg = is_stack_access e1 sreg in
      is_stack_access e2 sreg
    | _ -> sreg
  in is_stack_access a DS


let segment_address sreg a =
  if Disasm_options.is_ignored_segment sreg then a
  else
    let open Dba_types in
    let gdt = Dba.ExprVar ("gdt", 32, None) in
    let name = X86Util.segment_reg_to_string sreg in
    let sreg = Dba.ExprVar (name, 16, None) in
    let sreg = Dba.ExprExtU (sreg, 32) in
    let e =
      Expr.add gdt sreg
      |> Expr.load (Basic_types.ByteSize.create 4) Dba.LittleEndian in
    Expr.add e a


let effective_address a sreg =
  if Disasm_options.ProtectedMode.get ()
  then
    let r =
      match sreg with
      | None -> retrieve_sreg a
      | Some r -> r
    in segment_address r a
  else
    a


let lhs_of_mem mode ?(sreg=None) a =
  let a = effective_address a sreg in
  let nbytes = X86Util.bytesize_of_szmode mode in
  Dba_types.LValue.store nbytes Dba.LittleEndian a


let lhs_of_mem32 = lhs_of_mem `M32
and lhs_of_mem16 = lhs_of_mem `M16
and lhs_of_mem8  = lhs_of_mem `M8


let lhs_of_mem_xmm mm a sreg =
  let a = effective_address a sreg in
  let nbytes = X86Util.bytesize_of_simd_size mm in
  Dba_types.LValue.store nbytes Dba.LittleEndian a


let expr_of_mem (mode:X86Types.sizeMode) ?(sreg=None) a =
  let a = effective_address a sreg in
  let nbytes = X86Util.bytesize_of_szmode mode in
  Dba_types.Expr.load nbytes Dba.LittleEndian a


let expr_of_mem32 = expr_of_mem `M32
and expr_of_mem16 = expr_of_mem `M16
and expr_of_mem8  = expr_of_mem `M8

let expr_of_mem_xmm mm ?(sreg=None) a =
  let a = effective_address a sreg in
  let nbytes = X86Util.bytesize_of_simd_size mm in
  Dba_types.Expr.load nbytes Dba.LittleEndian a


let assign_flag flag ?(flag_cmp=Dba.FlgUnspecified) e =
  Predba.assign (lhs_of_flag flag flag_cmp) e


let assign_register register mode e =
  Predba.assign (lhs_of_reg register mode) e


let fail_immediate_lvalue fname =
  let msg = Format.sprintf "%s: immediate cannot be lvalue" fname in
  failwith msg


let disas_lval op mode sreg =
  match op with
  | Reg r -> lhs_of_reg r mode
  | Address a -> lhs_of_mem mode (expr_of_addr a) ~sreg
  | Imm _ -> fail_immediate_lvalue "disas_lval"


let disas_lval_xmm op xmm_t size_t sreg =
  match op with
  | Reg r -> lhs_of_reg_xmm r xmm_t
  | Address a -> lhs_of_mem_xmm size_t (expr_of_addr a) sreg
  | Imm _ -> fail_immediate_lvalue "disas_lval_xmm"


let disas_lval16 op sreg =
  match op with
  | Reg r -> lhs_of_reg16 r
  | Address a -> lhs_of_mem16 (expr_of_addr a) ~sreg
  | Imm _ -> fail_immediate_lvalue "disas_lval16"


let disas_lval8 op sreg =
  match op with
  | Reg r -> lhs_of_reg8 r
  | Address a -> lhs_of_mem8 (expr_of_addr a) ~sreg
  | Imm _ -> fail_immediate_lvalue "disas_lval8"


let disas_expr op mode sreg =
  match op with
  | Reg r -> expr_of_reg mode r
  | Address a -> expr_of_mem mode ~sreg (expr_of_addr a)
  | Imm i ->
    match mode with
    | `M32 -> cst_of_int64_32 i
    | `M16 -> cst_of_int64_16 i
    | `M8  -> cst_of_int64_8 i


let disas_expr_xmm op xmm_t size_t sreg =
  match op with
  | Reg r -> expr_of_reg_xmm r xmm_t
  | Address a -> expr_of_mem_xmm size_t ~sreg (expr_of_addr a)
  | Imm i -> cst_of_int64_xmm i


let disas_expr16 op sreg =
  match op with
  | Reg r -> expr_of_reg16 r
  | Address a -> expr_of_mem16 (expr_of_addr a) ~sreg
  | Imm i -> cst_of_int64_16 i

let disas_expr8 op sreg =
  match op with
  | Reg r -> expr_of_reg8 r
  | Address a -> expr_of_mem8 (expr_of_addr a) ~sreg
  | Imm i -> cst_of_int64_8 i


let assign lhs rhs lo hi =
  let open Dba_types in
  match lhs with
  | Dba.LhsStore (_, endian, e) ->
    let nbits = Dba_types.LValue.bitsize lhs in
    let tmp = LValue.temp nbits in
    let tmp_name = Utils.unsafe_get_opt (LValue.name_of tmp) in
    let nbytes = Basic_types.ByteSize.of_bitsize nbits in
    [ Predba.assign tmp (Expr.load nbytes endian e);
      Predba.assign (LValue.restrict tmp_name nbits lo hi) rhs;
      Predba.assign lhs (Expr.temp nbits) ]
  | Dba.LhsVar (s, size, _) ->
    let lval = LValue.restrict s (Basic_types.BitSize.create size) lo hi in
    [ Predba.assign lval rhs ]
  | Dba.LhsVarRestrict (s, _, o1, o2) ->
    let lo = o1 + lo and hi = o1 + hi in
    let nbits = Basic_types.BitSize.create (o2 - o1) in
    [ Predba.assign (LValue.restrict s nbits lo hi) rhs ]


let assign_xmm gop1 off1 off2 gop2 off3 off4 xmm mm sreg =
  let lhs = disas_lval_xmm gop1 xmm mm sreg in
  let rhs = Dba_types.Expr.restrict (disas_expr_xmm gop2 xmm mm sreg) off3 off4 in
  assign lhs rhs off1 off2


let assign_xmm_zero gop1 off1 off2 gop2 off3 off4 xmm mm sreg =
  let lhs = disas_lval_xmm gop1 xmm mm sreg in
  let rhs = Dba.ExprRestrict ((disas_expr_xmm gop2 xmm mm sreg), off3, off4) in
  let open Dba_types in
  let nbits = LValue.bitsize lhs in
  match lhs with
  | Dba.LhsStore (size, endian, e) ->
    let tmp = temp_size size in
    [ Predba.assign (LValue.temp nbits) (Expr.load nbits endian e);
      Predba.assign (LValue.restrict tmp nbits off1 off2) rhs;
      Predba.assign lhs (Expr.temp nbits) ]
  | Dba.LhsVar (s, size, _) ->
      Predba.assign (LValue.restrict s nbits off1 off2) rhs ::
      (match xmm with
      | XMM ->
        [ Predba.assign (LValue.restrict s nbits (off2 + 1) (size - 1))
            (Expr.zeros (size - off2 - 1)) ]
      | MM -> []
    )
  | Dba.LhsVarRestrict (s, _, o1, o2) ->
    let nbits = Basic_types.BitSize.create (o2 - o1) in
    [ Predba.assign (LValue.restrict s nbits (o1 + off1) (o1 + off2)) rhs;
      Predba.assign (LValue.restrict s nbits (o1 + off2 + 1) (o1 - o2 - 1))
        (Expr.zeros (o2 - off2 + 1)) ]


let assign_xmm_expr gop1 lo hi expr xmm mm sreg =
  let lhs = disas_lval_xmm gop1 xmm mm sreg in
  let rhs = expr in
  assign lhs rhs lo hi


let assign_expr_expr gop1 off1 off2 expr _xmm _mm sreg =
  let lhs = disas_lval gop1 `M32 sreg in
  let rhs = expr in
  assign lhs rhs off1 off2


let assign_expr_xmm gop1 off1 off2 gop2 off3 off4 xmm mm sreg =
  let lhs = disas_lval gop1 `M32 sreg in
  let rhs = Dba_types.Expr.restrict (disas_expr_xmm gop2 xmm mm sreg) off3 off4 in
  assign lhs rhs off1 off2


let clear_flag fl inst =
  Predba.assign (lhs_of_flag fl inst) (Dba_types.Expr.zeros 1)


let update_CF op1 op2 _res size op flag_cmp =
  let open Dba_types in
  match op with
  | Add ->
    let sz = size + 1 in
    let op1' = Dba.ExprExtU (op1, sz) in
    let op2' = Dba.ExprExtU (op2, sz) in
    let sum' = Expr.add op1' op2' in
    assign_flag CF (Expr.restrict_to_bit sum' size)
  | Adc ->
    let sz = size + 1 in
    let op1' = Dba.ExprExtU (op1, sz) in
    let op2' = Dba.ExprExtU (op2, sz) in
    let cf_flag = Dba.ExprExtU (cf_flag, sz) in
    let sum' = Expr.add (Expr.add op1' op2') cf_flag in
    assign_flag CF (Expr.restrict_to_bit sum' size)
  | Sub ->
    (* carry = 1 <->  A <_u B *)
    assign_flag CF ~flag_cmp (Expr.ult op1 op2)
  | Sbb ->
    (* carry = 1 <->  A <_u B + CF *)
    let cf_flag = Dba.ExprExtU (cf_flag, size) in
    assign_flag CF (Expr.ult op1 (Expr.add op2 cf_flag))
  | Xor | Or | And -> clear_flag CF Dba.FlgUnspecified


let update_OF op1 op2 res size op flag_cmp =
  let open Dba_types in
  let bit = size - 1 in
  let signbit1 = Expr.restrict_to_bit op1 bit
  and signbit2 = Expr.restrict_to_bit op2 bit
  and rres = Expr.restrict_to_bit res bit in
  match op with
  | Add | Adc -> (* ADC & ADD behaves the same for the OF flag *)
    (* ov=1 <->  A[n]=B[n] /\ A[n] \= (A+B)[n] *)
    assign_flag OF Expr.(logand (eq signbit1 signbit2) (diff signbit1 rres))
  | Sub ->
    (* ov=1 <->  A[n]\= B[n] /\ A[n] \= (A-B)[n] *)
    assign_flag OF ~flag_cmp
      Expr.(logand (diff signbit1 signbit2) (diff signbit1 rres))
  | Sbb ->
    let cf_flag = Dba.ExprExtU (cf_flag, size) in
    let op2' = Expr.add op1 cf_flag in
    let signbit2 = Expr.restrict_to_bit op2' bit in
    (* ov=1 <->  A[n]\= B[n] /\ A[n] \= (A-B)[n] *)
    assign_flag OF
      Expr.(logand (diff signbit1 signbit2) (diff signbit1 rres))
  | Xor | Or | And -> clear_flag OF Dba.FlgUnspecified


let update_ZF res size flag_cmp =
  let open Dba_types in
  assign_flag ZF ~flag_cmp (Expr.eq res (Expr.zeros size))


let update_SF res size flag_cmp =
  assign_flag SF ~flag_cmp Dba_types.Expr.(slt res (zeros size))
(* Another way to update SF: Dba.ExprRestrict(res,size-1, size-1) *)


let update_PF res _size flag_cmp =
  let open Dba_types in
  let rec xor_sum acc i =
    if i < 8
    then xor_sum Expr.(logxor acc (restrict_to_bit res i)) (i + 1)
    else acc
  in
  let e = Expr.(lognot (xor_sum (restrict_to_bit res 0) 1)) in
  assign_flag PF ~flag_cmp e


let update_AF op1 op2 _res _size op flag_cmp =
  let open Dba_types in
  let op1 = Expr.restrict op1 0 7 in
  let op2 = Expr.restrict op2 0 7 in
  let size = 8 in
  let esize = size + 1 in
  let af_flag = Dba.ExprExtU (expr_of_flag AF, size) in
  let uext_esize e = Expr.uext e (Basic_types.BitSize.create esize) in
  match op with
  | Add ->
    let op1' = uext_esize op1 in
    let op2' = uext_esize op2 in
    let sum' = Expr.add op1' op2' in
    assign_flag AF (Expr.restrict_to_bit sum' size)
  | Adc ->
    let op1'    = uext_esize op1 in
    let op2'    = uext_esize op2 in
    let af_flag = uext_esize af_flag in
    let sum' = Expr.add (Expr.add op1' op2') af_flag in
    assign_flag AF (Expr.restrict_to_bit sum' size)
  | Sub -> assign_flag AF ~flag_cmp (Expr.ult op1 op2)
  | Sbb -> assign_flag AF (Expr.ult op1 (Expr.add op2 af_flag))
  | Xor
  | Or
  | And -> clear_flag AF Dba.FlgUnspecified


(* Utility function covering a widespread code pattern *)
let getopt_or_fail ~(in_function:string) = function
  | None ->
    failwith (Format.sprintf "x86toDBA@%s : no result provided" in_function)
  | Some r -> r


let affect_flags_inc op res' size =
  let res = getopt_or_fail ~in_function:"affect_flags_inc" res' in
  [ update_OF op (cst_of_int 1 32) res size Add Dba.FlgUnspecified;
    update_SF res size Dba.FlgUnspecified;
    update_AF op (cst_of_int 1 32) res size Add Dba.FlgUnspecified;
    update_PF res size Dba.FlgUnspecified;
    update_ZF res size Dba.FlgUnspecified]


let affect_flags_dec op res' size =
  let res = getopt_or_fail ~in_function:"affect_flags_dec" res' in
  [ update_OF op (cst_of_int 1 32) res size Sub Dba.FlgUnspecified;
    update_SF res size Dba.FlgUnspecified;
    update_AF op (cst_of_int 1 32) res size Sub Dba.FlgUnspecified;
    update_PF res size Dba.FlgUnspecified;
    update_ZF res size Dba.FlgUnspecified]


let affect_flags_arith op op1 op2 res_f size =
  let res = getopt_or_fail ~in_function:"affect_flags_arith" res_f in
  match op with
  | Add ->
    [ update_OF op1 op2 res size Add Dba.FlgUnspecified;
      update_SF res size Dba.FlgUnspecified;
      update_ZF res size Dba.FlgUnspecified;
      update_AF op1 op2 res size Add Dba.FlgUnspecified;
      update_PF res size Dba.FlgUnspecified;
      update_CF op1 op2 res size Add Dba.FlgUnspecified ]
  | Adc ->
    [ update_OF op1 op2 res size Adc Dba.FlgUnspecified;
      update_SF res size Dba.FlgUnspecified;
      update_ZF res size Dba.FlgUnspecified;
      update_AF op1 op2 res size Adc Dba.FlgUnspecified;
      update_PF res size Dba.FlgUnspecified;
      update_CF op1 op2 res size Adc Dba.FlgUnspecified ]
  | Sub ->
    let inst = Dba.FlgSub (op1, op2) in
    [ update_OF op1 op2 res size Sub inst;
      update_SF res size inst;
      update_ZF res size inst;
      update_AF op1 op2 res size Sub inst;
      update_PF res size inst;
      update_CF op1 op2 res size Sub inst ]
  | Sbb ->
    [	update_OF op1 op2 res size Sbb Dba.FlgUnspecified;
      update_SF res size Dba.FlgUnspecified;
      update_ZF res size Dba.FlgUnspecified;
      update_AF op1 op2 res size Sbb Dba.FlgUnspecified;
      update_PF res size Dba.FlgUnspecified;
      update_CF op1 op2 res size Sbb Dba.FlgUnspecified]
  | And ->
    [ update_OF op1 op2 res size And Dba.FlgUnspecified;
      update_SF res size Dba.FlgUnspecified;
      update_ZF res size Dba.FlgUnspecified;
      update_AF op1 op2 res size And Dba.FlgUnspecified;
      update_PF res size Dba.FlgUnspecified;
      update_CF op1 op2 res size And Dba.FlgUnspecified ]
  | Or ->
    [ update_OF op1 op2 res size Or Dba.FlgUnspecified;
      update_SF res size Dba.FlgUnspecified;
      update_ZF res size Dba.FlgUnspecified;
      update_AF op1 op2 res size Or Dba.FlgUnspecified;
      update_PF res size Dba.FlgUnspecified;
      clear_flag CF Dba.FlgUnspecified; ]
  | Xor ->
    [ update_OF op1 op2 res size Xor Dba.FlgUnspecified;
      update_SF res size Dba.FlgUnspecified;
      update_ZF res size Dba.FlgUnspecified;
      update_AF op1 op2 res size Xor Dba.FlgUnspecified;
      update_PF res size Dba.FlgUnspecified;
      clear_flag CF Dba.FlgUnspecified; ]


let affect_flags_shift op expr shift res' size =
  let open Dba_types in
  let res = getopt_or_fail ~in_function:"affect_flags_shift" res' in
  let shift =
    Expr.uext (Expr.restrict shift 0 4) (Basic_types.BitSize.create 32) in
  let sz = size - 1 in
  match op with
  | Shl ->
    let shift_minus_one = Expr.(shift_left expr (sub shift (cst_of_int 1 32))) in
    let inst_OF =
      if Dba_types.Expr.is_one shift then
        assign_flag OF (Expr.(logxor (restrict_to_bit res sz) cf_flag))
      else
        Predba.undefined (lhs_of_flag OF Dba.FlgUnspecified)  in
    [ inst_OF;
      update_SF res size Dba.FlgUnspecified;
      update_ZF res size Dba.FlgUnspecified;
      assign_flag CF (Expr.restrict_to_bit shift_minus_one sz); ]
  | Shr ->
    let shift_minus_one =
      Expr.shift_right expr (Expr.sub shift (cst_of_int 1 32)) in
    let inst_OF =
      if Dba_types.Expr.is_one shift then
        assign_flag OF (Expr.restrict_to_bit expr 0)
      else
        Predba.undefined (lhs_of_flag OF Dba.FlgUnspecified)  in
    [ inst_OF;
      update_SF res size Dba.FlgUnspecified;
      update_ZF res size Dba.FlgUnspecified;
      assign_flag CF (Expr.restrict_to_bit shift_minus_one 0);
    ]
  | Sar ->
    let shift_minus_one =
      Expr.shift_right_signed expr (Expr.sub shift (cst_of_int 1 32)) in
    let inst_OF =
      if Dba_types.Expr.is_one shift then assign_flag OF Expr.zero
      else Predba.undefined (lhs_of_flag OF Dba.FlgUnspecified)  in
    [ inst_OF;
      update_SF res size Dba.FlgUnspecified;
      update_ZF res size Dba.FlgUnspecified;
      assign_flag CF (Expr.restrict_to_bit shift_minus_one 0);
     ]

let affect_flags_rotate op _expr rop res' size =
  let open Dba_types in
  let res = getopt_or_fail ~in_function:"affect_flags_rotate" res' in
  let inst_OF =
    if Expr.is_one rop then
      let sz = size - 1 in
      assign_flag OF Expr.(logxor (restrict res sz sz) (cf_flag))
    else undef_flag OF
  in
  let assign_to_cf e = assign_flag CF e in
  match op with
  | Rol ->
    [ assign_to_cf (Expr.restrict_to_bit res 0);
      inst_OF;
      update_SF res size Dba.FlgUnspecified;
      update_ZF res size Dba.FlgUnspecified]
  | Ror ->
    [ assign_to_cf (Expr.restrict_to_bit res (size - 1));
      inst_OF;
      update_SF res size Dba.FlgUnspecified;
      update_ZF res size Dba.FlgUnspecified]
  | Rcl ->
    [ assign_to_cf (Expr.restrict_to_bit res size); inst_OF ]
  | Rcr ->
    [ inst_OF; assign_to_cf (Expr.restrict_to_bit res size); ]


let affect_flags_cmp op1 op2 res' size =
  let res = getopt_or_fail ~in_function:"affect_flags_cmp" res' in
  let inst = Dba.FlgCmp (op1, op2) in
  [ update_OF op1 op2 res size Sub inst;
    update_SF res size inst;
    update_ZF res size inst;
    update_AF op1 op2 res size Sub inst;
    update_PF res size inst;
    update_CF op1 op2 res size Sub inst ]

let affect_flags_test op1 op2 res' size =
  let res = getopt_or_fail ~in_function:"affect_flags_test" res' in
  let inst = Dba.FlgTest (op1, op2) in
  [ clear_flag OF inst;
    update_SF res size inst;
    Predba.undefined (lhs_of_flag AF inst);
    update_PF res size inst;
    update_ZF res size inst;
    clear_flag CF inst ]

let affect_flags_mul res mode =
  let open Dba_types in
  let res = getopt_or_fail ~in_function:"affect_flags_mul" res in
  let middle = size_mode mode in
  let res_res = Expr.restrict res middle (middle * 2 - 1) in
  [
    assign_flag OF (Expr.(diff res_res (zeros (size_mode mode))));
    undef_flag SF; undef_flag ZF;
    assign_flag CF of_flag; ]

let affect_flags_imul res mode =
  let res = getopt_or_fail ~in_function:"affect_flags_imul" res in
  let middle = size_mode mode in
  let extended_eax = Dba.ExprExtS (expr_of_reg mode EAX, middle * 2) in
  [ assign_flag OF (Dba_types.Expr.diff res extended_eax);
    undef_flag SF; undef_flag ZF;
    assign_flag CF of_flag;
  ]

let affect_flags_imul2_3 res' op2 op3 =
  let res = getopt_or_fail ~in_function:"affect_flags_imul2_3" res' in
  let sext64 e = Dba_types.Expr.sext e Basic_types.BitSize.bits64 in
  [ Predba.assign
        (Dba.LhsVar("temp64", 64, Some Dba.Temp))
        (Dba.ExprBinary(Dba.MultS, sext64 op2, sext64 op3));
    assign_flag OF
      (Dba.ExprBinary(
          Dba.Diff,
          Dba.ExprVar ("temp64", 64, Some Dba.Temp),
          sext64 res));
    undef_flag SF; undef_flag ZF;
    assign_flag CF of_flag; ]

let affect_flags_div = undef_flags [OF; SF; ZF; CF;]

let affect_flags_idiv = affect_flags_div

let affect_flags_neg op res' size =
  let res = getopt_or_fail ~in_function:"affect_flags_neg" res' in
  [ assign_flag OF
               (Dba.ExprBinary(Dba.Eq, op, maxi_bv size));
    update_SF res size Dba.FlgUnspecified;
    update_ZF res size Dba.FlgUnspecified;
    assign_flag CF (Dba.ExprBinary(Dba.Diff, op, Dba_types.Expr.zeros size)); ]


let affect_flags_ptest xmm size op1 op2 sreg =
  let open Dba_types.Expr in
  let e1 = disas_expr_xmm op1 xmm size sreg in
  let e2 = disas_expr_xmm op2 xmm size sreg in
  let v = constant (Bitvector.zeros 128) in
  let and_e1 = logand e1 in
  let c1 = eq (and_e1 e2) v in
  let c2 = eq (and_e1 (lognot e2)) v in
  [ clear_flag OF Dba.FlgUnspecified;
    clear_flag SF Dba.FlgUnspecified;
    assign_flag ZF c1;
    assign_flag CF c2;
  ]


let affect_flags_shiftd = undef_flags [ OF; SF; CF; ZF]

let affect_flags_aad res =
  [
    undef_flag OF;
    update_SF res 8 Dba.FlgUnspecified;
    update_ZF res 8 Dba.FlgUnspecified;
    undef_flag CF;
  ]


let affect_flags_bt mode base offset sreg =
  let open Dba_types in
  let op =
    match offset with
    | Reg _
    | Address _ ->
      let offset = disas_expr offset mode sreg in
      let op = Expr.shift_right base offset in
      Expr.restrict_to_bit op 0
    | Imm i ->
      let i = Int64.to_int i in Expr.restrict_to_bit base i
  in
  assign_flag CF op ::
  undef_flags [OF; SF; AF; PF;]


let affect_flags ins res sreg =
  let open Dba_types in
  match ins with
  | Arith(mode, op, gop1, gop2) ->
    affect_flags_arith op
      (disas_expr gop1 mode sreg) (disas_expr gop2 mode sreg) res (size_mode mode)
  | Aad _
  | Aam _ ->
    affect_flags_aad (Expr.restrict (Dba.ExprVar (X86Util.reg32_to_string EAX, 32, None)) 0 7)
  | Inc (mode, gop) ->
    affect_flags_inc (disas_expr gop mode sreg) res (size_mode mode)
  | Dec (mode, gop) ->
    affect_flags_dec (disas_expr gop mode sreg) res (size_mode mode)
  | Shift (mode, op, gop32, gop8) ->
    affect_flags_shift op (disas_expr gop32 mode sreg) (disas_expr8 gop8 sreg) res (size_mode mode)
  | Shiftd (_, _, _, _, _) -> affect_flags_shiftd
  | Rotate (mode, op, gop32, gop8) ->
    affect_flags_rotate op (disas_expr gop32) (disas_expr8 gop8 sreg) res (size_mode mode)
  | Cmp (mode, gop1, gop2) ->
    affect_flags_cmp (disas_expr gop1 mode sreg) (disas_expr gop2 mode sreg) res (size_mode mode)
  | Cmps mode ->
    let sreg = match sreg with None -> Some ES | _ -> sreg in
    let gop1 = expr_of_mem mode (esi_expr) ~sreg in
    let gop2 = expr_of_mem mode (edi_expr) ~sreg in
    affect_flags_cmp gop1 gop2 res (size_mode mode)
  | CmpXchg (mode, gop1, _gop2) ->
    affect_flags_cmp  (disas_expr gop1 mode sreg) (expr_of_reg mode EAX) res (size_mode mode)
  | Test (mode, gop1, gop2) ->
    affect_flags_test (disas_expr gop1 mode sreg) (disas_expr gop2 mode sreg) res (size_mode mode)
  | Cmc -> [ assign_flag CF (Expr.lognot (cf_flag)) ]
  | Clc -> [ assign_flag CF Expr.zero ]
  | Stc -> [ assign_flag CF Expr.one ]
  | Cld -> [ assign_flag DF Expr.zero ]
  | Std -> [ assign_flag DF Expr.one ]
  | Mul (mode, _) -> affect_flags_mul res mode
  | IMul (mode, _) -> affect_flags_imul res mode
  | IMul2 (mode, gop1, gop2) -> affect_flags_imul2_3 res (disas_expr gop1 mode sreg) (disas_expr gop2 mode sreg)
  | IMul3 (mode, _gop1, gop2, gop3) -> affect_flags_imul2_3 res (disas_expr gop2 mode sreg) (disas_expr gop3 mode sreg)
  | Div (_) -> affect_flags_div
  | IDiv (_) -> affect_flags_idiv
  | Neg (mode, gop) -> affect_flags_neg (disas_expr gop mode sreg) res (size_mode mode)
  | Scas mode ->
    let sreg = match sreg with None -> Some ES | _ -> sreg in
    affect_flags_cmp (expr_of_reg mode EAX) (expr_of_mem mode (edi_expr) ~sreg) res (size_mode mode)
  | Ptest (xmm, size, gop1, gop2) -> affect_flags_ptest xmm size gop1 gop2 sreg
  | Bt (mode, base, offset)
  | Bts (mode, base, offset)
  | Btr (mode, base, offset) ->
    affect_flags_bt mode (disas_expr base mode sreg) offset sreg
  | Popfd _
  | Pushfd _
  | Xchg _
  | SetCc (_, _) | Push _ | Pop _ | PushS _ | PopS _ | Not (_, _)
  | Movsx _ | Movsx16 _ | Movzx _ | Movzx16 _
  | MovQ _ | MovdQA _ | MovdQU _ | Movd _ | Movs _ | Lods _ | Stos _
  | CMovcc _ | Mov _ | MovSegRight _ |MovSegLeft _
  | Movaps _
  | Movlpd _ | Movlps _ | Movhlps _ | Movddup _
  | Movsldup _ | Palignr _ | CBW _ | CWD _ | PushA _
  | Pcmpeqb _ | Pcmpeqw _ | Pcmpeqd _
  | Pcmpgtb _ | Pcmpgtw _ | Pcmpgtd _
  | PmovMSKB _ | Pminub _ | Pmaxud _
  | Pmaxuw _ | Pmaxub _ | Pxor _ | Por _
  | Pand _ | Pandn _ | Punpcklbw _ | Punpcklwd _
  | Punpckldq _
  | Lea _ | DJmp _ | Jmp _ | Jcc _
  | DCall _ | Call _ | Bad | Unhandled | Halt
  | Ret | Reti _ | Retf | Retfi _ | Nop | Wait | Leave | Bsr _ | Bsf _
  | Bswap _ | Xadd _ | Jcxz _ | Pshufw _
  | Pshuflw _ | Pshufhw _ | Pshufd _
  | Movntq _ | Movhpd _ | Movhps _ | Movlhps _
  | Movshdup _
  | Psubb _
  | Psrlw _
  | Psrld _
  | Psrlq _
  | Loop _ | Loopz _ | Loopnz _
  | Popa _
  | Psllw _
  | Pslld _
  | Psllq _

  | Psraw _
  | Psrad _

  | CmpXchg8b _

  | Movupd _ | Movups _

  | Xlat _
  | Psrldq _
  | Pslldq _
  | Lsl _
  | Fld | Fxch _
  | Salc
  | Sahf
  | Lahf -> []

let pcmpeq gop1 gop2 xmm mm size sreg =
  let rec aux acc i j base_idx bound =
    if j >= bound then acc
    else
      let restrict_genop gop =
        Dba.ExprRestrict (disas_expr_xmm gop xmm mm sreg, i, j) in
      let l1 n =
        [ Predba.jif
            (Dba.CondReif
               (Dba_types.Expr.eq (restrict_genop gop1) (restrict_genop gop2)))
            (Dba.JInner (base_idx + n))
        ]
      in
      let aux_assign cst = assign_xmm_expr gop1 i j cst xmm mm sreg in
      let l2 = maxi_bv size |> aux_assign in
      let l3 = Dba_types.Expr.zeros size |> aux_assign in
      let l2len = List.length l2 in
      let l3len = List.length l3 in
      let jump_id = base_idx + l2len + l3len + 2 in
      let l = l1 (l2len + 2) @ l2
              @ ( Predba.static_jump (Dba.JInner jump_id) :: l3)
      in aux (acc @ l) (i + size) (j + size) jump_id bound
  in aux [] 0 (size - 1) 0 (X86Util.bitsize_of_xmm_mm xmm)


let pmovMSK gop1 gop2 xmm mm sreg =
  let rec aux acc i bound =
    if i >= bound then List_utils.rev_flatten acc
    else
      let v = 8 * i + 7 in
      let l = assign_expr_xmm gop1 i i gop2 v v xmm mm sreg in
      aux (l :: acc) (i + 1) bound
  in
  let assign_expr lo hi =
    assert (lo < hi);
    assign_expr_expr gop1 lo hi (Dba_types.Expr.zeros (1 + hi - lo)) xmm mm sreg
  in
  let start = X86Util.bytesize_of_xmm_mm xmm in
  (aux [] 0 start) @ assign_expr start 31


let pminu gop1 gop2 xmm mm size sreg =
  let rec aux acc i j base_idx bound =
    if j >= bound then acc
    else
      let restrict_genop gop =
        Dba.ExprRestrict(disas_expr_xmm gop xmm mm sreg, i, j) in
      let l1 n =
        [ Predba.jif
            (Dba.CondReif (
                Dba_types.Expr.ult (restrict_genop gop1) (restrict_genop gop2)))
            (Dba.JInner (base_idx + n)) ]
      in
      let l2 = assign_xmm gop1 i j gop2 i j xmm mm sreg in
      let len2 = List.length l2 + 1 in
      let l = (l1 len2) @ l2 in
      aux (acc @ l) (i+size) (j+size) (base_idx + len2)  bound
  in aux [] 0 7 0 (X86Util.bitsize_of_xmm_mm xmm)


let repeat_instrs l rep =
  match rep with
  | NoRep -> l
  | _ ->
    let size = (List.length l) + 3 in
    let ecx = expr_of_reg32 ECX in
    let pre_l =
      Predba.jif (
        Dba.CondReif (Dba_types.Expr.eq ecx (Dba_types.Expr.zeros 32)))
        (Dba.JInner size)  in
    let post_l = [
      Predba.assign (lhs_of_reg32 ECX) (Dba_types.Expr.sub ecx (cst_of_int 1 32));
      Predba.static_jump (Dba.JInner 0) ]
    in
    let l =
      List.map (
        fun elem ->
          match elem with
          | Predba.If(cond, Dba.JInner a) ->
            Predba.jif cond (Dba.JInner (a + 1))
          | Predba.SJump(Dba.JInner a, tag) ->
            Predba.static_jump (Dba.JInner (a + 1)) ~tag
          | _ -> elem
      ) l in
    pre_l :: l @ post_l


let mk_temp base size = base^string_of_int size, size, Some Dba.Temp

let mk_lhs_temp base size =
  let name, sz, tag = mk_temp base size in Dba.LhsVar(name, sz, tag)

let mk_res_temp base size =
  let name, sz, tag = mk_temp base size in Dba.ExprVar(name, sz, tag)


let res_lhs mode = mk_lhs_temp "res" (size_mode mode)
let double_res_lhs mode = mk_lhs_temp "res" (2 * size_mode mode)
let temp_lhs mode = mk_lhs_temp "temp" (size_mode mode)
let double_temp_lhs mode = mk_lhs_temp "temp" (2 * size_mode mode)


let res_expr mode = mk_res_temp "res" (size_mode mode)
let double_res_expr mode = mk_res_temp "res" (2 * size_mode mode)
let temp_expr mode = mk_res_temp "temp" (size_mode mode)
let double_temp_expr mode =  mk_res_temp "temp" (2 * size_mode mode)


let decode_push mode genop sreg =
  let mem =
    Dba_types.Expr.sub esp_expr (cst_of_int (nbytes_mode mode) 32) in
  [ Predba.assign (lhs_of_mem mode mem) (disas_expr genop mode sreg);
    Predba.assign esp_lval mem
  ]

let decode_pushS reg _sreg =
  let e = Dba_types.Expr.sub esp_expr (cst_of_int 2 32) in
  [ Predba.assign (lhs_of_mem16 e) (expr_of_seg reg);
    Predba.assign esp_lval e;
  ]

let decode_pushA mode sreg =
  let push_dec reg = decode_push mode (Reg reg) sreg in
  Predba.assign (temp_lhs mode) (expr_of_reg mode ESP) ::
  List_utils.flat_map push_dec [EAX; ECX; EDX; EBX;] @
  let esp = esp_expr in
  let v = cst_of_int (nbytes_mode mode) 32 in
  let e = Dba_types.Expr.sub esp v in
  [ Predba.assign (lhs_of_mem mode e) (temp_expr mode);
    Predba.assign esp_lval e;
  ] @
  List_utils.flat_map push_dec [EBP; ESI; EDI;]


let decode_pop mode genop sreg =
  let open Dba_types in
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in  *)
  [ Predba.assign (disas_lval genop mode sreg) (expr_of_mem mode esp_expr);
    Predba.assign esp_lval
      (Expr.add esp_expr (cst_of_int (nbytes_mode mode) 32))  ]


let decode_popS reg _sreg =
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in  *)
  [ Predba.assign (lhs_of_seg reg) (expr_of_mem16 esp_expr);
    Predba.assign esp_lval (Dba_types.Expr.add esp_expr (cst_of_int 2 32)) ]


let decode_popa mode sreg =
  let word_size = (* in byte *) nbytes_mode mode in
  let aux_popa (n, register) =
    let lval = disas_lval (Reg register) mode sreg in
    let byte_offset = cst_of_int (n * word_size) 32 in
    let rval = Dba_types.Expr.add esp_expr byte_offset |> expr_of_mem ~sreg mode in
    Predba.assign lval rval
  in
  List.map aux_popa [0, EDI; 1, ESI; 2, EBP; 4, EBX; 5, EDX; 6, ECX; 7, EAX;]
  @ [ Predba.assign esp_lval
        (Dba_types.Expr.add esp_expr ((cst_of_int (8 * word_size) 32)))]


let decode_arith mode op gop1 gop2 sreg =
  let open Dba_types in
  let disassed_gop1 = disas_expr gop1 mode sreg in
  let disassed_gop2 = disas_expr gop2 mode sreg in
  let add_carry e =
    Expr.add e (Dba.ExprExtU (cf_flag, size_mode mode)) in
  let rhs =
    match op with
    | Add -> Expr.add disassed_gop1 disassed_gop2
    | Adc ->
      let dba_gop2_carry = add_carry disassed_gop2 in
      Expr.add disassed_gop1 dba_gop2_carry
    | Sub -> Expr.sub disassed_gop1 disassed_gop2
    | Sbb ->
      let dba_gop2_carry = add_carry disassed_gop2 in
      Expr.sub disassed_gop1 dba_gop2_carry
    | And -> Expr.logand disassed_gop1 disassed_gop2
    | Or ->  Expr.logor disassed_gop1 disassed_gop2
    | Xor -> Expr.logxor disassed_gop1 disassed_gop2
  in
  let ins = Arith (mode, op, gop1, gop2) in
  Predba.assign (res_lhs mode) rhs ::
  affect_flags ins (Some (res_expr mode)) sreg @
  [ Predba.assign (disas_lval gop1 mode sreg) (res_expr mode) ]


let decode_shift mode shift_op gop32 gop8 sreg =
  let open Dba_types in
  let size = size_mode mode in
  let ins = Shift (mode, shift_op, gop32, gop8) in
  let shift = Dba.ExprExtU (Expr.restrict (disas_expr8 gop8 sreg) 0 4, size) in
  let dba_op =
    match shift_op with
    | Shl -> Expr.shift_left
    | Shr -> Expr.shift_right
    | Sar -> Expr.shift_right_signed
  in
  Predba.assign (res_lhs mode) (dba_op (disas_expr gop32 mode sreg) shift) ::
  affect_flags ins (Some (res_expr mode)) sreg @
  [ Predba.assign(disas_lval gop32 mode sreg) (res_expr mode) ]


let decode_rotate mode rotate_op gop32 gop8 sreg =
  let open Dba_types in
  let size = size_mode mode in
  let sz = size + 1 in
  let tmp = temp_size sz in
  let res_lhs_1 = Dba.LhsVar (tmp, sz, Some Dba.Temp) in
  let res_expr_1 = Dba.ExprVar (tmp, sz, Some Dba.Temp) in
  let ins = Rotate (mode, rotate_op, gop32, gop8) in
  (* let res33_expr = Dba.ExprVar ("res33", 33, Some Dba.Temp) in *)
  let dba_op, dba_gop1, dba_gop2, size =
    match rotate_op with
    | Rol ->
      Dba.LeftRotate,  disas_expr gop32 mode sreg, res_expr mode, size
    | Rcl ->
      let tmp = Expr.append cf_flag (disas_expr gop32 mode sreg) in
      Dba.LeftRotate, tmp, Expr.restrict res_expr_1 0 (size - 1), sz
    | Ror -> Dba.RightRotate, disas_expr gop32 mode sreg, res_expr mode, size
    | Rcr ->
      let tmp = Expr.append cf_flag (disas_expr gop32 mode sreg) in
      Dba.RightRotate, tmp, Expr.restrict res_expr_1 0 (size - 1), sz
  in
  let rop = Dba.ExprExtU(Expr.restrict (disas_expr8 gop8 sreg) 0 4, size) in
  match rotate_op with
  | Rol | Ror ->
    Predba.assign (res_lhs mode) (Dba.ExprBinary (dba_op, dba_gop1, rop))
    :: affect_flags ins (Some (res_expr mode)) sreg
    @ [ Predba.assign (disas_lval gop32 mode sreg) (dba_gop2) ]
  | Rcl | Rcr ->
    Predba.assign (res_lhs_1) (Dba.ExprBinary (dba_op, dba_gop1, rop))
    :: affect_flags ins (Some (res_expr_1)) sreg
    @ [ Predba.assign (disas_lval gop32 mode sreg) (dba_gop2) ]


let decode_shiftd mode shift_op src dst gop8 sreg =
  let open Dba_types in
  let size = size_mode mode in
  let ins = Shiftd (mode, shift_op, src, dst, gop8) in
  let count = Dba.ExprExtU (Expr.umod (disas_expr8 gop8 sreg) (cst_of_int 32 8), size) in
  let esrc = disas_expr src mode sreg in
  let edst = disas_expr dst mode sreg in
  let ldst = disas_lval dst mode sreg in
  let dba_op1, dba_op2 =
    match shift_op with
    | Shld -> Expr.shift_left, Expr.shift_right
    | Shrd -> Expr.shift_right, Expr.shift_left
  in
  let tmp = dba_op2 esrc (Expr.sub (cst_of_int size size) count) in
  let jump10 = Dba.JInner 10 in
  affect_flags ins None sreg @
  [ Predba.jif (Dba.CondReif (Expr.eq count (Expr.zeros size))) jump10;
    Predba.jif (Dba.CondReif (Expr.ule count (cst_of_int size size))) (Dba.JInner 8);
    Predba.undefined ldst;
    Predba.static_jump jump10;
    Predba.assign (ldst) (dba_op1 edst count);
    Predba.assign (ldst) (Expr.logor edst tmp);
  ]


let decode_cmp mode gop1 gop2 sreg =
  let open Dba_types in
  let ins = Cmp (mode, gop1, gop2) in
  let rhs = Expr.sub (disas_expr gop1 mode sreg) (disas_expr gop2 mode sreg) in
  Predba.assign (res_lhs mode) (rhs) :: affect_flags ins (Some (res_expr mode)) sreg


let decode_cmps mode sreg =
  let open Dba_types in
  let gop1 = expr_of_mem mode (esi_expr) ~sreg in
  let gop2 = expr_of_mem mode (edi_expr) ~sreg in
  let cst = cst_of_int (nbytes_mode mode) 32 in
  [ Predba.assign (res_lhs mode) (Expr.sub gop1 gop2);
    if_df (Dba.JInner 5);
    Predba.assign (esi_lval) (Expr.add (esi_expr) cst);
    Predba.assign (edi_lval) (Expr.add (edi_expr) cst);
    Predba.static_jump (Dba.JInner 7);
    Predba.assign (esi_lval) (Expr.sub (esi_expr) cst);
    Predba.assign (edi_lval) (Expr.sub (edi_expr) cst);
  ] @
  affect_flags (Cmps mode) (Some (res_expr mode)) sreg


let decode_cmpXchg mode gop1 gop2 sreg =
  let ins = CmpXchg (mode, gop1, gop2) in
  let eax_expr = expr_of_reg mode EAX in
  let e1 = disas_expr gop1 mode sreg in
  let open Dba_types in
  Predba.assign (res_lhs mode) (Expr.sub e1 eax_expr) ::
  affect_flags ins (Some (res_expr mode)) sreg @
  [ Predba.jif (Dba.CondReif (Expr.eq eax_expr e1)) (Dba.JInner 10);
    Predba.assign (lhs_of_reg EAX mode) e1;
    Predba.static_jump (Dba.JInner 11);
    Predba.assign (disas_lval gop1 mode sreg) (disas_expr gop2 mode sreg)
  ]


let decode_test mode gop1 gop2 sreg =
  let ins = Test (mode, gop1, gop2) in
  let rhs = Dba_types.Expr.logand
      (disas_expr gop1 mode sreg) (disas_expr gop2 mode sreg) in
  Predba.assign (res_lhs mode) (rhs) ::
  affect_flags ins (Some (res_expr mode)) sreg


let decode_movd xmm pos gop1 gop2 sreg =
  match pos, xmm with
  | Left, MM ->
    assign_xmm_expr gop1 0 31 (disas_expr gop2 `M32 sreg) xmm S32 sreg @
    assign_xmm_expr gop1 32 63 (Dba_types.Expr.zeros 32) xmm S32 sreg
  | Left, XMM ->
    assign_xmm_expr gop1 0 31 (disas_expr gop2 `M32 sreg) xmm S32 sreg @
    assign_xmm_expr gop1 32 127 (Dba_types.Expr.zeros 96) xmm S32 sreg
  | Right, _ ->
    [ Predba.assign (disas_lval gop2 `M32 sreg)
        (Dba.ExprRestrict (disas_expr_xmm gop1 xmm S32 sreg, 0, 31)) ]


let decode_movs mode rep sreg =
  let open Dba_types in
  let mk_rhs_reg ereg =
    let cst = cst_of_int (nbytes_mode mode) 32 in
    Expr.ite (Dba.CondReif (expr_of_flag DF))
      (Expr.sub ereg cst) (Expr.add ereg cst) in
  let tmp_esi = mk_rhs_reg esi_expr in
  let tmp_edi = mk_rhs_reg edi_expr in
  let esi_reg = sreg (* Segment register for ESI can be overriden : see 3-489 *)
  and edi_reg = Some X86Types.ES in (* Segment register for EDI is fixed : see 3-489 *)
  let l = [
    Predba.assign (lhs_of_mem mode edi_expr ~sreg:edi_reg) (expr_of_mem mode esi_expr ~sreg:esi_reg);
    Predba.assign esi_lval tmp_esi;
    Predba.assign edi_lval tmp_edi;
  ]
  in repeat_instrs l rep


let decode_lods mode rep sreg =
  let cst = cst_of_int (nbytes_mode mode) 32 in
  let l = [
    Predba.assign(lhs_of_reg EAX mode) (expr_of_mem mode esi_expr ~sreg);
    if_df (Dba.JInner 4);
    Predba.assign esi_lval (Dba.ExprBinary(Dba.Plus, esi_expr, cst));
    Predba.static_jump (Dba.JInner 5);
    Predba.assign esi_lval (Dba.ExprBinary(Dba.Minus, esi_expr, cst));
  ]
  in repeat_instrs l rep



let decode_stos mode rep sreg =
  let open Dba_types in
  let cst = cst_of_int (nbytes_mode mode) 32 in
  let l = [
    Predba.assign (lhs_of_mem mode edi_expr ~sreg) (expr_of_reg mode EAX);
    if_df (Dba.JInner 4);
    Predba.assign(edi_lval) (Expr.add edi_expr cst);
    Predba.static_jump (Dba.JInner 5) ;
    Predba.assign(edi_lval) (Expr.sub edi_expr cst);
  ]
  in repeat_instrs l rep


let decode_scas mode rep sreg =
  let open Dba_types in
  let sreg = match sreg with None -> Some ES | _ -> sreg in
  let ins = Scas mode in
  let cst = cst_of_int (nbytes_mode mode) 32 in
  let cmp_l =
    Predba.assign(res_lhs mode)
      (Expr.sub (expr_of_reg mode EAX) (expr_of_mem mode edi_expr ~sreg)) ::
    affect_flags ins (Some (res_expr mode)) sreg
  in
  let l = [
    Predba.assign edi_lval
      (Dba.ExprIte (Dba.CondReif (expr_of_flag DF),
                    Expr.sub edi_expr cst,
                    Expr.add edi_expr cst
                   ));
  ]
  in repeat_instrs (cmp_l @ l) rep


let decode_cmovcc mode cc gop1 gop2 _ sreg =
  let cond = cond_of_cc cc in
  let lhs = disas_lval gop1 mode sreg in
  let new_expr = disas_expr gop2 mode sreg in
  let old_expr = disas_expr gop1 mode sreg in
  [ Predba.assign lhs (Dba.ExprIte (cond, new_expr, old_expr)) ]


let decode_movsldup mm gop1 gop2 sreg =
  assign_xmm gop1 0  31  gop2 0  31 XMM mm sreg @
  assign_xmm gop1 32 63  gop2 0  31 XMM mm sreg @
  assign_xmm gop1 64 95  gop2 64 95 XMM mm sreg @
  assign_xmm gop1 96 127 gop2 64 95 XMM mm sreg


let decode_palignr xmm mm gop1 gop2 imm sreg =
  let open Dba_types in
  let size1, size2 =
    match xmm with
    | MM -> 128, 63
    | XMM -> 256, 127
  in
  let nbits = Basic_types.BitSize.create size1 in
  let lval = LValue.temp nbits in
  let rval = Expr.temp nbits in
  Predba.assign lval
    (Expr.append
       (disas_expr_xmm gop1 xmm mm sreg) (disas_expr_xmm gop2 xmm mm sreg))
  ::
  Predba.assign lval (Expr.shift_right rval (cst_of_int (imm * 8) 32))
  ::
  assign_xmm_expr gop2 0 size2 (Expr.restrict rval 0 size2) xmm mm sreg


let decode_leave _sreg =
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in *)
  [ Predba.assign esp_lval (expr_of_reg32 EBP);
    Predba.assign (lhs_of_reg32 EBP) (expr_of_mem32 esp_expr); (* todo: check if ESP should be on 16bits *)
    Predba.assign esp_lval (Dba_types.Expr.add esp_expr four_32) ]


let decode_call src nextaddr _sreg =
  let open Dba_types in
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in *)
  [ Predba.assign esp_lval (Expr.sub esp_expr four_32);
    Predba.assign (lhs_of_mem32 esp_expr) (Expr.constant (nextaddr.Dba.base));
    let tag = Some (Dba.Call nextaddr) in
    Predba.static_jump (strange_addr_of_int64 src) ~tag ]


let decode_dcall gop nextaddr sreg =
    let open Dba_types in
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in *)
  [ Predba.assign (esp_lval) (Expr.sub esp_expr four_32);
    Predba.assign (lhs_of_mem32 esp_expr) (Expr.constant (nextaddr.Dba.base));
    Predba.dynamic_jump (disas_expr gop `M32 sreg)
      ~tag:(Some (Dba.Call nextaddr))
  ]


let decode_ret _sreg =
  [ Predba.assign (esp_lval) (Dba.ExprBinary(Dba.Plus, esp_expr, four_32));
    Predba.dynamic_jump (expr_of_mem32 (Dba_types.Expr.sub esp_expr four_32))
      ~tag:(Some Dba.Return)]

let decode_reti imm _sreg =
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in *)
  let v = cst_of_int (4 + imm) 32 in
  [ Predba.assign (esp_lval) (Dba_types.Expr.add esp_expr v);
    Predba.dynamic_jump (expr_of_mem32 (Dba_types.Expr.sub esp_expr v))
      ~tag:(Some Dba.Return)]

let decode_retf _sreg =
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in   *)
  let open Dba_types in
  [ Predba.assign esp_lval (Expr.add esp_expr four_32);
    Predba.assign (lhs_of_seg CS) (expr_of_mem16 esp_expr);
    Predba.assign esp_lval (Expr.add esp_expr (cst_of_int 2 32));
    Predba.dynamic_jump
      (expr_of_mem32 (Expr.sub esp_expr (cst_of_int 6 32)))
      ~tag:(Some Dba.Return)]

let decode_retfi imm _sreg =
  let open Dba_types in
  let add_esp = Expr.add esp_expr in
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in *)
  [ Predba.assign (esp_lval) (add_esp (cst_of_int (4 + imm) 32));
    Predba.assign (lhs_of_seg CS) (expr_of_mem16 esp_expr);
    Predba.assign (esp_lval) (add_esp (cst_of_int 2 32));
    Predba.dynamic_jump
      (expr_of_mem32 (Expr.sub esp_expr (cst_of_int (6 + imm) 32)))
      ~tag:(Some Dba.Return)]


let decode_not mode gop sreg =
  (* let mask =  *)
  (*   match mode with  *)
  (*   | `M32 -> cst_of_int64_32 (Int64.of_string "0xffffffff") *)
  (*   | `M16 -> cst_of_int64_16 (Int64.of_string "0xffff")  *)
  (*   | `M8 -> cst_of_int64_8 (Int64.of_string "0xff")  *)
  (* in *)
  [ Predba.assign (disas_lval gop mode sreg)
      (Dba_types.Expr.lognot (disas_expr gop mode sreg)) ]


let decode_neg mode gop sreg =
  let ins = Neg (mode, gop) in
  Predba.assign (res_lhs mode) (Dba_types.Expr.uminus (disas_expr gop mode sreg))
  ::
  affect_flags ins (Some (res_expr mode)) sreg
  @
  [ Predba.assign(disas_lval gop mode sreg) ((res_expr mode)) ]


let decode_inc mode gop sreg =
  let ins = Inc (mode, gop) in
  let open Dba_types in
  let rmode = res_expr mode in
  Predba.assign(res_lhs mode)
(            Expr.add (disas_expr gop mode sreg)
              (Expr.ones (size_mode mode)))
  :: affect_flags ins (Some rmode) sreg
  @ [ Predba.assign(disas_lval gop mode sreg) (rmode) ]


let decode_dec mode gop sreg =
  let ins = Dec (mode, gop) in
  let rmode = res_expr mode in
  let open Dba_types in
  Predba.assign(res_lhs mode)
(            Expr.sub (disas_expr gop mode sreg)
              (Expr.ones (size_mode mode)))
  :: affect_flags ins (Some rmode) sreg
  @ [ Predba.assign(disas_lval gop mode sreg) (rmode) ]


let decode_xchg mode gop1 gop2 sreg =
  [ Predba.assign(temp_lhs mode) (disas_expr gop1 mode sreg);
    Predba.assign(disas_lval gop1 mode sreg) (disas_expr gop2 mode sreg);
    Predba.assign(disas_lval gop2 mode sreg) (temp_expr mode) ]


let decode_mul mode gop sreg =
  let open Dba_types in
  let ins = Mul (mode, gop) in
  let mode_size = size_mode mode in
  let drexpr = double_res_expr mode
  and drlval = double_res_lhs mode in
  match mode with
  | `M32 ->
    Predba.assign drlval
        (Expr.umul (Dba.ExprExtU(expr_of_reg32 EAX, 64))
           (Dba.ExprExtU(disas_expr gop mode sreg, 64)))
    ::
    Predba.assign (lhs_of_reg EAX mode)
      (Expr.restrict drexpr 0 (mode_size - 1))
    ::
    Predba.assign (lhs_of_reg EDX mode)
      (Expr.restrict drexpr mode_size (mode_size * 2 -1))
    :: affect_flags ins (Some drexpr) sreg

  | `M16 ->
    [ Predba.assign drlval
        (Expr.umul
           (Dba.ExprExtU(expr_of_reg mode EAX, 32))
           (Dba.ExprExtU(disas_expr gop mode sreg, 32)));
      Predba.assign(lhs_of_reg EAX mode) (Expr.restrict drexpr 0 (mode_size - 1));
      Predba.assign(lhs_of_reg EDX mode)
        (Expr.restrict drexpr mode_size (mode_size * 2 - 1)) ] @
    affect_flags ins (Some drexpr) sreg

  | `M8 ->
    [ Predba.assign drlval
        (Expr.umul (Dba.ExprExtU(expr_of_reg mode EAX, 16))
           (Dba.ExprExtU(disas_expr gop mode sreg, 16)));
      Predba.assign(lhs_of_reg EAX `M16) (drexpr) ] @
    affect_flags ins (Some drexpr) sreg


let decode_imul mode gop sreg =
  let ins = IMul (mode, gop) in
  let size = size_mode mode * 2 in
  let drlval = double_res_lhs mode in
  let drrval = double_res_expr mode in
  let nbits = Basic_types.BitSize.create size in
  let szmode = size_mode mode in
  let open Dba_types in
  match mode with
  | `M8 ->
    Predba.assign drlval
      (Expr.smul (Expr.sext (expr_of_reg mode EAX) nbits)
         (Expr.sext (disas_expr gop mode sreg) nbits))
    :: Predba.assign (lhs_of_reg EAX `M16) (drrval)
    :: affect_flags ins (Some drrval) sreg
  | _ ->
    Predba.assign drlval
      (Expr.smul (Expr.sext (expr_of_reg mode EAX) nbits)
         (Expr.sext (disas_expr gop mode sreg) nbits))
    :: Predba.assign (lhs_of_reg EAX mode) (Expr.restrict drrval 0 (szmode - 1))
    :: Predba.assign (lhs_of_reg EDX mode) (Expr.restrict drrval szmode (size - 1))
    :: affect_flags ins (Some drrval) sreg


let decode_imul2 mode gop1 gop2 sreg =
  let ins = IMul2 (mode, gop1, gop2) in
  let drrval = res_expr mode in
  let rval =
    Dba_types.Expr.smul (disas_expr gop1 mode sreg) (disas_expr gop2 mode sreg) in
  Predba.assign (res_lhs mode) rval
  :: affect_flags ins (Some drrval) sreg
  @ [ Predba.assign (disas_lval gop1 mode sreg) (drrval) ]


let decode_imul3 mode gop1 gop2 gop3 sreg =
  let ins = IMul3 (mode, gop1, gop2, gop3) in
  let drrval = res_expr mode in
  Predba.assign (res_lhs mode)
    (Dba_types.Expr.smul (disas_expr gop2 mode sreg) (disas_expr gop3 mode sreg))
  :: affect_flags ins (Some drrval) sreg
  @ [ Predba.assign(disas_lval gop1 mode sreg) (drrval) ]

let decode_div mode gop sreg =
  let open Dba_types in
  let size = size_mode mode in
  let ins = Div (mode, gop) in
  let drexpr = double_res_expr mode in
  let dtexpr = double_temp_expr mode in
  let ereg = expr_of_reg mode in
  let double_size = 2 * size in
  let disassed_gop = disas_expr gop mode sreg in
  let ue = Expr.uext disassed_gop (Basic_types.BitSize.create double_size) in
  let re = Expr.restrict drexpr 0 (size -1) in
  [ Predba.assign (double_temp_lhs mode) (Expr.append (ereg EDX) (ereg EAX));
    Predba.assign (double_res_lhs mode) (Expr.udiv dtexpr ue);
    Predba.jif (Dba.CondReif
                  Expr.(eq (restrict drexpr size (double_size - 1)) (zeros size)))
      (Dba.JInner 4);
    Predba.stop Dba.KO;
    Predba.assign (lhs_of_reg EAX mode) (re);
    Predba.assign (double_temp_lhs mode) (Expr.umod dtexpr ue);
    Predba.assign (lhs_of_reg EDX mode) (re);
  ] @ affect_flags ins (Some drexpr) sreg


let decode_idiv mode gop sreg =
  let open Dba_types in
  let size = size_mode mode in
  let ins = IDiv (mode, gop) in
  let dllval = double_temp_lhs mode in
  let drlval = double_res_lhs mode in
  let nbits = Basic_types.BitSize.create (2 * size) in
  let e1 = disas_expr gop mode sreg in
  [ Predba.assign dllval
      (Expr.append (expr_of_reg mode EDX) (expr_of_reg mode EAX));
    Predba.assign drlval
      (Expr.sdiv (double_temp_expr mode) (Expr.sext e1 nbits));
    Predba.assign (temp_lhs mode) (Dba.ExprRestrict(double_res_expr mode, size, (size * 2 -1)));
    Predba.jif (Dba.CondReif (Expr.eq (temp_expr mode) (Expr.zeros size)))
      (Dba.JInner 5);
    Predba.stop Dba.KO;
    Predba.assign (lhs_of_reg EAX mode) (Dba.ExprRestrict(double_res_expr mode, 0, size - 1));
    Predba.assign (double_temp_lhs mode)
      (Expr.smod (double_temp_expr mode) (Dba.ExprExtS(e1, size * 2)));
    Predba.assign (lhs_of_reg EDX mode) (Dba.ExprRestrict(double_temp_expr mode, 0,
                                                    size - 1));
  ] @ affect_flags ins (Some (double_res_expr mode)) sreg


let decode_cbw mode =
  match mode with
  | `M32 -> [assign_register EAX mode (Dba.ExprExtS (expr_of_reg `M16 EAX, 32))]
  | `M16 -> [assign_register EAX mode (Dba.ExprExtS (expr_of_reg `M8  EAX, 16))]


let decode_cwd mode =
  let open Dba_types in
  let size, name =
    match mode with
    | `M32 -> 64, "temp64"
    | `M16 -> 32, "temp32"
  in
  let temp_lhs = Dba.LhsVar  (name, size, Some Dba.Temp) in
  let temp_exp = Dba.ExprVar (name, size, Some Dba.Temp) in
  [ Predba.assign (temp_lhs) (Dba.ExprExtS (expr_of_reg mode EAX, size));
    assign_register EDX mode (Expr.restrict temp_exp (size / 2) (size - 1));
  ]


let decode_bsr mode dst src sreg =
  let open Dba_types in
  let src_exp = disas_expr src mode sreg in
  let dst_lhs = lhs_of_reg dst mode in
  let size =
    match mode with
    | `M32 -> 31
    | `M16 -> 15
    | `M8 -> assert false
  in
  let sz = size + 1 in
  let tmp = temp_size sz in
  let cpt = cpt_size sz in
  let temp_lhs = Dba.LhsVar(tmp, sz, Some Dba.Temp) in
  let temp_exp = Dba.ExprVar(tmp, sz, Some Dba.Temp) in
  let cpt_lhs = Dba.LhsVar(cpt, sz, Some Dba.Temp) in
  let cpt_exp = Dba.ExprVar(cpt, sz, Some Dba.Temp) in
  let extone = cst_of_int 1 sz in
  [ Predba.jif (Dba.CondReif (Expr.diff src_exp (Expr.zeros sz))) (Dba.JInner 4);
    assign_flag ZF (cst_of_int 1 1);
    Predba.undefined dst_lhs;
    Predba.static_jump (Dba.JInner 12);
    assign_flag ZF Expr.zero;
    Predba.assign temp_lhs src_exp;
    Predba.assign cpt_lhs (cst_of_int size (size + 1));
    Predba.jif
      (Dba.CondReif Expr.(eq (restrict_to_bit temp_exp size) bool_true))
      (Dba.JInner 11);
    Predba.assign temp_lhs (Expr.shift_left temp_exp extone);
    Predba.assign cpt_lhs (Expr.sub cpt_exp extone);
    Predba.static_jump (Dba.JInner 7);
    Predba.assign dst_lhs cpt_exp;
  ] @ undef_flags [CF; OF; SF;]


let decode_bsf mode dst src sreg =
  let src_exp = disas_expr src mode sreg in
  let dst_lhs = lhs_of_reg dst mode in
  let size =
    match mode with
    | `M32 -> 31
    | `M16 -> 15
    | `M8 -> assert false
  in
  let open Dba_types in
  let sz = size + 1 in
  let tmp = temp_size sz in
  let cpt = cpt_size sz in
  let temp_lhs = Dba.LhsVar(tmp, sz, Some Dba.Temp) in
  let temp_exp = Dba.ExprVar(tmp, sz, Some Dba.Temp) in
  let cpt_lhs = Dba.LhsVar(cpt, sz, Some Dba.Temp) in
  let cpt_exp = Dba.ExprVar(cpt, sz, Some Dba.Temp) in

  [ Predba.jif
      (Dba.CondReif (Dba_types.Expr.diff src_exp (Expr.zeros sz)))
      (Dba.JInner 4);
    Predba.assign (lhs_of_flag ZF Dba.FlgUnspecified) (cst_of_int 1 1);
    Predba.undefined dst_lhs;
    Predba.static_jump (Dba.JInner 12);
    Predba.assign (lhs_of_flag ZF Dba.FlgUnspecified) (Expr.bool_false);
    Predba.assign temp_lhs (src_exp);
    Predba.assign cpt_lhs (Expr.zeros sz);
    Predba.jif
      (Dba.CondReif Expr.(eq (restrict_to_bit temp_exp 0) bool_true))
      (Dba.JInner 11);
    Predba.assign temp_lhs (Dba.ExprBinary (Dba.RShiftU, temp_exp, cst_of_int 1 sz));
    Predba.assign cpt_lhs (Expr.add  cpt_exp (cst_of_int 1 sz));
    Predba.static_jump (Dba.JInner 6);
    Predba.assign dst_lhs cpt_exp;
  ] @ undef_flags [CF; OF; SF;]



let decode_bswap mode dst =
  let size = Basic_types.BitSize.to_int (X86Util.bitsize_of_mode mode) in
  let tmp = temp_size size in
  let temp_lhs = Dba.LhsVar(tmp, size, Some Dba.Temp) in
  let temp_exp = Dba.ExprVar(tmp, size, Some Dba.Temp) in
  let dst_exp = expr_of_reg mode dst in
  let rec assign acc dst temp off1 off2 =
    if off1 + 7 < size && off2 - 7 >= 0 then
      Predba.assign
        (Dba.LhsVarRestrict (dst, 32, off1, off1 + 7))
        (Dba.ExprRestrict (temp, off2 - 7, off2)) ::
      assign acc dst temp (off1 + 8) (off2 - 8)
    else acc
  in
  Predba.assign temp_lhs dst_exp ::
  assign [] (X86Util.reg32_to_string dst) temp_exp 0 (size - 1)


let decode_xadd mode gop1 gop2 sreg =
  let dba_gop1, dba_gop2 =
    disas_expr gop1 mode sreg, disas_expr gop2 mode sreg in
  let ins = Arith (mode, Add, gop1, gop2) in
  Predba.assign (res_lhs mode) (Dba_types.Expr.add dba_gop1 dba_gop2)
  :: affect_flags ins (Some (res_expr mode)) sreg @
  [ Predba.assign (disas_lval gop2 mode sreg) (disas_expr gop1 mode sreg);
    Predba.assign (disas_lval gop1 mode sreg) (res_expr mode) ]


let decode_jcxz mode src =
  let size = size_mode mode in
  [ Predba.jif
      (Dba.CondReif Dba_types.Expr.(eq (expr_of_reg mode ECX) (zeros size)))
      (strange_addr_of_int64 src);
  ]


let decode_pshuf reg_t size_t r gop imm off_min off_max sreg =
  let size =
    match size_t with
    | S128 -> 128
    | S64 -> 64
    | S32 -> 32
  in
  let src_expr = disas_expr_xmm gop reg_t size_t sreg in
  let range = (off_max - off_min) / 4 in
  let open Dba_types in
  let rec assign acc dst src off1 off2 =
    if off1 + (range - 1) < off_max then
      let shift_temp1 = Dba.ExprExtU (Dba.ExprRestrict (cst_of_int imm 8, off2, off2 + 1), size) in
      let shift_temp2 = cst_of_int range size in
      let shift = Expr.umul shift_temp1 shift_temp2 in
      let temp = Expr.shift_left src_expr shift in
      let acc =
        Predba.assign (Dba.LhsVarRestrict (dst, size, off1, off1 + range - 1))
          (Expr.restrict temp off_min (off_min + range - 1))
        :: acc
      in assign acc dst src (off1 + range) (off2 + 2)
    else List.rev acc
  in
  let dst =
    match reg_t with
    | MM -> X86Util.mm_reg_to_string (X86Util.xmm_reg_to_mm_reg r)
    | XMM -> X86Util.xmm_reg_to_string r
  in
  let decoded = assign [] dst gop off_min 0 in
  if off_min > 0
  then
    let lim = off_min - 1 in
    Predba.assign
      (Dba.LhsVarRestrict (dst, size, 0, lim))
      (Expr.restrict src_expr 0 lim)
    :: decoded
  else if off_max < size
  then decoded @
       [ let sz = size - 1 in
         Predba.assign
           (Dba.LhsVarRestrict (dst, size, off_max, sz))
           (Expr.restrict src_expr off_max sz) ]
  else decoded


let decode_movshdup mm gop1 gop2 sreg =
  List.flatten
    [ assign_xmm gop1 0  31  gop2 32 63  XMM mm sreg;
      assign_xmm gop1 32 63  gop2 32 63  XMM mm sreg;
      assign_xmm gop1 64 95  gop2 96 127 XMM mm sreg;
      assign_xmm gop1 96 127 gop2 96 127 XMM mm sreg;
    ]


let decode_psubb xmm mm gop1 gop2 sreg =
  let size = X86Util.bitsize_of_xmm_mm xmm in
  let gop1_expr = disas_expr_xmm gop1 xmm mm sreg in
  let gop2_expr = disas_expr_xmm gop2 xmm mm sreg in
  let open Dba_types in
  let rec assign acc off1 off2 =
    if off2 >= size then acc
    else
      let temp1 = Expr.restrict gop1_expr off1 off2 in
      let temp2 = Expr.restrict gop2_expr off1 off2 in
      let expr = Expr.sub temp1 temp2 in
      assign_xmm_expr gop1 off1 off2 expr xmm mm sreg
      @ assign acc (off1 + 8) (off2 + 8)
  in assign [] 0 8


let decode_psrl xmm size gop1 gop2 range sreg =
  let e_gop1 = disas_expr_xmm gop1 xmm size sreg in
  let count =  disas_expr_xmm gop2 xmm size sreg in
  let sz, max_count =
    match xmm with
    | MM  -> 64, range
    | XMM ->
      let max_count = if range = 63 then 15 else range in 128, max_count
  in
  let open Dba_types in
  match gop2  with
  | Imm i ->

    if Int64.to_int i > max_count
    then [ Predba.assign (disas_lval_xmm gop1 xmm size sreg) (Expr.zeros sz) ]
    else
      let rec assign acc off =
        if off >= sz then acc
        else
          let e =
            Expr.shift_right (Expr.restrict e_gop1 off (off + range))
              (cst_of_int (Int64.to_int i) (range + 1))
          in assign_xmm_expr gop1 off (off + range) e xmm size sreg
             @ assign acc (off + range + 1)
      in assign [] 0
  | _ ->
    let cond = Dba.CondReif (Expr.ule count (cst_of_int max_count sz)) in
    [ Predba.jif cond (Dba.JInner 3);
      Predba.assign (disas_lval_xmm gop1 xmm size sreg) (Expr.zeros sz);
      Predba.static_jump (Dba.JInner (sz / (range + 1) + 3)) ]
    @
    let rec assign acc off =
      if off >= sz then acc
      else
        let r = off + range in
        let e =
          Expr.shift_right
            (Expr.restrict e_gop1 off r)
            (Expr.restrict count 0 range) in
        assign_xmm_expr gop1 off r e xmm size sreg
        @ assign acc (r + 1)
    in assign [] 0



let decode_psll xmm size gop1 gop2 range sreg =
  let e_gop1 = disas_expr_xmm gop1 xmm size sreg in
  let count =  disas_expr_xmm gop2 xmm size sreg in
  let sz, max_count = X86Util.bitsize_of_xmm_mm xmm, range in
  match gop2  with
  | Imm i ->
    if Int64.to_int i > max_count
    then [ Predba.assign (disas_lval_xmm gop1 xmm size sreg) (Dba_types.Expr.zeros sz) ]
    else
      let rec assign acc off =
        if off >= sz
        then acc
        else
          let e =
            Dba.ExprBinary (Dba.LShift, Dba.ExprRestrict (e_gop1, off, off + range), cst_of_int (Int64.to_int i) (range + 1)) in
          assign_xmm_expr gop1 off (off + range) e xmm size sreg @ assign acc (off + range + 1)
      in assign [] 0
  | _ ->
    let cond = Dba.CondReif (Dba.ExprBinary (Dba.LeqU, count, cst_of_int max_count sz)) in
    [ Predba.jif cond (Dba.JInner 3);
      Predba.assign (disas_lval_xmm gop1 xmm size sreg) (Dba_types.Expr.zeros sz);
      Predba.static_jump (Dba.JInner (sz / (range + 1) + 3)) ]
    @
    let rec assign acc off =
      if off >= sz
      then acc
      else
        let e =
          Dba.ExprBinary (Dba.LShift, Dba.ExprRestrict (e_gop1, off, off + range),  Dba.ExprRestrict (count, 0, range)) in
        assign_xmm_expr gop1 off (off + range) e xmm size sreg @ assign acc (off + range + 1)
    in assign [] 0


let decode_psra xmm size gop1 gop2 range sreg =
  let open Dba_types in
  let e_gop1 = disas_expr_xmm gop1 xmm size sreg in
  let count =  disas_expr_xmm gop2 xmm size sreg in
  let sz, max_count = X86Util.bitsize_of_xmm_mm xmm, range in
  let rec assign rshift acc off =
    if off >= sz then acc
    else
      let roff = off + range in
      let e = Expr.(shift_right (restrict e_gop1 off roff) rshift) in
      assign_xmm_expr gop1 off roff e xmm size sreg @
      assign rshift acc (roff + 1)
  in
  match gop2  with
  | Imm i ->
    if Int64.to_int i > max_count
    then [ Predba.assign (disas_lval_xmm gop1 xmm size sreg) (Expr.zeros sz) ]
    else assign (cst_of_int (Int64.to_int i) (range + 1)) [] 0
  | _ ->
    let cond = Dba.CondReif (Expr.ule count (cst_of_int max_count sz)) in
    [ Predba.jif cond (Dba.JInner 3);
      Predba.assign (disas_lval_xmm gop1 xmm size sreg) (Expr.zeros sz);
      Predba.assign (disas_lval_xmm gop2 xmm size sreg) (cst_of_int range sz) ] @
    assign (Expr.restrict count 0 range) [] 0


let decode_ps_ldq shift gop1 imm sreg =
  let lval = disas_lval_xmm gop1 XMM S128 sreg in
  let mke = Dba_types.Expr.binary shift (disas_expr_xmm gop1 XMM S128 sreg) in
  let v = if imm > 15 then 128 else imm in
  [ Predba.assign lval (mke (cst_of_int v 128)) ]


let decode_ptest xmm size gop1 gop2 = affect_flags_ptest xmm size gop1 gop2


let decode_predicate p xmm mm lop rop sreg =
  let lvalue = disas_lval_xmm lop xmm mm sreg
  and l_e = disas_expr_xmm lop xmm mm sreg
  and r_e = disas_expr_xmm rop xmm mm sreg in
  let e = p l_e r_e in
  [ Predba.assign lvalue e ]


let decode_pxor = decode_predicate Dba_types.Expr.logxor
and decode_por  = decode_predicate Dba_types.Expr.logor
and decode_pand = decode_predicate Dba_types.Expr.logand
and decode_pandn =
  decode_predicate (fun le re -> Dba_types.Expr.(logand (lognot le) re))


let decode_punpcklbw xmm size gop1 gop2 step sreg =
  match xmm with
  | MM ->
    let rec assign acc off1 off2 =
      if off1 - 2 * step - 1 < 0
      then
        acc
      else
        assign_xmm gop1 (off1 - step) off1 gop2 (off2 - step) off2 xmm size sreg @
        assign_xmm gop1 (off1 - 2 * step - 1) (off1 - step - 1) gop1 (off2 - step) off2 xmm size sreg @
        assign acc (off1 - 2 * (step + 1)) (off2 - step - 1)
    in
    assign [] 63 31
  | XMM ->
    let rec assign acc off1 off2 =
      if off1 + 2 * step + 1 > 127
      then acc
      else
        assign_xmm gop1 off1 (off1 + step) gop1 off2 (off2 + step) xmm size sreg @
        assign_xmm gop1 (off1 + step + 1) (off1 + 2 * step + 1) gop2 off2 (off2 + step) xmm size sreg @
        assign acc (off1 + 2 * (step + 1)) (off2 + step + 1)
    in
    assign [] 0 0


let decode_pmaxu xmm size gop1 gop2 step sreg =
  let e1 = disas_expr_xmm gop1 xmm size sreg in
  let e2 = disas_expr_xmm gop2 xmm size sreg in
  let sz = X86Util.bitsize_of_xmm_mm xmm in
  let rec assign acc off id =
    if off >= sz
    then acc
    else
      let off2 = step + off in
      let restrict e = Dba_types.Expr.restrict e off off2 in
      let cond =
        Dba.CondReif (Dba_types.Expr.ugt (restrict e1) (restrict e2)) in
      Predba.jif cond (Dba.JInner id) ::
      assign_xmm gop1 off off2 gop2 off off2 xmm size sreg @
      assign acc (off2 + 1) (id + 2)
  in assign [] 0 2


let decode_pcmpgt xmm s_mode gop1 gop2 size sreg =
  let rec aux acc i j base_idx bound =
    if j >= bound then acc
    else
      let l1 n =
        Predba.jif
          (Dba.CondReif
            Dba_types.Expr.(
              ugt (restrict (disas_expr_xmm gop1 xmm s_mode sreg) i j)
                (restrict (disas_expr_xmm gop2 xmm s_mode sreg) i j)))
          (Dba.JInner (base_idx + n))
      in
      let l2 = assign_xmm_expr gop1 i j (maxi_bv size) xmm s_mode sreg in
      let l3 = assign_xmm_expr gop1 i j (Dba_types.Expr.zeros size) xmm s_mode
          sreg in
      let l2len = List.length l2
      and l3len = List.length l3 in
      let next = base_idx + l2len + l3len + 2 in
      let l =
        l1 (l2len +2)
        :: l2 @
        ( Predba.static_jump (Dba.JInner next) :: l3) in
      aux (acc @ l) (i + size) (j + size) next bound
  in aux [] 0 (size - 1) 0 (X86Util.bitsize_of_xmm_mm xmm)


let lv_restrict_eax =
  let eax_size = Basic_types.BitSize.create 32 in
  Dba_types.LValue.restrict (X86Util.reg32_to_string EAX) eax_size

let al_lval = lv_restrict_eax 0 7
let ah_lval = lv_restrict_eax 8 15

let e_restrict_eax =
  Dba_types.Expr.restrict
    (Dba.ExprVar (X86Util.reg32_to_string EAX, 32, None))

let al_expr = e_restrict_eax 0 7
let ah_expr = e_restrict_eax 8 15

let decode_aad imm =
  let open Dba_types in
  let e1 = Expr.umul ah_expr (cst_of_int imm 8) in
  let e2 = Expr.add al_expr e1 in
  let e = Expr.logand e2 (cst_of_int 255 8) in
  [
    Predba.assign al_lval e;
    Predba.assign ah_lval (Expr.zeros 8)
  ] @ affect_flags_aad al_expr


let decode_aam imm =
  let open Dba_types in
  let v = cst_of_int imm 8 in
  let e1 = Expr.udiv al_expr v in
  let e2 = Expr.umod al_expr v in
  [
    Predba.assign ah_lval e1;
    Predba.assign al_lval e2
  ] @ affect_flags_aad al_expr


let decode_address_mode = function
  | A32 -> 32, `M32
  | A16 -> 16, `M16


let decode_loop_cond cond a_mode src =
  let size, mode = decode_address_mode a_mode in
  [
    Predba.assign (lhs_of_reg ECX mode)
      (Dba_types.Expr.sub (expr_of_reg mode ECX) (cst_of_int 1 size));
    Predba.jif cond (strange_addr_of_int64 src)
  ]


let loop_cond_e a_mode =
  let size, mode = decode_address_mode a_mode in
  Dba_types.Expr.(diff (expr_of_reg mode ECX) (zeros size))


let decode_loop a_mode _mode src =
  let cond = Dba.CondReif (loop_cond_e a_mode) in
  decode_loop_cond cond a_mode src


let decode_loopz a_mode _mode src =
  let cond1 = loop_cond_e a_mode in
  let cond = Dba.CondReif (Dba_types.Expr.logand (zf_flag) cond1) in
  decode_loop_cond cond a_mode src


let decode_loopnz a_mode _mode src =
  let open Dba_types in
  let cond1 = loop_cond_e a_mode in
  let cond2 = Expr.lognot zf_flag in
  let cond = Dba.CondReif (Expr.logand cond1 cond2) in
  decode_loop_cond cond a_mode src


let decode_xlat addr_mode sreg =
  let open Dba_types in
  let al_lhs = Dba.LhsVarRestrict (X86Util.reg32_to_string EAX, 32, 0, 7) in
  let al_expr =
    Expr.restrict (Dba.ExprVar (X86Util.reg32_to_string EAX, 32, None)) 0 7 in
  let e = Dba.ExprExtU (al_expr, 32) in
  let ebx_expr = Dba.ExprVar (X86Util.reg32_to_string EBX, 32, None) in
  match addr_mode with
  | A32 ->
    [ Predba.assign al_lhs (expr_of_mem8 (Expr.add ebx_expr e) ~sreg) ]
  | A16 ->
    let ebx_expr = Expr.restrict ebx_expr 0 15 in
    let ebx_expr = Dba.ExprExtU (ebx_expr, 32) in
    [ Predba.assign al_lhs (expr_of_mem8 (Expr.add ebx_expr e) ~sreg) ]


let decode_fxch float_register =
  let open Dba_types in
  let operand1_lhs = lhs_of_float_reg float_register in
  let operand1_expr = expr_of_float_reg float_register in
  let operand2_lhs = lhs_of_float_reg ST0 in
  let operand2_expr = expr_of_float_reg ST0 in
  let name = "temp80"
  and size = Basic_types.BitSize.create 80
  and vtag_opt = Some Dba.Temp in
  let temp_lhs = LValue.var name ~bitsize:size vtag_opt in
  let temp_expr = Expr.var name size vtag_opt in
  [ Predba.assign temp_lhs operand1_expr;
    Predba.assign operand1_lhs operand2_expr;
    Predba.assign operand2_lhs temp_expr
  ]

let decode_bts mode base offset sreg =
  let open Dba_types in
  let size = size_mode mode in
  let base_expr = disas_expr base mode sreg in
  let base_lhs = disas_lval base mode sreg in
  match offset with
  | Reg _
  | Address _ ->
    let offset = disas_expr offset mode sreg in
    let one = cst_of_int 1 size in
    let mask = Expr.shift_left one offset in
    let op = Expr.logor base_expr mask in
    [ Predba.assign base_lhs op ]
  | Imm i ->
    let i = Int64.to_int i in
    let nbits = Basic_types.BitSize.create size in
    let tmp = LValue.temp nbits in
    let tmp_name = Utils.unsafe_get_opt (LValue.name_of tmp) in
    [
      Predba.assign (tmp) (base_expr);
      Predba.assign (LValue.restrict_to_bit tmp_name nbits i) (cst_of_int 1 1);
      Predba.assign (base_lhs) (Expr.temp nbits);
    ]


let decode_btr mode base offset sreg =
  let open Dba_types in
  let twoPower n = Bigint.power_int_positive_int 2 n in
  let max_b n = Bigint.pred_big_int (twoPower n) in
  let size = size_mode mode in
  let base_expr = disas_expr base mode sreg in
  let base_lhs = disas_lval base mode sreg in
  match offset with
  | Reg _
  | Address _ ->
    let offset = disas_expr offset mode sreg in
    let max1 = max_b size in
    let max2 = twoPower (size - 2) in
    let mask = Bigint.sub_big_int max1 max2 in
    let mask = Expr.constant (Bitvector.create mask size) in
    let op = Expr.logand base_expr mask in
    let max = cst_of_int (size - 1) size in
    let c = Dba.CondReif (Expr.eq offset max) in
    let tmp = temp_size size in
    [
      Predba.jif c (Dba.JInner 4);
      Predba.assign (base_lhs) (op);
      Predba.static_jump (Dba.JInner 7);
      Predba.assign (Dba.LhsVar(tmp, size, Some Dba.Temp)) base_expr;
      Predba.assign (Dba.LhsVarRestrict(tmp, size, size-1, size-1)) Expr.zero;
      Predba.assign base_lhs (Dba.ExprVar(tmp, size, Some Dba.Temp))
    ]
  | Imm i ->
    let i = Int64.to_int i in
    let tmp = temp_size size in
    [
      Predba.assign (Dba.LhsVar(tmp, size, Some Dba.Temp)) base_expr;
      Predba.assign (Dba.LhsVarRestrict(tmp, size, i, i)) Expr.zero;
      Predba.assign base_lhs (Dba.ExprVar(tmp, size, Some Dba.Temp))
    ]


(* EFLAGS :

   |0|0|0|0|0|0|0|0|0|0|ID|VIP|VIF|AC|VM|RF|0|NT|IOPL(2)|OF|DF|IF|TF|SF|ZF|0|AF|0|PF|1|CF|*)

let decode_pushfd mode _sreg =
  let pf_flag = expr_of_flag PF in
  let af_flag = expr_of_flag AF in
  let sf_flag = expr_of_flag SF in
  let df_flag = expr_of_flag DF in
  let open Dba_types in
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in  *)
  let gen_code size =
    let nbits = Basic_types.BitSize.create size in
    let tmp_lhs = Dba_types.LValue.temp nbits in
    let tmp_exp = Expr.temp nbits in
    let zero2 = Expr.zeros 2 in
    let one = cst_of_int 1 1 in
    let flags = [of_flag; df_flag; zero2;
                 sf_flag; zf_flag; Expr.zero;
                 af_flag; Expr.zero ; pf_flag; one; cf_flag;] in
    let eflags = catenate_expressions flags in
    let eflags = Expr.uext eflags nbits in
    let cst = cst_of_int (nbytes_mode mode) 32 in
    [
      Predba.assign (tmp_lhs) (eflags);
      Predba.assign (lhs_of_mem mode (Expr.sub esp_expr cst)) (tmp_exp);
      Predba.assign (esp_lval) (Expr.sub esp_expr cst)
    ]
  in
  size_mode mode |> gen_code


let decode_popfd mode _sreg =
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in  *)
  let open Dba_types in
  let gen_code size =
    let nbits = Basic_types.BitSize.create size in
    let tmp_lhs = LValue.temp nbits in
    let tmp_exp = Expr.temp nbits in
    Predba.assign (tmp_lhs) (expr_of_mem mode esp_expr) ::
    Predba.assign (esp_lval)
      (Expr.add (expr_of_reg mode ESP)
                 (cst_of_int (nbytes_mode mode) 32))
    ::
    List.map2
      (fun flag n ->
         Predba.assign (lhs_of_flag flag Dba.FlgUnspecified)
           (Expr.restrict_to_bit tmp_exp n))
      [CF; PF; AF; ZF; SF; DF; OF;]
      [0;  2;  4;  6;  7;  10; 11;]
  in size_mode mode |> gen_code


let decode_lsl mode _src dst sreg =
  match mode with
  | `M16 | `M32 ->
    [
      Predba.non_deterministic (lhs_of_flag ZF Dba.FlgUnspecified) `Constant;
      Predba.non_deterministic (disas_lval dst mode sreg) `Constant
    ]
  | `M8 -> failwith "decode lsl with operands on 8 bits"



let decode_cmpXchg8b reg_t size_t gop sreg =
  match size_t with
  | S64 ->
    let open Dba_types in
    let dst_exp = disas_expr_xmm gop reg_t size_t sreg in
    let dst_lhs = disas_lval_xmm gop reg_t size_t sreg in
    let eax_expr = expr_of_reg32 EAX in
    let eax_lhs = lhs_of_reg32 EAX in
    let ebx_expr = expr_of_reg32 EBX in
    let ecx_expr = expr_of_reg32 ECX in
    let edx_expr = expr_of_reg32 EDX in
    let edx_lhs = lhs_of_reg32 EDX in
    let op = Expr.append edx_expr eax_expr in
    let op2 = Expr.append ecx_expr ebx_expr in
    let c = Expr.eq op dst_exp in
    let c = Dba.CondReif c in
    [
      Predba.jif c (Dba.JInner 5);
      Predba.assign (lhs_of_flag ZF Dba.FlgUnspecified) (Expr.zero);
      Predba.assign (eax_lhs) (Expr.restrict dst_exp 0  31);
      Predba.assign (edx_lhs) (Expr.restrict dst_exp 32 63);
      Predba.static_jump (Dba.JInner 7);
      Predba.assign (lhs_of_flag ZF Dba.FlgUnspecified) (Expr.one);
      Predba.assign (dst_lhs) (op2)
    ]
  | _ -> failwith "decode_cmpXchg8b ?"


let decode_lahf () =
  let pf_flag = expr_of_flag PF in
  let af_flag = expr_of_flag AF in
  let sf_flag = expr_of_flag SF in
  let eflags =
    catenate_expressions
      [ sf_flag; zf_flag; Dba_types.Expr.zero; af_flag; Dba_types.Expr.zero;
        pf_flag; Dba_types.Expr.one; cf_flag;] in
  [ Predba.assign (lhs_of_reg EAX `M8) (eflags) ]

let decode_sahf =
  let flag_bit_alist = [SF, 7; ZF, 6; AF, 4; PF, 2; CF, 0; ] in
  let eax_expr = expr_of_reg32 EAX in
  List.map
    (fun (flag, eax_bit) ->
      let rval = Dba_types.Expr.restrict_to_bit eax_expr eax_bit in
      assign_flag flag rval)
    flag_bit_alist


let decode_salc () =
  [ Predba.assign (lhs_of_reg EAX `M8) (Dba.ExprExtS (cf_flag, 8)) ]

let decode_movzx ~signed mode r e =
  let ext = if signed then Dba_types.Expr.sext else Dba_types.Expr.uext in
  let sz = Basic_types.BitSize.create (size_mode mode) in
  [ Predba.assign (lhs_of_reg r mode) (ext e sz) ]

let decode_mov mode lop rop sreg =
  let lval = disas_lval lop mode sreg
  and rval = disas_expr rop mode sreg in
  [ Predba.assign lval rval ]


let instruction_to_dba rep sreg nextaddr opcode instruction =
  match instruction with
  | Push (mode, genop) -> decode_push mode genop sreg
  | PushS reg -> decode_pushS reg sreg
  | PushA mode ->  decode_pushA (mode:>X86Types.sizeMode) sreg
  | Pushfd mode -> decode_pushfd (mode:>X86Types.sizeMode) sreg
  | Popfd mode -> decode_popfd (mode:>X86Types.sizeMode) sreg
  | Pop (mode, genop) -> decode_pop mode genop sreg
  | PopS reg -> decode_popS reg sreg
  | Arith (mode, op, gop1, gop2) -> decode_arith mode op gop1 gop2 sreg
  | Aad imm -> decode_aad imm
  | Aam imm -> decode_aam imm
  | Shift (mode, shift_op, gop32, gop8) ->
    decode_shift mode shift_op gop32 gop8 sreg
  | Rotate (mode, rotate_op, gop32, gop8) ->
    decode_rotate mode rotate_op gop32 gop8  sreg
  | Shiftd (mode, shift_op, src, dst, gop8) ->
    decode_shiftd mode shift_op src dst gop8 sreg
  | Cmp (mode, gop1, gop2) -> decode_cmp mode gop1 gop2 sreg
  | Cmps mode -> decode_cmps mode sreg
  | CmpXchg (mode, gop1, gop2) -> decode_cmpXchg mode gop1 gop2 sreg
  | Test (mode, gop1, gop2) -> decode_test mode gop1 gop2 sreg
  | Movd (xmm, pos, gop1, gop2) -> decode_movd xmm pos gop1 gop2 sreg
  | MovQ (xmm, mm, gop1, gop2) ->
    assign_xmm_zero gop1 0 63 gop2 0 63 xmm mm sreg
  | MovdQA (xmm, mm, gop1, gop2)
  | MovdQU (xmm, mm, gop1, gop2) -> assign_xmm gop1 0 127 gop2 0 127 xmm mm sreg

  | Movs mode -> decode_movs mode rep sreg
  | Lods mode -> decode_lods mode rep sreg
  | Stos mode -> decode_stos mode rep sreg
  | Scas mode -> decode_scas mode rep sreg
  | CMovcc (mode, cc, gop1, gop2) ->
    decode_cmovcc mode cc gop1 gop2 nextaddr sreg
  | Movaps (mm, gop1, gop2) ->
    let lval = disas_lval_xmm gop1 XMM mm sreg
    and e = disas_expr_xmm gop2 XMM mm sreg in
    [ Predba.assign lval e ]
  | Movlpd (mm, gop1, gop2) -> assign_xmm gop1 0 63 gop2 0 63 XMM mm sreg
  | Movlps (mm, gop1, gop2) -> assign_xmm gop1 0 63 gop2 0 63 XMM mm sreg
  | Movhlps (mm, gop1, gop2) -> assign_xmm gop1 0 63 gop2 64 127 XMM mm sreg
  | Movddup (mm, gop1, gop2) ->
    assign_xmm gop1 0 63 gop2 0 63 XMM mm sreg @
    assign_xmm gop2 64 127 gop2 0 63 XMM mm sreg
  | Movsldup (mm, gop1, gop2) -> decode_movsldup mm gop1 gop2 sreg
  | Palignr (xmm, mm, gop1, gop2, imm) -> decode_palignr xmm mm gop1 gop2 imm sreg
  | Pcmpeqb (xmm, mm, gop1, gop2) -> pcmpeq gop1 gop2 xmm mm 8 sreg
  | Pcmpeqw (xmm, mm, gop1, gop2) -> pcmpeq gop1 gop2 xmm mm 16 sreg
  | Pcmpeqd (xmm, mm, gop1, gop2) -> pcmpeq gop1 gop2 xmm mm 32 sreg
  | PmovMSKB (xmm, mm, gop1, gop2) -> pmovMSK gop1 gop2 xmm mm sreg
  | Pminub (xmm, mm, gop1 , gop2) -> pminu gop1 gop2 xmm mm 8 sreg
  | Pxor (xmm, mm, gop1, gop2) ->   decode_pxor xmm mm gop1 gop2 sreg
  | Por (xmm, mm, gop1, gop2) ->    decode_por xmm mm gop1 gop2 sreg
  | Pand (xmm, mm, gop1, gop2) ->   decode_pand xmm mm gop1 gop2 sreg
  | Pandn (xmm, mm, gop1, gop2) ->  decode_pandn xmm mm gop1 gop2 sreg
  | Pmaxub (xmm, mm, gop1, gop2) -> decode_pmaxu xmm mm gop1 gop2 7  sreg
  | Pmaxuw (xmm, mm, gop1, gop2) -> decode_pmaxu xmm mm gop1 gop2 15 sreg
  | Pmaxud (xmm, mm, gop1, gop2) -> decode_pmaxu xmm mm gop1 gop2 31 sreg
  | Mov (mode, gop1, gop2) ->
    decode_mov mode gop1 gop2 sreg
  | MovSegLeft (reg, gop) ->
    [ Predba.assign (lhs_of_seg reg) (disas_expr16 gop sreg) ]
  | MovSegRight(gop, reg) ->
    [ Predba.assign (disas_lval16 gop sreg) (expr_of_seg reg) ]
  | Bt _ -> affect_flags instruction None sreg
  | Bts (mode, gop1, gop2) ->
    affect_flags instruction None sreg @ decode_bts mode gop1 gop2 sreg
  | Btr (mode, gop1, gop2) ->
    affect_flags instruction None sreg @ decode_btr mode gop1 gop2 sreg

  | Movzx (mode, r, op8) ->
    decode_movzx ~signed:false mode r (disas_expr8 op8 sreg)
  | Movzx16 (mode, r, op16) ->
    decode_movzx ~signed:false mode r (disas_expr16 op16 sreg)
  | Movsx (mode, r, op8) ->
    decode_movzx ~signed:true mode r  (disas_expr8 op8 sreg)
  | Movsx16 (mode, r, op16) ->
    decode_movzx ~signed:true mode r  (disas_expr16 op16 sreg)

  | Leave -> decode_leave sreg
  | Lea(_mode, src, dst) ->
    let a = expr_of_addr dst in
    let a = effective_address a sreg in
    [ Predba.assign (lhs_of_reg32 src) a ]
  | Jmp src -> [ Predba.static_jump (strange_addr_of_int64 src) ]
  | DJmp gop -> [ Predba.dynamic_jump (disas_expr gop `M32 sreg) ]
  | Jcc (cc, src) -> [ Predba.jif (cond_of_cc cc) (strange_addr_of_int64 src) ]
  | Call src -> decode_call src nextaddr sreg
  | DCall gop -> decode_dcall gop nextaddr sreg
  | Ret -> decode_ret	sreg
  | Retf -> decode_retf sreg
  | Reti imm -> decode_reti imm sreg
  | Retfi imm -> decode_retfi imm sreg
  | SetCc (cc, dst) ->
    let lvalue = disas_lval8 dst sreg in
    let e =
      Dba_types.Expr.ite
        (cond_of_cc cc) (cst_of_int 1 8) (Dba_types.Expr.zeros 8) in
    [ Predba.assign lvalue e ]
  | Nop -> []
  | Wait -> []
  | Not (mode, gop) -> decode_not mode gop sreg
  | Neg (mode, gop) -> decode_neg mode gop sreg
  | Halt -> [Predba.stop Dba.OK]
  | Cmc
  | Clc
  | Stc
  | Cld
  | Std -> affect_flags instruction None sreg
  | Inc (mode, gop) -> decode_inc mode gop sreg
  | Dec (mode, gop) -> decode_dec mode gop sreg
  | Xchg (mode, gop1, gop2) -> decode_xchg mode gop1 gop2 sreg
  | Mul (mode, gop) -> decode_mul mode gop sreg
  | IMul (mode, gop) -> decode_imul mode gop sreg
  | IMul2 (mode, gop1, gop2) -> decode_imul2 mode gop1 gop2 sreg
  | IMul3 (mode, gop1, gop2, gop3) -> decode_imul3 mode gop1 gop2 gop3 sreg
  | Div (mode, gop) -> decode_div mode gop sreg
  | IDiv (mode, gop) -> decode_idiv mode gop sreg
  | CBW mode -> decode_cbw mode
  | CWD mode -> decode_cwd mode
  | Bsr (mode, r, gop) -> decode_bsr (mode:>X86Types.sizeMode) r gop sreg
  | Bsf (mode, r, gop) -> decode_bsf (mode:>X86Types.sizeMode) r gop sreg
  | Bswap (mode, r) -> decode_bswap mode r
  | Xadd (mode, gop1, gop2) -> decode_xadd mode gop1 gop2 sreg
  | Jcxz (mode, src) -> decode_jcxz mode src

  | CmpXchg8b (reg_t, size_t, gop) -> decode_cmpXchg8b reg_t size_t gop sreg

  | Pshufw (reg_t, size_t, r, gop, imm)  ->
    decode_pshuf reg_t size_t r gop imm 0 64 sreg
  | Pshuflw (reg_t, size_t, r, gop, imm) ->
    decode_pshuf reg_t size_t r gop imm 0 64 sreg
  | Pshufhw (reg_t, size_t, r, gop, imm) ->
    decode_pshuf reg_t size_t r gop imm 64 128 sreg
  | Pshufd (reg_t, size_t, r, gop, imm)  ->
    decode_pshuf reg_t size_t r gop imm 0 128 sreg
  | Movntq (xmm, size, gop1, gop2) ->
    let lval = disas_lval_xmm gop1 xmm size sreg
    and e = disas_expr_xmm gop2 xmm size sreg in
    [ Predba.assign lval e ]

  | Movhpd (mm, gop1, gop2)
  | Movhps (mm, gop1, gop2)
  | Movlhps (mm, gop1, gop2) -> assign_xmm gop1 64 127 gop2 0 63 XMM mm sreg

  | Movshdup (mm, gop1, gop2) -> decode_movshdup mm gop1 gop2 sreg
  | Psubb (xmm, mm, gop1, gop2) -> decode_psubb xmm mm gop1 gop2 sreg

  | Psrlw (xmm, size, gop1, gop2) -> decode_psrl xmm size gop1 gop2 15 sreg
  | Psrld (xmm, size, gop1, gop2) -> decode_psrl xmm size gop1 gop2 31 sreg
  | Psrlq (xmm, size, gop1, gop2) -> decode_psrl xmm size gop1 gop2 63 sreg

  | Psllw (xmm, size, gop1, gop2) -> decode_psll xmm size gop1 gop2 15 sreg
  | Pslld (xmm, size, gop1, gop2) -> decode_psll xmm size gop1 gop2 31 sreg
  | Psllq (xmm, size, gop1, gop2) -> decode_psll xmm size gop1 gop2 63 sreg

  | Psraw (xmm, size, gop1, gop2) -> decode_psra xmm size gop1 gop2 15 sreg
  | Psrad (xmm, size, gop1, gop2) -> decode_psra xmm size gop1 gop2 31 sreg

  | Psrldq (gop, imm) -> decode_ps_ldq Dba.RShiftU gop imm sreg
  | Pslldq (gop, imm) -> decode_ps_ldq Dba.LShift gop imm sreg

  | Ptest (xmm, size, gop1, gop2) -> decode_ptest xmm size gop1 gop2 sreg
  | Punpcklbw (xmm, size, gop1, gop2) ->
    decode_punpcklbw xmm size gop1 gop2 7 sreg
  | Punpcklwd (xmm, size, gop1, gop2) ->
    decode_punpcklbw xmm size gop1 gop2 15 sreg
  | Punpckldq (xmm, size, gop1, gop2) ->
    decode_punpcklbw xmm size gop1 gop2 31 sreg

  | Pcmpgtb (xmm, size, gop1, gop2) ->
    decode_pcmpgt xmm size gop1 gop2 8 sreg
  | Pcmpgtw (xmm, size, gop1, gop2) ->
    decode_pcmpgt xmm size gop1 gop2 1 sreg
  | Pcmpgtd (xmm, size, gop1, gop2) ->
    decode_pcmpgt xmm size gop1 gop2 32 sreg

  | Movups (gop1, gop2)
  | Movupd (gop1, gop2) ->
    [ Predba.assign (disas_lval_xmm gop1 XMM S128 sreg)
        (disas_expr_xmm gop2 XMM S128 sreg) ]

  | Popa mode -> decode_popa mode sreg

  | Loop (mode, addr_size, src) -> decode_loop addr_size mode src
  | Loopz (mode, addr_size, src) -> decode_loopz addr_size mode src
  | Loopnz (mode, addr_size, src) -> decode_loopnz addr_size mode src

  | Xlat addr_size -> decode_xlat addr_size sreg

  | Lsl (mode, src, dst) -> decode_lsl mode src dst sreg

  | Fld -> [ Predba.undefined (lhs_of_float_reg ST0) ]
  | Fxch float_register -> decode_fxch float_register

  | Lahf -> decode_lahf ()
  | Sahf -> decode_sahf
  | Salc -> decode_salc ()
  | Unhandled -> [Predba.stop (Dba.Unsupported opcode)]
  | Bad -> [Predba.stop (Dba.Undefined opcode)]

(* End instruction_to_dba *)


let aux_decode ins nextaddr rep sreg opcode =
  let dba_instructions = instruction_to_dba rep sreg nextaddr opcode ins in
  let check elem =
    try
      if not (Dba_utils.checksize_instruction elem)
      then raise (Invalid_argument "Failed size check")
    with
    | exn ->
      let reason, msg =
        match exn with
        | Errors.Bad_exp_size ->
          "Bad expression size", ""
        | Errors.Bad_bound s ->
          "Bad bound ", s
        | Errors.Size_error s ->
          "Size error", s
        | Errors.Bad_address_size ->
          "Wrong address size", ""
        | Invalid_argument s ->
          s, ""
        | _ ->
          "Unknown exception", ""
      in
      Logger.error "Check failed : %s%s %@ %a"
        reason msg Dba_printer.Ascii.pp_instruction elem
      ; exit 2
  in
  let dba_block = Predba.blockify nextaddr dba_instructions in
  Dba_types.Block.iter check dba_block;
  dba_instructions


let aux_decode2 basic_instr addr rep sreg =
  let open X86Instruction in
  let nextaddr =
    addr + (Basic_types.ByteSize.to_int basic_instr.size)
    |> Dba_types.Caddress.block_start_of_int
  in
  aux_decode basic_instr.mnemonic nextaddr rep sreg basic_instr.opcode


let x86decode = X86decoder.read


let x86decode_from_reader addr reader =
  let binstr, rep, sreg = x86decode reader in
  (* convert x86 IR -> DBA *)
  let insnslst = aux_decode2 binstr addr rep sreg in
  let nextaddr =
    addr + (Basic_types.ByteSize.to_int binstr.X86Instruction.size)
    |> Dba_types.Caddress.block_start_of_int
  in
  let block = Predba.blockify nextaddr insnslst in
  assert (Dba_types.Block.Check.has_inbound_inner_jumps block);
  assert (Dba_types.Block.Check.no_temporary_leak block);
  binstr, block


let decode (addr:Dba_types.Virtual_address.t) =
  let reader = Lreader.of_img (Loader_utils.get_img ()) ~cursor:(addr:>int) in
  x86decode_from_reader (addr:>int) reader


(* addr_size in Bytes *)
let decode_string hopc addr =
  try
    let base = Int64.to_int addr in
    let reader = Lreader.of_binstream ~base hopc in
    x86decode_from_reader base reader
  with
  | X86decoder.Parse s
  | Failure s -> raise (InstructionUnhandled s)
