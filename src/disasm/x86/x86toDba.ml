(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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
open Disasm_options

exception InstructionUnhandled of string

let is_ignored_segment sreg =
  let segment_name = X86Util.segment_reg_to_string sreg in
  let handled_segments = Disasm_options.HandleSegments.get () in
  Basic_types.String.Set.mem segment_name handled_segments |> not


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
    List.fold_left Dba.Expr.append e es


let cst_of_int n size =
  let bv = Bitvector.create (Bigint.big_int_of_int n) size in
  Dba.Expr.constant bv


let four_32 = cst_of_int 4 32

let maxi_bv n = Dba.Expr.constant (Bitvector.max_ubv n)

let cst_of_int64_xmm n =
  let size = 128 in
  let bv =
    if Int64.logand n high_bit_128 = Int64.zero then
      Bitvector.create (Bigint.big_int_of_int64 n) size
    else
      Bitvector.create
        (Bigint.big_int_of_int64 (Int64.neg (Int64.sub higher_bit_128 n)))
        size
  in Dba.Expr.constant bv

let cst_of_int64_32 n =
  let size = 32 in
  if Int64.logand n high_bit = Int64.zero then
    let bv = Bitvector.create (Bigint.big_int_of_int64 n) size in
    Dba.Expr.constant bv
  else
    let bv = Bitvector.create (Bigint.big_int_of_int64 (Int64.neg (Int64.sub higher_bit n))) size in
    Dba.Expr.constant bv

let cst_of_int64_16 n =
  let size = 16 in
  if Int64.logand n high_bit_16 = Int64.zero then
    let bv = Bitvector.create (Bigint.big_int_of_int64 n) size in
    Dba.Expr.constant bv
  else
    let bv = Bitvector.create (Bigint.big_int_of_int64 (Int64.neg (Int64.sub higher_bit_16 n))) size in
    Dba.Expr.constant bv

let cst_of_int64_8 n =
  let size = 8 in
  if Int64.logand n high_bit_8 = Int64.zero then
    let bv = Bitvector.create (Bigint.big_int_of_int64 n) size in
    Dba.Expr.constant bv
  else
    let bv =
      Bitvector.create (Bigint.big_int_of_int64 (Int64.neg (Int64.sub higher_bit_8 n))) size in
    Dba.Expr.constant bv

let _is_too_big_shift cst size =
  match cst with
  | Dba.Expr.Cst (_, bi) ->
    Bigint.ge_big_int (Bitvector.value_of bi) (Bigint.big_int_of_int size)
  | _ -> false

let _addr_of_int n = (Bitvector.create (Bigint.big_int_of_int n) 32, 0)

let strange_bitvector_of_int64 n size =
  let v = if Int64.compare n Int64.zero = -1 then Int64.add higher_bit n else n
  in Bitvector.create (Bigint.big_int_of_int64 v) size

let _strange_cst_of_int64 n size =
  let bv = strange_bitvector_of_int64 n size in
  Dba.Expr.constant bv

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


let lhs_of_seg s =
  Dba.LValue.var (X86Util.segment_reg_to_string s)
    ~bitsize:Size.Bit.bits16 None

let lhs_of_reg r mode =
  let bitsize = Size.Bit.bits32 in
  let mk_var name = Dba.LValue.var name ~bitsize None in
  match mode with
  | `M32 -> X86Util.reg32_to_string r |> mk_var
  | `M16 ->
    let off1, off2, r32 = offsets_of_reg16_32 r in
    Dba.LValue.restrict (X86Util.reg32_to_string r32) bitsize off1 off2
  | `M8 ->
    let off1, off2, r32 = offsets_of_reg8_32 r in
    Dba.LValue.restrict (X86Util.reg32_to_string r32) bitsize off1 off2

let lhs_of_reg32 r =
  Dba.LValue.var (X86Util.reg32_to_string r) ~bitsize:Size.Bit.bits32 None

let lhs_of_reg16 r =
  let off1, off2, r32 = offsets_of_reg16 r in
  Dba.LValue.restrict (X86Util.reg32_to_string r32) Size.Bit.bits32 off1 off2

let lhs_of_reg8 r =
  let off1, off2, r32 = offsets_of_reg8 r in
  Dba.LValue.restrict (X86Util.reg32_to_string r32) Size.Bit.bits32 off1 off2


let edi_lval = lhs_of_reg32 EDI
let esi_lval = lhs_of_reg32 ESI
let esp_lval = lhs_of_reg32 ESP

let lhs_of_reg_xmm r reg_t =
  let open Size.Bit in
  match reg_t with
  | XMM ->
    Dba.LValue.var (X86Util.xmm_reg_to_string r) ~bitsize:bits128 None
  | MM  ->
    Dba.LValue.var
      (X86Util.mm_reg_to_string (X86Util.xmm_reg_to_mm_reg r))
      ~bitsize:bits64 None

let bits80 = Size.Bit.create 80

let lhs_of_float_reg r =
  Dba.LValue.var (X86Util.float_reg_to_string r) ~bitsize:bits80 None

let _lhs_of_xmm_reg_restrict r off1 off2 =
  let open Size.Bit in
  Dba.LValue.restrict (X86Util.xmm_reg_to_string r) bits128 off1 off2

let lhs_of_flag f inst =
  Dba.LValue.var (X86Util.flag_to_string f)
    ~bitsize:Size.Bit.bits1 (Some (Dba.VarTag.flag inst))

let undef_flag flag = Predba.undefined (lhs_of_flag flag Dba.Flag.unspecified)

let undef_flags flags = List.map undef_flag flags

let expr_of_seg s = Dba.Expr.var (X86Util.segment_reg_to_string s) 16 None

let expr_of_reg mode r =
  let open Dba.Expr in
  match mode with
  | `M32 -> Dba.Expr.var (X86Util.reg32_to_string r) 32 None
  | `M16 ->
    let off1, off2, r32 = offsets_of_reg16_32 r in
    restrict off1 off2 (var (X86Util.reg32_to_string r32) 32 None)
  | `M8 ->
    let off1, off2, r32 = offsets_of_reg8_32 r in
    restrict off1 off2 (var (X86Util.reg32_to_string r32) 32 None)


let expr_of_reg32 = expr_of_reg `M32

let expr_of_reg16 r =
  let off1, off2, r32 = offsets_of_reg16 r in
  Dba.Expr.(restrict off1 off2 (var (X86Util.reg32_to_string r32) 32 None))

let expr_of_reg8 r =
  let off1, off2, r32 = offsets_of_reg8 r in
  Dba.Expr.restrict off1 off2
    (Dba.Expr.var (X86Util.reg32_to_string r32) 32 None)

let esp_expr = expr_of_reg32 ESP
and esi_expr = expr_of_reg32 ESI
and edi_expr = expr_of_reg32 EDI


let e_of_reg addrMode r =
  let open Dba.Expr in
  match addrMode with
  | A32 -> var (X86Util.reg32_to_string r) 32 None
  | A16 ->
    let off1, off2, r32 = offsets_of_reg16_32 r in
    let e = restrict off1 off2 (var (X86Util.reg32_to_string r32) 32 None) in
    uext 32 e


let expr_of_reg_xmm r xmm =
  let open Dba.Expr in
  let v = var (X86Util.xmm_reg_to_string r) 128 None in
  match xmm with
  | XMM -> v
  | MM -> restrict 0 63 v

let expr_of_float_reg r = Dba.Expr.var (X86Util.float_reg_to_string r) 80 None

let expr_of_flag f =
  Dba.Expr.var (X86Util.flag_to_string f) 1 (Some (Dba.VarTag.flag Dba.Flag.unspecified))

let cf_flag = expr_of_flag CF
let of_flag = expr_of_flag OF
let zf_flag = expr_of_flag ZF
let pf_flag = expr_of_flag PF
let sf_flag = expr_of_flag SF
let af_flag = expr_of_flag AF
let df_flag = expr_of_flag DF

let expr_of_addr addr =
  let open Dba in
  let disp = addr.addrDisp in
  let expr_reg = e_of_reg addr.addrMode in
  match (disp = Int64.zero, addr.addrBase, addr.addrIndex) with
  | true, None, None -> Expr.zeros 32
  | true, Some r, None -> expr_reg r
  | true, Some r1, Some (sc, r2) ->
    let bop = Expr.mul (expr_reg r2) (cst_of_int (X86Util.scale_to_size sc) 32) in
    Expr.add (expr_reg r1) bop
  | true, None, Some (Scale1, r) -> expr_reg r
  | true, None, Some (sc, r) ->
    Expr.mul (expr_reg r) (cst_of_int (X86Util.scale_to_size sc) 32)
  | false, _, _ ->
    begin
      match addr.addrBase, addr.addrIndex with
      | None, None  -> cst_of_int64_32 disp
      | Some r, None ->
        Expr.add (expr_reg r) (cst_of_int64_32 disp)
      | Some r1, Some (Scale1, r2) ->
        Expr.add (expr_reg r1) (Expr.add (expr_reg r2) (cst_of_int64_32 disp))
      | Some r1, Some (sc, r2) ->
        let bop =
          Expr.mul (expr_reg r2) (cst_of_int (X86Util.scale_to_size sc) 32) in
        let bop = Expr.add bop (cst_of_int64_32 disp) in
        Expr.add (expr_reg r1) bop
      | None, Some (Scale1, r) ->
        Expr.add (expr_reg r) (cst_of_int64_32 disp)
      | None, Some (sc, r) ->
        let bop =
          Expr.mul (expr_reg r) (cst_of_int (X86Util.scale_to_size sc) 32) in
        Expr.add bop (cst_of_int64_32 disp)
    end


let if_df = Predba.conditional_jump (expr_of_flag DF)

let cond_of_cc cc =
  let open Dba in
  let open Expr in
  match cc.truth_value, cc.condition with
  | true,  O  -> of_flag
  | false, O  -> lognot of_flag
  | true,  B  -> cf_flag
  | false, B  -> lognot cf_flag
  | true,  Z  -> zf_flag
  | false, Z  -> lognot zf_flag
  | true,  BE -> logor cf_flag zf_flag
  | false, BE -> logand (lognot cf_flag) (lognot zf_flag)
  | true,  S  -> sf_flag
  | false, S  -> lognot sf_flag
  | true,  P  -> pf_flag
  | false, P  -> lognot pf_flag
  | true,  L  -> Expr.diff sf_flag of_flag
  | false, L  -> Expr.equal sf_flag of_flag
  | true,  LE -> logor zf_flag (Expr.diff sf_flag of_flag)
  | false, LE -> logand (lognot zf_flag) (Expr.equal sf_flag of_flag)


let retrieve_sreg a =
  let rec is_stack_access e sreg =
    match e with
    | Dba.Expr.Var (name, _, _) ->
      if name = "ebp" || name = "esp" then SS else DS
    | Dba.Expr.Binary (_, e1, e2) ->
      let sreg = is_stack_access e1 sreg in
      is_stack_access e2 sreg
    | _ -> sreg
  in is_stack_access a DS


(* We represent the base of segments using a hidden register which is
   loaded only when the segment register changes, which corresponds to
   what the actual processor does. *)
let segment_address sreg a =
  if is_ignored_segment sreg then a
  else
    let open Dba in
    let name = X86Util.segment_reg_to_string sreg in
    let sreg = Expr.var  (name ^ "_base") 32 None in
    Expr.add sreg a

let effective_address a sreg =
  match sreg with
  | None ->
     if Disasm_options.ProtectedMode.get ()
     then segment_address (retrieve_sreg a) a else a
  | Some sr ->
     let sr_name = X86Util.segment_reg_to_string sr in
     if Disasm_options.ProtectedMode.get ()
        || Basic_types.String.Set.mem sr_name
             (Disasm_options.HandleSegments.get ())
     then segment_address sr a
     else a


let lhs_of_mem mode ?(sreg=None) a =
  let a = effective_address a sreg in
  let nbytes = X86Util.bytesize_of_szmode mode in
  Dba.LValue.store nbytes Dba.LittleEndian a


let lhs_of_mem32 = lhs_of_mem `M32
and lhs_of_mem16 = lhs_of_mem `M16
and lhs_of_mem8  = lhs_of_mem `M8


let lhs_of_mem_xmm mm a sreg =
  let a = effective_address a sreg in
  let nbytes = X86Util.bytesize_of_simd_size mm in
  Dba.LValue.store nbytes Dba.LittleEndian a


let expr_of_mem (mode:X86Types.sizeMode) ?(sreg=None) a =
  let a = effective_address a sreg in
  let nbytes = X86Util.bytesize_of_szmode mode in
  Dba.Expr.load nbytes Dba.LittleEndian a


let expr_of_mem32 = expr_of_mem `M32
and expr_of_mem16 = expr_of_mem `M16
and expr_of_mem8  = expr_of_mem `M8

let expr_of_mem_xmm mm ?(sreg=None) a =
  let a = effective_address a sreg in
  let nbytes = X86Util.bytesize_of_simd_size mm in
  Dba.Expr.load nbytes Dba.LittleEndian a


let assign_flag flag ?(flag_cmp=Dba.Flag.unspecified) e =
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
  let open Dba in
  match lhs with
  | Dba.LValue.Store (_, endian, e) ->
    let nbits = Dba_types.LValue.bitsize lhs in
    let tmp = LValue.temp nbits in
    let tmp_name = Utils.unsafe_get_opt (Dba_types.LValue.name_of tmp) in
    let nbytes = Size.Byte.of_bitsize nbits in
    [ Predba.assign tmp (Dba.Expr.load nbytes endian e);
      Predba.assign (LValue.restrict tmp_name nbits lo hi) rhs;
      Predba.assign lhs (Dba_types.Expr.temp nbits) ]
  | Dba.LValue.Var (s, size, _) ->
    let lval = LValue.restrict s (Size.Bit.create size) lo hi in
    [ Predba.assign lval rhs ]
  | Dba.LValue.Restrict (s, _, {Interval.lo=o1; Interval.hi=o2}) ->
    let lo = o1 + lo and hi = o1 + hi in
    let nbits = Size.Bit.create (o2 - o1) in
    [ Predba.assign (LValue.restrict s nbits lo hi) rhs ]


let assign_xmm gop1 off1 off2 gop2 off3 off4 xmm mm sreg =
  let lhs = disas_lval_xmm gop1 xmm mm sreg in
  let rhs = Dba.Expr.restrict off3 off4 (disas_expr_xmm gop2 xmm mm sreg)  in
  assign lhs rhs off1 off2


let assign_xmm_zero ~dst off1 off2 ~src off3 off4 xmm mm sreg =
  let lhs = disas_lval_xmm dst xmm mm sreg in
  let rhs =
    Dba.Expr.restrict off3 off4 (disas_expr_xmm src xmm mm sreg) in
  let open Dba_types in
  let nbits = LValue.bitsize lhs in
  let nbytes = Size.Byte.of_bitsize nbits in
  match lhs with
  | Dba.LValue.Store (size, endian, e) ->
    let tmp = temp_size size in
    [ Predba.assign (Dba.LValue.temp nbits) (Dba.Expr.load nbytes endian e);
      Predba.assign (Dba.LValue.restrict tmp nbits off1 off2) rhs;
      Predba.assign lhs (Expr.temp nbits) ]
  | Dba.LValue.Var (s, size, _) ->
    Predba.assign (Dba.LValue.restrict s nbits off1 off2) rhs ::
    (match xmm with
     | XMM ->
       [ Predba.assign (Dba.LValue.restrict s nbits (off2 + 1) (size - 1))
           (Dba.Expr.zeros (size - off2 - 1)) ]
     | MM -> []
    )
  | Dba.LValue.Restrict (s, _, {Interval.lo=o1; Interval.hi=o2}) ->
    let nbits = Size.Bit.create (o2 - o1) in
    [ Predba.assign (Dba.LValue.restrict s nbits (o1 + off1) (o1 + off2)) rhs;
      Predba.assign (Dba.LValue.restrict s nbits (o1 + off2 + 1) (o1 - o2 - 1))
        (Dba.Expr.zeros (o2 - off2 + 1)) ]


let assign_xmm_expr gop1 lo hi expr xmm mm sreg =
  let lhs = disas_lval_xmm gop1 xmm mm sreg in
  assign lhs expr lo hi


let assign_expr_expr gop1 off1 off2 expr _xmm _mm sreg =
  let lhs = disas_lval gop1 `M32 sreg in
  assign lhs expr off1 off2


let assign_expr_xmm gop1 off1 off2 gop2 off3 off4 xmm mm sreg =
  let lhs = disas_lval gop1 `M32 sreg in
  let rhs = Dba.Expr.restrict off3 off4 (disas_expr_xmm gop2 xmm mm sreg)in
  assign lhs rhs off1 off2


let clear_flag fl inst =
  Predba.assign (lhs_of_flag fl inst) (Dba.Expr.zeros 1)


let update_CF op1 op2 _res size op flag_cmp =
  match op with
  | Add ->
    let open Dba in
    let sz = size + 1 in
    let op1' = Expr.uext sz op1 in
    let op2' = Expr.uext sz op2 in
    let sum' = Expr.add op1' op2' in
    assign_flag CF (Expr.bit_restrict size sum')
  | Adc ->
    let open Dba in
    let sz = size + 1 in
    let op1' = Expr.uext sz op1 in
    let op2' = Expr.uext sz op2 in
    let cf_flag = Expr.uext sz cf_flag in
    let sum' = Expr.add (Expr.add op1' op2') cf_flag in
    assign_flag CF (Expr.bit_restrict size sum')
  | Sub ->
    (* carry = 1 <->  A <_u B *)
    assign_flag CF ~flag_cmp (Dba.Expr.ult op1 op2)
  | Sbb ->
    (* carry = 1 <->  A <_u B + CF *)
    (* carry = 1 ? A =<_u B : A <_u B *)
    let open Dba in
    let cf_true  = Expr.ule op1 op2 in
    let cf_false = Expr.ult op1 op2 in
    assign_flag CF (Expr.ite cf_flag cf_true cf_false)
  | Xor | Or | And -> clear_flag CF Dba.Flag.unspecified


let update_OF op1 op2 res size op flag_cmp =
  let bit = size - 1 in
  let signbit1 = Dba.Expr.bit_restrict bit op1
  and signbit2 = Dba.Expr.bit_restrict bit op2
  and rres = Dba.Expr.bit_restrict bit res in
  match op with
  | Add | Adc -> (* ADC & ADD behaves the same for the OF flag *)
    (* ov=1 <->  A[n]=B[n] /\ A[n] \= (A+B)[n] *)
    assign_flag OF Dba.Expr.(logand (equal signbit1 signbit2) (diff signbit1 rres))
  | Sub ->
    (* ov=1 <->  A[n]\= B[n] /\ A[n] \= (A-B)[n] *)
    assign_flag OF ~flag_cmp
      Dba.Expr.(logand (diff signbit1 signbit2) (diff signbit1 rres))
  | Sbb ->
    let open Dba in
    let cf_flag = Expr.uext size cf_flag in
    let op2' = Expr.add op1 cf_flag in
    let signbit2 = Expr.bit_restrict bit op2' in
    (* ov=1 <->  A[n]\= B[n] /\ A[n] \= (A-B)[n] *)
    assign_flag OF
      Expr.(logand (diff signbit1 signbit2) (diff signbit1 rres))
  | Xor | Or | And -> clear_flag OF Dba.Flag.unspecified


let update_ZF res size flag_cmp =
  let open Dba in
  assign_flag ZF ~flag_cmp (Expr.equal res (Expr.zeros size))


let update_SF res size flag_cmp =
  assign_flag SF ~flag_cmp Dba.Expr.(slt res (zeros size))
(* Another way to update SF: Dba.ExprRestrict(res,size-1, size-1) *)


let update_PF res _size flag_cmp =
  let open Dba in
  let rec xor_sum acc i =
    if i < 8
    then xor_sum Expr.(logxor acc (bit_restrict i res)) (i + 1)
    else acc
  in
  let e = Expr.(lognot (xor_sum (bit_restrict 0 res) 1)) in
  assign_flag PF ~flag_cmp e


let update_AF op1 op2 _res _size op flag_cmp =
  let op1_res = Dba.Expr.restrict 0 3 op1 in
  let op2_res = Dba.Expr.restrict 0 3 op2 in
  match op with
  | Add ->
    let open Dba in
    let op1_ext = Expr.uext 5 op1_res in
    let op2_ext = Expr.uext 5 op2_res in
    let sum = Expr.add op1_ext op2_ext in
    assign_flag AF (Expr.bit_restrict 4 sum)
  | Adc ->
    let open Dba in
    let op1_ext = Expr.uext 5 op1_res in
    let op2_ext = Expr.uext 5 op2_res in
    let af_flag_ext = Expr.uext 5 af_flag in
    let sum = Expr.add op1_ext (Expr.add op2_ext af_flag_ext) in
    assign_flag AF (Expr.bit_restrict 4 sum)
  | Sub -> assign_flag AF ~flag_cmp (Dba.Expr.ult op1_res op2_res)
  | Sbb ->
    let open Dba in
    let cf_true = Expr.ule op1_res op2_res in
    let cf_false = Expr.ult op1_res op2_res in
    assign_flag AF (Expr.ite af_flag cf_true cf_false)
  | Xor
  | Or
  | And -> clear_flag AF Dba.Flag.unspecified


(* Utility function covering a widespread code pattern *)
let getopt_or_fail ~(in_function:string) = function
  | None ->
    failwith (Format.sprintf "x86toDBA@%s : no result provided" in_function)
  | Some r -> r


let affect_flags_inc op res' size =
  let res = getopt_or_fail ~in_function:"affect_flags_inc" res' in
  [ update_OF op (cst_of_int 1 32) res size Add Dba.Flag.unspecified;
    update_SF res size Dba.Flag.unspecified;
    update_AF op (cst_of_int 1 32) res size Add Dba.Flag.unspecified;
    update_PF res size Dba.Flag.unspecified;
    update_ZF res size Dba.Flag.unspecified]


let affect_flags_dec op res' size =
  let res = getopt_or_fail ~in_function:"affect_flags_dec" res' in
  [ update_OF op (cst_of_int 1 32) res size Sub Dba.Flag.unspecified;
    update_SF res size Dba.Flag.unspecified;
    update_AF op (cst_of_int 1 32) res size Sub Dba.Flag.unspecified;
    update_PF res size Dba.Flag.unspecified;
    update_ZF res size Dba.Flag.unspecified]


let affect_flags_arith op op1 op2 res_f size =
  let res = getopt_or_fail ~in_function:"affect_flags_arith" res_f in
  match op with
  | Add ->
    [ update_OF op1 op2 res size Add Dba.Flag.unspecified;
      update_SF res size Dba.Flag.unspecified;
      update_ZF res size Dba.Flag.unspecified;
      update_AF op1 op2 res size Add Dba.Flag.unspecified;
      update_PF res size Dba.Flag.unspecified;
      update_CF op1 op2 res size Add Dba.Flag.unspecified ]
  | Adc ->
    [ update_OF op1 op2 res size Adc Dba.Flag.unspecified;
      update_SF res size Dba.Flag.unspecified;
      update_ZF res size Dba.Flag.unspecified;
      update_AF op1 op2 res size Adc Dba.Flag.unspecified;
      update_PF res size Dba.Flag.unspecified;
      update_CF op1 op2 res size Adc Dba.Flag.unspecified ]
  | Sub ->
    let inst = Dba.Flag.subtraction op1 op2 in
    [ update_OF op1 op2 res size Sub inst;
      update_SF res size inst;
      update_ZF res size inst;
      update_AF op1 op2 res size Sub inst;
      update_PF res size inst;
      update_CF op1 op2 res size Sub inst ]
  | Sbb ->
    [ update_OF op1 op2 res size Sbb Dba.Flag.unspecified;
      update_SF res size Dba.Flag.unspecified;
      update_ZF res size Dba.Flag.unspecified;
      update_AF op1 op2 res size Sbb Dba.Flag.unspecified;
      update_PF res size Dba.Flag.unspecified;
      update_CF op1 op2 res size Sbb Dba.Flag.unspecified]
  | And ->
    [ update_OF op1 op2 res size And Dba.Flag.unspecified;
      update_SF res size Dba.Flag.unspecified;
      update_ZF res size Dba.Flag.unspecified;
      update_AF op1 op2 res size And Dba.Flag.unspecified;
      update_PF res size Dba.Flag.unspecified;
      update_CF op1 op2 res size And Dba.Flag.unspecified ]
  | Or ->
    [ update_OF op1 op2 res size Or Dba.Flag.unspecified;
      update_SF res size Dba.Flag.unspecified;
      update_ZF res size Dba.Flag.unspecified;
      update_AF op1 op2 res size Or Dba.Flag.unspecified;
      update_PF res size Dba.Flag.unspecified;
      clear_flag CF Dba.Flag.unspecified; ]
  | Xor ->
    [ update_OF op1 op2 res size Xor Dba.Flag.unspecified;
      update_SF res size Dba.Flag.unspecified;
      update_ZF res size Dba.Flag.unspecified;
      update_AF op1 op2 res size Xor Dba.Flag.unspecified;
      update_PF res size Dba.Flag.unspecified;
      clear_flag CF Dba.Flag.unspecified; ]


let affect_flags_shift op expr shift res' size =
  let open Dba in
  let res = getopt_or_fail ~in_function:"affect_flags_shift" res' in
  let sz = size - 1 in
  let shift =
    Expr.uext size (Expr.restrict 0 4 shift) in
  match op with
  | Shl ->
    let shift_minus_one = Expr.(shift_left expr (sub shift (cst_of_int 1 size))) in
    let inst_OF =
      if Dba_types.Expr.is_one shift then
        assign_flag OF (Expr.(logxor (bit_restrict sz res) cf_flag))
      else
        Predba.undefined (lhs_of_flag OF Dba.Flag.unspecified)  in
    [ inst_OF;
      update_SF res size Dba.Flag.unspecified;
      update_ZF res size Dba.Flag.unspecified;
      assign_flag CF (Expr.bit_restrict (size - 1) shift_minus_one); ]
  | Shr ->
    let shift_minus_one =
      Expr.shift_right expr (Expr.sub shift (cst_of_int 1 size)) in
    let inst_OF =
      if Dba_types.Expr.is_one shift then
        assign_flag OF (Expr.bit_restrict 0 expr)
      else
        Predba.undefined (lhs_of_flag OF Dba.Flag.unspecified)  in
    [ inst_OF;
      update_SF res size Dba.Flag.unspecified;
      update_ZF res size Dba.Flag.unspecified;
      assign_flag CF (Expr.bit_restrict 0 shift_minus_one);
    ]
  | Sar ->
    let shift_minus_one =
      Expr.shift_right_signed expr (Expr.sub shift (cst_of_int 1 size)) in
    let inst_OF =
      if Dba_types.Expr.is_one shift then assign_flag OF Dba.Expr.zero
      else Predba.undefined (lhs_of_flag OF Dba.Flag.unspecified)  in
    [ inst_OF;
      update_SF res size Dba.Flag.unspecified;
      update_ZF res size Dba.Flag.unspecified;
      assign_flag CF (Expr.bit_restrict 0 shift_minus_one);
    ]

let affect_flags_rotate op _expr rop res' size =
  let open Dba in
  let res = getopt_or_fail ~in_function:"affect_flags_rotate" res' in
  let inst_OF =
    if Dba_types.Expr.is_one rop then
      let sz = size - 1 in
      assign_flag OF Expr.(logxor (bit_restrict sz res) (cf_flag))
    else undef_flag OF
  in
  let assign_to_cf e = assign_flag CF e in
  match op with
  | Rol ->
    [ assign_to_cf (Expr.bit_restrict 0 res);
      inst_OF;
      update_SF res size Dba.Flag.unspecified;
      update_ZF res size Dba.Flag.unspecified]
  | Ror ->
    [ assign_to_cf (Expr.bit_restrict (size - 1) res );
      inst_OF;
      update_SF res size Dba.Flag.unspecified;
      update_ZF res size Dba.Flag.unspecified]
  | Rcl ->
    [ assign_to_cf (Expr.bit_restrict size res); inst_OF ]
  | Rcr ->
    [ inst_OF; assign_to_cf (Expr.bit_restrict size res); ]


let affect_flags_cmp op1 op2 res' size =
  let res = getopt_or_fail ~in_function:"affect_flags_cmp" res' in
  let inst = Dba.Flag.comparison op1 op2 in
  [ update_OF op1 op2 res size Sub inst;
    update_SF res size inst;
    update_ZF res size inst;
    update_AF op1 op2 res size Sub inst;
    update_PF res size inst;
    update_CF op1 op2 res size Sub inst ]

let affect_flags_test op1 op2 res' size =
  let res = getopt_or_fail ~in_function:"affect_flags_test" res' in
  let inst = Dba.Flag.test op1 op2 in
  [ clear_flag OF inst;
    update_SF res size inst;
    Predba.undefined (lhs_of_flag AF inst);
    update_PF res size inst;
    update_ZF res size inst;
    clear_flag CF inst ]

let affect_flags_mul res mode =
  let open Dba in
  let res = getopt_or_fail ~in_function:"affect_flags_mul" res in
  let middle = size_mode mode in
  let res_res = Expr.restrict middle (middle * 2 - 1) res in
  assign_flag OF (Expr.(diff res_res (zeros (size_mode mode)))) ::
  assign_flag CF of_flag ::
  undef_flags [SF; ZF; AF; PF;]

let affect_flags_imul res mode =
  let open Dba.Expr in
  let res = getopt_or_fail ~in_function:"affect_flags_imul" res in
  let middle = size_mode mode in
  let extended_eax = sext (middle * 2) (expr_of_reg mode EAX)  in
  assign_flag OF (diff res extended_eax) ::
  assign_flag CF of_flag ::
  undef_flags [SF; ZF; AF; PF;]

let affect_flags_imul2_3 res' op2 op3 =
  let open Dba.Expr in
  let open Size.Bit in
  let open Predba in
  let res = getopt_or_fail ~in_function:"affect_flags_imul2_3" res' in
  let sext64 e = Dba_types.Expr.sext e bits64 in
  [ Dba.LValue.temporary "temp64" bits64 <<-
    Dba.Expr.mul (sext64 op2) (sext64 op3);
    assign_flag OF
      (diff
         (temporary "temp64" ~size:64)
         (sext64 res));
    undef_flag SF; undef_flag ZF;
    undef_flag AF; undef_flag PF;
    assign_flag CF of_flag; ]

let affect_flags_div = undef_flags [OF; SF; ZF; CF;]

let affect_flags_neg op res' size =
  let open Dba.Expr in
  let res = getopt_or_fail ~in_function:"affect_flags_neg" res' in
  [ assign_flag OF (equal op (maxi_bv size));
    update_SF res size Dba.Flag.unspecified;
    update_ZF res size Dba.Flag.unspecified;
    assign_flag CF (diff op (zeros size)); ]


let affect_flags_ptest xmm size op1 op2 sreg =
  let open Dba.Expr in
  let e1 = disas_expr_xmm op1 xmm size sreg in
  let e2 = disas_expr_xmm op2 xmm size sreg in
  let v = constant (Bitvector.zeros 128) in
  let and_e1 = logand e1 in
  let c1 = equal(and_e1 e2) v in
  let c2 = equal(and_e1 (lognot e2)) v in
  [ clear_flag OF Dba.Flag.unspecified;
    clear_flag SF Dba.Flag.unspecified;
    assign_flag ZF c1;
    assign_flag CF c2;
  ]


let affect_flags_shiftd = undef_flags [ OF; SF; CF; ZF]

let affect_flags_aad res =
  [
    undef_flag OF;
    update_SF res 8 Dba.Flag.unspecified;
    update_ZF res 8 Dba.Flag.unspecified;
    undef_flag CF;
  ]


let affect_flags_bt mode base offset sreg =
  let op =
    match offset with
    | Reg _
    | Address _ ->
      let modulo =
        let sz =
          X86Util.bitsize_of_szmode mode
          |> Size.Bit.to_int in
        let value = sz |> Bigint.big_int_of_int in
        Dba.Expr.constant (Bitvector.create value sz) in
      let e = disas_expr offset mode sreg in
      let offset = Dba.Expr.umod e modulo in
      let op = Dba.Expr.shift_right base offset in
      Dba.Expr.bit_restrict 0 op
    | Imm i ->
      let i = Int64.to_int i in Dba.Expr.bit_restrict i base
  in
  assign_flag CF op ::
  undef_flags [OF; SF; AF; PF;]

let pcmpeq gop1 gop2 xmm mm size sreg =
  let rec aux acc i j base_idx bound =
    if j >= bound then acc
    else
      let restrict_genop gop =
        Dba.Expr.restrict i j (disas_expr_xmm gop xmm mm sreg) in
      let l1 n =
        [ Predba.conditional_jump
            (Dba.Expr.equal(restrict_genop gop1) (restrict_genop gop2))
            (Dba.JInner (base_idx + n))
        ]
      in
      let aux_assign cst = assign_xmm_expr gop1 i j cst xmm mm sreg in
      let l2 = maxi_bv size |> aux_assign in
      let l3 = Dba.Expr.zeros size |> aux_assign in
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
    assign_expr_expr gop1 lo hi (Dba.Expr.zeros (1 + hi - lo)) xmm mm sreg
  in
  let start = X86Util.bytesize_of_xmm_mm xmm in
  (aux [] 0 start) @ assign_expr start 31


let pminu gop1 gop2 xmm mm size sreg =
  let rec aux acc i j base_idx bound =
    if j >= bound then acc
    else
      let restrict_genop gop =
        Dba.Expr.restrict i j (disas_expr_xmm gop xmm mm sreg) in
      let l1 n =
        [ Predba.conditional_jump
            Dba.((Expr.ult (restrict_genop gop1) (restrict_genop gop2)))
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
    let size = List.length l + 3 in
    let ecx = expr_of_reg32 ECX in
    let pre_l =
      Predba.conditional_jump
        Dba.(Expr.equal ecx (Dba.Expr.zeros 32))
        (Dba.JInner size) in
    let post_l = [
      Predba.assign (lhs_of_reg32 ECX) (Dba.Expr.sub ecx (cst_of_int 1 32));
      Predba.static_jump (Dba.JInner 0) ]
    in
    let l =
      List.map (
        fun elem ->
          match elem with
          | Predba.If(cond, Dba.JInner a) ->
            Predba.conditional_jump cond (Dba.JInner (a + 1))
          | Predba.SJump(Dba.JInner a, tag) ->
            Predba.static_jump (Dba.JInner (a + 1)) ~tag
          | _ -> elem
      ) l in
    pre_l :: l @ post_l


let mk_temp base size = base^string_of_int size, size, Some Dba.VarTag.temp

let mk_lhs_temp base size =
  let name, sz, tag = mk_temp base size in
  let bitsize = Size.Bit.create sz in
  Dba.LValue.var name ~bitsize tag


let mk_res_temp base size =
  let name, sz, tag = mk_temp base size in Dba.Expr.var name sz tag


let res_lhs mode = mk_lhs_temp "res" (size_mode mode)
let double_res_lhs mode = mk_lhs_temp "res" (2 * size_mode mode)
let temp_lhs mode = mk_lhs_temp "temp" (size_mode mode)
let double_temp_lhs mode = mk_lhs_temp "temp" (2 * size_mode mode)


let res_expr mode = mk_res_temp "res" (size_mode mode)
let double_res_expr mode = mk_res_temp "res" (2 * size_mode mode)
let temp_expr mode = mk_res_temp "temp" (size_mode mode)
let double_temp_expr mode =  mk_res_temp "temp" (2 * size_mode mode)



module type B = sig val base: string val tag : Dba.VarTag.t option end
module Bidirectional_name(X: B) = struct
  type t = {
    dst : Dba.LValue.t;
    src : Dba.Expr.t;
  }

  open Size.Bit
  let mk =
    let i = ref 0 in
    (fun bitsize ->
       incr i;
       let size = to_int bitsize in
       let name = Printf.sprintf "%s%d_%d" X.base !i size in
       (* dst is a L-value *)
       let dst = Dba.LValue.var name ~bitsize X.tag in
       let src = Dba.Expr.var name size X.tag in
       { dst; src; })


  (* let mk8 ()  = mk bits8
   * let mk16 () = mk bits16
   * let mk32 () = mk bits32 *)

  let of_mode mode =
    let bitsize = X86Util.bitsize_of_szmode mode in
    mk bitsize

  let (<--) t e = Predba.assign t.dst e
  let (-->) t lv = Predba.assign lv t.src
end

module Bidirectional_tmp = struct
  include Bidirectional_name(struct
      let base = "temp"
      let tag = Some Dba.VarTag.temp
    end)
end


let lift_push mode genop sreg =
  let mem =
    Dba.Expr.sub esp_expr (cst_of_int (nbytes_mode mode) 32) in
  [ Predba.assign (lhs_of_mem mode mem) (disas_expr genop mode sreg);
    Predba.assign esp_lval mem
  ]

let lift_pushS reg _sreg =
  let e = Dba.Expr.sub esp_expr (cst_of_int 2 32) in
  [ Predba.assign (lhs_of_mem16 e) (expr_of_seg reg);
    Predba.assign esp_lval e;
  ]

let lift_pushA mode sreg =
  let push_dec reg = lift_push mode (Reg reg) sreg in
  Predba.assign (temp_lhs mode) (expr_of_reg mode ESP) ::
  List_utils.flat_map push_dec [EAX; ECX; EDX; EBX;] @
  let esp = esp_expr in
  let v = cst_of_int (nbytes_mode mode) 32 in
  let e = Dba.Expr.sub esp v in
  Predba.assign esp_lval e ::
  Predba.assign (lhs_of_mem mode e) (temp_expr mode) ::
  List_utils.flat_map push_dec [EBP; ESI; EDI;]


let lift_pop mode genop sreg =
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in  *)
  let esp_contents = expr_of_mem mode esp_expr in
  match genop with
  | Reg ESP ->
    [ Predba.assign (disas_lval genop mode sreg) esp_contents; ]

  | _ ->
    [ Predba.assign (disas_lval genop mode sreg) esp_contents;
      Predba.assign esp_lval
        (Dba.Expr.add esp_expr (cst_of_int (nbytes_mode mode) 32))  ]


let lift_popS reg _sreg =
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in  *)
  [ Predba.assign (lhs_of_seg reg) (expr_of_mem16 esp_expr);
    Predba.assign esp_lval (Dba.Expr.add esp_expr (cst_of_int 2 32)) ]


let lift_popA mode sreg =
  let word_size = (* in byte *) nbytes_mode mode in
  let aux_popa (n, register) =
    let lval = disas_lval (Reg register) mode sreg in
    let byte_offset = cst_of_int (n * word_size) 32 in
    let rval = Dba.Expr.add esp_expr byte_offset |> expr_of_mem ~sreg mode in
    Predba.assign lval rval
  in
  List.map aux_popa [0, EDI; 1, ESI; 2, EBP; 4, EBX; 5, EDX; 6, ECX; 7, EAX;]
  @ [ Predba.assign esp_lval
        (Dba.Expr.add esp_expr (cst_of_int (8 * word_size) 32))]


let lift_arith mode op gop1 gop2 sreg =
  let disassed_gop1 = disas_expr gop1 mode sreg in
  let disassed_gop2 = disas_expr gop2 mode sreg in
  let add_carry e =
    let open Dba in
    Expr.add e (Expr.uext (size_mode mode) cf_flag) in
  let rhs =
    match op with
    | Add -> Dba.Expr.add disassed_gop1 disassed_gop2
    | Adc ->
      let dba_gop2_carry = add_carry disassed_gop2 in
      Dba.Expr.add disassed_gop1 dba_gop2_carry
    | Sub -> Dba.Expr.sub disassed_gop1 disassed_gop2
    | Sbb ->
      let dba_gop2_carry = add_carry disassed_gop2 in
      Dba.Expr.sub disassed_gop1 dba_gop2_carry
    | And -> Dba.Expr.logand disassed_gop1 disassed_gop2
    | Or ->  Dba.Expr.logor disassed_gop1 disassed_gop2
    | Xor -> Dba.Expr.logxor disassed_gop1 disassed_gop2
  in
  let res = Some (res_expr mode) in
  Predba.assign (res_lhs mode) rhs ::
  affect_flags_arith op (disas_expr gop1 mode sreg) (disas_expr gop2 mode sreg)
    res (size_mode mode) @
  [ Predba.assign (disas_lval gop1 mode sreg) (res_expr mode) ]


let lift_shift mode shift_op gop32 gop8 sreg =
  let open Dba in
  let size = size_mode mode in
  let shift = Expr.uext size (Expr.restrict 0 4 (disas_expr8 gop8 sreg)) in
  let dba_op =
    match shift_op with
    | Shl -> Expr.shift_left
    | Shr -> Expr.shift_right
    | Sar -> Expr.shift_right_signed
  in
  let res = Some (res_expr mode) in
  Predba.assign (res_lhs mode) (dba_op (disas_expr gop32 mode sreg) shift) ::
  affect_flags_shift shift_op (disas_expr gop32 mode sreg)
    (disas_expr8 gop8 sreg) res (size_mode mode) @
  [ Predba.assign(disas_lval gop32 mode sreg) (res_expr mode) ]


let lift_rotate mode rotate_op gop32 gop8 sreg =
  let open Dba in
  let size = size_mode mode in
  let sz = size + 1 in
  let bits = Size.Bit.create sz in
  let tmp = temp_size sz in
  let res_lhs_1 =
    LValue.temporary tmp bits in
  let res_expr_1 = Expr.temporary tmp ~size:sz in
  (* let res33_expr = Dba.ExprVar ("res33", 33, Some Dba.Temp) in *)
  let dba_op, dba_gop1, dba_gop2, size =
    match rotate_op with
    | Rol ->
      Dba.Binary_op.LeftRotate,  disas_expr gop32 mode sreg, res_expr mode, size
    | Rcl ->
      let tmp = Expr.append cf_flag (disas_expr gop32 mode sreg) in
      Dba.Binary_op.LeftRotate, tmp, Expr.restrict 0 (size - 1) res_expr_1 , sz
    | Ror -> Dba.Binary_op.RightRotate, disas_expr gop32 mode sreg, res_expr mode, size
    | Rcr ->
      let tmp = Expr.append cf_flag (disas_expr gop32 mode sreg) in
      Dba.Binary_op.RightRotate, tmp, Expr.restrict 0 (size - 1) res_expr_1 , sz
  in
  let rop = Expr.uext size (Expr.restrict 0 4 (disas_expr8 gop8 sreg)) in
  match rotate_op with
  | Rol | Ror ->
    let res = Some (res_expr mode) in
    Predba.assign (res_lhs mode) (Expr.binary dba_op dba_gop1 rop)
    :: affect_flags_rotate rotate_op (disas_expr gop32)
      (disas_expr8 gop8 sreg) res (size_mode mode)
    @ [ Predba.assign (disas_lval gop32 mode sreg) (dba_gop2) ]
  | Rcl | Rcr ->
    let res = Some (res_expr_1) in
    Predba.assign (res_lhs_1) (Expr.binary dba_op dba_gop1 rop)
    :: affect_flags_rotate rotate_op (disas_expr gop32)
      (disas_expr8 gop8 sreg) res (size_mode mode)
    @ [ Predba.assign (disas_lval gop32 mode sreg) (dba_gop2) ]


let lift_shiftd mode shift_op dst src gop8 sreg =
  let open Dba in
  let size = size_mode mode in
  let count =
    Expr.uext size (Expr.umod (disas_expr8 gop8 sreg) (cst_of_int 32 8)) in
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
  affect_flags_shiftd @
  [ Predba.conditional_jump (Expr.equal count (Expr.zeros size)) jump10;
    Predba.conditional_jump
      (Expr.ule count (cst_of_int size size)) (Dba.JInner 8);
    Predba.undefined ldst;
    Predba.static_jump jump10;
    Predba.assign (ldst) (dba_op1 edst count);
    Predba.assign (ldst) (Expr.logor edst tmp);
  ]


let lift_cmp mode gop1 gop2 sreg =
  let rhs =
    Dba.Expr.sub (disas_expr gop1 mode sreg) (disas_expr gop2 mode sreg) in
  let res = Some (res_expr mode) in
  Predba.assign (res_lhs mode) rhs ::
  affect_flags_cmp (disas_expr gop1 mode sreg)
    (disas_expr gop2 mode sreg) res (size_mode mode)


let lift_cmps mode sreg =
  let open Dba in
  let cst = cst_of_int (nbytes_mode mode) 32 in
  let sreg = match sreg with None -> Some ES | _ -> sreg in
  let gop1 = expr_of_mem mode (esi_expr) ~sreg in
  let gop2 = expr_of_mem mode (edi_expr) ~sreg in
  let res = Some (res_expr mode) in
  let prelude =
    Predba.assign (res_lhs mode) (Expr.sub gop1 gop2) ::
    affect_flags_cmp gop1 gop2 res (size_mode mode) in
  let len = List.length prelude in
  prelude @
  [ if_df (Dba.JInner (len + 1 + 3))                    ;
    Predba.assign esi_lval (Expr.add esi_expr cst) ;
    Predba.assign edi_lval (Expr.add edi_expr cst) ;
    Predba.static_jump (Dba.JInner (len + 1 + 3 + 2))    ;
    Predba.assign esi_lval (Expr.sub esi_expr cst) ;
    Predba.assign edi_lval (Expr.sub edi_expr cst) ;
  ]


let lift_cmpXchg mode gop1 gop2 sreg =
  let eax_expr = expr_of_reg mode EAX in
  let e1 = disas_expr gop1 mode sreg in
  let res = Some (res_expr mode) in
  let open Dba in
  Predba.assign (res_lhs mode) (Expr.sub eax_expr e1) ::
  affect_flags_cmp (disas_expr gop1 mode sreg)
    (expr_of_reg mode EAX) res (size_mode mode) @
  [ Predba.conditional_jump (Expr.equal eax_expr e1) (Dba.JInner 10);
    Predba.assign (lhs_of_reg EAX mode) e1;
    Predba.static_jump (Dba.JInner 11);
    Predba.assign (disas_lval gop1 mode sreg) (disas_expr gop2 mode sreg)
  ]


let lift_test mode gop1 gop2 sreg =
  let rhs = Dba.Expr.logand
      (disas_expr gop1 mode sreg) (disas_expr gop2 mode sreg) in
  let res = Some (res_expr mode) in
  Predba.assign (res_lhs mode) (rhs) ::
  affect_flags_test (disas_expr gop1 mode sreg) (disas_expr gop2 mode sreg)
    res (size_mode mode)


let lift_movd xmm pos gop1 gop2 sreg =
  match pos, xmm with
  | Left, MM ->
    assign_xmm_expr gop1 0 31 (disas_expr gop2 `M32 sreg) xmm S32 sreg @
    assign_xmm_expr gop1 32 63 (Dba.Expr.zeros 32) xmm S32 sreg
  | Left, XMM ->
    assign_xmm_expr gop1 0 31 (disas_expr gop2 `M32 sreg) xmm S32 sreg @
    assign_xmm_expr gop1 32 127 (Dba.Expr.zeros 96) xmm S32 sreg
  | Right, _ ->
    [ Predba.assign (disas_lval gop2 `M32 sreg)
        (Dba.Expr.restrict 0 31 (disas_expr_xmm gop1 xmm S32 sreg)) ]


let lift_movs mode rep sreg =
  let open Dba in
  let mk_rhs_reg ereg =
    let cst = cst_of_int (nbytes_mode mode) 32 in
    Expr.ite (expr_of_flag DF)
      (Expr.sub ereg cst) (Expr.add ereg cst) in
  let tmp_esi = mk_rhs_reg esi_expr in
  let tmp_edi = mk_rhs_reg edi_expr in
  let esi_reg = sreg (* Segment register for ESI can be overriden : see 3-489 *)
  and edi_reg = Some X86Types.ES in (* Segment register for EDI is fixed : see 3-489 *)
  let l = [
    Predba.assign
      (lhs_of_mem mode edi_expr ~sreg:edi_reg)
      (expr_of_mem mode esi_expr ~sreg:esi_reg);
    Predba.assign esi_lval tmp_esi;
    Predba.assign edi_lval tmp_edi;
  ]
  in repeat_instrs l rep


let lift_lods mode rep sreg =
  let cst = cst_of_int (nbytes_mode mode) 32 in
  let l = [
    Predba.assign(lhs_of_reg EAX mode) (expr_of_mem mode esi_expr ~sreg);
    if_df (Dba.JInner 4);
    Predba.assign esi_lval (Dba.Expr.add esi_expr cst);
    Predba.static_jump (Dba.JInner 5);
    Predba.assign esi_lval (Dba.Expr.sub esi_expr cst);
  ]
  in repeat_instrs l rep



let lift_stos mode rep sreg =
  let open Dba in
  let cst = cst_of_int (nbytes_mode mode) 32 in
  let l = [
    Predba.assign (lhs_of_mem mode edi_expr ~sreg) (expr_of_reg mode EAX);
    if_df (Dba.JInner 4);
    Predba.assign(edi_lval) (Expr.add edi_expr cst);
    Predba.static_jump (Dba.JInner 5) ;
    Predba.assign(edi_lval) (Expr.sub edi_expr cst);
  ]
  in repeat_instrs l rep


let lift_scas mode rep sreg =
  let open Dba in
  let sreg = match sreg with None -> Some ES | _ -> sreg in
  let cst = cst_of_int (nbytes_mode mode) 32 in
  let res = Some (res_expr mode) in
  let cmp_l =
    Predba.assign(res_lhs mode)
      (Expr.sub (expr_of_reg mode EAX) (expr_of_mem mode edi_expr ~sreg)) ::
    affect_flags_cmp (expr_of_reg mode EAX)
      (expr_of_mem mode (edi_expr) ~sreg) res (size_mode mode)
  in
  let l = [
    Predba.assign edi_lval
      (Expr.ite (expr_of_flag DF)
         (Expr.sub edi_expr cst)
         (Expr.add edi_expr cst)
      );
  ]
  in repeat_instrs (cmp_l @ l) rep


let lift_cmovcc mode cc gop1 gop2 _ sreg =
  let cond = cond_of_cc cc in
  let lhs = disas_lval gop1 mode sreg in
  let new_expr = disas_expr gop2 mode sreg in
  let old_expr = disas_expr gop1 mode sreg in
  [ Predba.assign lhs (Dba.Expr.ite cond new_expr old_expr) ]


let lift_movsldup mm ~dst ~src sreg =
  assign_xmm dst 0  31  src 0  31 XMM mm sreg @
  assign_xmm dst 32 63  src 0  31 XMM mm sreg @
  assign_xmm dst 64 95  src 64 95 XMM mm sreg @
  assign_xmm dst 96 127 src 64 95 XMM mm sreg


let lift_palignr xmm mm ~dst ~src imm sreg =
  let open Dba_types in
  let size1, size2 =
    match xmm with
    | MM -> 128, 63
    | XMM -> 256, 127
  in
  let nbits = Size.Bit.create size1 in
  let lval = Dba.LValue.temp nbits in
  let rval = Expr.temp nbits in
  Predba.assign lval
    (Dba.Expr.append
       (disas_expr_xmm dst xmm mm sreg) (disas_expr_xmm src xmm mm sreg))
  ::
  Predba.assign lval (Dba.Expr.shift_right rval (cst_of_int (imm * 8) 32))
  ::
  assign_xmm_expr dst 0 size2 (Dba.Expr.restrict 0 size2 rval) xmm mm sreg


let lift_leave _sreg =
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in *)
  [ Predba.assign esp_lval (expr_of_reg32 EBP);
    Predba.assign (lhs_of_reg32 EBP) (expr_of_mem32 esp_expr); (* todo: check if ESP should be on 16bits *)
    Predba.assign esp_lval (Dba.Expr.add esp_expr four_32) ]


let lift_call src nextaddr _sreg =
  let open Dba in
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in *)
  [ Predba.assign esp_lval (Expr.sub esp_expr four_32);
    Predba.assign (lhs_of_mem32 esp_expr) (Expr.constant (nextaddr.Dba.base));
    let tag = Some (Dba.Call nextaddr) in
    Predba.static_jump (strange_addr_of_int64 src) ~tag ]


let lift_dcall gop nextaddr sreg =
  let open Dba in
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in *)
  [ Predba.assign (esp_lval) (Expr.sub esp_expr four_32);
    Predba.assign (lhs_of_mem32 esp_expr) (Expr.constant (nextaddr.Dba.base));
    Predba.dynamic_jump (disas_expr gop `M32 sreg)
      ~tag:(Some (Dba.Call nextaddr))
  ]


let lift_ret _sreg =
  [ Predba.assign (esp_lval) (Dba.Expr.add esp_expr four_32);
    Predba.dynamic_jump (expr_of_mem32 (Dba.Expr.sub esp_expr four_32))
      ~tag:(Some Dba.Return)]

let lift_reti imm _sreg =
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in *)
  let v = cst_of_int (4 + imm) 32 in
  [ Predba.assign (esp_lval) (Dba.Expr.add esp_expr v);
    Predba.dynamic_jump (expr_of_mem32 (Dba.Expr.sub esp_expr v))
      ~tag:(Some Dba.Return)]

let lift_retf _sreg =
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in   *)
  let open Dba in
  [ Predba.assign esp_lval (Expr.add esp_expr four_32);
    Predba.assign (lhs_of_seg CS) (expr_of_mem16 esp_expr);
    Predba.assign esp_lval (Expr.add esp_expr (cst_of_int 2 32));
    Predba.dynamic_jump
      (expr_of_mem32 (Expr.sub esp_expr (cst_of_int 6 32)))
      ~tag:(Some Dba.Return)]

let lift_retfi imm _sreg =
  let open Dba in
  let add_esp = Expr.add esp_expr in
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in *)
  [ Predba.assign (esp_lval) (add_esp (cst_of_int (4 + imm) 32));
    Predba.assign (lhs_of_seg CS) (expr_of_mem16 esp_expr);
    Predba.assign (esp_lval) (add_esp (cst_of_int 2 32));
    Predba.dynamic_jump
      (expr_of_mem32 (Expr.sub esp_expr (cst_of_int (6 + imm) 32)))
      ~tag:(Some Dba.Return)]


let lift_not mode gop sreg =
  (* let mask =  *)
  (*   match mode with  *)
  (*   | `M32 -> cst_of_int64_32 (Int64.of_string "0xffffffff") *)
  (*   | `M16 -> cst_of_int64_16 (Int64.of_string "0xffff")  *)
  (*   | `M8 -> cst_of_int64_8 (Int64.of_string "0xff")  *)
  (* in *)
  [ Predba.assign (disas_lval gop mode sreg)
      (Dba.Expr.lognot (disas_expr gop mode sreg)) ]


let lift_neg mode gop sreg =
  let res = Some (res_expr mode) in
  Predba.assign (res_lhs mode) (Dba.Expr.uminus (disas_expr gop mode sreg))
  ::
  affect_flags_neg (disas_expr gop mode sreg)
    res (size_mode mode)
  @
  [ Predba.assign(disas_lval gop mode sreg) ((res_expr mode)) ]


let lift_inc mode gop sreg =
  let open Dba in
  let rmode = res_expr mode in
  let res = Some rmode in
  Predba.assign
    (res_lhs mode)
    (Expr.add (disas_expr gop mode sreg)
       (Expr.ones (size_mode mode)))
  :: affect_flags_inc (disas_expr gop mode sreg) res (size_mode mode)
  @ [ Predba.assign(disas_lval gop mode sreg) (rmode) ]


let lift_dec mode gop sreg =
  let rmode = res_expr mode in
  let res = Some rmode in
  let open Dba in
  Predba.assign
    (res_lhs mode)
    (Expr.sub (disas_expr gop mode sreg)
       (Expr.ones (size_mode mode)))
  :: affect_flags_dec (disas_expr gop mode sreg) res (size_mode mode)
  @ [ Predba.assign(disas_lval gop mode sreg) (rmode) ]


let lift_xchg mode gop1 gop2 sreg =
  let open Bidirectional_tmp in
  let size = X86Util.bitsize_of_szmode mode in
  let tmp1 = mk size and tmp2 = mk size in
  [ tmp1 <-- (disas_expr gop1 mode sreg);
    tmp2 <-- (disas_expr gop2 mode sreg);
    tmp2 --> (disas_lval gop1 mode sreg);
    tmp1 --> (disas_lval gop2 mode sreg);
  ]


let lift_mul mode gop sreg =
  let open Dba in
  let mode_size = size_mode mode in
  let drexpr = double_res_expr mode
  and drlval = double_res_lhs mode in
  let res = Some drexpr in
  match mode with
  | `M32 ->
    Predba.assign drlval
      (Expr.mul (Expr.uext 64 (expr_of_reg32 EAX))
         (Expr.uext 64 (disas_expr gop mode sreg)))
    ::
    Predba.assign (lhs_of_reg EAX mode)
      (Expr.restrict 0 (mode_size - 1) drexpr)
    ::
    Predba.assign (lhs_of_reg EDX mode)
      (Expr.restrict mode_size (mode_size * 2 -1) drexpr)
    :: affect_flags_mul res mode

  | `M16 ->
    [ Predba.assign drlval
        (Expr.mul
           (Expr.uext 32 (expr_of_reg mode EAX))
           (Expr.uext 32 (disas_expr gop mode sreg)));
      Predba.assign
        (lhs_of_reg EAX mode) (Expr.restrict 0 (mode_size - 1) drexpr);
      Predba.assign (lhs_of_reg EDX mode)
        (Expr.restrict mode_size (mode_size * 2 - 1) drexpr) ] @
    affect_flags_mul res mode

  | `M8 ->
    [ Predba.assign drlval
        (Expr.mul
           (Expr.uext 16 (expr_of_reg mode EAX))
           (Expr.uext 16 (disas_expr gop mode sreg)));
      Predba.assign(lhs_of_reg EAX `M16) (drexpr) ] @
    affect_flags_mul res mode


let lift_imul mode gop sreg =
  let size = size_mode mode * 2 in
  let drlval = double_res_lhs mode in
  let drrval = double_res_expr mode in
  let nbits = Size.Bit.create size in
  let szmode = size_mode mode in
  let res = Some drrval in
  let open Dba in
  let open Predba in
  (drlval <<-
   Expr.mul
     (Dba_types.Expr.sext (expr_of_reg mode EAX) nbits)
     (Dba_types.Expr.sext (disas_expr gop mode sreg) nbits))
  ::
  match mode with
  | `M8 -> (lhs_of_reg EAX `M16 <<- drrval)
           :: affect_flags_imul res mode
  | _ ->
    (lhs_of_reg EAX mode <<- Expr.restrict 0 (szmode - 1) drrval)
    :: (lhs_of_reg EDX mode <<- Expr.restrict szmode (size - 1) drrval)
    :: affect_flags_imul res mode


let lift_imul2 mode gop1 gop2 sreg =
  let open Predba in
  let drrval = res_expr mode in
  let rval =
    Dba.Expr.mul (disas_expr gop1 mode sreg) (disas_expr gop2 mode sreg) in
  let res = Some drrval in
  (res_lhs mode <<- rval)
  :: affect_flags_imul2_3 res (disas_expr gop1 mode sreg)
    (disas_expr gop2 mode sreg)
  @ [ Predba.assign (disas_lval gop1 mode sreg) (drrval) ]


let lift_imul3 mode gop1 gop2 gop3 sreg =
  let open Predba in
  let drrval = res_expr mode in
  let res = Some drrval in
  (res_lhs mode <<-
   Dba.Expr.mul (disas_expr gop2 mode sreg) (disas_expr gop3 mode sreg))
  :: affect_flags_imul2_3 res (disas_expr gop2 mode sreg)
    (disas_expr gop3 mode sreg)
  @ [ disas_lval gop1 mode sreg <<- (drrval) ]

let lift_div mode gop sreg =
  let open Dba in
  let size = size_mode mode in
  let drexpr = double_res_expr mode in
  let dtexpr = double_temp_expr mode in
  let ereg = expr_of_reg mode in
  let double_size = 2 * size in
  let disassed_gop = disas_expr gop mode sreg in
  let ue = Expr.uext double_size disassed_gop in
  let re = Expr.restrict  0 (size -1) drexpr in
  [ Predba.assign (double_temp_lhs mode) (Expr.append (ereg EDX) (ereg EAX));
    Predba.assign (double_res_lhs mode) (Expr.udiv dtexpr ue);
    Predba.conditional_jump
      (Expr.(equal(restrict size (double_size - 1) drexpr) (zeros size)))
      (Dba.JInner 4);
    Predba.stop Dba.KO;
    Predba.assign (lhs_of_reg EAX mode) re;
    Predba.assign (double_res_lhs mode) (Expr.umod dtexpr ue);
    Predba.assign (lhs_of_reg EDX mode) re;
  ] @ affect_flags_div


let lift_idiv mode gop sreg =
  let open Dba in
  let size = size_mode mode in
  let dllval = double_temp_lhs mode in
  let drlval = double_res_lhs mode in
  let double_size = 2 * size in
  let e1 = disas_expr gop mode sreg in
  let drem = double_res_expr mode in
  let dtem = double_temp_expr mode in
  Predba.assign dllval
    (Expr.append (expr_of_reg mode EDX) (expr_of_reg mode EAX)) ::
  Predba.assign drlval
    (Expr.sdiv dtem (Expr.sext double_size e1)) ::
  Predba.assign (temp_lhs mode)
    (Expr.restrict size (double_size - 1) drem)
  ::
  Predba.conditional_jump (Expr.equal(temp_expr mode) (Expr.zeros size))
    (Dba.JInner 5) ::
  Predba.stop Dba.KO ::
  Predba.assign (lhs_of_reg EAX mode)
    (Expr.restrict  0 (size - 1) drem) ::
  Predba.assign (double_temp_lhs mode)
    (Expr.smod dtem (Expr.sext (2 * size) e1)) ::
  Predba.assign (lhs_of_reg EDX mode)
    (Expr.restrict 0 (size - 1) dtem) ::
  affect_flags_div


let lift_cbw mode =
  match mode with
  | `M32 -> [assign_register EAX mode (Dba.Expr.sext 32 (expr_of_reg `M16 EAX))]
  | `M16 -> [assign_register EAX mode (Dba.Expr.sext 16 (expr_of_reg `M8  EAX))]


let lift_cwd mode =
  let size, name =
    match mode with
    | `M32 -> 64, "temp64"
    | `M16 -> 32, "temp32"
  in
  let open Dba.Expr in
  let temp_lhs = Dba.LValue.temporary name (Size.Bit.create size) in
  let temp_exp = temporary name ~size in
  [ Predba.assign (temp_lhs) (sext size (expr_of_reg mode EAX));
    assign_register EDX mode (restrict (size / 2) (size - 1) temp_exp);
  ]


let lift_bsr mode dst src sreg =
  let open Dba_types in
  let src_exp = disas_expr src mode sreg in
  let dst_lhs = lhs_of_reg dst mode in
  let size =
    match mode with
    | `M32 -> 31
    | `M16 -> 15
    | `M8 -> assert false
  in
  let open Dba.Expr in
  let sz = size + 1 in
  let bits = Size.Bit.create sz in
  let tmp = temp_size sz in
  let cpt = cpt_size sz in
  let temp_lhs = Dba.LValue.temporary tmp bits  in
  let temp_exp = temporary tmp ~size:sz in
  let cpt_lhs = Dba.LValue.temporary cpt bits in
  let cpt_exp = temporary cpt ~size:sz in
  let extone = cst_of_int 1 sz in
  [ Predba.conditional_jump (diff src_exp (zeros sz)) (Dba.JInner 4);
    assign_flag ZF (cst_of_int 1 1);
    Predba.undefined dst_lhs;
    Predba.static_jump (Dba.JInner 12);
    assign_flag ZF zero;
    Predba.assign temp_lhs src_exp;
    Predba.assign cpt_lhs (cst_of_int size (size + 1));
    Predba.conditional_jump
      Expr.(equal(bit_restrict size temp_exp) bool_true)
      (Dba.JInner 11);
    Predba.assign temp_lhs (shift_left temp_exp extone);
    Predba.assign cpt_lhs (sub cpt_exp extone);
    Predba.static_jump (Dba.JInner 7);
    Predba.assign dst_lhs cpt_exp;
  ] @ undef_flags [CF; OF; SF;]


let lift_bsf mode dst src sreg =
  let src_exp = disas_expr src mode sreg in
  let dst_lhs = lhs_of_reg dst mode in
  let size =
    match mode with
    | `M32 -> 31
    | `M16 -> 15
    | `M8 -> assert false
  in
  let open Dba in
  let sz = size + 1 in
  let bits = Size.Bit.create sz in
  let tmp = temp_size sz in
  let cpt = cpt_size sz in
  let temp_lhs = Dba.LValue.temporary tmp bits  in
  let temp_exp = Expr.temporary tmp ~size:sz in
  let cpt_lhs = Dba.LValue.temporary cpt bits in
  let cpt_exp = Expr.temporary cpt ~size:sz in
  let zeros = Expr.zeros sz in
  [ Predba.conditional_jump
      (Expr.diff src_exp zeros)
      (Dba.JInner 4);
    Predba.assign (lhs_of_flag ZF Dba.Flag.unspecified) (cst_of_int 1 1);
    Predba.undefined dst_lhs;
    Predba.static_jump (Dba.JInner 12);
    Predba.assign (lhs_of_flag ZF Dba.Flag.unspecified) (Dba_types.Expr.bool_false);
    Predba.assign temp_lhs (src_exp);
    Predba.assign cpt_lhs zeros;
    Predba.conditional_jump
      Expr.(equal(bit_restrict 0 temp_exp) Dba_types.Expr.bool_true)
      (Dba.JInner 11);
    Predba.assign temp_lhs (Expr.shift_right temp_exp (cst_of_int 1 sz));
    Predba.assign cpt_lhs (Expr.add  cpt_exp (cst_of_int 1 sz));
    Predba.static_jump (Dba.JInner 6);
    Predba.assign dst_lhs cpt_exp;
  ] @ undef_flags [CF; OF; SF;]



let lift_bswap mode dst =
  let msize = X86Util.bitsize_of_mode mode in
  let size = Size.Bit.to_int msize in
  let tmp = temp_size size in
  let temp_lhs = Dba.LValue.temporary tmp msize in
  let temp_exp = Dba.Expr.temporary tmp ~size in
  let dst_exp = expr_of_reg mode dst in
  let rec assign acc dst temp off1 off2 =
    if off1 + 7 < size && off2 - 7 >= 0 then
      Predba.assign
        (Dba.LValue.restrict dst Size.Bit.bits32 off1 (off1 + 7))
        (Dba.Expr.restrict (off2 - 7) off2 temp) ::
      assign acc dst temp (off1 + 8) (off2 - 8)
    else acc
  in
  Predba.assign temp_lhs dst_exp ::
  assign [] (X86Util.reg32_to_string dst) temp_exp 0 (size - 1)


let lift_xadd mode gop1 gop2 sreg =
  let open Bidirectional_tmp in
  let tmp1 = of_mode mode and tmp2 = of_mode mode in
  let dba_gop1, dba_gop2 =
    disas_expr gop1 mode sreg, disas_expr gop2 mode sreg in
  (tmp1 <-- Dba.Expr.add dba_gop1 dba_gop2) ::
  (tmp2 <-- dba_gop1) ::
  affect_flags_arith Add (disas_expr gop1 mode sreg)
    (disas_expr gop2 mode sreg) (Some tmp1.src) (size_mode mode) @
  [ tmp1 --> disas_lval gop1 mode sreg;
    tmp2 --> disas_lval gop2 mode sreg;
  ]


let lift_jcxz mode src =
  let size = size_mode mode in
  [ Predba.conditional_jump
      (Dba.Expr.(equal(expr_of_reg mode ECX) (zeros size)))
      (strange_addr_of_int64 src);
  ]


let lift_pshuf reg_t size_t r gop imm off_min off_max sreg =
  let size =
    match size_t with
    | S128 -> 128
    | S64  -> 64
    | S32  -> 32
  in
  let bits = Size.Bit.create size in
  let src_expr = disas_expr_xmm gop reg_t size_t sreg in
  let range = (off_max - off_min) / 4 in
  let open Dba in
  let rec assign acc dst src off1 off2 =
    let hi = off1 + range - 1 in
    if hi < off_max then
      let shift_temp1 =
        Expr.restrict off2 (off2 + 1) (cst_of_int imm 8)
        |> Expr.uext size in
      let shift_temp2 = cst_of_int range size in
      let shift = Expr.mul shift_temp1 shift_temp2 in
      let temp = Expr.shift_right src_expr shift in
      let acc =
        Predba.assign (Dba.LValue.restrict dst bits off1 hi)
          (Expr.restrict off_min (off_min + range - 1) temp)
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
    Predba.
      assign
      (Dba.LValue.restrict dst bits 0 lim)
      (Expr.restrict 0 lim src_expr )
    :: decoded
  else if off_max < size
  then decoded @
       [ let sz = size - 1 in
         Predba.assign
           (Dba.LValue.restrict dst bits off_max sz)
           (Expr.restrict off_max sz src_expr ) ]
  else decoded


let lift_movshdup mm gop1 gop2 sreg =
  List.flatten
    [ assign_xmm gop1 0  31  gop2 32 63  XMM mm sreg;
      assign_xmm gop1 32 63  gop2 32 63  XMM mm sreg;
      assign_xmm gop1 64 95  gop2 96 127 XMM mm sreg;
      assign_xmm gop1 96 127 gop2 96 127 XMM mm sreg;
    ]


let lift_psubb xmm mm gop1 gop2 sreg =
  let size = X86Util.bitsize_of_xmm_mm xmm in
  let gop1_expr = disas_expr_xmm gop1 xmm mm sreg in
  let gop2_expr = disas_expr_xmm gop2 xmm mm sreg in
  let open Dba in
  let rec assign acc off1 off2 =
    if off2 >= size then acc
    else
      let temp1 = Expr.restrict off1 off2 gop1_expr in
      let temp2 = Expr.restrict off1 off2 gop2_expr in
      let expr = Expr.sub temp1 temp2 in
      assign_xmm_expr gop1 off1 off2 expr xmm mm sreg
      @ assign acc (off1 + 8) (off2 + 8)
  in assign [] 0 8


let lift_psrl xmm size gop1 gop2 range sreg =
  let e_gop1 = disas_expr_xmm gop1 xmm size sreg in
  let count =  disas_expr_xmm gop2 xmm size sreg in
  let sz, max_count =
    match xmm with
    | MM  -> 64, range
    | XMM ->
      let max_count = if range = 63 then 15 else range in 128, max_count
  in
  let open Dba in
  match gop2  with
  | Imm i ->
    if Int64.to_int i > max_count
    then
      [ Predba.assign (disas_lval_xmm gop1 xmm size sreg) (Expr.zeros sz) ]
    else
      let rec assign acc off =
        if off >= sz then acc
        else
          let e =
            Expr.shift_right (Expr.restrict off (off + range) e_gop1 )
              (cst_of_int (Int64.to_int i) (range + 1))
          in assign_xmm_expr gop1 off (off + range) e xmm size sreg
             @ assign acc (off + range + 1)
      in assign [] 0
  | _ ->
    let cond = Expr.ule count (cst_of_int max_count sz) in
    [ Predba.conditional_jump cond (Dba.JInner 3);
      Predba.assign (disas_lval_xmm gop1 xmm size sreg) (Expr.zeros sz);
      Predba.static_jump (Dba.JInner (sz / (range + 1) + 3)) ]
    @
    let rec assign acc off =
      if off >= sz then acc
      else
        let r = off + range in
        let e =
          Expr.shift_right
            (Expr.restrict off r e_gop1)
            (Expr.restrict 0 range count) in
        assign_xmm_expr gop1 off r e xmm size sreg
        @ assign acc (r + 1)
    in assign [] 0



let lift_psll xmm size gop1 gop2 range sreg =
  let e_gop1 = disas_expr_xmm gop1 xmm size sreg in
  let count =  disas_expr_xmm gop2 xmm size sreg in
  let sz, max_count = X86Util.bitsize_of_xmm_mm xmm, range in
  let open Dba in
  match gop2  with
  | Imm i ->
    if Int64.to_int i > max_count
    then [ Predba.assign (disas_lval_xmm gop1 xmm size sreg) (Dba.Expr.zeros sz) ]
    else
      let rec assign acc off =
        if off >= sz
        then acc
        else
          let hi = off + range in
          let e =
            Expr.shift_left
              (Expr.restrict off hi e_gop1 )
              (cst_of_int (Int64.to_int i) (range + 1)) in
          assign_xmm_expr gop1 off hi e xmm size sreg @ assign acc (hi + 1)
      in assign [] 0
  | _ ->
    let cond = Expr.ule count (cst_of_int max_count sz) in
    [ Predba.conditional_jump cond (Dba.JInner 3);
      Predba.assign (disas_lval_xmm gop1 xmm size sreg) (Expr.zeros sz);
      Predba.static_jump (Dba.JInner (sz / (range + 1) + 3)) ]
    @
    let rec assign acc off =
      if off >= sz
      then acc
      else
        let hi = off + range in
        let e =
          Expr.shift_left
            (Expr.restrict off hi e_gop1 )
            (Expr.restrict 0 range count) in
        assign_xmm_expr gop1 off hi e xmm size sreg @ assign acc (hi + 1)
    in assign [] 0


let lift_psra xmm size gop1 gop2 range sreg =
  let open Dba in
  let e_gop1 = disas_expr_xmm gop1 xmm size sreg in
  let count =  disas_expr_xmm gop2 xmm size sreg in
  let sz, max_count = X86Util.bitsize_of_xmm_mm xmm, range in
  let rec assign rshift acc off =
    if off >= sz then acc
    else
      let roff = off + range in
      let e = Expr.(shift_right (restrict off roff e_gop1) rshift) in
      assign_xmm_expr gop1 off roff e xmm size sreg @
      assign rshift acc (roff + 1)
  in
  match gop2  with
  | Imm i ->
    if Int64.to_int i > max_count
    then [ Predba.assign (disas_lval_xmm gop1 xmm size sreg) (Expr.zeros sz) ]
    else assign (cst_of_int (Int64.to_int i) (range + 1)) [] 0
  | _ ->
    let cond = Expr.ule count (cst_of_int max_count sz) in
    Predba.conditional_jump cond (Dba.JInner 3) ::
    Predba.assign (disas_lval_xmm gop1 xmm size sreg) (Expr.zeros sz) ::
    Predba.assign (disas_lval_xmm gop2 xmm size sreg) (cst_of_int range sz) ::
    assign (Expr.restrict 0 range count) [] 0


let lift_ps_ldq shift gop1 imm sreg =
  let lval = disas_lval_xmm gop1 XMM S128 sreg in
  let mke = Dba.Expr.binary shift (disas_expr_xmm gop1 XMM S128 sreg) in
  let v = if imm > 15 then 128 else imm in
  [ Predba.assign lval (mke (cst_of_int v 128)) ]


let lift_ptest xmm size gop1 gop2 = affect_flags_ptest xmm size gop1 gop2


let lift_predicate p xmm mm lop rop sreg =
  let lvalue = disas_lval_xmm lop xmm mm sreg
  and l_e = disas_expr_xmm lop xmm mm sreg
  and r_e = disas_expr_xmm rop xmm mm sreg in
  let e = p l_e r_e in
  [ Predba.assign lvalue e ]


let lift_pxor = lift_predicate Dba.Expr.logxor
and lift_por  = lift_predicate Dba.Expr.logor
and lift_pand = lift_predicate Dba.Expr.logand
and lift_pandn =
  lift_predicate (fun le re -> Dba.Expr.(logand (lognot le) re))


let lift_punpcklbw xmm size gop1 gop2 step sreg =
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


let lift_pmaxu xmm size gop1 gop2 step sreg =
  let e1 = disas_expr_xmm gop1 xmm size sreg in
  let e2 = disas_expr_xmm gop2 xmm size sreg in
  let sz = X86Util.bitsize_of_xmm_mm xmm in
  let rec assign acc off id =
    if off >= sz
    then acc
    else
      let off2 = step + off in
      let restrict = Dba.Expr.restrict off off2 in
      let cond =
        Dba.Expr.ugt (restrict e1) (restrict e2) in
      Predba.conditional_jump cond (Dba.JInner id) ::
      assign_xmm gop1 off off2 gop2 off off2 xmm size sreg @
      assign acc (off2 + 1) (id + 2)
  in assign [] 0 2


let lift_pcmpgt xmm s_mode gop1 gop2 size sreg =
  let rec aux acc i j base_idx bound =
    if j >= bound then acc
    else
      let l1 n =
        Predba.conditional_jump
          Dba.Expr.(
            ugt (restrict i j (disas_expr_xmm gop1 xmm s_mode sreg))
              (restrict i j (disas_expr_xmm gop2 xmm s_mode sreg) ))
          (Dba.JInner (base_idx + n))
      in
      let l2 = assign_xmm_expr gop1 i j (maxi_bv size) xmm s_mode sreg in
      let l3 = assign_xmm_expr gop1 i j (Dba.Expr.zeros size) xmm s_mode
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
  let eax_size = Size.Bit.create 32 in
  Dba.LValue.restrict (X86Util.reg32_to_string EAX) eax_size

let al_lval = lv_restrict_eax 0 7
let ah_lval = lv_restrict_eax 8 15

let e_restrict_eax lo hi =
  Dba.Expr.(
    restrict lo hi
      (var (X86Util.reg32_to_string EAX) 32 None))

let al_expr = e_restrict_eax 0 7
let ah_expr = e_restrict_eax 8 15

let lift_aad imm =
  let open Dba in
  let e1 = Expr.mul ah_expr (cst_of_int imm 8) in
  let e2 = Expr.add al_expr e1 in
  let e = Expr.logand e2 (cst_of_int 255 8) in
  [
    Predba.assign al_lval e;
    Predba.assign ah_lval (Expr.zeros 8)
  ] @ affect_flags_aad al_expr


let lift_aam imm =
  let open Dba in
  let v = cst_of_int imm 8 in
  let e1 = Expr.udiv al_expr v in
  let e2 = Expr.umod al_expr v in
  [
    Predba.assign ah_lval e1;
    Predba.assign al_lval e2
  ] @ affect_flags_aad al_expr


let lift_address_mode = function
  | A32 -> 32, `M32
  | A16 -> 16, `M16


let lift_loop_cond cond a_mode src =
  let size, mode = lift_address_mode a_mode in
  [
    Predba.assign (lhs_of_reg ECX mode)
      (Dba.Expr.sub (expr_of_reg mode ECX) (cst_of_int 1 size));
    Predba.conditional_jump cond (strange_addr_of_int64 src)
  ]


let loop_cond_e a_mode =
  let size, mode = lift_address_mode a_mode in
  Dba.Expr.(diff (expr_of_reg mode ECX) (zeros size))


let lift_loop a_mode _mode src =
  let cond = loop_cond_e a_mode in
  lift_loop_cond cond a_mode src


let lift_loopz a_mode _mode src =
  let cond1 = loop_cond_e a_mode in
  let cond = Dba.Expr.logand zf_flag cond1 in
  lift_loop_cond cond a_mode src


let lift_loopnz a_mode _mode src =
  let open Dba in
  let cond1 = loop_cond_e a_mode in
  let cond2 = Expr.lognot zf_flag in
  let cond = Expr.logand cond1 cond2 in
  lift_loop_cond cond a_mode src


let lift_xlat addr_mode sreg =
  let open Dba in
  let al_expr = Expr.restrict 0 7 (expr_of_reg32 EAX)  in
  let e = Expr.uext 32 al_expr in
  let ebx_expr = expr_of_reg32 EBX in
  match addr_mode with
  | A32 ->
    [ Predba.assign al_lval (expr_of_mem8 (Expr.add ebx_expr e) ~sreg) ]
  | A16 ->
    let ebx_expr = Expr.restrict 0 15 ebx_expr  in
    let ebx_expr = Expr.uext 32 ebx_expr in
    [ Predba.assign al_lval (expr_of_mem8 (Expr.add ebx_expr e) ~sreg) ]


let lift_fxch float_register =
  let open Dba_types in
  let operand1_lhs = lhs_of_float_reg float_register in
  let operand1_expr = expr_of_float_reg float_register in
  let operand2_lhs = lhs_of_float_reg ST0 in
  let operand2_expr = expr_of_float_reg ST0 in
  let name = "temp80"
  and size = Size.Bit.create 80
  and vtag_opt = Some Dba.VarTag.temp in
  let temp_lhs = Dba.LValue.var name ~bitsize:size vtag_opt in
  let temp_expr = Expr.var name size vtag_opt in
  [ Predba.assign temp_lhs operand1_expr;
    Predba.assign operand1_lhs operand2_expr;
    Predba.assign operand2_lhs temp_expr
  ]

let lift_bts mode base offset sreg =
  let open Dba in
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
    let nbits = Size.Bit.create size in
    let tmp = Dba.LValue.temp nbits in
    let tmp_name = Utils.unsafe_get_opt (Dba_types.LValue.name_of tmp) in
    [
      Predba.assign tmp base_expr;
      Predba.assign (Dba.LValue.bit_restrict tmp_name nbits i) (cst_of_int 1 1);
      Predba.assign base_lhs (Dba_types.Expr.temp nbits);
    ]


let lift_btr mode base offset sreg =
  let open Dba in
  let twoPower n = Bigint.power_int_positive_int 2 n in
  let max_b n = Bigint.pred_big_int (twoPower n) in
  let size = size_mode mode in
  let bits = Size.Bit.create size in
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
    let c = Expr.equal offset max in
    let tmp = temp_size size in
    let last = size - 1 in
    [
      Predba.conditional_jump c (Dba.JInner 4);
      Predba.assign base_lhs op;
      Predba.static_jump (Dba.JInner 7);
      Predba.assign (Dba.LValue.temporary tmp bits) base_expr;
      Predba.assign (Dba.LValue.bit_restrict tmp bits last) Dba.Expr.zero;
      Predba.assign base_lhs (Expr.temporary tmp ~size)
    ]
  | Imm i ->
    let i = Int64.to_int i in
    let tmp = temp_size size in
    [
      Predba.assign (Dba.LValue.temporary tmp bits) base_expr;
      Predba.assign (Dba.LValue.bit_restrict tmp bits i) Dba.Expr.zero;
      Predba.assign base_lhs (Expr.temporary tmp ~size)
    ]


(* EFLAGS :

   |0|0|0|0|0|0|0|0|0|0|ID|VIP|VIF|AC|VM|RF|0|NT|IOPL(2)|OF|DF|IF|TF|SF|ZF|0|AF|0|PF|1|CF|*)

let lift_pushfd mode _sreg =
  let open Dba in
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in  *)
  let gen_code size =
    let nbits = Size.Bit.create size in
    let tmp_lhs = Dba.LValue.temp nbits in
    let tmp_exp = Dba_types.Expr.temp nbits in
    let zero2 = Expr.zeros 2 in
    let one = cst_of_int 1 1 in
    let flags = [of_flag; df_flag; zero2;
                 sf_flag; zf_flag; Dba.Expr.zero;
                 af_flag; Dba.Expr.zero ; pf_flag; one; cf_flag;] in
    let eflags = catenate_expressions flags in
    let eflags = Expr.uext size eflags in
    let cst = cst_of_int (nbytes_mode mode) 32 in
    [
      Predba.assign tmp_lhs eflags;
      Predba.assign (lhs_of_mem mode (Expr.sub esp_expr cst)) tmp_exp;
      Predba.assign esp_lval (Expr.sub esp_expr cst)
    ]
  in size_mode mode |> gen_code


let lift_popfd mode _sreg =
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in  *)
  let open Dba_types in
  let gen_code size =
    let nbits = Size.Bit.create size in
    let tmp_lhs = Dba.LValue.temp nbits in
    let tmp_exp = Expr.temp nbits in
    Predba.assign (tmp_lhs) (expr_of_mem mode esp_expr) ::
    Predba.assign (esp_lval)
      (Dba.Expr.add (expr_of_reg mode ESP)
         (cst_of_int (nbytes_mode mode) 32))
    ::
    List.map2
      (fun flag n ->
         Predba.assign (lhs_of_flag flag Dba.Flag.unspecified)
           (Dba.Expr.bit_restrict n tmp_exp ))
      [CF; PF; AF; ZF; SF; DF; OF;]
      [0;  2;  4;  6;  7;  10; 11;]
  in size_mode mode |> gen_code


let lift_lsl mode _src dst sreg =
  match mode with
  | `M16 | `M32 ->
    [
      Predba.non_deterministic (lhs_of_flag ZF Dba.Flag.unspecified) `Constant;
      Predba.non_deterministic (disas_lval dst mode sreg) `Constant
    ]
  | `M8 -> failwith "decode lsl with operands on 8 bits"



let lift_cmpXchg8b reg_t size_t gop sreg =
  match size_t with
  | S64 ->
    let open Dba in
    let dst_exp = disas_expr_xmm gop reg_t size_t sreg in
    let dst_lhs = disas_lval_xmm gop reg_t size_t sreg in
    let eax_expr = expr_of_reg32 EAX in
    let eax_lhs = lhs_of_reg32 EAX in
    let ebx_expr = expr_of_reg32 EBX in
    let ecx_expr = expr_of_reg32 ECX in
    let edx_expr = expr_of_reg32 EDX in
    let edx_lhs = lhs_of_reg32 EDX in
    let op  = Expr.append edx_expr eax_expr in
    let op2 = Expr.append ecx_expr ebx_expr in
    let c = Expr.equal op dst_exp in
    [
      Predba.conditional_jump c (Dba.JInner 5);
      Predba.assign (lhs_of_flag ZF Dba.Flag.unspecified) Dba.Expr.zero;
      Predba.assign eax_lhs (Expr.restrict 0 31 dst_exp);
      Predba.assign edx_lhs (Expr.restrict 32 63 dst_exp);
      Predba.static_jump (Dba.JInner 7);
      Predba.assign (lhs_of_flag ZF Dba.Flag.unspecified) Dba.Expr.one;
      Predba.assign dst_lhs op2
    ]
  | _ -> failwith "lift_cmpXchg8b ?"


let lift_lahf () =
  let eflags =
    [ sf_flag; zf_flag; Dba.Expr.zero; af_flag; Dba.Expr.zero;
      pf_flag; Dba.Expr.one; cf_flag;] in
  List.mapi
    (fun i e ->
       let nth_bit = 15 - i in
       let lv = lv_restrict_eax nth_bit nth_bit in
       Predba.assign lv e
    ) eflags


let lift_sahf =
  let flag_bit_alist = [SF, 15; ZF, 14; AF, 12; PF, 10; CF, 8; ] in
  let eax_expr = expr_of_reg32 EAX in
  List.map
    (fun (flag, eax_bit) ->
       let rval = Dba.Expr.bit_restrict eax_bit eax_expr in
       assign_flag flag rval)
    flag_bit_alist


let lift_salc () =
  [ Predba.assign (lhs_of_reg EAX `M8) (Dba.Expr.sext 8 cf_flag) ]

let lift_movzx ~signed mode r e =
  let ext = if signed then Dba.Expr.sext else Dba.Expr.uext in
  let sz = size_mode mode in
  [ Predba.assign (lhs_of_reg r mode) (ext sz e) ]

let lift_mov mode lop rop sreg =
  let lval = disas_lval lop mode sreg
  and rval = disas_expr rop mode sreg in
  [ Predba.assign lval rval ]


let lift_aas () =
  let open Dba in
  let set_af_cf e =
    [ assign_flag AF e;
      assign_flag CF e;
    ]
  in
  let eax_e = expr_of_reg32 EAX in
  let al_e = Expr.restrict 0 7 eax_e
  and ah_e = Expr.restrict 8 15 eax_e in
  let size = 8 in
  let byte_constant n =
    Expr.constant
      (Bitvector.create (Bigint.big_int_of_int n) size) in
  let v_6 = byte_constant 6 in
  let v_15 = byte_constant 15 in
  let check = Expr.logand al_e v_15 in
  let then_block = [
    Predba.assign al_lval (Expr.sub al_e v_6);
    Predba.assign ah_lval (Expr.sub ah_e (Expr.ones size));
  ] @ set_af_cf Expr.one
  and else_block = set_af_cf Expr.zero
  and last_instruction = [ Predba.assign al_lval check ]
  and cond =
    let loper = Expr.ugt check (byte_constant 9)in
    let roper = Expr.equal af_flag Expr.one in
    Expr.logor loper roper
  in
  Predba.conditional_jump cond (Jump_target.inner 4) ::
  else_block @
  [ Predba.static_jump (Jump_target.inner 8) ] @
  then_block @
  last_instruction


let lift_lea ~mode ~src ~dst sreg =
  let a = expr_of_addr src in
  let a = effective_address a sreg in
  let rv =
    match mode with
    | `M32 -> a
    | `M16 -> Dba.Expr.restrict 0 15 a
    | `M8 -> assert false
  in
  let lval = lhs_of_reg dst mode in
  [ Predba.assign lval rv ]


let is_aligned_address byte_boundary = function
  | { addrDisp; addrMode = _; addrBase = None; addrIndex = None;} ->
    let open Int64 in
    rem addrDisp (of_int byte_boundary) = zero
  | _ -> false


let lift_movaps ~src ~dst simd_sz sreg =
  let lval = disas_lval_xmm dst XMM simd_sz sreg in
  let e  =
    match src with
    | Address addr ->
      (* check that displacement is on a 16-byte boundary *)
      if not (is_aligned_address 16 addr) then
        Logger.warning "Address %a should be aligned on a 16 byte boundary"
          X86pp.pp_address addr;
      disas_expr_xmm src XMM simd_sz sreg

    | Imm _ -> assert false
    | Reg _ ->
      disas_expr_xmm src XMM simd_sz sreg
  in
  [ Predba.assign lval e ]

let lift_popcnt mode gop1 gop2 sreg =
  let egop2 = disas_expr gop2 mode sreg in
  let lgop1 = disas_lval gop1 mode sreg in
  let eres = res_expr mode in
  let lres = res_lhs mode in
  let size = size_mode mode in
  let init = Predba.assign lres (Dba.Expr.zeros size) in
  let final = [ clear_flag OF Dba.Flag.unspecified;
                clear_flag SF Dba.Flag.unspecified;
                update_ZF egop2 size Dba.Flag.unspecified;
                clear_flag AF Dba.Flag.unspecified;
                clear_flag PF Dba.Flag.unspecified;
                clear_flag CF Dba.Flag.unspecified;
                Predba.assign lgop1 eres ] in
  let rec unroll i sum =
    if i < 0 then sum
    else
      let open Dba in
      let rhs = Expr.add eres (Expr.uext size (Expr.bit_restrict i egop2)) in
      Predba.assign lres rhs :: sum |> unroll (i - 1) in
  init :: unroll (size - 1) final

let lift_mv_seg_left ~dst ~src sreg =
  let base_var =
    let name = (X86Util.segment_reg_to_string dst) ^ "_base" in
    Dba.LValue.var name ~bitsize:(Size.Bit.create 32) None
  in
  let value = (disas_expr16 src sreg) in
  (* Mask the 8 last bits. *)
  let index =
    Dba.Expr.logand value @@
      Dba.Expr.constant (Bitvector.create (Bigint.big_int_of_int 0xfffc) 16) in
  let gdt = Dba.Expr.var "gdt" 32 None in
  let ldt = Dba.Expr.var "ldt" 32 None in
  let cond = Dba.Expr.bit_restrict 2 value in
  let gdt_or_ldt = Dba.Expr.ite cond ldt gdt in
  let base_address = Dba.Expr.add gdt_or_ldt (Dba.Expr.uext 32 index) in

  let tmp_lv = Dba.LValue.temporary "temp32" (Size.Bit.create 32) in
  let tmp_rv = Dba.Expr.temporary ~size:32 "temp32"  in

  (* First chunk. *)
  let bits0_23 =
    let address =
      Dba.Expr.add tmp_rv
        (Dba.Expr.constant (Bitvector.create (Bigint.big_int_of_int 2) 32))
    in
    Dba.Expr.load (Size.Byte.create 3) Dba.LittleEndian address
  in

  let bits24_31 =
    let address =
      Dba.Expr.add tmp_rv
        (Dba.Expr.constant (Bitvector.create (Bigint.big_int_of_int 7) 32)) in
    Dba.Expr.load (Size.Byte.create 1) Dba.LittleEndian address in

  let bits = Dba.Expr.append bits24_31 bits0_23 in

  [ Predba.assign (lhs_of_seg dst) value;
    Predba.assign tmp_lv base_address;
    Predba.assign base_var bits
  ]

let clear_register mode reg =
  assign_register reg mode (Dba.Expr.zeros (size_mode mode))

let lift_mv_seg_right ~dst ~src sreg =
  (* Intel documentation on p.480 says:

     For the Pentium 4, Intel Xeon, and P6 family processors, the two high-order
     bytes are filled with zeros; for earlier 32-bit IA-32 processors, the two
     high order bytes are undefined.

     The encoding here will thus fill the high ordere bytes of reg with zeros if
     needed.
  *)
  let reg32 = match dst with
    | Reg r -> X86Util.reg16_to_reg32 r
    | _ -> failwith "Left hand side of mov must be a register" in
  [ clear_register `M32 reg32;
    Predba.assign (disas_lval16 dst sreg) (expr_of_seg src);
  ]

let instruction_to_dba rep sreg nextaddr opcode instruction =
  match instruction with
  | Push (mode, genop) -> lift_push mode genop sreg
  | PushS reg -> lift_pushS reg sreg
  | PushA mode ->  lift_pushA (mode:>X86Types.sizeMode) sreg
  | Pushfd mode -> lift_pushfd (mode:>X86Types.sizeMode) sreg
  | Pop (mode, genop) -> lift_pop mode genop sreg
  | PopS reg -> lift_popS reg sreg
  | PopA mode -> lift_popA (mode:>X86Types.sizeMode) sreg
  | Popfd mode -> lift_popfd (mode:>X86Types.sizeMode) sreg
  | Arith (mode, op, gop1, gop2) -> lift_arith mode op gop1 gop2 sreg
  | Aas -> lift_aas ()
  | Aad imm -> lift_aad imm
  | Aam imm -> lift_aam imm
  | Shift (mode, shift_op, gop32, gop8) ->
    lift_shift mode shift_op gop32 gop8 sreg
  | Rotate (mode, rotate_op, gop32, gop8) ->
    lift_rotate mode rotate_op gop32 gop8  sreg
  | Shiftd (mode, shift_op, gop1, gop2, gop8) ->
    lift_shiftd mode shift_op gop1 gop2 gop8 sreg
  | Cmp (mode, gop1, gop2) -> lift_cmp mode gop1 gop2 sreg
  | Cmps mode -> lift_cmps mode sreg
  | CmpXchg (mode, gop1, gop2) -> lift_cmpXchg mode gop1 gop2 sreg
  | Test (mode, gop1, gop2) -> lift_test mode gop1 gop2 sreg
  | Movd (xmm, pos, gop1, gop2) -> lift_movd xmm pos gop1 gop2 sreg
  | MovQ (xmm, mm, dst, src) ->
    assign_xmm_zero ~dst 0 63 ~src 0 63 xmm mm sreg
  | MovdQA (xmm, mm, gop1, gop2)
  | MovdQU (xmm, mm, gop1, gop2) -> assign_xmm gop1 0 127 gop2 0 127 xmm mm sreg

  | Movs mode -> lift_movs mode rep sreg
  | Lods mode -> lift_lods mode rep sreg
  | Stos mode -> lift_stos mode rep sreg
  | Scas mode -> lift_scas mode rep sreg
  | CMovcc (mode, cc, gop1, gop2) ->
    lift_cmovcc mode cc gop1 gop2 nextaddr sreg
  | Movaps (simd_sz, dst, src) ->
    lift_movaps ~src ~dst simd_sz sreg
  | Movlpd (mm, gop1, gop2) -> assign_xmm gop1 0 63 gop2 0 63 XMM mm sreg
  | Movlps (mm, gop1, gop2) -> assign_xmm gop1 0 63 gop2 0 63 XMM mm sreg
  | Movhlps (mm, gop1, gop2) -> assign_xmm gop1 0 63 gop2 64 127 XMM mm sreg
  | Movddup (mm, gop1, gop2) ->
    assign_xmm gop1 0 63 gop2 0 63 XMM mm sreg @
    assign_xmm gop2 64 127 gop2 0 63 XMM mm sreg
  | Movsldup (mm, dst, src) -> lift_movsldup mm ~dst ~src sreg
  | Palignr (xmm, mm, dst, src, imm) ->
    lift_palignr xmm mm ~dst ~src imm sreg
  | Pcmpeqb (xmm, mm, gop1, gop2) -> pcmpeq gop1 gop2 xmm mm 8 sreg
  | Pcmpeqw (xmm, mm, gop1, gop2) -> pcmpeq gop1 gop2 xmm mm 16 sreg
  | Pcmpeqd (xmm, mm, gop1, gop2) -> pcmpeq gop1 gop2 xmm mm 32 sreg
  | PmovMSKB (xmm, mm, gop1, gop2) -> pmovMSK gop1 gop2 xmm mm sreg
  | Pminub (xmm, mm, gop1 , gop2) -> pminu gop1 gop2 xmm mm 8 sreg
  | Pxor (xmm, mm, gop1, gop2) ->   lift_pxor xmm mm gop1 gop2 sreg
  | Por (xmm, mm, gop1, gop2) ->    lift_por xmm mm gop1 gop2 sreg
  | Pand (xmm, mm, gop1, gop2) ->   lift_pand xmm mm gop1 gop2 sreg
  | Pandn (xmm, mm, gop1, gop2) ->  lift_pandn xmm mm gop1 gop2 sreg
  | Pmaxub (xmm, mm, gop1, gop2) -> lift_pmaxu xmm mm gop1 gop2 7  sreg
  | Pmaxuw (xmm, mm, gop1, gop2) -> lift_pmaxu xmm mm gop1 gop2 15 sreg
  | Pmaxud (xmm, mm, gop1, gop2) -> lift_pmaxu xmm mm gop1 gop2 31 sreg
  | Mov (mode, gop1, gop2) ->
    lift_mov mode gop1 gop2 sreg
  | MovSegLeft (dst, src) -> lift_mv_seg_left ~dst ~src sreg

  | MovSegRight(dst, src) -> lift_mv_seg_right ~dst ~src sreg
  | Bt (mode, gop1, gop2) ->
    affect_flags_bt mode (disas_expr gop1 mode sreg) gop2 sreg
  | Bts (mode, gop1, gop2) ->
    affect_flags_bt mode (disas_expr gop1 mode sreg) gop2 sreg
    @ lift_bts mode gop1 gop2 sreg
  | Btr (mode, gop1, gop2) ->
    affect_flags_bt mode (disas_expr gop1 mode sreg) gop2 sreg
    @ lift_btr mode gop1 gop2 sreg

  | Movzx (mode, r, op8) ->
    lift_movzx ~signed:false mode r (disas_expr8 op8 sreg)
  | Movzx16 (mode, r, op16) ->
    lift_movzx ~signed:false mode r (disas_expr16 op16 sreg)
  | Movsx (mode, r, op8) ->
    lift_movzx ~signed:true mode r  (disas_expr8 op8 sreg)
  | Movsx16 (mode, r, op16) ->
    lift_movzx ~signed:true mode r  (disas_expr16 op16 sreg)

  | Leave -> lift_leave sreg
  | Lea (mode, dst, src) -> lift_lea ~mode ~src ~dst sreg
  | Jmp src ->  [ Predba.static_jump (strange_addr_of_int64 src) ]
  | DJmp gop -> [ Predba.dynamic_jump (disas_expr gop `M32 sreg) ]
  | Jcc (cc, src) ->
    [ Predba.conditional_jump (cond_of_cc cc) (strange_addr_of_int64 src) ]
  | Call src -> lift_call src nextaddr sreg
  | DCall gop -> lift_dcall gop nextaddr sreg
  | Ret -> lift_ret	sreg
  | Retf -> lift_retf sreg
  | Reti imm -> lift_reti imm sreg
  | Retfi imm -> lift_retfi imm sreg
  | SetCc (cc, dst) ->
    let lvalue = disas_lval8 dst sreg in
    let e =
      Dba.Expr.ite
        (cond_of_cc cc) (cst_of_int 1 8) (Dba.Expr.zeros 8) in
    [ Predba.assign lvalue e ]
  | Nop -> []
  | Wait -> []
  | Not (mode, gop) -> lift_not mode gop sreg
  | Neg (mode, gop) -> lift_neg mode gop sreg
  | Halt -> [ Predba.stop Dba.OK ]
  | Cmc
  | Clc -> [ assign_flag CF Dba.Expr.zero ]
  | Stc -> [ assign_flag CF Dba.Expr.one ]
  | Cld -> [ assign_flag DF Dba.Expr.zero ]
  | Std -> [ assign_flag DF Dba.Expr.one ]
  | Inc (mode, gop) -> lift_inc mode gop sreg
  | Dec (mode, gop) -> lift_dec mode gop sreg
  | Xchg (mode, gop1, gop2) -> lift_xchg mode gop1 gop2 sreg
  | Mul (mode, gop) -> lift_mul mode gop sreg
  | IMul (mode, gop) -> lift_imul mode gop sreg
  | IMul2 (mode, gop1, gop2) -> lift_imul2 mode gop1 gop2 sreg
  | IMul3 (mode, gop1, gop2, gop3) -> lift_imul3 mode gop1 gop2 gop3 sreg
  | Div (mode, gop) -> lift_div mode gop sreg
  | IDiv (mode, gop) -> lift_idiv mode gop sreg
  | CBW mode -> lift_cbw mode
  | CWD mode -> lift_cwd mode
  | Bsr (mode, r, gop) -> lift_bsr (mode:>X86Types.sizeMode) r gop sreg
  | Bsf (mode, r, gop) -> lift_bsf (mode:>X86Types.sizeMode) r gop sreg
  | Bswap (mode, r) -> lift_bswap mode r
  | Xadd (mode, gop1, gop2) -> lift_xadd mode gop1 gop2 sreg
  | Jcxz (mode, src) -> lift_jcxz mode src

  | CmpXchg8b (reg_t, size_t, gop) -> lift_cmpXchg8b reg_t size_t gop sreg

  | Pshufw (reg_t, size_t, r, gop, imm)  ->
    lift_pshuf reg_t size_t r gop imm 0 64 sreg
  | Pshuflw (reg_t, size_t, r, gop, imm) ->
    lift_pshuf reg_t size_t r gop imm 0 64 sreg
  | Pshufhw (reg_t, size_t, r, gop, imm) ->
    lift_pshuf reg_t size_t r gop imm 64 128 sreg
  | Pshufd (reg_t, size_t, r, gop, imm)  ->
    lift_pshuf reg_t size_t r gop imm 0 128 sreg
  | Movntq (xmm, size, gop1, gop2) ->
    let lval = disas_lval_xmm gop1 xmm size sreg
    and e = disas_expr_xmm gop2 xmm size sreg in
    [ Predba.assign lval e ]

  | Movhpd (mm, gop1, gop2)
  | Movhps (mm, gop1, gop2)
  | Movlhps (mm, gop1, gop2) -> assign_xmm gop1 64 127 gop2 0 63 XMM mm sreg

  | Movshdup (mm, gop1, gop2) -> lift_movshdup mm gop1 gop2 sreg
  | Psubb (xmm, mm, gop1, gop2) -> lift_psubb xmm mm gop1 gop2 sreg

  | Psrlw (xmm, size, gop1, gop2) -> lift_psrl xmm size gop1 gop2 15 sreg
  | Psrld (xmm, size, gop1, gop2) -> lift_psrl xmm size gop1 gop2 31 sreg
  | Psrlq (xmm, size, gop1, gop2) -> lift_psrl xmm size gop1 gop2 63 sreg

  | Psllw (xmm, size, gop1, gop2) -> lift_psll xmm size gop1 gop2 15 sreg
  | Pslld (xmm, size, gop1, gop2) -> lift_psll xmm size gop1 gop2 31 sreg
  | Psllq (xmm, size, gop1, gop2) -> lift_psll xmm size gop1 gop2 63 sreg

  | Psraw (xmm, size, gop1, gop2) -> lift_psra xmm size gop1 gop2 15 sreg
  | Psrad (xmm, size, gop1, gop2) -> lift_psra xmm size gop1 gop2 31 sreg

  | Psrldq (gop, imm) -> lift_ps_ldq Dba.Binary_op.RShiftU gop imm sreg
  | Pslldq (gop, imm) -> lift_ps_ldq Dba.Binary_op.LShift gop imm sreg

  | Ptest (xmm, size, gop1, gop2) -> lift_ptest xmm size gop1 gop2 sreg
  | Punpcklbw (xmm, size, gop1, gop2) ->
    lift_punpcklbw xmm size gop1 gop2 7 sreg
  | Punpcklwd (xmm, size, gop1, gop2) ->
    lift_punpcklbw xmm size gop1 gop2 15 sreg
  | Punpckldq (xmm, size, gop1, gop2) ->
    lift_punpcklbw xmm size gop1 gop2 31 sreg

  | Pcmpgtb (xmm, size, gop1, gop2) ->
    lift_pcmpgt xmm size gop1 gop2 8 sreg
  | Pcmpgtw (xmm, size, gop1, gop2) ->
    lift_pcmpgt xmm size gop1 gop2 1 sreg
  | Pcmpgtd (xmm, size, gop1, gop2) ->
    lift_pcmpgt xmm size gop1 gop2 32 sreg

  | Movups (gop1, gop2)
  | Movupd (gop1, gop2) ->
    [ Predba.assign (disas_lval_xmm gop1 XMM S128 sreg)
        (disas_expr_xmm gop2 XMM S128 sreg) ]

  | Loop (mode, addr_size, src) -> lift_loop addr_size mode src
  | Loopz (mode, addr_size, src) -> lift_loopz addr_size mode src
  | Loopnz (mode, addr_size, src) -> lift_loopnz addr_size mode src

  | Xlat addr_size -> lift_xlat addr_size sreg

  | Lsl (mode, src, dst) -> lift_lsl mode src dst sreg

  | Fld -> [ Predba.undefined (lhs_of_float_reg ST0) ]
  | Fxch float_register -> lift_fxch float_register

  | Lahf -> lift_lahf ()
  | Sahf -> lift_sahf
  | Salc -> lift_salc ()
  | Popcnt (mode, dest, src) -> lift_popcnt mode dest src sreg
  | Unsupported _ -> [Predba.stop (Dba.Unsupported opcode)]
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
  Dhunk.iter ~f:check dba_block;
  dba_instructions


let aux_decode2 basic_instr addr rep sreg =
  let open X86Instruction in
  let nextaddr =
    addr + (Size.Byte.to_int basic_instr.size)
    |> Dba_types.Caddress.block_start_of_int
  in
  aux_decode basic_instr.mnemonic nextaddr rep sreg basic_instr.opcode


let x86decode = X86decoder.read


let x86lift_from_reader addr reader =
  let binstr, rep, sreg = x86decode reader in
  (* convert x86 IR -> DBA *)
  let insnslst = aux_decode2 binstr addr rep sreg in
  let nextaddr =
    addr + (Size.Byte.to_int binstr.X86Instruction.size)
    |> Dba_types.Caddress.block_start_of_int
  in
  let block = Predba.blockify nextaddr insnslst in
  assert (Dhunk.Check.has_inbound_inner_jumps block);
  assert (Dhunk.Check.no_temporary_leak block);
  binstr, block


let decode reader (addr:Virtual_address.t) =
  x86lift_from_reader (addr:>int) reader

(* addr_size in Bytes *)
(* A base address of 0 is used here by default.
*)
let decode_binstream ?(base_addr:Virtual_address.t=(Virtual_address.create 0)) hopc  =
  try
    let base = (base_addr:>int) in
    Lreader.of_binstream ~base hopc |> x86lift_from_reader base
  with Failure s -> raise (InstructionUnhandled s)
