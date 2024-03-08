(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
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
open X86_options

exception InstructionUnhandled of string

let is_ignored_segment sreg =
  let segment_name = X86Util.segment_reg_to_string sreg in
  let handled_segments = X86_options.HandleSegments.get () in
  Basic_types.String.Set.mem segment_name handled_segments |> not

type instr_tbl = { mutable insertions : int; tbl : (string, unit) Hashtbl.t }

let create_instr_tbl () = { insertions = 0; tbl = Hashtbl.create 7 }

let _add instr_tbl instruction =
  instr_tbl.insertions <- instr_tbl.insertions + 1;
  Hashtbl.replace instr_tbl.tbl instruction ()

type instruction_stats = { handled : instr_tbl; unknown : instr_tbl }

let ( _add_unknown_instruction,
      _add_handled_instruction,
      handled_instructions,
      unknown_instructions,
      pp_unknown_instructions,
      native_instructions_decoded ) =
  let stats =
    { handled = create_instr_tbl (); unknown = create_instr_tbl () }
  in
  ( (fun s -> _add stats.handled s),
    (fun s -> _add stats.unknown s),
    (fun () -> (stats.handled.insertions, Hashtbl.length stats.handled.tbl)),
    (fun () -> (stats.unknown.insertions, Hashtbl.length stats.unknown.tbl)),
    (fun fmt () ->
      let open Format in
      fprintf fmt "@[<hov 0>";
      Hashtbl.iter (fun k _ -> fprintf fmt "%s;@ " k) stats.unknown.tbl;
      fprintf fmt "@]"),
    fun () -> stats.handled.insertions + stats.unknown.insertions )

let high_bit_8 = Int64.shift_left Int64.one 7
let higher_bit_8 = Int64.shift_left Int64.one 8
let high_bit_16 = Int64.shift_left Int64.one 15
let higher_bit_16 = Int64.shift_left Int64.one 16
let high_bit = Int64.shift_left Int64.one 31
let higher_bit = Int64.shift_left Int64.one 32
let temp_size size = Format.sprintf "temp%d" size
let cpt_size size = Format.sprintf "cpt%d" size
let size_mode m = match m with `M32 -> 32 | `M16 -> 16 | `M8 -> 8
let nbytes_mode m = match m with `M32 -> 4 | `M16 -> 2 | `M8 -> 1

let cst_of_int n size =
  let bv = Bitvector.create (Z.of_int n) size in
  Dba.Expr.constant bv

let four_32 = cst_of_int 4 32

let cst_of_int64_32 n =
  let size = 32 in
  if Int64.logand n high_bit = Int64.zero then
    let bv = Bitvector.create (Z.of_int64 n) size in
    Dba.Expr.constant bv
  else
    let bv =
      Bitvector.create (Z.of_int64 (Int64.neg (Int64.sub higher_bit n))) size
    in
    Dba.Expr.constant bv

let cst_of_int64_16 n =
  let size = 16 in
  if Int64.logand n high_bit_16 = Int64.zero then
    let bv = Bitvector.create (Z.of_int64 n) size in
    Dba.Expr.constant bv
  else
    let bv =
      Bitvector.create (Z.of_int64 (Int64.neg (Int64.sub higher_bit_16 n))) size
    in
    Dba.Expr.constant bv

let cst_of_int64_8 n =
  let size = 8 in
  if Int64.logand n high_bit_8 = Int64.zero then
    let bv = Bitvector.create (Z.of_int64 n) size in
    Dba.Expr.constant bv
  else
    let bv =
      Bitvector.create (Z.of_int64 (Int64.neg (Int64.sub higher_bit_8 n))) size
    in
    Dba.Expr.constant bv

let _is_too_big_shift cst size =
  match cst with
  | Dba.Expr.Cst bi -> Z.geq (Bitvector.value_of bi) (Z.of_int size)
  | _ -> false

let _addr_of_int n = (Bitvector.create (Z.of_int n) 32, 0)

let strange_bitvector_of_int64 n size =
  let v =
    if Int64.compare n Int64.zero = -1 then Int64.add higher_bit n else n
  in
  Bitvector.create (Z.of_int64 v) size

let _strange_cst_of_int64 n size =
  let bv = strange_bitvector_of_int64 n size in
  Dba.Expr.constant bv

let strange_addr_of_int64 n =
  let vaddr = Virtual_address.of_int64 n in
  Dba.JOuter (Dba_types.Caddress.block_start vaddr)

let offsets_of_reg8 r =
  let i = X86Util.reg8_to_int r in
  if i < 4 then (0, 7, X86Util.int_to_reg32 i)
  else (8, 15, X86Util.int_to_reg32 (i - 4))

let offsets_of_reg16 r =
  match r with
  | X86Types.AX -> (0, 15, X86Types.EAX)
  | X86Types.CX -> (0, 15, X86Types.ECX)
  | X86Types.DX -> (0, 15, X86Types.EDX)
  | X86Types.BX -> (0, 15, X86Types.EBX)
  | X86Types.SP -> (0, 15, X86Types.ESP)
  | X86Types.BP -> (0, 15, X86Types.EBP)
  | X86Types.SI -> (0, 15, X86Types.ESI)
  | X86Types.DI -> (0, 15, X86Types.EDI)

let offsets_of_reg16_32 = function
  | X86Types.EAX -> (0, 15, X86Types.EAX)
  | X86Types.ECX -> (0, 15, X86Types.ECX)
  | X86Types.EDX -> (0, 15, X86Types.EDX)
  | X86Types.EBX -> (0, 15, X86Types.EBX)
  | X86Types.ESP -> (0, 15, X86Types.ESP)
  | X86Types.EBP -> (0, 15, X86Types.EBP)
  | X86Types.ESI -> (0, 15, X86Types.ESI)
  | X86Types.EDI -> (0, 15, X86Types.EDI)

let offsets_of_reg8_32 r =
  match r with
  | X86Types.EAX -> (0, 7, X86Types.EAX)
  | X86Types.ECX -> (0, 7, X86Types.ECX)
  | X86Types.EDX -> (0, 7, X86Types.EDX)
  | X86Types.EBX -> (0, 7, X86Types.EBX)
  | X86Types.ESP -> (8, 15, X86Types.EAX) (* CHECK THAT *)
  | X86Types.EBP -> (8, 15, X86Types.ECX)
  | X86Types.ESI -> (8, 15, X86Types.EDX)
  | X86Types.EDI -> (8, 15, X86Types.EBX)

let lhs_of_seg s =
  Dba.LValue.var (X86Util.segment_reg_to_string s) ~bitsize:Size.Bit.bits16

let lhs_of_reg r mode =
  let bitsize = Size.Bit.bits32 in
  let mk_var name = Dba.LValue.var name ~bitsize in
  match mode with
  | `M32 -> X86Util.reg32_to_string r |> mk_var
  | `M16 ->
      let off1, off2, r32 = offsets_of_reg16_32 r in
      Dba.LValue._restrict (X86Util.reg32_to_string r32) bitsize off1 off2
  | `M8 ->
      let off1, off2, r32 = offsets_of_reg8_32 r in
      Dba.LValue._restrict (X86Util.reg32_to_string r32) bitsize off1 off2

let lhs_of_reg32 r =
  Dba.LValue.var (X86Util.reg32_to_string r) ~bitsize:Size.Bit.bits32

let lhs_of_reg16 r =
  let off1, off2, r32 = offsets_of_reg16 r in
  Dba.LValue._restrict (X86Util.reg32_to_string r32) Size.Bit.bits32 off1 off2

let lhs_of_reg8 r =
  let off1, off2, r32 = offsets_of_reg8 r in
  Dba.LValue._restrict (X86Util.reg32_to_string r32) Size.Bit.bits32 off1 off2

let edi_lval = lhs_of_reg32 EDI
let esi_lval = lhs_of_reg32 ESI
let esp_lval = lhs_of_reg32 ESP

let lhs_of_reg_xmm r reg_t =
  let open Size.Bit in
  match reg_t with
  | XMM -> Dba.LValue.var (X86Util.xmm_reg_to_string r) ~bitsize:bits128
  | MM ->
      Dba.LValue.var
        (X86Util.mm_reg_to_string (X86Util.xmm_reg_to_mm_reg r))
        ~bitsize:bits64

let bits80 = Size.Bit.create 80

let lhs_of_float_reg r =
  Dba.LValue.var (X86Util.float_reg_to_string r) ~bitsize:bits80

let _lhs_of_xmm_reg_restrict r off1 off2 =
  let open Size.Bit in
  Dba.LValue._restrict (X86Util.xmm_reg_to_string r) bits128 off1 off2

let lhs_of_flag f =
  Dba.LValue.var (X86Util.flag_to_string f)
    ~bitsize:Size.Bit.(if f = IOPL then create 2 else bits1)
    ~tag:Dba.Var.Tag.Flag

let undef_flag flag = Predba.undefined (lhs_of_flag flag)
let undef_flags flags = List.map undef_flag flags
let expr_of_seg s = Dba.Expr.var (X86Util.segment_reg_to_string s) 16

let expr_of_reg mode r =
  let open Dba.Expr in
  match mode with
  | `M32 -> Dba.Expr.var (X86Util.reg32_to_string r) 32
  | `M16 ->
      let off1, off2, r32 = offsets_of_reg16_32 r in
      restrict off1 off2 (var (X86Util.reg32_to_string r32) 32)
  | `M8 ->
      let off1, off2, r32 = offsets_of_reg8_32 r in
      restrict off1 off2 (var (X86Util.reg32_to_string r32) 32)

let expr_of_reg32 = expr_of_reg `M32

let expr_of_reg16 r =
  let off1, off2, r32 = offsets_of_reg16 r in
  Dba.Expr.(restrict off1 off2 (var (X86Util.reg32_to_string r32) 32))

let expr_of_reg8 r =
  let off1, off2, r32 = offsets_of_reg8 r in
  Dba.Expr.restrict off1 off2 (Dba.Expr.var (X86Util.reg32_to_string r32) 32)

let esp_expr = expr_of_reg32 ESP
and ebp_expr = expr_of_reg32 EBP
and esi_expr = expr_of_reg32 ESI
and edi_expr = expr_of_reg32 EDI

let e_of_reg addrMode r =
  let open Dba.Expr in
  match addrMode with
  | A32 -> var (X86Util.reg32_to_string r) 32
  | A16 ->
      let off1, off2, r32 = offsets_of_reg16_32 r in
      let e = restrict off1 off2 (var (X86Util.reg32_to_string r32) 32) in
      uext 32 e

(* FIXME: why is this conversion needed? *)
let expr_of_reg_xmm r xmm =
  let open Dba.Expr in
  match xmm with
  | XMM -> var (X86Util.xmm_reg_to_string r) 128
  | MM -> var (X86Util.xmm_reg_to_mm_reg r |> X86Util.mm_reg_to_string) 64

let expr_of_float_reg r = Dba.Expr.var (X86Util.float_reg_to_string r) 80

let expr_of_flag f =
  Dba_types.Expr.flag (X86Util.flag_to_string f) ~bits:Size.Bit.bits1

let cf_flag = expr_of_flag CF
let pf_flag = expr_of_flag PF
let af_flag = expr_of_flag AF
let zf_flag = expr_of_flag ZF
let sf_flag = expr_of_flag SF
let of_flag = expr_of_flag OF

type push_action =
  | U
  (* update flag *)
  | C
  (* always clear *)
  | K
(* keep unchanged *)

let eflags22, split_eflags =
  let flag =
    [| CF; PF; AF; ZF; SF; TF; IF; DF; OF; IOPL; NT; RF; VM; AC; VIF; VIP; ID |]
  and size = [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 2; 1; 1; 1; 1; 1; 1; 1 |]
  and position =
    [| 0; 2; 4; 6; 7; 8; 9; 10; 11; 12; 14; 16; 17; 18; 19; 20; 21 |]
  and action = [| U; U; U; U; U; U; U; U; U; U; U; C; K; U; K; K; U |]
  and tag = Dba.Var.Tag.Flag in
  let rec iter1 i e =
    if i = Array.length flag then e
    else
      iter1 (i + 1)
        Dba.Expr.(
          logor e
            (shift_left
               (uext 22 (var ~tag (X86Util.flag_to_string flag.(i)) size.(i)))
               (cst_of_int position.(i) 22)))
  and iter2 i l e =
    if i = Array.length flag || position.(i) + size.(i) > Dba.Expr.size_of e
    then l
    else
      iter2 (i + 1)
        (if action.(i) = K then l
         else
           let f =
             Dba.LValue.var ~tag
               (X86Util.flag_to_string flag.(i))
               ~bitsize:(Size.Bit.create size.(i))
           in
           let e =
             if action.(i) = C then Dba.Expr.zeros size.(i)
             else Dba.Expr.restrict position.(i) (position.(i) + size.(i) - 1) e
           in
           Predba.assign f e :: l)
        e
  in
  (iter1 0 (cst_of_int 2 22), iter2 0 [])

let expr_of_addr addr =
  let open Dba in
  let disp = addr.addrDisp in
  let expr_reg = e_of_reg addr.addrMode in
  match (disp = Int64.zero, addr.addrBase, addr.addrIndex) with
  | true, None, None -> Expr.zeros 32
  | true, Some r, None -> expr_reg r
  | true, Some r1, Some (sc, r2) ->
      let bop =
        Expr.mul (expr_reg r2) (cst_of_int (X86Util.scale_to_size sc) 32)
      in
      Expr.add (expr_reg r1) bop
  | true, None, Some (Scale1, r) -> expr_reg r
  | true, None, Some (sc, r) ->
      Expr.mul (expr_reg r) (cst_of_int (X86Util.scale_to_size sc) 32)
  | false, _, _ -> (
      match (addr.addrBase, addr.addrIndex) with
      | None, None -> cst_of_int64_32 disp
      | Some r, None -> Expr.add (expr_reg r) (cst_of_int64_32 disp)
      | Some r1, Some (Scale1, r2) ->
          Expr.add (expr_reg r1) (Expr.add (expr_reg r2) (cst_of_int64_32 disp))
      | Some r1, Some (sc, r2) ->
          let bop =
            Expr.mul (expr_reg r2) (cst_of_int (X86Util.scale_to_size sc) 32)
          in
          let bop = Expr.add bop (cst_of_int64_32 disp) in
          Expr.add (expr_reg r1) bop
      | None, Some (Scale1, r) -> Expr.add (expr_reg r) (cst_of_int64_32 disp)
      | None, Some (sc, r) ->
          let bop =
            Expr.mul (expr_reg r) (cst_of_int (X86Util.scale_to_size sc) 32)
          in
          Expr.add bop (cst_of_int64_32 disp))

let cond_of_cc cc =
  let open Dba in
  let open Expr in
  match (cc.truth_value, cc.condition) with
  | true, O -> of_flag
  | false, O -> lognot of_flag
  | true, B -> cf_flag
  | false, B -> lognot cf_flag
  | true, Z -> zf_flag
  | false, Z -> lognot zf_flag
  | true, BE -> logor cf_flag zf_flag
  | false, BE -> logand (lognot cf_flag) (lognot zf_flag)
  | true, S -> sf_flag
  | false, S -> lognot sf_flag
  | true, P -> pf_flag
  | false, P -> lognot pf_flag
  | true, L -> Expr.diff sf_flag of_flag
  | false, L -> Expr.equal sf_flag of_flag
  | true, LE -> logor zf_flag (Expr.diff sf_flag of_flag)
  | false, LE -> logand (lognot zf_flag) (Expr.equal sf_flag of_flag)

let retrieve_sreg a =
  let rec is_stack_access e sreg =
    match e with
    | Dba.(Expr.Var { name; _ }) ->
        if name = "ebp" || name = "esp" then SS else DS
    | Dba.Expr.Binary (_, e1, e2) ->
        let sreg = is_stack_access e1 sreg in
        is_stack_access e2 sreg
    | _ -> sreg
  in
  is_stack_access a DS

(* We represent the base of segments using a hidden register which is
   loaded only when the segment register changes, which corresponds to
   what the actual processor does. *)
let segment_address sreg a =
  if is_ignored_segment sreg then a
  else
    let open Dba in
    let name = X86Util.segment_reg_to_string sreg in
    let sreg = Expr.var (name ^ "_base") 32 in
    Expr.add sreg a

let effective_address a sreg =
  match sreg with
  | None ->
      if X86_options.ProtectedMode.get () then
        segment_address (retrieve_sreg a) a
      else a
  | Some sr ->
      let sr_name = X86Util.segment_reg_to_string sr in
      if
        X86_options.ProtectedMode.get ()
        || Basic_types.String.Set.mem sr_name
             (X86_options.HandleSegments.get ())
      then segment_address sr a
      else a

let lhs_of_mem mode ?(sreg = None) a =
  let a = effective_address a sreg in
  let nbytes = X86Util.bytesize_of_szmode mode in
  Dba.LValue.store nbytes Machine.LittleEndian a

let lhs_of_mem32 = lhs_of_mem `M32
and lhs_of_mem16 = lhs_of_mem `M16
and lhs_of_mem8 = lhs_of_mem `M8

let lhs_of_mem_xmm mm a sreg =
  let a = effective_address a sreg in
  let nbytes = X86Util.bytesize_of_simd_size mm in
  Dba.LValue.store nbytes Machine.LittleEndian a

let expr_of_mem (mode : X86Types.sizeMode) ?(sreg = None) a =
  let a = effective_address a sreg in
  let nbytes = X86Util.bytesize_of_szmode mode in
  Dba.Expr.load nbytes Machine.LittleEndian a

let expr_of_mem32 = expr_of_mem `M32
and expr_of_mem16 = expr_of_mem `M16
and expr_of_mem8 = expr_of_mem `M8

let expr_of_mem_xmm mm ?(sreg = None) a =
  let a = effective_address a sreg in
  let nbytes = X86Util.bytesize_of_simd_size mm in
  Dba.Expr.load nbytes Machine.LittleEndian a

let assign_flag flag e = Predba.assign (lhs_of_flag flag) e
let assign_register register mode e = Predba.assign (lhs_of_reg register mode) e

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
  | Imm i -> (
      match mode with
      | `M32 -> cst_of_int64_32 i
      | `M16 -> cst_of_int64_16 i
      | `M8 -> cst_of_int64_8 i)

let disas_expr_xmm op xmm_t size_t sreg =
  match op with
  | Reg r -> expr_of_reg_xmm r xmm_t
  | Address a -> expr_of_mem_xmm size_t ~sreg (expr_of_addr a)
  | Imm i ->
      assert (Int64.shift_right i 8 = Int64.zero);
      cst_of_int (Int64.to_int i)
        (Size.Bit.to_int (X86Util.bitsize_of_simd_size size_t))

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
  let bytesize = Natural.to_int Basic_types.Constants.bytesize in
  match lhs with
  | LValue.Store (sz, endian, e, _)
    when lo mod bytesize = 0 && hi mod bytesize = bytesize - 1 ->
      let sz' = Size.(Byte.of_bitsize (Bit.create (hi - lo + 1))) in
      let o' =
        match endian with
        | Machine.LittleEndian -> lo / bytesize
        | Machine.BigEndian -> (sz - hi) / bytesize
      in
      let e' =
        Expr.add e (Expr.constant (Bitvector.of_int ~size:(Expr.size_of e) o'))
      in
      let lval = LValue.store sz' endian e' in
      Predba.assign lval rhs
  | LValue.Store (sz, endian, e, _) when lo mod bytesize = 0 ->
      let top =
        Expr.(
          restrict (hi + 1)
            ((bytesize * sz) - 1)
            (load (Size.Byte.create sz) endian e))
      in
      let rhs = Expr.append top rhs in
      Predba.assign lhs rhs
  | LValue.Store (sz, endian, e, _) when hi mod bytesize = bytesize - 1 ->
      let bot =
        Expr.(restrict 0 (lo - 1) (load (Size.Byte.create sz) endian e))
      in
      let rhs = Expr.append rhs bot in
      Predba.assign lhs rhs
  | LValue.Store (sz, endian, e, _) ->
      let old = Expr.load (Size.Byte.create sz) endian e in
      let top = Expr.restrict (hi + 1) ((bytesize * sz) - 1) old in
      let bot = Expr.restrict 0 (lo - 1) old in
      let rhs = Expr.(append top (append rhs bot)) in
      Predba.assign lhs rhs
  | LValue.Var v as lval ->
      let lval =
        if hi - lo + 1 = v.size then lval else LValue.restrict v lo hi
      in
      Predba.assign lval rhs
  | Dba.LValue.Restrict (v, { Interval.lo = o1; Interval.hi = _ }) ->
      let lo = o1 + lo and hi = o1 + hi in
      Predba.assign (LValue.restrict v lo hi) rhs

let assign_xmm gop1 off1 off2 gop2 off3 off4 xmm mm sreg =
  let lhs = disas_lval_xmm gop1 xmm mm sreg in
  let rhs = Dba.Expr.restrict off3 off4 (disas_expr_xmm gop2 xmm mm sreg) in
  assign lhs rhs off1 off2

let assign_xmm_zero_ext ~dst off1 off2 ~src off3 off4 xmm mm sreg =
  match assign_xmm dst off1 off2 src off3 off4 xmm mm sreg with
  | Predba.Assign (Dba.LValue.Restrict (({ size; _ } as v), _), e) ->
      Predba.assign (Dba.LValue.v v) (Dba.Expr.uext size e)
  | res -> res

let assign_xmm_expr gop1 lo hi expr xmm mm sreg =
  let lhs = disas_lval_xmm gop1 xmm mm sreg in
  assign lhs expr lo hi

let assign_expr_expr gop1 off1 off2 expr _xmm _mm sreg =
  let lhs = disas_lval gop1 `M32 sreg in
  assign lhs expr off1 off2

let assign_expr_xmm gop1 off1 off2 gop2 off3 off4 xmm mm sreg =
  let lhs = disas_lval gop1 `M32 sreg in
  let rhs = Dba.Expr.restrict off3 off4 (disas_expr_xmm gop2 xmm mm sreg) in
  assign lhs rhs off1 off2

let clear_flag fl = Predba.assign (lhs_of_flag fl) (Dba.Expr.zeros 1)

let update_CF op1 op2 _res size op =
  let open Dba in
  match op with
  | Add ->
      let sz = size + 1 in
      let op1' = Expr.uext sz op1 in
      let op2' = Expr.uext sz op2 in
      let sum' = Expr.add op1' op2' in
      assign_flag CF (Expr.bit_restrict size sum')
  | Adc ->
      let sz = size + 1 in
      let op1' = Expr.uext sz op1 in
      let op2' = Expr.uext sz op2 in
      let cf_flag = Expr.uext sz cf_flag in
      let sum' = Expr.add (Expr.add op1' op2') cf_flag in
      assign_flag CF (Expr.bit_restrict size sum')
  | Sub ->
      (* carry = 1 <->  A <_u B *)
      assign_flag CF (Dba.Expr.ult op1 op2)
  | Sbb ->
      (* carry = 1 <->  A <_u B + CF *)
      (* carry = 1 ? A =<_u B : A <_u B *)
      let cf_true = Expr.ule op1 op2 in
      let cf_false = Expr.ult op1 op2 in
      assign_flag CF (Expr.ite cf_flag cf_true cf_false)
  | Xor | Or | And -> clear_flag CF

let update_OF op1 op2 res size op =
  let bit = size - 1 in
  let signbit1 = Dba.Expr.bit_restrict bit op1
  and signbit2 = Dba.Expr.bit_restrict bit op2
  and rres = Dba.Expr.bit_restrict bit res in
  match op with
  | Add | Adc ->
      (* ADC & ADD behaves the same for the OF flag *)
      (* ov=1 <->  A[n]=B[n] /\ A[n] \= (A+B)[n] *)
      assign_flag OF
        Dba.Expr.(logand (equal signbit1 signbit2) (diff signbit1 rres))
  | Sub ->
      (* ov=1 <->  A[n]\= B[n] /\ A[n] \= (A-B)[n] *)
      assign_flag OF
        Dba.Expr.(logand (diff signbit1 signbit2) (diff signbit1 rres))
  | Sbb ->
      (* ov=1 <->  A[n]\= B[n] /\ A[n] \= (A-B)[n] *)
      assign_flag OF
        Dba.Expr.(logand (diff signbit1 signbit2) (diff signbit1 rres))
  | Xor | Or | And -> clear_flag OF

let update_ZF res size =
  let open Dba in
  assign_flag ZF (Expr.equal res (Expr.zeros size))

let update_SF res size = assign_flag SF Dba.Expr.(slt res (zeros size))
(* Another way to update SF: Dba.ExprRestrict(res,size-1, size-1) *)

let update_PF res _size =
  let open Dba in
  let rec xor_sum acc i =
    if i < 8 then xor_sum Expr.(logxor acc (bit_restrict i res)) (i + 1)
    else acc
  in
  let e = Expr.(lognot (xor_sum (bit_restrict 0 res) 1)) in
  assign_flag PF e

let update_AF op1 op2 _res _size op =
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
  | Sub -> assign_flag AF (Dba.Expr.ult op1_res op2_res)
  | Sbb ->
      let open Dba in
      let cf_true = Expr.ule op1_res op2_res in
      let cf_false = Expr.ult op1_res op2_res in
      assign_flag AF (Expr.ite af_flag cf_true cf_false)
  | Xor | Or | And -> clear_flag AF

let affect_flags_inc op res size =
  [
    update_OF op (cst_of_int 1 32) res size Add;
    update_SF res size;
    update_AF op (cst_of_int 1 32) res size Add;
    update_PF res size;
    update_ZF res size;
  ]

let affect_flags_dec op res size =
  [
    update_OF op (cst_of_int 1 32) res size Sub;
    update_SF res size;
    update_AF op (cst_of_int 1 32) res size Sub;
    update_PF res size;
    update_ZF res size;
  ]

let affect_flags_arith op op1 op2 res size =
  match op with
  | Add ->
      [
        update_OF op1 op2 res size Add;
        update_SF res size;
        update_ZF res size;
        update_AF op1 op2 res size Add;
        update_PF res size;
        update_CF op1 op2 res size Add;
      ]
  | Adc ->
      [
        update_OF op1 op2 res size Adc;
        update_SF res size;
        update_ZF res size;
        update_AF op1 op2 res size Adc;
        update_PF res size;
        update_CF op1 op2 res size Adc;
      ]
  | Sub ->
      [
        update_OF op1 op2 res size Sub;
        update_SF res size;
        update_ZF res size;
        update_AF op1 op2 res size Sub;
        update_PF res size;
        update_CF op1 op2 res size Sub;
      ]
  | Sbb ->
      [
        update_OF op1 op2 res size Sbb;
        update_SF res size;
        update_ZF res size;
        update_AF op1 op2 res size Sbb;
        update_PF res size;
        update_CF op1 op2 res size Sbb;
      ]
  | And ->
      [
        update_OF op1 op2 res size And;
        update_SF res size;
        update_ZF res size;
        update_AF op1 op2 res size And;
        update_PF res size;
        update_CF op1 op2 res size And;
      ]
  | Or ->
      [
        update_OF op1 op2 res size Or;
        update_SF res size;
        update_ZF res size;
        update_AF op1 op2 res size Or;
        update_PF res size;
        clear_flag CF;
      ]
  | Xor ->
      [
        update_OF op1 op2 res size Xor;
        update_SF res size;
        update_ZF res size;
        update_AF op1 op2 res size Xor;
        update_PF res size;
        clear_flag CF;
      ]

let affect_flags_shift op expr shift res size =
  let open Dba in
  let shift = Expr.uext size (Expr.restrict 0 4 shift) in
  if Expr.(is_equal shift (zeros size)) then []
  else
    let one_s = Expr.ones size in
    let shift_minus_one = Expr.sub shift one_s in
    let last_bit =
      match op with
      | Shl -> Expr.(bit_restrict (size - 1) (shift_left expr shift_minus_one))
      | Shr | Sar -> Expr.(bit_restrict 0 (shift_right expr shift_minus_one))
    in
    let affect_flags =
      [
        update_SF res size;
        update_ZF res size;
        assign_flag CF last_bit;
        undef_flag AF;
      ]
    in
    if Expr.is_equal shift one_s then
      let overflow =
        match op with
        | Shl -> Expr.(logxor (bit_restrict (size - 1) res) last_bit)
        | Shr -> Expr.bit_restrict (size - 1) expr
        | Sar -> Expr.zero
      in
      assign_flag OF overflow :: affect_flags
    else Predba.undefined (lhs_of_flag OF) :: affect_flags

let affect_flags_rotate op _expr rop res size =
  let open Dba in
  let inst_OF =
    if Dba_types.Expr.is_one rop then
      let sz = size - 1 in
      assign_flag OF Expr.(logxor (bit_restrict sz res) cf_flag)
    else undef_flag OF
  in
  let assign_to_cf e = assign_flag CF e in
  match op with
  | Rol ->
      [
        assign_to_cf (Expr.bit_restrict 0 res);
        inst_OF;
        update_SF res size;
        update_ZF res size;
      ]
  | Ror ->
      [
        assign_to_cf (Expr.bit_restrict (size - 1) res);
        inst_OF;
        update_SF res size;
        update_ZF res size;
      ]
  | Rcl -> [ assign_to_cf (Expr.bit_restrict size res); inst_OF ]
  | Rcr -> [ inst_OF; assign_to_cf (Expr.bit_restrict size res) ]

let affect_flags_cmp op1 op2 res size =
  [
    update_OF op1 op2 res size Sub;
    update_SF res size;
    update_ZF res size;
    update_AF op1 op2 res size Sub;
    update_PF res size;
    update_CF op1 op2 res size Sub;
  ]

let affect_flags_test res size =
  [
    clear_flag OF;
    update_SF res size;
    Predba.undefined (lhs_of_flag AF);
    update_PF res size;
    update_ZF res size;
    clear_flag CF;
  ]

let affect_flags_mul res mode =
  let open Dba in
  let middle = size_mode mode in
  let res_res = Expr.restrict middle ((middle * 2) - 1) res in
  assign_flag OF Expr.(diff res_res (zeros (size_mode mode)))
  :: assign_flag CF of_flag
  :: undef_flags [ SF; ZF; AF; PF ]

let affect_flags_imul res mode =
  let open Dba.Expr in
  let middle = size_mode mode in
  let extended_res = sext (middle * 2) (restrict 0 (middle - 1) res) in
  assign_flag OF (diff res extended_res)
  :: assign_flag CF of_flag
  :: undef_flags [ SF; ZF; AF; PF ]

let affect_flags_div = undef_flags [ OF; SF; ZF; CF ]

let affect_flags_neg op res size =
  let open Dba.Expr in
  [
    assign_flag OF (equal op (constant (Bitvector.min_sbv size)));
    update_SF res size;
    update_ZF res size;
    assign_flag CF (diff op (zeros size));
  ]

let affect_flags_ptest xmm size op1 op2 sreg =
  let open Dba.Expr in
  let e1 = disas_expr_xmm op1 xmm size sreg in
  let e2 = disas_expr_xmm op2 xmm size sreg in
  let v = constant (Bitvector.zeros 128) in
  let c1 = equal (logand e1 e2) v in
  let c2 = equal (logand (lognot e1) e2) v in
  [ clear_flag OF; clear_flag SF; assign_flag ZF c1; assign_flag CF c2 ]

let affect_flags_aad res =
  [ undef_flag OF; update_SF res 8; update_ZF res 8; undef_flag CF ]

let pmovMSK gop1 gop2 xmm mm sreg =
  let rec aux acc i bound =
    if i >= bound then (* List_utils.rev_flatten *) List.rev acc
    else
      let v = (8 * i) + 7 in
      let l = assign_expr_xmm gop1 i i gop2 v v xmm mm sreg in
      aux (l :: acc) (i + 1) bound
  in
  let assign_expr lo hi =
    assert (lo < hi);
    assign_expr_expr gop1 lo hi (Dba.Expr.zeros (1 + hi - lo)) xmm mm sreg
  in
  let start = X86Util.bytesize_of_xmm_mm xmm in
  aux [] 0 start @ [ assign_expr start 31 ]

let mk_temp base size = (base ^ string_of_int size, size, Dba.Var.Tag.Temp)

let mk_lhs_temp base size =
  let name, sz, tag = mk_temp base size in
  let bitsize = Size.Bit.create sz in
  Dba.LValue.var name ~bitsize ~tag

let mk_res_temp base size =
  let name, sz, tag = mk_temp base size in
  Dba.Expr.var name sz ~tag

let res_lhs mode = mk_lhs_temp "res" (size_mode mode)
let temp_lhs mode = mk_lhs_temp "temp" (size_mode mode)
let res_expr mode = mk_res_temp "res" (size_mode mode)
let temp_expr mode = mk_res_temp "temp" (size_mode mode)

module type B = sig
  val base : string
  val tag : Dba.Var.Tag.t
end

module Bidirectional_name (X : B) = struct
  type t = { b_dst : Dba.LValue.t; b_src : Dba.Expr.t }

  open Size.Bit

  let mk =
    let i = ref 0 in
    fun bitsize ->
      incr i;
      let size = to_int bitsize in
      let name = Printf.sprintf "%s%d_%d" X.base !i size in
      (* dst is a L-value *)
      let b_dst = Dba.LValue.var name ~bitsize ~tag:X.tag in
      let b_src = Dba.Expr.var name size ~tag:X.tag in
      { b_dst; b_src }

  (* let mk8 ()  = mk bits8
   * let mk16 () = mk bits16
   * let mk32 () = mk bits32 *)

  let of_mode mode =
    let bitsize = X86Util.bitsize_of_szmode mode in
    mk bitsize

  let ( <-- ) t e = Predba.assign t.b_dst e
  let ( --> ) t lv = Predba.assign lv t.b_src
end

module Bidirectional_tmp = struct
  include Bidirectional_name (struct
    let base = "temp"
    let tag = Dba.Var.Tag.Temp
  end)
end

let lift_push mode genop sreg =
  let mem = Dba.Expr.sub esp_expr (cst_of_int (nbytes_mode mode) 32) in
  [
    Predba.assign (lhs_of_mem mode mem) (disas_expr genop mode sreg);
    Predba.assign esp_lval mem;
  ]

let lift_pushS reg _sreg =
  let e = Dba.Expr.sub esp_expr (cst_of_int 2 32) in
  [ Predba.assign (lhs_of_mem16 e) (expr_of_seg reg); Predba.assign esp_lval e ]

let lift_pushA mode sreg =
  let push_dec reg = lift_push mode (Reg reg) sreg in
  Predba.assign (temp_lhs mode) (expr_of_reg mode ESP)
  :: List_utils.flat_map push_dec [ EAX; ECX; EDX; EBX ]
  @
  let esp = esp_expr in
  let v = cst_of_int (nbytes_mode mode) 32 in
  let e = Dba.Expr.sub esp v in
  Predba.assign (lhs_of_mem mode e) (temp_expr mode)
  :: Predba.assign esp_lval e
  :: List_utils.flat_map push_dec [ EBP; ESI; EDI ]

let lift_pop_aux mode lval =
  let esp_contents = expr_of_mem mode esp_expr in
  [
    Predba.assign lval esp_contents;
    Predba.assign esp_lval
      (Dba.Expr.add esp_expr (cst_of_int (nbytes_mode mode) 32));
  ]

let lift_pop mode genop sreg =
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in  *)
  let lval = disas_lval genop mode sreg in
  match genop with
  | Reg ESP ->
      let esp_contents = expr_of_mem mode esp_expr in
      [ Predba.assign lval esp_contents ]
  | _ -> lift_pop_aux mode lval

let lift_popS reg _sreg =
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in  *)
  [
    Predba.assign (lhs_of_seg reg) (expr_of_mem16 esp_expr);
    Predba.assign esp_lval (Dba.Expr.add esp_expr (cst_of_int 2 32));
  ]

let lift_popA mode sreg =
  let word_size = (* in byte *) nbytes_mode mode in
  let aux_popa (n, register) =
    let lval = disas_lval (Reg register) mode sreg in
    let byte_offset = cst_of_int (n * word_size) 32 in
    let rval = Dba.Expr.add esp_expr byte_offset |> expr_of_mem ~sreg mode in
    Predba.assign lval rval
  in
  List.map aux_popa
    [ (0, EDI); (1, ESI); (2, EBP); (4, EBX); (5, EDX); (6, ECX); (7, EAX) ]
  @ [
      Predba.assign esp_lval
        (Dba.Expr.add esp_expr (cst_of_int (8 * word_size) 32));
    ]

let lift_arith mode op gop1 gop2 sreg =
  let disassed_gop1 = disas_expr gop1 mode sreg in
  let disassed_gop2 = disas_expr gop2 mode sreg in
  let add_carry e =
    let open Dba in
    Expr.add e (Expr.uext (size_mode mode) cf_flag)
  in
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
    | Or -> Dba.Expr.logor disassed_gop1 disassed_gop2
    | Xor -> Dba.Expr.logxor disassed_gop1 disassed_gop2
  in
  let res = res_expr mode in
  Predba.assign (res_lhs mode) rhs
  :: affect_flags_arith op disassed_gop1 disassed_gop2 res (size_mode mode)
  @ [ Predba.assign (disas_lval gop1 mode sreg) (res_expr mode) ]

let lift_shift mode shift_op gop32 gop8 sreg =
  let open Dba in
  let size = size_mode mode in
  let disassed_gop8 = disas_expr8 gop8 sreg in
  let disassed_gop32 = disas_expr gop32 mode sreg in
  let shift = Expr.uext size (Expr.restrict 0 4 disassed_gop8) in
  let dba_op =
    match shift_op with
    | Shl -> Expr.shift_left
    | Shr -> Expr.shift_right
    | Sar -> Expr.shift_right_signed
  in
  let res = res_expr mode in
  Predba.assign (res_lhs mode) (dba_op disassed_gop32 shift)
  :: affect_flags_shift shift_op disassed_gop32 disassed_gop8 res
       (size_mode mode)
  @ [ Predba.assign (disas_lval gop32 mode sreg) (res_expr mode) ]

let lift_rotate mode rotate_op gop32 gop8 sreg =
  let open Dba in
  let size = size_mode mode in
  let sz = size + 1 in
  let bits = Size.Bit.create sz in
  let tmp = temp_size sz in
  let res_lhs_1 = LValue.temporary tmp bits in
  let res_expr_1 = Expr.temporary tmp ~size:sz in
  (* let res33_expr = Dba.ExprVar ("res33", 33, Some Dba.Temp) in *)
  let dba_op, dba_gop1, dba_gop2, size =
    match rotate_op with
    | Rol ->
        ( Dba.Binary_op.LeftRotate,
          disas_expr gop32 mode sreg,
          res_expr mode,
          size )
    | Rcl ->
        let tmp = Expr.append cf_flag (disas_expr gop32 mode sreg) in
        ( Dba.Binary_op.LeftRotate,
          tmp,
          Expr.restrict 0 (size - 1) res_expr_1,
          sz )
    | Ror ->
        ( Dba.Binary_op.RightRotate,
          disas_expr gop32 mode sreg,
          res_expr mode,
          size )
    | Rcr ->
        let tmp = Expr.append cf_flag (disas_expr gop32 mode sreg) in
        ( Dba.Binary_op.RightRotate,
          tmp,
          Expr.restrict 0 (size - 1) res_expr_1,
          sz )
  in
  let count = Expr.uext size (disas_expr8 gop8 sreg) in
  match rotate_op with
  | Rol | Ror ->
      let count_msb = match mode with `M8 -> 2 | `M16 -> 3 | `M32 -> 4 in
      let rop = Expr.(uext size (restrict 0 count_msb count)) in
      let res = res_expr mode in
      Predba.assign (res_lhs mode) (Expr.binary dba_op dba_gop1 rop)
      :: affect_flags_rotate rotate_op (disas_expr gop32)
           (disas_expr8 gop8 sreg) res (size_mode mode)
      @ [ Predba.assign (disas_lval gop32 mode sreg) dba_gop2 ]
  | Rcl | Rcr ->
      let rop = Expr.umod count (cst_of_int size size) in
      Predba.assign res_lhs_1 (Expr.binary dba_op dba_gop1 rop)
      :: affect_flags_rotate rotate_op (disas_expr gop32)
           (disas_expr8 gop8 sreg) res_expr_1 (size_mode mode)
      @ [ Predba.assign (disas_lval gop32 mode sreg) dba_gop2 ]

let lift_shiftd mode shift_op dst src gop8 sreg =
  let open Dba in
  let size = size_mode mode in
  let esrc = disas_expr src mode sreg in
  let edst = disas_expr dst mode sreg in
  let ldst = disas_lval dst mode sreg in
  match disas_expr8 gop8 sreg with
  | Expr.Cst bv when Bitvector.ugt bv @@ Bitvector.of_int ~size:8 size ->
      Predba.undefined ldst :: undef_flags [ CF; OF; SF; AF; PF ]
  | count ->
      let count = Expr.(uext (2 * size) (restrict 0 4 count)) in
      let shift =
        match shift_op with
        | Shld -> Expr.(shift_left (append edst esrc))
        | Shrd -> Expr.(shift_right (append esrc edst))
      in
      let select =
        match shift_op with
        | Shld -> Expr.(restrict size ((2 * size) - 1) (shift count))
        | Shrd -> Expr.(restrict 0 (size - 1) (shift count))
      in
      let lres = res_lhs mode and eres = res_expr mode in
      let instrs = [ Predba.assign ldst eres ] in
      let instrs =
        if Expr.(is_equal count (zeros (2 * size))) then instrs
        else if Expr.(is_equal count (ones (2 * size))) then
          assign_flag CF
            (match shift_op with
            | Shld -> Expr.bit_restrict (size - 1) edst
            | Shrd -> Expr.bit_restrict 0 edst)
          :: update_SF eres size :: update_ZF eres size :: update_PF eres size
          :: assign_flag OF
               Expr.(
                 diff
                   (bit_restrict (size - 1) edst)
                   (bit_restrict (size - 1) eres))
          :: undef_flag AF :: instrs
        else if Expr.is_constant count then
          assign_flag CF
            (let count = Expr.(sub count (ones (2 * size))) in
             match shift_op with
             | Shld -> Expr.(bit_restrict ((2 * size) - 1) (shift count))
             | Shrd -> Expr.(bit_restrict 0 (shift count)))
          :: update_SF eres size :: update_ZF eres size :: update_PF eres size
          :: undef_flag OF :: undef_flag AF :: instrs
        else
          Predba.conditional_jump
            Expr.(ugt count (cst_of_int size (2 * size)))
            (JInner 10)
          :: Predba.conditional_jump
               Expr.(equal count (zeros (2 * size)))
               (JInner 18)
          :: assign_flag CF
               (let count = Expr.(sub count (ones (2 * size))) in
                match shift_op with
                | Shld -> Expr.(bit_restrict ((2 * size) - 1) (shift count))
                | Shrd -> Expr.(bit_restrict 0 (shift count)))
          :: update_SF eres size :: update_ZF eres size :: update_PF eres size
          :: Predba.conditional_jump
               Expr.(ugt count (ones (2 * size)))
               (JInner 15)
          :: assign_flag OF
               Expr.(
                 diff
                   (bit_restrict (size - 1) edst)
                   (bit_restrict (size - 1) eres))
          :: Predba.static_jump (JInner 16)
          :: Predba.undefined lres :: undef_flag CF :: undef_flag SF
          :: undef_flag ZF :: undef_flag PF :: undef_flag OF :: undef_flag AF
          :: instrs
      in
      Predba.assign lres select :: instrs

let lift_cmp mode gop1 gop2 sreg =
  let rhs =
    Dba.Expr.sub (disas_expr gop1 mode sreg) (disas_expr gop2 mode sreg)
  in
  let res = res_expr mode in
  Predba.assign (res_lhs mode) rhs
  :: affect_flags_cmp
       (disas_expr gop1 mode sreg)
       (disas_expr gop2 mode sreg)
       res (size_mode mode)

let lift_cmpXchg mode gop1 gop2 sreg =
  let eax_expr = expr_of_reg mode EAX in
  let e1 = disas_expr gop1 mode sreg in
  let res = res_expr mode in
  let open Dba in
  Predba.assign (res_lhs mode) (Expr.sub eax_expr e1)
  :: affect_flags_cmp
       (disas_expr gop1 mode sreg)
       (expr_of_reg mode EAX) res (size_mode mode)
  @ [
      Predba.conditional_jump (Expr.equal eax_expr e1) (Dba.JInner 10);
      Predba.assign (lhs_of_reg EAX mode) e1;
      Predba.static_jump (Dba.JInner 11);
      Predba.assign (disas_lval gop1 mode sreg) (disas_expr gop2 mode sreg);
    ]

let lift_test mode gop1 gop2 sreg =
  let rhs =
    Dba.Expr.logand (disas_expr gop1 mode sreg) (disas_expr gop2 mode sreg)
  in
  let res = res_expr mode in
  Predba.assign (res_lhs mode) rhs :: affect_flags_test res (size_mode mode)

let lift_movd xmm pos gop1 gop2 sreg =
  match (pos, xmm) with
  | Left, MM ->
      [
        assign_xmm_expr gop1 0 31 (disas_expr gop2 `M32 sreg) xmm S32 sreg;
        assign_xmm_expr gop1 32 63 (Dba.Expr.zeros 32) xmm S32 sreg;
      ]
  | Left, XMM ->
      [
        assign_xmm_expr gop1 0 31 (disas_expr gop2 `M32 sreg) xmm S32 sreg;
        assign_xmm_expr gop1 32 127 (Dba.Expr.zeros 96) xmm S32 sreg;
      ]
  | Right, _ ->
      [
        Predba.assign
          (disas_lval gop2 `M32 sreg)
          (Dba.Expr.restrict 0 31 (disas_expr_xmm gop1 xmm S32 sreg));
      ]

let mk_rhs_reg mode ereg =
  let cst = cst_of_int (nbytes_mode mode) 32 in
  Dba.(Expr.ite (expr_of_flag DF) (Expr.sub ereg cst) (Expr.add ereg cst))

let repeat_instrs rep addr nextaddr l =
  match rep with
  | NoRep -> l
  | _ ->
      let ecx = expr_of_reg32 ECX in
      let zf = expr_of_flag ZF in
      let pre_l =
        Predba.conditional_jump
          Dba.Expr.(equal ecx (zeros 32))
          (Dba.JOuter nextaddr)
      in
      let self = Dba.JOuter (Dba_types.Caddress.of_virtual_address addr) in
      let post_l =
        [
          Predba.assign (lhs_of_reg32 ECX) (Dba.Expr.sub ecx (cst_of_int 1 32));
          (match rep with
          | NoRep -> assert false
          | Rep -> Predba.static_jump self
          | RepE -> Predba.conditional_jump zf self
          | RepNE -> Predba.conditional_jump (Dba.Expr.lognot zf) self);
        ]
      in
      (* assert l does not contain Jump instructions *)
      pre_l :: (l @ post_l)

let lift_movs mode rep addr nextaddr sreg =
  (* Segment register for ESI can be overriden : see 3-489 *)
  let esi_reg = sreg
  (* Segment register for EDI is fixed : see 3-489 *)
  and edi_reg = Some X86Types.ES in
  repeat_instrs rep addr nextaddr
  @@ [
       Predba.assign
         (lhs_of_mem mode edi_expr ~sreg:edi_reg)
         (expr_of_mem mode esi_expr ~sreg:esi_reg);
       Predba.assign esi_lval (mk_rhs_reg mode esi_expr);
       Predba.assign edi_lval (mk_rhs_reg mode edi_expr);
     ]

let lift_lods mode rep addr nextaddr sreg =
  repeat_instrs rep addr nextaddr
  @@ [
       Predba.assign (lhs_of_reg EAX mode) (expr_of_mem mode esi_expr ~sreg);
       Predba.assign esi_lval (mk_rhs_reg mode esi_expr);
     ]

let lift_stos mode rep addr nextaddr sreg =
  repeat_instrs rep addr nextaddr
  @@ [
       Predba.assign (lhs_of_mem mode edi_expr ~sreg) (expr_of_reg mode EAX);
       Predba.assign edi_lval (mk_rhs_reg mode edi_expr);
     ]

let lift_cmps mode rep addr nextaddr sreg =
  let sreg = match sreg with None -> Some ES | _ -> sreg in
  let gop1 = expr_of_mem mode esi_expr ~sreg in
  let gop2 = expr_of_mem mode edi_expr ~sreg in
  let res = res_expr mode in
  repeat_instrs rep addr nextaddr
  @@ Predba.assign (res_lhs mode) (Dba.Expr.sub gop1 gop2)
     :: affect_flags_cmp gop1 gop2 res (size_mode mode)
  @ [
      Predba.assign esi_lval (mk_rhs_reg mode esi_expr);
      Predba.assign edi_lval (mk_rhs_reg mode edi_expr);
    ]

let lift_scas mode rep addr nextaddr sreg =
  let sreg = match sreg with None -> Some ES | _ -> sreg in
  let res = res_expr mode in
  repeat_instrs rep addr nextaddr
  @@ Predba.assign (res_lhs mode)
       (Dba.Expr.sub (expr_of_reg mode EAX) (expr_of_mem mode edi_expr ~sreg))
     :: affect_flags_cmp (expr_of_reg mode EAX)
          (expr_of_mem mode edi_expr ~sreg)
          res (size_mode mode)
  @ [ Predba.assign edi_lval (mk_rhs_reg mode edi_expr) ]

let lift_cmovcc mode cc gop1 gop2 _ sreg =
  let cond = cond_of_cc cc in
  let lhs = disas_lval gop1 mode sreg in
  let new_expr = disas_expr gop2 mode sreg in
  let old_expr = disas_expr gop1 mode sreg in
  [ Predba.assign lhs (Dba.Expr.ite cond new_expr old_expr) ]

let lift_movsldup mm ~dst ~src sreg =
  [
    assign_xmm dst 0 31 src 0 31 XMM mm sreg;
    assign_xmm dst 32 63 src 0 31 XMM mm sreg;
    assign_xmm dst 64 95 src 64 95 XMM mm sreg;
    assign_xmm dst 96 127 src 64 95 XMM mm sreg;
  ]

let lift_palignr xmm mm ~dst ~src imm sreg =
  let hi = X86Util.bitsize_of_xmm_mm xmm - 1 in
  let combined =
    Dba.Expr.append
      (disas_expr_xmm dst xmm mm sreg)
      (disas_expr_xmm src xmm mm sreg)
  in
  let restricted = Dba.Expr.restrict (8 * imm) ((8 * imm) + hi) combined in
  [ assign_xmm_expr dst 0 hi restricted xmm mm sreg ]

let lift_enter mode alloc level =
  let level = level mod 32 in
  let nbytes = nbytes_mode mode and nbits = size_mode mode in
  let tail =
    assign_register EBP mode (expr_of_reg mode ESP)
    ::
    (match alloc + (level * nbytes) with
    | 0 -> []
    | alloc ->
        [
          assign_register ESP mode
            (Dba.Expr.sub (expr_of_reg mode ESP) (cst_of_int alloc nbits));
        ])
  in
  Predba.assign esp_lval (Dba.Expr.sub esp_expr (cst_of_int nbytes 32))
  :: Predba.assign (lhs_of_mem mode esp_expr) (expr_of_reg mode EBP)
  ::
  (if level = 0 then tail
   else
     let addr = Dba.Expr.sub esp_expr (cst_of_int (nbytes * level) 32) in
     let tail = Predba.assign (lhs_of_mem mode addr) esp_expr :: tail in
     if level = 1 then tail
     else
       let addr = Dba.Expr.sub esp_expr (cst_of_int nbytes 32) in
       let lval =
         Dba.LValue.store
           (Size.Byte.create (nbytes * (level - 1)))
           Machine.LittleEndian addr
       in
       let addr = Dba.Expr.sub ebp_expr (cst_of_int nbytes 32) in
       let rval =
         Dba.Expr.load
           (Size.Byte.create (nbytes * (level - 1)))
           Machine.LittleEndian addr
       in
       Predba.assign lval rval :: tail)

let lift_leave mode =
  [
    assign_register ESP mode (expr_of_reg mode EBP);
    assign_register EBP mode (expr_of_mem mode esp_expr);
    Predba.assign esp_lval
      (Dba.Expr.add esp_expr (cst_of_int (nbytes_mode mode) 32));
  ]

let lift_call src nextaddr _sreg =
  let open Dba in
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in *)
  [
    Predba.assign esp_lval (Expr.sub esp_expr four_32);
    Predba.assign (lhs_of_mem32 esp_expr)
      (Expr.constant
         (Bitvector.create (Virtual_address.to_bigint nextaddr.Dba.base) 32));
    (let tag = Some (Dba.Call nextaddr) in
     Predba.static_jump (strange_addr_of_int64 src) ?tag);
  ]

let lift_dcall gop nextaddr sreg =
  let open Dba in
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in *)
  let temp32_lv = LValue.var ~bitsize:Size.Bit.bits32 "temp32" in
  let temp32_rv = Expr.var "temp32" 32 in
  [
    Predba.assign temp32_lv (disas_expr gop `M32 sreg);
    Predba.assign esp_lval (Expr.sub esp_expr four_32);
    Predba.assign (lhs_of_mem32 esp_expr)
      (Expr.constant
         (Bitvector.create (Virtual_address.to_bigint nextaddr.Dba.base) 32));
    Predba.dynamic_jump temp32_rv ~tag:(Dba.Call nextaddr);
  ]

let lift_ret _sreg =
  [
    Predba.assign esp_lval (Dba.Expr.add esp_expr four_32);
    Predba.dynamic_jump
      (expr_of_mem32 (Dba.Expr.sub esp_expr four_32))
      ~tag:Dba.Return;
  ]

let lift_reti imm _sreg =
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in *)
  let v = cst_of_int (4 + imm) 32 in
  [
    Predba.assign esp_lval (Dba.Expr.add esp_expr v);
    Predba.dynamic_jump
      (expr_of_mem32 (Dba.Expr.sub esp_expr v))
      ~tag:Dba.Return;
  ]

let lift_retf _sreg =
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in   *)
  let open Dba in
  [
    Predba.assign esp_lval (Expr.add esp_expr four_32);
    Predba.assign (lhs_of_seg CS) (expr_of_mem16 esp_expr);
    Predba.assign esp_lval (Expr.add esp_expr (cst_of_int 2 32));
    Predba.dynamic_jump
      (expr_of_mem32 (Expr.sub esp_expr (cst_of_int 6 32)))
      ~tag:Dba.Return;
  ]

let lift_retfi imm _sreg =
  let open Dba in
  let add_esp = Expr.add esp_expr in
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in *)
  [
    Predba.assign esp_lval (add_esp (cst_of_int (4 + imm) 32));
    Predba.assign (lhs_of_seg CS) (expr_of_mem16 esp_expr);
    Predba.assign esp_lval (add_esp (cst_of_int 2 32));
    Predba.dynamic_jump
      (expr_of_mem32 (Expr.sub esp_expr (cst_of_int (6 + imm) 32)))
      ~tag:Dba.Return;
  ]

let lift_not mode gop sreg =
  (* let mask =  *)
  (*   match mode with  *)
  (*   | `M32 -> cst_of_int64_32 (Int64.of_string "0xffffffff") *)
  (*   | `M16 -> cst_of_int64_16 (Int64.of_string "0xffff")  *)
  (*   | `M8 -> cst_of_int64_8 (Int64.of_string "0xff")  *)
  (* in *)
  [
    Predba.assign (disas_lval gop mode sreg)
      (Dba.Expr.lognot (disas_expr gop mode sreg));
  ]

let lift_neg mode gop sreg =
  Predba.assign (res_lhs mode) (Dba.Expr.uminus (disas_expr gop mode sreg))
  :: affect_flags_neg (disas_expr gop mode sreg) (res_expr mode)
       (size_mode mode)
  @ [ Predba.assign (disas_lval gop mode sreg) (res_expr mode) ]

let lift_inc mode gop sreg =
  let open Dba in
  let rmode = res_expr mode in
  Predba.assign (res_lhs mode)
    (Expr.add (disas_expr gop mode sreg) (Expr.ones (size_mode mode)))
  :: affect_flags_inc (disas_expr gop mode sreg) rmode (size_mode mode)
  @ [ Predba.assign (disas_lval gop mode sreg) rmode ]

let lift_dec mode gop sreg =
  let rmode = res_expr mode in
  let open Dba in
  Predba.assign (res_lhs mode)
    (Expr.sub (disas_expr gop mode sreg) (Expr.ones (size_mode mode)))
  :: affect_flags_dec (disas_expr gop mode sreg) rmode (size_mode mode)
  @ [ Predba.assign (disas_lval gop mode sreg) rmode ]

let lift_xchg mode gop1 gop2 sreg =
  let open Bidirectional_tmp in
  let size = X86Util.bitsize_of_szmode mode in
  let tmp1 = mk size and tmp2 = mk size in
  [
    tmp1 <-- disas_expr gop1 mode sreg;
    tmp2 <-- disas_expr gop2 mode sreg;
    tmp2 --> disas_lval gop1 mode sreg;
    tmp1 --> disas_lval gop2 mode sreg;
  ]

let lift_xmul ext affect_flags dst1_opt src1 mode src2 sreg =
  let open Dba in
  let size = size_mode mode in
  let double_size = 2 * size in
  let temp_lval = LValue.temp (Size.Bit.create double_size) in
  let temp_rval = LValue.to_expr temp_lval in
  let src1_rval = disas_expr src1 mode sreg
  and src2_rval = disas_expr src2 mode sreg in
  let flags_assigns = affect_flags temp_rval mode in
  Predba.assign temp_lval
    (Expr.mul (ext double_size src1_rval) (ext double_size src2_rval))
  ::
  (match mode with
  | `M8 ->
      (* can only be F6 /5 or F6 /7 *)
      assert (dst1_opt = None);
      Predba.assign (lhs_of_reg EAX `M16) temp_rval :: flags_assigns
  | `M16 | `M32 -> (
      match dst1_opt with
      | None ->
          Predba.assign (lhs_of_reg EAX mode)
            (Expr.restrict 0 (size - 1) temp_rval)
          :: Predba.assign (lhs_of_reg EDX mode)
               (Expr.restrict size (double_size - 1) temp_rval)
          :: flags_assigns
      | Some dst1 ->
          Predba.assign
            (disas_lval dst1 mode sreg)
            (Expr.mul src1_rval src2_rval)
          :: flags_assigns))

let lift_mul = lift_xmul Dba.Expr.uext affect_flags_mul None (Reg EAX)
let lift_imul = lift_xmul Dba.Expr.sext affect_flags_imul

let lift_xdiv div rem ext cond_err mode gop sreg =
  let open Dba in
  let size = size_mode mode in
  let double_size = 2 * size in
  let double_bitsize = Size.Bit.create double_size in
  let div_lval =
    LValue.temporary (Printf.sprintf "div%d" double_size) double_bitsize
  in
  let div_rval = LValue.to_expr div_lval in
  let quo_lval =
    LValue.temporary (Printf.sprintf "quo%d" double_size) double_bitsize
  in
  let quo_rval = LValue.to_expr quo_lval in
  let rem_lval =
    LValue.temporary (Printf.sprintf "rem%d" double_size) double_bitsize
  in
  let rem_rval = LValue.to_expr rem_lval in
  let src_expr = disas_expr gop mode sreg in
  let rval_dividend, lval_remainder =
    match mode with
    | `M8 -> (expr_of_reg16 AX, lhs_of_reg8 AH)
    | `M16 | `M32 ->
        ( Expr.append (expr_of_reg mode EDX) (expr_of_reg mode EAX),
          lhs_of_reg EDX mode )
  in
  Predba.dynamic_assert (Expr.diff src_expr (Expr.zeros size))
  :: Predba.assign div_lval rval_dividend
  :: Predba.assign quo_lval (div div_rval (ext double_size src_expr))
  :: Predba.assign rem_lval (rem div_rval (ext double_size src_expr))
  :: Predba.dynamic_assert (cond_err quo_rval)
  :: Predba.assign (lhs_of_reg EAX mode) (Expr.restrict 0 (size - 1) quo_rval)
  :: Predba.assign lval_remainder (Expr.restrict 0 (size - 1) rem_rval)
  :: affect_flags_div

let lift_div =
  let cond_err res =
    let double_size = Dba.Expr.size_of res in
    let size = double_size / 2 in
    Dba.Expr.(equal (restrict size (double_size - 1) res) (zeros size))
  in
  lift_xdiv Dba.Expr.udiv Dba.Expr.umod Dba.Expr.uext cond_err

let lift_idiv =
  let cond_err res =
    let double_size = Dba.Expr.size_of res in
    let size = double_size / 2 in
    Dba.Expr.(
      equal
        (restrict size (double_size - 1) res)
        (sext size (bit_restrict (size - 1) res)))
  in
  lift_xdiv Dba.Expr.sdiv Dba.Expr.smod Dba.Expr.sext cond_err

let lift_cbw mode =
  match mode with
  | `M32 ->
      [ assign_register EAX mode (Dba.Expr.sext 32 (expr_of_reg `M16 EAX)) ]
  | `M16 ->
      [ assign_register EAX mode (Dba.Expr.sext 16 (expr_of_reg `M8 EAX)) ]

let lift_cwd mode =
  let size, name =
    match mode with `M32 -> (64, "temp64") | `M16 -> (32, "temp32")
  in
  let open Dba.Expr in
  let temp_lhs = Dba.LValue.temporary name (Size.Bit.create size) in
  let temp_exp = temporary name ~size in
  [
    Predba.assign temp_lhs (sext size (expr_of_reg mode EAX));
    assign_register EDX mode (restrict (size / 2) (size - 1) temp_exp);
  ]

let lift_bsr mode dst src sreg =
  let open Dba_types in
  let src_exp = disas_expr src mode sreg in
  let dst_lhs = lhs_of_reg dst mode in
  let size = match mode with `M32 -> 31 | `M16 -> 15 | `M8 -> assert false in
  let open Dba.Expr in
  let sz = size + 1 in
  let bits = Size.Bit.create sz in
  let tmp = temp_size sz in
  let cpt = cpt_size sz in
  let temp_lhs = Dba.LValue.temporary tmp bits in
  let temp_exp = temporary tmp ~size:sz in
  let cpt_lhs = Dba.LValue.temporary cpt bits in
  let cpt_exp = temporary cpt ~size:sz in
  let extone = cst_of_int 1 sz in
  [
    Predba.conditional_jump (diff src_exp (zeros sz)) (Dba.JInner 4);
    assign_flag ZF (cst_of_int 1 1);
    Predba.undefined dst_lhs;
    Predba.static_jump (Dba.JInner 12);
    assign_flag ZF zero;
    Predba.assign temp_lhs src_exp;
    Predba.assign cpt_lhs (cst_of_int size (size + 1));
    Predba.conditional_jump
      Expr.(equal (bit_restrict size temp_exp) bool_true)
      (Dba.JInner 11);
    Predba.assign temp_lhs (shift_left temp_exp extone);
    Predba.assign cpt_lhs (sub cpt_exp extone);
    Predba.static_jump (Dba.JInner 7);
    Predba.assign dst_lhs cpt_exp;
  ]
  @ undef_flags [ CF; OF; SF ]

let lift_bsf mode dst src sreg =
  let src_exp = disas_expr src mode sreg in
  let dst_lhs = lhs_of_reg dst mode in
  let size = match mode with `M32 -> 31 | `M16 -> 15 | `M8 -> assert false in
  let open Dba in
  let sz = size + 1 in
  let bits = Size.Bit.create sz in
  let tmp = temp_size sz in
  let cpt = cpt_size sz in
  let temp_lhs = Dba.LValue.temporary tmp bits in
  let temp_exp = Expr.temporary tmp ~size:sz in
  let cpt_lhs = Dba.LValue.temporary cpt bits in
  let cpt_exp = Expr.temporary cpt ~size:sz in
  let zeros = Expr.zeros sz in
  [
    Predba.conditional_jump (Expr.diff src_exp zeros) (Dba.JInner 4);
    Predba.assign (lhs_of_flag ZF) (cst_of_int 1 1);
    Predba.undefined dst_lhs;
    Predba.static_jump (Dba.JInner 12);
    Predba.assign (lhs_of_flag ZF) Dba_types.Expr.bool_false;
    Predba.assign temp_lhs src_exp;
    Predba.assign cpt_lhs zeros;
    Predba.conditional_jump
      Expr.(equal (bit_restrict 0 temp_exp) Dba_types.Expr.bool_true)
      (Dba.JInner 11);
    Predba.assign temp_lhs (Expr.shift_right temp_exp (cst_of_int 1 sz));
    Predba.assign cpt_lhs (Expr.add cpt_exp (cst_of_int 1 sz));
    Predba.static_jump (Dba.JInner 7);
    Predba.assign dst_lhs cpt_exp;
  ]
  @ undef_flags [ CF; OF; SF ]

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
        (Dba.LValue._restrict dst Size.Bit.bits32 off1 (off1 + 7))
        (Dba.Expr.restrict (off2 - 7) off2 temp)
      :: assign acc dst temp (off1 + 8) (off2 - 8)
    else acc
  in
  Predba.assign temp_lhs dst_exp
  :: assign [] (X86Util.reg32_to_string dst) temp_exp 0 (size - 1)

let lift_xadd mode gop1 gop2 sreg =
  let open Bidirectional_tmp in
  let tmp1 = of_mode mode and tmp2 = of_mode mode in
  let dba_gop1, dba_gop2 =
    (disas_expr gop1 mode sreg, disas_expr gop2 mode sreg)
  in
  (tmp1 <-- Dba.Expr.add dba_gop1 dba_gop2)
  :: (tmp2 <-- dba_gop1)
  :: affect_flags_arith Add
       (disas_expr gop1 mode sreg)
       (disas_expr gop2 mode sreg)
       tmp1.b_src (size_mode mode)
  @ [ tmp1 --> disas_lval gop1 mode sreg; tmp2 --> disas_lval gop2 mode sreg ]

let lift_jcxz mode src =
  let size = size_mode mode in
  [
    Predba.conditional_jump
      Dba.Expr.(equal (expr_of_reg mode ECX) (zeros size))
      (strange_addr_of_int64 src);
  ]

let lift_pshuf reg_t size_t r gop imm off_min off_max sreg =
  let size = match size_t with S128 -> 128 | S64 -> 64 | S32 -> 32 in
  let bits = Size.Bit.create size in
  let src_expr = disas_expr_xmm gop reg_t size_t sreg in
  let range = (off_max - off_min) / 4 in
  let open Dba in
  let rec assign acc dst src off1 off2 =
    let hi = off1 + range - 1 in
    if hi < off_max then
      let shift_temp1 =
        Expr.restrict off2 (off2 + 1) (cst_of_int imm 8) |> Expr.uext size
      in
      let shift_temp2 = cst_of_int range size in
      let shift = Expr.mul shift_temp1 shift_temp2 in
      let temp = Expr.shift_right src_expr shift in
      let acc =
        Predba.assign
          (Dba.LValue._restrict dst bits off1 hi)
          (Expr.restrict off_min (off_min + range - 1) temp)
        :: acc
      in
      assign acc dst src (off1 + range) (off2 + 2)
    else List.rev acc
  in
  let dst =
    match reg_t with
    | MM -> X86Util.mm_reg_to_string (X86Util.xmm_reg_to_mm_reg r)
    | XMM -> X86Util.xmm_reg_to_string r
  in
  let decoded = assign [] dst gop off_min 0 in
  if off_min > 0 then
    let lim = off_min - 1 in
    Predba.assign
      (Dba.LValue._restrict dst bits 0 lim)
      (Expr.restrict 0 lim src_expr)
    :: decoded
  else if off_max < size then
    decoded
    @ [
        (let sz = size - 1 in
         Predba.assign
           (Dba.LValue._restrict dst bits off_max sz)
           (Expr.restrict off_max sz src_expr));
      ]
  else decoded

let lift_movshdup mm gop1 gop2 sreg =
  [
    assign_xmm gop1 0 31 gop2 32 63 XMM mm sreg;
    assign_xmm gop1 32 63 gop2 32 63 XMM mm sreg;
    assign_xmm gop1 64 95 gop2 96 127 XMM mm sreg;
    assign_xmm gop1 96 127 gop2 96 127 XMM mm sreg;
  ]

let lift_p f range xmm mm gop1 gop2 sreg =
  let size = X86Util.bitsize_of_xmm_mm xmm in
  let gop1_expr = disas_expr_xmm gop1 xmm mm sreg in
  let gop2_expr = disas_expr_xmm gop2 xmm mm sreg in
  let open Dba in
  let rec unroll acc off1 off2 =
    if off1 = size then acc
    else
      let temp1 = Expr.restrict off1 off2 gop1_expr in
      let temp2 = Expr.restrict off1 off2 gop2_expr in
      let expr = f temp1 temp2 in
      let acc = assign_xmm_expr gop1 off1 off2 expr xmm mm sreg :: acc in
      unroll acc (off1 + range) (off2 + range)
  in
  unroll [] 0 (range - 1)

let lift_padd = lift_p Dba.Expr.add
let lift_psub = lift_p Dba.Expr.sub
let select_p f op1 op2 = Dba.Expr.ite (f op1 op2) op1 op2
let lift_pmaxu = lift_p @@ select_p Dba.Expr.ugt
let lift_pmaxs = lift_p @@ select_p Dba.Expr.sgt
let lift_pminu = lift_p @@ select_p Dba.Expr.ult
let lift_pmins = lift_p @@ select_p Dba.Expr.slt

let ssatured_add a b =
  let size = Dba.Expr.size_of a in
  let smax = Dba.Expr.constant (Bitvector.fill ~hi:(size - 2) size)
  and smin = Dba.Expr.constant (Bitvector.fill ~lo:(size - 1) size) in
  let tbp = Dba.Expr.(sge b (zeros size)) in
  let tsp = Dba.Expr.(sgt a (sub smax b))
  and tsn = Dba.Expr.(slt a (sub smin b)) in
  let addab = Dba.Expr.add a b in
  Dba.Expr.(ite tbp (ite tsp smax addab) (ite tsn smin addab))

let lift_padds = lift_p ssatured_add

let ssatured_sub a b =
  let size = Dba.Expr.size_of a in
  let smax = Dba.Expr.constant (Bitvector.fill ~hi:(size - 2) size)
  and smin = Dba.Expr.constant (Bitvector.fill ~lo:(size - 1) size) in
  let tbp = Dba.Expr.(sge b (zeros size)) in
  let tsp = Dba.Expr.(slt a (add smin b))
  and tsn = Dba.Expr.(sgt a (add smax b)) in
  let subab = Dba.Expr.sub a b in
  Dba.Expr.(ite tbp (ite tsp smin subab) (ite tsn smax subab))

let lift_psubs = lift_p ssatured_sub

let usatured_add a b =
  let size = Dba.Expr.size_of a in
  let umax = Dba.Expr.constant (Bitvector.fill size) in
  Dba.Expr.(ite (ugt a (sub umax b)) umax (add a b))

let lift_paddus = lift_p usatured_add

let usatured_sub a b =
  let size = Dba.Expr.size_of a in
  let umin = Dba.Expr.zeros size in
  Dba.Expr.(ite (ult a b) umin (sub a b))

let lift_psubus = lift_p usatured_sub

let avgu size a b =
  Dba.Expr.(
    restrict 1 size
      (add (add (uext (size + 1) a) (uext (size + 1) b)) (ones (size + 1))))

let lift_pavgu size = lift_p (avgu size) size
let mulhw a b = Dba.Expr.(restrict 16 31 (mul (sext 32 a) (sext 32 b)))
let lift_pmulhw = lift_p mulhw 16

let mulhrw a b =
  Dba.Expr.(
    restrict 16 31 (add (mul (sext 32 a) (sext 32 b)) (cst_of_int 0x8000 32)))

let lift_pmulhrw = lift_p mulhrw 16
let lift_pmullw = lift_p Dba.Expr.mul 16

let lift_pmuludq xmm mm gop1 gop2 sreg =
  let range = 32 and drange = 64 in
  let size = X86Util.bitsize_of_xmm_mm xmm in
  let gop1_expr = disas_expr_xmm gop1 xmm mm sreg in
  let gop2_expr = disas_expr_xmm gop2 xmm mm sreg in
  let open Dba in
  let rec unroll acc off =
    if off >= size then acc
    else
      let temp1 =
        Expr.restrict off (off + range - 1) gop1_expr |> Expr.uext drange
      in
      let temp2 =
        Expr.restrict off (off + range - 1) gop2_expr |> Expr.uext drange
      in
      let res = Expr.mul temp1 temp2 in
      let acc =
        assign_xmm_expr gop1 off (off + drange - 1) res xmm mm sreg :: acc
      in
      unroll acc (off + drange)
  in
  unroll [] 0

let lift_ps_l f range xmm size gop1 gop2 sreg =
  let count = disas_expr_xmm gop2 xmm size sreg in
  let sz = X86Util.bitsize_of_xmm_mm xmm in
  let cond = Dba.Expr.ult count (cst_of_int range sz) in
  if Dba.Expr.(is_equal cond zero) then
    [ Predba.assign (disas_lval_xmm gop1 xmm size sreg) (Dba.Expr.zeros sz) ]
  else
    let count = Dba.Expr.restrict 0 (range - 1) count in
    let body = lift_p (fun a _ -> f a count) range xmm size gop1 gop2 sreg in
    if Dba.Expr.(is_equal cond one) then body
    else
      Predba.conditional_jump cond (Dba.JInner 3)
      :: Predba.assign (disas_lval_xmm gop1 xmm size sreg) (Dba.Expr.zeros sz)
      :: Predba.static_jump (Dba.JInner ((sz / range) + 3))
      :: body

let lift_psrl = lift_ps_l Dba.Expr.shift_right
let lift_psll = lift_ps_l Dba.Expr.shift_left

let lift_psra range xmm size gop1 gop2 sreg =
  let count = disas_expr_xmm gop2 xmm size sreg in
  let sz = X86Util.bitsize_of_xmm_mm xmm in
  let cond = Dba.Expr.ult count (cst_of_int range sz) in
  if Dba.Expr.(is_equal cond zero) then
    lift_p
      (fun a _ -> Dba.Expr.(sext range (slt a (zeros range))))
      range xmm size gop1 gop2 sreg
  else
    let count = Dba.Expr.restrict 0 (range - 1) count in
    let body =
      lift_p
        (fun a _ -> Dba.Expr.shift_right_signed a count)
        range xmm size gop1 gop2 sreg
    in
    if Dba.Expr.(is_equal cond one) then body
    else
      Predba.conditional_jump cond (Dba.JInner 3)
      :: Predba.assign (disas_lval_xmm gop1 xmm size sreg) (Dba.Expr.zeros sz)
      :: Predba.static_jump (Dba.JInner ((sz / range) + 3))
      :: body

let lift_ps_ldq shift gop1 imm sreg =
  let lval = disas_lval_xmm gop1 XMM S128 sreg in
  let eval = disas_expr_xmm gop1 XMM S128 sreg in
  let v =
    if imm > 15 then Dba.Expr.zeros 128
    else Dba.Expr.binary shift eval (cst_of_int (8 * imm) 128)
  in
  [ Predba.assign lval v ]

let lift_ptest xmm size gop1 gop2 = affect_flags_ptest xmm size gop1 gop2

let lift_predicate p xmm mm lop rop sreg =
  let lvalue = disas_lval_xmm lop xmm mm sreg
  and l_e = disas_expr_xmm lop xmm mm sreg
  and r_e = disas_expr_xmm rop xmm mm sreg in
  let e = p l_e r_e in
  [ Predba.assign lvalue e ]

let lift_pxor = lift_predicate Dba.Expr.logxor
and lift_por = lift_predicate Dba.Expr.logor
and lift_pand = lift_predicate Dba.Expr.logand
and lift_pandn = lift_predicate (fun le re -> Dba.Expr.(logand (lognot le) re))

let lift_punpckl xmm size gop1 gop2 step sreg =
  let bitsize = X86Util.bitsize_of_xmm_mm xmm in
  let rec unroll acc off1 off2 =
    if off1 = bitsize then acc
    else
      let acc =
        assign_xmm gop1 (off1 + step)
          (off1 + (2 * step) - 1)
          gop2 off2
          (off2 + step - 1)
          xmm size sreg
        :: assign_xmm gop1 off1
             (off1 + step - 1)
             gop1 off2
             (off2 + step - 1)
             xmm size sreg
        :: acc
      in
      unroll acc (off1 + (2 * step)) (off2 + step)
  in
  unroll [] 0 0

let lift_punpckh xmm size gop1 gop2 step sreg =
  let bitsize = X86Util.bitsize_of_xmm_mm xmm in
  let rec unroll acc off1 off2 =
    if off1 = 0 then acc
    else
      let acc =
        assign_xmm gop1
          (off1 - (2 * step))
          (off1 - step - 1)
          gop1 (off2 - step) (off2 - 1) xmm size sreg
        :: assign_xmm gop1 (off1 - step) (off1 - 1) gop2 (off2 - step)
             (off2 - 1) xmm size sreg
        :: acc
      in
      unroll acc (off1 - (2 * step)) (off2 - step)
  in
  unroll [] bitsize bitsize

let lift_pack_s xmm size gop1 gop2 step sreg ~max ~satur1 ~min ~satur2 =
  let bitsize = X86Util.bitsize_of_xmm_mm xmm in
  let e1 = disas_expr_xmm gop1 xmm size sreg in
  let e2 = disas_expr_xmm gop2 xmm size sreg in
  let lv = disas_lval_xmm gop1 xmm size sreg in
  let rec unroll acc ex off1 off2 =
    let open Dba in
    if off1 = 0 then acc
    else
      let x' = Expr.restrict (2 * (off1 - step)) ((2 * off1) - step - 1) ex in
      let x = Expr.restrict (2 * (off1 - step)) ((2 * off1) - 1) ex in
      let test = Expr.sgt x max in
      let expr = Expr.ite test satur1 x' in
      let test = Expr.slt x min in
      let expr = Expr.ite test satur2 expr in
      let acc = assign lv expr (off2 - step) (off2 - 1) :: acc in
      unroll acc ex (off1 - step) (off2 - step)
  in
  unroll (unroll [] e2 (bitsize / 2) bitsize) e1 (bitsize / 2) (bitsize / 2)

let lift_packus xmm size gop1 gop2 step sreg =
  let max = Dba.Expr.constant (Bitvector.fill ~hi:(step - 1) (2 * step)) in
  let satur1 = Dba.Expr.constant (Bitvector.fill step) in
  let min = Dba.Expr.zeros @@ (2 * step) in
  let satur2 = Dba.Expr.zeros step in
  lift_pack_s xmm size gop1 gop2 step sreg ~max ~satur1 ~min ~satur2

let lift_packss xmm size gop1 gop2 step sreg =
  let max = Dba.Expr.constant (Bitvector.fill ~hi:(step - 2) (2 * step)) in
  let satur1 = Dba.Expr.constant (Bitvector.fill ~hi:(step - 2) step) in
  let min = Dba.Expr.constant (Bitvector.fill ~lo:(step - 1) (2 * step)) in
  let satur2 = Dba.Expr.constant (Bitvector.fill ~lo:(step - 1) step) in
  lift_pack_s xmm size gop1 gop2 step sreg ~max ~satur1 ~min ~satur2

let lift_pmadd step ext1 ext2 add xmm size gop1 gop2 sreg =
  let bitsize = X86Util.bitsize_of_xmm_mm xmm in
  let e1 = disas_expr_xmm gop1 xmm size sreg in
  let e2 = disas_expr_xmm gop2 xmm size sreg in
  let lv = disas_lval_xmm gop1 xmm size sreg in
  let ep1 = Dba.Expr.temporary ~size:(2 * step) "dp1"
  and ep2 = Dba.Expr.temporary ~size:(2 * step) "dp2" in
  let lp1 = Dba.LValue.of_expr ep1 and lp2 = Dba.LValue.of_expr ep2 in
  let rec unroll acc off1 off2 =
    if off1 = bitsize then acc
    else
      let x0 = Dba.Expr.restrict off1 (off1 + step - 1) e2 in
      let y0 = Dba.Expr.restrict off1 (off1 + step - 1) e1 in
      let x1 = Dba.Expr.restrict (off1 + step) (off1 + (2 * step) - 1) e2 in
      let y1 = Dba.Expr.restrict (off1 + step) (off1 + (2 * step) - 1) e1 in
      let x0 = ext2 (2 * step) x0 in
      let y0 = ext1 (2 * step) y0 in
      let x1 = ext2 (2 * step) x1 in
      let y1 = ext1 (2 * step) y1 in
      let x0y0 = Dba.Expr.mul x0 y0 and x1y1 = Dba.Expr.mul x1 y1 in
      let expr = add ep1 ep2 in
      let acc =
        Predba.assign lp1 x0y0 :: Predba.assign lp2 x1y1
        :: assign lv expr off1 (off1 + (2 * step) - 1)
        :: acc
      in
      unroll acc (off1 + (2 * step)) (off2 + step)
  in
  unroll [] 0 0

let lift_pmaddwd = lift_pmadd 16 Dba.Expr.sext Dba.Expr.sext Dba.Expr.add
let lift_pmaddusbsw = lift_pmadd 8 Dba.Expr.uext Dba.Expr.sext ssatured_add
let lift_pcmpeq = lift_p (fun a b -> Dba.Expr.(sext (size_of a) (equal a b)))
let lift_pcmpgt = lift_p (fun a b -> Dba.Expr.(sext (size_of a) (ugt a b)))

let lv_restrict_eax =
  let eax_size = Size.Bit.create 32 in
  Dba.LValue._restrict (X86Util.reg32_to_string EAX) eax_size

let al_lval = lv_restrict_eax 0 7
let ah_lval = lv_restrict_eax 8 15

let e_restrict_eax lo hi =
  Dba.Expr.(restrict lo hi (var (X86Util.reg32_to_string EAX) 32))

let al_expr = e_restrict_eax 0 7
let ah_expr = e_restrict_eax 8 15

let lift_aad imm =
  let open Dba in
  let e1 = Expr.mul ah_expr (cst_of_int imm 8) in
  let e2 = Expr.add al_expr e1 in
  let e = Expr.logand e2 (cst_of_int 255 8) in
  [ Predba.assign al_lval e; Predba.assign ah_lval (Expr.zeros 8) ]
  @ affect_flags_aad al_expr

let lift_aam imm =
  let open Dba in
  let v = cst_of_int imm 8 in
  let e1 = Expr.udiv al_expr v in
  let e2 = Expr.umod al_expr v in
  [ Predba.assign ah_lval e1; Predba.assign al_lval e2 ]
  @ affect_flags_aad al_expr

let lift_address_mode = function A32 -> (32, `M32) | A16 -> (16, `M16)

let lift_loop_cond cond a_mode src =
  let size, mode = lift_address_mode a_mode in
  [
    Predba.assign (lhs_of_reg ECX mode)
      (Dba.Expr.sub (expr_of_reg mode ECX) (cst_of_int 1 size));
    Predba.conditional_jump cond (strange_addr_of_int64 src);
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
  let al_expr = Expr.restrict 0 7 (expr_of_reg32 EAX) in
  let e = Expr.uext 32 al_expr in
  let ebx_expr = expr_of_reg32 EBX in
  match addr_mode with
  | A32 -> [ Predba.assign al_lval (expr_of_mem8 (Expr.add ebx_expr e) ~sreg) ]
  | A16 ->
      let ebx_expr = Expr.restrict 0 15 ebx_expr in
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
  and vtag_opt = Dba.Var.Tag.Temp in
  let temp_lhs = Dba.LValue.var name ~bitsize:size ~tag:vtag_opt in
  let temp_expr = Expr.var name size vtag_opt in
  [
    Predba.assign temp_lhs operand1_expr;
    Predba.assign operand1_lhs operand2_expr;
    Predba.assign operand2_lhs temp_expr;
  ]

let lift_btX ~sreg mode base offset =
  let open Dba in
  let size = size_mode mode in
  let const_expr i = Expr.constant (Bitvector.of_int ~size i) in
  match base with
  | Reg _ -> (
      let base_expr = disas_expr base mode sreg in
      match offset with
      | Imm i ->
          let i = Int64.to_int i mod size in
          let bit_select = Expr.bit_restrict i base_expr in
          (bit_select, bit_select, Expr.one)
      | Reg _ ->
          let off_expr =
            Expr.umod
              (disas_expr offset mode sreg)
              (Expr.constant (Bitvector.of_int ~size size))
          in
          let bit_mask = Expr.(shift_left (ones size) off_expr) in
          let bit_select =
            Expr.(diff (zeros size) (logand base_expr bit_mask))
          in
          (bit_select, base_expr, bit_mask)
      | Address _ -> assert false)
  | Imm _ -> assert false
  | Address a -> (
      let base_addr = expr_of_addr a in
      let size_addr = Expr.size_of base_addr in
      let const_addr i = Expr.constant (Bitvector.of_int ~size:size_addr i) in
      let bytesize = Size.(Byte.(to_int (of_bitsize (Bit.create size)))) in
      match offset with
      | Imm i ->
          let i = Int64.to_int i in
          let off = bytesize * (i / size) and pos = i mod size in
          let base_expr =
            expr_of_mem mode ~sreg (Expr.add base_addr (const_addr off))
          in
          let bit_mask = const_expr (1 lsl pos) in
          (Expr.bit_restrict pos base_expr, base_expr, bit_mask)
      | Reg _ ->
          let i = disas_expr offset mode sreg in
          let pos_expr = Expr.umod i (const_expr size) in
          let off_expr =
            Expr.(
              mul (const_addr bytesize)
                (udiv (uext size_addr i) (const_addr size)))
          in
          let base_expr =
            expr_of_mem mode ~sreg (Expr.add base_addr off_expr)
          in
          let bit_mask = Expr.(shift_left (ones size) pos_expr) in
          let bit_select =
            Expr.(diff (zeros size) (logand base_expr bit_mask))
          in
          (bit_select, base_expr, bit_mask)
      | Address _ -> assert false)

let undef_flags_bt = undef_flags [ OF; SF; AF; PF ]

let lift_bt ~sreg { mode; src = offset; dst = base } =
  let bit_select, _, _ = lift_btX mode base offset ~sreg in
  assign_flag CF bit_select :: undef_flags_bt

let lift_bts ~sreg { mode; src = offset; dst = base } =
  let bit_select, base_expr, bit_mask = lift_btX mode base offset ~sreg in
  let base_lhs = Dba.LValue.of_expr base_expr in
  let bit_set = Dba.Expr.logor base_expr bit_mask in
  assign_flag CF bit_select :: Predba.assign base_lhs bit_set :: undef_flags_bt

let lift_btr ~sreg { mode; src = offset; dst = base } =
  let bit_select, base_expr, bit_mask = lift_btX mode base offset ~sreg in
  let base_lhs = Dba.LValue.of_expr base_expr in
  let bit_reset = Dba.Expr.(logand base_expr (lognot bit_mask)) in
  assign_flag CF bit_select
  :: Predba.assign base_lhs bit_reset
  :: undef_flags_bt

let lift_btc ~sreg { mode; src = offset; dst = base } =
  let bit_select, base_expr, bit_mask = lift_btX mode base offset ~sreg in
  let base_lhs = Dba.LValue.of_expr base_expr in
  let bit_comp = Dba.Expr.logxor base_expr bit_mask in
  assign_flag CF bit_select :: Predba.assign base_lhs bit_comp :: undef_flags_bt

(* assumes Real-Address Mode *)
let lift_pushfd mode _sreg =
  let open Dba in
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in  *)
  let gen_code size =
    let nbits = Size.Bit.create size in
    let tmp_var = Dba.Var.temp nbits in
    let tmp_llow = LValue.restrict tmp_var 0 (min size 22 - 1) in
    let tmp_exp = Expr.v tmp_var in
    let eflags =
      Expr.(
        restrict 0 (min size 22 - 1) (logand (cst_of_int 0xfcffff 22) eflags22))
    in
    let cst = cst_of_int (nbytes_mode mode) 32 in
    let code =
      [
        Predba.assign tmp_llow eflags;
        Predba.assign (lhs_of_mem mode (Expr.sub esp_expr cst)) tmp_exp;
        Predba.assign esp_lval (Expr.sub esp_expr cst);
      ]
    in
    if size > 22 then Predba.undefined (LValue.v tmp_var) :: code else code
  in
  size_mode mode |> gen_code

(* assumes Real-Address Mode *)
let lift_popfd mode _sreg =
  (* let sreg = match sreg with None -> Some SS | _ -> sreg in  *)
  let gen_code size =
    let nbits = Size.Bit.create size in
    let tmp_lhs = Dba.LValue.temp nbits in
    let tmp_exp = Dba.LValue.to_expr tmp_lhs in
    Predba.assign tmp_lhs (expr_of_mem mode esp_expr)
    :: Predba.assign esp_lval
         (Dba.Expr.add (expr_of_reg32 ESP) (cst_of_int (nbytes_mode mode) 32))
    :: split_eflags tmp_exp
  in
  size_mode mode |> gen_code

let lift_lsl mode _src dst sreg =
  match mode with
  | `M16 | `M32 ->
      [
        Predba.non_deterministic (lhs_of_flag ZF);
        Predba.non_deterministic (disas_lval dst mode sreg);
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
      let op = Expr.append edx_expr eax_expr in
      let op2 = Expr.append ecx_expr ebx_expr in
      let c = Expr.equal op dst_exp in
      [
        Predba.conditional_jump c (Dba.JInner 5);
        Predba.assign (lhs_of_flag ZF) Dba.Expr.zero;
        Predba.assign eax_lhs (Expr.restrict 0 31 dst_exp);
        Predba.assign edx_lhs (Expr.restrict 32 63 dst_exp);
        Predba.static_jump (Dba.JInner 7);
        Predba.assign (lhs_of_flag ZF) Dba.Expr.one;
        Predba.assign dst_lhs op2;
      ]
  | _ -> failwith "lift_cmpXchg8b ?"

let lift_lahf () =
  let eflags =
    [
      sf_flag;
      zf_flag;
      Dba.Expr.zero;
      af_flag;
      Dba.Expr.zero;
      pf_flag;
      Dba.Expr.one;
      cf_flag;
    ]
  in
  List.mapi
    (fun i e ->
      let nth_bit = 15 - i in
      let lv = lv_restrict_eax nth_bit nth_bit in
      Predba.assign lv e)
    eflags

let lift_sahf =
  let flag_bit_alist = [ (SF, 15); (ZF, 14); (AF, 12); (PF, 10); (CF, 8) ] in
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
  let lval = disas_lval lop mode sreg and rval = disas_expr rop mode sreg in
  [ Predba.assign lval rval ]

let lift_aas () =
  let open Dba in
  let set_af_cf e = [ assign_flag AF e; assign_flag CF e ] in
  let eax_e = expr_of_reg32 EAX in
  let al_e = Expr.restrict 0 7 eax_e and ah_e = Expr.restrict 8 15 eax_e in
  let size = 8 in
  let byte_constant n = Expr.constant (Bitvector.create (Z.of_int n) size) in
  let v_6 = byte_constant 6 in
  let v_15 = byte_constant 15 in
  let check = Expr.logand al_e v_15 in
  let then_block =
    [
      Predba.assign al_lval (Expr.sub al_e v_6);
      Predba.assign ah_lval (Expr.sub ah_e (Expr.ones size));
    ]
    @ set_af_cf Expr.one
  and else_block = set_af_cf Expr.zero
  and last_instruction = [ Predba.assign al_lval check ]
  and cond =
    let loper = Expr.ugt check (byte_constant 9) in
    let roper = Expr.equal af_flag Expr.one in
    Expr.logor loper roper
  in
  (Predba.conditional_jump cond (Jump_target.inner 4) :: else_block)
  @ [ Predba.static_jump (Jump_target.inner 8) ]
  @ then_block @ last_instruction

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

let lift_movaps ~src ~dst simd_sz sreg =
  let lval = disas_lval_xmm dst XMM simd_sz sreg in
  match src with
  | Address addr ->
      let addr = expr_of_addr addr in
      let size = Dba.Expr.size_of addr in
      [
        (* check that displacement is on a 16-byte boundary *)
        (Predba.dynamic_assert
        @@ Dba.Expr.(equal (umod addr (cst_of_int 16 size)) (zeros size)));
        Predba.assign lval @@ disas_expr_xmm src XMM simd_sz sreg;
      ]
  | Imm _ -> assert false
  | Reg _ -> [ Predba.assign lval @@ disas_expr_xmm src XMM simd_sz sreg ]

let lift_popcnt mode gop1 gop2 sreg =
  let egop2 = disas_expr gop2 mode sreg in
  let lgop1 = disas_lval gop1 mode sreg in
  let eres = res_expr mode in
  let lres = res_lhs mode in
  let size = size_mode mode in
  let init = Predba.assign lres (Dba.Expr.zeros size) in
  let final =
    [
      clear_flag OF;
      clear_flag SF;
      update_ZF egop2 size;
      clear_flag AF;
      clear_flag PF;
      clear_flag CF;
      Predba.assign lgop1 eres;
    ]
  in
  let rec unroll i sum =
    if i < 0 then sum
    else
      let open Dba in
      let rhs = Expr.add eres (Expr.uext size (Expr.bit_restrict i egop2)) in
      Predba.assign lres rhs :: sum |> unroll (i - 1)
  in
  init :: unroll (size - 1) final

let lift_lzcnt mode gop1 gop2 sreg =
  let egop2 = disas_expr gop2 mode sreg in
  let lgop1 = disas_lval gop1 mode sreg in
  let size = size_mode mode in
  let etmp = Dba.Expr.temporary ~size @@ temp_size size in
  let ltmp = Dba.LValue.of_expr etmp in
  let ecpt = Dba.Expr.temporary ~size @@ cpt_size size in
  let lcpt = Dba.LValue.of_expr ecpt in
  [
    Predba.assign lcpt (Dba.Expr.constant (Bitvector.of_int ~size size));
    Predba.conditional_jump Dba.Expr.(equal egop2 (zeros size)) (Dba.JInner 8);
    Predba.assign lcpt (Dba.Expr.zeros size);
    Predba.assign ltmp egop2;
    Predba.conditional_jump
      Dba.Expr.(equal (bit_restrict (size - 1) etmp) one)
      (Dba.JInner 8);
    Predba.assign ltmp Dba.Expr.(shift_left etmp (ones size));
    Predba.assign lcpt (Dba.Expr.add ecpt (Dba.Expr.ones size));
    Predba.static_jump (Dba.JInner 4);
    undef_flag OF;
    undef_flag SF;
    assign_flag ZF Dba.Expr.(equal ecpt (zeros size));
    undef_flag AF;
    undef_flag PF;
    assign_flag CF Dba.Expr.(equal egop2 (zeros size));
    Predba.assign lgop1 ecpt;
  ]

let lift_ldt base_name mode gop sreg =
  let base_addr_op =
    match gop with
    | Address ({ addrDisp; _ } as a) ->
        Address { a with addrDisp = Int64.add addrDisp @@ Int64.of_int 2 }
    | _ -> assert false
  in
  let egop = disas_expr base_addr_op mode sreg in
  (* FIXME: also handle the "limit" (i.e. size(, and not only the base. *)
  let base_reg = Dba.LValue.var base_name ~bitsize:Size.Bit.bits32 in
  [ Predba.assign base_reg egop ]

let lift_pclmulqdq xmm size gop1 gop2 select sreg =
  let e1 = disas_expr_xmm gop1 xmm size sreg in
  let e2 = disas_expr_xmm gop2 xmm size sreg in
  let lv = disas_lval_xmm gop1 xmm size sreg in
  let ae = Dba.Expr.temporary ~size:64 "clmul_a" in
  let al = Dba.LValue.of_expr ae in
  let be = Dba.Expr.temporary ~size:128 "clmul_b" in
  let bl = Dba.LValue.of_expr be in
  let re = Dba.Expr.temporary ~size:128 "clmul_r" in
  let rl = Dba.LValue.of_expr re in
  let s1 =
    if select land 0x01 = 0 then Dba.Expr.restrict 0 63 e1
    else Dba.Expr.restrict 64 127 e1
  in
  let s2 =
    if select land 0x10 = 0 then Dba.Expr.restrict 0 63 e2
    else Dba.Expr.restrict 64 127 e2
  in
  [
    Predba.assign al s1;
    Predba.assign bl @@ Dba.Expr.uext 128 s2;
    Predba.assign rl @@ Dba.Expr.zeros 128;
    Dba.Jump_target.inner 9
    |> Predba.conditional_jump @@ Dba.Expr.equal ae @@ Dba.Expr.zeros 64;
    Dba.Jump_target.inner 6
    |> Predba.conditional_jump @@ Dba.Expr.lognot @@ Dba.Expr.restrict 0 0 ae;
    Predba.assign rl @@ Dba.Expr.logxor re be;
    Predba.assign al @@ Dba.Expr.shift_right ae @@ Dba.Expr.ones 64;
    Predba.assign bl @@ Dba.Expr.shift_left be @@ Dba.Expr.ones 128;
    Predba.static_jump (Dba.Jump_target.inner 3);
    Predba.assign lv re;
  ]

module MvSegLeft = struct
  type reg_dst = Normal of segment_reg | TaskReg
end

let lift_mv_seg_left_gen ~dst ~rv =
  (* FIXME: incomplete semantics, various checks are missing. *)
  let open MvSegLeft in
  let open Dba in
  let sel_lv, base_var, desc_var, desc_rv =
    match dst with
    | Normal seg ->
        let base_name = X86Util.segment_reg_to_string seg ^ "_base" in
        let desc_name = X86Util.segment_reg_to_string seg ^ "_desc" in
        ( lhs_of_seg seg,
          LValue.var base_name ~bitsize:Size.Bit.bits32,
          LValue.var desc_name ~bitsize:Size.Bit.bits64,
          Expr.var desc_name 64 )
    | TaskReg ->
        ( LValue.var "tr" ~bitsize:Size.Bit.bits16,
          LValue.var "tr_base" ~bitsize:Size.Bit.bits32,
          LValue.var "tr_desc" ~bitsize:Size.Bit.bits64,
          Expr.var "tr_desc" 64 )
  in

  (* Conditions for the checks. *)
  let sel_not_zero = Expr.(diff rv (zeros 16)) in

  (* Mask the 8 last bits. *)
  let index =
    Expr.logand rv @@ Expr.constant (Bitvector.create (Z.of_int 0xfffc) 16)
  in
  let gdt = Expr.var "gdt" 32 in
  let ldt = Expr.var "ldt" 32 in
  let cond = Expr.bit_restrict 2 rv in
  let gdt_or_ldt = Expr.ite cond ldt gdt in
  let base_address = Expr.add gdt_or_ldt (Expr.uext 32 index) in

  let tmp_lv = LValue.temporary "temp32" (Size.Bit.create 32) in
  let tmp_rv = Expr.temporary ~size:32 "temp32" in

  let desc = Expr.load (Size.Byte.create 8) Machine.LittleEndian tmp_rv in

  (* First chunk. *)
  let bits0_23 =
    let chunk =
      Expr.logand desc_rv
        (Expr.constant
           (Bitvector.create
              (Z.of_bits "\x00\x00\xff\xff\xff")
              (* "0x00_00_00_ff_ff_ff_00_00" *) 64))
    in
    Expr.shift_right chunk (Expr.constant (Bitvector.create (Z.of_int 16) 64))
    |> Expr.restrict 0 23
  in

  let bits24_31 =
    let chunk =
      Expr.logand desc_rv
        (Expr.constant
           (Bitvector.create
              (Z.of_bits "\x00\x00\x00\x00\x00\x00\x00\xff")
              (* "0xff_00_00_00_00_00_00_00" *) 64))
    in
    Expr.shift_right chunk (Expr.constant (Bitvector.create (Z.of_int 56) 64))
    |> Expr.restrict 0 7
  in

  let bits = Expr.append bits24_31 bits0_23 in

  [
    Predba.assign sel_lv rv;
    Predba.dynamic_assert sel_not_zero;
    Predba.assign tmp_lv base_address;
    Predba.assign desc_var desc;
    Predba.assign base_var bits;
  ]

let lift_mv_seg_left ~dst ~src sreg =
  let open MvSegLeft in
  lift_mv_seg_left_gen ~dst:(Normal dst) ~rv:(disas_expr16 src sreg)

let lift_jmpf segment_selector dst_addr sreg =
  lift_mv_seg_left ~dst:CS ~src:(Imm (Int64.of_int segment_selector)) sreg
  @ [ Predba.static_jump (strange_addr_of_int64 dst_addr) ]

let lift_ltr op sreg =
  let open MvSegLeft in
  lift_mv_seg_left_gen ~dst:TaskReg ~rv:(disas_expr16 op sreg)

let lift_iret mode =
  let open MvSegLeft in
  let open Dba in
  (* FIXME: all cases not handled. This translation is only correct in
     protected mode, for returning to outer privilege level, without
     virtualization (VM = 0) or nested tasks (NT = 0) involved. *)
  (* FIXME: support 16-bit operand size *)
  assert (mode = `M32);
  let nxt_eip_lval = LValue.var ~bitsize:Size.Bit.bits32 "temp32_0" in
  let nxt_eip_rv = Expr.var "temp32_0" 32 in
  let tmp32_1_lval = LValue.var ~bitsize:Size.Bit.bits32 "temp32_1" in
  let nxt_eflags_lval = LValue.var ~bitsize:Size.Bit.bits32 "temp32_2" in
  let nxt_eflags_rv = Expr.var "temp32_2" 32 in
  let tmp32_1_rv = Expr.var "temp32_1" 32 in
  let tmp32_1_restr_rv = Expr.restrict 0 15 tmp32_1_rv in
  let nxt_esp_lval = LValue.temporary "temp32_3" Size.Bit.bits32 in
  let nxt_esp_rv = Expr.temporary ~size:32 "temp32_3" in

  let assign_flags =
    [
      assign_flag CF (Expr.restrict 0 0 nxt_eflags_rv);
      assign_flag PF (Expr.restrict 2 2 nxt_eflags_rv);
      assign_flag AF (Expr.restrict 4 4 nxt_eflags_rv);
      assign_flag ZF (Expr.restrict 6 6 nxt_eflags_rv);
      assign_flag SF (Expr.restrict 7 7 nxt_eflags_rv);
      assign_flag TF (Expr.restrict 8 8 nxt_eflags_rv);
      assign_flag DF (Expr.restrict 10 10 nxt_eflags_rv);
      assign_flag OF (Expr.restrict 11 11 nxt_eflags_rv);
      assign_flag NT (Expr.restrict 14 14 nxt_eflags_rv);
      assign_flag RF (Expr.restrict 16 16 nxt_eflags_rv);
      assign_flag AC (Expr.restrict 18 18 nxt_eflags_rv);
      assign_flag ID (Expr.restrict 21 21 nxt_eflags_rv);
      (* FIXME: these updates are conditioned to the current values of CPL and
       * IOPL. *)
      assign_flag IF (Expr.restrict 9 9 nxt_eflags_rv);
      assign_flag IOPL (Expr.restrict 12 13 nxt_eflags_rv);
      assign_flag VIF (Expr.restrict 19 19 nxt_eflags_rv);
      assign_flag VIP (Expr.restrict 20 20 nxt_eflags_rv);
    ]
  in

  let cpl_lval = LValue.var ~bitsize:(Size.Bit.create 2) "cpl" in

  lift_pop_aux mode nxt_eip_lval
  @ lift_pop_aux mode tmp32_1_lval
  @ lift_mv_seg_left_gen ~dst:(Normal CS) ~rv:tmp32_1_restr_rv
  @ lift_pop_aux mode nxt_eflags_lval
  @ lift_pop_aux mode nxt_esp_lval
  @ lift_pop_aux mode tmp32_1_lval
  @ [ Predba.assign esp_lval nxt_esp_rv ]
  @ lift_mv_seg_left_gen ~dst:(Normal SS) ~rv:tmp32_1_restr_rv
  (* FIXME: check that EIP is within CS limit *)
  @ assign_flags
  @ [
      Predba.assign cpl_lval @@ Expr.restrict 0 1 @@ expr_of_seg CS;
      Predba.dynamic_jump nxt_eip_rv;
    ]

(* FIXME: segment selectors should be set to zero under some conditions *)
(*
PROTECTED-MODE:
                EIP ← Pop();
                CS ← Pop(); (* 32-bit pop, high-order 16 bits discarded *)
                tempEFLAGS ← Pop();
RETURN-TO-OUTER-PRIVILEGE-LEVEL:
    IF OperandSize = 32
        THEN
                ESP ← Pop();
                SS ← Pop(); (* 32-bit pop, high-order 16 bits discarded *)
    FI;
    IF new mode ≠ 64-Bit Mode
        THEN
                IF EIP is not within CS limit
                        THEN #GP(0); FI;
    FI;
    EFLAGS (CF, PF, AF, ZF, SF, TF, DF, OF, NT) ← tempEFLAGS;
    IF OperandSize = 32 or or OperandSize = 64
        THEN EFLAGS(RF, AC, ID) ← tempEFLAGS; FI;
    IF CPL ≤ IOPL
        THEN EFLAGS(IF) ← tempEFLAGS; FI;
    IF CPL = 0
        THEN
                EFLAGS(IOPL) ← tempEFLAGS;
                IF OperandSize = 32 or OperandSize = 64
                        THEN EFLAGS(VIF, VIP) ← tempEFLAGS; FI;
    FI;
    CPL ← CS(RPL);
    FOR each SegReg in (ES, FS, GS, and DS)
        DO
                tempDesc ← descriptor cache for SegReg (* hidden part of segment register *)
                IF (SegmentSelector == NULL) OR (tempDesc(DPL) < CPL AND tempDesc(Type) is (data or non-conforming code)))
                        THEN (* Segment register invalid *)
                            SegmentSelector ← 0; (*Segment selector becomes null*)
                FI;
        OD;
END;
 *)

let lift_load_far mode seg reg address =
  let address = expr_of_addr address in
  let bytesize = X86Util.bytesize_of_mode mode in
  let seg_part =
    Dba.Expr.(
      load (Size.Byte.create 2) Machine.LittleEndian
        (add address (cst_of_int (Size.Byte.to_int bytesize) (size_of address))))
  in
  let offset = Dba.Expr.load bytesize Machine.LittleEndian address in
  lift_mv_seg_left_gen ~dst:(Normal seg) ~rv:seg_part
  @ [ assign_register reg mode offset ]

let lift_mv_seg_right ~dst ~src sreg =
  (* Intel documentation on p.480 says:

     For the Pentium 4, Intel Xeon, and P6 family processors, the two high-order
     bytes are filled with zeros; for earlier 32-bit IA-32 processors, the two
     high order bytes are undefined.

     The encoding here will thus fill the high ordere bytes of reg with zeros if
     needed.
  *)
  let src size = Dba.Expr.uext size (expr_of_seg src) in
  match dst with
  | Reg r -> [ assign_register (X86Util.reg16_to_reg32 r) `M32 (src 32) ]
  | Address _ -> [ Predba.assign (disas_lval16 dst sreg) (src 16) ]
  | Imm _ -> assert false

let instruction_to_dba sreg addr nextaddr opcode instruction =
  match instruction with
  | Push (mode, genop) -> lift_push mode genop sreg
  | PushS reg -> lift_pushS reg sreg
  | PushA mode -> lift_pushA (mode :> X86Types.sizeMode) sreg
  | Pushfd mode -> lift_pushfd (mode :> X86Types.sizeMode) sreg
  | Pop (mode, genop) -> lift_pop mode genop sreg
  | PopS reg -> lift_popS reg sreg
  | PopA mode -> lift_popA (mode :> X86Types.sizeMode) sreg
  | Popfd mode -> lift_popfd (mode :> X86Types.sizeMode) sreg
  | Arith (mode, op, gop1, gop2) -> lift_arith mode op gop1 gop2 sreg
  | Aas -> lift_aas ()
  | Aad imm -> lift_aad imm
  | Aam imm -> lift_aam imm
  | Shift (mode, shift_op, gop32, gop8) ->
      lift_shift mode shift_op gop32 gop8 sreg
  | Rotate (mode, rotate_op, gop32, gop8) ->
      lift_rotate mode rotate_op gop32 gop8 sreg
  | Shiftd (mode, shift_op, gop1, gop2, gop8) ->
      lift_shiftd mode shift_op gop1 gop2 gop8 sreg
  | Cmp (mode, gop1, gop2) -> lift_cmp mode gop1 gop2 sreg
  | Cmps (rep, mode) -> lift_cmps mode rep addr nextaddr sreg
  | CmpXchg (mode, gop1, gop2) -> lift_cmpXchg mode gop1 gop2 sreg
  | Test (mode, gop1, gop2) -> lift_test mode gop1 gop2 sreg
  | Movd (xmm, pos, gop1, gop2) -> lift_movd xmm pos gop1 gop2 sreg
  | MovQ (xmm, mm, dst, src) ->
      [ assign_xmm_zero_ext ~dst 0 63 ~src 0 63 xmm mm sreg ]
  | MovdQA (xmm, mm, gop1, gop2) | MovdQU (xmm, mm, gop1, gop2) ->
      [ assign_xmm gop1 0 127 gop2 0 127 xmm mm sreg ]
  | Movs (rep, mode) -> lift_movs mode rep addr nextaddr sreg
  | Lods (rep, mode) -> lift_lods mode rep addr nextaddr sreg
  | Stos (rep, mode) -> lift_stos mode rep addr nextaddr sreg
  | Scas (rep, mode) -> lift_scas mode rep addr nextaddr sreg
  | CMovcc (mode, cc, gop1, gop2) -> lift_cmovcc mode cc gop1 gop2 nextaddr sreg
  | Movaps (simd_sz, dst, src) -> lift_movaps ~src ~dst simd_sz sreg
  | Movlpd (mm, gop1, gop2) -> [ assign_xmm gop1 0 63 gop2 0 63 XMM mm sreg ]
  | Movlps (mm, gop1, gop2) -> [ assign_xmm gop1 0 63 gop2 0 63 XMM mm sreg ]
  | Movhlps (mm, gop1, gop2) -> [ assign_xmm gop1 0 63 gop2 64 127 XMM mm sreg ]
  | Movddup (mm, gop1, gop2) ->
      [
        assign_xmm gop1 0 63 gop2 0 63 XMM mm sreg;
        assign_xmm gop2 64 127 gop2 0 63 XMM mm sreg;
      ]
  | Movsldup (mm, dst, src) -> lift_movsldup mm ~dst ~src sreg
  | Palignr (xmm, mm, dst, src, imm) -> lift_palignr xmm mm ~dst ~src imm sreg
  | Pcmpeqb (xmm, mm, gop1, gop2) -> lift_pcmpeq 8 xmm mm gop1 gop2 sreg
  | Pcmpeqw (xmm, mm, gop1, gop2) -> lift_pcmpeq 16 xmm mm gop1 gop2 sreg
  | Pcmpeqd (xmm, mm, gop1, gop2) -> lift_pcmpeq 32 xmm mm gop1 gop2 sreg
  | PmovMSKB (xmm, mm, gop1, gop2) -> pmovMSK gop1 gop2 xmm mm sreg
  | Pminu (xmm, mm, gop1, gop2, range) -> lift_pminu range xmm mm gop1 gop2 sreg
  | Pmins (xmm, mm, gop1, gop2, range) -> lift_pmins range xmm mm gop1 gop2 sreg
  | Pxor (xmm, mm, gop1, gop2) -> lift_pxor xmm mm gop1 gop2 sreg
  | Por (xmm, mm, gop1, gop2) -> lift_por xmm mm gop1 gop2 sreg
  | Pand (xmm, mm, gop1, gop2) -> lift_pand xmm mm gop1 gop2 sreg
  | Pandn (xmm, mm, gop1, gop2) -> lift_pandn xmm mm gop1 gop2 sreg
  | Pmaxu (xmm, mm, gop1, gop2, range) -> lift_pmaxu range xmm mm gop1 gop2 sreg
  | Pmaxs (xmm, mm, gop1, gop2, range) -> lift_pmaxs range xmm mm gop1 gop2 sreg
  | Mov (mode, gop1, gop2) -> lift_mov mode gop1 gop2 sreg
  | MovSegLeft (dst, src) -> lift_mv_seg_left ~dst ~src sreg
  | MovSegRight (dst, src) -> lift_mv_seg_right ~dst ~src sreg
  | Bt op2 -> lift_bt ~sreg op2
  | Bts op2 -> lift_bts ~sreg op2
  | Btr op2 -> lift_btr ~sreg op2
  | Btc op2 -> lift_btc ~sreg op2
  | Movzx (mode, r, op8) ->
      lift_movzx ~signed:false mode r (disas_expr8 op8 sreg)
  | Movzx16 (mode, r, op16) ->
      lift_movzx ~signed:false mode r (disas_expr16 op16 sreg)
  | Movsx (mode, r, op8) ->
      lift_movzx ~signed:true mode r (disas_expr8 op8 sreg)
  | Movsx16 (mode, r, op16) ->
      lift_movzx ~signed:true mode r (disas_expr16 op16 sreg)
  | Enter (mode, alloc, level) -> lift_enter (mode :> sizeMode) alloc level
  | Leave mode -> lift_leave (mode :> sizeMode)
  | Lea (mode, dst, src) -> lift_lea ~mode ~src ~dst sreg
  | LoadFarPointer (mode, seg, reg, address) ->
      lift_load_far mode seg reg address
  | Jmp src -> [ Predba.static_jump (strange_addr_of_int64 src) ]
  | DJmp gop -> [ Predba.dynamic_jump (disas_expr gop `M32 sreg) ]
  | Jmpf (segment_selector, dst_addr) ->
      lift_jmpf segment_selector dst_addr sreg
  | Jcc (cc, src) ->
      [ Predba.conditional_jump (cond_of_cc cc) (strange_addr_of_int64 src) ]
  | Call src -> lift_call src nextaddr sreg
  | DCall gop -> lift_dcall gop nextaddr sreg
  | Ret -> lift_ret sreg
  | Retf -> lift_retf sreg
  | Reti imm -> lift_reti imm sreg
  | Retfi imm -> lift_retfi imm sreg
  | SetCc (cc, dst) ->
      let lvalue = disas_lval8 dst sreg in
      let e =
        Dba.Expr.ite (cond_of_cc cc) (cst_of_int 1 8) (Dba.Expr.zeros 8)
      in
      [ Predba.assign lvalue e ]
  | Nop -> []
  | Wait -> []
  | Emms -> []
  | Prefetch _ -> []
  | Not (mode, gop) -> lift_not mode gop sreg
  | Neg (mode, gop) -> lift_neg mode gop sreg
  | Halt -> [ Predba.stop Dba.OK ]
  | Cmc -> [ assign_flag CF (Dba.Expr.lognot cf_flag) ]
  | Clc -> [ assign_flag CF Dba.Expr.zero ]
  | Stc -> [ assign_flag CF Dba.Expr.one ]
  | Cld -> [ assign_flag DF Dba.Expr.zero ]
  | Std -> [ assign_flag DF Dba.Expr.one ]
  | Inc (mode, gop) -> lift_inc mode gop sreg
  | Dec (mode, gop) -> lift_dec mode gop sreg
  | Xchg (mode, gop1, gop2) -> lift_xchg mode gop1 gop2 sreg
  | Mul (mode, gop) -> lift_mul mode gop sreg
  | IMul (mode, gop1_opt, gop2, gop3) -> lift_imul gop1_opt gop2 mode gop3 sreg
  | Div (mode, gop) -> lift_div mode gop sreg
  | IDiv (mode, gop) -> lift_idiv mode gop sreg
  | CBW mode -> lift_cbw mode
  | CWD mode -> lift_cwd mode
  | Bsr (mode, r, gop) -> lift_bsr (mode :> X86Types.sizeMode) r gop sreg
  | Bsf (mode, r, gop) -> lift_bsf (mode :> X86Types.sizeMode) r gop sreg
  | Bswap (mode, r) -> lift_bswap mode r
  | Xadd (mode, gop1, gop2) -> lift_xadd mode gop1 gop2 sreg
  | Jcxz (mode, src) -> lift_jcxz mode src
  | CmpXchg8b (reg_t, size_t, gop) -> lift_cmpXchg8b reg_t size_t gop sreg
  | Pshufw (reg_t, size_t, r, gop, imm) ->
      lift_pshuf reg_t size_t r gop imm 0 64 sreg
  | Pshuflw (reg_t, size_t, r, gop, imm) ->
      lift_pshuf reg_t size_t r gop imm 0 64 sreg
  | Pshufhw (reg_t, size_t, r, gop, imm) ->
      lift_pshuf reg_t size_t r gop imm 64 128 sreg
  | Pshufd (reg_t, size_t, r, gop, imm) ->
      lift_pshuf reg_t size_t r gop imm 0 128 sreg
  | Movntq (xmm, size, gop1, gop2) ->
      let lval = disas_lval_xmm gop1 xmm size sreg
      and e = disas_expr_xmm gop2 xmm size sreg in
      [ Predba.assign lval e ]
  | Movhpd (mm, gop1, gop2) | Movhps (mm, gop1, gop2) | Movlhps (mm, gop1, gop2)
    ->
      [ assign_xmm gop1 64 127 gop2 0 63 XMM mm sreg ]
  | Movshdup (mm, gop1, gop2) -> lift_movshdup mm gop1 gop2 sreg
  | Padd (xmm, mm, gop1, gop2, range) -> lift_padd range xmm mm gop1 gop2 sreg
  | Padds (xmm, mm, gop1, gop2, range) -> lift_padds range xmm mm gop1 gop2 sreg
  | Paddus (xmm, mm, gop1, gop2, range) ->
      lift_paddus range xmm mm gop1 gop2 sreg
  | Psub (xmm, mm, gop1, gop2, range) -> lift_psub range xmm mm gop1 gop2 sreg
  | Psubs (xmm, mm, gop1, gop2, range) -> lift_psubs range xmm mm gop1 gop2 sreg
  | Psubus (xmm, mm, gop1, gop2, range) ->
      lift_psubus range xmm mm gop1 gop2 sreg
  | Pavgu (xmm, mm, gop1, gop2, size) -> lift_pavgu size xmm mm gop1 gop2 sreg
  | Pmulhw (xmm, mm, gop1, gop2) -> lift_pmulhw xmm mm gop1 gop2 sreg
  | Pmulhrw (xmm, mm, gop1, gop2) -> lift_pmulhrw xmm mm gop1 gop2 sreg
  | Pmullw (xmm, mm, gop1, gop2) -> lift_pmullw xmm mm gop1 gop2 sreg
  | Pmuludq (xmm, mm, gop1, gop2) -> lift_pmuludq xmm mm gop1 gop2 sreg
  | Psrl (xmm, size, gop1, gop2, range) ->
      lift_psrl range xmm size gop1 gop2 sreg
  | Psll (xmm, size, gop1, gop2, range) ->
      lift_psll range xmm size gop1 gop2 sreg
  | Psra (xmm, size, gop1, gop2, range) ->
      lift_psra range xmm size gop1 gop2 sreg
  | Psrldq (gop, imm) -> lift_ps_ldq Dba.Binary_op.RShiftU gop imm sreg
  | Pslldq (gop, imm) -> lift_ps_ldq Dba.Binary_op.LShift gop imm sreg
  | Ptest (xmm, size, gop1, gop2) -> lift_ptest xmm size gop1 gop2 sreg
  | Punpckl (xmm, size, gop1, gop2, range) ->
      lift_punpckl xmm size gop1 gop2 range sreg
  | Punpckh (xmm, size, gop1, gop2, range) ->
      lift_punpckh xmm size gop1 gop2 range sreg
  | Packus (xmm, size, gop1, gop2, range) ->
      lift_packus xmm size gop1 gop2 range sreg
  | Packss (xmm, size, gop1, gop2, range) ->
      lift_packss xmm size gop1 gop2 range sreg
  | Pmaddwd (xmm, size, gop1, gop2) -> lift_pmaddwd xmm size gop1 gop2 sreg
  | Pmaddusbsw (xmm, size, gop1, gop2) ->
      lift_pmaddusbsw xmm size gop1 gop2 sreg
  | Pcmpgtb (xmm, size, gop1, gop2) -> lift_pcmpgt 8 xmm size gop1 gop2 sreg
  | Pcmpgtw (xmm, size, gop1, gop2) -> lift_pcmpgt 16 xmm size gop1 gop2 sreg
  | Pcmpgtd (xmm, size, gop1, gop2) -> lift_pcmpgt 32 xmm size gop1 gop2 sreg
  | Movups (gop1, gop2) | Movupd (gop1, gop2) ->
      [
        Predba.assign
          (disas_lval_xmm gop1 XMM S128 sreg)
          (disas_expr_xmm gop2 XMM S128 sreg);
      ]
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
  | Lzcnt (mode, dest, src) -> lift_lzcnt mode dest src sreg
  | Pclmulqdq (xmm, size, gop1, gop2, select) ->
      lift_pclmulqdq xmm size gop1 gop2 select sreg
  | Lgdt (mode, gop) -> lift_ldt "gdt" mode gop sreg
  | Lidt (mode, gop) -> lift_ldt "idt" mode gop sreg
  | Ltr op16 -> lift_ltr op16 sreg
  | Iret mode -> lift_iret mode
  | OutPortImm _imm8 ->
      (* FIXME: treat as noop for now. *)
      []
  | OutPortDx ->
      (* FIXME: treat as noop for now. *)
      []
  | Unsupported _ -> [ Predba.stop (Dba.Unsupported opcode) ]
  | Undecoded -> [ Predba.stop (Dba.Undecoded opcode) ]

(* End instruction_to_dba *)

let aux_decode ins addr nextaddr sreg opcode =
  let dba_instructions = instruction_to_dba sreg addr nextaddr opcode ins in
  let check elem =
    try
      if not (Dba_utils.checksize_instruction elem) then
        raise (Invalid_argument "Failed size check")
    with exn ->
      let reason, msg =
        match exn with
        | Errors.Bad_exp_size -> ("Bad expression size", "")
        | Errors.Bad_bound s -> ("Bad bound ", s)
        | Errors.Size_error s -> ("Size error", s)
        | Errors.Bad_address_size -> ("Wrong address size", "")
        | Invalid_argument s -> (s, "")
        | _ -> ("Unknown exception", "")
      in
      Logger.error "Check failed : %s%s %@ %a" reason msg
        Dba_printer.Ascii.pp_instruction elem;
      exit 2
  in
  let dba_block = Predba.blockify nextaddr dba_instructions in
  Dhunk.iter ~f:check dba_block;
  dba_instructions

let decaux basic_instr addr sreg =
  let open X86Instruction in
  let nextaddr =
    Virtual_address.add_int (Size.Byte.to_int basic_instr.size) addr
    |> Dba_types.Caddress.of_virtual_address
  in
  aux_decode basic_instr.mnemonic addr nextaddr sreg basic_instr.opcode

let x86decode = X86decoder.read

let x86lift_from_reader addr reader =
  let binstr, sreg = x86decode addr reader in
  (* convert x86 IR -> DBA *)
  let insts = decaux binstr addr sreg in
  let nextaddr =
    Virtual_address.add_int (Size.Byte.to_int binstr.X86Instruction.size) addr
    |> Dba_types.Caddress.of_virtual_address
  in
  let block = Predba.blockify nextaddr insts in
  assert (Dhunk.Check.has_inbound_inner_jumps block);
  assert (Dhunk.Check.no_temporary_leak block);
  (binstr, block)

let decode reader (addr : Virtual_address.t) = x86lift_from_reader addr reader

(* addr_size in Bytes *)
(* A base address of 0 is used here by default.
*)
let decode_binstream ?(base_addr : Virtual_address.t = Virtual_address.zero)
    hopc =
  try Lreader.of_binstream hopc |> x86lift_from_reader base_addr
  with Failure s -> raise (InstructionUnhandled s)
