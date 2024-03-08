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

module M = Machine
module A = Z80_arch

module I = struct
  include Dba.Instr

  let static_relative_jump ?tag pc offset =
    static_outer_jump ?tag (Virtual_address.add_int offset pc)

  let jump_cc e pc i =
    ite e (Dba.Jump_target.outer (Dba_types.Caddress.of_virtual_address pc)) i

  let static_call pc target =
    static_outer_jump
      ~tag:(Call (Dba_types.Caddress.of_virtual_address pc))
      target
end

module B = Bitvector
module E = Dba.Expr
module L = Dba.LValue

type t =
  | F of string * I.t array  (** Fallthrough *)
  | T of string * Dhunk.t  (** Terminator *)
  | U of string  (** Unhandled *)
  | I  (** Invalid *)

let two16 = E.constant (B.of_int ~size:16 2)
let one8, one16 = (E.ones 8, E.ones 16)
let zero4, zero8, zero16 = (E.zeros 4, E.zeros 8, E.zeros 16)
let smax8 = E.constant (B.max_sbv 8)
let umax4 = E.constant (B.max_ubv 4)
let smin8 = E.constant (B.min_sbv 8)

let six4, nine4, ten4 =
  ( E.constant (B.of_int ~size:4 0x6),
    E.constant (B.of_int ~size:4 0x9),
    E.constant (B.of_int ~size:4 0xa) )

let b1, b2 = (Size.Byte.create 1, Size.Byte.create 2)

let lt4, et4 =
  let bitsize = Size.Bit.create 4 in
  let t4 = Dba.Var.create "%%0" ~bitsize ~tag:Dba.Var.Tag.Temp in
  (L.v t4, E.v t4)

let lt8, et8 =
  let t8 = Dba.Var.create "%%0" ~bitsize:Size.Bit.bits8 ~tag:Dba.Var.Tag.Temp in
  (L.v t8, E.v t8)

let lt16, et16 =
  let t16 =
    Dba.Var.create "%%0" ~bitsize:Size.Bit.bits16 ~tag:Dba.Var.Tag.Temp
  in
  (L.v t16, E.v t16)

module Cond = struct
  type t = NZ | Z | NC | C | PO | PE | P | M

  let to_string = function
    | NZ -> "NZ"
    | Z -> "Z"
    | NC -> "NC"
    | C -> "C"
    | PO -> "PO"
    | PE -> "PE"
    | P -> "P"
    | M -> "M"

  let cc = function
    | 0b000 -> NZ
    | 0b001 -> Z
    | 0b010 -> NC
    | 0b011 -> C
    | 0b100 -> PO
    | 0b101 -> PE
    | 0b110 -> P
    | 0b111 -> M
    | _ -> assert false

  let ss = function
    | 0b11 -> C
    | 0b10 -> NC
    | 0b01 -> Z
    | 0b00 -> NZ
    | _ -> assert false

  let sf, zf, vf, cf = (A.(expr sf), A.(expr zf), A.(expr vf), A.(expr cf))

  let expr = function
    | NZ -> E.lognot zf
    | Z -> zf
    | NC -> E.lognot cf
    | C -> cf
    | PO -> E.lognot vf
    | PE -> vf
    | P -> E.lognot sf
    | M -> sf
end

module R8 = struct
  let r = function
    | 0b000 -> A.b
    | 0b001 -> A.c
    | 0b010 -> A.d
    | 0b011 -> A.e
    | 0b100 -> A.h
    | 0b101 -> A.l
    | 0b111 -> A.a
    | _ -> assert false

  let p = function
    | 0b000 -> A.b
    | 0b001 -> A.c
    | 0b010 -> A.d
    | 0b011 -> A.e
    | 0b100 -> A.ixh
    | 0b101 -> A.ixl
    | 0b111 -> A.a
    | _ -> assert false

  let q = function
    | 0b000 -> A.b
    | 0b001 -> A.c
    | 0b010 -> A.d
    | 0b011 -> A.e
    | 0b100 -> A.iyh
    | 0b101 -> A.iyl
    | 0b111 -> A.a
    | _ -> assert false
end

(* 8.1 8-Bit Load Group *)
module LD8 = struct
  let ld dst_str src_str dst src =
    F (Format.sprintf "LD %s, %s" dst_str src_str, [| I.assign dst src 1 |])

  let ld_reg_reg dst src =
    ld (A.name dst) (A.name src) (A.lval dst) (A.expr src)

  let ld_reg_imm dst src =
    ld (A.name dst)
      (Format.sprintf "%02xh" src)
      (A.lval dst)
      (E.constant (B.of_int ~size:8 src))

  let ld_reg_mem dst mem load = ld (A.name dst) mem (A.lval dst) load

  let ld_reg_memdisp dst addr disp =
    ld (A.name dst)
      (Format.sprintf "(%s+%d)" (A.name addr) disp)
      (A.lval dst)
      (E.load b1 M.LittleEndian
         (E.add (A.expr addr) (E.constant (B.of_int ~size:16 disp))))

  let ld_mem_reg mem store src = ld mem (A.name src) store (A.expr src)

  let ld_memdisp_reg addr disp src =
    ld
      (Format.sprintf "(%s+%d)" (A.name addr) disp)
      (A.name src)
      (L.store b1 M.LittleEndian
         (E.add (A.expr addr) (E.constant (B.of_int ~size:16 disp))))
      (A.expr src)

  let ld_mem_imm mem store src =
    ld mem
      (Format.sprintf "%02xh" src)
      store
      (E.constant (B.of_int ~size:8 src))

  let ld_memdisp_imm addr disp src =
    ld
      (Format.sprintf "(%s+%d)" (A.name addr) disp)
      (Format.sprintf "%02xh" src)
      (L.store b1 M.LittleEndian
         (E.add (A.expr addr) (E.constant (B.of_int ~size:16 disp))))
      (E.constant (B.of_int ~size:8 src))

  let ld_r_r' r r' = ld_reg_reg (R8.r r) (R8.r r')
  let ld_p_p' p p' = ld_reg_reg (R8.p p) (R8.p p')
  let ld_q_q' q q' = ld_reg_reg (R8.q q) (R8.q q')
  let ld_r_n r n = ld_reg_imm (R8.r r) n
  let ld_p_n p n = ld_reg_imm (R8.p p) n
  let ld_q_n q n = ld_reg_imm (R8.q q) n
  let ld_r_HL r = ld_reg_mem (R8.r r) A.(mem hl) A.(load1 hl)
  let ld_r_IXd r d = ld_reg_memdisp (R8.r r) A.ix d
  let ld_r_IYd r d = ld_reg_memdisp (R8.r r) A.iy d
  let ld_HL_r r = ld_mem_reg A.(mem hl) A.(store1 hl) (R8.r r)
  let ld_IXd_r d r = ld_memdisp_reg A.ix d (R8.r r)
  let ld_IYd_r d r = ld_memdisp_reg A.iy d (R8.r r)
  let ld_HL_n n = ld_mem_imm A.(mem hl) A.(store1 hl) n
  let ld_IXd_n d n = ld_memdisp_imm A.ix d n
  let ld_IYd_n d n = ld_memdisp_imm A.iy d n
  let ld_a_BC = ld_reg_mem A.a A.(mem bc) A.(load1 bc)
  let ld_a_DE = ld_reg_mem A.a A.(mem de) A.(load1 de)

  let ld_a_NN nn =
    ld_reg_mem A.a
      (Format.sprintf "(%04x)" nn)
      (E.load b1 M.LittleEndian (E.constant (B.of_int ~size:16 nn)))

  let ld_BC_a = ld_mem_reg A.(mem bc) A.(store1 bc) A.a
  let ld_DE_a = ld_mem_reg A.(mem de) A.(store1 de) A.a

  let ld_NN_a nn =
    ld_mem_reg
      (Format.sprintf "(%04x)" nn)
      (L.store b1 M.LittleEndian (E.constant (B.of_int ~size:16 nn)))
      A.a

  let ld_a_i = U "LD A, I"
  let ld_a_r = U "LD A, R"
  let ld_i_a = U "LD I, A"
  let ld_r_a = U "LD R, A"
end

(* 8.2 16-Bit Load Group *)
module LD16 = struct
  module R16 = struct
    let dd = function
      | 0b00 -> A.bc
      | 0b01 -> A.de
      | 0b10 -> A.hl
      | 0b11 -> A.sp
      | _ -> assert false

    let qq = function
      | 0b00 -> A.bc
      | 0b01 -> A.de
      | 0b10 -> A.hl
      | _ -> assert false
  end

  let ld = LD8.ld
  let ld_reg_reg = LD8.ld_reg_reg
  let ld_reg_mem = LD8.ld_reg_mem
  let ld_mem_reg = LD8.ld_mem_reg

  let ld_reg_imm dst src =
    ld (A.name dst)
      (Format.sprintf "%04xh" src)
      (A.lval dst)
      (E.constant (B.of_int ~size:16 src))

  let ld_reg_NN r nn =
    ld_reg_mem r
      (Format.sprintf "(%04x)" nn)
      (E.load b2 M.LittleEndian (E.constant (B.of_int ~size:16 nn)))

  let ld_NN_reg nn r =
    ld_mem_reg
      (Format.sprintf "(%04x)" nn)
      (L.store b2 M.LittleEndian (E.constant (B.of_int ~size:16 nn)))
      r

  let ld_dd_nn dd nn = ld_reg_imm (R16.dd dd) nn
  let ld_ix_nn nn = ld_reg_imm A.ix nn
  let ld_iy_nn nn = ld_reg_imm A.iy nn
  let ld_hl_NN nn = ld_reg_NN A.hl nn
  let ld_dd_NN dd nn = ld_reg_NN (R16.dd dd) nn
  let ld_ix_NN nn = ld_reg_NN A.ix nn
  let ld_iy_NN nn = ld_reg_NN A.iy nn
  let ld_NN_hl nn = ld_NN_reg nn A.hl
  let ld_NN_dd nn dd = ld_NN_reg nn (R16.dd dd)
  let ld_NN_ix nn = ld_NN_reg nn A.ix
  let ld_NN_iy nn = ld_NN_reg nn A.iy
  let ld_sp_hl = ld_reg_reg A.sp A.hl
  let ld_sp_ix = ld_reg_reg A.sp A.ix
  let ld_sp_iy = ld_reg_reg A.sp A.iy

  let push src_str src =
    F
      ( Format.sprintf "PUSH %s" src_str,
        [|
          I.assign A.(lval sp) (E.sub A.(expr sp) two16) 1;
          I.assign A.(store2 sp) src 2;
        |] )

  let push_af = push "AF" (E.append A.(expr a) A.(expr f))
  let push_reg r = push (A.name r) (A.expr r)
  let push_qq qq = push_reg (R16.qq qq)
  let push_ix = push_reg A.ix
  let push_iy = push_reg A.iy

  let pop_af =
    F
      ( Format.sprintf "POP AF",
        [|
          I.assign A.(lval f) A.(load1 sp) 1;
          I.assign A.(lval sp) (E.add A.(expr sp) one16) 2;
          I.assign A.(lval a) A.(load1 sp) 3;
          I.assign A.(lval sp) (E.add A.(expr sp) one16) 4;
        |] )

  let pop_reg dst =
    F
      ( Format.sprintf "POP %s" (A.name dst),
        [|
          I.assign (A.lval dst) A.(load2 sp) 1;
          I.assign A.(lval sp) (E.add A.(expr sp) two16) 2;
        |] )

  let pop_qq qq = pop_reg (R16.qq qq)
  let pop_ix = pop_reg A.ix
  let pop_iy = pop_reg A.iy
end

(* 8.3 Exchange, Block Transfer, Search Group *)
module EBTS = struct
  let ex_de_hl =
    F
      ( "EX DE, HL",
        [|
          I.assign lt16 A.(expr hl) 1;
          I.assign A.(lval hl) A.(expr de) 2;
          I.assign A.(lval de) et16 3;
        |] )

  let ex_af_af' =
    F
      ( "EX AF, AF'",
        [|
          I.assign lt8 A.(expr a) 1;
          I.assign A.(lval a) A.(expr a') 2;
          I.assign A.(lval a') et8 3;
          I.assign lt8 A.(expr f) 4;
          I.assign A.(lval f) A.(expr f') 5;
          I.assign A.(lval f') et8 6;
        |] )

  let exx =
    F
      ( "EXX",
        [|
          I.assign lt16 A.(expr bc) 1;
          I.assign A.(lval bc) A.(expr bc') 2;
          I.assign A.(lval bc') et16 3;
          I.assign lt16 A.(expr de) 4;
          I.assign A.(lval de) A.(expr de') 5;
          I.assign A.(lval de') et16 6;
          I.assign lt16 A.(expr hl) 7;
          I.assign A.(lval hl) A.(expr hl') 8;
          I.assign A.(lval hl') et16 9;
        |] )

  let ex_SP_reg r =
    F
      ( Format.sprintf "EX (SP), %s" (A.name r),
        [|
          I.assign lt16 (A.expr r) 1;
          I.assign (A.lval r) A.(load2 sp) 2;
          I.assign A.(store2 sp) et16 3;
        |] )

  let ex_SP_hl = ex_SP_reg A.hl
  let ex_SP_IX = ex_SP_reg A.ix
  let ex_SP_IY = ex_SP_reg A.iy

  let flag_ld =
    let sf_x5 = E.restrict 5 7 A.(expr f) in
    let hf = E.zero in
    let x3 = E.restrict 3 3 A.(expr f) in
    let vf = E.diff A.(expr bc) zero16 in
    let nf = E.zero in
    let c = E.restrict 0 0 A.(expr f) in
    Array.fold_right E.append [| sf_x5; hf; x3; vf; nf |] c

  let ld_f f =
    [|
      I.assign A.(store1 de) A.(load1 hl) 1;
      I.assign A.(lval de) (f A.(expr de) one16) 2;
      I.assign A.(lval hl) (f A.(expr hl) one16) 3;
      I.assign A.(lval bc) (E.sub A.(expr bc) one16) 4;
      I.assign A.(lval f) flag_ld 5;
    |]

  let ld_f_r f pc =
    Dhunk.init 7 (function
      | 0 -> I.assign A.(store1 de) A.(load1 hl) 1
      | 1 -> I.assign A.(lval de) (f A.(expr de) one16) 2
      | 2 -> I.assign A.(lval hl) (f A.(expr hl) one16) 3
      | 3 -> I.assign A.(lval bc) (E.sub A.(expr bc) one16) 4
      | 4 -> I.assign A.(lval f) flag_ld 5
      | 5 -> I.jump_cc (E.diff A.(expr bc) zero16) pc 6
      | _ -> I.static_relative_jump pc 2)

  let ldi = F ("LDI", ld_f E.add)
  let ldir pc = T ("LDIR", ld_f_r E.add pc)
  let ldd = F ("LDD", ld_f E.sub)
  let lddr pc = T ("LDDR", ld_f_r E.sub pc)

  let flag_cp =
    let sf = E.restrict 7 7 et8 in
    let zf = E.equal et8 zero8 in
    let x5 = E.restrict 5 5 A.(expr f) in
    let hf =
      E.diff (E.restrict 4 4 et8)
        (E.logxor (E.restrict 4 4 A.(expr a)) (E.restrict 4 4 A.(load1 hl)))
    in
    let x3 = E.restrict 3 3 A.(expr f) in
    let vf = E.diff A.(expr bc) zero16 in
    let nf = E.one in
    let c = E.restrict 0 0 A.(expr f) in
    Array.fold_right E.append [| sf; zf; x5; hf; x3; vf; nf |] c

  let cp_f f =
    [|
      I.assign A.(lval bc) (E.sub A.(expr bc) one16) 1;
      I.assign lt8 (E.sub A.(expr a) A.(load1 hl)) 2;
      I.assign A.(lval f) flag_cp 3;
      I.assign A.(lval hl) (f A.(expr hl) one16) 4;
    |]

  let cp_f_r f pc =
    Dhunk.init 6 (function
      | 0 -> I.assign A.(lval bc) (E.sub A.(expr bc) one16) 1
      | 1 -> I.assign lt8 (E.sub A.(expr a) A.(load1 hl)) 2
      | 2 -> I.assign A.(lval f) flag_cp 3
      | 3 -> I.assign A.(lval hl) (f A.(expr hl) one16) 4
      | 4 -> I.jump_cc (E.diff A.(expr bc) zero16) pc 5
      | _ -> I.static_relative_jump pc 2)

  let cpi = F ("CPI", cp_f E.add)
  let cpir pc = T ("CPIR", cp_f_r E.add pc)
  let cpd = F ("CPD", cp_f E.sub)
  let cpdr pc = T ("CPDR", cp_f_r E.sub pc)
end

(* 8.4 8-Bit Arithmetic and Logical Group *)
module Arith8 = struct
  type op = ADD | ADC | SUB | SBC | AND | OR | XOR

  let op = function
    | 0b000 -> ADD
    | 0b001 -> ADC
    | 0b010 -> SUB
    | 0b011 -> SBC
    | 0b100 -> AND
    | 0b110 -> OR
    | 0b101 -> XOR
    | _ -> assert false

  let to_string = function
    | ADD -> "ADD"
    | ADC -> "ADC"
    | SUB -> "SUB"
    | SBC -> "SBC"
    | AND -> "AND"
    | OR -> "OR"
    | XOR -> "XOR"

  let build_flag8 ?(sf = E.restrict 7 7 et8) ?(zf = E.equal et8 zero8)
      ?(yf = E.restrict 5 5 A.(expr f)) ?(hf = E.zero)
      ?(xf = E.restrict 3 3 A.(expr f)) ~vf ?(nf = E.zero)
      ?(cf = E.restrict 0 0 A.(expr f)) () =
    Array.fold_right E.append [| sf; zf; yf; hf; xf; vf; nf |] cf

  let flag_add src =
    build_flag8
      ~hf:
        (E.restrict 4 4
           (E.add
              (E.uext 5 (E.restrict 0 3 A.(expr a)))
              (E.uext 5 (E.restrict 0 3 src))))
      ~vf:(E.diff (E.add (E.sext 9 A.(expr a)) (E.sext 9 src)) (E.sext 9 et8))
      ~cf:(E.restrict 8 8 (E.add (E.uext 9 A.(expr a)) (E.uext 9 src)))
      ()

  let flag_adc src =
    build_flag8
      ~hf:
        (E.restrict 4 4
           (E.add
              (E.add
                 (E.uext 5 (E.restrict 0 3 A.(expr a)))
                 (E.uext 5 (E.restrict 0 3 src)))
              (E.uext 5 Cond.cf)))
      ~vf:
        (E.diff
           (E.add
              (E.add (E.sext 9 A.(expr a)) (E.sext 9 src))
              (E.uext 9 Cond.cf))
           (E.sext 9 et8))
      ~cf:
        (E.restrict 8 8
           (E.add
              (E.add (E.uext 9 A.(expr a)) (E.uext 9 src))
              (E.uext 9 Cond.cf)))
      ()

  let flag_sub src =
    build_flag8
      ~hf:(E.ult (E.restrict 0 3 A.(expr a)) (E.restrict 0 3 src))
      ~vf:(E.diff (E.sub (E.sext 9 A.(expr a)) (E.sext 9 src)) (E.sext 9 et8))
      ~nf:E.one
      ~cf:(E.ult A.(expr a) src)
      ()

  let flag_sbc src =
    build_flag8
      ~hf:
        (E.ult
           (E.uext 5 (E.restrict 0 3 A.(expr a)))
           (E.add (E.uext 5 (E.restrict 0 3 src)) (E.uext 5 Cond.cf)))
      ~vf:
        (E.diff
           (E.sub
              (E.sext 9 A.(expr a))
              (E.add (E.sext 9 src) (E.uext 9 Cond.cf)))
           (E.sext 9 et8))
      ~nf:E.one
      ~cf:
        (E.ult (E.uext 9 A.(expr a)) (E.add (E.uext 9 src) (E.uext 9 Cond.cf)))
      ()

  let pf =
    let rec fold e n =
      if n < 0 then E.lognot e
      else fold (E.logxor e (E.restrict n n et8)) (n - 1)
    in
    fold E.zero 7

  let flag_and _ = build_flag8 ~hf:E.one ~vf:pf ~cf:E.zero ()
  let flag_or _ = build_flag8 ~vf:pf ~cf:E.zero ()
  let flag_xor _ = build_flag8 ~vf:pf ~cf:E.zero ()

  let flag op src =
    match op with
    | ADD -> flag_add src
    | ADC -> flag_adc src
    | SUB -> flag_sub src
    | SBC -> flag_sbc src
    | AND -> flag_and src
    | OR -> flag_or src
    | XOR -> flag_xor src

  let app op a b =
    match op with
    | ADD -> E.add a b
    | ADC -> E.add a (E.add b Cond.cf)
    | SUB -> E.sub a b
    | SBC -> E.sub a (E.add b Cond.cf)
    | AND -> E.logand a b
    | OR -> E.logor a b
    | XOR -> E.logxor a b

  let op_a_s f src_str src =
    let op = op f in
    F
      ( Format.sprintf "%s A, %s" (to_string op) src_str,
        [|
          I.assign lt8 (app op A.(expr a) src) 1;
          I.assign A.(lval f) (flag op src) 2;
          I.assign A.(lval a) et8 3;
        |] )

  let op_a_reg op r = op_a_s op (A.name r) (A.expr r)
  let op_a_r op r = op_a_reg op (R8.r r)
  let op_a_p op p = op_a_reg op (R8.p p)
  let op_a_q op q = op_a_reg op (R8.q q)

  let op_a_n op n =
    op_a_s op (Format.sprintf "%02xh" n) (E.constant (B.of_int ~size:8 n))

  let op_a_HL op = op_a_s op A.(mem hl) A.(load1 hl)

  let op_a_memdisp op addr disp =
    op_a_s op
      (Format.sprintf "(%s+%d)" (A.name addr) disp)
      (E.load b1 M.LittleEndian
         (E.add (A.expr addr) (E.constant (B.of_int ~size:16 disp))))

  let op_a_IXd op d = op_a_memdisp op A.ix d
  let op_a_IYd op d = op_a_memdisp op A.iy d

  let cp_a_s src_str src =
    F
      ( Format.sprintf "CP A, %s" src_str,
        [|
          I.assign lt8 (E.sub A.(expr a) src) 1;
          I.assign A.(lval f) (flag_sub src) 2;
        |] )

  let cp_a_reg r = cp_a_s (A.name r) (A.expr r)
  let cp_a_r r = cp_a_reg (R8.r r)
  let cp_a_p p = cp_a_reg (R8.p p)
  let cp_a_q q = cp_a_reg (R8.q q)

  let cp_a_n n =
    cp_a_s (Format.sprintf "%02xh" n) (E.constant (B.of_int ~size:8 n))

  let cp_a_HL = cp_a_s A.(mem hl) A.(load1 hl)

  let cp_a_memdisp addr disp =
    cp_a_s
      (Format.sprintf "(%s+%d)" (A.name addr) disp)
      (E.load b1 M.LittleEndian
         (E.add (A.expr addr) (E.constant (B.of_int ~size:16 disp))))

  let cp_a_IXd d = cp_a_memdisp A.ix d
  let cp_a_IYd d = cp_a_memdisp A.iy d

  let flag_inc src =
    build_flag8
      ~hf:(E.equal (E.restrict 0 3 src) umax4)
      ~vf:(E.equal src smax8) ()

  let incdec_reg op f g r =
    F
      ( Format.sprintf "%s %s" op (A.name r),
        [|
          I.assign lt8 (f (A.expr r) one8) 1;
          I.assign A.(lval f) (g (A.expr r)) 2;
          I.assign (A.lval r) et8 3;
        |] )

  let incdec_HL op f g =
    let (D { store1; load1; _ }) = A.hl in
    F
      ( Format.sprintf "%s (HL)" op,
        [|
          I.assign lt8 (f load1 one8) 1;
          I.assign A.(lval f) (g load1) 2;
          I.assign store1 et8 3;
        |] )

  let incdec_memdisp op f g A.(D { name; expr; _ }) disp =
    let addr = E.add expr (E.constant (B.of_int ~size:16 disp)) in
    let load1 = E.load b1 M.LittleEndian addr in
    let store1 = L.store b1 M.LittleEndian addr in
    F
      ( Format.sprintf "%s (%s+%d)" op name disp,
        [|
          I.assign lt8 (f load1 one8) 1;
          I.assign A.(lval f) (g load1) 2;
          I.assign store1 et8 3;
        |] )

  let inc_reg r = incdec_reg "INC" E.add flag_inc r
  let inc_r r = inc_reg (R8.r r)
  let inc_p p = inc_reg (R8.p p)
  let inc_q q = inc_reg (R8.q q)
  let inc_HL = incdec_HL "INC" E.add flag_inc
  let inc_memdisp r d = incdec_memdisp "INC" E.add flag_inc r d
  let inc_IXd d = inc_memdisp A.ix d
  let inc_IYd d = inc_memdisp A.iy d

  let flag_dec src =
    build_flag8
      ~hf:(E.equal (E.restrict 0 3 src) zero4)
      ~vf:(E.equal src smin8) ~nf:E.one ()

  let dec_reg r = incdec_reg "DEC" E.sub flag_dec r
  let dec_r r = dec_reg (R8.r r)
  let dec_p p = dec_reg (R8.p p)
  let dec_q q = dec_reg (R8.q q)
  let dec_HL = incdec_HL "DEC" E.sub flag_dec
  let dec_memdisp r d = incdec_memdisp "DEC" E.sub flag_dec r d
  let dec_IXd d = dec_memdisp A.ix d
  let dec_IYd d = dec_memdisp A.iy d
end

(* 8.5 General-Purpose Arithmetic and CPU Control Group *)
module GPAC = struct
  let daa =
    let hf = A.(expr hf)
    and nf = A.(expr nf)
    and cf = A.(expr cf)
    and a = A.(expr a) in
    let al = E.restrict 0 3 a and ah = E.restrict 4 7 a in
    let ln = E.uge al ten4 in
    let cf' =
      E.logor cf (E.logor (E.uge ah ten4) (E.logand (E.equal ah nine4) ln))
    in
    let acc =
      let diff =
        E.append (E.ite cf' six4 zero4) (E.ite (E.logor hf ln) six4 zero4)
      in
      E.ite nf (E.sub a diff) (E.add a diff)
    in
    let hf' = E.ite nf (E.logand hf (E.ult al six4)) ln in
    let flag =
      Arith8.build_flag8 ~hf:hf' ~vf:Arith8.pf ~nf:A.(expr nf) ~cf:cf' ()
    in
    F
      ( "DAA",
        [|
          I.assign lt8 acc 1;
          I.assign A.(lval f) flag 2;
          I.assign A.(lval a) et8 3;
        |] )

  let flag_cpl =
    let sf_x5 = E.restrict 5 7 A.(expr f) in
    let hf = E.one in
    let x3_vf = E.restrict 2 3 A.(expr f) in
    let nf = E.one in
    let c = E.restrict 0 0 A.(expr f) in
    Array.fold_right E.append [| sf_x5; hf; x3_vf; nf |] c

  let cpl =
    F
      ( "CPL",
        [|
          I.assign A.(lval f) flag_cpl 1;
          I.assign A.(lval a) (E.lognot A.(expr a)) 2;
        |] )

  let flag_neg =
    let f = A.(expr f) and a = A.(expr a) in
    let sf = E.sgt a zero8 in
    let zf = E.equal a zero8 in
    let yf = E.restrict 5 5 f in
    let hf = E.diff (E.restrict 0 3 a) zero4 in
    let xf = E.restrict 3 3 f in
    let vf = E.equal a smin8 in
    let nf = E.one in
    let cf = E.diff a zero8 in
    Array.fold_right E.append [| sf; zf; yf; hf; xf; vf; nf |] cf

  let neg =
    F
      ( "NEG",
        [|
          I.assign A.(lval f) flag_neg 1;
          I.assign A.(lval a) (E.uminus A.(expr a)) 2;
        |] )

  let ccf =
    let f = A.(expr f) in
    let sf_yf = E.restrict 5 7 f in
    let hf = E.restrict 0 0 f in
    let xf_vf = E.restrict 2 3 f in
    let nf = E.zero in
    let cf = E.lognot hf in
    let f' = Array.fold_right E.append [| sf_yf; hf; xf_vf; nf |] cf in
    F ("CCF", [| I.assign A.(lval f) f' 1 |])

  let scf =
    let f = A.(expr f) in
    let sf_yf = E.restrict 5 7 f in
    let hf = E.zero in
    let xf_vf = E.restrict 2 3 f in
    let nf = E.zero in
    let cf = E.one in
    let f' = Array.fold_right E.append [| sf_yf; hf; xf_vf; nf |] cf in
    F ("SCF", [| I.assign A.(lval f) f' 1 |])

  let nop = F ("NOP", [||])
  let halt = T ("HALT", Dhunk.stop)

  let set_iffts e =
    [| I.assign A.(lval ifft1) e 1; I.assign A.(lval ifft2) e 2 |]

  let di = F ("DI", set_iffts E.zero)
  let ei = F ("EI", set_iffts E.one)
  let im0 = U "IM 0"
  let im1 = U "IM 1"
  let im2 = U "IM 2"
end

(* 8.6 16-Bit Arithmetic Group *)
module Arith16 = struct
  module R16 = struct
    let ss = function
      | 0b00 -> A.bc
      | 0b01 -> A.de
      | 0b10 -> A.hl
      | 0b11 -> A.sp
      | _ -> assert false

    let pp = function
      | 0b00 -> A.bc
      | 0b01 -> A.de
      | 0b10 -> A.ix
      | 0b11 -> A.sp
      | _ -> assert false

    let qq = function
      | 0b00 -> A.bc
      | 0b01 -> A.de
      | 0b10 -> A.iy
      | 0b11 -> A.sp
      | _ -> assert false
  end

  let flag_add16 src1 src2 =
    let sf_yf = E.restrict 5 7 A.(expr f) in
    let hf =
      E.restrict 12 12
        (E.add
           (E.uext 13 (E.restrict 0 11 src1))
           (E.uext 13 (E.restrict 0 11 src2)))
    in
    let xf_vf = E.restrict 2 3 A.(expr f) in
    let nf = E.zero in
    let c = E.restrict 16 16 (E.add (E.uext 17 src1) (E.uext 17 src2)) in
    Array.fold_right E.append [| sf_yf; hf; xf_vf; nf |] c

  let add_reg_reg A.(D { name; expr; loc; _ })
      A.(D { name = name'; expr = expr'; _ }) =
    F
      ( Format.sprintf "ADD %s, %s" name name',
        [|
          I.assign lt16 (E.add expr expr') 1;
          I.assign A.(lval f) (flag_add16 expr expr') 2;
          I.assign loc et16 3;
        |] )

  let add_hl_ss ss = add_reg_reg A.hl (R16.ss ss)

  let flag_adc16 src =
    let f = A.(expr f) and cf = A.(expr cf) and hl = A.(expr hl) in
    let sf = E.restrict 15 15 et16 in
    let zf = E.equal et16 zero16 in
    let yf = E.restrict 5 5 f in
    let hf =
      E.restrict 12 12
        (E.add
           (E.add
              (E.uext 13 (E.restrict 0 11 hl))
              (E.uext 13 (E.restrict 0 11 src)))
           (E.uext 13 cf))
    in
    let xf = E.restrict 3 3 f in
    let vf =
      E.diff
        (E.add (E.add (E.sext 17 hl) (E.sext 17 src)) (E.uext 17 cf))
        (E.sext 17 et16)
    in
    let nf = E.zero in
    let cf =
      E.restrict 16 16
        (E.add (E.add (E.uext 17 hl) (E.uext 17 src)) (E.uext 17 cf))
    in
    Array.fold_right E.append [| sf; zf; yf; hf; xf; vf; nf |] cf

  let adc_hl_ss ss =
    let (D { name; expr; _ }) = R16.ss ss in
    F
      ( Format.sprintf "ADC HL, %s" name,
        [|
          I.assign lt16
            (E.add (E.add A.(expr hl) expr) (E.uext 16 A.(expr cf)))
            1;
          I.assign A.(lval f) (flag_adc16 expr) 2;
          I.assign A.(lval hl) et16 3;
        |] )

  let flag_sbc16 src =
    let f = A.(expr f) and cf = A.(expr cf) and hl = A.(expr hl) in
    let sf = E.restrict 15 15 et16 in
    let zf = E.equal et16 zero16 in
    let yf = E.restrict 5 5 f in
    let hf =
      E.ult
        (E.uext 13 (E.restrict 0 11 hl))
        (E.add (E.uext 13 (E.restrict 0 11 src)) (E.uext 13 cf))
    in
    let xf = E.restrict 3 3 f in
    let vf =
      E.diff
        (E.sub (E.sub (E.sext 17 hl) (E.sext 17 src)) (E.uext 17 cf))
        (E.sext 17 et16)
    in
    let nf = E.one in
    let cf = E.ult (E.uext 17 hl) (E.add (E.uext 17 src) (E.uext 17 cf)) in
    Array.fold_right E.append [| sf; zf; yf; hf; xf; vf; nf |] cf

  let sbc_hl_ss ss =
    let (D { name; expr; _ }) = R16.ss ss in
    F
      ( Format.sprintf "SBC HL, %s" name,
        [|
          I.assign lt16
            (E.sub (E.sub A.(expr hl) expr) (E.uext 16 A.(expr cf)))
            1;
          I.assign A.(lval f) (flag_sbc16 expr) 2;
          I.assign A.(lval hl) et16 3;
        |] )

  let add_ix_pp pp = add_reg_reg A.ix (R16.pp pp)
  let add_iy_qq qq = add_reg_reg A.iy (R16.qq qq)

  let op_reg op f (r : [ `x16 ] A.t) =
    let (D { name; expr; loc; _ }) = r in
    F (Format.sprintf "%s %s" op name, [| I.assign loc (f expr one16) 1 |])

  let inc_reg r = op_reg "INC" E.add r
  let dec_reg r = op_reg "DEC" E.sub r
  let inc_ss ss = inc_reg (R16.ss ss)
  let inc_ix = inc_reg A.ix
  let inc_iy = inc_reg A.iy
  let dec_ss ss = dec_reg (R16.ss ss)
  let dec_ix = dec_reg A.ix
  let dec_iy = dec_reg A.iy
end

(* 8.7 Rotate and Shift Group *)
module RS = struct
  let flag_rlca =
    let sf_yf = E.restrict 5 7 A.(expr f) in
    let hf = E.zero in
    let xf_vf = E.restrict 2 3 A.(expr f) in
    let nf = E.zero in
    let cf = E.restrict 7 7 A.(expr a) in
    Array.fold_right E.append [| sf_yf; hf; xf_vf; nf |] cf

  let rlca =
    F
      ( "RLCA",
        [|
          I.assign A.(lval f) flag_rlca 1;
          I.assign A.(lval a) (E.rotate_left A.(expr a) one8) 2;
        |] )

  let flag_rrca =
    let sf_yf = E.restrict 5 7 A.(expr f) in
    let hf = E.zero in
    let xf_vf = E.restrict 2 3 A.(expr f) in
    let nf = E.zero in
    let cf = E.restrict 0 0 A.(expr a) in
    Array.fold_right E.append [| sf_yf; hf; xf_vf; nf |] cf

  let rrca =
    F
      ( "RRCA",
        [|
          I.assign A.(lval f) flag_rrca 1;
          I.assign A.(lval a) (E.rotate_right A.(expr a) one8) 2;
        |] )

  let rla =
    F
      ( "RLA",
        [|
          I.assign A.(lval f) flag_rlca 1;
          I.assign A.(lval a) (E.append (E.restrict 0 6 A.(expr a)) Cond.cf) 4;
        |] )

  let rra =
    F
      ( "RRA",
        [|
          I.assign A.(lval f) flag_rrca 1;
          I.assign A.(lval a) (E.append Cond.cf (E.restrict 1 7 A.(expr a))) 2;
        |] )

  type op = RLC | RL | RRC | RR | SLA | SLL | SRA | SRL

  let op = function
    | 0b000 -> RLC
    | 0b001 -> RL
    | 0b010 -> RRC
    | 0b011 -> RR
    | 0b100 -> SLA
    | 0b110 -> SLL
    | 0b101 -> SRA
    | 0b111 -> SRL
    | _ -> assert false

  let to_string = function
    | RLC -> "RLC"
    | RL -> "RL"
    | RRC -> "RRC"
    | RR -> "RR"
    | SLA -> "SLA"
    | SLL -> "SLL"
    | SRA -> "SRA"
    | SRL -> "SRL"

  let build_flag8 ~cf =
    Arith8.build_flag8 ~hf:E.zero ~vf:Arith8.pf ~nf:E.zero ~cf ()

  let app op src =
    match op with
    | RLC -> E.rotate_left src one8
    | RL -> E.append (E.restrict 0 6 src) A.(expr cf)
    | RRC -> E.rotate_right src one8
    | RR -> E.append A.(expr cf) (E.restrict 1 7 src)
    | SLA -> E.shift_left src one8
    | SLL -> E.append (E.restrict 0 6 src) E.one
    | SRA -> E.shift_right_signed src one8
    | SRL -> E.shift_right src one8

  let flag op src =
    let cf =
      match op with
      | RLC | RL | SLA | SLL -> E.restrict 7 7 src
      | RRC | RR | SRA | SRL -> E.restrict 0 0 src
    in
    build_flag8 ~cf

  let op_m ?(lt8 = lt8) ?(et8 = et8) f str loc expr =
    let op = op f in
    F
      ( Format.sprintf "%s %s" (to_string op) str,
        [|
          I.assign lt8 (app op expr) 1;
          I.assign A.(lval f) (flag op expr) 2;
          I.assign loc et8 3;
        |] )

  let op_m_memdisp ?lt8 ?et8 ?(extra = "") op A.(D { name; expr; _ }) disp =
    let addr = E.add expr (E.constant (B.of_int ~size:16 disp)) in
    op_m ?lt8 ?et8 op
      (Format.sprintf "(%s+%d)%s" name disp extra)
      (L.store b1 M.LittleEndian addr)
      (E.load b1 M.LittleEndian addr)

  let op_r op r =
    let r = R8.r r in
    op_m op (A.name r) (A.lval r) (A.expr r)

  let op_HL op = op_m op A.(mem hl) A.(store1 hl) A.(load1 hl)
  let op_IXd op d = op_m_memdisp op A.ix d
  let op_IYd op d = op_m_memdisp op A.iy d

  let op_m_memdisp_r op r disp r' =
    let r' = R8.r r' in
    op_m_memdisp ~lt8:(A.lval r') ~et8:(A.expr r)
      ~extra:(Format.sprintf ", %s" (A.name r'))
      op r disp

  let op_IXd_r op d r = op_m_memdisp_r op A.ix d r
  let op_IYd_r op d r = op_m_memdisp_r op A.iy d r

  let flag =
    let a = A.(expr a) and f = A.(expr f) in
    let sf = E.restrict 7 7 a in
    let zf = E.equal a zero8 in
    let yf = E.restrict 5 5 f in
    let hf = E.zero in
    let xf = E.restrict 3 3 f in
    let pf =
      let rec fold e n =
        if n < 0 then E.lognot e
        else fold (E.logxor e (E.restrict n n a)) (n - 1)
      in
      fold E.zero 7
    in
    let nf = E.zero in
    let cf = E.restrict 0 0 f in
    Array.fold_right E.append [| sf; zf; yf; hf; xf; pf; nf |] cf

  let rld =
    let a = A.(expr a) and load1 = A.(load1 hl) in
    F
      ( "RLD",
        [|
          I.assign lt4 (E.restrict 0 3 a) 1;
          I.assign
            A.(lval a)
            (E.append (E.restrict 4 7 a) (E.restrict 4 7 load1))
            2;
          I.assign A.(store1 hl) (E.append (E.restrict 0 3 load1) et4) 3;
          I.assign A.(lval f) flag 4;
        |] )

  let rrd =
    let a = A.(expr a) and load1 = A.(load1 hl) in
    F
      ( "RRD",
        [|
          I.assign lt4 (E.restrict 0 3 a) 1;
          I.assign
            A.(lval a)
            (E.append (E.restrict 4 7 a) (E.restrict 0 3 load1))
            2;
          I.assign A.(store1 hl) (E.append et4 (E.restrict 4 7 load1)) 3;
          I.assign A.(lval f) flag 4;
        |] )
end

(* 8.8 Bit Set, Reset and Test Group *)
module BSRT = struct
  let flag_bit b src =
    let f = A.(expr f) in
    let sf = if b = 7 then E.restrict 7 7 src else E.zero in
    let zf = E.lognot (E.restrict b b src) in
    let yf = E.restrict 5 5 f in
    let hf = E.one in
    let xf = E.restrict 3 3 f in
    let pf = zf in
    let nf = E.zero in
    let cf = E.restrict 0 0 f in
    Array.fold_right E.append [| sf; zf; yf; hf; xf; pf; nf |] cf

  let bit_b b str expr =
    F
      ( Format.sprintf "BIT %d, %s" b str,
        [| I.assign A.(lval f) (flag_bit b expr) 1 |] )

  let bit_b_memdisp b r disp =
    bit_b b
      (Format.sprintf "(%s+%d)" (A.name r) disp)
      (E.load b1 M.LittleEndian
         (E.add (A.expr r) (E.constant (B.of_int ~size:16 disp))))

  let bit_b_r b r =
    let r = R8.r r in
    bit_b b (A.name r) (A.expr r)

  let bit_b_HL b = bit_b b A.(mem hl) A.(load1 hl)
  let bit_b_IXd b d = bit_b_memdisp b A.ix d
  let bit_b_IYd b d = bit_b_memdisp b A.iy d

  let setres_b op f b str loc expr =
    F (Format.sprintf "%s %d, %s" op b str, [| I.assign loc (f expr b) 1 |])

  let setres_b_memdisp op f b A.(D { name; expr; _ }) disp =
    let addr = E.add expr (E.constant (B.of_int ~size:16 disp)) in
    setres_b op f b
      (Format.sprintf "(%s+%d)" name disp)
      (L.store b1 M.LittleEndian addr)
      (E.load b1 M.LittleEndian addr)

  let setres_b_memdisp_r op f b A.(D { name; expr; _ }) disp r =
    let r = R8.r r in
    let addr = E.add expr (E.constant (B.of_int ~size:16 disp)) in
    let store1 = L.store b1 M.LittleEndian addr
    and load1 = E.load b1 M.LittleEndian addr in
    F
      ( Format.sprintf "%s %d, (%s+%d), %s" op b name disp (A.name r),
        [| I.assign (A.lval r) (f load1 b) 1; I.assign store1 (A.expr r) 2 |] )

  let set e b = E.logor e (E.constant (B.of_int ~size:8 (1 lsl b)))
  let clear e b = E.logand e (E.constant (B.of_int ~size:8 (lnot (1 lsl b))))
  let set_b b str loc expr = setres_b "SET" set b str loc expr
  let set_b_memdisp b r d = setres_b_memdisp "SET" set b r d
  let set_b_memdisp_r b r d r' = setres_b_memdisp_r "SET" set b r d r'
  let res_b b str loc expr = setres_b "RES" clear b str loc expr
  let res_b_memdisp b r d = setres_b_memdisp "RES" clear b r d
  let res_b_memdisp_r b r d r' = setres_b_memdisp_r "RES" clear b r d r'
  let set_b_reg b A.(S { name; loc; expr }) = set_b b name loc expr
  let set_b_r b r = set_b_reg b (R8.r r)
  let set_b_HL b = set_b b A.(mem hl) A.(store1 hl) A.(load1 hl)
  let set_b_IXd b d = set_b_memdisp b A.ix d
  let set_b_IYd b d = set_b_memdisp b A.iy d
  let set_b_IXd_r b d r = set_b_memdisp_r b A.ix d r
  let set_b_IYd_r b d r = set_b_memdisp_r b A.iy d r
  let res_b_reg b A.(S { name; loc; expr }) = res_b b name loc expr
  let res_b_r b r = res_b_reg b (R8.r r)
  let res_b_HL b = res_b b A.(mem hl) A.(store1 hl) A.(load1 hl)
  let res_b_IXd b d = res_b_memdisp b A.ix d
  let res_b_IYd b d = res_b_memdisp b A.iy d
  let res_b_IXd_r b d r = res_b_memdisp_r b A.ix d r
  let res_b_IYd_r b d r = res_b_memdisp_r b A.iy d r
end

(* 8.9 Jump Group *)
module JMP = struct
  let jp_reg r =
    T
      ( Format.sprintf "JP %s" (A.mem r),
        Dhunk.singleton (I.dynamic_jump (A.expr r)) )

  let jp_cc_nn pc cc nn =
    let cc = Cond.cc cc in
    T
      ( Format.sprintf "JP %s, %04x" (Cond.to_string cc) nn,
        Dhunk.init 2 (function
          | 0 -> I.jump_cc (Cond.expr cc) (Virtual_address.create nn) 1
          | _ -> I.static_relative_jump pc 3) )

  let jp_nn nn =
    T
      ( Format.sprintf "JP %04x" nn,
        Dhunk.singleton (I.static_outer_jump (Virtual_address.create nn)) )

  let jp_HL = jp_reg A.hl
  let jp_IX = jp_reg A.ix
  let jp_IY = jp_reg A.iy

  let jr_e pc e =
    let e = e + 2 in
    T (Format.sprintf "JR %d" e, Dhunk.singleton (I.static_relative_jump pc e))

  let jr_ss_e pc ss e =
    let c = Cond.ss ss and e = e + 2 in
    T
      ( Format.sprintf "JP %s, %d" (Cond.to_string c) e,
        Dhunk.init 2 (function
          | 0 -> I.jump_cc (Cond.expr c) (Virtual_address.add_int e pc) 1
          | _ -> I.static_relative_jump pc 2) )

  let djnz_e pc e =
    let e = e + 2 in
    T
      ( Format.sprintf "DJNZ %d" e,
        Dhunk.init 3 (function
          | 0 -> I.assign A.(lval b) (E.sub A.(expr b) one8) 1
          | 1 ->
              I.jump_cc
                (E.diff A.(expr b) zero8)
                (Virtual_address.add_int e pc)
                2
          | _ -> I.static_relative_jump pc 2) )
end

(* 8.10 Call and Return Group *)
module RTN = struct
  let call pc nn =
    let next = Virtual_address.add_int 3 pc in
    let succ = B.create (Virtual_address.to_bigint next) 16 in
    T
      ( Format.sprintf "CALL %04x" nn,
        Dhunk.init 3 (function
          | 0 -> I.assign A.(lval sp) (E.sub A.(expr sp) two16) 1
          | 1 -> I.assign A.(store2 sp) (E.constant succ) 2
          | _ -> I.static_call next (Virtual_address.create nn)) )

  let call_cc_nn pc cc nn =
    let next = Virtual_address.add_int 3 pc in
    let succ = B.create (Virtual_address.to_bigint next) 16 in
    let c = Cond.cc cc in
    T
      ( Format.sprintf "CALL %s, %04x" (Cond.to_string c) nn,
        Dhunk.init 4 (function
          | 0 -> I.jump_cc (E.lognot (Cond.expr c)) next 1
          | 1 -> I.assign A.(lval sp) (E.sub A.(expr sp) two16) 2
          | 2 -> I.assign A.(store2 sp) (E.constant succ) 3
          | _ -> I.static_call next (Virtual_address.create nn)) )

  let ret =
    T
      ( "RET",
        Dhunk.init 3 (function
          | 0 -> I.assign lt16 A.(load2 sp) 1
          | 1 -> I.assign A.(lval sp) (E.add A.(expr sp) two16) 2
          | _ -> I.dynamic_jump ~tag:Return et16) )

  let ret_cc pc cc =
    let c = Cond.cc cc in
    T
      ( Format.sprintf "RET %s" (Cond.to_string c),
        Dhunk.init 5 (function
          | 0 ->
              I.jump_cc
                (E.lognot (Cond.expr c))
                (Virtual_address.add_int 1 pc)
                1
          | 1 -> I.assign lt16 A.(load2 sp) 2
          | 2 -> I.assign A.(lval sp) (E.add A.(expr sp) two16) 3
          | _ -> I.dynamic_jump ~tag:Return et16) )

  let reti = U "RETI"
  let retn = U "RETN"

  let rst_p pc p =
    let p = p lsl 3 in
    let next = Virtual_address.add_int 1 pc in
    let succ = B.create (Virtual_address.to_bigint next) 16 in
    T
      ( Format.sprintf "RST %02x" p,
        Dhunk.init 3 (function
          | 0 -> I.assign A.(lval sp) (E.sub A.(expr sp) two16) 1
          | 1 -> I.assign A.(store2 sp) (E.constant succ) 2
          | _ -> I.static_call next (Virtual_address.create p)) )
end

(* 8.11 Input and Output Group *)
module IO = struct
  let in_a_N n = U (Format.sprintf "IN A, (%02x)" n)
  let in_r_C r = U (Format.sprintf "IN %s, (C)" (A.name (R8.r r)))
  let in_f_C = U "IN (C)"
  let ini = U "INI"
  let inir = U "INIR"
  let ind = U "IND"
  let indr = U "INDR"
  let out_N_a n = U (Format.sprintf "OUT (%02x), A" n)
  let out_C_r r = U (Format.sprintf "OUT (C), %s" (A.name (R8.r r)))
  let out_C_0 = U "OUT (C), 0"
  let outi = U "OUTI"
  let otir = U "OTIR"
  let outd = U "OUTD"
  let otdr = U "OTDR"
end

let decode_cb reader =
  let op = Lreader.Read.u8 reader in
  match Char.unsafe_chr op with
  | '\x00' .. '\x05'
  | '\x07' .. '\x0d'
  | '\x0f' .. '\x15'
  | '\x17' .. '\x1d'
  | '\x1f' .. '\x25'
  | '\x27' .. '\x2d'
  | '\x2f' .. '\x35'
  | '\x37' .. '\x3d'
  | '\x3f' ->
      RS.op_r (op lsr 3) (op land 0b111)
  | '\x06' | '\x0e' | '\x16' | '\x1e' | '\x26' | '\x2e' | '\x36' | '\x3e' ->
      RS.op_HL (op lsr 3)
  | '\x40' .. '\x45'
  | '\x47' .. '\x4d'
  | '\x4f' .. '\x55'
  | '\x57' .. '\x5d'
  | '\x5f' .. '\x65'
  | '\x67' .. '\x6d'
  | '\x6f' .. '\x75'
  | '\x77' .. '\x7d'
  | '\x7f' ->
      BSRT.bit_b_r ((op lsr 3) land 0b111) (op land 0b111)
  | '\x46' | '\x4e' | '\x56' | '\x5e' | '\x66' | '\x6e' | '\x76' | '\x7e' ->
      BSRT.bit_b_HL ((op lsr 3) land 0b111)
  | '\x80' .. '\x85'
  | '\x87' .. '\x8d'
  | '\x8f' .. '\x95'
  | '\x97' .. '\x9d'
  | '\x9f' .. '\xa5'
  | '\xa7' .. '\xad'
  | '\xaf' .. '\xb5'
  | '\xb7' .. '\xbd'
  | '\xbf' ->
      BSRT.res_b_r ((op lsr 3) land 0b111) (op land 0b111)
  | '\x86' | '\x8e' | '\x96' | '\x9e' | '\xa6' | '\xae' | '\xb6' | '\xbe' ->
      BSRT.res_b_HL ((op lsr 3) land 0b111)
  | '\xc0' .. '\xc5'
  | '\xc7' .. '\xcd'
  | '\xcf' .. '\xd5'
  | '\xd7' .. '\xdd'
  | '\xdf' .. '\xe5'
  | '\xe7' .. '\xed'
  | '\xef' .. '\xf5'
  | '\xf7' .. '\xfd'
  | '\xff' ->
      BSRT.set_b_r ((op lsr 3) land 0b111) (op land 0b111)
  | '\xc6' | '\xce' | '\xd6' | '\xde' | '\xe6' | '\xee' | '\xf6' | '\xfe' ->
      BSRT.set_b_HL ((op lsr 3) land 0b111)

let decode_ddcb reader =
  let d = Lreader.Read.i8 reader in
  let op = Lreader.Read.u8 reader in
  match Char.unsafe_chr op with
  | '\x00' .. '\x05'
  | '\x07' .. '\x0d'
  | '\x0f' .. '\x15'
  | '\x17' .. '\x1d'
  | '\x1f' .. '\x25'
  | '\x27' .. '\x2d'
  | '\x2f' .. '\x35'
  | '\x37' .. '\x3d'
  | '\x3f' ->
      RS.op_IXd_r ((op lsr 3) land 0b111) d (op land 0b111)
  | '\x06' | '\x0e' | '\x16' | '\x1e' | '\x26' | '\x2e' | '\x36' | '\x3e' ->
      RS.op_IXd (op lsr 3) d
  | '\x40' .. '\x7f' -> BSRT.bit_b_IXd ((op lsr 3) land 0b111) d
  | '\x80' .. '\x85'
  | '\x87' .. '\x8d'
  | '\x8f' .. '\x95'
  | '\x97' .. '\x9d'
  | '\x9f' .. '\xa5'
  | '\xa7' .. '\xad'
  | '\xaf' .. '\xb5'
  | '\xb7' .. '\xbd'
  | '\xbf' ->
      BSRT.res_b_IXd_r ((op lsr 3) land 0b111) d (op land 0b111)
  | '\x86' | '\x8e' | '\x96' | '\x9e' | '\xa6' | '\xae' | '\xb6' | '\xbe' ->
      BSRT.res_b_IXd ((op lsr 3) land 0b111) d
  | '\xc0' .. '\xc5'
  | '\xc7' .. '\xcd'
  | '\xcf' .. '\xd5'
  | '\xd7' .. '\xdd'
  | '\xdf' .. '\xe5'
  | '\xe7' .. '\xed'
  | '\xef' .. '\xf5'
  | '\xf7' .. '\xfd'
  | '\xff' ->
      BSRT.set_b_IXd_r ((op lsr 3) land 0b111) d (op land 0b111)
  | '\xc6' | '\xce' | '\xd6' | '\xde' | '\xe6' | '\xee' | '\xf6' | '\xfe' ->
      BSRT.set_b_IXd ((op lsr 3) land 0b111) d

let decode_dd reader =
  let op = Lreader.Read.u8 reader in
  match Char.unsafe_chr op with
  | '\x24' | '\x2c' -> Arith8.inc_p (op lsr 3)
  | '\x25' | '\x2d' -> Arith8.dec_p (op lsr 3)
  | '\x26' | '\x2e' ->
      LD8.ld_p_n ((op lsr 3) land 0b111) (Lreader.Read.u8 reader)
  | '\x09' | '\x19' | '\x29' | '\x39' -> Arith16.add_ix_pp (op lsr 4)
  | '\x21' -> LD16.ld_ix_nn (Lreader.Read.u16 reader)
  | '\x22' -> LD16.ld_NN_ix (Lreader.Read.u16 reader)
  | '\x23' -> Arith16.inc_ix
  | '\x2a' -> LD16.ld_ix_NN (Lreader.Read.u16 reader)
  | '\x2b' -> Arith16.dec_ix
  | '\x34' -> Arith8.inc_IXd (Lreader.Read.i8 reader)
  | '\x35' -> Arith8.dec_IXd (Lreader.Read.i8 reader)
  | '\x36' ->
      let d = Lreader.Read.u8 reader in
      LD8.ld_IXd_n d (Lreader.Read.i8 reader)
  | '\x44' | '\x45' | '\x4c' | '\x4d' | '\x54' | '\x55' | '\x5c' | '\x5d'
  | '\x60' .. '\x65'
  | '\x67' .. '\x6d'
  | '\x6f' | '\x7c' | '\x7d' ->
      LD8.ld_p_p' ((op lsr 3) land 0b111) (op land 0b111)
  | '\x46' | '\x4e' | '\x56' | '\x5e' | '\x66' | '\x6e' | '\x7e' ->
      LD8.ld_r_IXd ((op lsr 3) land 0b111) (Lreader.Read.i8 reader)
  | '\x70' .. '\x75' | '\x77' ->
      LD8.ld_IXd_r (Lreader.Read.i8 reader) (op land 0b111)
  | '\x84' | '\x85' | '\x8c' | '\x8d' | '\x94' | '\x95' | '\x9c' | '\x9d'
  | '\xa4' | '\xa5' | '\xac' | '\xad' | '\xb4' | '\xb5' ->
      Arith8.op_a_p ((op lsr 3) land 0b111) (op land 0b111)
  | '\xbc' | '\xbd' -> Arith8.cp_a_p (op land 0b111)
  | '\x86' | '\x8e' | '\x96' | '\x9e' | '\xa6' | '\xae' | '\xb6' ->
      Arith8.op_a_IXd ((op lsr 3) land 0b111) (Lreader.Read.i8 reader)
  | '\xbe' -> Arith8.cp_a_IXd (Lreader.Read.i8 reader)
  | '\xcb' -> decode_ddcb reader
  | '\xe1' -> LD16.pop_ix
  | '\xe3' -> EBTS.ex_SP_IX
  | '\xe5' -> LD16.push_ix
  | '\xe9' -> JMP.jp_IX
  | '\xf9' -> LD16.ld_sp_ix
  | _ -> I

let decode_ed pc reader =
  let op = Lreader.Read.u8 reader in
  match Char.unsafe_chr op with
  | '\x40' | '\x48' | '\x50' | '\x58' | '\x60' | '\x68' | '\x78' ->
      IO.in_r_C ((op lsr 3) land 0b111)
  | '\x41' | '\x49' | '\x51' | '\x59' | '\x61' | '\x69' | '\x79' ->
      IO.out_C_r ((op lsr 3) land 0b111)
  | '\x42' | '\x52' | '\x62' | '\x72' -> Arith16.sbc_hl_ss ((op lsr 4) land 0b11)
  | '\x43' | '\x53' | '\x63' | '\x73' ->
      LD16.ld_NN_dd (Lreader.Read.u16 reader) ((op lsr 4) land 0b11)
  | '\x44' | '\x4c' | '\x54' | '\x5c' | '\x64' | '\x6c' | '\x74' | '\x7c' ->
      GPAC.neg
  | '\x45' | '\x55' | '\x5d' | '\x65' | '\x6d' | '\x75' | '\x7d' -> RTN.retn
  | '\x46' | '\x4e' | '\x66' | '\x6e' -> GPAC.im0
  | '\x47' -> LD8.ld_i_a
  | '\x4a' | '\x5a' | '\x6a' | '\x7a' -> Arith16.adc_hl_ss ((op lsr 4) land 0b11)
  | '\x4b' | '\x5b' | '\x6b' | '\x7b' ->
      LD16.ld_dd_NN ((op lsr 4) land 0b11) (Lreader.Read.u16 reader)
  | '\x4d' -> RTN.reti
  | '\x4f' -> LD8.ld_r_a
  | '\x56' | '\x76' -> GPAC.im1
  | '\x57' -> LD8.ld_a_i
  | '\x5e' | '\x7e' -> GPAC.im2
  | '\x5f' -> LD8.ld_a_r
  | '\x67' -> RS.rrd
  | '\x6f' -> RS.rld
  | '\x70' -> IO.in_f_C
  | '\x71' -> IO.out_C_0
  | '\xa0' -> EBTS.ldi
  | '\xa1' -> EBTS.cpi
  | '\xa2' -> IO.ini
  | '\xa3' -> IO.outi
  | '\xa8' -> EBTS.ldd
  | '\xa9' -> EBTS.cpd
  | '\xaa' -> IO.ind
  | '\xab' -> IO.outd
  | '\xb0' -> EBTS.ldir pc
  | '\xb1' -> EBTS.cpir pc
  | '\xb2' -> IO.inir
  | '\xb3' -> IO.otir
  | '\xb8' -> EBTS.lddr pc
  | '\xb9' -> EBTS.cpdr pc
  | '\xba' -> IO.indr
  | '\xbb' -> IO.otdr
  | _ -> I

let decode_fdcb reader =
  let d = Lreader.Read.i8 reader in
  let op = Lreader.Read.u8 reader in
  match Char.unsafe_chr op with
  | '\x00' .. '\x05'
  | '\x07' .. '\x0d'
  | '\x0f' .. '\x15'
  | '\x17' .. '\x1d'
  | '\x1f' .. '\x25'
  | '\x27' .. '\x2d'
  | '\x2f' .. '\x35'
  | '\x37' .. '\x3d'
  | '\x3f' ->
      RS.op_IYd_r ((op lsr 3) land 0b111) d (op land 0b111)
  | '\x06' | '\x0e' | '\x16' | '\x1e' | '\x26' | '\x2e' | '\x36' | '\x3e' ->
      RS.op_IYd (op lsr 3) d
  | '\x40' .. '\x7f' -> BSRT.bit_b_IYd ((op lsr 3) land 0b111) d
  | '\x80' .. '\x85'
  | '\x87' .. '\x8d'
  | '\x8f' .. '\x95'
  | '\x97' .. '\x9d'
  | '\x9f' .. '\xa5'
  | '\xa7' .. '\xad'
  | '\xaf' .. '\xb5'
  | '\xb7' .. '\xbd'
  | '\xbf' ->
      BSRT.res_b_IYd_r ((op lsr 3) land 0b111) d (op land 0b111)
  | '\x86' | '\x8e' | '\x96' | '\x9e' | '\xa6' | '\xae' | '\xb6' | '\xbe' ->
      BSRT.res_b_IYd ((op lsr 3) land 0b111) d
  | '\xc0' .. '\xc5'
  | '\xc7' .. '\xcd'
  | '\xcf' .. '\xd5'
  | '\xd7' .. '\xdd'
  | '\xdf' .. '\xe5'
  | '\xe7' .. '\xed'
  | '\xef' .. '\xf5'
  | '\xf7' .. '\xfd'
  | '\xff' ->
      BSRT.set_b_IYd_r ((op lsr 3) land 0b111) d (op land 0b111)
  | '\xc6' | '\xce' | '\xd6' | '\xde' | '\xe6' | '\xee' | '\xf6' | '\xfe' ->
      BSRT.set_b_IYd ((op lsr 3) land 0b111) d

let decode_fd reader =
  let op = Lreader.Read.u8 reader in
  match Char.unsafe_chr op with
  | '\x24' | '\x2c' -> Arith8.inc_q (op lsr 3)
  | '\x25' | '\x2d' -> Arith8.dec_q (op lsr 3)
  | '\x26' | '\x2e' ->
      LD8.ld_q_n ((op lsr 3) land 0b111) (Lreader.Read.u8 reader)
  | '\x09' | '\x19' | '\x29' | '\x39' -> Arith16.add_iy_qq (op lsr 4)
  | '\x21' -> LD16.ld_iy_nn (Lreader.Read.u16 reader)
  | '\x22' -> LD16.ld_NN_iy (Lreader.Read.u16 reader)
  | '\x23' -> Arith16.inc_iy
  | '\x2a' -> LD16.ld_iy_NN (Lreader.Read.u16 reader)
  | '\x2b' -> Arith16.dec_iy
  | '\x34' -> Arith8.inc_IYd (Lreader.Read.i8 reader)
  | '\x35' -> Arith8.dec_IYd (Lreader.Read.i8 reader)
  | '\x36' ->
      let d = Lreader.Read.u8 reader in
      LD8.ld_IYd_n d (Lreader.Read.i8 reader)
  | '\x44' | '\x45' | '\x4c' | '\x4d' | '\x54' | '\x55' | '\x5c' | '\x5d'
  | '\x60' .. '\x65'
  | '\x67' .. '\x6d'
  | '\x6f' | '\x7c' | '\x7d' ->
      LD8.ld_q_q' ((op lsr 3) land 0b111) (op land 0b111)
  | '\x46' | '\x4e' | '\x56' | '\x5e' | '\x66' | '\x6e' | '\x7e' ->
      LD8.ld_r_IYd ((op lsr 3) land 0b111) (Lreader.Read.i8 reader)
  | '\x70' .. '\x75' | '\x77' ->
      LD8.ld_IYd_r (Lreader.Read.i8 reader) (op land 0b111)
  | '\x84' | '\x85' | '\x8c' | '\x8d' | '\x94' | '\x95' | '\x9c' | '\x9d'
  | '\xa4' | '\xa5' | '\xac' | '\xad' | '\xb4' | '\xb5' ->
      Arith8.op_a_q ((op lsr 3) land 0b111) (op land 0b111)
  | '\xbc' | '\xbd' -> Arith8.cp_a_q (op land 0b111)
  | '\x86' | '\x8e' | '\x96' | '\x9e' | '\xa6' | '\xae' | '\xb6' ->
      Arith8.op_a_IYd ((op lsr 3) land 0b111) (Lreader.Read.i8 reader)
  | '\xbe' -> Arith8.cp_a_IYd (Lreader.Read.i8 reader)
  | '\xcb' -> decode_fdcb reader
  | '\xe1' -> LD16.pop_iy
  | '\xe3' -> EBTS.ex_SP_IY
  | '\xe5' -> LD16.push_iy
  | '\xe9' -> JMP.jp_IY
  | '\xf9' -> LD16.ld_sp_iy
  | _ -> I

let decode pc reader =
  let op = Lreader.Read.u8 reader in
  match Char.unsafe_chr op with
  | '\x00' -> GPAC.nop
  | '\x01' | '\x11' | '\x21' | '\x31' ->
      LD16.ld_dd_nn (op lsr 4) (Lreader.Read.u16 reader)
  | '\x02' -> LD8.ld_BC_a
  | '\x03' | '\x13' | '\x23' | '\x33' -> Arith16.inc_ss (op lsr 4)
  | '\x04' | '\x0c' | '\x14' | '\x1c' | '\x24' | '\x2c' | '\x3c' ->
      Arith8.inc_r (op lsr 3)
  | '\x05' | '\x0d' | '\x15' | '\x1d' | '\x25' | '\x2d' | '\x3d' ->
      Arith8.dec_r (op lsr 3)
  | '\x06' | '\x0e' | '\x16' | '\x1e' | '\x26' | '\x2e' | '\x3e' ->
      LD8.ld_r_n (op lsr 3) (Lreader.Read.u8 reader)
  | '\x07' -> RS.rlca
  | '\x08' -> EBTS.ex_af_af'
  | '\x09' | '\x19' | '\x29' | '\x39' -> Arith16.add_hl_ss (op lsr 4)
  | '\x0a' -> LD8.ld_a_BC
  | '\x0b' | '\x1b' | '\x2b' | '\x3b' -> Arith16.dec_ss (op lsr 4)
  | '\x0f' -> RS.rrca
  | '\x10' -> JMP.djnz_e pc (Lreader.Read.i8 reader)
  | '\x12' -> LD8.ld_DE_a
  | '\x17' -> RS.rla
  | '\x18' -> JMP.jr_e pc (Lreader.Read.i8 reader)
  | '\x1a' -> LD8.ld_a_DE
  | '\x1f' -> RS.rra
  | '\x20' | '\x28' | '\x30' | '\x38' ->
      JMP.jr_ss_e pc ((op lsr 3) land 0b11) (Lreader.Read.i8 reader)
  | '\x22' -> LD16.ld_NN_hl (Lreader.Read.u16 reader)
  | '\x27' -> GPAC.daa
  | '\x2a' -> LD16.ld_hl_NN (Lreader.Read.u16 reader)
  | '\x2f' -> GPAC.cpl
  | '\x32' -> LD8.ld_NN_a (Lreader.Read.u16 reader)
  | '\x34' -> Arith8.inc_HL
  | '\x35' -> Arith8.dec_HL
  | '\x36' -> LD8.ld_HL_n (Lreader.Read.u8 reader)
  | '\x37' -> GPAC.scf
  | '\x3a' -> LD8.ld_a_NN (Lreader.Read.u16 reader)
  | '\x3f' -> GPAC.ccf
  | '\x40' .. '\x45'
  | '\x47' .. '\x4d'
  | '\x4f' .. '\x55'
  | '\x57' .. '\x5d'
  | '\x5f' .. '\x65'
  | '\x67' .. '\x6d'
  | '\x6f'
  | '\x78' .. '\x7d'
  | '\x7f' ->
      LD8.ld_r_r' ((op lsr 3) land 0b111) (op land 0b111)
  | '\x46' | '\x4e' | '\x56' | '\x5e' | '\x66' | '\x6e' | '\x7e' ->
      LD8.ld_r_HL ((op lsr 3) land 0b111)
  | '\x70' .. '\x75' | '\x77' -> LD8.ld_HL_r (op land 0b111)
  | '\x76' -> GPAC.halt
  | '\x80' .. '\x85'
  | '\x87' .. '\x8d'
  | '\x8f' .. '\x95'
  | '\x97' .. '\x9d'
  | '\x9f' .. '\xa5'
  | '\xa7' .. '\xad'
  | '\xaf' .. '\xb5'
  | '\xb7' ->
      Arith8.op_a_r ((op lsr 3) land 0b111) (op land 0b111)
  | '\x86' | '\x8e' | '\x96' | '\x9e' | '\xa6' | '\xae' | '\xb6' ->
      Arith8.op_a_HL ((op lsr 3) land 0b111)
  | '\xb8' .. '\xbd' | '\xbf' -> Arith8.cp_a_r (op land 0b111)
  | '\xbe' -> Arith8.cp_a_HL
  | '\xc0' | '\xc8' | '\xd0' | '\xd8' | '\xe0' | '\xe8' | '\xf0' | '\xf8' ->
      RTN.ret_cc pc ((op lsr 3) land 0b111)
  | '\xc1' | '\xd1' | '\xe1' -> LD16.pop_qq ((op lsr 4) land 0b11)
  | '\xf1' -> LD16.pop_af
  | '\xc2' | '\xca' | '\xd2' | '\xda' | '\xe2' | '\xea' | '\xf2' | '\xfa' ->
      JMP.jp_cc_nn pc ((op lsr 3) land 0b111) (Lreader.Read.u16 reader)
  | '\xc3' -> JMP.jp_nn (Lreader.Read.u16 reader)
  | '\xc4' | '\xcc' | '\xd4' | '\xdc' | '\xe4' | '\xec' | '\xf4' | '\xfc' ->
      RTN.call_cc_nn pc ((op lsr 3) land 0b111) (Lreader.Read.u16 reader)
  | '\xc5' | '\xd5' | '\xe5' -> LD16.push_qq ((op lsr 4) land 0b11)
  | '\xf5' -> LD16.push_af
  | '\xc6' | '\xce' | '\xd6' | '\xde' | '\xe6' | '\xee' | '\xf6' ->
      Arith8.op_a_n ((op lsr 3) land 0b111) (Lreader.Read.u8 reader)
  | '\xfe' -> Arith8.cp_a_n (Lreader.Read.u8 reader)
  | '\xc7' | '\xcf' | '\xd7' | '\xdf' | '\xe7' | '\xef' | '\xf7' | '\xff' ->
      RTN.rst_p pc ((op lsr 3) land 0b111)
  | '\xc9' -> RTN.ret
  | '\xcb' -> decode_cb reader
  | '\xcd' -> RTN.call pc (Lreader.Read.u16 reader)
  | '\xd3' -> IO.out_N_a (Lreader.Read.u8 reader)
  | '\xd9' -> EBTS.exx
  | '\xdb' -> IO.in_a_N (Lreader.Read.u8 reader)
  | '\xdd' -> decode_dd reader
  | '\xe3' -> EBTS.ex_SP_hl
  | '\xe9' -> JMP.jp_HL
  | '\xeb' -> EBTS.ex_de_hl
  | '\xed' -> decode_ed pc reader
  | '\xf3' -> GPAC.di
  | '\xf9' -> LD16.ld_sp_hl
  | '\xfb' -> GPAC.ei
  | '\xfd' -> decode_fd reader

let decode reader vaddr =
  let lo = Lreader.get_pos reader in
  let disasm = decode vaddr reader in
  let hi = Lreader.get_pos reader - 1 in
  let size = hi - lo + 1 in
  let mnemonic, dhunk =
    match disasm with
    | F (mnemonic, instrs) ->
        let len = Array.length instrs in
        ( Mnemonic.supported mnemonic Format.pp_print_string,
          Dhunk.init (len + 1) (fun i ->
              if i = len then I.static_relative_jump vaddr size
              else Array.get instrs i) )
    | T (mnemonic, dhunk) ->
        (Mnemonic.supported mnemonic Format.pp_print_string, dhunk)
    | U mnemonic_hint ->
        ( Mnemonic.unsupported ~mnemonic_hint (),
          Dhunk.singleton (I.stop (Some (Dba.Unsupported mnemonic_hint))) )
    | I -> (Mnemonic.unknown, Dhunk.singleton (I._assert E.zero 0))
  in
  let hi = Lreader.get_pos reader - 1 in
  let size = hi - lo + 1 in
  let opcode =
    String_utils.to_hex
      (Bytes.unsafe_to_string (Lreader.get_slice reader ~hi ~lo))
  in
  let ginst = Instruction.Generic.create size opcode mnemonic in
  (ginst, dhunk)
