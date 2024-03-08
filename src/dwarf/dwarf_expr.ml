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

let map = Isa_helper.get_dwarf_register

open Loader_buf

module Operator = struct
  type t =
    | Addr of int
    | Deref
    | Const of int
    | Dup
    | Drop
    | Over
    | Pick of int
    | Swap
    | Rot
    | Xderef
    | Abs
    | And
    | Div
    | Minus
    | Mod
    | Mul
    | Neg
    | Not
    | Or
    | Plus
    | Plus_uconst of int
    | Shl
    | Shr
    | Shra
    | Xor
    | Bra of int
    | Eq
    | Ge
    | Gt
    | Le
    | Lt
    | Ne
    | Skip of int
    | Regx of int
    | Fbreg of int
    | Bregx of int * int
    | Piece of int
    | Deref_size of int
    | Xderef_size of int
    | Nop
    | Push_object_address
    | Call2 of int
    | Call4 of int
    | Call_ref of int
    | Form_tls_address
    | Call_frame_cfa
    | Bit_piece of int * int
    | Implicit_value of int * string
    | Stack_value
    | Implicit_pointer of int * int
    | Entry_value of int * string
    | Const_type of int * int * string
    | Regval_type of int * int
    | Deref_type of int * int
    | Xderef_type of int * int
    | Convert of int
    | Reinterpret of int

  let pp ppf = function
    | Addr a -> Format.fprintf ppf "DW_OP_addr (%d)" a
    | Deref -> Format.fprintf ppf "DW_OP_deref"
    | Const c -> Format.fprintf ppf "DW_OP_const (%d)" c
    | Dup -> Format.fprintf ppf "DW_OP_dup"
    | Drop -> Format.fprintf ppf "DW_OP_drop"
    | Over -> Format.fprintf ppf "DW_OP_over"
    | Pick o -> Format.fprintf ppf "DW_OP_pick (%d)" o
    | Swap -> Format.fprintf ppf "DW_OP_swap"
    | Rot -> Format.fprintf ppf "DW_OP_rot"
    | Xderef -> Format.fprintf ppf "DW_OP_xderef"
    | Abs -> Format.fprintf ppf "DW_OP_abs"
    | And -> Format.fprintf ppf "DW_OP_and"
    | Div -> Format.fprintf ppf "DW_OP_div"
    | Minus -> Format.fprintf ppf "DW_OP_minus"
    | Mod -> Format.fprintf ppf "DW_OP_mod"
    | Mul -> Format.fprintf ppf "DW_OP_mul"
    | Neg -> Format.fprintf ppf "DW_OP_neg"
    | Not -> Format.fprintf ppf "DW_OP_not"
    | Or -> Format.fprintf ppf "DW_OP_or"
    | Plus -> Format.fprintf ppf "DW_OP_plus"
    | Plus_uconst a -> Format.fprintf ppf "DW_OP_plus_uconst (%d)" a
    | Shl -> Format.fprintf ppf "DW_OP_shl"
    | Shr -> Format.fprintf ppf "DW_OP_shr"
    | Shra -> Format.fprintf ppf "DW_OP_shra"
    | Xor -> Format.fprintf ppf "DW_OP_xor"
    | Bra o -> Format.fprintf ppf "DW_OP_bra (%d)" o
    | Eq -> Format.fprintf ppf "DW_OP_eq"
    | Ge -> Format.fprintf ppf "DW_OP_ge"
    | Gt -> Format.fprintf ppf "DW_OP_gt"
    | Le -> Format.fprintf ppf "DW_OP_le"
    | Lt -> Format.fprintf ppf "DW_OP_lt"
    | Ne -> Format.fprintf ppf "DW_OP_ne"
    | Skip o -> Format.fprintf ppf "DW_OP_skip (%d)" o
    | Regx 0 -> Format.fprintf ppf "DW_OP_reg0"
    | Regx 1 -> Format.fprintf ppf "DW_OP_reg1"
    | Regx 2 -> Format.fprintf ppf "DW_OP_reg2"
    | Regx 3 -> Format.fprintf ppf "DW_OP_reg3"
    | Regx 4 -> Format.fprintf ppf "DW_OP_reg4"
    | Regx 5 -> Format.fprintf ppf "DW_OP_reg5"
    | Regx 6 -> Format.fprintf ppf "DW_OP_reg6"
    | Regx 7 -> Format.fprintf ppf "DW_OP_reg7"
    | Regx 8 -> Format.fprintf ppf "DW_OP_reg8"
    | Regx 9 -> Format.fprintf ppf "DW_OP_reg9"
    | Regx 10 -> Format.fprintf ppf "DW_OP_reg10"
    | Regx 11 -> Format.fprintf ppf "DW_OP_reg11"
    | Regx 12 -> Format.fprintf ppf "DW_OP_reg12"
    | Regx 13 -> Format.fprintf ppf "DW_OP_reg13"
    | Regx 14 -> Format.fprintf ppf "DW_OP_reg14"
    | Regx 15 -> Format.fprintf ppf "DW_OP_reg15"
    | Regx 16 -> Format.fprintf ppf "DW_OP_reg16"
    | Regx 17 -> Format.fprintf ppf "DW_OP_reg17"
    | Regx 18 -> Format.fprintf ppf "DW_OP_reg18"
    | Regx 19 -> Format.fprintf ppf "DW_OP_reg19"
    | Regx 20 -> Format.fprintf ppf "DW_OP_reg20"
    | Regx 21 -> Format.fprintf ppf "DW_OP_reg21"
    | Regx 22 -> Format.fprintf ppf "DW_OP_reg22"
    | Regx 23 -> Format.fprintf ppf "DW_OP_reg23"
    | Regx 24 -> Format.fprintf ppf "DW_OP_reg24"
    | Regx 25 -> Format.fprintf ppf "DW_OP_reg25"
    | Regx 26 -> Format.fprintf ppf "DW_OP_reg26"
    | Regx 27 -> Format.fprintf ppf "DW_OP_reg27"
    | Regx 28 -> Format.fprintf ppf "DW_OP_reg28"
    | Regx 29 -> Format.fprintf ppf "DW_OP_reg29"
    | Regx 30 -> Format.fprintf ppf "DW_OP_reg30"
    | Regx 31 -> Format.fprintf ppf "DW_OP_reg31"
    | Bregx (0, a) -> Format.fprintf ppf "DW_OP_breg0 (%d)" a
    | Bregx (1, a) -> Format.fprintf ppf "DW_OP_breg1 (%d)" a
    | Bregx (2, a) -> Format.fprintf ppf "DW_OP_breg2 (%d)" a
    | Bregx (3, a) -> Format.fprintf ppf "DW_OP_breg3 (%d)" a
    | Bregx (4, a) -> Format.fprintf ppf "DW_OP_breg4 (%d)" a
    | Bregx (5, a) -> Format.fprintf ppf "DW_OP_breg5 (%d)" a
    | Bregx (6, a) -> Format.fprintf ppf "DW_OP_breg6 (%d)" a
    | Bregx (7, a) -> Format.fprintf ppf "DW_OP_breg7 (%d)" a
    | Bregx (8, a) -> Format.fprintf ppf "DW_OP_breg8 (%d)" a
    | Bregx (9, a) -> Format.fprintf ppf "DW_OP_breg9 (%d)" a
    | Bregx (10, a) -> Format.fprintf ppf "DW_OP_breg10 (%d)" a
    | Bregx (11, a) -> Format.fprintf ppf "DW_OP_breg11 (%d)" a
    | Bregx (12, a) -> Format.fprintf ppf "DW_OP_breg12 (%d)" a
    | Bregx (13, a) -> Format.fprintf ppf "DW_OP_breg13 (%d)" a
    | Bregx (14, a) -> Format.fprintf ppf "DW_OP_breg14 (%d)" a
    | Bregx (15, a) -> Format.fprintf ppf "DW_OP_breg15 (%d)" a
    | Bregx (16, a) -> Format.fprintf ppf "DW_OP_breg16 (%d)" a
    | Bregx (17, a) -> Format.fprintf ppf "DW_OP_breg17 (%d)" a
    | Bregx (18, a) -> Format.fprintf ppf "DW_OP_breg18 (%d)" a
    | Bregx (19, a) -> Format.fprintf ppf "DW_OP_breg19 (%d)" a
    | Bregx (20, a) -> Format.fprintf ppf "DW_OP_breg20 (%d)" a
    | Bregx (21, a) -> Format.fprintf ppf "DW_OP_breg21 (%d)" a
    | Bregx (22, a) -> Format.fprintf ppf "DW_OP_breg22 (%d)" a
    | Bregx (23, a) -> Format.fprintf ppf "DW_OP_breg23 (%d)" a
    | Bregx (24, a) -> Format.fprintf ppf "DW_OP_breg24 (%d)" a
    | Bregx (25, a) -> Format.fprintf ppf "DW_OP_breg25 (%d)" a
    | Bregx (26, a) -> Format.fprintf ppf "DW_OP_breg26 (%d)" a
    | Bregx (27, a) -> Format.fprintf ppf "DW_OP_breg27 (%d)" a
    | Bregx (28, a) -> Format.fprintf ppf "DW_OP_breg28 (%d)" a
    | Bregx (29, a) -> Format.fprintf ppf "DW_OP_breg29 (%d)" a
    | Bregx (30, a) -> Format.fprintf ppf "DW_OP_breg30 (%d)" a
    | Bregx (31, a) -> Format.fprintf ppf "DW_OP_breg31 (%d)" a
    | Regx n -> Format.fprintf ppf "DW_OP_regx (%d)" n
    | Fbreg a -> Format.fprintf ppf "DW_OP_fbreg (%d)" a
    | Bregx (r, a) -> Format.fprintf ppf "DW_OP_bregx (%d:%d)" r a
    | Piece _ -> Format.fprintf ppf "DW_OP_piece"
    | Deref_size _ -> Format.fprintf ppf "DW_OP_deref_size"
    | Xderef_size _ -> Format.fprintf ppf "DW_OP_xderef_size"
    | Nop -> Format.fprintf ppf "DW_OP_nop"
    | Push_object_address -> Format.fprintf ppf "DW_OP_push_object_address"
    | Call2 _ -> Format.fprintf ppf "DW_OP_call2"
    | Call4 _ -> Format.fprintf ppf "DW_OP_call4"
    | Call_ref _ -> Format.fprintf ppf "DW_OP_call_ref"
    | Form_tls_address -> Format.fprintf ppf "DW_OP_form_tls_address"
    | Call_frame_cfa -> Format.fprintf ppf "DW_OP_call_frame_cfa"
    | Bit_piece _ -> Format.fprintf ppf "DW_OP_bit_piece"
    | Implicit_value _ -> Format.fprintf ppf "DW_OP_implicit_value"
    | Stack_value -> Format.fprintf ppf "DW_OP_stack_value"
    | Implicit_pointer _ -> Format.fprintf ppf "DW_OP_implicit_pointer"
    | Entry_value _ -> Format.fprintf ppf "DW_OP_entry_value"
    | Const_type _ -> Format.fprintf ppf "DW_OP_const_type"
    | Regval_type _ -> Format.fprintf ppf "DW_OP_regval_type"
    | Deref_type _ -> Format.fprintf ppf "DW_OP_deref_type"
    | Xderef_type _ -> Format.fprintf ppf "DW_OP_xderef_type"
    | Convert _ -> Format.fprintf ppf "DW_OP_convert"
    | Reinterpret _ -> Format.fprintf ppf "DW_OP_reinterpret"

  let load format cursor : t =
    match Read.u8 cursor with
    | 0x03 -> Addr (Dwarf_utils.read_addr cursor)
    | 0x06 -> Deref
    | 0x08 -> Const (Read.u8 cursor)
    | 0x09 -> Const (Read.s8 cursor)
    | 0x0a -> Const (Read.u16 cursor)
    | 0x0b -> Const (Read.s16 cursor)
    | 0x0c -> Const (Read.u32 cursor)
    | 0x0d -> Const (Read.s32 cursor)
    | 0x0e -> Const (Read.u64 cursor)
    | 0x0f -> Const (Read.s64 cursor)
    | 0x10 -> Const (Read.uleb128 cursor)
    | 0x11 -> Const (Read.sleb128 cursor)
    | 0x12 -> Dup
    | 0x13 -> Drop
    | 0x14 -> Over
    | 0x15 -> Pick (Read.u8 cursor)
    | 0x16 -> Swap
    | 0x17 -> Rot
    | 0x18 -> Xderef
    | 0x19 -> Abs
    | 0x1a -> And
    | 0x1b -> Div
    | 0x1c -> Minus
    | 0x1d -> Mod
    | 0x1e -> Mul
    | 0x1f -> Neg
    | 0x20 -> Not
    | 0x21 -> Or
    | 0x22 -> Plus
    | 0x23 -> Plus_uconst (Read.uleb128 cursor)
    | 0x24 -> Shl
    | 0x25 -> Shr
    | 0x26 -> Shra
    | 0x27 -> Xor
    | 0x28 -> Bra (Read.s16 cursor)
    | 0x29 -> Eq
    | 0x2a -> Ge
    | 0x2b -> Gt
    | 0x2c -> Le
    | 0x2d -> Lt
    | 0x2e -> Ne
    | 0x2f -> Skip (Read.s16 cursor)
    | 0x30 -> Const 0
    | 0x31 -> Const 1
    | 0x32 -> Const 2
    | 0x33 -> Const 3
    | 0x34 -> Const 4
    | 0x35 -> Const 5
    | 0x36 -> Const 6
    | 0x37 -> Const 7
    | 0x38 -> Const 8
    | 0x39 -> Const 9
    | 0x3a -> Const 10
    | 0x3b -> Const 11
    | 0x3c -> Const 12
    | 0x3d -> Const 13
    | 0x3e -> Const 14
    | 0x3f -> Const 15
    | 0x40 -> Const 16
    | 0x41 -> Const 17
    | 0x42 -> Const 18
    | 0x43 -> Const 19
    | 0x44 -> Const 20
    | 0x45 -> Const 21
    | 0x46 -> Const 22
    | 0x47 -> Const 23
    | 0x48 -> Const 24
    | 0x49 -> Const 25
    | 0x4a -> Const 26
    | 0x4b -> Const 27
    | 0x4c -> Const 28
    | 0x4d -> Const 29
    | 0x4e -> Const 30
    | 0x4f -> Const 31
    | 0x50 -> Regx 0
    | 0x51 -> Regx 1
    | 0x52 -> Regx 2
    | 0x53 -> Regx 3
    | 0x54 -> Regx 4
    | 0x55 -> Regx 5
    | 0x56 -> Regx 6
    | 0x57 -> Regx 7
    | 0x58 -> Regx 8
    | 0x59 -> Regx 9
    | 0x5a -> Regx 10
    | 0x5b -> Regx 11
    | 0x5c -> Regx 12
    | 0x5d -> Regx 13
    | 0x5e -> Regx 14
    | 0x5f -> Regx 15
    | 0x60 -> Regx 16
    | 0x61 -> Regx 17
    | 0x62 -> Regx 18
    | 0x63 -> Regx 19
    | 0x64 -> Regx 20
    | 0x65 -> Regx 21
    | 0x66 -> Regx 22
    | 0x67 -> Regx 23
    | 0x68 -> Regx 24
    | 0x69 -> Regx 25
    | 0x6a -> Regx 26
    | 0x6b -> Regx 27
    | 0x6c -> Regx 28
    | 0x6d -> Regx 29
    | 0x6e -> Regx 30
    | 0x6f -> Regx 31
    | 0x70 -> Bregx (0, Read.sleb128 cursor)
    | 0x71 -> Bregx (1, Read.sleb128 cursor)
    | 0x72 -> Bregx (2, Read.sleb128 cursor)
    | 0x73 -> Bregx (3, Read.sleb128 cursor)
    | 0x74 -> Bregx (4, Read.sleb128 cursor)
    | 0x75 -> Bregx (5, Read.sleb128 cursor)
    | 0x76 -> Bregx (6, Read.sleb128 cursor)
    | 0x77 -> Bregx (7, Read.sleb128 cursor)
    | 0x78 -> Bregx (8, Read.sleb128 cursor)
    | 0x79 -> Bregx (9, Read.sleb128 cursor)
    | 0x7a -> Bregx (10, Read.sleb128 cursor)
    | 0x7b -> Bregx (11, Read.sleb128 cursor)
    | 0x7c -> Bregx (12, Read.sleb128 cursor)
    | 0x7d -> Bregx (13, Read.sleb128 cursor)
    | 0x7e -> Bregx (14, Read.sleb128 cursor)
    | 0x7f -> Bregx (15, Read.sleb128 cursor)
    | 0x80 -> Bregx (16, Read.sleb128 cursor)
    | 0x81 -> Bregx (17, Read.sleb128 cursor)
    | 0x82 -> Bregx (18, Read.sleb128 cursor)
    | 0x83 -> Bregx (19, Read.sleb128 cursor)
    | 0x84 -> Bregx (20, Read.sleb128 cursor)
    | 0x85 -> Bregx (21, Read.sleb128 cursor)
    | 0x86 -> Bregx (22, Read.sleb128 cursor)
    | 0x87 -> Bregx (23, Read.sleb128 cursor)
    | 0x88 -> Bregx (24, Read.sleb128 cursor)
    | 0x89 -> Bregx (25, Read.sleb128 cursor)
    | 0x8a -> Bregx (26, Read.sleb128 cursor)
    | 0x8b -> Bregx (27, Read.sleb128 cursor)
    | 0x8c -> Bregx (28, Read.sleb128 cursor)
    | 0x8d -> Bregx (29, Read.sleb128 cursor)
    | 0x8e -> Bregx (30, Read.sleb128 cursor)
    | 0x8f -> Bregx (31, Read.sleb128 cursor)
    | 0x90 -> Regx (Read.uleb128 cursor)
    | 0x91 -> Fbreg (Read.sleb128 cursor)
    | 0x92 ->
        let r = Read.uleb128 cursor in
        let o = Read.sleb128 cursor in
        Bregx (r, o)
    | 0x93 -> Piece (Read.uleb128 cursor)
    | 0x94 -> Deref_size (Read.u8 cursor)
    | 0x95 -> Xderef_size (Read.u8 cursor)
    | 0x96 -> Nop
    | 0x97 -> Push_object_address
    | 0x98 -> Call2 (Read.u8 cursor)
    | 0x99 -> Call4 (Read.u16 cursor)
    | 0x9a -> Call_ref (read format cursor)
    | 0x9b -> Form_tls_address
    | 0x9c -> Call_frame_cfa
    | 0x9d ->
        let x = Read.uleb128 cursor in
        let y = Read.uleb128 cursor in
        Bit_piece (x, y)
    | 0x9e ->
        let size = Read.uleb128 cursor in
        let block = String.init size (fun _ -> Char.chr (Read.u8 cursor)) in
        Implicit_value (size, block)
    | 0x9f -> Stack_value
    | 0xa0 ->
        let x = read format cursor in
        Implicit_pointer (x, Read.sleb128 cursor)
    | 0xa1 -> Addr (Read.uleb128 cursor)
    | 0xa2 -> Const (Read.uleb128 cursor)
    | 0xa3 ->
        let size = Read.uleb128 cursor in
        let block = String.init size (fun _ -> Char.chr (Read.u8 cursor)) in
        Entry_value (size, block)
    | 0xa4 ->
        let offset = Read.uleb128 cursor in
        let size = Read.u8 cursor in
        let block = String.init size (fun _ -> Char.chr (Read.u8 cursor)) in
        Const_type (offset, size, block)
    | 0xa5 -> Regval_type (Read.uleb128 cursor, Read.uleb128 cursor)
    | 0xa6 -> Deref_type (Read.u8 cursor, Read.uleb128 cursor)
    | 0xa7 -> Xderef_type (Read.u8 cursor, Read.uleb128 cursor)
    | 0xa8 -> Convert (Read.uleb128 cursor)
    | 0xa9 -> Reinterpret (Read.uleb128 cursor)
    | x ->
        raise @@ Errors.not_yet_implemented
        @@ Format.sprintf "Non supported operator 0x%x" x

  let loc stack frame op =
    match (op, stack) with
    | Addr a, _ | Const a, _ ->
        let size = Dba.Expr.size_of (map 0) in
        Dba.Expr.constant (Bitvector.of_int ~size a) :: stack
    | Deref, a :: stack ->
        Dba.Expr.load
          Size.(Byte.of_bitsize (Bit.create (Dba.Expr.size_of a)))
          (Kernel_options.Machine.endianness ())
          a
        :: stack
    | Dup, x :: _ -> x :: stack
    | Drop, _ :: stack -> stack
    | Over, _ :: x :: _ -> x :: stack
    | Pick n, _ -> List.nth stack n :: stack
    | Swap, x :: y :: stack -> y :: x :: stack
    | Rot, x :: y :: z :: stack -> y :: z :: x :: stack
    | Abs, x :: stack ->
        Dba.Expr.(ite (slt x (zeros (size_of x))) (uminus x) x) :: stack
    | And, x :: y :: stack -> Dba.Expr.logand y x :: stack
    | Minus, x :: y :: stack -> Dba.Expr.sub y x :: stack
    | Mul, x :: y :: stack -> Dba.Expr.mul y x :: stack
    | Neg, x :: stack -> Dba.Expr.uminus x :: stack
    | Not, x :: stack -> Dba.Expr.lognot x :: stack
    | Or, x :: y :: stack -> Dba.Expr.logor y x :: stack
    | Plus, x :: y :: stack -> Dba.Expr.add y x :: stack
    | Plus_uconst a, x :: stack ->
        let size = Dba.Expr.size_of x in
        Dba.Expr.(add x (constant (Bitvector.of_int ~size a))) :: stack
    | Shl, x :: y :: stack -> Dba.Expr.shift_left y x :: stack
    | Shr, x :: y :: stack -> Dba.Expr.shift_right y x :: stack
    | Shra, x :: y :: stack -> Dba.Expr.shift_right_signed y x :: stack
    | Xor, x :: y :: stack -> Dba.Expr.logxor y x :: stack
    | Eq, x :: y :: stack -> Dba.Expr.(uext (size_of y) (equal y x)) :: stack
    | Ge, x :: y :: stack -> Dba.Expr.(uext (size_of y) (sge y x)) :: stack
    | Gt, x :: y :: stack -> Dba.Expr.(uext (size_of y) (sgt y x)) :: stack
    | Le, x :: y :: stack -> Dba.Expr.(uext (size_of y) (sle y x)) :: stack
    | Lt, x :: y :: stack -> Dba.Expr.(uext (size_of y) (slt y x)) :: stack
    | Ne, x :: y :: stack -> Dba.Expr.(uext (size_of y) (diff y x)) :: stack
    | Regx x, _ -> map x :: stack
    | Fbreg o, _ ->
        let frame = Utils.unsafe_get_opt frame in
        let size = Dba.Expr.size_of frame in
        Dba.Expr.(add frame (constant (Bitvector.of_int ~size o))) :: stack
    | Bregx (r, a), _ ->
        let reg = map r in
        let size = Dba.Expr.size_of reg in
        Dba.Expr.(add reg (constant (Bitvector.of_int ~size a))) :: stack
    | Nop, _ -> stack
    | Call_frame_cfa, _ -> Utils.unsafe_get_opt frame :: stack
    | _ -> Errors.not_yet_implemented "Non compliant operator"
end

type t = Operator.t list

let load format cursor : t =
  let block = sub cursor (Read.uleb128 cursor) in
  let rec loop ops =
    if at_end block then List.rev ops
    else loop (Operator.load format block :: ops)
  in
  loop []

let loc ?cfa expr : Dba.Expr.t =
  List.hd (List.fold_left (fun stack op -> Operator.loc stack cfa op) [] expr)

let pp ppf ops =
  List.iter (fun op -> Format.fprintf ppf "%a " Operator.pp op) ops
