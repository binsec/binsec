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

(** Instruction set decoder, parametrized by the register size *)
module Riscv_to_Dba (M : Riscv_arch.RegisterSize) = struct
  (** Register size *)
  let mode = M.size

  (** Register size (in bits) *)
  let mode_size = Riscv_arch.Mode.size mode

  let fail msg =
    let msg = Printf.sprintf "Risc-V %d: %s" mode_size msg in
    failwith msg

  let assert_size size name =
    if mode <> size then
      fail
        (Printf.sprintf "%s only available in %d mode" name
           (Riscv_arch.Mode.size size))

  let assert_mode_is_64 = assert_size Riscv_arch.Mode.m64

  module De = Dba.Expr
  module Di = Dba.Instr
  module Bv = Bitvector
  module Rar = Riscv_arch.Register (M)
  module StrMap = Basic_types.String.Map
  module L = Riscv_options.Logger

  (* In 64 mode, a few operation work on 32 bit values then sign extend them *)
  let to_32 x = De.restrict 0 31 x
  let sext x = De.sext mode_size x

  (* Instruction size *)
  module Isz = struct
    type t = I16 | I32

    let bytes = function I16 -> 2 | I32 -> 4
  end

  module D_status = struct
    type t = { addr : Virtual_address.t; isz : Isz.t }

    let addr t = t.addr
    let bysz t = Isz.bytes t.isz
    let create ?(isz = Isz.I32) va = { addr = va; isz }
    let switch isz t = { t with isz }
    let switch16 = switch Isz.I16
    let switch32 = switch Isz.I32
    let next t = Virtual_address.add_int (bysz t) (addr t)
  end

  (* let mode_bits = Size.Bit.create mode_size *)

  (* let mode_bytes = Size.Byte.of_bitsize mode_bits *)

  let mk_bv ?(size = mode_size) v = Bitvector.create v size
  let mk_cst ?(size = mode_size) extend bv = De.constant (extend bv size)
  let mk_imm ?(size = mode_size) = mk_cst ~size Bv.extend_signed
  let _mk_offset = mk_cst Bv.extend_signed
  let _mk_uint = mk_cst Bv.extend

  let scale_by n bv =
    let bvn = Bitvector.of_int ~size:(Bv.size_of bv) n in
    Bv.mul bv bvn

  let uint bv =
    let v = Bv.value_of bv |> Z.to_int in
    assert (v >= 0);
    v

  module D = struct
    type label = string
    type jt = E of Dba.Expr.t | C of Virtual_address.t | L of string

    type inst =
      | Asg of Dba.Expr.t * Dba.Expr.t (* Asg(dst, src) *)
      | Jmp of jt
      | Lab of label * inst
      | Nop

    let _label =
      let n = ref (-1) in
      fun () ->
        incr n;
        L ("l" ^ string_of_int !n)

    let ( <-- ) dst src =
      if Dba.Expr.is_equal dst src then Nop else Asg (dst, src)

    let jmp j = Jmp j
    let ejmp e = jmp (E e)
    let cjmp c = jmp (C c)

    let aoff ?(offset = Bv.zero) va =
      let ve = De.constant (mk_bv (Virtual_address.to_bigint va)) in
      if Bv.is_zero offset then ve else De.add ve (mk_imm offset)

    let vajmp ?(offset = 0) va = cjmp (Virtual_address.add_int offset va)
    let _ljmp l = jmp (L l)

    let lab label inst =
      match inst with
      | Lab _ -> raise (Invalid_argument "label")
      | _ -> Lab (label, inst)

    let is_zero_var v = v.Dba.Var.name = "zero"

    let rec replace_zero e =
      let zero_cst = De.zeros (Kernel_options.Machine.word_size ()) in
      match e with
      | De.Var v when is_zero_var v -> zero_cst
      | De.Var _ -> e
      | De.Load (size, endianness, expr, array) ->
          let e = replace_zero expr in
          De.load (Size.Byte.create size) endianness e ?array
      | De.Cst _ -> e
      | De.Unary (uop, e) ->
          let e = replace_zero e in
          De.unary uop e
      | De.Binary (bop, e1, e2) ->
          let e1 = replace_zero e1 in
          let e2 = replace_zero e2 in
          De.binary bop e1 e2
      | De.Ite (cond, then_, else_) ->
          let cond = replace_zero cond in
          let then_ = replace_zero then_ in
          let else_ = replace_zero else_ in
          De.ite cond then_ else_

    let rec to_dba_instr instr ?id lbl_id =
      match instr with
      | Asg (src, dst) ->
          assert (Dba.LValue.is_expr_translatable src);
          let lval = Dba.LValue.of_expr src in
          let is_zero =
            match lval with
            | Dba.LValue.Var v | Dba.LValue.Restrict (v, _) -> is_zero_var v
            | _ -> false
          in
          if is_zero then None
          else
            let lval =
              match lval with
              | Dba.LValue.Var _ | Dba.LValue.Restrict _ -> lval
              | Dba.LValue.Store (sz, endianness, e, array) ->
                  let e = replace_zero e in
                  Dba.LValue.store (Size.Byte.create sz) endianness e ?array
            in
            let dst = replace_zero dst in
            let id =
              match id with
              | None -> raise (Invalid_argument "to_dba_instr")
              | Some i -> i
            in
            Some (Di.assign lval dst (id + 1))
      | Jmp jt ->
          Some
            (match jt with
            | E e -> Di.dynamic_jump (replace_zero e)
            | C c ->
                Di.static_jump
                  Dba.(JOuter (Dba_types.Caddress.of_virtual_address c))
            | L l -> Di.static_inner_jump (StrMap.find l lbl_id))
      | Lab (_, i) -> to_dba_instr i lbl_id
      | Nop -> None

    let pp_e = Dba_printer.EICAscii.pp_bl_term

    let rec pp ppf = function
      | Asg (d, s) -> Format.fprintf ppf "@[<h>%a <-- %a@]" pp_e d pp_e s
      | Jmp (E e) -> Format.fprintf ppf "@[<h>ej %a@]" pp_e e
      | Jmp (C va) -> Format.fprintf ppf "@[<h>j %@%a@]" Virtual_address.pp va
      | Jmp (L l) -> Format.fprintf ppf "@[<h>lj :%s:@]" l
      | Lab (l, i) -> Format.fprintf ppf "@[<h>:%s: %a@]" l pp i
      | Nop -> Format.pp_print_string ppf "nop"

    module Block = struct
      type t = { sealed : bool; insts : inst list }

      let last_label = "last"
      let empty = { insts = []; sealed = false }
      let is_empty t = t.insts = []

      (* Actually, we can never have a sealed block without DBA instructions *)

      (* Works only on open blocks
         I chose +++ because it is as long as "ini" thus has a prettier alignment
         default
      *)
      let ( +++ ) b inst =
        assert (not b.sealed);
        { b with insts = inst :: b.insts }

      let ini inst = empty +++ inst
      let ( !! ) b = { insts = List.rev b.insts; sealed = true }

      let seal a b =
        let last = lab last_label (vajmp a) in
        !!(b +++ last)

      let is_sealed b = b.sealed

      let _pp ppf b =
        let insts = if b.sealed then b.insts else List.rev b.insts in
        let open Format in
        pp_open_vbox ppf 0;
        List.iter
          (fun i ->
            pp ppf i;
            pp_print_cut ppf ())
          insts;
        pp_close_box ppf ()

      let add_opt opt l = match opt with None -> l | Some x -> x :: l

      (* takes Pre_dba block and returns Dhunk.t *)
      let to_dba b =
        (* check that block is sealed *)
        assert (is_sealed b);
        (* get label -> id *)
        let _, lbl_id =
          List.fold_left
            (fun (id, map) instr ->
              (* invariant: no Label (Label e)) *)
              match instr with
              | Lab (lbl, _) -> (id + 1, StrMap.add lbl id map)
              | Asg _ | Jmp _ | Nop -> (id + 1, map))
            (0, StrMap.empty) b.insts
        in
        (* check that block contains more than Nop *)
        assert (b.insts <> [ Nop ]);
        (* pre_dba -> dba *)
        let _, instrs =
          List.fold_left
            (fun (id, l) instr ->
              match instr with
              | Nop -> (id, l)
              | Jmp _ -> (id + 1, add_opt (to_dba_instr instr lbl_id) l)
              | Asg _ | Lab _ ->
                  (id + 1, add_opt (to_dba_instr ~id instr lbl_id) l))
            (0, []) b.insts
        in
        List.rev instrs |> Dhunk.of_list
    end
  end

  module Inst = struct
    type t = { mnemonic : string; opcode : Bitvector.t; dba : D.Block.t }

    let _empty = { mnemonic = ""; opcode = Bitvector.zero; dba = D.Block.empty }

    let _is_empty inst =
      Bitvector.equal inst.opcode Bitvector.zero
      && String.length inst.mnemonic = 0
      && D.Block.is_empty inst.dba

    let _set_opcode t opcode = { t with opcode }
    let _set_dba t dba = { t with dba }
    let create ~dba ~opcode ~mnemonic = { dba; opcode; mnemonic }
  end

  type _config = { mode : Riscv_arch.Mode.t }
  (* Functorize the interface to set the mode in different decoders *)

  module Bitset = struct
    let restrict ~lo ~hi bits =
      let open Interval in
      Bv.extract bits { lo; hi }

    let eight = Bitvector.of_int ~size:mode_size 8

    (* RVC registers are actually offsets from x8. See p.71 of User-Level ISA
       V2.2 *)
    let reg3 ~lo ~hi bits =
      assert (hi - lo + 1 = 3);
      let bvreg = restrict ~lo ~hi bits in
      let bv32 = Bv.extend bvreg mode_size in
      Bv.add bv32 eight
  end

  (** Bitvector utils *)
  let cut bv n = Bv.extract bv { lo = n; hi = n }

  let signed_int bv = Bv.signed_of bv |> Z.to_int
  let reg_num bv = Rar.of_int_exn @@ Z.to_int @@ Bitvector.value_of bv
  let reg_bv bv = Rar.expr (reg_num bv)
  let reg_name bv = Rar.name (reg_num bv)

  let op_imm mnemonic ~src ~dst ~imm =
    Printf.sprintf "%s %s,%s,%s" mnemonic (reg_name dst) (reg_name src)
      (Z.to_string (Bv.signed_of imm))

  let op3_str ~name ~dst ~src1 ~src2 =
    Printf.sprintf "%s %s,%s,%s" name (reg_name dst) (reg_name src1)
      (reg_name src2)

  (** Amounts of bits used to represent a shift
    Since are smaller then 32 (or 64 bits)
    they fit on 5 or 6 bits *)
  let shift_size force_32 =
    if force_32 || Riscv_arch.Mode.is_m32 mode then 4
    else if Riscv_arch.Mode.is_m64 mode then 5
    else assert false

  module Lift = struct
    open D

    let is_x0 =
      let z = Rar.expr Rar.zero in
      ( = ) z

    let is_ra bv = Z.to_int (Bv.value_of bv) = Rar.num Rar.ra

    open Block

    let nop st = Block.empty |> seal (D_status.next st)

    let loadd t =
      assert_mode_is_64 "loadd";
      De.load (Size.Byte.create 8) Machine.LittleEndian t

    let loadw = De.load (Size.Byte.create 4) Machine.LittleEndian

    (* is h/b defined differenlty when in 64 mode ? (ie., 32/16 bits) *)
    let loadh = De.load (Size.Byte.create 2) Machine.LittleEndian
    let loadb = De.load (Size.Byte.create 1) Machine.LittleEndian

    let load name load_f st ~dst ~src ~offset =
      let dba =
        ini (reg_bv dst <-- load_f (De.add (reg_bv src) (mk_imm offset)))
        |> seal (D_status.next st)
      in
      let mnemonic =
        Printf.sprintf "%s %s,%s(%s)" name (reg_name dst)
          (Z.to_string (Bv.signed_of offset))
          (reg_name src)
      in
      (mnemonic, dba)

    let ld = load "ld" loadd
    let lw = load "lw" (fun e -> sext (loadw e))
    let lwu = load "lwu" (fun e -> De.uext mode_size (loadw e))
    let lh = load "lh" (fun e -> sext (loadh e))
    let lhu = load "lhu" (fun e -> De.uext mode_size (loadh e))
    let lb = load "lb" (fun e -> sext (loadb e))
    let lbu = load "lbu" (fun e -> De.uext mode_size (loadb e))

    let store name store_f restrict st ~offset ~base ~src =
      let dba =
        ini
          (store_f (De.add (reg_bv base) (mk_imm offset))
          <-- restrict (reg_bv src))
        |> seal (D_status.next st)
      in
      let mnemonic =
        (* The order src,offset(dst) is the one adopted by objdump *)
        Printf.sprintf "%s %s,%s(%s)" name (reg_name src)
          (Z.to_string (Bv.signed_of offset))
          (reg_name base)
      in
      (mnemonic, dba)

    (** Store instructions (8 bit, 16 bit, 32 bit, 64 bit) *)
    let sb = store "sb" loadb (De.restrict 0 7)

    let sh = store "sh" loadh (De.restrict 0 15)
    let sw = store "sw" loadw (De.restrict 0 31)
    let sd = store "sd" loadd (De.restrict 0 63)

    let jmp_offset st offset =
      let offset = Z.to_int (Bitvector.signed_of offset) in
      Virtual_address.add_int offset (D_status.addr st)

    let branch name cmp st ~src1 ~src2 ~offset =
      let jump_addr = jmp_offset st offset in
      let dba =
        let jneg = aoff (D_status.next st) in
        let jpos = aoff jump_addr in
        !!(ini @@ ejmp (De.ite (cmp (reg_bv src1) (reg_bv src2)) jpos jneg))
      in
      let mnemonic =
        (* If the second source register is zero then print special 'z' form of
           operator. *)
        if Bv.is_zeros src2 then
          Format.asprintf "%sz %s,%a" name (reg_name src1) Virtual_address.pp
            jump_addr
        else
          Format.asprintf "%s %s,%s,%a" name (reg_name src1) (reg_name src2)
            Virtual_address.pp jump_addr
      in
      (mnemonic, dba)

    let bne = branch "bne" De.diff
    let beq = branch "beq" De.equal
    let blt = branch "blt" De.slt
    let bltu = branch "bltu" De.ult
    let bge = branch "bge" De.sge
    let bgeu = branch "bgeu" De.uge

    let slti st ~dst ~src ~imm =
      let dba =
        ini
          (reg_bv dst
          <-- De.ite
                (De.slt (reg_bv src) (mk_imm imm))
                (De.ones mode_size) (De.zeros mode_size))
        |> seal (D_status.next st)
      in
      let mnemonic = op_imm "slti" ~dst ~src ~imm in
      (mnemonic, dba)

    let sltiu st ~dst ~src ~imm =
      let dba =
        ini
          (reg_bv dst
          <-- De.ite
                (De.ult (reg_bv src) (mk_imm imm))
                (* immediate extension is
                   signed or unsigned ? *)
                (De.ones mode_size)
                (De.zeros mode_size))
        |> seal (D_status.next st)
      in
      let mnemonic = op_imm "sltiu" ~dst ~src ~imm in
      (mnemonic, dba)

    (** add with immediate: dst = src + imm *)
    let addi st ~dst ~src ~imm =
      let dst_e = reg_bv dst in
      let do_seal = seal (D_status.next st) in
      (* nop when dst is zero *)
      if is_x0 dst_e then ("nop", do_seal empty)
      else
        let src_e = reg_bv src in
        let mnemonic =
          if Bv.is_zeros imm then
            Printf.sprintf "mv %s,%s" (reg_name dst) (reg_name src)
          else op_imm "addi" ~dst ~src ~imm
        in
        (mnemonic, do_seal (ini (dst_e <-- De.add src_e (mk_imm imm))))

    (** add word with immediate: dst = sign_extend (src[31:0] + imm) *)
    let addiw st ~dst ~src ~imm =
      assert_mode_is_64 "addiw";
      let dst_e = reg_bv dst in
      let do_seal = seal (D_status.next st) in
      (* nop when dst is zero *)
      if is_x0 dst_e then ("nop", do_seal empty)
      else
        let src_e = reg_bv src in
        let mnemonic = op_imm "addiw" ~dst ~src ~imm in
        ( mnemonic,
          do_seal
            (ini (dst_e <-- sext (De.add (to_32 src_e) (mk_imm ~size:32 imm))))
        )

    let logicali name logop st ~dst ~src ~imm =
      let dba =
        ini (reg_bv dst <-- logop (reg_bv src) (mk_imm imm))
        |> seal (D_status.next st)
      in
      let mnemonic = op_imm name ~src ~dst ~imm in
      (mnemonic, dba)

    let andi = logicali "andi" De.logand
    let xori = logicali "xori" De.logxor
    let ori = logicali "ori" De.logor

    (** Binary operation with given name and function *)
    let bop name f st ~dst ~src1 ~src2 =
      let dba =
        ini (reg_bv dst <-- f (reg_bv src1) (reg_bv src2))
        |> seal (D_status.next st)
      in
      let mnemonic = op3_str ~name ~dst ~src1 ~src2 in
      (mnemonic, dba)

    let add = bop "add" De.add
    let sub = bop "sub" De.sub
    let logxor = bop "xor" De.logxor
    let logor = bop "or" De.logor
    let logand = bop "and" De.logand

    (** Binary operation on word in 64 bit mode *)
    let bop_w name f st ~dst ~src1 ~src2 =
      assert_mode_is_64 name;
      let dba =
        ini (reg_bv dst <-- sext (f (to_32 (reg_bv src1)) (to_32 (reg_bv src2))))
        |> seal (D_status.next st)
      in
      let mnemonic = op3_str ~name ~dst ~src1 ~src2 in
      (mnemonic, dba)

    let addw = bop_w "addw" De.add
    let subw = bop_w "subw" De.sub

    let _mul name restrict ext1 ext2 st ~dst ~src1 ~src2 =
      let dba =
        ini
          (reg_bv dst
          <-- restrict (De.mul (ext1 (reg_bv src1)) (ext2 (reg_bv src2))))
        |> seal (D_status.next st)
      in
      let mnemonic = op3_str ~name ~dst ~src1 ~src2 in
      (mnemonic, dba)

    let rmul_lo = De.restrict 0 (mode_size - 1)
    let rmul_hi = De.restrict mode_size ((2 * mode_size) - 1)
    let sextmul = De.sext @@ (2 * mode_size)
    let uextmul = De.uext @@ (2 * mode_size)
    let mul = _mul "mul" rmul_lo sextmul sextmul

    (* let mulu  = _mul "mulu" rmul_lo uextmul uextmul ;;
     * let mulsu = _mul "mulsu" rmul_lo sextmul uextmul ;; *)

    let mulh = _mul "mulh" rmul_hi sextmul sextmul
    let mulhu = _mul "mulhu" rmul_hi uextmul uextmul
    let mulhsu = _mul "mulhsu" rmul_hi sextmul uextmul

    let mulw st ~dst ~src1 ~src2 =
      assert_mode_is_64 "mulw";
      let dba =
        ini
          (reg_bv dst
          <-- sext (De.mul (to_32 (reg_bv src1)) (to_32 (reg_bv src2))))
        |> seal (D_status.next st)
      in
      let mnemonic = op3_str ~name:"mulw" ~dst ~src1 ~src2 in
      (mnemonic, dba)

    let eq_zero e = De.equal e (De.zeros (De.size_of e))
    let minus_one size = De.constant (Bv.of_int ~size (-1))
    let min_int size = De.constant (Bv.min_sbv size)
    let eq_minus_one e = De.equal e (minus_one (De.size_of e))
    let eq_min_int e = De.equal e (min_int (De.size_of e))

    let _div ?on_overflow ~on_div_by_zero name f st ~dst ~src1 ~src2 =
      let dba =
        let divisor = reg_bv src2 in
        let dividend = reg_bv src1 in
        let e =
          De.ite (eq_zero divisor) on_div_by_zero
            (match on_overflow with
            | Some of_e ->
                De.ite
                  (De.logand (eq_minus_one divisor) (eq_min_int dividend))
                  of_e (f dividend divisor)
            | None -> f dividend divisor)
        in
        ini (reg_bv dst <-- e) |> seal (D_status.next st)
      in
      let mnemonic = op3_str ~name ~dst ~src1 ~src2 in
      (mnemonic, dba)

    let div =
      _div ~on_overflow:(min_int mode_size)
        ~on_div_by_zero:(minus_one mode_size) "div" De.sdiv

    let divu =
      _div ~on_div_by_zero:(De.constant (Bv.max_ubv mode_size)) "divu" De.udiv

    let rem st ~dst ~src1 ~src2 =
      _div ~on_div_by_zero:(reg_bv src1) ~on_overflow:(De.zeros mode_size) "rem"
        De.smod st ~dst ~src1 ~src2

    let remu st ~dst ~src1 ~src2 =
      _div ~on_div_by_zero:(reg_bv src1) "remu" De.umod st ~dst ~src1 ~src2

    (** 32 bit division in 64 bit mode - same semantics as 32 bit division
      with a final sign extension *)
    let _divw ?on_overflow ~on_div_by_zero name f st ~dst ~src1 ~src2 =
      assert_mode_is_64 name;
      let dba =
        let divisor = to_32 (reg_bv src2) in
        let dividend = to_32 (reg_bv src1) in
        let res = De.sext mode_size (f dividend divisor) in
        let e =
          De.ite (eq_zero divisor) on_div_by_zero
            (match on_overflow with
            | Some of_e ->
                De.ite
                  (De.logand (eq_minus_one divisor) (eq_min_int dividend))
                  of_e res
            | None -> res)
        in
        ini (reg_bv dst <-- sext e) |> seal (D_status.next st)
      in
      let mnemonic = op3_str ~name ~dst ~src1 ~src2 in
      (mnemonic, dba)

    let divw =
      _divw
        ~on_overflow:(De.constant (Bv.fill ~lo:31 mode_size))
        ~on_div_by_zero:(minus_one mode_size) "divw" De.sdiv

    let divuw =
      _divw ~on_div_by_zero:(De.constant (Bv.max_ubv mode_size)) "divuw" De.udiv

    let remw st ~dst ~src1 ~src2 =
      _divw ~on_div_by_zero:(reg_bv src1) ~on_overflow:(De.zeros mode_size)
        "remw" De.smod st ~dst ~src1 ~src2

    let remuw st ~dst ~src1 ~src2 =
      _divw ~on_div_by_zero:(reg_bv src1) "remuw" De.umod st ~dst ~src1 ~src2

    let slt_f name cmp st ~dst ~src1 ~src2 =
      let dba =
        ini
          (reg_bv dst
          <-- De.ite
                (cmp (reg_bv src1) (reg_bv src2))
                (De.ones mode_size) (De.zeros mode_size))
        |> seal (D_status.next st)
      in
      let mnemonic = op3_str ~name ~dst ~src1 ~src2 in
      (mnemonic, dba)

    let slt = slt_f "slt" De.slt
    let sltu = slt_f "ult" De.ult

    let shift_f name shop st ~dst ~src1 ~src2 =
      let shift_size = shift_size false in
      let dba =
        ini
          (reg_bv dst
          <-- shop (reg_bv src1)
                (* For some reason DBA allows negative shifts... *)
                (De.uext mode_size (De.restrict 0 shift_size (reg_bv src2))))
        |> seal (D_status.next st)
      in
      let mnemonic = op3_str ~name ~dst ~src1 ~src2 in
      (mnemonic, dba)

    let sll = shift_f "sll" De.shift_left
    let srl = shift_f "srl" De.shift_right
    let sra = shift_f "sra" De.shift_right_signed

    let shift_f_w name shop st ~dst ~src1 ~src2 =
      assert_mode_is_64 name;
      let shift_size = shift_size true in
      let dba =
        ini
          (reg_bv dst
          <-- sext
                (shop
                   (to_32 (reg_bv src1))
                   (De.uext 32 (De.restrict 0 shift_size (reg_bv src2)))))
        |> seal (D_status.next st)
      in
      let mnemonic = op3_str ~name ~dst ~src1 ~src2 in
      (mnemonic, dba)

    let sllw = shift_f_w "sllw" De.shift_left
    let srlw = shift_f_w "srlw" De.shift_right
    let sraw = shift_f_w "sraw" De.shift_right_signed

    let mov st ~dst ~src =
      if Bv.equal dst src then ("nop", seal (D_status.next st) empty)
      else
        ( Printf.sprintf "mv %s,%s" (reg_name dst) (reg_name src),
          ini (reg_bv dst <-- reg_bv src) |> seal (D_status.next st) )

    let jal st ~dst ~offset =
      let jmp_addr =
        let offset = Z.to_int (Bitvector.signed_of offset) in
        Virtual_address.add_int offset (D_status.addr st)
      in
      let dba =
        !!(ini (reg_bv dst <-- aoff (D_status.next st)) +++ vajmp jmp_addr)
      in
      let mnemonic = Format.asprintf "jal %a" Virtual_address.pp jmp_addr in
      (mnemonic, dba)

    (** instr_size is the size in bytes, so 2 for compressed and 4 for normal *)
    let jalr st ~instr_size ~dst ~src ~offset =
      let jump_addr =
        let _offset = Z.to_int (Bitvector.signed_of offset) in
        Virtual_address.add_int instr_size (D_status.addr st)
      in
      let r = reg_bv dst in
      (* We need a temporary value for when dst = src *)
      let temp = De.temporary ~size:mode_size "tmp_jalr" in
      let base = ini (temp <-- De.add (reg_bv src) (mk_imm offset)) in
      let sr =
        if is_x0 r then base
        else
          let next = aoff jump_addr in
          base +++ (r <-- next)
      in
      let dba = !!(sr +++ ejmp temp) in
      let mnemonic =
        if is_x0 r then "ret"
        else
          Format.asprintf "jalr %s,%s,%a" (reg_name dst) (reg_name src)
            Virtual_address.pp jump_addr
      in
      (mnemonic, dba)

    let jr st r =
      let _, dba =
        jalr st ~instr_size:4 ~dst:Bitvector.zero ~src:r ~offset:Bitvector.zero
      in
      let mnemonic =
        if is_ra r then "ret" else Printf.sprintf "jr %s" (reg_name r)
      in
      (mnemonic, dba)

    let auipc st ~dst ~offset =
      let dba =
        let offset =
          Bv.extend_signed (Bv.append offset (Bv.zeros 12)) mode_size
        in
        ini (reg_bv dst <-- aoff (D_status.addr st) ~offset)
        |> seal (D_status.next st)
      in
      let mnemonic =
        Printf.sprintf "auipc %s,%s" (reg_name dst) (Bv.to_hexstring offset)
      in
      (mnemonic, dba)

    let lui st ~dst ~offset =
      let dba =
        ini
          (reg_bv dst
          <-- De.sext mode_size
                (mk_imm ~size:32 (Bv.append offset (Bv.zeros 12))))
        |> seal (D_status.next st)
      in
      let mnemonic =
        Printf.sprintf "lui %s,%s" (reg_name dst)
          (Bitvector.to_hexstring offset)
      in
      (mnemonic, dba)

    let shift_immediate shop st ~dst ~src ~shamt =
      ini (reg_bv dst <-- shop (reg_bv src) (mk_cst Bv.extend shamt))
      |> seal (D_status.next st)

    let slli = shift_immediate De.shift_left
    let srli = shift_immediate De.shift_right
    let srai = shift_immediate De.shift_right_signed

    let shift_immediate_word shop st ~dst ~src ~shamt =
      assert_mode_is_64 "shift word (slliw/srliw/sraiw)";
      ini
        (reg_bv dst
        <-- sext (shop (to_32 (reg_bv src)) (mk_cst ~size:32 Bv.extend shamt)))
      |> seal (D_status.next st)

    let slliw = shift_immediate_word De.shift_left
    let srliw = shift_immediate_word De.shift_right
    let sraiw = shift_immediate_word De.shift_right_signed

    module C = struct
      let slli st ~dst ~shamt = slli st ~dst ~src:dst ~shamt
      let srli st ~dst ~shamt = srli st ~dst ~src:dst ~shamt
      let srai st ~dst ~shamt = srai st ~dst ~src:dst ~shamt

      let jstitch11 offset =
        let open Basic_types in
        let bvimm =
          Bv.concat
            [
              cut offset 10;
              (* bit 11 *)
              cut offset 6;
              (* bit 10 *)
              Bv.extract offset { lo = 7; hi = 8 };
              cut offset 4;
              (* bit 7 *)
              cut offset 5;
              (* bit 6 *)
              cut offset 0;
              (* bit 5 *)
              cut offset 9;
              Bv.extract offset { lo = 1; hi = 3 };
            ]
        in
        L.debug "C.j offset %a (%d)" Bv.pp bvimm (signed_int bvimm);
        Bv.extend_signed bvimm mode_size

      let real_offset offset = jstitch11 offset |> scale_by 2

      let jmp rdst st ~offset =
        let dst = Rar.(bvnum rdst) in
        jal st ~dst ~offset:(real_offset offset)

      let j st ~offset =
        let _, dba = jmp Rar.zero st ~offset in
        let jmp_addr = jmp_offset st (real_offset offset) in
        (Format.asprintf "j %a" Virtual_address.pp jmp_addr, dba)

      let jal = jmp (Rar.of_int_exn 1)

      let jalr st ~src =
        let dst = Rar.(bvnum ra) (* always ra *) in
        let offset = Bv.zero in
        let _, dba = jalr st ~instr_size:2 ~dst ~src ~offset in
        (Printf.sprintf "jalr %s" (reg_name src), dba)

      let swsp st ~src ~imm6 =
        let bvoff =
          (* imm6 is offset[5:2|7:6] *)
          Bv.concat
            [
              Bv.extract imm6 { lo = 0; hi = 1 };
              Bv.extract imm6 { lo = 2; hi = 4 };
            ]
        in
        let offset = scale_by 4 (Bv.extend bvoff mode_size) in
        sw st ~src ~base:Rar.(bvnum sp) ~offset

      let sdsp st ~src ~imm6 =
        assert_mode_is_64 "C.sdsp";
        let bvoff =
          (* imm6 is offset[5:3|8:6] *)
          Bv.concat
            [
              Bv.extract imm6 { lo = 0; hi = 2 };
              Bv.extract imm6 { lo = 3; hi = 4 };
            ]
        in
        let offset = scale_by 8 (Bv.extend bvoff mode_size) in
        sd st ~src ~base:Rar.(bvnum sp) ~offset

      let sw st bits =
        let base = Bitset.reg3 ~lo:7 ~hi:9 bits in
        let src = Bitset.reg3 ~lo:2 ~hi:4 bits in
        let imm3 = Bitset.restrict ~lo:10 ~hi:12 bits in
        (* offset[5:3]*)
        let imm2 = Bitset.restrict ~lo:5 ~hi:6 bits in
        (* offset[2|6] *)
        let bvoff = Bv.concat [ cut imm2 0; imm3; cut imm2 1 ] in
        let offset = scale_by 4 (Bv.extend bvoff mode_size) in
        sw st ~base ~src ~offset

      let sd st bits =
        assert_mode_is_64 "C.sd";
        let base = Bitset.reg3 ~lo:7 ~hi:9 bits in
        let src = Bitset.reg3 ~lo:2 ~hi:4 bits in
        let imm3 = Bitset.restrict ~lo:10 ~hi:12 bits in
        (* offset[5:3]*)
        let imm2 = Bitset.restrict ~lo:5 ~hi:6 bits in
        (* offset[7:6]*)
        let bvoff = Bv.concat [ imm2; imm3 ] in
        let offset = scale_by 8 (Bv.extend bvoff mode_size) in
        sd st ~base ~src ~offset

      let lwsp st ~dst ~off1 ~off5 =
        (* offset is split in two fields:
            - off1 is offset[5]
            - off2 is offset[4:2|7:6]
        *)
        let bvoff =
          Bv.concat
            [
              Bv.extract off5 { lo = 0; hi = 1 };
              off1;
              Bv.extract off5 { lo = 2; hi = 4 };
            ]
        in
        let offset = scale_by 4 (Bv.extend bvoff mode_size) in
        lw st ~dst ~offset ~src:Rar.(bvnum sp)

      let ldsp st ~dst ~off1 ~off5 =
        assert_mode_is_64 "C.ldsp";
        (* offset is split in two fields:
            - off1 is offset[5]
            - off2 is offset[4:3|8:6]
        *)
        let bvoff =
          Bv.concat
            [
              Bv.extract off5 { lo = 0; hi = 2 };
              off1;
              Bv.extract off5 { lo = 3; hi = 4 };
            ]
        in
        let offset = scale_by 8 (Bv.extend bvoff mode_size) in
        ld st ~dst ~offset ~src:Rar.(bvnum sp)

      let lw st bits =
        let dst = Bitset.reg3 ~lo:2 ~hi:4 bits in
        let base = Bitset.reg3 ~lo:7 ~hi:9 bits in
        let imm2 = Bitset.restrict ~lo:5 ~hi:6 bits in
        (* offset[2|6] *)
        let imm3 = Bitset.restrict ~lo:10 ~hi:12 bits in
        (* offset[5:3] *)
        let bvoff = Bv.concat [ cut imm2 0; imm3; cut imm2 1 ] in
        let offset = scale_by 4 (Bv.extend_signed bvoff mode_size) in
        lw st ~dst ~offset ~src:base

      let ld st bits =
        assert_mode_is_64 "C.ld";
        let dst = Bitset.reg3 ~lo:2 ~hi:4 bits in
        let base = Bitset.reg3 ~lo:7 ~hi:9 bits in
        let imm2 = Bitset.restrict ~lo:5 ~hi:6 bits in
        (* offset[7:6] *)
        let imm3 = Bitset.restrict ~lo:10 ~hi:12 bits in
        (* offset[5:3] *)
        let bvoff = Bv.concat [ imm2; imm3 ] in
        let offset = scale_by 8 (Bv.extend_signed bvoff mode_size) in
        ld st ~dst ~offset ~src:base

      let li st ~dst ~imm5 ~imm1 =
        let src = Bitvector.zero (* x0 *) in
        let imm = Bv.concat [ imm1; imm5 ] in
        let _, dba = addi st ~src ~dst ~imm in
        ( Printf.sprintf "li %s,%s" (reg_name dst)
            (Z.to_string (Bv.signed_of imm)),
          dba )

      let addi16sp st ~imm5 ~imm1 =
        L.debug "addi16sp";
        let bv5 = imm5 and bv1 = imm1 in
        let nzimm =
          Bitvector.concat
            [
              bv1;
              Bitvector.extract bv5 { lo = 1; hi = 2 };
              cut bv5 3;
              cut bv5 0;
              cut bv5 4;
            ]
        in
        assert (not @@ Bitvector.is_zeros nzimm);
        let imm = scale_by 16 (Bv.extend_signed nzimm mode_size) in
        let dst = Rar.(bvnum sp) in
        addi st ~src:dst ~dst ~imm

      let addi4spn st bits =
        L.debug "addi4spn";
        let bv8 = Bitset.restrict ~lo:5 ~hi:12 bits in
        (* bv8 is nzuimm[5:4|9:6|2|3] *)
        let bvoff =
          Bv.concat
            [
              Bv.extract bv8 { lo = 2; hi = 5 };
              Bv.extract bv8 { lo = 6; hi = 7 };
              cut bv8 0;
              cut bv8 1;
              Bv.zeros 2;
            ]
        in
        let imm = Bv.extend bvoff 12 in
        let dst = Bitset.reg3 ~lo:2 ~hi:4 bits in
        L.debug "rd is x%d" (Bv.to_int dst);
        addi st ~dst ~imm ~src:Rar.(bvnum sp)

      let addi st ~dst ~imm5 ~imm1 =
        let imm = Bv.append imm1 imm5 in
        addi st ~src:dst ~dst ~imm

      let addiw st ~dst ~imm5 ~imm1 =
        let imm = Bv.append imm1 imm5 in
        addiw st ~src:dst ~dst ~imm

      let andi st opcode =
        let imm1 = cut opcode 12 in
        (* imm[5] *)
        let imm5 = Bv.extract opcode { lo = 2; hi = 6 } in
        let dst = Bitset.reg3 ~lo:7 ~hi:9 opcode in
        let imm = Bv.append imm1 imm5 in
        andi st ~dst ~src:dst ~imm

      let lift_to_uncompressed bop st ~dst ~src =
        bop st ~dst ~src1:dst ~src2:src

      let add = lift_to_uncompressed add
      let sub = lift_to_uncompressed sub
      let addw = lift_to_uncompressed addw
      let subw = lift_to_uncompressed subw
      let logor = lift_to_uncompressed logor
      let logxor = lift_to_uncompressed logxor
      let logand = lift_to_uncompressed logand

      let lui st ~dst ~imm5 ~imm1 =
        assert (
          let v = Bitvector.value_of dst |> Z.to_int in
          v <> 0 && v <> 2);
        let offset = Bitvector.append imm1 imm5 in
        lui st ~dst ~offset

      let bz name branch_f st ~src ~imm3 ~imm5 =
        let bvoff =
          let open Basic_types in
          Bv.concat
            [
              cut imm3 2;
              Bv.extract imm5 { lo = 3; hi = 4 };
              cut imm5 0;
              Bv.extract imm3 { lo = 0; hi = 1 };
              Bv.extract imm5 { lo = 1; hi = 2 };
            ]
        in
        let offset = scale_by 2 bvoff in
        let _, dba = branch_f st ~src1:src ~src2:Rar.(bvnum zero) ~offset in
        ( Format.asprintf "%s %s,%a" name (reg_name src) Virtual_address.pp
            (jmp_offset st offset),
          dba )

      let beqz = bz "beqz" beq
      let bnez = bz "bnez" bne
    end
  end

  type inst = Unhandled of string | Unknown of string | Inst of Inst.t

  let unh s = Unhandled s
  let unk s = Unknown s
  let ins t = Inst t

  [@@@warning "-60"]

  module I = struct
    (* Basic instruction type for RISC-V are described page 23 of manual *)
    module Rtype = struct
      type t = {
        funct7 : Bitvector.t;
        rs2 : Bitvector.t;
        rs1 : Bitvector.t;
        funct3 : Bitvector.t;
        rd : Bitvector.t;
        opcode : Bitvector.t;
      }

      let slice bits =
        let open Bitset in
        let opcode = restrict ~lo:0 ~hi:6 bits
        and rd = restrict ~lo:7 ~hi:11 bits
        and funct3 = restrict ~lo:12 ~hi:14 bits
        and rs1 = restrict ~lo:15 ~hi:19 bits
        and rs2 = restrict ~lo:20 ~hi:24 bits
        and funct7 = restrict ~lo:25 ~hi:31 bits in
        { funct7; rd; funct3; opcode; rs1; rs2 }

      let apply lift_f st opcode =
        let s = slice opcode in
        let dst = s.rd and src1 = s.rs1 and src2 = s.rs2 in
        let mnemonic, dba = lift_f st ~dst ~src1 ~src2 in
        Inst.create ~dba ~mnemonic ~opcode

      let sub = apply Lift.sub
      let add = apply Lift.add
      let logxor = apply Lift.logxor
      let logor = apply Lift.logor
      let logand = apply Lift.logand
      let addw = apply Lift.addw
      let subw = apply Lift.subw
      let slt = apply Lift.slt
      let sltu = apply Lift.sltu
      let sll = apply Lift.sll
      let srl = apply Lift.srl
      let sra = apply Lift.sra
      let sllw = apply Lift.sllw
      let srlw = apply Lift.srlw
      let sraw = apply Lift.sraw
    end

    module Itype = struct
      type t = {
        opcode : Bv.t;
        rd : Bv.t;
        funct3 : Bv.t;
        rs1 : Bv.t;
        imm12 : Bv.t;
      }

      let restrict bits =
        let open Bitset in
        let opcode = restrict ~lo:0 ~hi:6 bits
        and rd = restrict ~lo:7 ~hi:11 bits
        and funct3 = restrict ~lo:12 ~hi:14 bits
        and rs1 = restrict ~lo:15 ~hi:19 bits
        and imm12 = restrict ~lo:20 ~hi:31 bits in
        { rd; opcode; funct3; rs1; imm12 }

      let lift_aux lifter st opcode =
        let s = restrict opcode in
        let dst = s.rd in
        let src = s.rs1 in
        let imm = s.imm12 in
        let mnemonic, dba = lifter st ~dst ~src ~imm in
        Inst.create ~dba ~opcode ~mnemonic

      let addi = lift_aux Lift.addi
      let addiw = lift_aux Lift.addiw
      let slti = lift_aux Lift.slti
      let sltiu = lift_aux Lift.sltiu
      let andi = lift_aux Lift.andi
      let ori = lift_aux Lift.ori
      let xori = lift_aux Lift.xori

      let apply_off lift_f st opcode =
        let s = restrict opcode in
        let dst = s.rd and src = s.rs1 and offset = s.imm12 in
        lift_f st ~dst ~src ~offset

      let ld = apply_off Lift.ld
      let lw = apply_off Lift.lw
      let lwu = apply_off Lift.lwu
      let lh = apply_off Lift.lh
      let lb = apply_off Lift.lb
      let lhu = apply_off Lift.lhu
      let lbu = apply_off Lift.lbu
      let jalr = apply_off (Lift.jalr ~instr_size:4)

      (* Shift with immediates *)
      let apply_shift name lift_f st ~shamt ~dst ~src ~opcode =
        let dba = lift_f st ~dst ~src ~shamt in
        let mnemonic =
          Printf.sprintf "%s %s,%s,%s" name (reg_name dst) (reg_name src)
            (Z.to_string (Bitvector.value_of shamt))
        in
        Inst.create ~mnemonic ~dba ~opcode

      (** Spilts the 12 bit immediate into:
       - shamt (5-6 bits) imm[4:0] or imm[5:0] depending on 32 or 64 bit mode
       - imm[10] - used to determine right shift operation
       asserts all other bits are 0*)
      let get_shamt force_32 imm =
        let shamt_size = shift_size force_32 in
        (* Invalid shift operation if imm[11,9-shamt_size+1] not zero*)
        assert (Bv.is_zero (Bitset.restrict ~lo:11 ~hi:11 imm));
        assert (Bv.is_zeros (Bitset.restrict ~lo:(shamt_size + 1) ~hi:9 imm));
        ( Bitset.restrict ~lo:0 ~hi:shamt_size imm,
          Bitset.restrict ~lo:10 ~hi:10 imm )

      (** shift left logical with immediate, is_word is for 64bit word shifts *)
      let slli is_word st bits =
        let s = restrict bits in
        let shamt, imm10 = get_shamt is_word s.imm12 in
        assert (Bv.is_zero imm10);
        let name, lift_f =
          if is_word then ("slliw", Lift.slliw) else ("slli", Lift.slli)
        in
        apply_shift name lift_f st ~shamt ~dst:s.rd ~src:s.rs1 ~opcode:bits

      (** shift right with immediate, logical or arithmetic *)
      let srxi is_word st opcode =
        let s = restrict opcode in
        let shamt, imm10 = get_shamt is_word s.imm12 in
        let name, lift_f =
          match (Bitvector.is_zero imm10, is_word) with
          | true, false -> ("srli", Lift.srli)
          | false, false -> ("srai", Lift.srai)
          | true, true -> ("srliw", Lift.srliw)
          | false, true -> ("sraiw", Lift.sraiw)
        in
        apply_shift name lift_f st ~shamt ~opcode ~dst:s.rd ~src:s.rs1
    end

    module Stype = struct
      type t = {
        opcode : Bv.t;
        imm5 : Bv.t;
        funct3 : Bv.t;
        rs1 : Bv.t;
        rs2 : Bv.t;
        imm7 : Bv.t;
      }

      let restrict bits =
        let open Bitset in
        let opcode = restrict ~lo:0 ~hi:6 bits
        and imm5 = restrict ~lo:7 ~hi:11 bits
        and funct3 = restrict ~lo:12 ~hi:14 bits
        and rs1 = restrict ~lo:15 ~hi:19 bits
        and rs2 = restrict ~lo:20 ~hi:24 bits
        and imm7 = restrict ~lo:25 ~hi:31 bits in
        { imm7; imm5; funct3; opcode; rs1; rs2 }

      let branch lift st opcode =
        let s = restrict opcode in
        let bv7 = s.imm7 and bv5 = s.imm5 in
        let offset =
          let open Basic_types in
          Bv.concat
            [
              cut bv7 6;
              cut bv5 0;
              Bv.extract bv7 { lo = 0; hi = 5 };
              Bv.extract bv5 { lo = 1; hi = 4 };
              Bv.zero;
              (* is that what the sentence "in multiple of 2" means p.17 ? *)
            ]
        in
        let src1 = s.rs1 and src2 = s.rs2 in
        let mnemonic, dba = lift st ~src1 ~src2 ~offset in
        Inst.create ~dba ~mnemonic ~opcode

      let beq = branch Lift.beq
      let bne = branch Lift.bne
      let blt = branch Lift.blt
      let bge = branch Lift.bge
      let bltu = branch Lift.bltu
      let bgeu = branch Lift.bgeu

      let store store_f st opcode =
        let s = restrict opcode in
        let offset = Bv.append s.imm7 s.imm5 in
        let base = s.rs1 and src = s.rs2 in
        store_f st ~offset ~base ~src

      (** Store instructions (8 bit, 16 bit, 32 bit, 64 bit) *)
      let sb = store Lift.sb

      let sh = store Lift.sh
      let sw = store Lift.sw
      let sd = store Lift.sd
    end

    module Utype = struct
      type t = { opcode : Bv.t; rd : Bv.t; imm20 : Bv.t }

      let restrict bits =
        let open Bitset in
        let opcode = restrict ~lo:0 ~hi:6 bits
        and rd = restrict ~lo:7 ~hi:11 bits
        and imm20 = restrict ~lo:12 ~hi:31 bits in
        { opcode; rd; imm20 }

      let apply lift_f st opcode =
        let s = restrict opcode in
        (* Is offset a good name here since mnemonics add unsigned immediates ? *)
        let dst = s.rd and offset = s.imm20 in
        lift_f st ~dst ~offset

      let lui = apply Lift.lui
      let auipc = apply Lift.auipc
    end

    module Jtype = struct
      type t = { opcode : Bv.t; rd : Bv.t; imm20 : Bv.t }

      let restrict bits =
        let open Bitset in
        let opcode = restrict ~lo:0 ~hi:6 bits
        and rd = restrict ~lo:7 ~hi:11 bits
        and imm20 = restrict ~lo:12 ~hi:31 bits in
        { rd; imm20; opcode }

      let jal st bits =
        let s = restrict bits in
        let bvimm = s.imm20 in
        let bvoff =
          let open Basic_types in
          Bv.concat
            [
              cut bvimm 19;
              Bv.extract bvimm { lo = 0; hi = 7 };
              cut bvimm 8;
              Bv.extract bvimm { lo = 9; hi = 18 };
            ]
        in
        let offset = Bv.extend_signed bvoff mode_size |> scale_by 2 in
        Lift.jal st ~dst:s.rd ~offset
    end

    (* RV32M/RV64M Standard Extension *)
    module M = struct
      let mul = Rtype.apply Lift.mul
      let mulh = Rtype.apply Lift.mulh
      let mulhu = Rtype.apply Lift.mulhu
      let mulhsu = Rtype.apply Lift.mulhsu
      let mulw = Rtype.apply Lift.mulw
      let div = Rtype.apply Lift.div
      let divu = Rtype.apply Lift.divu
      let rem = Rtype.apply Lift.rem
      let remu = Rtype.apply Lift.remu
      let divw = Rtype.apply Lift.divw
      let divuw = Rtype.apply Lift.divuw
      let remw = Rtype.apply Lift.remw
      let remuw = Rtype.apply Lift.remuw
    end

    (* Compressed instruction format *)
    module C = struct
      let shift name shift_f st opcode =
        let dst = Bitset.reg3 ~lo:7 ~hi:9 opcode in
        let shamt5 = Bitset.restrict ~lo:2 ~hi:6 opcode in
        let shamt1 = Bitset.restrict ~lo:12 ~hi:12 opcode in
        if Riscv_arch.Mode.is_m32 mode then assert (Bv.is_zero shamt1);
        let shamt = Bv.append shamt1 shamt5 in
        assert (not (Bv.is_zeros shamt));
        let dba = shift_f st ~dst ~shamt in
        let mnemonic =
          Printf.sprintf "%s %s,%s,%s" name (reg_name dst) (reg_name dst)
            (Z.to_string (Bv.value_of shamt))
        in
        Inst.create ~dba ~mnemonic ~opcode

      let srli = shift "srli" Lift.C.srli
      let srai = shift "srai" Lift.C.srai
      let slli = shift "slli" Lift.C.slli

      let c0 st opcode =
        let funct3 = uint @@ Bitset.restrict ~lo:13 ~hi:15 opcode in
        match funct3 with
        | 0 ->
            let rd = Bitset.reg3 ~lo:2 ~hi:4 opcode in
            let nzuimm = Bitset.restrict ~lo:5 ~hi:12 opcode in
            if Bv.is_zeros rd && Bv.is_zeros nzuimm then
              unh "Illegal instruction"
            else
              let mnemonic, dba = Lift.C.addi4spn st opcode in
              ins @@ Inst.create ~mnemonic ~dba ~opcode
        | 1 -> unh "C.fld"
        | 2 ->
            let mnemonic, dba = Lift.C.lw st opcode in
            ins @@ Inst.create ~mnemonic ~dba ~opcode
        | 3 ->
            if Riscv_arch.Mode.is_m32 mode then unh "C.flw"
            else
              let mnemonic, dba = Lift.C.ld st opcode in
              ins @@ Inst.create ~mnemonic ~dba ~opcode
        | 4 -> unh "C.reserved"
        | 5 -> if Riscv_arch.Mode.is_m128 mode then unh "C.sq" else unh "C.fsd"
        | 6 ->
            let mnemonic, dba = Lift.C.sw st opcode in
            ins @@ Inst.create ~opcode ~mnemonic ~dba
        | 7 ->
            if Riscv_arch.Mode.is_m32 mode then unh "C.fsw"
            else
              let mnemonic, dba = Lift.C.sd st opcode in
              ins @@ Inst.create ~opcode ~mnemonic ~dba
        | _ -> assert false
      (* no 3 opcode value should reach here *)

      let c1_arith st opcode =
        (* opcode is 1 and funct3 is 4 *)
        let funct2 = uint @@ Bitset.restrict ~lo:10 ~hi:11 opcode in
        match funct2 with
        | 0 -> ins @@ srli st opcode
        | 1 -> ins @@ srai st opcode
        | 2 ->
            let mnemonic, dba = Lift.C.andi st opcode in
            ins @@ Inst.create ~opcode ~dba ~mnemonic
        | 3 -> (
            let funct1 = uint @@ Bitset.restrict ~lo:12 ~hi:12 opcode in
            let funct2 = uint @@ Bitset.restrict ~lo:5 ~hi:6 opcode in
            let dst = Bitset.reg3 ~lo:7 ~hi:9 opcode in
            let src = Bitset.reg3 ~lo:2 ~hi:4 opcode in
            if funct1 = 0 then
              let mnemonic, dba =
                match funct2 with
                | 0 -> Lift.C.sub st ~dst ~src
                | 1 -> Lift.C.logxor st ~dst ~src
                | 2 -> Lift.C.logor st ~dst ~src
                | 3 -> Lift.C.logand st ~dst ~src
                | _ -> assert false
              in
              ins @@ Inst.create ~opcode ~dba ~mnemonic
            else
              match funct2 with
              | 0 ->
                  let mnemonic, dba = Lift.C.subw st ~dst ~src in
                  ins @@ Inst.create ~opcode ~dba ~mnemonic
              | 1 ->
                  let mnemonic, dba = Lift.C.addw st ~dst ~src in
                  ins @@ Inst.create ~opcode ~dba ~mnemonic
              | 2 -> unh "reserved"
              | 3 -> unh "reserved"
              | _ -> assert false)
        | _ -> assert false

      let bz bz_lift opcode =
        let imm5 = Bitset.restrict ~lo:2 ~hi:6 opcode in
        let imm3 = Bitset.restrict ~lo:10 ~hi:12 opcode in
        let src = Bitset.reg3 ~lo:7 ~hi:9 opcode in
        let mnemonic, dba = bz_lift ~src ~imm3 ~imm5 in
        ins @@ Inst.create ~mnemonic ~dba ~opcode

      let c1 st opcode =
        let funct3 = uint @@ Bitset.restrict ~lo:13 ~hi:15 opcode in
        match funct3 with
        | 0 ->
            let r = Bitset.restrict ~lo:7 ~hi:11 opcode in
            let nzuimm5 = Bitset.restrict ~lo:2 ~hi:6 opcode in
            let nzuimm1 = Bitset.restrict ~lo:12 ~hi:12 opcode in
            if Bv.is_zeros r && Bv.is_zeros nzuimm1 && Bv.is_zeros nzuimm5 then
              ins @@ Inst.create ~dba:(Lift.nop st) ~mnemonic:"nop" ~opcode
            else
              let mnemonic, dba =
                Lift.C.addi st ~dst:r ~imm5:nzuimm5 ~imm1:nzuimm1
              in
              ins @@ Inst.create ~dba ~mnemonic ~opcode
        | 1 ->
            if Riscv_arch.Mode.is_m32 mode then
              let offset = Bitset.restrict ~lo:2 ~hi:12 opcode in
              let mnemonic, dba = Lift.C.jal st ~offset in
              ins @@ Inst.create ~mnemonic ~dba ~opcode
            else
              let dst = Bitset.restrict ~lo:7 ~hi:11 opcode in
              let imm5 = Bitset.restrict ~lo:2 ~hi:6 opcode in
              let imm1 = Bitset.restrict ~lo:12 ~hi:12 opcode in
              let mnemonic, dba = Lift.C.addiw st ~dst ~imm5 ~imm1 in
              ins @@ Inst.create ~mnemonic ~dba ~opcode
        | 2 ->
            let rd = Bitset.restrict ~lo:7 ~hi:11 opcode in
            let imm5 = Bitset.restrict ~lo:2 ~hi:6 opcode in
            let imm1 = Bitset.restrict ~lo:12 ~hi:12 opcode in
            let mnemonic, dba = Lift.C.li st ~dst:rd ~imm5 ~imm1 in
            ins @@ Inst.create ~dba ~mnemonic ~opcode
        | 3 ->
            let rd = Bitset.restrict ~lo:7 ~hi:11 opcode in
            let imm5 = Bitset.restrict ~lo:2 ~hi:6 opcode in
            let imm1 = Bitset.restrict ~lo:12 ~hi:12 opcode in
            assert (not @@ Bv.is_zeros rd);
            let mnemonic, dba =
              if uint rd = 2 then Lift.C.addi16sp st ~imm1 ~imm5
              else Lift.C.lui st ~dst:rd ~imm1 ~imm5
            in
            ins @@ Inst.create ~dba ~mnemonic ~opcode
        | 4 -> c1_arith st opcode
        | 5 ->
            let offset = Bitset.restrict ~lo:2 ~hi:12 opcode in
            let mnemonic, dba = Lift.C.j st ~offset in
            ins @@ Inst.create ~mnemonic ~dba ~opcode
        | 6 -> bz (Lift.C.beqz st) opcode
        | 7 -> bz (Lift.C.bnez st) opcode
        | _ -> assert false
      (* no 3 opcode value should reach here *)

      let c2 st opcode =
        let _opcode = 0x2 in
        let funct3 = Bitset.restrict ~lo:13 ~hi:15 opcode in
        match uint funct3 with
        | 0 -> ins @@ slli st opcode
        | 1 -> unh "C.fldsp"
        | 2 ->
            let dst = Bitset.restrict ~lo:7 ~hi:11 opcode in
            let off5 = Bitset.restrict ~lo:2 ~hi:6 opcode in
            let off1 = Bitset.restrict ~lo:12 ~hi:12 opcode in
            let mnemonic, dba = Lift.C.lwsp st ~dst ~off1 ~off5 in
            ins @@ Inst.create ~opcode ~mnemonic ~dba
        | 3 ->
            if Riscv_arch.Mode.is_m32 mode then unh "C.flwsp"
            else
              let dst = Bitset.restrict ~lo:7 ~hi:11 opcode in
              let off5 = Bitset.restrict ~lo:2 ~hi:6 opcode in
              let off1 = Bitset.restrict ~lo:12 ~hi:12 opcode in
              let mnemonic, dba = Lift.C.ldsp st ~dst ~off1 ~off5 in
              ins @@ Inst.create ~opcode ~mnemonic ~dba
        | 4 -> (
            let funct4 = Bitset.restrict ~lo:12 ~hi:15 opcode in
            match uint funct4 with
            | 0x8 ->
                let rs2 = Bitset.restrict ~lo:2 ~hi:6 opcode in
                let rx = Bitset.restrict ~lo:7 ~hi:11 opcode in
                let mnemonic, dba =
                  if Bv.is_zeros rs2 then Lift.jr st rx
                  else Lift.mov st ~dst:rx ~src:rs2
                in
                ins @@ Inst.create ~opcode ~mnemonic ~dba
            | 0x9 ->
                let r1 = Bitset.restrict ~lo:7 ~hi:11 opcode in
                let r2 = Bitset.restrict ~lo:2 ~hi:6 opcode in
                if Bv.is_zeros r2 then
                  if Bv.is_zeros r1 then unh "C.ebreak"
                  else
                    let mnemonic, dba = Lift.C.jalr st ~src:r1 in
                    ins @@ Inst.create ~opcode ~mnemonic ~dba
                else
                  let mnemonic, dba = Lift.C.add st ~src:r2 ~dst:r1 in
                  ins @@ Inst.create ~opcode ~mnemonic ~dba
            | _ -> assert false)
        | 5 -> unh "C.fsdsp/C.sqps"
        | 6 ->
            let imm6 = Bitset.restrict ~lo:7 ~hi:12 opcode in
            let src = Bitset.restrict ~lo:2 ~hi:6 opcode in
            let mnemonic, dba = Lift.C.swsp st ~imm6 ~src in
            ins @@ Inst.create ~opcode ~mnemonic ~dba
        | 7 ->
            if Riscv_arch.Mode.is_m32 mode then unh "C.fswsp"
            else
              let imm6 = Bitset.restrict ~lo:7 ~hi:12 opcode in
              let src = Bitset.restrict ~lo:2 ~hi:6 opcode in
              let mnemonic, dba = Lift.C.sdsp st ~imm6 ~src in
              ins @@ Inst.create ~opcode ~mnemonic ~dba
        | _ -> assert false
    end
  end

  let is_compressed bits =
    let open Interval in
    let bv = Bv.extract bits { lo = 0; hi = 1 } in
    let v = uint bv in
    v <> 3

  module Uncompressed = struct
    (* *)
    module P = Print_utils

    let funct3 opcode = uint (Bitset.restrict ~lo:12 ~hi:14 opcode)
    let funct7 opcode = uint (Bitset.restrict ~lo:25 ~hi:31 opcode)

    (* Instructions with opcode 0x13 (0b0010011) are I type
       (arithmetic with immediate) *)
    let lift_0x13 st bits =
      match funct3 bits with
      | 0 -> ins @@ I.Itype.addi st bits
      | 1 -> ins @@ I.Itype.slli false st bits
      | 2 -> ins @@ I.Itype.slti st bits
      | 3 -> ins @@ I.Itype.sltiu st bits
      | 4 -> ins @@ I.Itype.xori st bits
      | 5 -> ins @@ I.Itype.srxi false st bits
      | 6 -> ins @@ I.Itype.ori st bits
      | 7 -> ins @@ I.Itype.andi st bits
      | _ -> assert false

    (* Instruction with opcode 0x1b (0b011011) are I type in RV64I
       32bit addition with immediate *)
    let lift_0x1b st bits =
      match funct3 bits with
      | 0 -> ins @@ I.Itype.addiw st bits
      | 1 -> ins @@ I.Itype.slli true st bits
      | 5 -> ins @@ I.Itype.srxi true st bits
      | _ -> assert false

    (* Instructions with opcode 0x23 (0b0100011) are S type
       (memory stores M[rs1 + imm] <- rs2) *)
    let lift_0x23 st bv =
      let mnemonic, dba =
        match funct3 bv with
        | 0 -> I.Stype.sb st bv
        | 1 -> I.Stype.sh st bv
        | 2 -> I.Stype.sw st bv
        | 3 ->
            assert_mode_is_64 "sd";
            I.Stype.sd st bv
        | _ -> assert false
      in
      ins @@ Inst.create ~opcode:bv ~mnemonic ~dba

    (* Instructions with opcode 0x33 (0b0110011) are R type (or M extension)
       register arithmetic *)
    let lift_0x33 st opcode =
      let funct7 = funct7 opcode in
      ins
      @@
      match funct3 opcode with
      | 0 -> (
          match funct7 with
          | 0 -> I.Rtype.add st opcode
          | 1 -> I.M.mul st opcode
          | 0x20 -> I.Rtype.sub st opcode
          | _ -> assert false)
      | 1 -> if funct7 = 0 then I.Rtype.sll st opcode else I.M.mulh st opcode
      | 2 -> if funct7 = 0 then I.Rtype.slt st opcode else I.M.mulhsu st opcode
      | 3 -> if funct7 = 0 then I.Rtype.sltu st opcode else I.M.mulhu st opcode
      | 4 -> if funct7 = 0 then I.Rtype.logxor st opcode else I.M.div st opcode
      | 5 -> (
          match funct7 with
          | 0 -> I.Rtype.srl st opcode
          | 1 -> I.M.divu st opcode
          | 0x20 -> I.Rtype.sra st opcode
          | _ -> assert false)
      | 6 -> if funct7 = 0 then I.Rtype.logor st opcode else I.M.rem st opcode
      | 7 -> if funct7 = 0 then I.Rtype.logand st opcode else I.M.remu st opcode
      | _ -> assert false

    (* Instructions with opcode 0x3b (0b0111011) are R type (or M extension)
       register arithmetic on 32bit words in 64 bit mode *)
    let lift_0x3b st opcode =
      let funct7 = funct7 opcode in
      ins
      @@
      match funct3 opcode with
      | 0 -> (
          match funct7 with
          | 0 -> I.Rtype.addw st opcode
          | 1 -> I.M.mulw st opcode
          | 0x20 -> I.Rtype.subw st opcode
          | _ -> assert false)
      | 1 ->
          assert (funct7 = 0);
          I.Rtype.sllw st opcode
      | 4 ->
          assert (funct7 = 1);
          I.M.divw st opcode
      | 5 -> (
          match funct7 with
          | 0 -> I.Rtype.srlw st opcode
          | 0x20 -> I.Rtype.sraw st opcode
          | 1 -> I.M.divuw st opcode
          | _ -> assert false)
      | 6 ->
          assert (funct7 = 1);
          I.M.remw st opcode
      | 7 ->
          assert (funct7 = 1);
          I.M.remuw st opcode
      | _ -> assert false

    (* Instructions with opcode 0x3 (0b0000011) are I type
       Memory load rd <- M[rs1 + imm] *)
    let lift_0x3 st opcode =
      let ret (mnemonic, dba) = ins @@ Inst.create ~opcode ~mnemonic ~dba in
      match funct3 opcode with
      | 0 -> ret @@ I.Itype.lb st opcode
      | 1 -> ret @@ I.Itype.lh st opcode
      | 2 -> ret @@ I.Itype.lw st opcode
      | 4 -> ret @@ I.Itype.lbu st opcode
      | 5 -> ret @@ I.Itype.lhu st opcode
      (* RV64I *)
      | 3 -> ret @@ I.Itype.ld st opcode
      | 6 -> ret @@ I.Itype.lwu st opcode
      | _ -> assert false

    (* Instructions with opcode 0x63 (0b1100011) are B type
       Conditionnal jumps (if rs1 ?? rs2 then PC = PC + imm) *)
    let lift_0x63 st opcode =
      match funct3 opcode with
      | 0 -> I.Stype.beq st opcode
      | 1 -> I.Stype.bne st opcode
      | 4 -> I.Stype.blt st opcode
      | 5 -> I.Stype.bge st opcode
      | 6 -> I.Stype.bltu st opcode
      | 7 -> I.Stype.bgeu st opcode
      | _ -> assert false

    let fetch_opcode_operator bits = uint @@ Bitset.restrict ~lo:0 ~hi:6 bits

    let lift st opcode =
      let opc_op = fetch_opcode_operator opcode in
      L.debug "Opcode operator %x" opc_op;
      match opc_op with
      | 3 -> lift_0x3 st opcode
      | 0x0f -> unh "fence"
      | 0x73 -> unh "ecal/ebreak/csrr(w|s|c|/unimp...)"
      | 0x23 -> lift_0x23 st opcode
      | 0x13 -> lift_0x13 st opcode
      | 0x33 -> lift_0x33 st opcode
      | 0x1b -> lift_0x1b st opcode
      | 0x3b -> lift_0x3b st opcode
      | 0x17 ->
          let mnemonic, dba = I.Utype.auipc st opcode in
          ins @@ Inst.create ~mnemonic ~dba ~opcode
      | 0x37 ->
          let mnemonic, dba = I.Utype.lui st opcode in
          ins @@ Inst.create ~mnemonic ~dba ~opcode
      | 0x63 -> ins @@ lift_0x63 st opcode
      | 0x6f ->
          let mnemonic, dba = I.Jtype.jal st opcode in
          ins @@ Inst.create ~mnemonic ~dba ~opcode
      | 0x67 ->
          let mnemonic, dba = I.Itype.jalr st opcode in
          ins @@ Inst.create ~mnemonic ~dba ~opcode
      | _ -> unk @@ Format.asprintf "Unknown opcode %a" Bitvector.pp_hex opcode
  end

  module Compressed = struct
    let lift st bits =
      let quadrant = uint @@ Bitset.restrict bits ~lo:0 ~hi:1 in
      L.debug "Compressed instruction, quadrant %d" quadrant;
      match quadrant with
      | 0 -> I.C.c0 st bits
      | 1 -> I.C.c1 st bits
      | 2 -> I.C.c2 st bits
      | 3 -> fail "Reserved for uncompressed opcode"
      | n ->
          let msg = Printf.sprintf "Unexpected quadrant value %d" n in
          fail msg
  end

  let lift st bits =
    if is_compressed bits then Compressed.lift (D_status.switch16 st) bits
    else Uncompressed.lift (D_status.switch32 st) bits

  let decode reader vaddr =
    let size, bits =
      (* TODO: will break if I am wrong -- cursor is always well positioned *)

      (* First we position the cursor at the right address.
         Then we peek a 16 bits value to check out if the opcode is compressed or
         not.
         If so, just read 16 bits and decode the compressed opcode.
         Otherwise, the opcode is 32 bits long. Decode an uncompressed opcode.
      *)
      (* let vaddr = Virtual_address.to_int vaddr in
       * let c = Lreader.get_pos reader in
       * let displ = vaddr - c in
       * if displ >= 0 then Lreader.advance reader displ
       * else Lreader.rewind reader (-displ); *)
      let bits = Lreader.Peek.bv16 reader in
      L.debug "RISC-V peeked bits %a" Bv.pp_hex bits;
      if is_compressed bits then (2, Lreader.Read.bv16 reader)
      else (4, Lreader.Read.bv32 reader)
    in
    L.debug "Decoding RISC-V bits %a (%d)" Bv.pp_hex bits (uint bits);
    let st = D_status.create vaddr in
    let s = lift st bits in
    match s with
    | Unhandled mnemonic_hint ->
        let opcode = Bitvector.to_hexstring bits in
        let mnemonic = Mnemonic.unsupported ~mnemonic_hint () in
        let ginst = Instruction.Generic.create size opcode mnemonic in
        L.debug "unhandled %s" mnemonic_hint;
        (ginst, Dhunk.empty)
    | Unknown _ ->
        let opcode = Bitvector.to_hexstring bits in
        let mnemonic = Mnemonic.unknown in
        let ginst = Instruction.Generic.create size opcode mnemonic in
        L.debug "unknown %s" opcode;
        (ginst, Dhunk.empty)
    | Inst i ->
        let open Inst in
        let opcode = Bitvector.to_hexstring i.opcode in
        let mnemonic = Mnemonic.supported i.mnemonic Format.pp_print_string in
        let ginst = Instruction.Generic.create size opcode mnemonic in
        let dhunk = D.Block.to_dba i.dba in
        L.debug "@[<hov>Dba:@ %a@]@]" Dhunk.pp dhunk;
        (ginst, dhunk)
end

(* For a simplistic interface, only expose the decode function *)

module Reg32 : Riscv_arch.RegisterSize = struct
  let size = Riscv_arch.Mode.m32
end

module Riscv32_to_Dba = Riscv_to_Dba (Reg32)

let decode_32 = Riscv32_to_Dba.decode

module Reg64 : Riscv_arch.RegisterSize = struct
  let size = Riscv_arch.Mode.m64
end

module Riscv64_to_Dba = Riscv_to_Dba (Reg64)

let decode_64 = Riscv64_to_Dba.decode
