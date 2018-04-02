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

 (* Copyright (c) 2005, Regents of the University of California
 * All rights reserved.
 *
 * Author: Adam Chlipala
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * - Neither the name of the University of California, Berkeley nor the names of
 *   its contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 *
 * Modified for BINSEC
*)

(* Parsing opcodes *)

open X86Types
open X86Util


exception Parse of string


let imode = function
  | `M32 -> 32
  | `M16 -> 16


let of_mode = function
  | `M32 -> `M32
  | `M16 -> `M16


let nCcs = 16
(* Number of distinct condition codes
 * Lookup http://x86.renejeschke.de/html/file_module_x86_id_146.html
 *)


let nRegs32 = 8
(** Number of 32-bit registers *)


let sign_extend n =
  if n >= 0x80 && n <= 0xff then n - 0xff - 1
  else if n >= 0x8000 && n <= 0xffff then n - 0xffff - 1
  else if n >= 0x80000000 && n <= 0xffffffff then n - 0xffffffff - 1
  else n


let with_xmm_simd mode f =
  let mm, simd_size =
    match mode with
    | `M32 -> MM, S64
    | `M16 -> XMM, S128
  in f mm simd_size


let read_byte_as_int64 lr =
  Lreader.Read.u8 lr |> Int64.of_int


let select_reader = function
  | 8 -> Lreader.Read.u8
  | 16 -> Lreader.Read.u16
  | 32 -> Lreader.Read.u32
  | _ -> assert false


(* A 32 bits displacement.
   We must be careful to stay inside bounds. *)
let displacement lr rel =
  let vcursor = Lreader.get_virtual_cursor lr in
  Disasm_options.Logger.debug ~level:4 "displacement from %x of %x" vcursor rel;
  (vcursor + rel) land 0xffffffff


let signed_displacement lr rel =
  displacement lr (sign_extend rel)

let bytes_to_opcode_string bytes =
  let b = Buffer.create (3 * List.length bytes) in
  let rec loop = function
    | [] -> assert false
    | [by] ->
      Buffer.add_string b (Format.sprintf "%02x" by);
      Buffer.contents b
    | by :: bytes ->
      Buffer.add_string b (Format.sprintf "%02x " by);
      loop bytes
  in loop bytes


let shift_or_rotate_from_spare
    ~shift ~rotate spare =
  match spare with
  | 0 -> rotate Rol
  | 1 -> rotate Ror
  | 2 -> rotate Rcl
  | 3 -> rotate Rcr
  | 4
  | 6 -> shift Shl
  (* According to http://ref.x86asm.net/geek.html#xC0_6
                 C0 /6 is an alias for C0 /4.
     This is the same for the other cases.
     Simple guess that explains this fact:
     - 6 stands for Sal which is actually the same as Shl (i.e. 4).
  *)
  | 5 -> shift Shr
  | 7 -> shift Sar
  | _ -> assert false (* All cases have been treated *)


exception Unknown_byte


(* Some instructions only read a mod/rm after the opcode.
   When they are uhandled, use this function.
*)
let modrm_unhandled address_mode lr =
  ignore(X86Util.read_modrm address_mode lr);
  Unhandled


let read_0f_3a mode address_mode lr =
  let b3 = Lreader.Read.u8 lr in
  match b3 with
  | 0x0F ->
    let src, spare = read_modrm_xmm address_mode lr in
    let imm = Lreader.Read.u8 lr in
    with_xmm_simd mode
      (fun mm simd -> Palignr (mm, simd, Reg (int_to_xmm_reg spare), src, imm)
      )
  | 0x63 -> (* 66 0F 3A 63 /r imm8 : PCMPISTRI xmm1, xmm2/m128, imm8 *)
    let _ = X86Util.read_modrm address_mode lr in
    let _imm8 = Lreader.Read.u8 lr in
    Unhandled
  | _ -> raise Unknown_byte


let read_d8 address_mode lr =
  match Lreader.Peek.u8 lr with
  | 0xd1  (* FCOM *)
  | 0xd9 -> (* FCOMP *)
    Lreader.advance lr 1; Unhandled
  | _ -> modrm_unhandled address_mode lr


let read_d9 address_mode lr =
  match Lreader.Peek.u8 lr with
  | 0xd0 -> (* FNOP *) Lreader.advance lr 1; Nop
  | 0xc9
  | 0xe0 | 0xe1 | 0xe4 | 0xe5 | 0xe8 | 0xe9
  | 0xea | 0xeb | 0xec | 0xed | 0xee ->
    Lreader.advance lr 1; Unhandled
  | n when n >= 0xf0 && n <= 0xff ->
    Lreader.advance lr 1; Unhandled
  | _ ->
    ignore (X86Util.read_modrm address_mode lr);
    Unhandled


let read_db address_mode lr =
  let byte = Lreader.Peek.u8 lr in
  let _, spare = X86Util.read_modrm address_mode lr in
  if spare = 4 then
    match byte with
    | 0xe0 | 0xe1 | 0xe2 | 0xe3 | 0xe4 -> Unhandled
    | _ -> Bad
  else Unhandled


let read_da address_mode lr =
  let byte = Lreader.Peek.u8 lr in
  let _, spare = X86Util.read_modrm address_mode lr in
  if spare = 5 && byte = 0xe9 then
    (* fucompp : just extracted as special case but behavior is the same *)
    Unhandled
  else Unhandled (* all other cases are*)


let read_2bytes_opcode mode address_mode rep lr =
  let byte = Lreader.Read.u8 lr in
  assert (byte = 0x0F);
  let on_xmm_simd = with_xmm_simd mode in
  let b2 = Lreader.Read.u8 lr in
  match b2 with
  | 0x03 ->
    let dst, spare = read_modrm address_mode lr in
    Lsl (of_mode mode, Reg (int_to_reg32 spare), dst)
  | 0x12 ->
    begin match rep, mode with
      | _, `M16 ->
        let src, spare = read_modrm_xmm address_mode lr in
        Movlpd (S64, Reg (int_to_xmm_reg spare), src)
      | NoRep,`M32 ->
        let src, spare = read_modrm_xmm address_mode lr in
        (match src with
        | Reg _ -> Movhlps (S128, Reg (int_to_xmm_reg spare), src)
        | Address _ -> Movlps (S64, Reg (int_to_xmm_reg spare), src)
        | Imm _ -> assert false
        )
      | RepNE, `M32 ->
        let src, spare = read_modrm_xmm address_mode lr in
        Movddup (S64, Reg (int_to_xmm_reg spare), src)

      | RepE, `M32 ->
        let src, spare = read_modrm_xmm address_mode lr in
        Movsldup (S128, Reg (int_to_xmm_reg spare), src)
    end
  | 0x13 ->
    begin match mode with
      | `M32 ->
        let dst, spare = read_modrm_xmm address_mode lr in
        Movlps (S64, dst, Reg (int_to_xmm_reg spare))
      | `M16 ->
        let dst, spare = read_modrm_xmm address_mode lr in
        Movlpd (S64, dst, Reg (int_to_xmm_reg spare))
    end
  | 0x16 ->
    begin match rep, mode with
      | _, `M16 ->
        let src, spare = read_modrm_xmm address_mode lr in
        Movhpd (S64, Reg (int_to_xmm_reg spare), src)
      | RepE, `M32 ->
        let src, spare = read_modrm_xmm address_mode lr in
        Movshdup (S128, Reg (int_to_xmm_reg spare), src)

      | _,`M32 ->
        let src, spare = read_modrm_xmm address_mode lr in
        match src with
        | Reg _     -> Movlhps (S128, Reg (int_to_xmm_reg spare), src)
        | Address _ -> Movhps  (S64 , Reg (int_to_xmm_reg spare), src)
        | Imm _ -> assert false
    end
  | 0x28 ->
    begin match mode with
      | `M32 ->
        let src, spare = read_modrm_xmm address_mode lr in
        Movaps (S128,Reg (int_to_xmm_reg spare), src)

      | `M16 ->
        let src, spare = read_modrm_xmm address_mode lr in
        Movaps (S128, Reg (int_to_xmm_reg spare), src)

    end
  | 0x29 ->
    begin match mode with
      | `M16 ->
        let dst, spare = read_modrm_xmm address_mode lr in
        let instr = Movaps (S128, dst, Reg (int_to_xmm_reg spare)) in
        instr
      | `M32 ->
        let dst, spare = read_modrm_xmm address_mode lr in
        let instr = Movaps (S128, dst, Reg (int_to_xmm_reg spare)) in
        instr
    end

  | 0x38 ->
    let b3 = Lreader.Read.u8 lr in
    let fail_msg = Format.asprintf "Unknown opcode: 0f 38 %02x" b3 in
    begin match b3 with
      | 0x17 ->
        let src, spare = read_modrm_xmm address_mode lr in
        Ptest (XMM, S128, Reg (int_to_xmm_reg spare), src)
      | 0x3E ->
        begin match mode with
          | `M16 ->
            let src, spare = read_modrm_xmm address_mode lr in
            Pmaxuw (XMM, S128, Reg (int_to_xmm_reg spare), src)
          | _ -> raise (Parse fail_msg)
        end
      | 0x3F ->
        begin match mode with
          | `M16 ->
            let src, spare = read_modrm_xmm address_mode lr in
            Pmaxud (XMM, S128, Reg (int_to_xmm_reg spare), src)
          | _ -> raise (Parse fail_msg)
        end
      | 0x00 | 0xf0 | 0xf1 ->
        let _ = X86Util.read_modrm address_mode lr in
        Unhandled
      | _ ->
        raise (Parse fail_msg)
    end


  | 0x3A -> read_0f_3a mode address_mode lr

  | b2 when (b2 >= 0x40 && b2 < 0x40 + nCcs) ->
    let ncc = b2 - 0x40 in
    begin match ncc with
      | 10 | 11 -> Unhandled
      | _ ->
        let dst, spare = read_modrm address_mode lr in
        CMovcc (of_mode mode, int_to_cc (b2 - 0x40), Reg (int_to_reg32 spare), dst)
    end

  | 0x60 ->
    let src, spare = read_modrm_xmm address_mode lr in
     on_xmm_simd
      (fun xmm simd -> Punpcklbw (xmm, simd, Reg (int_to_xmm_reg spare), src) )


  | 0x61 ->
    let src, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun xmm simd -> Punpcklwd (xmm, simd, Reg (int_to_xmm_reg spare), src))

  | 0x62 ->
    let src, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun xmm simd -> Punpckldq (xmm, simd, Reg (int_to_xmm_reg spare), src) )

  | 0x64 ->
    let src, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun mm simd_size ->
         Pcmpgtb (mm, simd_size, Reg (int_to_xmm_reg spare), src))

  | 0x65 ->
    let src, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun mm simd_size ->
         Pcmpgtw (mm, simd_size, Reg (int_to_xmm_reg spare), src))


  | 0x66 ->
    let src, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun mm simdsize ->
         Pcmpgtd (mm, simdsize, Reg (int_to_xmm_reg spare), src))

  | 0x6E ->
    let src, spare = read_modrm address_mode lr in
    on_xmm_simd
        (fun mm _ -> Movd(mm, Left, Reg (int_to_xmm_reg spare), src))

  | 0x6F ->
    let src, spare = read_modrm_xmm address_mode lr in
    begin
      match mode with
      | `M16 -> MovdQA (XMM, S128, Reg (int_to_xmm_reg spare), src)
      | `M32 ->
        match rep with
        | RepE -> MovdQU (XMM, S128, Reg (int_to_xmm_reg spare), src)
        | _ -> MovQ (MM, S64, Reg (int_to_xmm_reg spare), src)
    end

  | 0x70 ->
    let src, spare = read_modrm_xmm address_mode lr in
    let imm = Lreader.Read.u8 lr in
    let xmm_reg = int_to_xmm_reg spare in
    begin
      match mode with
      | `M32 ->
        begin
          match rep with
          | NoRep -> Pshufw  (MM,  S64,  xmm_reg, src, imm)
          | RepNE -> Pshuflw (XMM, S128, xmm_reg, src, imm)
          | RepE  -> Pshufhw (XMM, S128, xmm_reg, src, imm)
        end

      | `M16 -> Pshufd (XMM, S128, xmm_reg, src, imm)
    end

  | 0x71 ->
    let gop, spare = read_modrm_xmm address_mode lr in
    let imm = read_byte_as_int64 lr in
    on_xmm_simd
      (fun mm simd_size ->
         match spare with
         | 2 -> Psrlw (mm, simd_size, gop, Imm imm)
         | 4 -> Psraw (mm, simd_size, gop, Imm imm)
         | 6 -> Psllw (mm, simd_size, gop, Imm imm)
         | _ -> Bad)

  | 0x72 ->
    let gop, spare = read_modrm_xmm address_mode lr in
    let imm = read_byte_as_int64 lr in
    on_xmm_simd
      (fun mm simd_size ->
        match spare with
        | 2 -> Psrld (mm, simd_size, gop, Imm imm)
        | 4 -> Psrad (mm, simd_size, gop, Imm imm)
        | 6 -> Pslld (mm, simd_size, gop, Imm imm)
        | _ -> Bad)

  | 0x73 ->
    let gop, spare = read_modrm_xmm address_mode lr in
    let imm = read_byte_as_int64 lr in
    begin match mode with
      | `M32 ->
        begin match spare with
          | 2 -> Psrlq (MM, S64, gop, Imm imm)
          | 6 -> Psllq (MM, S64, gop, Imm imm)
          | _ -> Bad
        end
      | `M16 ->
        begin match spare with
          | 2 -> Psrlq (XMM, S128, gop, Imm imm)
          | 3 -> Psrldq (gop, Int64.to_int imm)
          | 6 -> Psllq (XMM, S128, gop, Imm imm)
          | 7 -> Pslldq (gop, Int64.to_int imm)
          | _ -> Bad
        end
    end

  | 0x74 ->
    let src, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun mm simd_size -> Pcmpeqb (mm, simd_size, Reg (int_to_xmm_reg spare), src))

  | 0x75 ->
    let src, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun mm simd_size -> Pcmpeqw (mm, simd_size, Reg (int_to_xmm_reg spare), src))

  | 0x76 ->
    let src, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun mm simd_size -> Pcmpeqd (mm, simd_size, Reg (int_to_xmm_reg spare), src))

  | 0x7E ->
    begin match mode with
      | `M16 ->
        let src, spare = read_modrm address_mode lr in
        Movd (XMM, Right, Reg (int_to_xmm_reg spare), src)
      | `M32 ->
        match rep with
        | RepE ->
          let src, spare = read_modrm_xmm address_mode lr in
          MovQ (XMM, S64, Reg (int_to_xmm_reg spare), src)
        | _ ->
          let src, spare = read_modrm address_mode lr in
          Movd (MM, Right, Reg (int_to_xmm_reg spare), src)
    end

  | 0x7F ->
    let dst, spare = read_modrm_xmm address_mode lr in
    begin
      match mode with
      | `M16 -> MovdQA (XMM, S128, dst, Reg (int_to_xmm_reg spare))
      | `M32 ->
        match rep with
        | RepE -> MovdQU (XMM, S128, dst, Reg (int_to_xmm_reg spare))
        | _    -> MovQ (MM, S64, dst, Reg (int_to_xmm_reg spare))
    end

  | b2 when b2 >= 0x80 && b2 < 0x80 + nCcs ->
    (* FIXME: Operand size attribute could force that to read a word instead of a
       double-word.
       CHECK: if it corresponds to [mode] argument.
    *)
    let rel = Lreader.Read.u32 lr in
    Jcc (int_to_cc (b2 - 0x80), Int64.of_int (displacement lr rel))

  | b2 when (b2 >= 0x90 && b2 < 0x90 + nCcs) ->
    let dst, _spare = read_rm8_with_spare lr in
    let ncc = b2 - 0x90 in
    begin match ncc with
      | 10 | 11 -> Unhandled
      | _ -> SetCc (int_to_cc (b2 - 0x90), dst)
    end

  | 0xA0 -> PushS FS
  | 0xA1 -> PopS FS

  | 0xA2 -> Unhandled

  | 0xA3 ->
    let src, spare = read_modrm address_mode lr in
    Bt (of_mode mode, src, Reg(int_to_reg32 spare))

  | 0xA4 ->
    let dst, spare = read_modrm address_mode lr in
    let imm = read_byte_as_int64 lr in
    Shiftd (of_mode mode, Shld, dst, Reg (int_to_reg32 spare), Imm imm)

  | 0xA5 ->
    let dst, spare = read_modrm address_mode lr in
    Shiftd (of_mode mode, Shld, dst, Reg (int_to_reg32 spare), Reg CL)

  | 0xA8 -> PushS GS
  | 0xA9 -> PopS GS

  | 0xAB ->
    let src, spare = read_modrm address_mode lr in
    Bts (of_mode mode, src, Reg (int_to_reg32 spare))

  | 0xAC ->
    let dst, spare = read_modrm address_mode lr in
    let imm = read_byte_as_int64 lr in
    Shiftd (of_mode mode, Shrd, dst, Reg (int_to_reg32 spare), Imm imm)

  | 0xAD ->
    let dst, spare = read_modrm address_mode lr in
    Shiftd (of_mode mode, Shrd, dst, Reg (int_to_reg32 spare), Reg CL)

  | 0xAF ->
    let src, spare = read_modrm address_mode lr in
    IMul2 (of_mode mode, Reg (int_to_reg32 spare), src)

  | 0xB1 ->
    let dst, spare = read_modrm address_mode lr in
    CmpXchg (of_mode mode, dst, Reg (int_to_reg32 spare))

  | 0xB6 ->
    let src, spare = read_rm8_with_spare lr in
    Movzx (of_mode mode, int_to_reg32 spare, src)

  | 0xB7 ->
    let src, spare = read_rm16_with_spare lr in
    Movzx16 (of_mode mode, int_to_reg32 spare, src)

  | 0xBA ->
    let gop, spare = read_modrm address_mode lr in
    begin match spare with
      | 4 ->
        let imm = read_byte_as_int64 lr in
        Bt (of_mode mode, gop, Imm imm)
      | 5 ->
        let imm = read_byte_as_int64 lr in
        Bts (of_mode mode, gop, Imm imm)
      | 6 ->
        let imm = read_byte_as_int64 lr in
        Btr (of_mode mode, gop, Imm imm)
      | _ -> Bad
    end
  | 0xBC ->
    let src, spare = read_modrm address_mode lr in
    Bsf (of_mode mode, int_to_reg32 spare, src)

  | 0xBD ->
    let src, spare = read_modrm address_mode lr in
    Bsr (of_mode mode, int_to_reg32 spare, src)

  | 0xBE ->
    let src, spare = read_rm8_with_spare lr in
    Movsx (of_mode mode, int_to_reg32 spare, src)

  | 0xBF ->
    let src, spare = read_rm16_with_spare lr in
    Movsx16 (of_mode mode, int_to_reg32 spare, src)

  | 0xC0 ->
    let dst, spare = read_modrm address_mode lr in
    Xadd (`M8, dst, Reg (int_to_reg32 spare))
  | 0xC1 ->
    let dst, spare = read_modrm address_mode lr in
    Xadd (of_mode mode, dst, Reg (int_to_reg32 spare))

  | 0xC7 ->
    let src, spare = read_modrm_xmm address_mode lr in
    begin match spare with
      | 1 -> CmpXchg8b (MM, S64, src)
      | _ -> Bad
    end

  | b2 when (b2 >= 0xC8 && b2 < 0xC8 + nRegs32) ->
    Bswap (of_mode mode, int_to_reg32 (b2 - 0xC8))

  | 0xD1 ->
    let gop, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun mm simd_size ->
         Psrlw (mm, simd_size, Reg (int_to_xmm_reg spare), gop))


  | 0xD2 ->
    let gop, spare = read_modrm_xmm address_mode lr in
    begin match mode with
      | `M32 ->
        Psrld (MM, S64, Reg (int_to_xmm_reg spare), gop)
      | `M16 ->
        Psrld (XMM, S128, Reg (int_to_xmm_reg spare), gop)
    end

  | 0xD3 ->
    let gop, spare = read_modrm_xmm address_mode lr in
    begin match mode with
      | `M32 ->
        Psrlq (MM, S64, Reg (int_to_xmm_reg spare), gop)
      | `M16 ->
        Psrlq (XMM, S128, Reg (int_to_xmm_reg spare), gop)
    end

  | 0xD6 -> (
      match mode with
      | `M16 ->
        let src, spare = read_modrm_xmm address_mode lr in
        MovQ (XMM, S64, Reg (int_to_xmm_reg spare), src)
      | _ ->
        let message = Format.sprintf "Unknown opcode: 0F %02x" b2 in
        raise (Parse message)
    )

  | 0xD7 ->
    let src, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun mm simd -> PmovMSKB (mm, simd, Reg (int_to_reg32 spare), src) )

  | 0xDA ->
    let src, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun mm simd -> Pminub (mm, simd, Reg (int_to_xmm_reg spare), src) )

  | 0xDB ->
    let src, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun mm simd -> Pand (mm, simd, Reg (int_to_xmm_reg spare), src) )


  | 0xDE ->
    let src, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun mm simd -> Pmaxub (mm, simd, Reg (int_to_xmm_reg spare), src))

  | 0xDF ->
    let src, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun mm simd -> Pandn (mm, simd, Reg (int_to_xmm_reg spare), src))

  | 0xE1 ->
    let gop, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun xmm simd -> Psraw (xmm, simd, Reg (int_to_xmm_reg spare), gop))

  | 0xE2 ->
    let gop, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun xmm simd -> Psrad (xmm, simd, Reg (int_to_xmm_reg spare), gop))


  | 0xE7 ->
    let dst, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun xmm simd -> Movntq (xmm, simd, dst, Reg (int_to_xmm_reg spare)))

  | 0xEB ->
    let src, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun xmm simd -> Por (xmm, simd, Reg (int_to_xmm_reg spare), src))

  | 0xEF ->
    let src, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun xmm simd -> Pxor (xmm, simd, Reg (int_to_xmm_reg spare), src))


  | 0xF1 ->
    let gop, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
        (fun mm simdsize ->
           Psllw (mm, simdsize, Reg (int_to_xmm_reg spare), gop))

  | 0xF2 ->
    let gop, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun xmm simd -> Pslld (xmm, simd, Reg (int_to_xmm_reg spare), gop))

  | 0xF3 ->
    let gop, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun xmm simd -> Psllq (xmm, simd, Reg (int_to_xmm_reg spare), gop))

  | 0xF8 ->
    let src, spare = read_modrm_xmm address_mode lr in
    on_xmm_simd
      (fun xmm simd -> Psubb (xmm, simd, Reg (int_to_xmm_reg spare), src))


  | 0x01 | 0x02 | 0x10 | 0x11 | 0x14 | 0x15 | 0x17 | 0x18
  | 0x20 | 0x21 | 0x22 | 0x23 | 0x2b | 0x2e | 0x2f
  | 0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55 | 0x56 | 0x57
  | 0x58 | 0x59 | 0x5A | 0x5B | 0x5C | 0x5D | 0x5E | 0x5F
  | 0x63 | 0x67 | 0x68 | 0x69 | 0x6A | 0x6B
  | 0x78 | 0x79
  | 0xae
  | 0xb2 | 0xb3 | 0xb4 | 0xb5 | 0xb8 | 0xb9 | 0xe0 ->
    let _ = X86Util.read_modrm address_mode lr in
    Unhandled
  | 0x05 | 0x06 | 0x07 | 0x08 | 0x09 | 0x0A | 0x0B | 0x0C
  | 0x0D | 0x0E | 0x0F
  | 0x24 | 0x26
  | 0x30 | 0x31 | 0x32 | 0x33 | 0x34 | 0x35 | 0x36 | 0x37
  | 0x77
  | 0xaa
  | 0xff -> Unhandled
  | 0xbb ->
    let _ = X86Util.read_modrm address_mode lr in
    let _ = Lreader.Read.u8 lr in
    Unhandled

  | b ->
    let message = Format.asprintf "Unknown opcode: 0f %02x" b in
    raise (Parse message)

(* End of read_2bytes_opcode *)



let read lr =
  (* lr is a cursor *)
 (*  Logger.debug "@[EIP %x,%d(%x) : %a@]" (fst eip) (snd eip) (snd eip / 8)
  *  Bits.pp bits; *)
  Disasm_options.Logger.debug ~level:4 "[x86 decode] %a" Lreader.pp lr;
  let sreg = ref None in
  let rep = ref NoRep in
  (*   let succ_eip (bv_addr, off_addr) = succ bv_addr, succ off_addr in *)

  let rec aux_read_instr mode address_mode lr =
    let byte = Lreader.Peek.u8 lr in
    (*    let byte, bits = Bits.read_uint bits 8 in *)
    if byte = 0x0F then
      read_2bytes_opcode mode address_mode !rep lr
    else

      let unknown () = Format.sprintf "Unknown instruction : %02x" byte in

      let arith_to_rm aop =
        let dst, spare = read_modrm address_mode lr in
        Arith (of_mode mode, aop, dst, Reg (int_to_reg32 spare)) in

      let arith_from_rm aop =
        let src, spare = read_modrm address_mode lr in
        Arith (of_mode mode, aop, Reg (int_to_reg32 spare), src) in

      match Lreader.Read.u8 lr with
      | 0x00 ->
        let dst, spare = read_modrm address_mode lr in
        Arith (`M8, Add, dst, Reg (int_to_reg32 spare))

      | 0x01 -> arith_to_rm Add
      | 0x02 -> (* added *)
        let src, spare = read_modrm address_mode lr in
        Arith (`M8, Add, Reg (int_to_reg32 spare), src)

      | 0x03 -> arith_from_rm Add

      | 0x04 -> (* added *)
        let imm = read_byte_as_int64 lr in
        Arith (`M8, Add, Reg EAX, Imm imm)

      | 0x05 ->
        let imm = select_reader (imode mode) lr in
        Arith (of_mode mode, Add, Reg EAX, Imm (Int64.of_int imm))

      | 0x06 -> PushS ES
      | 0x07 -> PopS ES
      | 0x08 -> (* added *)
        let dst, spare = read_modrm address_mode lr in
        Arith (`M8, Or, dst, Reg (int_to_reg32 spare))

      | 0x09 -> arith_to_rm Or
      | 0x0A -> (* added *)
        let src, spare = read_modrm address_mode lr in
        Arith (`M8, Or, Reg (int_to_reg32 spare), src)

      | 0x0B -> arith_from_rm Or
      | 0x0C -> (* added *)
        let imm = read_byte_as_int64 lr in
        Arith (`M8, Or, Reg EAX, Imm imm)

      | 0x0D -> (* added *)
        let imm = select_reader (imode mode) lr in
        Arith (of_mode mode, Or, Reg EAX, Imm (Int64.of_int imm))

      | 0x0E -> PushS CS
      | 0x10 -> (* added *)
        let dst, spare = read_modrm address_mode lr in
        Arith (`M8, Adc, dst, Reg (int_to_reg32 spare))

      | 0x11 -> arith_to_rm Adc
      | 0x12 -> (* added *)
        let src, spare = read_modrm address_mode lr in
        Arith (`M8, Adc, Reg (int_to_reg32 spare), src)

      | 0x13 -> arith_from_rm Adc
      | 0x14 ->
        let imm = read_byte_as_int64 lr in
        Arith (`M8, Adc, Reg EAX, Imm imm)

      | 0x15 -> (* added *)
        let imm = select_reader (imode mode) lr in
        Arith (of_mode mode, Adc, Reg EAX, Imm (Int64.of_int imm))

      | 0x16 -> PushS SS
      | 0x17 -> PopS SS
      | 0x18 -> (* added *)
        let dst, spare = read_modrm address_mode lr in
        Arith (`M8, Sbb, dst, Reg (int_to_reg32 spare))

      | 0x19 -> arith_to_rm Sbb
      | 0x1A -> (* added *)
        let src, spare = read_modrm address_mode lr in
        Arith (`M8, Sbb, Reg (int_to_reg32 spare), src)

      | 0x1B -> arith_from_rm Sbb
      | 0x1C -> (* added *)
        let imm = read_byte_as_int64 lr in
        Arith (`M8, Sbb, Reg EAX, Imm imm)

      | 0x1D -> (* added *)
        let imm = select_reader (imode mode) lr in
        Arith (of_mode mode, Sbb, Reg EAX, Imm (Int64.of_int imm))

      | 0x1E -> PushS DS
      | 0x1F -> PopS DS
      | 0x20 -> (* added *)
        let dst, spare = read_modrm address_mode lr in
        Arith (`M8, And, dst, Reg (int_to_reg32 spare))

      | 0x21 -> arith_to_rm And
      | 0x22 -> (* added *)
        let src, spare = read_modrm address_mode lr in
        Arith (`M8, And, Reg (int_to_reg32 spare), src)

      | 0x23 -> arith_from_rm And

      | 0x24 -> (* added *)
        let imm = read_byte_as_int64 lr in
        Arith (`M8, And, Reg EAX, Imm imm)

      | 0x25 -> (* added *)
        let imm = select_reader (imode mode) lr in
        Arith (of_mode mode, And, Reg EAX, Imm (Int64.of_int imm))

      | 0x26 -> (* Design choice *)
        sreg := Some ES;
        aux_read_instr mode address_mode lr

      | 0x28 ->
        let dst, spare = read_modrm address_mode lr in
        Arith (`M8, Sub, dst, Reg (int_to_reg32 spare))

      | 0x29 -> arith_to_rm Sub
      | 0x2A -> (* added *)
        let src, spare = read_modrm address_mode lr in
        Arith (`M8, Sub, Reg (int_to_reg32 spare), src)

      | 0x2B -> arith_from_rm Sub
      | 0x2C -> (* added *)
        let imm = read_byte_as_int64 lr in
        Arith (`M8, Sub, Reg EAX, Imm imm)

      | 0x2D -> (* added *)
        let imm = select_reader (imode mode) lr in
        Arith (of_mode mode, Sub, Reg EAX, Imm (Int64.of_int imm))

      | 0x2E -> (* Design choice *)
        sreg := Some CS;
        aux_read_instr mode address_mode lr

      | 0x30 -> (* added *)
        let dst, spare = read_modrm address_mode lr in
        Arith (`M8, Xor, dst, Reg (int_to_reg32 spare))

      | 0x31 -> arith_to_rm Xor
      | 0x32 -> (* added *)
        let src, spare = read_modrm address_mode lr in
        Arith (`M8, Xor, Reg (int_to_reg32 spare), src)

      | 0x33 -> arith_from_rm Xor
      | 0x34 -> (* added *)
        let imm = read_byte_as_int64 lr in
        Arith (`M8, Xor, Reg EAX, Imm imm)

      | 0x35 ->
        let imm = select_reader (imode mode) lr in
        Arith (`M32, Xor, Reg EAX, Imm (Int64.of_int imm))

      | 0x36 ->
        sreg := Some SS;
        aux_read_instr mode address_mode lr
      | 0x38 ->
        let dst, spare = read_modrm address_mode lr in
        Cmp (`M8, dst, Reg (int_to_reg32 spare))

      | 0x39 ->
        let dst, spare = read_modrm address_mode lr in
        Cmp (of_mode mode, dst, Reg (int_to_reg32 spare))

      | 0x3A -> (* added *)
        let src, spare = read_modrm address_mode lr in
        Cmp (`M8, Reg (int_to_reg32 spare), src)

      | 0x3B ->
        let src, spare = read_modrm address_mode lr in
        Cmp (of_mode mode, Reg (int_to_reg32 spare), src)

      | 0x3C -> (* added *)
        let imm = read_byte_as_int64 lr in
        Cmp (`M8, Reg EAX, Imm imm)

      | 0x3D ->
        let imm = select_reader (imode mode) lr in
        Cmp (of_mode mode, Reg EAX, Imm (Int64.of_int imm))

      | 0x3E -> (* Design choice *)
        sreg := Some DS;
        aux_read_instr mode address_mode lr
      | byte when (byte >= 0x40 && byte < 0x40 + nRegs32) ->
        Inc (of_mode mode, Reg (int_to_reg32 (byte - 0x40)))

      | byte when (byte >= 0x48 && byte < 0x48 + nRegs32) ->
        Dec (of_mode mode, Reg (int_to_reg32 (byte - 0x48)))

      | byte when (byte >= 0x50 && byte < 0x50 + nRegs32) ->
        Push (of_mode mode, Reg (int_to_reg32 (byte - 0x50)))

      | byte when (byte >= 0x58 && byte < 0x58 + nRegs32) ->
        Pop (of_mode mode, Reg (int_to_reg32 (byte - 0x58)))

      | 0x61 -> Popa (of_mode mode)
      | 0x60 -> PushA mode
      | 0x64 ->
        sreg := Some FS;
        aux_read_instr mode address_mode lr
      | 0x65 ->
        sreg := Some GS;
        aux_read_instr mode address_mode lr
      | 0x66 ->
        let mode = `M16 in
        aux_read_instr mode address_mode lr
      | 0x67 ->
        let address_mode = A16 in
        aux_read_instr mode address_mode lr
      | 0x68 ->
        let imm = select_reader (imode mode) lr in
        Push (of_mode mode, Imm (Int64.of_int imm))
      | 0x69 -> (* added *)
        let src, spare = read_modrm address_mode lr in
        let imm = select_reader (imode mode) lr in
        IMul3 (of_mode mode, Reg (int_to_reg32 spare), src,
                           Imm (Int64.of_int imm))
      | 0x6A ->
        let imm = read_byte_as_int64 lr in
        Push (of_mode mode,Imm imm)

      | 0x6B ->
        let src, spare = read_modrm address_mode lr in
        let imm = read_byte_as_int64 lr in
        IMul3 (of_mode mode, Reg (int_to_reg32 spare), src, Imm imm)

      | 0x6e | 0x6f ->
        (* outs/outsb/outsw/outsd from dx *)
        Unhandled

      | byte when (byte >= 0x70 && byte < 0x70 + nCcs) ->
        let rel8 = Lreader.Read.u8 lr in
        let v = Int64.of_int (signed_displacement lr rel8) in
        Jcc (int_to_cc (byte - 0x70), v)

      | 0x80 ->
        let dst, spare = read_modrm address_mode lr in
        let disp = read_byte_as_int64 lr in
        begin
          match spare with
          | 7 -> Cmp (`M8, dst, Imm disp)
          | _ ->
            let aop = int_to_arith_op spare in
            Arith (`M8, aop, dst, Imm disp)
        end
      | 0x81 ->
        let dst, spare = read_modrm address_mode lr in
        let imode = imode mode in
        let disp = select_reader imode lr in
        begin
          let disp = Int64.of_int disp in
          match spare with
          | 7 -> Cmp (of_mode mode, dst, Imm disp)
          | _ ->
            let aop = int_to_arith_op spare in
            Arith (of_mode mode, aop, dst, Imm disp)
        end
      | 0x82 ->
        let dst, spare = read_modrm address_mode lr in
        let disp = read_byte_as_int64 lr in
        begin match spare with
          | 7 -> Cmp (`M8, dst, Imm disp)
          | _ -> Arith (`M8, int_to_arith_op spare, dst, Imm disp)
        end

      | 0x83 ->
        let dst, spare = read_modrm address_mode lr in
        let disp64 = Lreader.Read.u8 lr |> sign_extend |> Int64.of_int in
        if spare = 7 then Cmp (of_mode mode, dst, Imm disp64)
        else Arith (of_mode mode, int_to_arith_op spare, dst, Imm disp64)

      | 0x84 ->
        let dst, spare = read_modrm address_mode lr in
        Test (`M8, dst, Reg (int_to_reg32 spare))
      | 0x85 ->
        let dst, spare = read_modrm address_mode lr in
        Test (of_mode mode, dst, Reg (int_to_reg32 spare))
      | 0x86 ->
        let src, spare = read_modrm address_mode lr in
        Xchg (`M8, Reg (int_to_reg32 spare), src)

      | 0x87 ->
        let src, spare = read_modrm address_mode lr in
        Xchg (of_mode mode, Reg (int_to_reg32 spare), src)
      | 0x88 ->
        let dst, spare = read_modrm address_mode lr in
        Mov (`M8, dst, Reg (int_to_reg32 spare))

      | 0x89 ->
        let dst, spare = read_modrm address_mode lr in
        Mov (`M32, dst, Reg (int_to_reg32 spare))

      | 0x8A ->
        let src, spare = read_modrm address_mode lr in
        Mov (`M8, Reg (int_to_reg32 spare), src)
      | 0x8B ->
        let src, spare = read_modrm address_mode lr in
        Mov (of_mode mode, Reg (int_to_reg32 spare), src)

      | 0x8c ->
        let dst, spare = read_rm16_with_spare lr in
        MovSegRight (dst, int_to_segment_reg spare)

      | 0x8D ->
        let gop, spare = read_modrm address_mode lr in
        begin match gop with
          | Address addr -> Lea (of_mode mode, int_to_reg32 spare, addr)
          | _ -> raise (Parse "Weird LEA operand")
        end

      | 0x8e ->
        let src, spare = read_rm16_with_spare lr in
        MovSegLeft (int_to_segment_reg spare, src)

      | 0x8F -> (* added *)
        let dst, spare = read_modrm address_mode lr in
        begin match spare with
          | 0 -> Pop (of_mode mode, dst)
          | _ -> raise (Parse (unknown ()))
        end

      | 0x90 -> Nop

      | byte when (byte > 0x90 && byte < 0x90 + nRegs32) ->
        Xchg (of_mode mode, Reg (int_to_reg32 (byte - 0x90)), Reg EAX)

      | 0x98 -> CBW mode
      | 0x99 -> CWD mode
      | 0x9B -> Wait

      | 0x9C -> Pushfd (of_mode mode)
      | 0x9D -> Popfd (of_mode mode)
      | 0x9F -> Lahf
      | 0xA0 ->
        let imm = select_reader (imode mode) lr in
        let addr = X86Util.mk_address ~address_mode imm in
        Mov (`M8, Reg EAX, addr)

      | 0xA1 ->
        let imm = select_reader (imode mode) lr in
        let addr = X86Util.mk_address ~address_mode imm in
        Mov (of_mode mode, Reg EAX, addr)

      | 0xA2 ->
        let imm = select_reader (imode mode) lr in
        let addr = X86Util.mk_address ~address_mode imm in
        Mov (of_mode mode, addr, Reg EAX)

      | 0xA3 ->
        let imm = Lreader.Read.u32 lr in
        let addr = X86Util.mk_address ~address_mode imm in
        Mov (of_mode mode, addr, Reg EAX)

      | 0xA4 -> Movs `M8
      | 0xA5 -> Movs (of_mode mode)
      | 0xA6 -> Cmps `M8
      | 0xA7 -> Cmps `M32

      | 0xA8 ->
        let imm = read_byte_as_int64 lr in
        Test (`M8, Reg EAX, Imm imm)

      | 0xA9 -> (* added *)
        let imm = select_reader (imode mode) lr in
        Test ((of_mode mode), Reg EAX, Imm (Int64.of_int imm))

      | 0xAA -> Stos `M8
      | 0xAB -> Stos (of_mode mode)
      | 0xAC -> Lods `M8
      | 0xAD -> Lods (of_mode mode)
      | 0xAE -> Scas `M8
      | 0xAF -> Scas (of_mode mode)

      | byte when (byte >= 0xB0 && byte < 0xB0 + nRegs32) ->
        let imm = read_byte_as_int64 lr in
        Mov (`M8, Reg (int_to_reg32 (byte - 0xB0)), Imm imm)

      | byte when (byte >= 0xB8 && byte < 0xB8 + nRegs32) ->
        let imm = select_reader (imode mode) lr in
        Mov (of_mode mode, Reg (int_to_reg32 (byte - 0xB8)),
             Imm (Int64.of_int imm))

      | 0xC0 ->
        let dst, spare = read_modrm address_mode lr in
        let imm64 = Imm (read_byte_as_int64 lr) in
        let rotate rot_type = Rotate (`M8, rot_type, dst, imm64)
        and shift shift_type = Shift (`M8, shift_type, dst, imm64) in
        shift_or_rotate_from_spare ~shift ~rotate spare

      | 0xC1 ->
        let dst, spare = read_modrm address_mode lr in
        let imm64 = Imm (read_byte_as_int64 lr) in
        let rotate rot_type = Rotate (of_mode mode, rot_type, dst, imm64)
        and shift shift_type = Shift (of_mode mode, shift_type, dst, imm64) in
        shift_or_rotate_from_spare ~shift ~rotate spare

      | 0xC2 ->
        let imm = Lreader.Read.u16 lr in
        Reti imm

      | 0xC3 -> Ret
      | 0xc4 (* les *)
      | 0xc5 (* lds *) ->  modrm_unhandled address_mode lr

      | 0xC6 ->
        let dst, spare = read_modrm address_mode lr in
        begin match spare with
          | 0 ->
            let imm = Lreader.Read.u8 lr in
            Mov (`M8, dst, Imm (Int64.of_int imm))
          | _ -> raise (Parse (unknown ()))
        end
      | 0xC7 ->
        let dst, spare = read_modrm address_mode lr in
        begin match spare with
          | 0 ->
            let imode = imode mode in
            let imm = select_reader imode lr in
            Mov (of_mode mode, dst, Imm (Int64.of_int imm))
          | _ -> raise (Parse (unknown ()))
        end

      | 0xc8 -> (* enter imm16 imm8 : make stack frame *)
        ignore (Lreader.Read.u16 lr);
        ignore (Lreader.Read.u8 lr);
        Unhandled
      | 0xC9 -> Leave
      | 0xCA ->
        let imm = Lreader.Read.u16 lr in
        Retfi imm
      | 0xcb ->  Retf
      | 0xcc -> (* int 3 *) Unhandled
      | 0xcd -> (* int imm8 *) ignore(Lreader.Read.u8 lr); Unhandled
      | 0xce | 0xcf -> (* into | iret/iretd *) Unhandled
      | 0xD0 ->
          let dst, spare = read_modrm address_mode lr in
        let shift shift_type = Shift (`M8, shift_type, dst, Imm Int64.one)
        and rotate rotate_type =
          Rotate (`M8, rotate_type, dst, Imm Int64.one) in
        shift_or_rotate_from_spare ~shift ~rotate spare

      | 0xD1 ->
        let dst, spare = read_modrm address_mode lr in
        let shift shift_type = Shift (of_mode mode, shift_type, dst, Imm Int64.one)
        and rotate rotate_type =
          Rotate (of_mode mode, rotate_type, dst, Imm Int64.one) in
        shift_or_rotate_from_spare ~shift ~rotate spare

      | 0xD2 ->
        let dst, spare = read_modrm address_mode lr in
        let rotate rotate_type = Rotate (`M8, rotate_type, dst, Reg CL)
        and shift shift_type = Shift (`M8, shift_type, dst, Reg CL) in
        shift_or_rotate_from_spare ~shift ~rotate spare

      | 0xD3 ->
        let dst, spare = read_modrm address_mode lr in
        let rotate rotate_type = Rotate (of_mode mode, rotate_type, dst, Reg CL)
        and shift shift_type = Shift (of_mode mode, shift_type, dst, Reg CL) in
        shift_or_rotate_from_spare ~shift ~rotate spare

      | 0xD4 ->
        let imm = Lreader.Read.u8 lr in Aam imm

      | 0xD5 ->
        let imm = Lreader.Read.u8 lr in Aad imm

      | 0xD6 -> Salc

      | 0xd7 -> (* xlat / xlatb *) Unhandled
      | 0xE0 ->
        let rel8 = Lreader.Read.u8 lr in
        let v = Int64.of_int (signed_displacement lr rel8) in
        Loopnz (mode, address_mode, v)

      | 0xE1 ->
        let rel8 = Lreader.Read.u8 lr in
        let v = Int64.of_int (signed_displacement lr rel8) in
        Loopz (mode, address_mode, v)

      | 0xE2 ->
        let rel8 = Lreader.Read.u8 lr in
        let v = Int64.of_int (signed_displacement lr rel8) in
        Loop (mode, address_mode, v)
      (* End loop *)

      | 0xE3 ->
        let rel8 = Lreader.Read.u8 lr in
        let v = Int64.of_int (signed_displacement lr rel8) in
        Jcxz (of_mode mode, v)

      | 0xE8 ->
        let rel32 = Lreader.Read.u32 lr in
        Call (Int64.of_int (displacement lr rel32))

      | 0xE9 ->
        (* FIXME: Can be 16/32 bits 3-333 *)
        let rel32 = Lreader.Read.u32 lr in
        Jmp (Int64.of_int (displacement lr rel32))

      | 0xea ->
        ignore (Lreader.Read.u16 lr);
        ignore(select_reader (imode mode) lr);
        Unhandled

      | 0xeb ->
        let rel8 = Lreader.Read.u8 lr in
        Jmp (Int64.of_int (signed_displacement lr rel8))

      | 0xF0 ->
        aux_read_instr mode address_mode lr

      | 0xF2 -> (* added *)
        let byte2 = Lreader.Read.u8 lr in
        begin match byte2 with
          | 0xC3 -> Ret
          | _ ->
            rep := RepNE;
            Lreader.rewind lr 1;
            aux_read_instr mode address_mode lr
        end
      | 0xF3 -> (* added *)
        begin match Lreader.Read.u8 lr with
          | 0xC3 -> Ret
          | _ ->
             Logger.debug "Rep set";
             rep := RepE;
             Lreader.rewind lr 1;
             (*Lreader.advance lr 1;*)
             aux_read_instr mode address_mode lr
        end

      | 0xF4 -> Halt
      | 0xF5 -> Cmc
      | 0xF6 ->
        (* Lookup http://ref.x86asm.net/coder32.html#xF6 *)
        let src, spare = read_modrm address_mode lr in
        begin match spare with
          | 0
          | 1 ->
            let v = Lreader.Read.u8 lr in
            Test (`M8, src, Imm (Int64.of_int v))
          | 2 -> Not (`M8, src)
          | 3 -> Neg (`M8, src)
          | 4 -> Mul (`M8, src)
          | 5 -> IMul (`M8, src)
          | 6 -> Div (`M8, src)
          | 7 -> IDiv (`M8, src)
          | _ -> assert false
        end

      | 0xF7 ->
        let src, spare = read_modrm address_mode lr in
        begin match spare with
          | 0
          | 1 ->
            let imm16_32 = select_reader (imode mode) lr in
            Test (of_mode mode, src, Imm (Int64.of_int imm16_32))
          | 2 -> Not (of_mode mode, src)
          | 3 -> Neg (of_mode mode, src)
          | 4 -> Mul (of_mode mode, src)
          | 5 -> IMul (of_mode mode, src)
          | 6 -> Div (of_mode mode, src)
          | 7 -> IDiv (of_mode mode, src)
          | _ -> assert false
        end

      | 0xf8 -> Clc
      | 0xf9 -> Stc
      | 0xfc -> Cld
      | 0xfd -> Std
      | 0xfe ->
        let gop, spare = read_modrm address_mode lr in
         begin match spare with
          | 0 -> Inc (`M8, gop)
          | 1 -> Dec (`M8, gop)
          | _ -> Bad
        end

      | 0xff ->
        let gop, spare = read_modrm address_mode lr in
        begin
          match spare with
          | 0 ->  Inc (of_mode mode, gop)
          | 1 ->  Dec (of_mode mode, gop)
          | 2 ->  DCall gop
          | 3
          | 5 ->  Unhandled
          | 4 ->  DJmp gop
          | 6 ->  Push (of_mode mode, gop)
          | _ ->  Bad
        end
      | 0x27 | 0x2F
      | 0x37 | 0x3f
      | 0x62 | 0x63 | 0x6c | 0x6d
      | 0x9a -> Unhandled
      | 0x9e -> Sahf
      | 0xd8 -> read_d8 address_mode lr
      | 0xd9 -> read_d9 address_mode lr
      | 0xdb -> read_db address_mode lr
      | 0xda -> read_da address_mode lr
      | 0xdc
      | 0xdd
      | 0xde
      | 0xdf -> modrm_unhandled address_mode lr

      | 0xec (* in al dx *)
      | 0xed (* in eax dx *)
      | 0xee (* out dx eal *)
      | 0xef (* out dx eax *)
      | 0xe4 | 0xe5 | 0xe6 | 0xe7
      | 0xf1 (* int1 / icebp *)
      | 0xfa (* cli : clear interrupt flags *)
      | 0xfb (* sti : set interrupt flags *)
        ->
        ignore(Lreader.Read.u8 lr); Unhandled
      | _ -> raise (Parse (unknown ()))
  in
  let initial_position = Lreader.get_virtual_cursor lr in
  let mode = `M32 in
  let address_mode = A32 in
  let mnemonic = aux_read_instr mode address_mode lr in
  Logger.debug ~level:3 "Get opcode slice [%x, %x[ for %a"
    initial_position (Lreader.get_virtual_cursor lr) X86pp.pp_instr mnemonic;
  let opcode =
    Lreader.get_slice lr initial_position (Lreader.get_virtual_cursor lr)
    |> bytes_to_opcode_string
  in
  let size = Lreader.get_virtual_cursor lr - initial_position in
  let instruction = X86Instruction.create size opcode mnemonic in
  instruction, !rep, !sreg
