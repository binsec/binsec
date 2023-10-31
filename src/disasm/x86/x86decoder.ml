(**************************************************************************)
(*  Copyright (c) 2005, Regents of the University of California           *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Author: Adam Chlipala                                                 *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*                                                                        *)
(*  - Redistributions of source code must retain the above copyright      *)
(*    notice, this list of conditions and the following disclaimer.       *)
(*  - Redistributions in binary form must reproduce the above copyright   *)
(*    notice, this list of conditions and the following disclaimer in     *)
(*    the documentation and/or other materials provided with the          *)
(*    distribution.                                                       *)
(*  - Neither the name of the University of California, Berkeley nor      *)
(*    the names of its contributors may be used to endorse or promote     *)
(*    products derived from this software without specific prior          *)
(*    written permission.                                                 *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE        *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,  *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER      *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT    *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN     *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE       *)
(*  POSSIBILITY OF SUCH DAMAGE.                                           *)
(*                                                                        *)
(*                                                                        *)
(*  Modified for BINSEC                                                   *)
(*                                                                        *)
(**************************************************************************)

(* Parsing opcodes *)

open X86Types
open X86Util
open X86_options

exception Decode_abort

type prefix = F2 | F3

let rep_of = function None | Some F2 -> NoRep | Some F3 -> Rep
let repcc_of = function None -> NoRep | Some F2 -> RepNE | Some F3 -> RepE
let abort () = raise Decode_abort
let imode = function `M32 -> 32 | `M16 -> 16
let of_mode = function `M32 -> `M32 | `M16 -> `M16
let nCcs = 16
(* Number of distinct condition codes
 * Lookup http://x86.renejeschke.de/html/file_module_x86_id_146.html
 *)

(** Number of 32-bit registers *)
let nRegs32 = 8

let sign_extend n =
  if n >= 0x80 && n <= 0xff then n - 0xff - 1
  else if n >= 0x8000 && n <= 0xffff then n - 0xffff - 1
  else if n >= 0x80000000 && n <= 0xffffffff then n - 0xffffffff - 1
  else n

let with_xmm_simd mode f =
  let mm, simd_size =
    match mode with `M32 -> (MM, S64) | `M16 -> (XMM, S128)
  in
  f mm simd_size

let read_byte_as_int64 lr = Lreader.Read.u8 lr |> sign_extend |> Int64.of_int

let select_reader = function
  | 8 -> Lreader.Read.u8
  | 16 -> Lreader.Read.u16
  | 32 ->
      fun c -> Int32.to_int (Lreader.Read.i32 c) land 0xffffffff
      (* TODO: unsafe on 32bit *)
  | _ -> assert false

(* A 32 bits displacement.
   We must be careful to stay inside bounds. *)
let displacement addr lr rel =
  let vcursor = addr + Lreader.get_pos lr in
  Logger.debug ~level:4 "displacement from %x of %x" vcursor rel;
  (vcursor + rel) land 0xffffffff

let signed_displacement addr lr rel = displacement addr lr (sign_extend rel)

let bytes_to_opcode_string =
  let lookup = "0123456789abcdef" in
  fun bytes ->
    let size = Bytes.length bytes in
    let b = Bytes.create ((3 * size) - 1) in
    Bytes.iteri
      (fun i c ->
        let c = Char.code c in
        let hi = c lsr 4 and lo = c land 0x0f in
        Bytes.set b (3 * i) (String.get lookup hi);
        Bytes.set b ((3 * i) + 1) (String.get lookup lo);
        if i + 1 != size then Bytes.set b ((3 * i) + 2) ' ')
      bytes;
    Bytes.unsafe_to_string b

let shift_or_rotate_from_spare ~shift ~rotate spare =
  match spare with
  | 0 -> rotate Rol
  | 1 -> rotate Ror
  | 2 -> rotate Rcl
  | 3 -> rotate Rcr
  | 4 | 6 -> shift Shl
  (* According to http://ref.x86asm.net/geek.html#xC0_6
                 C0 /6 is an alias for C0 /4.
     This is the same for the other cases.
     Simple guess that explains this fact:
     - 6 stands for Sal which is actually the same as Shl (i.e. 4).
  *)
  | 5 -> shift Shr
  | 7 -> shift Sar
  | _ -> assert false
(* All cases have been treated *)

(* Functions to deal with unsupported opcodes *)
let unsupported descr = Unsupported descr

let unsupported_modrm descr address_mode lr =
  ignore (read_modrm address_mode lr);
  Unsupported descr

let unsupported_modrm_xmm descr address_mode lr =
  ignore (read_modrm_xmm address_mode lr);
  Unsupported descr

let unsupported_imm descr size lr =
  Lreader.advance lr size;
  Unsupported descr

let unsupported_modrm_imm descr address_mode size lr =
  ignore (read_modrm address_mode lr);
  Lreader.advance lr size;
  Unsupported descr

let unsupported_m16 unsupported mode =
  match mode with `M16 -> unsupported | `M32 -> abort ()

let read_0f_01 mode address_mode lr =
  match Lreader.Read.u8 lr with
  | 0xc1 -> Unsupported "vmcall"
  | 0xc2 -> Unsupported "vmlaunch"
  | 0xc3 -> Unsupported "vmresume"
  | 0xc4 -> Unsupported "vmxoff"
  | 0xc8 -> Unsupported "monitor"
  | 0xc9 -> Unsupported "mwait"
  | 0xd0 -> Unsupported "xgetbv"
  | 0xd1 -> Unsupported "xsetbv"
  | 0xf8 -> Unsupported "swapgs"
  | 0xf9 -> Unsupported "rdtscp"
  | _ -> (
      Lreader.rewind lr 1;
      let genop, spare = read_modrm address_mode lr in
      match spare with
      | 0 -> Unsupported "sgdt"
      | 1 -> Unsupported "sidt"
      | 2 -> Lgdt (of_mode mode, genop)
      | 3 -> Lidt (of_mode mode, genop)
      | 4 -> Unsupported "smsw"
      | 6 -> Unsupported "lmsw"
      | 7 -> Unsupported "invlpg"
      | _ -> raise Decode_abort)

(* 3D now *)
let read_0f_0f mode address_mode lr =
  let src, spare = read_modrm_xmm address_mode lr in
  match Lreader.Read.u8 lr with
  | 0xb7 ->
      with_xmm_simd mode (fun xmm simd ->
          Pmulhrw (xmm, simd, Reg (int_to_xmm_reg spare), src))
  | 0xbf ->
      with_xmm_simd mode (fun xmm simd ->
          Pavgu (xmm, simd, Reg (int_to_xmm_reg spare), src, 8))
  | _ -> raise Decode_abort

let read_0f_38 mode address_mode lr =
  let b3 = Lreader.Read.u8 lr in
  let un_ssse3 () = unsupported_modrm "op from ssse3" address_mode lr in
  let un_m16 descr =
    unsupported_m16 (unsupported_modrm descr address_mode lr) mode
  in
  let sse41 = "op from sse41" in
  match b3 with
  | 0x4 ->
      let src, spare = read_modrm_xmm address_mode lr in
      with_xmm_simd mode (fun xmm simd ->
          Pmaddusbsw (xmm, simd, Reg (int_to_xmm_reg spare), src))
  | n when 0x00 <= n && n <= 0x0b -> un_ssse3 ()
  | 0x10 | 0x14 | 0x15 -> un_m16 sse41
  | 0x17 ->
      let src, spare = read_modrm_xmm address_mode lr in
      Ptest (XMM, S128, Reg (int_to_xmm_reg spare), src)
  | 0x1c | 0x1d | 0x1e -> un_ssse3 ()
  | n when 0x20 <= n && n <= 0x3d -> un_m16 sse41
  | 0x3e -> (
      match mode with
      | `M16 ->
          let src, spare = read_modrm_xmm address_mode lr in
          Pmaxu (XMM, S128, Reg (int_to_xmm_reg spare), src, 16)
      | `M32 -> abort ())
  | 0x3f -> (
      match mode with
      | `M16 ->
          let src, spare = read_modrm_xmm address_mode lr in
          Pmaxu (XMM, S128, Reg (int_to_xmm_reg spare), src, 32)
      | `M32 -> abort ())
  | 0x40 | 0x41 -> un_m16 sse41
  | 0x80 | 0x81 -> un_m16 "op from vmx"
  | 0xdb when mode = `M16 -> unsupported_modrm_xmm "aesimc" address_mode lr
  | 0xdc when mode = `M16 -> unsupported_modrm_xmm "aesenc" address_mode lr
  | 0xde when mode = `M16 -> unsupported_modrm_xmm "aesdec" address_mode lr
  | 0xdd when mode = `M16 -> unsupported_modrm_xmm "aesenclast" address_mode lr
  | 0xdf when mode = `M16 -> unsupported_modrm_xmm "aesdeclast" address_mode lr
  | 0xf0 -> unsupported_modrm "crc32 or movbe" address_mode lr
  | 0xf1 -> unsupported_modrm "crc32 or movbe" address_mode lr
  | _ -> abort ()

let read_0f_3a mode address_mode lr =
  let b3 = Lreader.Read.u8 lr in
  let un_imm_m16 descr =
    unsupported_m16 (unsupported_modrm_imm descr address_mode 1 lr) mode
  in
  match b3 with
  | 0x08 -> un_imm_m16 "roundps"
  | 0x09 -> un_imm_m16 "roundpd"
  | 0x0a -> un_imm_m16 "roundss"
  | 0x0b -> un_imm_m16 "roundsd"
  | 0x0c -> un_imm_m16 "blendps"
  | 0x0d -> un_imm_m16 "blendpd"
  | 0x0e -> un_imm_m16 "pblendw"
  | 0x0f ->
      let src, spare = read_modrm_xmm address_mode lr in
      let imm = Lreader.Read.u8 lr in
      with_xmm_simd mode (fun mm simd ->
          Palignr (mm, simd, Reg (int_to_xmm_reg spare), src, imm))
  | 0x14 -> un_imm_m16 "pextrb"
  | 0x15 -> un_imm_m16 "pextrw"
  | 0x16 -> un_imm_m16 "pextrd/q"
  | 0x17 -> un_imm_m16 "extractps"
  | 0x20 ->
      (* 66 0f 3a 20 /r imm8 : PINSRB xmm, m8/r32, imm8*)
      un_imm_m16 "pinsrb"
  | 0x21 -> un_imm_m16 "insertps"
  | 0x22 -> un_imm_m16 "pinsrd/q"
  | 0x40 -> un_imm_m16 "dpps"
  | 0x41 -> un_imm_m16 "dppd"
  | 0x42 -> un_imm_m16 "mpsadbw"
  | 0x44 ->
      let src, spare = read_modrm_xmm address_mode lr in
      let imm = Lreader.Read.u8 lr in
      with_xmm_simd mode (fun xmm simd ->
          Pclmulqdq (xmm, simd, Reg (int_to_xmm_reg spare), src, imm))
  | 0x60 -> un_imm_m16 "pcmpestrm"
  | 0x61 -> un_imm_m16 "pcmpestri"
  | 0x62 -> un_imm_m16 "pcmpistrm"
  | 0x63 ->
      (* 66 0f 3a 63 /r imm8 : PCMPISTRI xmm1, xmm2/m128, imm8 *)
      un_imm_m16 "pcmpistri"
  | 0xdf when mode = `M16 ->
      unsupported_modrm_imm "aeskeygenassist" address_mode 1 lr
  | _ -> abort ()

let read_d8 address_mode lr =
  match Lreader.Peek.u8 lr with
  | 0xd1 -> unsupported_imm "fcom" 1 lr
  | 0xd9 -> unsupported_imm "fcomp" 1 lr
  | _ -> unsupported_modrm "float op" address_mode lr

let read_d9 address_mode lr =
  match Lreader.Peek.u8 lr with
  | 0xd0 ->
      (* FNOP *)
      Lreader.advance lr 1;
      Nop
  | 0xc9 | 0xe0 | 0xe1 | 0xe4 | 0xe5 | 0xe8 | 0xe9 | 0xea | 0xeb | 0xec | 0xed
  | 0xee ->
      unsupported_imm "float op" 1 lr
  | n when n >= 0xf0 && n <= 0xff -> unsupported_imm "float op" 1 lr
  | _ -> unsupported_modrm "float op" address_mode lr

let read_db address_mode lr =
  let byte = Lreader.Peek.u8 lr in
  let _, spare = read_modrm address_mode lr in
  if spare = 4 then
    match byte with
    | 0xe0 | 0xe1 | 0xe2 | 0xe3 | 0xe4 -> unsupported "float op"
    | _ -> abort ()
  else unsupported "float op"

let read_da address_mode lr =
  let byte = Lreader.Peek.u8 lr in
  let _, spare = read_modrm address_mode lr in
  if spare = 5 && byte = 0xe9 then
    (* fucompp : just extracted as special case but behavior is the same *)
    unsupported "float op"
  else (* all other cases *)
    unsupported "float op"

let decode_load_far mode x r gop =
  let seg =
    match x with
    | 0xc4 -> ES
    | 0xc5 -> DS
    | 0xb2 -> SS
    | 0xb4 -> FS
    | 0xb5 -> GS
    | _ -> abort ()
  in
  match gop with
  | Address addr -> LoadFarPointer (mode, seg, int_to_reg32 r, addr)
  | _ -> abort ()

let read_2bytes_opcode addr mode address_mode prefix lr =
  let byte = Lreader.Read.u8 lr in
  assert (byte = 0x0f);
  let on_xmm_simd = with_xmm_simd mode in
  let b2 = Lreader.Read.u8 lr in
  match b2 with
  | 0x00 -> (
      match peek_spare lr with
      | 0 -> unsupported "sldt"
      | 1 -> unsupported "str"
      | 2 -> unsupported "lldt"
      | 3 ->
          let op, _ = read_rm16_with_spare lr in
          Ltr op
      | 4 -> unsupported "verr"
      | 5 -> unsupported "vrw"
      | _ -> abort ())
  | 0x01 -> read_0f_01 mode address_mode lr
  | 0x02 -> unsupported_modrm "lar" address_mode lr
  | 0x03 ->
      let dst, spare = read_modrm address_mode lr in
      Lsl (of_mode mode, Reg (int_to_reg32 spare), dst)
  | 0x06 -> unsupported "clts"
  | 0x08 -> unsupported "invd"
  | 0x09 -> unsupported "wbinvd"
  | 0x0b -> unsupported "ud2"
  | 0x0d ->
      ignore (read_modrm address_mode lr);
      Nop
  | 0x0f ->
      (* 3D now *)
      read_0f_0f mode address_mode lr
  | 0x10 -> unsupported_modrm "movups/movss/movupd/movsd" address_mode lr
  | 0x11 -> unsupported_modrm "movups/movss/movupd/movsd" address_mode lr
  | 0x12 -> (
      match (prefix, mode) with
      | _, `M16 ->
          let src, spare = read_modrm_xmm address_mode lr in
          Movlpd (S64, Reg (int_to_xmm_reg spare), src)
      | None, `M32 -> (
          let src, spare = read_modrm_xmm address_mode lr in
          match src with
          | Reg _ -> Movhlps (S128, Reg (int_to_xmm_reg spare), src)
          | Address _ -> Movlps (S64, Reg (int_to_xmm_reg spare), src)
          | Imm _ -> abort ())
      | Some F2, `M32 ->
          let src, spare = read_modrm_xmm address_mode lr in
          Movddup (S64, Reg (int_to_xmm_reg spare), src)
      | Some F3, `M32 ->
          let src, spare = read_modrm_xmm address_mode lr in
          Movsldup (S128, Reg (int_to_xmm_reg spare), src))
  | 0x13 -> (
      match mode with
      | `M32 ->
          let dst, spare = read_modrm_xmm address_mode lr in
          Movlps (S64, dst, Reg (int_to_xmm_reg spare))
      | `M16 ->
          let dst, spare = read_modrm_xmm address_mode lr in
          Movlpd (S64, dst, Reg (int_to_xmm_reg spare)))
  | 0x14 -> unsupported_modrm "unpcklp(s|d)" address_mode lr
  | 0x15 -> unsupported_modrm "unpckhp(s|d)" address_mode lr
  | 0x16 -> (
      match (prefix, mode) with
      | _, `M16 ->
          let src, spare = read_modrm_xmm address_mode lr in
          Movhpd (S64, Reg (int_to_xmm_reg spare), src)
      | Some F3, `M32 ->
          let src, spare = read_modrm_xmm address_mode lr in
          Movshdup (S128, Reg (int_to_xmm_reg spare), src)
      | _, `M32 -> (
          let src, spare = read_modrm_xmm address_mode lr in
          match src with
          | Reg _ -> Movlhps (S128, Reg (int_to_xmm_reg spare), src)
          | Address _ -> Movhps (S64, Reg (int_to_xmm_reg spare), src)
          | Imm _ -> abort ()))
  | 0x17 -> unsupported_modrm "movhp(s|d)" address_mode lr
  | 0x18 -> (
      let _, spare = read_modrm address_mode lr in
      match spare with
      | 0 -> Prefetch "nta"
      | 1 -> Prefetch "t0"
      | 2 -> Prefetch "t1"
      | 3 -> Prefetch "t2"
      | 4 | 5 | 6 | 7 -> Nop
      | _ -> abort ())
  | 0x19 | 0x1a | 0x1b | 0x1c | 0x1d | 0x1e | 0x1f ->
      ignore (read_modrm address_mode lr);
      Nop
  | 0x20 | 0x21 | 0x22 | 0x23 -> unsupported_modrm "mov" address_mode lr
  | 0x28 -> (
      match mode with
      | `M32 ->
          let src, spare = read_modrm_xmm address_mode lr in
          Movaps (S128, Reg (int_to_xmm_reg spare), src)
      | `M16 ->
          let src, spare = read_modrm_xmm address_mode lr in
          Movaps (S128, Reg (int_to_xmm_reg spare), src))
  | 0x29 -> (
      match mode with
      | `M16 ->
          let dst, spare = read_modrm_xmm address_mode lr in
          let instr = Movaps (S128, dst, Reg (int_to_xmm_reg spare)) in
          instr
      | `M32 ->
          let dst, spare = read_modrm_xmm address_mode lr in
          let instr = Movaps (S128, dst, Reg (int_to_xmm_reg spare)) in
          instr)
  | 0x2a -> unsupported_modrm_xmm "cvtpi2ps" address_mode lr
  | 0x2b -> unsupported_modrm_xmm "movntps" address_mode lr
  | 0x2c -> unsupported_modrm_xmm "cvttps2pi" address_mode lr
  | 0x2d -> unsupported_modrm_xmm "cvtps2pi" address_mode lr
  | 0x2e -> unsupported_modrm_xmm "ucomiss" address_mode lr
  | 0x2f -> unsupported_modrm_xmm "comiss" address_mode lr
  | 0x30 -> unsupported "wrmsr"
  | 0x31 -> unsupported "rdtsc"
  | 0x32 -> unsupported "rdmsr"
  | 0x33 -> unsupported "rdpmc"
  | 0x34 -> unsupported "sysenter"
  | 0x35 -> unsupported "sysexit"
  | 0x37 -> unsupported "getsec"
  | 0x38 -> read_0f_38 mode address_mode lr
  | 0x3a -> read_0f_3a mode address_mode lr
  | b2 when b2 >= 0x40 && b2 < 0x40 + nCcs ->
      let dst, spare = read_modrm address_mode lr in
      CMovcc (of_mode mode, int_to_cc (b2 - 0x40), Reg (int_to_reg32 spare), dst)
  | 0x50 -> unsupported_modrm_xmm "movmskps/d" address_mode lr
  | 0x51 -> unsupported_modrm_xmm "sqrtps/ss/pd/sd" address_mode lr
  | 0x52 -> unsupported_modrm_xmm "rsqrtps/ss" address_mode lr
  | 0x53 -> unsupported_modrm_xmm "rcpps/ss" address_mode lr
  | 0x54 -> unsupported_modrm_xmm "andps/d" address_mode lr
  | 0x55 -> unsupported_modrm_xmm "andnps/d" address_mode lr
  | 0x56 -> unsupported_modrm_xmm "orps/d" address_mode lr
  | 0x57 -> unsupported_modrm_xmm "xorps/d" address_mode lr
  | 0x58 -> unsupported_modrm_xmm "addps/ss/pd/sd" address_mode lr
  | 0x59 -> unsupported_modrm_xmm "mulps/ss/pd/sd" address_mode lr
  | 0x5a -> unsupported_modrm_xmm "cvtps2pd/pd2ps/ss2sd/sd2ss" address_mode lr
  | 0x5b -> unsupported_modrm_xmm "cvtdq2ps/ps2dq/tps2dq" address_mode lr
  | 0x5c -> unsupported_modrm_xmm "subps/ss/pd/sd" address_mode lr
  | 0x5d -> unsupported_modrm_xmm "minps/ss/pd/sd" address_mode lr
  | 0x5e -> unsupported_modrm_xmm "divps/ss/pd/sd" address_mode lr
  | 0x5f -> unsupported_modrm_xmm "maxps/ss/pd/sd" address_mode lr
  | 0x60 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Punpckl (xmm, simd, Reg (int_to_xmm_reg spare), src, 8))
  | 0x61 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Punpckl (xmm, simd, Reg (int_to_xmm_reg spare), src, 16))
  | 0x62 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Punpckl (xmm, simd, Reg (int_to_xmm_reg spare), src, 32))
  | 0x63 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Packss (xmm, simd, Reg (int_to_xmm_reg spare), src, 8))
  | 0x64 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun mm simd_size ->
          Pcmpgtb (mm, simd_size, Reg (int_to_xmm_reg spare), src))
  | 0x65 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun mm simd_size ->
          Pcmpgtw (mm, simd_size, Reg (int_to_xmm_reg spare), src))
  | 0x66 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun mm simdsize ->
          Pcmpgtd (mm, simdsize, Reg (int_to_xmm_reg spare), src))
  | 0x67 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun mm simdsize ->
          Packus (mm, simdsize, Reg (int_to_xmm_reg spare), src, 8))
  | 0x68 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Punpckh (xmm, simd, Reg (int_to_xmm_reg spare), src, 8))
  | 0x69 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Punpckh (xmm, simd, Reg (int_to_xmm_reg spare), src, 16))
  | 0x6a ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Punpckh (xmm, simd, Reg (int_to_xmm_reg spare), src, 32))
  | 0x6b ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Packss (xmm, simd, Reg (int_to_xmm_reg spare), src, 16))
  | 0x6c ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Punpckl (xmm, simd, Reg (int_to_xmm_reg spare), src, 64))
  | 0x6d ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Punpckh (xmm, simd, Reg (int_to_xmm_reg spare), src, 64))
  | 0x6e ->
      let src, spare = read_modrm address_mode lr in
      on_xmm_simd (fun mm _ -> Movd (mm, Left, Reg (int_to_xmm_reg spare), src))
  | 0x6f -> (
      let src, spare = read_modrm_xmm address_mode lr in
      match mode with
      | `M16 -> MovdQA (XMM, S128, Reg (int_to_xmm_reg spare), src)
      | `M32 -> (
          match prefix with
          | Some F3 -> MovdQU (XMM, S128, Reg (int_to_xmm_reg spare), src)
          | _ -> MovQ (MM, S64, Reg (int_to_xmm_reg spare), src)))
  | 0x70 -> (
      let src, spare = read_modrm_xmm address_mode lr in
      let imm = Lreader.Read.u8 lr in
      let xmm_reg = int_to_xmm_reg spare in
      match mode with
      | `M32 -> (
          match prefix with
          | None -> Pshufw (MM, S64, xmm_reg, src, imm)
          | Some F2 -> Pshuflw (XMM, S128, xmm_reg, src, imm)
          | Some F3 -> Pshufhw (XMM, S128, xmm_reg, src, imm))
      | `M16 -> Pshufd (XMM, S128, xmm_reg, src, imm))
  | 0x71 ->
      let gop, spare = read_modrm_xmm address_mode lr in
      let imm = read_byte_as_int64 lr in
      on_xmm_simd (fun mm simd_size ->
          match spare with
          | 2 -> Psrl (mm, simd_size, gop, Imm imm, 16)
          | 4 -> Psra (mm, simd_size, gop, Imm imm, 16)
          | 6 -> Psll (mm, simd_size, gop, Imm imm, 16)
          | _ -> abort ())
  | 0x72 ->
      let gop, spare = read_modrm_xmm address_mode lr in
      let imm = read_byte_as_int64 lr in
      on_xmm_simd (fun mm simd_size ->
          match spare with
          | 2 -> Psrl (mm, simd_size, gop, Imm imm, 32)
          | 4 -> Psra (mm, simd_size, gop, Imm imm, 32)
          | 6 -> Psll (mm, simd_size, gop, Imm imm, 32)
          | _ -> abort ())
  | 0x73 -> (
      let gop, spare = read_modrm_xmm address_mode lr in
      let imm = read_byte_as_int64 lr in
      match mode with
      | `M32 -> (
          match spare with
          | 2 -> Psrl (MM, S64, gop, Imm imm, 64)
          | 6 -> Psll (MM, S64, gop, Imm imm, 64)
          | _ -> abort ())
      | `M16 -> (
          match spare with
          | 2 -> Psrl (XMM, S128, gop, Imm imm, 64)
          | 3 -> Psrldq (gop, Int64.to_int imm)
          | 6 -> Psll (XMM, S128, gop, Imm imm, 64)
          | 7 -> Pslldq (gop, Int64.to_int imm)
          | _ -> abort ()))
  | 0x74 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun mm simd_size ->
          Pcmpeqb (mm, simd_size, Reg (int_to_xmm_reg spare), src))
  | 0x75 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun mm simd_size ->
          Pcmpeqw (mm, simd_size, Reg (int_to_xmm_reg spare), src))
  | 0x76 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun mm simd_size ->
          Pcmpeqd (mm, simd_size, Reg (int_to_xmm_reg spare), src))
  | 0x77 -> Emms
  | 0x78 -> unsupported_modrm "vmread" address_mode lr
  | 0x79 -> unsupported_modrm "vmwrite" address_mode lr
  | 0x7c -> (
      match mode with
      | `M16 -> unsupported_modrm_xmm "haddpd" address_mode lr
      | `M32 -> (
          match prefix with
          | Some F2 -> unsupported_modrm_xmm "haddps" address_mode lr
          | _ -> abort ()))
  | 0x7d -> (
      match mode with
      | `M16 -> unsupported_modrm_xmm "hsubpd" address_mode lr
      | `M32 -> (
          match prefix with
          | Some F2 -> unsupported_modrm_xmm "hsubps" address_mode lr
          | _ -> abort ()))
  | 0x7e -> (
      match mode with
      | `M16 ->
          let src, spare = read_modrm address_mode lr in
          Movd (XMM, Right, Reg (int_to_xmm_reg spare), src)
      | `M32 -> (
          match prefix with
          | Some F3 ->
              let src, spare = read_modrm_xmm address_mode lr in
              MovQ (XMM, S64, Reg (int_to_xmm_reg spare), src)
          | _ ->
              let src, spare = read_modrm address_mode lr in
              Movd (MM, Right, Reg (int_to_xmm_reg spare), src)))
  | 0x7f -> (
      let dst, spare = read_modrm_xmm address_mode lr in
      match mode with
      | `M16 -> MovdQA (XMM, S128, dst, Reg (int_to_xmm_reg spare))
      | `M32 -> (
          match prefix with
          | Some F3 -> MovdQU (XMM, S128, dst, Reg (int_to_xmm_reg spare))
          | _ -> MovQ (MM, S64, dst, Reg (int_to_xmm_reg spare))))
  | b2 when b2 >= 0x80 && b2 < 0x80 + nCcs ->
      (* FIXME: Operand size attribute could force that to read a word instead of a
         double-word.
         CHECK: if it corresponds to [mode] argument.
      *)
      let rel =
        Int32.to_int (Lreader.Read.i32 lr) land 0xffffffff
        (* TODO: unsafe on 32bit *)
      in
      Jcc (int_to_cc (b2 - 0x80), Int64.of_int (displacement addr lr rel))
  | b2 when b2 >= 0x90 && b2 < 0x90 + nCcs ->
      let dst, _spare = read_rm8_with_spare lr in
      SetCc (int_to_cc (b2 - 0x90), dst)
  | 0xa0 -> PushS FS
  | 0xa1 -> PopS FS
  | 0xa2 -> unsupported "cpuid"
  | 0xa3 ->
      let dst, spare = read_modrm address_mode lr in
      Bt { mode = of_mode mode; dst; src = Reg (int_to_reg32 spare) }
  | 0xa4 ->
      let dst, spare = read_modrm address_mode lr in
      let imm = read_byte_as_int64 lr in
      Shiftd (of_mode mode, Shld, dst, Reg (int_to_reg32 spare), Imm imm)
  | 0xa5 ->
      let dst, spare = read_modrm address_mode lr in
      Shiftd (of_mode mode, Shld, dst, Reg (int_to_reg32 spare), Reg CL)
  | 0xa8 -> PushS GS
  | 0xa9 -> PopS GS
  | 0xaa -> unsupported "rsm"
  | 0xab ->
      let dst, spare = read_modrm address_mode lr in
      Bts { mode = of_mode mode; dst; src = Reg (int_to_reg32 spare) }
  | 0xac ->
      let dst, spare = read_modrm address_mode lr in
      let imm = read_byte_as_int64 lr in
      Shiftd (of_mode mode, Shrd, dst, Reg (int_to_reg32 spare), Imm imm)
  | 0xad ->
      let dst, spare = read_modrm address_mode lr in
      Shiftd (of_mode mode, Shrd, dst, Reg (int_to_reg32 spare), Reg CL)
  | 0xae ->
      (* FIXME: detail all mnemonics *)
      let _, spare = read_modrm address_mode lr in
      let msg = Printf.sprintf "0x0f ae %i" spare in
      unsupported msg
  | 0xaf ->
      let src, spare = read_modrm address_mode lr in
      let dst1 = Reg (int_to_reg32 spare) in
      IMul (of_mode mode, Some dst1, dst1, src)
  | 0xb0 -> unsupported_modrm "cmpxchg" address_mode lr
  | 0xb1 ->
      let dst, spare = read_modrm address_mode lr in
      CmpXchg (of_mode mode, dst, Reg (int_to_reg32 spare))
  | (0xb2 | 0xb4 | 0xb5) as x ->
      let gop, spare = read_modrm address_mode lr in
      decode_load_far mode x spare gop
  | 0xb3 ->
      let dst, spare = read_modrm address_mode lr in
      let src = Reg (int_to_reg32 spare) in
      Btr { mode = of_mode mode; src; dst }
  | 0xb6 ->
      let src, spare = read_rm8_with_spare lr in
      Movzx (of_mode mode, int_to_reg32 spare, src)
  | 0xb7 ->
      let src, spare = read_rm16_with_spare lr in
      Movzx16 (of_mode mode, int_to_reg32 spare, src)
  | 0xb8 -> (
      (* added *)
      match prefix with
      | Some F3 ->
          let src, spare = read_modrm address_mode lr in
          Popcnt (of_mode mode, Reg (int_to_reg32 spare), src)
      | _ -> abort ())
  | 0xb9 -> unsupported_modrm "ud" address_mode lr
  | 0xba -> (
      let dst, spare = read_modrm address_mode lr in
      match spare with
      | 4 ->
          let imm = read_byte_as_int64 lr in
          Bt { mode = of_mode mode; dst; src = Imm imm }
      | 5 ->
          let imm = read_byte_as_int64 lr in
          Bts { mode = of_mode mode; dst; src = Imm imm }
      | 6 ->
          let imm = read_byte_as_int64 lr in
          Btr { mode = of_mode mode; dst; src = Imm imm }
      | 7 ->
          let imm = read_byte_as_int64 lr in
          Btc { mode = of_mode mode; dst; src = Imm imm }
      | _ -> abort ())
  | 0xbb -> unsupported_modrm "btc" address_mode lr
  | 0xbc ->
      let src, spare = read_modrm address_mode lr in
      Bsf (of_mode mode, int_to_reg32 spare, src)
  | 0xbd -> (
      let src, spare = read_modrm address_mode lr in
      match prefix with
      | Some F3 -> Lzcnt (of_mode mode, Reg (int_to_reg32 spare), src)
      | _ -> Bsr (of_mode mode, int_to_reg32 spare, src))
  | 0xbe ->
      let src, spare = read_rm8_with_spare lr in
      Movsx (of_mode mode, int_to_reg32 spare, src)
  | 0xbf ->
      let src, spare = read_rm16_with_spare lr in
      Movsx16 (of_mode mode, int_to_reg32 spare, src)
  | 0xc0 ->
      let dst, spare = read_modrm address_mode lr in
      Xadd (`M8, dst, Reg (int_to_reg32 spare))
  | 0xc1 ->
      let dst, spare = read_modrm address_mode lr in
      Xadd (of_mode mode, dst, Reg (int_to_reg32 spare))
  | 0xc2 -> unsupported_modrm_xmm "cmpps/ss/pd/sd" address_mode lr
  | 0xc3 -> unsupported_modrm "movnti" address_mode lr
  | 0xc4 -> unsupported_modrm "pinsrw" address_mode lr
  | 0xc5 -> unsupported_modrm_imm "pextrw" address_mode 1 lr
  | 0xc6 -> unsupported_modrm_imm "shufps/d" address_mode 1 lr
  | 0xc7 -> (
      let src, spare = read_modrm_xmm address_mode lr in
      match spare with
      | 1 -> CmpXchg8b (MM, S64, src)
      | 7 -> unsupported "vmptrst"
      | 6 -> (
          match mode with
          | `M16 -> unsupported "vmclear"
          | `M32 -> (
              match prefix with
              | Some F3 -> unsupported "vmxon"
              | _ -> unsupported "vlptrld"))
      | _ -> abort ())
  | b2 when b2 >= 0xc8 && b2 < 0xc8 + nRegs32 ->
      Bswap (of_mode mode, int_to_reg32 (b2 - 0xc8))
  | 0xd0 -> (
      match mode with
      | `M16 -> unsupported_modrm_xmm "addsubpd" address_mode lr
      | `M32 -> (
          match prefix with
          | Some F2 -> unsupported_modrm_xmm "addsubps" address_mode lr
          | _ -> abort ()))
  | 0xd1 ->
      let gop, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun mm simd_size ->
          Psrl (mm, simd_size, Reg (int_to_xmm_reg spare), gop, 16))
  | 0xd2 -> (
      let gop, spare = read_modrm_xmm address_mode lr in
      match mode with
      | `M32 -> Psrl (MM, S64, Reg (int_to_xmm_reg spare), gop, 32)
      | `M16 -> Psrl (XMM, S128, Reg (int_to_xmm_reg spare), gop, 32))
  | 0xd3 -> (
      let gop, spare = read_modrm_xmm address_mode lr in
      match mode with
      | `M32 -> Psrl (MM, S64, Reg (int_to_xmm_reg spare), gop, 64)
      | `M16 -> Psrl (XMM, S128, Reg (int_to_xmm_reg spare), gop, 64))
  | 0xd4 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Padd (xmm, simd, Reg (int_to_xmm_reg spare), src, 64))
  | 0xd5 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Pmullw (xmm, simd, Reg (int_to_xmm_reg spare), src))
  | 0xd6 -> (
      let src, spare = read_modrm_xmm address_mode lr in
      match mode with
      | `M16 -> MovQ (XMM, S64, src, Reg (int_to_xmm_reg spare))
      | `M32 -> (
          match prefix with
          | Some F3 -> unsupported "movq2dq"
          | Some F2 -> unsupported "movdq2q"
          | None -> abort ()))
  | 0xd7 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun mm simd ->
          PmovMSKB (mm, simd, Reg (int_to_reg32 spare), src))
  | 0xd8 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Psubus (xmm, simd, Reg (int_to_xmm_reg spare), src, 8))
  | 0xd9 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Psubus (xmm, simd, Reg (int_to_xmm_reg spare), src, 16))
  | 0xda ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun mm simd ->
          Pminu (mm, simd, Reg (int_to_xmm_reg spare), src, 8))
  | 0xdb ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun mm simd ->
          Pand (mm, simd, Reg (int_to_xmm_reg spare), src))
  | 0xdc ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Paddus (xmm, simd, Reg (int_to_xmm_reg spare), src, 8))
  | 0xdd ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Paddus (xmm, simd, Reg (int_to_xmm_reg spare), src, 16))
  | 0xde ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun mm simd ->
          Pmaxu (mm, simd, Reg (int_to_xmm_reg spare), src, 8))
  | 0xdf ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun mm simd ->
          Pandn (mm, simd, Reg (int_to_xmm_reg spare), src))
  | 0xe0 ->
      let src, spare = read_modrm_xmm address_mode lr in
      with_xmm_simd mode (fun xmm simd ->
          Pavgu (xmm, simd, Reg (int_to_xmm_reg spare), src, 8))
  | 0xe1 ->
      let gop, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Psra (xmm, simd, Reg (int_to_xmm_reg spare), gop, 16))
  | 0xe2 ->
      let gop, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Psra (xmm, simd, Reg (int_to_xmm_reg spare), gop, 32))
  | 0xe3 ->
      let src, spare = read_modrm_xmm address_mode lr in
      with_xmm_simd mode (fun xmm simd ->
          Pavgu (xmm, simd, Reg (int_to_xmm_reg spare), src, 16))
  | 0xe4 -> unsupported_modrm "pmulhuw" address_mode lr
  | 0xe5 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Pmulhw (xmm, simd, Reg (int_to_xmm_reg spare), src))
  | 0xe6 -> (
      match mode with
      | `M16 -> unsupported_modrm_xmm "cvttpd2dq" address_mode lr
      | `M32 -> (
          match prefix with
          | Some F2 -> unsupported_modrm_xmm "cvtpd2dq" address_mode lr
          | Some F3 -> unsupported_modrm_xmm "cvtdq2pd" address_mode lr
          | None -> abort ()))
  | 0xe7 ->
      let dst, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Movntq (xmm, simd, dst, Reg (int_to_xmm_reg spare)))
  | 0xe8 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Psubs (xmm, simd, Reg (int_to_xmm_reg spare), src, 8))
  | 0xe9 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Psubs (xmm, simd, Reg (int_to_xmm_reg spare), src, 16))
  | 0xea ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Pmins (xmm, simd, Reg (int_to_xmm_reg spare), src, 16))
  | 0xeb ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Por (xmm, simd, Reg (int_to_xmm_reg spare), src))
  | 0xee ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Pmaxs (xmm, simd, Reg (int_to_xmm_reg spare), src, 16))
  | 0xec ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Padds (xmm, simd, Reg (int_to_xmm_reg spare), src, 8))
  | 0xed ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Padds (xmm, simd, Reg (int_to_xmm_reg spare), src, 16))
  | 0xef ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Pxor (xmm, simd, Reg (int_to_xmm_reg spare), src))
  | 0xf0 -> (
      match prefix with
      | Some F2 -> unsupported_modrm_xmm "lddqu" address_mode lr
      | _ -> abort ())
  | 0xf1 ->
      let gop, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun mm simdsize ->
          Psll (mm, simdsize, Reg (int_to_xmm_reg spare), gop, 16))
  | 0xf2 ->
      let gop, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Psll (xmm, simd, Reg (int_to_xmm_reg spare), gop, 32))
  | 0xf3 ->
      let gop, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Psll (xmm, simd, Reg (int_to_xmm_reg spare), gop, 64))
  | 0xf4 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Pmuludq (xmm, simd, Reg (int_to_xmm_reg spare), src))
  | 0xf5 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Pmaddwd (xmm, simd, Reg (int_to_xmm_reg spare), src))
  | 0xf6 -> unsupported_modrm "psadbw" address_mode lr
  | 0xf7 -> (
      match mode with
      | `M32 -> unsupported_modrm "maskmovq" address_mode lr
      | `M16 -> unsupported_modrm_xmm "maskmovdqu" address_mode lr)
  | 0xf8 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Psub (xmm, simd, Reg (int_to_xmm_reg spare), src, 8))
  | 0xf9 ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Psub (xmm, simd, Reg (int_to_xmm_reg spare), src, 16))
  | 0xfa ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Psub (xmm, simd, Reg (int_to_xmm_reg spare), src, 32))
  | 0xfb ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Psub (xmm, simd, Reg (int_to_xmm_reg spare), src, 64))
  | 0xfc ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Padd (xmm, simd, Reg (int_to_xmm_reg spare), src, 8))
  | 0xfd ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Padd (xmm, simd, Reg (int_to_xmm_reg spare), src, 16))
  | 0xfe ->
      let src, spare = read_modrm_xmm address_mode lr in
      on_xmm_simd (fun xmm simd ->
          Padd (xmm, simd, Reg (int_to_xmm_reg spare), src, 32))
  | _byte -> abort ()

let read addr lr =
  (* lr is a cursor *)
  (*  Logger.debug "@[EIP %x,%d(%x) : %a@]" (fst eip) (snd eip) (snd eip / 8)
   *  Bits.pp bits; *)
  Logger.debug ~level:4 "[x86 decode] %a (%a)" Virtual_address.pp addr
    Lreader.pp lr;
  let lo = Lreader.get_pos lr in
  let addr = Virtual_address.to_int addr - lo in
  let sreg = ref None in

  (*   let succ_eip (bv_addr, off_addr) = succ bv_addr, succ off_addr in *)
  let rec aux_read_instr mode address_mode prefix lr =
    let byte = Lreader.Peek.u8 lr in
    (*    let byte, bits = Bits.read_uint bits 8 in *)
    if byte = 0x0f then read_2bytes_opcode addr mode address_mode prefix lr
    else
      let arith_to_rm aop =
        let dst, spare = read_modrm address_mode lr in
        Arith (of_mode mode, aop, dst, Reg (int_to_reg32 spare))
      in

      let arith_from_rm aop =
        let src, spare = read_modrm address_mode lr in
        Arith (of_mode mode, aop, Reg (int_to_reg32 spare), src)
      in

      match Lreader.Read.u8 lr with
      | 0x00 ->
          let dst, spare = read_modrm address_mode lr in
          Arith (`M8, Add, dst, Reg (int_to_reg32 spare))
      | 0x01 -> arith_to_rm Add
      | 0x02 ->
          (* added *)
          let src, spare = read_modrm address_mode lr in
          Arith (`M8, Add, Reg (int_to_reg32 spare), src)
      | 0x03 -> arith_from_rm Add
      | 0x04 ->
          (* added *)
          let imm = read_byte_as_int64 lr in
          Arith (`M8, Add, Reg EAX, Imm imm)
      | 0x05 ->
          let imm = select_reader (imode mode) lr in
          Arith (of_mode mode, Add, Reg EAX, Imm (Int64.of_int imm))
      | 0x06 -> PushS ES
      | 0x07 -> PopS ES
      | 0x08 ->
          (* added *)
          let dst, spare = read_modrm address_mode lr in
          Arith (`M8, Or, dst, Reg (int_to_reg32 spare))
      | 0x09 -> arith_to_rm Or
      | 0x0a ->
          (* added *)
          let src, spare = read_modrm address_mode lr in
          Arith (`M8, Or, Reg (int_to_reg32 spare), src)
      | 0x0b -> arith_from_rm Or
      | 0x0c ->
          (* added *)
          let imm = read_byte_as_int64 lr in
          Arith (`M8, Or, Reg EAX, Imm imm)
      | 0x0d ->
          (* added *)
          let imm = select_reader (imode mode) lr in
          Arith (of_mode mode, Or, Reg EAX, Imm (Int64.of_int imm))
      | 0x0e -> PushS CS
      | 0x10 ->
          (* added *)
          let dst, spare = read_modrm address_mode lr in
          Arith (`M8, Adc, dst, Reg (int_to_reg32 spare))
      | 0x11 -> arith_to_rm Adc
      | 0x12 ->
          (* added *)
          let src, spare = read_modrm address_mode lr in
          Arith (`M8, Adc, Reg (int_to_reg32 spare), src)
      | 0x13 -> arith_from_rm Adc
      | 0x14 ->
          let imm = read_byte_as_int64 lr in
          Arith (`M8, Adc, Reg EAX, Imm imm)
      | 0x15 ->
          (* added *)
          let imm = select_reader (imode mode) lr in
          Arith (of_mode mode, Adc, Reg EAX, Imm (Int64.of_int imm))
      | 0x16 -> PushS SS
      | 0x17 -> PopS SS
      | 0x18 ->
          (* added *)
          let dst, spare = read_modrm address_mode lr in
          Arith (`M8, Sbb, dst, Reg (int_to_reg32 spare))
      | 0x19 -> arith_to_rm Sbb
      | 0x1a ->
          (* added *)
          let src, spare = read_modrm address_mode lr in
          Arith (`M8, Sbb, Reg (int_to_reg32 spare), src)
      | 0x1b -> arith_from_rm Sbb
      | 0x1c ->
          (* added *)
          let imm = read_byte_as_int64 lr in
          Arith (`M8, Sbb, Reg EAX, Imm imm)
      | 0x1d ->
          (* added *)
          let imm = select_reader (imode mode) lr in
          Arith (of_mode mode, Sbb, Reg EAX, Imm (Int64.of_int imm))
      | 0x1e -> PushS DS
      | 0x1f -> PopS DS
      | 0x20 ->
          (* added *)
          let dst, spare = read_modrm address_mode lr in
          Arith (`M8, And, dst, Reg (int_to_reg32 spare))
      | 0x21 -> arith_to_rm And
      | 0x22 ->
          (* added *)
          let src, spare = read_modrm address_mode lr in
          Arith (`M8, And, Reg (int_to_reg32 spare), src)
      | 0x23 -> arith_from_rm And
      | 0x24 ->
          (* added *)
          let imm = read_byte_as_int64 lr in
          Arith (`M8, And, Reg EAX, Imm imm)
      | 0x25 ->
          (* added *)
          let imm = select_reader (imode mode) lr in
          Arith (of_mode mode, And, Reg EAX, Imm (Int64.of_int imm))
      | 0x26 ->
          (* Design choice *)
          sreg := Some ES;
          aux_read_instr mode address_mode prefix lr
      | 0x27 -> unsupported "daa"
      | 0x28 ->
          let dst, spare = read_modrm address_mode lr in
          Arith (`M8, Sub, dst, Reg (int_to_reg32 spare))
      | 0x29 -> arith_to_rm Sub
      | 0x2a ->
          (* added *)
          let src, spare = read_modrm address_mode lr in
          Arith (`M8, Sub, Reg (int_to_reg32 spare), src)
      | 0x2b -> arith_from_rm Sub
      | 0x2c ->
          (* added *)
          let imm = read_byte_as_int64 lr in
          Arith (`M8, Sub, Reg EAX, Imm imm)
      | 0x2d ->
          (* added *)
          let imm = select_reader (imode mode) lr in
          Arith (of_mode mode, Sub, Reg EAX, Imm (Int64.of_int imm))
      | 0x2e ->
          (* Design choice *)
          sreg := Some CS;
          aux_read_instr mode address_mode prefix lr
      | 0x2f -> unsupported "das"
      | 0x30 ->
          (* added *)
          let dst, spare = read_modrm address_mode lr in
          Arith (`M8, Xor, dst, Reg (int_to_reg32 spare))
      | 0x31 -> arith_to_rm Xor
      | 0x32 ->
          (* added *)
          let src, spare = read_modrm address_mode lr in
          Arith (`M8, Xor, Reg (int_to_reg32 spare), src)
      | 0x33 -> arith_from_rm Xor
      | 0x34 ->
          (* added *)
          let imm = read_byte_as_int64 lr in
          Arith (`M8, Xor, Reg EAX, Imm imm)
      | 0x35 ->
          let imm = select_reader (imode mode) lr in
          Arith (`M32, Xor, Reg EAX, Imm (Int64.of_int imm))
      | 0x36 ->
          sreg := Some SS;
          aux_read_instr mode address_mode prefix lr
      | 0x37 -> unsupported "aaa"
      | 0x38 ->
          let dst, spare = read_modrm address_mode lr in
          Cmp (`M8, dst, Reg (int_to_reg32 spare))
      | 0x39 ->
          let dst, spare = read_modrm address_mode lr in
          Cmp (of_mode mode, dst, Reg (int_to_reg32 spare))
      | 0x3a ->
          (* added *)
          let src, spare = read_modrm address_mode lr in
          Cmp (`M8, Reg (int_to_reg32 spare), src)
      | 0x3b ->
          let src, spare = read_modrm address_mode lr in
          Cmp (of_mode mode, Reg (int_to_reg32 spare), src)
      | 0x3c ->
          (* added *)
          let imm = read_byte_as_int64 lr in
          Cmp (`M8, Reg EAX, Imm imm)
      | 0x3d ->
          let imm = select_reader (imode mode) lr in
          Cmp (of_mode mode, Reg EAX, Imm (Int64.of_int imm))
      | 0x3e ->
          (* Design choice *)
          sreg := Some DS;
          aux_read_instr mode address_mode prefix lr
      | 0x3f -> Aas
      | byte when byte >= 0x40 && byte < 0x40 + nRegs32 ->
          Inc (of_mode mode, Reg (int_to_reg32 (byte - 0x40)))
      | byte when byte >= 0x48 && byte < 0x48 + nRegs32 ->
          Dec (of_mode mode, Reg (int_to_reg32 (byte - 0x48)))
      | byte when byte >= 0x50 && byte < 0x50 + nRegs32 ->
          Push (of_mode mode, Reg (int_to_reg32 (byte - 0x50)))
      | byte when byte >= 0x58 && byte < 0x58 + nRegs32 ->
          Pop (of_mode mode, Reg (int_to_reg32 (byte - 0x58)))
      | 0x60 -> PushA mode
      | 0x61 -> PopA mode
      | 0x62 -> unsupported "bound"
      | 0x63 -> unsupported "arpl"
      | 0x64 ->
          sreg := Some FS;
          aux_read_instr mode address_mode prefix lr
      | 0x65 ->
          sreg := Some GS;
          aux_read_instr mode address_mode prefix lr
      | 0x66 ->
          let mode = X86Util.switch_default_data_mode mode in
          aux_read_instr mode address_mode prefix lr
      | 0x67 ->
          let address_mode = X86Util.switch_address_mode address_mode in
          aux_read_instr mode address_mode prefix lr
      | 0x68 ->
          let imm = select_reader (imode mode) lr in
          Push (of_mode mode, Imm (Int64.of_int imm))
      | 0x69 ->
          (* added *)
          let src, spare = read_modrm address_mode lr in
          let imm = select_reader (imode mode) lr in
          IMul
            ( of_mode mode,
              Some (Reg (int_to_reg32 spare)),
              src,
              Imm (Int64.of_int imm) )
      | 0x6a ->
          let imm = read_byte_as_int64 lr in
          Push (of_mode mode, Imm imm)
      | 0x6b ->
          let src, spare = read_modrm address_mode lr in
          let imm = read_byte_as_int64 lr in
          IMul (of_mode mode, Some (Reg (int_to_reg32 spare)), src, Imm imm)
      | 0x6c -> unsupported "ins/insb"
      | 0x6d -> unsupported "ins/insw"
      | 0x6e -> unsupported "outs/outsb"
      | 0x6f -> unsupported "outs/outsw/outsd"
      | byte when byte >= 0x70 && byte < 0x70 + nCcs ->
          let rel8 = Lreader.Read.u8 lr in
          let v = Int64.of_int (signed_displacement addr lr rel8) in
          Jcc (int_to_cc (byte - 0x70), v)
      | 0x80 -> (
          let dst, spare = read_modrm address_mode lr in
          let disp = read_byte_as_int64 lr in
          match spare with
          | 7 -> Cmp (`M8, dst, Imm disp)
          | _ -> Arith (`M8, int_to_arith_op spare, dst, Imm disp))
      | 0x81 -> (
          let dst, spare = read_modrm address_mode lr in
          let imode = imode mode in
          let disp = select_reader imode lr |> Int64.of_int in
          match spare with
          | 7 -> Cmp (of_mode mode, dst, Imm disp)
          | _ -> Arith (of_mode mode, int_to_arith_op spare, dst, Imm disp))
      | 0x82 -> (
          let dst, spare = read_modrm address_mode lr in
          let disp = read_byte_as_int64 lr in
          match spare with
          | 7 -> Cmp (`M8, dst, Imm disp)
          | _ -> Arith (`M8, int_to_arith_op spare, dst, Imm disp))
      | 0x83 -> (
          let dst, spare = read_modrm address_mode lr in
          let disp64 = Lreader.Read.u8 lr |> sign_extend |> Int64.of_int in
          match spare with
          | 7 -> Cmp (of_mode mode, dst, Imm disp64)
          | _ -> Arith (of_mode mode, int_to_arith_op spare, dst, Imm disp64))
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
          Mov (of_mode mode, dst, Reg (int_to_reg32 spare))
      | 0x8a ->
          let src, spare = read_modrm address_mode lr in
          Mov (`M8, Reg (int_to_reg32 spare), src)
      | 0x8b ->
          let src, spare = read_modrm address_mode lr in
          Mov (of_mode mode, Reg (int_to_reg32 spare), src)
      | 0x8c ->
          let dst, spare = read_rm16_with_spare lr in
          MovSegRight (dst, int_to_segment_reg spare)
      | 0x8d -> (
          let gop, spare = read_modrm address_mode lr in
          match gop with
          | Address addr -> Lea (of_mode mode, int_to_reg32 spare, addr)
          | _ -> abort ())
      | 0x8e ->
          let src, spare = read_rm16_with_spare lr in
          MovSegLeft (int_to_segment_reg spare, src)
      | 0x8f -> (
          (* added *)
          let dst, spare = read_modrm address_mode lr in
          match spare with 0 -> Pop (of_mode mode, dst) | _ -> abort ())
      | 0x90 -> Nop
      | byte when byte > 0x90 && byte < 0x90 + nRegs32 ->
          Xchg (of_mode mode, Reg (int_to_reg32 (byte - 0x90)), Reg EAX)
      | 0x98 -> CBW mode
      | 0x99 -> CWD mode
      | 0x9a -> unsupported "callf"
      | 0x9b -> Wait
      | 0x9c -> Pushfd (of_mode mode)
      | 0x9d -> Popfd (of_mode mode)
      | 0x9e -> Sahf
      | 0x9f -> Lahf
      | 0xa0 ->
          let bits = (bitsize_of_address_mode address_mode :> int) in
          let imm = select_reader bits lr in
          let addr = X86Util.mk_address ~address_mode imm in
          Mov (`M8, Reg EAX, addr)
      | 0xa1 ->
          let bits = (bitsize_of_address_mode address_mode :> int) in
          let imm = select_reader bits lr in
          let addr = X86Util.mk_address ~address_mode imm in
          let inst = Mov (of_mode mode, Reg EAX, addr) in
          inst
      | 0xa2 ->
          let bits = (bitsize_of_address_mode address_mode :> int) in
          let imm = select_reader bits lr in
          let addr = X86Util.mk_address ~address_mode imm in
          Mov (of_mode mode, addr, Reg EAX)
      | 0xa3 ->
          let bits = (bitsize_of_address_mode address_mode :> int) in
          let imm = select_reader bits lr in
          let addr = X86Util.mk_address ~address_mode imm in
          Mov (of_mode mode, addr, Reg EAX)
      | 0xa4 -> Movs (rep_of prefix, `M8)
      | 0xa5 -> Movs (rep_of prefix, of_mode mode)
      | 0xa6 -> Cmps (repcc_of prefix, `M8)
      | 0xa7 -> Cmps (repcc_of prefix, of_mode mode)
      | 0xa8 ->
          let imm = read_byte_as_int64 lr in
          Test (`M8, Reg EAX, Imm imm)
      | 0xa9 ->
          (* added *)
          let imm = select_reader (imode mode) lr in
          Test (of_mode mode, Reg EAX, Imm (Int64.of_int imm))
      | 0xaa -> Stos (rep_of prefix, `M8)
      | 0xab -> Stos (rep_of prefix, of_mode mode)
      | 0xac -> Lods (rep_of prefix, `M8)
      | 0xad -> Lods (rep_of prefix, of_mode mode)
      | 0xae -> Scas (repcc_of prefix, `M8)
      | 0xaf -> Scas (repcc_of prefix, of_mode mode)
      | byte when byte >= 0xb0 && byte < 0xb0 + nRegs32 ->
          let imm = read_byte_as_int64 lr in
          Mov (`M8, Reg (int_to_reg32 (byte - 0xb0)), Imm imm)
      | byte when byte >= 0xb8 && byte < 0xb8 + nRegs32 ->
          let imm = select_reader (imode mode) lr in
          Mov
            ( of_mode mode,
              Reg (int_to_reg32 (byte - 0xb8)),
              Imm (Int64.of_int imm) )
      | 0xc0 ->
          let dst, spare = read_modrm address_mode lr in
          let imm64 = Imm (read_byte_as_int64 lr) in
          let rotate rot_type = Rotate (`M8, rot_type, dst, imm64)
          and shift shift_type = Shift (`M8, shift_type, dst, imm64) in
          shift_or_rotate_from_spare ~shift ~rotate spare
      | 0xc1 ->
          let dst, spare = read_modrm address_mode lr in
          let imm64 = Imm (read_byte_as_int64 lr) in
          let rotate rot_type = Rotate (of_mode mode, rot_type, dst, imm64)
          and shift shift_type = Shift (of_mode mode, shift_type, dst, imm64) in
          shift_or_rotate_from_spare ~shift ~rotate spare
      | 0xc2 ->
          let imm = Lreader.Read.u16 lr in
          Reti imm
      | 0xc3 -> Ret
      | (0xc4 | 0xc5) as x ->
          let gop, spare = read_modrm address_mode lr in
          decode_load_far mode x spare gop
      | 0xc6 -> (
          let dst, spare = read_modrm address_mode lr in
          match spare with
          | 0 ->
              let imm = read_byte_as_int64 lr in
              Mov (`M8, dst, Imm imm)
          | _ -> abort ())
      | 0xc7 -> (
          let dst, spare = read_modrm address_mode lr in
          match spare with
          | 0 ->
              let imode = imode mode in
              let imm = select_reader imode lr in
              Mov (of_mode mode, dst, Imm (Int64.of_int imm))
          | _ -> abort ())
      | 0xc8 ->
          (* enter imm16 imm8 : make stack frame *)
          let alloc = Lreader.Read.u16 lr in
          let level = Lreader.Read.u8 lr in
          Enter (mode, alloc, level)
      | 0xc9 -> Leave mode
      | 0xca ->
          let imm = Lreader.Read.u16 lr in
          Retfi imm
      | 0xcb -> Retf
      | 0xcc -> unsupported "int 3"
      | 0xcd ->
          (* int imm8 *)
          let v = Lreader.Read.u8 lr in
          let msg = Printf.sprintf "int %d" v in
          unsupported msg
      | 0xce -> unsupported "into"
      | 0xcf -> Iret (of_mode mode)
      | 0xd0 ->
          let dst, spare = read_modrm address_mode lr in
          let shift shift_type = Shift (`M8, shift_type, dst, Imm Int64.one)
          and rotate rotate_type =
            Rotate (`M8, rotate_type, dst, Imm Int64.one)
          in
          shift_or_rotate_from_spare ~shift ~rotate spare
      | 0xd1 ->
          let dst, spare = read_modrm address_mode lr in
          let shift shift_type =
            Shift (of_mode mode, shift_type, dst, Imm Int64.one)
          and rotate rotate_type =
            Rotate (of_mode mode, rotate_type, dst, Imm Int64.one)
          in
          shift_or_rotate_from_spare ~shift ~rotate spare
      | 0xd2 ->
          let dst, spare = read_modrm address_mode lr in
          let rotate rotate_type = Rotate (`M8, rotate_type, dst, Reg CL)
          and shift shift_type = Shift (`M8, shift_type, dst, Reg CL) in
          shift_or_rotate_from_spare ~shift ~rotate spare
      | 0xd3 ->
          let dst, spare = read_modrm address_mode lr in
          let rotate rotate_type =
            Rotate (of_mode mode, rotate_type, dst, Reg CL)
          and shift shift_type =
            Shift (of_mode mode, shift_type, dst, Reg CL)
          in
          shift_or_rotate_from_spare ~shift ~rotate spare
      | 0xd4 ->
          let imm = Lreader.Read.u8 lr in
          Aam imm
      | 0xd5 ->
          let imm = Lreader.Read.u8 lr in
          Aad imm
      | 0xd6 -> Salc
      | 0xd7 -> unsupported "xlat/xlatb"
      | 0xd8 -> read_d8 address_mode lr
      | 0xd9 -> read_d9 address_mode lr
      | 0xda -> read_da address_mode lr
      | 0xdb -> read_db address_mode lr
      | 0xdc | 0xdd | 0xde | 0xdf ->
          unsupported_modrm "float op" address_mode lr
      | 0xe0 ->
          let rel8 = Lreader.Read.u8 lr in
          let v = signed_displacement addr lr rel8 |> Int64.of_int in
          Loopnz (mode, address_mode, v)
      | 0xe1 ->
          let rel8 = Lreader.Read.u8 lr in
          let v = signed_displacement addr lr rel8 |> Int64.of_int in
          Loopz (mode, address_mode, v)
      | 0xe2 ->
          let rel8 = Lreader.Read.u8 lr in
          let v = signed_displacement addr lr rel8 |> Int64.of_int in
          Loop (mode, address_mode, v)
      | 0xe3 ->
          let rel8 = Lreader.Read.u8 lr in
          let v = signed_displacement addr lr rel8 |> Int64.of_int in
          Jcxz (of_mode mode, v)
      | 0xe4 -> unsupported_imm "in al imm8" 1 lr
      | 0xe5 -> unsupported_imm "in eax imm8" 1 lr
      | 0xe6 ->
          let imm8 = Lreader.Read.u8 lr in
          OutPortImm imm8
      | 0xe7 -> unsupported_imm "out imm8 eax" 1 lr
      | 0xe8 ->
          let rel32 =
            Int32.to_int (Lreader.Read.i32 lr) land 0xffffffff
            (* TODO: unsafe on 32bit *)
          in
          let v = displacement addr lr rel32 |> Int64.of_int in
          Call v
      | 0xe9 ->
          (* FIXME: Can be 16/32 bits 3-333 *)
          let rel32 =
            Int32.to_int (Lreader.Read.i32 lr) land 0xffffffff
            (* TODO: unsafe on 32bit *)
          in
          let v = displacement addr lr rel32 |> Int64.of_int in
          Jmp v
      | 0xea ->
          let addr = select_reader (imode mode) lr |> Int64.of_int in
          let segment_selector = Lreader.Read.u16 lr in
          Jmpf (segment_selector, addr)
      | 0xeb ->
          let rel8 = Lreader.Read.u8 lr in
          let v = signed_displacement addr lr rel8 |> Int64.of_int in
          Jmp v
      | 0xec -> unsupported "in al dx"
      | 0xed -> unsupported "in eax dx"
      | 0xee -> OutPortDx
      | 0xef -> unsupported "out dx eax"
      | 0xf0 -> aux_read_instr mode address_mode prefix lr
      | 0xf1 -> unsupported "int1/icebp"
      | 0xf2 -> aux_read_instr mode address_mode (Some F2) lr
      | 0xf3 -> aux_read_instr mode address_mode (Some F3) lr
      | 0xf4 -> Halt
      | 0xf5 -> Cmc
      | 0xf6 -> (
          (* Lookup http://ref.x86asm.net/coder32.html#xF6 *)
          let src, spare = read_modrm address_mode lr in
          match spare with
          | 0 | 1 ->
              let v = Lreader.Read.u8 lr |> Int64.of_int in
              Test (`M8, src, Imm v)
          | 2 -> Not (`M8, src)
          | 3 -> Neg (`M8, src)
          | 4 -> Mul (`M8, src)
          | 5 -> IMul (`M8, None, Reg EAX, src)
          | 6 -> Div (`M8, src)
          | 7 -> IDiv (`M8, src)
          | _ -> abort ())
      | 0xf7 -> (
          let src, spare = read_modrm address_mode lr in
          match spare with
          | 0 | 1 ->
              let imm16_32 = select_reader (imode mode) lr |> Int64.of_int in
              Test (of_mode mode, src, Imm imm16_32)
          | 2 -> Not (of_mode mode, src)
          | 3 -> Neg (of_mode mode, src)
          | 4 -> Mul (of_mode mode, src)
          | 5 -> IMul (of_mode mode, None, Reg EAX, src)
          | 6 -> Div (of_mode mode, src)
          | 7 -> IDiv (of_mode mode, src)
          | _ -> abort ())
      | 0xf8 -> Clc
      | 0xf9 -> Stc
      | 0xfa ->
          (* clear interrupt flags *)
          unsupported "cli"
      | 0xfb ->
          (* set interrupt flags *)
          unsupported "sti"
      | 0xfc -> Cld
      | 0xfd -> Std
      | 0xfe -> (
          let gop, spare = read_modrm address_mode lr in
          match spare with
          | 0 -> Inc (`M8, gop)
          | 1 -> Dec (`M8, gop)
          | _ -> abort ())
      | 0xff -> (
          let gop, spare = read_modrm address_mode lr in
          match spare with
          | 0 -> Inc (of_mode mode, gop)
          | 1 -> Dec (of_mode mode, gop)
          | 2 -> DCall gop
          | 3 -> unsupported "callf"
          | 4 -> DJmp gop
          | 5 -> unsupported "jmpf"
          | 6 -> Push (of_mode mode, gop)
          | _ -> abort ())
      | _byte -> abort ()
  in
  (* TODO: mode & address_mode should be settable by caller *)
  let mode = `M32 in
  let address_mode = A32 in
  let mnemonic =
    match aux_read_instr mode address_mode None lr with
    | m -> m
    | exception Decode_abort -> Undecoded
  in
  let hi = Lreader.get_pos lr - 1 in
  let opcode = bytes_to_opcode_string (Lreader.get_slice lr ~lo ~hi) in

  let size = hi - lo + 1 in
  Logger.debug ~level:3 "@[<v 0>Opcode %s (%d) %@ [%08x, %08x[:@ %a@]" opcode
    size addr (addr + size) X86pp.pp_instr mnemonic;
  let instruction = X86Instruction.create size opcode mnemonic in
  (instruction, !sreg)
