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

open Format
open X86Types
open X86Util

let high_bit = Int64.shift_left Int64.one 31
let _higher_bit = Int64.shift_left Int64.one 32

let pp_int16 ppf n =
  assert (n lsr 16 = 0);
  fprintf ppf "0x%x" n

let pp_int64 ppf n =
  if Int64.logand n high_bit = Int64.zero then fprintf ppf "0x%Lx" n
  else
    let mask = Int64.lognot (Int64.shift_left Int64.minus_one 32) in
    fprintf ppf "0x%Lx" (Int64.logand mask n)

let pp_reg_xmm ppf r = fprintf ppf "%s" (xmm_reg_to_string r)
let pp_reg8 ppf r = fprintf ppf "%s" (reg8_to_string r)
let pp_reg16 ppf r = fprintf ppf "%s" (reg16_to_string r)
let pp_reg32 ppf r = fprintf ppf "%s" (reg32_to_string r)
let pp_reg32_16 ppf r = fprintf ppf "%s" (reg32_to_string_16 r)
let pp_reg32_8 ppf r = fprintf ppf "%s" (reg32_to_string_8 r)
let pp_segment_reg ppf r = fprintf ppf "%s" (segment_reg_to_string r)
let _pp_float_reg ppf r = fprintf ppf "%s" (float_reg_to_string r)
let _pp_mmx_reg ppf r = fprintf ppf "%s" (mmx_reg_to_string r)
let _pp_control_reg ppf r = fprintf ppf "%s" (control_reg_to_string r)
let _pp_debug_reg ppf r = fprintf ppf "%s" (debug_reg_to_string r)
let _pp_test_reg ppf r = fprintf ppf "%s" (test_reg_to_string r)
let pp_cc ppf r = fprintf ppf "%s" (cc_to_string r)
let _pp_sse ppf r = fprintf ppf "%s" (sse_to_string r)
let pp_scale ppf r = fprintf ppf "%s" (scale_to_string r)
let pp_addr ppf n = fprintf ppf "0x%Lx" n

let pp_address ppf addr =
  match (addr.addrDisp = Int64.zero, addr.addrBase, addr.addrIndex) with
  | true, None, None -> fprintf ppf "[0]"
  | true, Some r, None -> fprintf ppf "[%a]" pp_reg32 r
  | true, Some r1, Some (Scale1, r2) ->
      fprintf ppf "[%a + %a]" pp_reg32 r1 pp_reg32 r2
  | true, Some r1, Some (sc, r2) ->
      fprintf ppf "[%a + %a * %a]" pp_reg32 r1 pp_scale sc pp_reg32 r2
  | true, None, Some (Scale1, r) -> fprintf ppf "[%a]" pp_reg32 r
  | true, None, Some (sc, r) -> fprintf ppf "[%a * %a]" pp_scale sc pp_reg32 r
  | false, None, None -> fprintf ppf "[%a]" pp_addr addr.addrDisp
  | false, Some r, None ->
      fprintf ppf "[%a + %a]" pp_reg32 r pp_int64 addr.addrDisp
  | false, Some r1, Some (Scale1, r2) ->
      fprintf ppf "[%a + %a + %a]" pp_reg32 r1 pp_reg32 r2 pp_int64
        addr.addrDisp
  | false, Some r1, Some (sc, r2) ->
      fprintf ppf "[%a * %a + %a + %a]" pp_reg32 r2 pp_scale sc pp_reg32 r1
        pp_int64 addr.addrDisp
  | false, None, Some (Scale1, r) ->
      fprintf ppf "[%a + %a]" pp_reg32 r pp_int64 addr.addrDisp
  | false, None, Some (sc, r) ->
      fprintf ppf "[%a * %a + %a]" pp_scale sc pp_reg32 r pp_int64 addr.addrDisp

let pp_genop pp_reg ppf = function
  | Imm n -> pp_int64 ppf n
  | Reg r -> pp_reg ppf r
  | Address addr -> pp_address ppf addr

let pp_genop_xmm = pp_genop pp_reg_xmm
let pp_genop32 = pp_genop pp_reg32
let pp_genop32_16 = pp_genop pp_reg32_16
let pp_genop32_8 = pp_genop pp_reg32_8
let pp_genop16 = pp_genop pp_reg16
let pp_genop8 = pp_genop pp_reg8

let pp_genop_addr pp_reg ppf = function
  | Imm n -> pp_addr ppf n
  | Reg r -> pp_reg ppf r
  | Address addr -> pp_address ppf addr

let pp_genop_addr32 = pp_genop_addr pp_reg32
let _pp_genop_addr8 = pp_genop_addr pp_reg8
let pp_arith_op ppf aop = fprintf ppf "%s" (arith_op_to_string aop)
let pp_shift_op ppf sop = fprintf ppf "%s" (shift_op_to_string sop)
let pp_rotate_op ppf rop = fprintf ppf "%s" (rotate_op_to_string rop)
let pp_shiftd_op ppf sop = fprintf ppf "%s" (shiftd_op_to_string sop)

let pp_rep ppf = function
  | NoRep -> fprintf ppf ""
  | Rep -> fprintf ppf "rep@ "
  | RepE -> fprintf ppf "repe@ "
  | RepNE -> fprintf ppf "repne@ "

let pp_genop32 = function
  | `M32 -> pp_genop32
  | `M16 -> pp_genop32_16
  | `M8 -> pp_genop32_8

let pp_ext0 ppf = function
  | 8 -> fprintf ppf "b"
  | 16 -> fprintf ppf "w"
  | 32 -> fprintf ppf "d"
  | 64 -> fprintf ppf "q"
  | _ -> assert false

let pp_ext1 ppf = function
  | 8 -> fprintf ppf "bw"
  | 16 -> fprintf ppf "wd"
  | 32 -> fprintf ppf "dq"
  | 64 -> fprintf ppf "qdq"
  | _ -> assert false

let pp_ext2 ppf = function
  | 8 -> fprintf ppf "wb"
  | 16 -> fprintf ppf "dw"
  | _ -> assert false

let pp_instr ppf instr =
  match instr with
  | Arith (mode, aop, dst, src) ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[%a@ %a,@ %a@]" pp_arith_op aop pp_genop dst pp_genop src
  | Call imm -> fprintf ppf "@[call@ %a@]" pp_addr imm
  | DCall dst -> fprintf ppf "@[dcall@ %a@]" pp_genop_addr32 dst
  | Cmp (mode, dst, src) ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[cmp@ %a,@ %a@]" pp_genop dst pp_genop src
  | Cmps _ -> fprintf ppf "@[cmp@ (esi),@ (edi)@]"
  | Xadd (mode, dst, src) ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[xadd@ %a,@ %a@]" pp_genop dst pp_genop src
  | Aas -> fprintf ppf "@[aas@]"
  | Aam imm -> fprintf ppf "@[%02x@]" imm
  | Aad imm -> fprintf ppf "@[%02x@]" imm
  | CmpXchg (mode, dst, src) ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[cmpxchg@ %a,@ %a@]" pp_genop dst pp_genop src
  | Test (mode, dst, src) ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[test@ %a,@ %a@]" pp_genop dst pp_genop src
  | Inc (mode, gop) -> fprintf ppf "@[inc@ %a@]" (pp_genop32 mode) gop
  | Jcc (cc, imm) -> fprintf ppf "@[j%a@ %a@]" pp_cc cc pp_addr imm
  | Jcxz (_, imm) -> fprintf ppf "@[jcxz@ %a@ %a@]" pp_addr imm pp_reg32 ECX
  | Jmp imm -> fprintf ppf "@[jmp@ %a@]" pp_addr imm
  | DJmp gop -> fprintf ppf "@[djmp@ %a@]" (pp_genop32 `M32) gop
  | Jmpf (seg_selector, imm) ->
      fprintf ppf "@[jmp@ %a:%a@]" pp_int16 seg_selector pp_addr imm
  | Lea (_, dst, src) ->
      fprintf ppf "@[lea@ %a,@ %a@]" pp_reg32 dst pp_address src
  | LoadFarPointer (mode, sreg, reg, address) ->
      let pp_reg ppf r = pp_genop32 mode ppf (Reg r) in
      fprintf ppf "@[l%a@ %a,@ %a@]" pp_segment_reg sreg pp_reg reg pp_address
        address
  | Enter (_, alloc, level) -> fprintf ppf "@[enter %d, %d@]" alloc level
  | Leave _ -> fprintf ppf "@[leave@]"
  | CMovcc (mode, cc, dst, src) ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[cmov%a@ %a,@ %a@]" pp_cc cc pp_genop dst pp_genop src
  | Movaps (_, dst, src) ->
      fprintf ppf "@[mov@ %a,@ %a@]" pp_genop_xmm dst pp_genop_xmm src
  | MovQ _ -> fprintf ppf "@[movq .. @]"
  | MovdQA _ | MovdQU _
  | Movd (_, _, _, _)
  | Movlpd (_, _, _)
  | Movhpd (_, _, _)
  | Movlps (_, _, _)
  | Movhps (_, _, _)
  | Movlhps (_, _, _)
  | Movhlps (_, _, _)
  | Movddup (_, _, _)
  | Movsldup (_, _, _)
  | Movshdup (_, _, _) ->
      fprintf ppf "@[movXmm ... @]"
  | Movups (_, _) -> fprintf ppf "@[movups ....@]"
  | Movupd (_, _) -> fprintf ppf "@[movupd ....@]"
  | Movntq (_, _, _, _) -> fprintf ppf "@[movntq ....@]"
  | CmpXchg8b (_, _, _) -> fprintf ppf "@[cmpxchg8b ...@]"
  | Pshufw (_, _, _, _, _) -> fprintf ppf "@[pshufw ...@]"
  | Pshuflw (_, _, _, _, _) -> fprintf ppf "@[pshuflw ...@]"
  | Pshufhw (_, _, _, _, _) -> fprintf ppf "@[pshufhw ...@]"
  | Pshufd (_, _, _, _, _) -> fprintf ppf "@[pshufd ...@]"
  | Xlat _ -> fprintf ppf "@[Xlat ...@]"
  | Loopnz (_, _, _) | Loopz (_, _, _) | Loop (_, _, _) ->
      fprintf ppf "@[loop@]"
  | Psrl (_, _, _, _, ext) -> fprintf ppf "@[psrl%a ...@]" pp_ext0 ext
  | Psll (_, _, _, _, ext) -> fprintf ppf "@[psll%a ...@]" pp_ext0 ext
  | Psra (_, _, _, _, ext) -> fprintf ppf "@[psra%a ...@]" pp_ext0 ext
  | Psrldq (_, _) -> fprintf ppf "@[psrldq ...@]"
  | Pslldq (_, _) -> fprintf ppf "@[pslldq ...@]"
  | Palignr (_xmm, _, dst, src, imm) ->
      fprintf ppf "@[palignr %a,%a,0x%x@]" pp_genop_xmm dst pp_genop_xmm src imm
  | Pcmpeqb (_, _, _, _) -> fprintf ppf "@[pcmpeqb ...@]"
  | Pcmpeqw (_, _, _, _) -> fprintf ppf "@[pcmpeqw ...@]"
  | Pcmpeqd (_, _, _, _) -> fprintf ppf "@[pcmpeqd ...@]"
  | Pcmpgtb (_, _, _, _) -> fprintf ppf "@[pcmpgtb ...@]"
  | Pcmpgtw (_, _, _, _) -> fprintf ppf "@[pcmpgtw ...@]"
  | Pcmpgtd (_, _, _, _) -> fprintf ppf "@[pcmpgtd ...@]"
  | PmovMSKB (_, _, _, _) -> fprintf ppf "@[pmovmskb ...@]"
  | Pminu (_, _, _, _, e) -> fprintf ppf "@[pminu%a ...@]" pp_ext0 e
  | Pmins (_, _, _, _, e) -> fprintf ppf "@[pmins%a ...@]" pp_ext0 e
  | Pxor (_, _, _, _) -> fprintf ppf "@[pxor ...@]"
  | Por (_, _, _, _) -> fprintf ppf "@[por ...@]"
  | Pand (_, _, _, _) -> fprintf ppf "@[pand ...@]"
  | Pandn (_, _, _, _) -> fprintf ppf "@[pandn ...@]"
  | Padd (_, _, _, _, _) -> fprintf ppf "@[padd ...@]"
  | Padds (_, _, _, _, _) -> fprintf ppf "@[padds ...@]"
  | Paddus (_, _, _, _, _) -> fprintf ppf "@[paddus ...@]"
  | Psub (_, _, _, _, _) -> fprintf ppf "@[psub ...@]"
  | Psubs (_, _, _, _, _) -> fprintf ppf "@[psubs ...@]"
  | Psubus (_, _, _, _, _) -> fprintf ppf "@[psubus ...@]"
  | Pavgu (_, _, _, _, e) -> fprintf ppf "@[pavgu%a ...@]" pp_ext0 e
  | Pmulhw (_, _, _, _) -> fprintf ppf "@[pmulhw ...@]"
  | Pmulhrw (_, _, _, _) -> fprintf ppf "@[pmulhrw ...@]"
  | Pmullw (_, _, _, _) -> fprintf ppf "@[pmullw ...@]"
  | Pmuludq (_, _, _, _) -> fprintf ppf "@[pmuludq ...@]"
  | Ptest (_, _, _, _) -> fprintf ppf "@[ptest ...@]"
  | Pmaxu (_, _, _, _, e) -> fprintf ppf "@[pmaxu%a ...@]" pp_ext0 e
  | Pmaxs (_, _, _, _, e) -> fprintf ppf "@[pmaxs%a ...@]" pp_ext0 e
  | Punpckl (_, _, _, _, ext) -> fprintf ppf "@[punpckl%a ...@]" pp_ext1 ext
  | Punpckh (_, _, _, _, ext) -> fprintf ppf "@[punpckh%a ...@]" pp_ext1 ext
  | Packus (_, _, _, _, ext) -> fprintf ppf "@[packus%a ...@]" pp_ext2 ext
  | Packss (_, _, _, _, ext) -> fprintf ppf "@[packss%a ...@]" pp_ext2 ext
  | Pmaddwd _ -> fprintf ppf "@[pmaddwd ...@]"
  | Pmaddusbsw _ -> fprintf ppf "@[pmaddusbsw ...@]"
  | Pclmulqdq _ -> fprintf ppf "@[pclmulqdq ...@]"
  | Bswap (_, dst) -> fprintf ppf "@[bswap@ %a@]" pp_reg32 dst
  | Bsr (mode, dst, src) ->
      fprintf ppf "@[bsr@ %a, %a@]" pp_reg32 dst (pp_genop32 mode) src
  | Bsf (mode, dst, src) ->
      fprintf ppf "@[bsf@ %a, %a@]" pp_reg32 dst (pp_genop32 mode) src
  | Mov (mode, dst, src) ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[mov@ %a, %a@]" pp_genop dst pp_genop src
  | MovSegRight (dst, src) ->
      fprintf ppf "@[mov@ %a, %a@]" pp_genop16 dst pp_segment_reg src
  | MovSegLeft (dst, src) ->
      fprintf ppf "@[mov@ %a, %a@]" pp_segment_reg dst pp_genop16 src
  | Movsx (_, dst, src) ->
      fprintf ppf "@[movsx@ %a, %a@]" pp_reg32 dst pp_genop8 src
  | Movsx16 (_, dst, src) ->
      fprintf ppf "@[movsx@ %a, %a@]" pp_reg32 dst pp_genop16 src
  | Movzx16 (`M32, dst, src) ->
      fprintf ppf "@[movzx %a, %a@]" pp_reg32 dst pp_genop16 src
  | Movzx16 (`M16, dst, src) ->
      fprintf ppf "@[movzx %a, %a@]" pp_reg32_16 dst pp_genop16 src
  | Movzx (`M32, dst, src) ->
      fprintf ppf "@[movzx %a, %a@]" pp_reg32 dst pp_genop8 src
  | Movzx (`M16, dst, src) ->
      fprintf ppf "@[movzx %a, %a@]" pp_reg32 dst pp_genop8 src
  | Movs (rep, `M8) -> fprintf ppf "@[%amovsb [edi] [esi]@]" pp_rep rep
  | Movs (rep, _) -> fprintf ppf "@[%amovs [edi] [esi]@]" pp_rep rep
  | Lods (rep, `M32) ->
      fprintf ppf "@[%alods %s [esi]@]" pp_rep rep (reg32_to_string EAX)
  | Lods (rep, `M16) ->
      fprintf ppf "@[%alods %s [esi]@]" pp_rep rep (reg16_to_string AX)
  | CBW _ -> fprintf ppf "@[cbw@]"
  | CWD _ -> fprintf ppf "@[cwd@]"
  | PushA _ -> fprintf ppf "@[pushal@]"
  | PopA _ -> fprintf ppf "@[popa@]"
  | Pushfd _ -> fprintf ppf "@[pushfd@]"
  | Popfd _ -> fprintf ppf "@[popfd@]"
  | Lods (rep, `M8) -> fprintf ppf "@[%alodsb al [esi]@]" pp_rep rep
  | Stos (rep, `M32) | Stos (rep, `M16) -> fprintf ppf "@[%astos@]" pp_rep rep
  | Stos (rep, `M8) -> fprintf ppf "@[%astosb@]" pp_rep rep
  | Scas (rep, `M32) | Scas (rep, `M16) -> fprintf ppf "@[%ascas@]" pp_rep rep
  | Scas (rep, `M8) -> fprintf ppf "@[%ascasb@]" pp_rep rep
  | PopS reg -> fprintf ppf "@[pop %a@]" pp_segment_reg reg
  | Pop (mode, gop) -> fprintf ppf "@[pop %a@]" (pp_genop32 mode) gop
  | Push (mode, gop) -> fprintf ppf "@[push %a@]" (pp_genop32 mode) gop
  | PushS reg -> fprintf ppf "@[push %a@]" pp_segment_reg reg
  | Ret -> fprintf ppf "@[ret@]"
  | Reti _ -> fprintf ppf "@[reti@]"
  | Retf -> fprintf ppf "@[retf@]"
  | Retfi _ -> fprintf ppf "@[retfi@]"
  | Shift (mode, sop, dst, offset) ->
      fprintf ppf "@[%a@ %a,@ %a@]" pp_shift_op sop (pp_genop32 mode) dst
        pp_genop8 offset
  | Rotate (mode, rop, dst, offset) ->
      fprintf ppf "@[%a@ %a,@ %a@]" pp_rotate_op rop (pp_genop32 mode) dst
        pp_genop8 offset
  | Shiftd (mode, sop, dst, src, offset) ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[%a@ %a,@ %a,@ %a@]" pp_shiftd_op sop pp_genop dst pp_genop
        src pp_genop8 offset
  | SetCc (cc, dst) -> fprintf ppf "@[set%a@ %a@]" pp_cc cc pp_genop8 dst
  | Nop -> fprintf ppf "@[nop@]"
  | Not (mode, src) -> fprintf ppf "@[not@ %a@]" (pp_genop32 mode) src
  | Neg (mode, src) -> fprintf ppf "@[neg@ %a@]" (pp_genop32 mode) src
  | Halt -> fprintf ppf "@[hlt@]"
  | Cmc -> fprintf ppf "@[cmc@]"
  | Clc -> fprintf ppf "@[clc@]"
  | Stc -> fprintf ppf "@[stc@]"
  | Cld -> fprintf ppf "@[cld@]"
  | Std -> fprintf ppf "@[std@]"
  | Bt { mode; dst; src } ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[bt@ %a,@ %a@]" pp_genop dst pp_genop src
  | Bts { mode; dst; src } ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[bts@ %a,@ %a@]" pp_genop dst pp_genop src
  | Btr { mode; dst; src } ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[btr@ %a,@ %a@]" pp_genop dst pp_genop src
  | Btc { mode; src; dst } ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[btc@ %a,@ %a@]" pp_genop dst pp_genop src
  | Xchg (`M32, _, _) -> fprintf ppf "@[xchg32@]"
  | Xchg (`M16, _, _) -> fprintf ppf "@[xchg16@]"
  | Xchg (`M8, _, _) -> fprintf ppf "@[xchg8@]"
  | Mul (mode, src) -> fprintf ppf "@[mul@ edx@ eax@ %a@]" (pp_genop32 mode) src
  | IMul (mode, None, _, src) ->
      fprintf ppf "@[imul@ edx@ eax@ %a@]" (pp_genop32 mode) src
  | Div (mode, src) -> fprintf ppf "@[div@ edx@ eax@ %a@]" (pp_genop32 mode) src
  | IDiv (mode, src) ->
      fprintf ppf "@[idiv@ edx@ eax@ %a@]" (pp_genop32 mode) src
  | IMul (mode, Some dst, src, imm) ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[imul@ %a@ %a@ %a@]" pp_genop dst pp_genop src pp_genop imm
  | Dec (mode, gop) -> fprintf ppf "@[dec@ %a@]" (pp_genop32 mode) gop
  | Lsl (mode, dst, src) ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[Lsl@ %a, %a@]" pp_genop dst pp_genop src
  | Fld -> fprintf ppf "@[fld ... unhandled ...@]"
  | Fxch _ -> fprintf ppf "@[fxch sti@]"
  | Lahf -> fprintf ppf "@[lahf@]"
  | Sahf -> fprintf ppf "@[sahf@]"
  | Salc -> fprintf ppf "@[salc@]"
  | Wait -> fprintf ppf "@[wait@]"
  | Emms -> fprintf ppf "@[emms@]"
  | Prefetch suffix -> fprintf ppf "@[prefetch%s@]" suffix
  | Popcnt (mode, dst, src) ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[popcnt@ %a,@ %a@]" pp_genop dst pp_genop src
  | Lzcnt (mode, dst, src) ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[lzcnt@ %a,@ %a@]" pp_genop dst pp_genop src
  | Lgdt (mode, dst) ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[lgdt@ %a@]" pp_genop dst
  | Lidt (mode, dst) ->
      let pp_genop = pp_genop32 mode in
      fprintf ppf "@[lidt@ %a@]" pp_genop dst
  | Ltr src -> fprintf ppf "@[ltr@ %a@]" pp_genop16 src
  | Iret _ -> fprintf ppf "@[iret@]"
  | OutPortImm imm8 -> fprintf ppf "@[outb@ %x,@ al@]" imm8
  | OutPortDx -> fprintf ppf "@[outb@ dx,@ al@]"
  | Undecoded -> fprintf ppf "@[binsec_undecoded@]"
  | Unsupported descr -> fprintf ppf "@[binsec_unsupported %s@]" descr

let pp_bytes nbytes ppf i =
  (* At most one word is read *)
  assert (nbytes > 0 && nbytes <= 4);
  let i = Z.of_int i in
  let bytesize = 8 in
  let rec aux ppf j =
    if j < nbytes then
      let byte = Z.to_int (Z.extract i (j * bytesize) bytesize) in
      Format.fprintf ppf "%02x %a" byte aux (j + 1)
  in
  aux ppf 0

let pp_byte = pp_bytes 1
let pp_word = pp_bytes 4
