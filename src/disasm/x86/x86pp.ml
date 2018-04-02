(***********************************************************************************)
(*  Copyright (c) 2005, Regents of the University of California                    *)
(*  All rights reserved.                                                           *)
(*                                                                                 *)
(*  Author: Adam Chlipala                                                          *)
(*                                                                                 *)
(*  Redistribution and use in source and binary forms, with or without             *)
(*  modification, are permitted provided that the following conditions are met:    *)
(*                                                                                 *)
(*  - Redistributions of source code must retain the above copyright notice,       *)
(*    this list of conditions and the following disclaimer.                        *)
(*  - Redistributions in binary form must reproduce the above copyright notice,    *)
(*    this list of conditions and the following disclaimer in the documentation    *)
(*    and/or other materials provided with the distribution.                       *)
(*  - Neither the name of the University of California, Berkeley nor the names of  *)
(*    its contributors may be used to endorse or promote products derived from     *)
(*    this software without specific prior written permission.                     *)
(*                                                                                 *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"    *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE      *)
(*  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE     *)
(*  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE       *)
(*  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR            *)
(*  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF           *)
(*  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS       *)
(*  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN        *)
(*  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)        *)
(*  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE     *)
(*  POSSIBILITY OF SUCH DAMAGE.                                                    *)
(*                                                                                 *)
(*                                                                                 *)
(*  Modified for BINSEC                                                            *)
(*                                                                                 *)
(***********************************************************************************)


open Format
open X86Types
open X86Util

let high_bit = Int64.shift_left Int64.one 31
let _higher_bit = Int64.shift_left Int64.one 32

let pp_int64 ppf n =
  if Int64.logand n high_bit = Int64.zero then fprintf ppf "0x%Lx" n
  else
    let mask = Int64.lognot (Int64.shift_left Int64.minus_one 32) in
    fprintf ppf "0x%Lx" (Int64.logand mask n)

let pp_reg_xmm ppf r = fprintf ppf "%s" (xmm_reg_to_string r)
let pp_reg8 ppf r = fprintf ppf "%s" (reg8_to_string r)
let pp_reg16 ppf r = fprintf ppf "%s" (reg16_to_string r)
let pp_reg32 ppf  r = fprintf ppf "%s" (reg32_to_string r)
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
  match addr.addrDisp = Int64.zero, addr.addrBase, addr.addrIndex with
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
    fprintf ppf "[%a + %a + %a]" pp_reg32 r1 pp_reg32 r2 pp_int64 addr.addrDisp
  | false, Some r1, Some (sc, r2) ->
    fprintf ppf "[%a * %a + %a + %a]"
      pp_reg32 r2 pp_scale sc pp_reg32 r1 pp_int64 addr.addrDisp
  | false, None, Some (Scale1, r) ->
    fprintf ppf "[%a + %a]" pp_reg32 r pp_int64 addr.addrDisp
  | false, None, Some (sc, r) ->
    fprintf ppf "[%a * %a + %a]" pp_scale sc pp_reg32 r pp_int64 addr.addrDisp

let pp_genop pp_reg ppf = function
  | Imm n -> pp_int64 ppf n
  | Reg r -> pp_reg ppf r
  | Address addr -> pp_address ppf addr

let pp_genop_xmm  = pp_genop pp_reg_xmm
let pp_genop32    = pp_genop pp_reg32
let pp_genop32_16 = pp_genop pp_reg32_16
let pp_genop32_8  = pp_genop pp_reg32_8
let pp_genop16    = pp_genop pp_reg16
let pp_genop8     = pp_genop pp_reg8

let pp_genop_addr pp_reg ppf = function
  | Imm n -> pp_addr ppf n
  | Reg r -> pp_reg ppf r
  | Address addr -> pp_address ppf addr

let pp_genop_addr32 = pp_genop_addr pp_reg32
let _pp_genop_addr8 = pp_genop_addr pp_reg8

let pp_arith_op ppf aop =
  fprintf ppf "%s" (arith_op_to_string aop)

let pp_shift_op ppf sop =
  fprintf ppf "%s" (shift_op_to_string sop)

let pp_rotate_op ppf rop =
  fprintf ppf "%s" (rotate_op_to_string rop)

let pp_shiftd_op ppf sop =
  fprintf ppf "%s" (shiftd_op_to_string sop)

let pp_rep ppf = function
  | NoRep -> fprintf ppf ""
  |  _ -> fprintf ppf "rep@ "

let pp_genop32 = function
  | `M32 -> pp_genop32
  | `M16 -> pp_genop32_16
  | `M8  -> pp_genop32_8

let pp_instr instr ppf rep =
  match instr with
  | Arith (mode, aop, dst, src) ->
    let pp_genop = pp_genop32 mode in
    fprintf ppf "@[%a@ %a,@ %a@]" pp_arith_op aop pp_genop dst pp_genop src
  | Call (imm)  -> fprintf ppf "@[call@ %a@]" pp_addr imm
  | DCall dst -> fprintf ppf "@[dcall@ %a@]" pp_genop_addr32 dst
  | Cmp (mode, dst, src) ->
    let pp_genop = pp_genop32 mode in
    fprintf ppf "@[cmp@ %a,@ %a@]" pp_genop dst pp_genop src
  | Cmps `M32 -> fprintf ppf "@[cmp@ (esi),@ (edi)@]"
  | Cmps `M16 -> fprintf ppf "@[cmp@ (esi),@ (edi)@]"
  | Cmps `M8 -> fprintf ppf "@[cmp@ (esi),@ (edi)@]"
  | Xadd (mode, dst, src) ->
    let pp_genop = pp_genop32 mode in
    fprintf ppf "@[xadd@ %a,@ %a@]" pp_genop dst pp_genop src
  | Aam imm -> fprintf ppf "@[%02x@]" imm
  | Aad imm -> fprintf ppf "@[%02x@]" imm
  | CmpXchg (mode, dst, src) ->
    let pp_genop = pp_genop32 mode in
    fprintf ppf "@[cmpxchg@ %a,@ %a@]" pp_genop dst pp_genop src
  | Test (mode, dst, src) ->
    let pp_genop = pp_genop32 mode in
    fprintf ppf "@[test@ %a,@ %a@]" pp_genop dst pp_genop src
  | Inc (mode, gop) -> fprintf ppf "@[inc@ %a@]" (pp_genop32 mode) gop
  | Jcc (cc,imm) -> fprintf ppf "@[j%a@ %a@]" pp_cc cc pp_addr imm
  | Jcxz (_,imm) -> fprintf ppf "@[jcxz@ %a@ %a@]" pp_addr imm pp_reg32 ECX
  | Jmp (imm) -> fprintf ppf "@[jmp@ %a@]" pp_addr imm
  | DJmp gop ->
    fprintf ppf "@[djmp@ %a@]" (pp_genop32 `M32) gop
  | Lea (_, dst, src) ->
    fprintf ppf "@[lea@ %a,@ %a@]" pp_reg32 dst pp_address src
  | Leave -> fprintf ppf "@[leave@]"
  | CMovcc (mode, cc, dst, src) ->
    let pp_genop = pp_genop32 mode in
    fprintf ppf "@[cmov%a@ %a,@ %a@]" pp_cc cc pp_genop dst pp_genop src
  | Movaps (_, dst, src) ->
    fprintf ppf "@[mov@ %a,@ %a@]" pp_genop_xmm dst pp_genop_xmm src
  | MovQ _
  | MovdQA _
  | MovdQU _
  | Movd (_,_,_,_)
  | Movlpd (_, _, _)
  | Movhpd (_,_,_)
  | Movlps (_, _, _)
  | Movhps (_,_,_)
  | Movlhps (_,_,_)
  | Movhlps (_, _, _)
  | Movddup (_, _, _)
  | Movsldup (_, _, _)
  | Movshdup (_,_,_) -> fprintf ppf "@[movXmm ... @]"
  | Movups (_,_) -> fprintf ppf "@[movups ....@]"
  | Movupd (_,_) -> fprintf ppf "@[movupd ....@]"
  | Movntq (_,_,_,_) -> fprintf ppf "@[movntq ....@]"
  | CmpXchg8b (_, _, _) -> fprintf ppf "@[cmpxchg8b ...@]"
  | Pshufw (_, _, _, _, _) -> fprintf ppf "@[pshufw ...@]"
  | Pshuflw (_, _, _, _, _) -> fprintf ppf "@[pshuflw ...@]"
  | Pshufhw (_, _, _, _, _) -> fprintf ppf "@[pshufhw ...@]"
  | Pshufd (_, _, _, _, _) -> fprintf ppf "@[pshufd ...@]"
  | Xlat _ -> fprintf ppf "@[Xlat ...@]"
  | Loopnz (_, _, _)
  | Loopz (_, _, _)
  | Loop (_, _, _) -> fprintf ppf "@[loop@]"
  | Psrlw (_,_,_,_)
  | Psrld (_,_,_,_)
  | Psrlq (_,_,_,_)
  | Psllw (_,_,_,_)
  | Pslld (_,_,_,_)
  | Psllq (_,_,_,_)
  | Psraw (_,_,_,_)
  | Psrad (_,_,_,_)
  | Psrldq (_,_)
  | Pslldq (_,_) -> fprintf ppf "@[psr/psl ....@]"
  | Palignr (_, _, _, _, _) -> fprintf ppf "@[palignr ...@]"
  | Pcmpeqb (_, _, _, _) -> fprintf ppf "@[pcmpeqb ...@]"
  | Pcmpeqw (_, _, _, _) -> fprintf ppf "@[pcmpeqw ...@]"
  | Pcmpeqd (_, _, _, _) -> fprintf ppf "@[pcmpeqd ...@]"
  | Pcmpgtb (_, _, _, _) -> fprintf ppf "@[pcmpgtb ...@]"
  | Pcmpgtw (_, _, _, _) -> fprintf ppf "@[pcmpgtw ...@]"
  | Pcmpgtd (_, _, _, _) -> fprintf ppf "@[pcmpgtd ...@]"
  | PmovMSKB (_, _, _, _) -> fprintf ppf "@[pmovmskb ...@]"
  | Pminub (_,_, _, _) -> fprintf ppf "@[pminub ...@]"
  | Pxor (_,_, _, _) -> fprintf ppf "@[pxor ...@]"
  | Por (_,_, _, _) -> fprintf ppf "@[por ...@]"
  | Pand (_,_, _, _) -> fprintf ppf "@[pand ...@]"
  | Pandn (_,_, _, _) -> fprintf ppf "@[pandn ...@]"
  | Psubb (_,_, _, _) -> fprintf ppf "@[psubb ...@]"
  | Ptest (_, _, _, _) -> fprintf ppf "@[ptest ...@]"
  | Pmaxub (_, _, _, _) -> fprintf ppf "@[pmaxub ...@]"
  | Pmaxuw (_, _, _, _) -> fprintf ppf "@[pmaxuw ...@]"
  | Pmaxud (_, _, _, _) -> fprintf ppf "@[pmaxud ...@]"
  | Punpcklbw (_,_, _, _) -> fprintf ppf "@[punpcklbw ...@]"
  | Punpcklwd (_,_, _, _) -> fprintf ppf "@[punpcklwd ...@]"
  | Punpckldq (_,_, _, _) -> fprintf ppf "@[punpckldq ...@]"
  | Popa _ -> fprintf ppf "@[popa@]"
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
  | Movzx (`M16,dst, src) ->
    fprintf ppf "@[movzx %a, %a@]" pp_reg32 dst pp_genop8 src
  | Movs `M8 ->
     fprintf ppf "@[%amovsb [edi] [esi]@]" pp_rep rep
  | Movs _ ->
     fprintf ppf "@[%amovs [edi] [esi]@]" pp_rep rep
  | Lods `M32 ->
    fprintf ppf "@[%alods %s [esi]@]"  pp_rep rep (reg32_to_string EAX)
  | Lods `M16 ->
    fprintf ppf "@[%alods %s [esi]@]"  pp_rep rep (reg16_to_string AX)
  | CBW _ -> fprintf ppf "@[cbw@]"
  | CWD _ -> fprintf ppf "@[cwd@]"
  | PushA _ -> fprintf ppf "@[pushal@]"
  | Pushfd _ -> fprintf ppf "@[pushfd@]"
  | Popfd _ -> fprintf ppf "@[popfd@]"
  | Lods `M8 -> fprintf ppf "@[%alodsb al [esi]@]" pp_rep rep
  | Stos `M32
  | Stos `M16 -> fprintf ppf "@[%astos@]"  pp_rep rep
  | Stos `M8 -> fprintf ppf "@[%astosb@]"  pp_rep rep
  | Scas `M32 
  | Scas `M16 -> fprintf ppf "@[%ascas@]"  pp_rep rep
  | Scas `M8 -> fprintf ppf "@[%ascasb@]"  pp_rep rep
  | PopS reg -> fprintf ppf "@[pop %a@]" pp_segment_reg reg
  | Pop (mode, gop) -> fprintf ppf "@[pop %a@]" (pp_genop32 mode) gop
  | Push (mode, gop) -> fprintf ppf "@[push %a@]" (pp_genop32 mode) gop
  | PushS reg -> fprintf ppf "@[push %a@]" pp_segment_reg reg
  | Ret -> fprintf ppf "@[ret@]"
  | Reti _ -> fprintf ppf "@[reti@]"
  | Retf -> fprintf ppf "@[retf@]"
  | Retfi _ -> fprintf ppf "@[retfi@]"
  | Shift (mode, sop, dst, offset) ->
    fprintf ppf "@[%a@ %a,@ %a@]"
      pp_shift_op sop (pp_genop32 mode) dst pp_genop8 offset
  | Rotate (mode, rop, dst, offset) ->
    fprintf ppf "@[%a@ %a,@ %a@]"
      pp_rotate_op rop (pp_genop32 mode) dst pp_genop8 offset
  | Shiftd (mode, sop, dst, src, offset) ->
    let pp_genop = pp_genop32 mode in
    fprintf ppf "@[%a@ %a,@ %a,@ %a@]"
      pp_shiftd_op sop pp_genop dst pp_genop src
      pp_genop8 offset
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
  | Bt (m, dst, src) ->
    let pp_genop = pp_genop32 m in
    fprintf ppf "@[bt@ %a,@ %a@]" pp_genop dst pp_genop src
  | Bts (m, dst, src) ->
    let pp_genop = pp_genop32 m in
    fprintf ppf "@[bts@ %a,@ %a@]" pp_genop dst pp_genop src
  | Btr (m, dst, src) ->
    let pp_genop = pp_genop32 m in
    fprintf ppf "@[btr@ %a,@ %a@]" pp_genop dst pp_genop src
  | Xchg (`M32, _, _) -> fprintf ppf "@[xchg32@]"
  | Xchg (`M16, _, _) -> fprintf ppf "@[xchg16@]"
  | Xchg (`M8, _, _) -> fprintf ppf "@[xchg8@]"
  | Mul  (mode, src) ->
    fprintf ppf "@[mul@ edx@ eax@ %a@]"   (pp_genop32 mode) src
  | IMul (mode, src) ->
    fprintf ppf "@[imul@ edx@ eax@ %a@]"  (pp_genop32 mode) src
  | Div  (mode, src) ->
    fprintf ppf "@[div@ edx@ eax@ %a@]"   (pp_genop32 mode) src
  | IDiv (mode, src) ->
    fprintf ppf "@[idiv@ edx@ eax@ %a@]"  (pp_genop32 mode) src
  | IMul2 (mode, dst, src) ->
    let pp_genop = pp_genop32 mode in
    fprintf ppf "@[imul@ %a,@ %a@]" pp_genop dst pp_genop src
  | IMul3 (mode, dst, src, imm) ->
    let pp_genop = pp_genop32 mode in
    fprintf ppf "@[imul@ %a@ %a@ %a@]" pp_genop dst pp_genop src pp_genop imm
  | Dec (mode, gop) ->
    fprintf ppf "@[dec@ %a@]" (pp_genop32 mode) gop
  | Bad -> fprintf ppf "@[bad@]"
  | Lsl (mode, dst, src) ->
    let pp_genop = pp_genop32 mode in
    fprintf ppf "@[Lsl@ %a, %a@]" pp_genop dst pp_genop src
  | Fld -> fprintf ppf "@[fld ... unhandled ...@]"
  | Fxch _ -> fprintf ppf "@[fxch sti@]"
  | Lahf -> fprintf ppf "@[lahf@]"
  | Sahf -> fprintf ppf "@[sahf@]"
  | Salc -> fprintf ppf "@[salc@]"
  | Wait -> fprintf ppf "@[wait@]"
  | Unhandled -> fprintf ppf "@[binsec_unhandled@]"

let pp_bytes nbytes ppf i =
  (* At most one word is read *)
  assert (nbytes > 0 && nbytes <= 4);
  let i = Bigint.big_int_of_int i in
  let bytesize = 8 in
  let rec aux ppf j =
    if j < nbytes then
      let byte =
        Bigint.(int_of_big_int (extract_big_int i (j * bytesize) bytesize)) in
      Format.fprintf ppf "%02x %a" byte aux (j + 1)
  in aux ppf 0


let pp_byte = pp_bytes 1
let pp_word = pp_bytes 4

let pp_instr ppf instr = pp_instr instr ppf NoRep
