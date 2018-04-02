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

val bitsize_of_xmm_mm : X86Types.xmm_mm -> int
val bytesize_of_xmm_mm : X86Types.xmm_mm -> int
val xmm_reg_to_string : X86Types.xmm_reg -> string
val xmm_reg_to_mm_reg : X86Types.xmm_reg -> X86Types.mmx_reg

val bitsize_of_simd_size  : X86Types.simd_size ->  Basic_types.BitSize.t
val bytesize_of_simd_size : X86Types.simd_size -> Basic_types.ByteSize.t

val bytesize_of_szmode : X86Types.sizeMode -> Basic_types.ByteSize.t
val bitsize_of_szmode  : X86Types.sizeMode -> Basic_types.BitSize.t

val bytesize_of_mode : X86Types.mode -> Basic_types.ByteSize.t
val bitsize_of_mode  : X86Types.mode -> Basic_types.BitSize.t



val reg8_to_string : X86Types.reg8 -> string
val reg8_to_int : X86Types.reg8 -> int

val reg16_to_string : X86Types.reg16 -> string

val reg32_to_string : X86Types.reg32 -> string
val reg32_to_string_8 : X86Types.reg32 -> string
val reg32_to_string_16 : X86Types.reg32 -> string

val mm_reg_to_string : X86Types.mmx_reg -> string
val int_to_reg32 : int -> X86Types.reg32
val segment_reg_to_string : X86Types.segment_reg -> string
val int_to_segment_reg : int -> X86Types.segment_reg
val float_reg_to_string : X86Types.float_reg -> string
val int_to_float_reg : int -> X86Types.float_reg
val mmx_reg_to_string : X86Types.mmx_reg -> string
val int_to_mmx_reg : int -> X86Types.mmx_reg
val int_to_xmm_reg : int -> X86Types.xmm_reg
val control_reg_to_string : X86Types.control_reg -> string
val int_to_control_reg : int -> X86Types.control_reg
val debug_reg_to_string : X86Types.debug_reg -> string
val int_to_debug_reg : int -> X86Types.debug_reg
val test_reg_to_string : X86Types.test_reg -> string
val int_to_test_reg : int -> X86Types.test_reg
val flag_to_string : X86Types.flag -> string
val flag_to_offset : X86Types.flag -> int

val cc_to_string : X86Types.cc -> string
val int_to_cc : int -> X86Types.cc
val condition_to_string : X86Types.condition -> string
val int_to_condition : int -> X86Types.condition

val sse_to_string : X86Types.sse -> string
val int_to_sse : int -> X86Types.sse

val scale_to_size : X86Types.scale -> int
val scale_to_string : X86Types.scale -> string

val int_to_scale : int -> X86Types.scale
val arith_op_to_string : X86Types.arith_op -> string
val int_to_arith_op : int -> X86Types.arith_op
val shift_op_to_string : X86Types.shift_op -> string
val rotate_op_to_string : X86Types.rotate_op -> string
val shiftd_op_to_string : X86Types.shiftd_op -> string

val int_to_rotate_op : int -> X86Types.rotate_op
val int_to_shift_op : int -> X86Types.shift_op


val mk_address:
  ?address_mode:X86Types.address_size_mode ->
  ?address_base:X86Types.reg32 option ->
  ?address_index:(X86Types.scale * X86Types.reg32) option ->
  int -> 'a X86Types.genop
(** Default values are:
    - [address_mode]:  [A32]
    - [address_base]:  [None]
    - [address_index]: [None]
*)

val read_modrm :
  X86Types.address_size_mode ->
  Lreader.t -> X86Types.reg32 X86Types.genop * int

val read_modrm_xmm :
  X86Types.address_size_mode ->
  Lreader.t -> X86Types.xmm_reg X86Types.genop * int

val read_rm8_with_spare :
  Lreader.t -> X86Types.reg8 X86Types.genop * int

val read_rm16_with_spare :
  Lreader.t -> X86Types.reg16 X86Types.genop * int


val reg_to_extract : string -> string * int * int
val string_of_register_restrict : string -> int -> int -> string
val get_flag_value : X86Types.flag -> int64 -> int64
