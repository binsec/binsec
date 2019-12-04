(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

(** X86 utility functions *)


val bitsize_of_xmm_mm : X86Types.xmm_mm -> int
val bytesize_of_xmm_mm : X86Types.xmm_mm -> int
val xmm_reg_to_string : X86Types.xmm_reg -> string
val xmm_reg_to_mm_reg : X86Types.xmm_reg -> X86Types.mmx_reg

val bitsize_of_simd_size  : X86Types.simd_size ->  Size.Bit.t
val bytesize_of_simd_size : X86Types.simd_size -> Size.Byte.t

val bytesize_of_szmode : X86Types.sizeMode -> Size.Byte.t
val bitsize_of_szmode  : X86Types.sizeMode -> Size.Bit.t

val bytesize_of_mode : X86Types.mode -> Size.Byte.t
val bitsize_of_mode  : X86Types.mode -> Size.Bit.t

val bytesize_of_address_mode : X86Types.address_size_mode -> Size.Byte.t
val bitsize_of_address_mode  : X86Types.address_size_mode -> Size.Bit.t


val reg8_to_string : X86Types.reg8 -> string
val reg8_to_int : X86Types.reg8 -> int

val reg16_to_string : X86Types.reg16 -> string
val reg16_to_reg32 : X86Types.reg16 -> X86Types.reg32


val reg32_to_string : X86Types.reg32 -> string
val reg32_to_string_8 : X86Types.reg32 -> string
val reg32_to_string_16 : X86Types.reg32 -> string

val mm_reg_to_string : X86Types.mmx_reg -> string
val int_to_reg32 : int -> X86Types.reg32

val segment_reg_to_string : X86Types.segment_reg -> string

val segment_of_string : string -> X86Types.segment_reg option
(** [segment_of_string s] returns the [Some segment_reg] if [s] is a valid
    segment name lie "fs" or "gs", [None] otherwise

    A valid name is the 2 letters string representation of the algebraic data
    type [X86Types.segment_reg].
*)

val segments : X86Types.segment_reg list
(** The list of of possible segement registers *)

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

val switch_default_data_mode : X86Types.mode -> X86Types.mode
(** Handle 0x66 prefix byte *)

val switch_address_mode :
  X86Types.address_size_mode -> X86Types.address_size_mode
(** Handle 0x67 prefix byte *)
