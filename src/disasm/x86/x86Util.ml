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

(* Utility functions *)

open X86Types

let bitsize_of_xmm_mm = function
  | XMM -> 128
  | MM -> 64

let bytesize_of_xmm_mm = function
  | XMM -> 16
  | MM -> 8

let bytesize_of_simd_size = function
  | S32  -> Basic_types.ByteSize.create 4
  | S64  -> Basic_types.ByteSize.create 8
  | S128 -> Basic_types.ByteSize.create 16

let bitsize_of_simd_size simd_size =
  bytesize_of_simd_size simd_size |> Basic_types.ByteSize.to_bitsize

let bytesize_of_szmode = function
  | `M8  -> Basic_types.ByteSize.create 1
  | `M16 -> Basic_types.ByteSize.create 2
  | `M32 -> Basic_types.ByteSize.create 4

let bitsize_of_szmode mode =
  bytesize_of_szmode mode |> Basic_types.ByteSize.to_bitsize

let bytesize_of_mode = function
  | `M16 -> Basic_types.ByteSize.create 2
  | `M32 -> Basic_types.ByteSize.create 4

let bitsize_of_mode mode =
  bytesize_of_mode mode |> Basic_types.ByteSize.to_bitsize

let reg8_to_int = function
  | AL -> 0
  | CL -> 1
  | DL -> 2
  | BL -> 3
  | AH -> 4
  | CH -> 5
  | DH -> 6
  | BH -> 7

let reg8_to_string = function
    AL -> "al"
  | CL -> "cl"
  | DL -> "dl"
  | BL -> "bl"
  | AH -> "ah"
  | CH -> "ch"
  | DH -> "dh"
  | BH -> "bh"

let int_to_reg8 = function
    0 -> AL
  | 1 -> CL
  | 2 -> DL
  | 3 -> BL
  | 4 -> AH
  | 5 -> CH
  | 6 -> DH
  | 7 -> BH
  | _ -> raise (Invalid_argument "int_to_reg8")

let _reg16_to_int = function
    AX -> 0
  | CX -> 1
  | DX -> 2
  | BX -> 3
  | SP -> 4
  | BP -> 5
  | SI -> 6
  | DI -> 7

let reg16_to_string = function
    AX -> "ax"
  | CX -> "cx"
  | DX -> "dx"
  | BX -> "bx"
  | SP -> "sp"
  | BP -> "bp"
  | SI -> "si"
  | DI -> "di"

let int_to_reg16 = function
    0 -> AX
  | 1 -> CX
  | 2 -> DX
  | 3 -> BX
  | 4 -> SP
  | 5 -> BP
  | 6 -> SI
  | 7 -> DI
  | _ -> raise (Invalid_argument "int_to_ret16")

let _reg32_to_int = function
    EAX -> 0
  | ECX -> 1
  | EDX -> 2
  | EBX -> 3
  | ESP -> 4
  | EBP -> 5
  | ESI -> 6
  | EDI -> 7

let reg32_to_string = function
    EAX -> "eax"
  | ECX -> "ecx"
  | EDX -> "edx"
  | EBX -> "ebx"
  | ESP -> "esp"
  | EBP -> "ebp"
  | ESI -> "esi"
  | EDI -> "edi"

let reg32_to_string_8 = function
    EAX -> "al"
  | ECX -> "cl"
  | EDX -> "dl"
  | EBX -> "bl"
  | ESP -> "ah"
  | EBP -> "ch"
  | ESI -> "dh"
  | EDI -> "bh"



let reg32_to_string_16 = function
    EAX -> "ax"
  | ECX -> "cx"
  | EDX -> "dx"
  | EBX -> "bx"
  | ESP -> "sp"
  | EBP -> "bp"
  | ESI -> "si"
  | EDI -> "di"


let xmm_reg_to_mm_reg = function
    XMM0 -> MM0
  | XMM1 -> MM1
  | XMM2 -> MM2
  | XMM3 -> MM3
  | XMM4 -> MM4
  | XMM5 -> MM5
  | XMM6 -> MM6
  | XMM7 -> MM7


let xmm_reg_to_string = function
    XMM0 -> "xmm0"
  | XMM1 -> "xmm1"
  | XMM2 -> "xmm2"
  | XMM3 -> "xmm3"
  | XMM4 -> "xmm4"
  | XMM5 -> "xmm5"
  | XMM6 -> "xmm6"
  | XMM7 -> "xmm7"


let mm_reg_to_string = function
    MM0 -> "mm0"
  | MM1 -> "mm1"
  | MM2 -> "mm2"
  | MM3 -> "mm3"
  | MM4 -> "mm4"
  | MM5 -> "mm5"
  | MM6 -> "mm6"
  | MM7 -> "mm7"


let int_to_reg32 = function
    0 -> EAX
  | 1 -> ECX
  | 2 -> EDX
  | 3 -> EBX
  | 4 -> ESP
  | 5 -> EBP
  | 6 -> ESI
  | 7 -> EDI
  | n ->
    Logger.debug "n = %d\n" n;
    raise (Invalid_argument "int_to_reg32\n")

let _segment_reg_to_int = function
    ES -> 0
  | CS -> 1
  | SS -> 2
  | DS -> 3
  | FS -> 4
  | GS -> 5

let segment_reg_to_string = function
    ES -> "es"
  | CS -> "cs"
  | SS -> "ss"
  | DS -> "ds"
  | FS -> "fs"
  | GS -> "gs"

let int_to_segment_reg = function
  | 0 -> ES
  | 1 -> CS
  | 2 -> SS
  | 3 -> DS
  | 4 -> FS
  | 5 -> GS
  | n ->
    let msg = Format.sprintf "int_to_segment_reg %d" n in
    raise (Invalid_argument msg)


let _float_reg_to_int = function
    ST0 -> 0
  | ST1 -> 1
  | ST2 -> 2
  | ST3 -> 3
  | ST4 -> 4
  | ST5 -> 5
  | ST6 -> 6
  | ST7 -> 7

let float_reg_to_string = function
    ST0 -> "st0"
  | ST1 -> "st1"
  | ST2 -> "st2"
  | ST3 -> "st3"
  | ST4 -> "st4"
  | ST5 -> "st5"
  | ST6 -> "st6"
  | ST7 -> "st7"

let int_to_float_reg = function
    0 -> ST0
  | 1 -> ST1
  | 2 -> ST2
  | 3 -> ST3
  | 4 -> ST4
  | 5 -> ST5
  | 6 -> ST6
  | 7 -> ST7
  | _ -> raise (Invalid_argument "int_to_float_reg")

let _mmx_reg_to_int = function
    MM0 -> 0
  | MM1 -> 1
  | MM2 -> 2
  | MM3 -> 3
  | MM4 -> 4
  | MM5 -> 5
  | MM6 -> 6
  | MM7 -> 7

let mmx_reg_to_string = function
    MM0 -> "MM0"
  | MM1 -> "MM1"
  | MM2 -> "MM2"
  | MM3 -> "MM3"
  | MM4 -> "MM4"
  | MM5 -> "MM5"
  | MM6 -> "MM6"
  | MM7 -> "MM7"

let int_to_mmx_reg = function
    0 -> MM0
  | 1 -> MM1
  | 2 -> MM2
  | 3 -> MM3
  | 4 -> MM4
  | 5 -> MM5
  | 6 -> MM6
  | 7 -> MM7
  | _ -> raise (Invalid_argument "int_to_mmx_reg")

let int_to_xmm_reg = function
    0 -> XMM0
  | 1 -> XMM1
  | 2 -> XMM2
  | 3 -> XMM3
  | 4 -> XMM4
  | 5 -> XMM5
  | 6 -> XMM6
  | 7 -> XMM7
  | _ -> raise (Invalid_argument "int_to_mmx_reg")

let _control_reg_to_int = function
    CR0 -> 0
  | CR2 -> 2
  | CR3 -> 3
  | CR4 -> 4

let control_reg_to_string = function
    CR0 -> "CR0"
  | CR2 -> "CR2"
  | CR3 -> "CR3"
  | CR4 -> "CR4"

let int_to_control_reg = function
    0 -> CR0
  | 2 -> CR2
  | 3 -> CR3
  | 4 -> CR4
  | _ -> raise (Invalid_argument "int_to_control_reg")

let _debug_reg_to_int = function
    DR0 -> 0
  | DR1 -> 1
  | DR2 -> 2
  | DR3 -> 3
  | DR6 -> 6
  | DR7 -> 7

let debug_reg_to_string = function
    DR0 -> "DR0"
  | DR1 -> "DR1"
  | DR2 -> "DR2"
  | DR3 -> "DR3"
  | DR6 -> "DR6"
  | DR7 -> "DR7"

let int_to_debug_reg = function
    0 -> DR0
  | 1 -> DR1
  | 2 -> DR2
  | 3 -> DR3
  | 6 -> DR6
  | 7 -> DR7
  | _ -> raise (Invalid_argument "int_to_debug_reg")

let _test_reg_to_int = function
    TR3 -> 3
  | TR4 -> 4
  | TR5 -> 5
  | TR6 -> 6
  | TR7 -> 7

let test_reg_to_string = function
    TR3 -> "TR3"
  | TR4 -> "TR4"
  | TR5 -> "TR5"
  | TR6 -> "TR6"
  | TR7 -> "TR7"

 let int_to_test_reg = function
    3 -> TR3
  | 4 -> TR4
  | 5 -> TR5
  | 6 -> TR6
  | 7 -> TR7
  | _ -> raise (Invalid_argument "int_to_test_reg")

let flag_to_string = function
  | ID   -> "ID"
  | VIP  -> "VIP"
  | VIF  -> "VIF"
  | AC   -> "AC"
  | VM   -> "VM"
  | RF   -> "RF"
  | NT   -> "NT"
  | IOPL -> "IOPL"
  | OF   -> "OF"
  | DF   -> "DF"
  | IF   -> "IF"
  | TF   -> "TF"
  | SF   -> "SF"
  | ZF   -> "ZF"
  | AF   -> "AF"
  | PF   -> "PF"
  | CF   -> "CF"


let flag_to_offset = function (* following the order in the eflags register *)
    ID -> 21
  | VIP -> 20
  | VIF -> 19
  | AC -> 18
  | VM -> 17
  | RF -> 16
  | NT -> 14
  | IOPL -> 12
  | OF -> 11
  | DF -> 10
  | IF -> 9
  | TF -> 8
  | SF -> 7
  | ZF -> 6
  | AF -> 4
  | PF -> 2
  | CF -> 0


let get_flag_value flag eflag =
  let pos = flag_to_offset flag in
  let mask = Int64.shift_left Int64.one pos in
  let value = Int64.logand eflag mask in
  Int64.shift_right_logical value pos


let cc_to_string cc =
  match cc.truth_value, cc.condition with
  | true  , O  -> "o"
  | false , O  -> "no"
  | true  , B  -> "b"
  | false , B  -> "ae"
  | true  , Z  -> "z"
  | false , Z  -> "nz"
  | true  , BE -> "be"
  | false , BE -> "a"
  | true  , S  -> "s"
  | false , S  -> "ns"
  | true  , P  -> "p"
  | false , P  -> "np"
  | true  , L  -> "l"
  | false , L  -> "ge"
  | true  , LE -> "le"
  | false , LE -> "g"


let condition_to_string condition =
  cc_to_string { truth_value = true; condition }


let int_to_condition = function
  | 0 -> O
  | 2 -> B
  | 4 -> Z
  | 6 -> BE
  | 8 -> S
  | 10 -> P
  | 12 -> L
  | 14 -> LE
  | n ->
    Logger.debug "n = %d\n" n;
    raise (Invalid_argument "int_to_condition\n")

let int_to_cc n =
  if n mod 2 = 0 then { truth_value = true; condition = int_to_condition n }
  else { truth_value = false; condition = int_to_condition (n - 1) }

let _sse_to_int = function
  | SseEQ -> 0
  | SseLT -> 1
  | SseLE -> 2
  | SseUNORD -> 3
  | SseNEQ -> 4
  | SseNLT -> 5
  | SseNLE -> 6
  | SseORD -> 7

let sse_to_string = function
    SseEQ -> "eq"
  | SseLT -> "lt"
  | SseLE -> "le"
  | SseUNORD -> "unord"
  | SseNEQ -> "neq"
  | SseNLT -> "nlt"
  | SseNLE -> "nle"
  | SseORD -> "ord"

let int_to_sse = function
    0 -> SseEQ
  | 1 -> SseLT
  | 2 -> SseLE
  | 3 -> SseUNORD
  | 4 -> SseNEQ
  | 5 -> SseNLT
  | 6 -> SseNLE
  | 7 -> SseORD
  | _ -> raise (Invalid_argument "int_to_sse")

let scale_to_size = function
    Scale1 -> 1
  | Scale2 -> 2
  | Scale4 -> 4
  | Scale8 -> 8

let _scale_to_int = function
    Scale1 -> 0
  | Scale2 -> 1
  | Scale4 -> 2
  | Scale8 -> 3

let scale_to_string scale =
  string_of_int (scale_to_size scale)

let int_to_scale = function
    0 -> Scale1
  | 1 -> Scale2
  | 2 -> Scale4
  | 3 -> Scale8
  | _ -> raise (Invalid_argument "scale_from_int")

let arith_op_to_string = function
    Add -> "add"
  | Adc -> "adc"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Sub -> "sub"
  | Sbb -> "sbb"

let _arith_op_to_int = function
  | Add -> 0
  | Or  -> 1
  | Adc -> 2
  | Sbb -> 3
  | And -> 4
  | Sub -> 5
  | Xor -> 6


let int_to_arith_op = function
  | 0 -> Add
  | 1 -> Or
  | 2 -> Adc
  | 3 -> Sbb
  | 4 -> And
  | 5 -> Sub
  | 6 -> Xor
  | 7 -> Sub
  | n ->
    Logger.debug "n = %d\n" n;
    raise (Invalid_argument "int_to_arith_op\n")

let shift_op_to_string = function
    Shl -> "shl"
  | Shr -> "shr"
  | Sar -> "sar"

let rotate_op_to_string = function
    Rol -> "rol"
  | Ror -> "ror"
  | Rcl -> "rcl"
  | Rcr -> "rcr"

let shiftd_op_to_string = function
    Shld -> "shld"
  | Shrd -> "shrd"

let _rotate_op_to_in = function
  | Rol -> 0
  | Ror -> 1
  | Rcl -> 2
  | Rcr -> 3

let _shift_op_to_int = function
  | Shl -> 4
  | Shr -> 5
  | Sar -> 7

let int_to_rotate_op = function
  | 0 -> Rol
  | 1 -> Ror
  | 2 -> Rcl
  | 3 -> Rcr
  | n ->
    Logger.debug "n = %d\n" n;
    raise (Invalid_argument "int_to_rotate_op\n")

let int_to_shift_op = function
  | 4 -> Shl
  | 5 -> Shr
  | 7 -> Sar
  | n ->
    Logger.debug "n = %d\n" n;
    raise (Invalid_argument "int_to_shift_op\n")


type modrm_byte = {
  rm : int;
  modb : int;
  reg : int;
}

let mask3 = 7
let mask2 = 3

let read_modrm_byte byte =
  Logger.debug ~level:5 "mod/rm byte : %x" byte;
  { rm   = byte land mask3;
    reg  = (byte lsr 3) land mask3;
    modb = (byte lsr 6) land mask2;
  }


let pp_modrm ppf modrm_byte =
  Format.fprintf ppf "@[<h>rm : %d; modb : %d; reg : %d@]"
    modrm_byte.rm modrm_byte.modb modrm_byte.reg

(*let read_modrm_byte bits =
  let rm, bits    = read_uint bits 3 in
  let reg, bits   = read_uint bits 3 in
  let modb, bits  = read_uint bits 2 in
  { rm; reg; modb; }, bits
*)

type sib_byte = {
  base : int;
  scale : int;
  index : int;
}

let read_sib_byte byte =
  Logger.debug ~level:5 "sib byte : %x" byte;
  { base   = byte land mask3;
    index  = (byte lsr 3) land mask3;
    scale  = (byte lsr 6) land mask2;
  }


let mk_address ?(address_mode=A32) ?(address_base=None) ?(address_index=None) disp =
  Address { addrMode  = address_mode;
            addrDisp  = Int64.of_int disp;
            addrBase  = address_base;
            addrIndex = address_index; }


let read_rm16_with_spare int_to_reg lr =
  let byte = Lreader.Read.u8 lr in
  let modrm_byte = read_modrm_byte byte in
  (* Logger.debug "modb = %d, rm = %d" modb rm; *)
  let get_base_index =  function
    | 0 -> Some EBX, Some (Scale1, ESI)
    | 1 -> Some EBX, Some (Scale1, EDI)
    | 2 -> Some EBP, Some (Scale1, ESI)
    | 3 -> Some EBP, Some (Scale1, EDI)
    | 4 -> Some ESI, None
    | 5 -> Some EDI, None
    | 6 -> Some EBP, None
    | 7 -> Some EBX, None
    | _ -> failwith "Unexpected ModR/M byte (rm > 7) !"
  in
  match modrm_byte with
  | { modb = 0; rm = 6; reg; } ->
    let disp = Lreader.Read.u16 lr in
    mk_address ~address_mode:A16 disp, reg
  | { modb; rm; reg; } ->
    let address_base, address_index = get_base_index rm in
    match modb with
    | 0 ->
      let disp = 0 in
      mk_address ~address_mode:A16 disp ~address_base ~address_index, reg
    | 1 ->
      let disp = Lreader.Read.u8 lr in
      (* let disp, bits = read_uint32_extend bits 8 in *)
      mk_address ~address_mode:A16 disp ~address_base ~address_index, reg
    | 2 ->
      let base = int_to_reg32 rm in
      let disp = Lreader.Read.u16 lr in
      mk_address ~address_mode:A16 disp ~address_base:(Some base) ~address_index, reg
    | 3 -> Reg (int_to_reg rm), reg
    | _ -> failwith "Unexpected ModR/M byte (mod > 3) !"


let read_rm32_with_spare int_to_reg lr =
  let byte = Lreader.Read.u8 lr in
  let modrm_byte = read_modrm_byte byte in
  Logger.debug ~level:4 "%a"  pp_modrm modrm_byte;
  match modrm_byte with
  | { modb = 0; rm = 5; reg; } ->
    let disp = Lreader.Read.u32 lr in
    mk_address disp,  reg
  | { modb = 3; rm; reg; } ->
    Reg (int_to_reg rm), reg
  | { modb ; rm = 4; reg; } ->
    let byte = Lreader.Read.u8 lr  in
    let sib_byte = read_sib_byte byte in
    let disp =
      match modb with
      | 0 -> 0
      | 1 -> Lreader.Read.u8 lr (* read_uint32_extend bits 8 *)
      | 2 -> Lreader.Read.u32 lr
      | 3 -> failwith "Impossible: matched above."
      | _ ->
        let msg = Format.sprintf "Impossible: 2-bit value for modb : %d" modb in
        failwith msg
    in
    let scale = int_to_scale sib_byte.scale in
    let address_index =
        match int_to_reg32 sib_byte.index with
        | ESP -> None
        | index -> Some (scale, index)
      in
    if sib_byte.base = 5 && modb = 0 then (* TODO : modb = 1, modb = 2 *)
      let disp = Lreader.Read.u32 lr in
      (* TODO : size := size + size + 4 *)
      mk_address disp ~address_index, reg
    else
      let base = int_to_reg32 sib_byte.base in
      let addr =
        mk_address ~address_mode:A32 disp ~address_base:(Some base) ~address_index in
      addr, reg
  | { modb = 0; rm; reg; } ->
    let base = int_to_reg32 rm in
    mk_address ~address_mode:A32 0 ~address_base:(Some base), reg
  | { modb = 1; rm; reg; } ->
    let base = int_to_reg32 rm in
    let disp = Lreader.Read.u8 lr in
    mk_address ~address_mode:A32 disp ~address_base:(Some base), reg
  | { modb = 2; rm; reg; } ->
    let base = int_to_reg32 rm in
    let disp = Lreader.Read.u32 lr in
    mk_address ~address_mode:A32 disp ~address_base:(Some base), reg
  | _ -> failwith "Unexpected ModR/M byte"


let read_modrm address_mode lr =
  let read_rm =
    match address_mode with
    | A32 -> read_rm32_with_spare
    | A16 -> read_rm16_with_spare
  in read_rm int_to_reg32 lr


let read_modrm_xmm address_mode lr =
  let read_rm =
    match address_mode with
    | A32 -> read_rm32_with_spare
    | A16 -> read_rm16_with_spare
  in read_rm int_to_xmm_reg lr

let read_rm8_with_spare = read_rm32_with_spare int_to_reg8

let read_rm16_with_spare = read_rm32_with_spare int_to_reg16


let string_of_register_restrict name low high =
  match name with
  | "eax" | "ebx" | "ecx" | "edx" ->
    begin
      match high - low + 1 with
      | 32 -> name
      | 16 -> Str.string_after name 1
      | 8 -> if low = 0 then (String.make 1 (String.get name 1))^"l"
        else (String.make 1 (String.get name 1))^"h"
      | _ -> name (* Does not resolve the name so return it as is *)
    end
  | _ -> name

let reg_to_extract (name:string): string * int * int =
  match name with
  | "eax" -> "eax", 0, 31
  | "ax"  -> "eax", 0, 15
  | "al"  -> "eax", 0, 7
  | "ah"  -> "eax", 8, 15
  | "ebx" -> "ebx", 0, 31
  | "bx"  -> "ebx", 0, 15
  | "bl"  -> "ebx", 0, 7
  | "bh"  -> "ebx", 8, 15
  | "ecx" -> "ecx", 0, 31
  | "cx"  -> "ecx", 0, 15
  | "cl"  -> "ecx", 0, 7
  | "ch"  -> "ecx", 8, 15
  | "edx" -> "edx", 0, 31
  | "dx"  -> "edx", 0, 15
  | "dl"  -> "edx", 0, 7
  | "dh"  -> "edx", 8, 15
  | "ebp" -> "ebp", 0, 31
  | "bp"  -> "ebp", 0, 15
  | "bpl" -> "ebx", 0, 7
  | "esi" -> "esi", 0, 31
  | "si"  -> "esi", 0, 15
  | "sil" -> "esi", 0, 7
  | "esp" -> "esp", 0, 31
  | "sp"  -> "esp", 0, 15
  | "spl" -> "esp", 0, 8
  | "edi" -> "edi", 0, 31
  (* Added for genericity *)
  | "CF"  -> "CF", 0, 0
  | "DF"  -> "DF", 0, 0
  | "ZF"  -> "ZF", 0, 0
  | "OF"  -> "OF", 0, 0
  | "SF"  -> "SF", 0, 0
  | "AF"  -> "AF", 0, 0
  | "PF"  -> "PF", 0, 0
  | _     -> failwith ("reg_to_extract unknown register: "^name)
