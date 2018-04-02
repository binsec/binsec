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

(** Pretty-printers for X86 *)

val pp_bytes : int -> Format.formatter -> int -> unit
(** [pp_bytes n ppf v] prints the first [n] bytes of [v] into [ppf].
    [n] must be between 0 (excluded) and 4 (included) as [v] represents a X86
    word (32 bits).
*)

val pp_byte :  Format.formatter -> int -> unit
(** [pp_byte ppf v] is [pp_bytes 1 ppf v] *)

val pp_word :  Format.formatter -> int -> unit
(** [pp_word ppf v] is [pp_bytes 4 ppf v] *)

val pp_instr :  Format.formatter -> X86Types.instruction_kind -> unit




