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

include Instruction.Make(
  struct
    type t = X86Types.instruction_kind
    let pp ppf v = X86pp.pp_instr ppf v
  end)


let to_generic_mnemonic = function
  | X86Types.Bad -> Mnemonic.unknown
  | X86Types.Unsupported mnemonic_hint ->
    Mnemonic.unsupported ~mnemonic_hint ()
  | other -> Mnemonic.supported other X86pp.pp_instr


let to_generic_instruction v =
  let mnemonic = to_generic_mnemonic v.mnemonic in
  let size = Size.Byte.to_int v.size in
  Instruction.Generic.create size v.opcode mnemonic
