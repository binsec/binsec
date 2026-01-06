(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

exception InstructionUnhandled of string

module M = Hashtbl.Make (struct
  type t = Machine.t

  let equal = ( = )
  let hash = Hashtbl.hash
end)

let tbl = M.create 8

let generic_decode decode reader address =
  let instr, dhunk = decode reader address in
  (* Side effects beware *)
  Instruction.of_generic_instruction address instr dhunk

let register isa decode =
  M.replace tbl isa (fun reader vaddr -> generic_decode decode reader vaddr)

let get = M.find tbl
