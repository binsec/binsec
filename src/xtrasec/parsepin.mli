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

type 'a number =
  | Zero
  | One of 'a
  | Several


(* Instruction in a trace, with all the memory operands that it did,
   and the values that were written to the registers. *)
type ins = private
  {
    (* Number of instruction in the sequence. *)
    count:int;

    (* Program counter when the instruction was executed. *)
    address: Virtual_address.t;

    (* Opcode of the instruction, as an hex string. *)
    code:string;

    (* Association list with the name of the registers written by the
       instruction, and their value after the instruction. *)
    reg_values:(String.t * Virtual_address.t) list;

    (* Address of the memory load and store operations that the
       instruction made (not counting things like instruction fetch). *)
    mem_read: Virtual_address.t number;
    mem_written: Virtual_address.t number;
}

(* A sequence (stream) of instructions. *)
type trace

(* Create a sequence of instruction from a file: points to the first
   instruction in the file. *)
val from: string -> trace

(* Get the first instruction in a trace; return the instruction and a
   trace that points to the next instruction. Returns None if this was the
   last instruction in the trace. *)
val pop_ins: trace -> (ins * trace) option

