(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
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

(* Hand-written parser for pintrace output.  *)
type parsed_ins = { count : int; addr : int; code : string }
type read_or_write = Read | Written
type mem = { read_or_write : read_or_write; address : Virtual_address.t }

type line =
  | Ins of parsed_ins
  | Mem of mem
  | Reg of { reg : string; value : int }

type 'a number = Zero | One of 'a | Several

(* Ins with all the mems that go with it, and the values that were
     written to the registers. *)
type ins = {
  count : int;
  address : Virtual_address.t;
  (* Where was the instruction executed. *)
  code : string;
  reg_values : (String.t * Virtual_address.t) list;
  (* alist. *)
  (* When there are more than one access at different addresses, we
     cannot know which address corresponds to which DBA instruction. So
      we can't use the memory information. Hence the number type.

      To alleviate the problem, we separate the load and store
      access. This allows handling instructions that read and write at
      different memory locations (e.g. x86 can push and read at the
      same time).

      This should handle most of the cases, so several should rarely
      happen in practice and we should be able to identify almost all
      memory accesses.

      Note: there are still instructions with several loads at different
      locations, e.g. string instructions like cmps.

      MAYBE: We should be outputting all the memory accesses; the fact
      that when there are several we cannot map them to DBA instructions
      should be handled by the tool that make use of this parsed information. *)
  mem_read : Virtual_address.t number;
  mem_written : Virtual_address.t number;
}

type trace = { channel : in_channel; pos : int }

let from file = { channel = open_in file; pos = 0 }

let parse_line chan =
  let l = input_line chan in
  match String.sub l 0 3 with
  | "ins" ->
      Scanf.sscanf l "ins %d @0x%x: code 0x%s" (fun count addr code ->
          Ins { count; addr; code })
  | "mem" ->
      Scanf.sscanf l "mem %d @0x%x: %c 0x%x"
        (fun _count _addr read_or_write addr ->
          let read_or_write =
            match read_or_write with
            | 'W' -> Written
            | 'R' -> Read
            | _ -> assert false
          in
          let address = Virtual_address.create addr in
          Mem { read_or_write; address })
  | "reg" ->
      Scanf.sscanf l "reg %s write 0x%x" (fun reg value -> Reg { reg; value })
  | _ -> assert false

let pop_ins x =
  try
    seek_in x.channel x.pos;
    let l = parse_line x.channel in
    let ins = match l with Ins x -> x | _ -> assert false in
    let acc =
      {
        count = ins.count;
        address = Virtual_address.create ins.addr;
        code = ins.code;
        reg_values = [];
        mem_read = Zero;
        mem_written = Zero;
      }
    in
    let rec loop acc =
      let pos = pos_in x.channel in
      let l = parse_line x.channel in
      match l with
      | Ins _ -> Some (acc, { x with pos })
      | Reg { reg; value } ->
          assert (not @@ List.mem_assoc reg acc.reg_values);
          let reg_values =
            (reg, Virtual_address.create value) :: acc.reg_values
          in
          loop { acc with reg_values }
      | Mem m -> (
          match m.read_or_write with
          | Read -> (
              match acc.mem_read with
              | Zero -> loop { acc with mem_read = One m.address }
              | One m' when Virtual_address.equal m' m.address -> loop acc
              | One _ | Several -> loop { acc with mem_read = Several })
          | Written -> (
              match acc.mem_written with
              | Zero -> loop { acc with mem_written = One m.address }
              | One m' when Virtual_address.equal m' m.address -> loop acc
              | One _ | Several -> loop { acc with mem_written = Several }))
    in
    loop acc
  with End_of_file -> None
