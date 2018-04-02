(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
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

open Bigint

  
let decode_opcode ?(addr=Int64.zero) opcode =
  try
    let basic_inst, instrs = X86toDba.decode_string opcode addr in
    let mnemonic =
      Format.asprintf "%a" X86Instruction.pp_mnemonic basic_inst in
    mnemonic, instrs
  with X86toDba.InstructionUnhandled _ ->
    Logger.warning "Not decoded @%Lx: %a" addr Basic_types.Binstream.pp opcode;
    "not decoded", Dba_types.Block.empty


let decode_bin_opcode ?(addr=Int64.zero) bytes =
  let opcode = Basic_types.Binstream.of_bytes bytes in
  decode_opcode ~addr opcode


let group_char_two (str:string): string list =
  let rec exp a b =
    if a < 1 then b
    else exp (a - 2) (((Char.escaped str.[a-1])^(Char.escaped str.[a])) :: b)
  in
  exp (String.length str - 1) []

let hex_string_to_list (raw:string): string list =
  let byte_to_bin s =
    Scanf.sscanf s "%x" (fun x -> String.make 1 (Char.chr x)) in
  List.map byte_to_bin (String_utils.remove_char ' ' raw |> group_char_two)

let hex_string_to_bin (raw:string): string =
  List.fold_left (fun acc i -> acc^i) "" (hex_string_to_list raw)

let decode_hex_opcode ?(addr=Int64.zero) (raw:string) =
  decode_opcode ~addr (Basic_types.Binstream.of_nibbles raw)

let string_to_hex ?(with_space=false) (raw:string) : string =
  let spacer = if with_space then " " else "" in
  String_utils.replace_chars
    (fun c -> Format.asprintf "%s%02x" spacer (Char.code c)) raw
  |> String.trim

let little_string_to_big_string ?(with_space=false) (raw:string): string =
  String_utils.reverse raw |> string_to_hex ~with_space

let string_to_int_list (raw:string):int list =
  let len = String.length raw in
  let rec aux acc i =
    if i < 0 then acc
    else aux ( Char.code raw.[i] :: acc) (i - 1)
  in aux [] (len - 1)

let string_to_big_int (raw:string): t =
  let rec loop acc n =
    if n > 0 then
      loop
        (or_big_int
           (shift_left_big_int acc 8)
           (big_int_of_int (Char.code raw.[n-1])))
        (n-1)
    else acc
  in
  loop zero_big_int (String.length raw)

let extract_byte (value: int64) (offset: int): char =
  Int64.shift_left (Int64.of_int 255) (8 * offset)
  |> Int64.logand value
  |> (fun i -> Int64.shift_right_logical i (8 * offset))
  |> Int64.to_int
  |> Char.chr

let _int64_to_bigendian_bin (value: int64) (addr_size: int): string =
  match addr_size with
  | 32 -> String.init 4 (fun i -> extract_byte value (4 - i))
  | 64 -> String.init 8 (fun i -> extract_byte value (8 - i))
  | _  -> failwith "Unknown addr_size"

let int64_to_littleendian_bin (value: int64) (addr_size: int): string =
  match addr_size with
  | 32 -> String.init 4 (fun i -> extract_byte value i)
  | 64 -> String.init 8 (fun i -> extract_byte value i)
  | _  -> failwith "Unknown addr_size"

let int64_to_char (value:int64): char =
  Int64.(logand value 0xFFL |> to_int) |> Char.chr
