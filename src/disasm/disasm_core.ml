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

open Format
open Disasm_options

(* Disasembly worklists works on Caddresses *)
module W = struct
  include Worklist.Make(Virtual_address)

  let add_list wl l = List.fold_left (fun wl a -> add a wl) wl l
  let of_list = add_list empty

  let add_set wl s = Virtual_address.Set.fold add s wl

  let add_filtered_set p wl s =
    Virtual_address.Set.fold (fun v wl -> if p v then add v wl else wl) s wl

  let of_set s = add_set empty s

  let of_filtered_set p s = add_filtered_set p empty s

  let singleton v = add v empty

  let pp ppf wl =
    fprintf ppf "@[<hov 0>{%a}@]"
      (fun ppf wl ->
         iter (fun a -> fprintf ppf "%a; " Virtual_address.pp a) wl)
      wl
end


let compute_next_address current_address current_instruction =
  let size = current_instruction.Instruction.Generic.size in
  if Size.Byte.is_zero size then None
  else Some (Virtual_address.add_int (size:>int) current_address )

(* Platform-specific decoding wrappers *)

let simplify_hunk dh =
  if Disasm_options.SimplifiedDisassembly.get () then
    Dhunk.Simplify.run dh

let generic_decode decode to_generic address =
  let xinstr, dhunk = decode address in
  simplify_hunk dhunk; (* Side effects beware *)
  let ginstr = to_generic xinstr in
  let next_address = compute_next_address address ginstr in
  Instruction.of_generic_instruction address ginstr dhunk,
  next_address

exception Decode_error of string
let x86_decode reader (addr:Virtual_address.t) =
  let result =
    generic_decode (X86toDba.decode reader)
      X86Instruction.to_generic_instruction addr in

  match result with
  | { Instruction.mnemonic = Mnemonic.Unknown; _} as inst, _ ->
    let open Instruction in
    Logger.error
      "@[<v 0>Unknown instruction opcode prefix %@ %a:@ %a@]"
      Virtual_address.pp inst.address
      pp inst;
    raise (Decode_error "Bad opcode sequence")
  | res -> res


let arm_decode reader (addr:Virtual_address.t) =
  let decoder =
    if Disasm_options.Cache_decoder.get () then ArmToDba.cached_decode reader
    else ArmToDba.decode reader in
  generic_decode decoder (fun x -> x) addr
;;

let riscv_decode reader vaddr =
  generic_decode (Riscv_to_dba.decode reader) (fun x -> x) vaddr
;;

(* End wrappers *)

let decode_at_address decode reader address =
  let instr, next = decode reader address in
  if Instruction.is_decoded instr then instr, next
  else begin
    Logger.warning "No instruction at %a ... stopping"
      Virtual_address.pp address;
    let dba_block = Dba.Instr.stop (Some Dba.KO) |> Dhunk.singleton in
    Instruction.set_dba_block instr dba_block,
    None
  end

let decoder_of_machine () =
  match Kernel_options.Machine.get () with
  | Machine.X86 { bits=`x32 } -> x86_decode
  | Machine.ARM { rev=`v7; _ } -> arm_decode
  | Machine.RISCV _ -> riscv_decode
  | Machine.Unknown ->
     failwith
       "Machine ISA set to unknown. Aborting. \
        Did you forget to set an -isa switch on the command line ?"
  | isa ->
    let msg = Format.asprintf "missing ISA %a" Machine.pp isa in
    Errors.not_yet_implemented msg

let decode (vaddress:Virtual_address.t) =
  let decoder = decoder_of_machine () in
  let reader=
    Lreader.of_img (Kernel_functions.get_img ()) ~cursor:(vaddress:>int) in
  let instr,next = decode_at_address decoder reader vaddress in
  try
    let repl = Disasm_options.Decode_replacement.get() in
    let subst = Virtual_address.Map.find vaddress repl in
    let open Instruction in
    Instruction.create instr.address instr.size instr.opcode instr.mnemonic subst,next
  with Not_found -> instr,next



let decode_from_reader lreader =
  let decoder = decoder_of_machine () in
  let vaddress = Lreader.get_virtual_cursor lreader in
  decode_at_address decoder lreader (Virtual_address.create vaddress)

let decode_binstream ?base bs =
  try Lreader.of_binstream ?base bs |> decode_from_reader
  with Not_found ->
    Logger.error
      "@[<v 0>Could not decode opcode %a.@,\
       The provided hexadecimal stream does not contain a recognized opcode.@,\
       Check that you selected the correct ISA.@,\
       Or maybe your input is too short or does not use the correct endianness.\
       @]" Binstream.pp bs;
    exit 2;
;;


module Successors = struct
  open Instruction

  let recursive instr =
    Logger.debug ~level:5
      "@[<v 0>Computing recursive successors for block@ %a@]"
      Dhunk.pp instr.dba_block;
    Dhunk.outer_jumps instr.dba_block

  let linear instr =
    assert (not (Size.Byte.is_zero instr.size));
    let hwa =
      Virtual_address.add_int (instr.size:>int) instr.address in
    Virtual_address.Set.singleton hwa

  let extended_linear instr =
    let succs1 = recursive instr in
    let succs2 = linear instr in
    Virtual_address.Set.union succs1 succs2

  let linear_bytewise instr =
    let next_byte_hwa = Virtual_address.succ instr.address in
    Virtual_address.Set.add next_byte_hwa (linear instr)
end

module type Iterable = sig
  val successors : Instruction.t -> Virtual_address.Set.t
end

module Make (I : Iterable) = struct
  let fold step_fun program worklist =
    let rec loop program worklist  =
      if W.is_empty worklist then program
      else
        let address, addresses = W.pop worklist in
        let p, wl =
          try
            let instr, _ = decode address in (* FIXME *)
            let succs = I.successors instr in
            step_fun program addresses instr succs
          with Invalid_argument msg ->
            Disasm_options.Logger.warning "%s" msg;
            program, addresses in
        loop p wl
    in loop program worklist

  let iter step_fun worklist =
    let step_fun' = fun () wl instr succs -> (), step_fun wl instr succs in
    fold step_fun' () worklist
end

(* Iterators *)
let fold step_fun program worklist =
  let rec loop program worklist  =
    if W.is_empty worklist then program
    else
      let address, addresses = W.pop worklist in
      let instr, _ = decode address in (* FIXME *)
      let fsuccs =
        match Disassembly_mode.get () with
        | Linear -> Successors.linear
        | Linear_byte_wise -> Successors.linear_bytewise
        | Recursive -> Successors.recursive
        | Extended_linear -> Successors.extended_linear
      in let p, wl = step_fun program addresses instr (fsuccs instr) in
      loop p wl
  in loop program worklist


let iter step_fun worklist =
  let step_fun' = fun () wl instr succs -> (), step_fun wl instr succs in
  fold step_fun' () worklist

(* End iterators *)
