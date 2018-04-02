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

open Format

(* Disasembly worklists works on Caddresses *)
module W = struct
  include Worklist.Make(Dba_types.Virtual_address)

  let add_list = List.fold_left (fun wl a -> add a wl)
  let of_list = add_list empty

  let add_virtual_addresses =
    Dba_types.Virtual_address.Set.fold add

  let pp ppf wl =
    fprintf ppf "@[<hov 0>{%a}@]"
      (fun ppf wl ->
         iter (fun a -> fprintf ppf "%a; " Dba_types.Virtual_address.pp a) wl)
      wl
end



let compute_next_address current_address current_instruction =
  let size = current_instruction.Disasm_types.GenericInstruction.size in
  if Basic_types.ByteSize.is_zero size then None
  else Some (Dba_types.Virtual_address.add_int current_address (size:>int))

(* Platform-specific decoding wrappers *)

let generic_decode decode to_generic address =
  let xinstr, dba_block = decode address in
  let ginstr = to_generic xinstr in
  let next_address = compute_next_address address ginstr in
  Disasm_types.Instruction.of_generic_instruction address ginstr dba_block,
  next_address

exception Decode_error of string

let x86_decode (addr:Dba_types.Virtual_address.t) =
  try generic_decode X86toDba.decode X86Instruction.to_generic_instruction addr
  with
  | X86decoder.Parse s -> raise (Decode_error s)


let arm_decode (addr:Dba_types.Virtual_address.t) =
  generic_decode ArmToDba.decode (fun x -> x) addr

(* End wrappers *)

let decode_at_address decode address =
  let instr, next = decode address in
  if Disasm_types.Instruction.is_decoded instr then instr, next
  else begin
    Logger.warning "No instruction at %a : STOP"
      Dba_types.Virtual_address.pp address;
    let dba_block = Dba_types.(Instruction.stop (Some Dba.KO) |> Block.singleton) in
    Disasm_types.Instruction.set_dba_block instr dba_block,
    None
  end


let decoder_of_machine () =
  let open Machine in
  match ISA.get () with
  | X86 -> x86_decode
  | ARM -> arm_decode
  | isa ->
    let msg = Format.asprintf "missing ISA %a" ISA.pp isa in
    Errors.not_yet_implemented msg


let decode vaddress =
    let decoder = decoder_of_machine () in
    decode_at_address decoder vaddress

         
module Successors = struct
  open Disasm_types.Instruction

  let recursive instr =
    Disasm_options.Logger.debug ~level:5
      "@[<v 0>Computing recursive successors for block@ %a@]"
      Dba_types.Block.pp instr.dba_block;
    Dba_types.Block.outer_jumps instr.dba_block

  let linear instr =
    assert (not (Basic_types.ByteSize.is_zero instr.size));
    let hwa =
      Dba_types.Virtual_address.add_int instr.address (instr.size:>int) in
    Dba_types.Virtual_address.Set.singleton hwa

  let extended_linear instr =
    let succs1 = recursive instr in
    let succs2 = linear instr in
    Dba_types.Virtual_address.Set.union succs1 succs2

  let linear_bytewise instr =
    let next_byte_hwa = Dba_types.Virtual_address.add_int instr.address 1 in
    Dba_types.Virtual_address.Set.add next_byte_hwa (linear instr)
end

module type Iterable = sig
  val successors : Disasm_types.Instruction.t -> Dba_types.Virtual_address.Set.t
end
                  
module Make (I : Iterable) = struct
  let fold step_fun program worklist =
    let rec loop program worklist  =
      if W.is_empty worklist then program
      else
        let address, addresses = W.pop worklist in
        let instr, _ = decode address in (* FIXME *)
        let succs = I.successors instr in
        let p, wl = step_fun program addresses instr succs in
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
        let open Disasm_options in
        match DisassemblyMode.get () with
        | DisassemblyMode.Linear -> Successors.linear
        | DisassemblyMode.Linear_byte_wise -> Successors.linear_bytewise
        | DisassemblyMode.Recursive -> Successors.recursive
        | DisassemblyMode.ExtendedLinear -> Successors.extended_linear
      in let p, wl = step_fun program addresses instr (fsuccs instr) in
      loop p wl
  in loop program worklist


let iter step_fun worklist =
  let step_fun' = fun () wl instr succs -> (), step_fun wl instr succs in
  fold step_fun' () worklist

(* End iterators *)
