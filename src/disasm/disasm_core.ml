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

open Format
open Disasm_options

(* Disasembly worklists works on Caddresses *)
module W = struct
  include Worklist.Make (Virtual_address)

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
      (fun ppf wl -> iter (fun a -> fprintf ppf "%a; " Virtual_address.pp a) wl)
      wl
end

let compute_next_address current_address current_instruction =
  let size = current_instruction.Instruction.Generic.size in
  if Size.Byte.is_zero size then None
  else Some (Virtual_address.add_int (size :> int) current_address)

(* Platform-specific decoding wrappers *)

let generic_decode reader decode to_generic address =
  let xinstr, dhunk = decode reader address in
  (* Side effects beware *)
  let ginstr = to_generic xinstr in
  let next_address = compute_next_address address ginstr in
  (Instruction.of_generic_instruction address ginstr dhunk, next_address)

exception Decode_error of string

let x86_decode reader (addr : Virtual_address.t) =
  let result =
    generic_decode reader X86toDba.decode X86Instruction.to_generic_instruction
      addr
  in

  match result with
  | ({ Instruction.mnemonic = Mnemonic.Unknown; _ } as inst), _ ->
      let open Instruction in
      Logger.error "@[<v 0>Unknown instruction opcode prefix %@ %a:@ %a@]"
        Virtual_address.pp inst.address pp inst;
      raise (Decode_error "Bad opcode sequence")
  | res -> res

let riscv32_decode reader vaddr =
  generic_decode reader Riscv_to_dba.decode_32 (fun x -> x) vaddr

let riscv64_decode reader vaddr =
  generic_decode reader Riscv_to_dba.decode_64 (fun x -> x) vaddr

let z80_decode reader vaddr =
  generic_decode reader Z80_to_dba.decode (fun x -> x) vaddr

(* End wrappers *)

let decode_at_address decode reader address =
  let instr, next = decode reader address in
  if Instruction.is_decoded instr then (instr, next)
  else (
    Logger.warning "No instruction at %a ... stopping" Virtual_address.pp
      address;
    let dba_block = Dba.Instr.stop (Some Dba.KO) |> Dhunk.singleton in
    (Instruction.set_dba_block instr dba_block, None))

module M = Hashtbl.Make (struct
  type t = Machine.t

  let equal = ( = )
  let hash = Hashtbl.hash
end)

let tbl = M.create 8

let register_decoder isa decode convert =
  M.replace tbl isa (fun reader vaddr ->
      generic_decode reader decode convert vaddr)

let () = M.add tbl Machine.x86 x86_decode
let () = M.add tbl (Machine.riscv `x32) riscv32_decode
let () = M.add tbl (Machine.riscv `x64) riscv64_decode
let () = M.add tbl Machine.z80 z80_decode

let () =
  M.add tbl Machine.unknown (fun _ ->
      failwith
        "Machine ISA set to unknown. Aborting. Did you forget to set an -isa \
         switch on the command line ?")

let decoder_of_machine () =
  let isa = Kernel_options.Machine.get () in
  try M.find tbl isa
  with Not_found ->
    let msg = Format.asprintf "missing ISA %a" Machine.pp isa in
    Errors.not_yet_implemented msg

let decode_replacement = ref None

(** parses the option -disasm-decode-replacement and memoises the result *)
let get_decode_replacement () =
  match !decode_replacement with
  | None ->
      let map =
        match Disasm_options.Decode_replacement.get_opt () with
        | None -> Virtual_address.Map.empty
        | Some str ->
            let l =
              Parse_utils.read_string ~parser:Parser.dhunk_substitutions_eof
                ~lexer:Lexer.token ~string:str
            in
            let img = Kernel_functions.get_img () in
            let map =
              List.fold_left
                (fun acc (loc, dhunk) ->
                  match Loader_utils.Binary_loc.to_virtual_address ~img loc with
                  | Some addr -> Virtual_address.Map.add addr dhunk acc
                  | None ->
                      Logger.fatal "unable to parse the address %a"
                        Loader_utils.Binary_loc.pp loc)
                Virtual_address.Map.empty l
            in
            map
      in
      decode_replacement := Some map;
      map
  | Some x -> x

let add_replacement addr dhunk =
  let map = get_decode_replacement () in
  decode_replacement := Some (Virtual_address.Map.add addr dhunk map)

let decode ?(img = Kernel_functions.get_img ()) (vaddress : Virtual_address.t) =
  let decoder = decoder_of_machine () in
  let reader = Lreader.of_img img ~at:(vaddress :> int) in
  let instr, next = decode_at_address decoder reader vaddress in
  try
    let repl = get_decode_replacement () in
    let subst = Virtual_address.Map.find vaddress repl in
    let open Instruction in
    ( Instruction.create instr.address instr.size instr.opcode instr.mnemonic
        subst,
      next )
  with Not_found -> (instr, next)

let decode_from reader (at : Virtual_address.t) =
  let decoder = decoder_of_machine () in
  let instr, next = decode_at_address decoder reader at in
  try
    let repl = get_decode_replacement () in
    let subst = Virtual_address.Map.find at repl in
    let open Instruction in
    ( Instruction.create instr.address instr.size instr.opcode instr.mnemonic
        subst,
      next )
  with Not_found -> (instr, next)

let decode_binstream ?(base = Virtual_address.zero) bs =
  try
    let decoder = decoder_of_machine () in
    let lreader = Lreader.of_binstream bs in
    decode_at_address decoder lreader base
  with Not_found ->
    Logger.error
      "@[<v 0>Could not decode opcode %a.@,\
       The provided hexadecimal stream does not contain a recognized opcode.@,\
       Check that you selected the correct ISA.@,\
       Or maybe your input is too short or does not use the correct \
       endianness.@]"
      Binstream.pp bs;
    exit 2

module Successors = struct
  open Instruction

  let recursive instr =
    Logger.debug ~level:5
      "@[<v 0>Computing recursive successors for block@ %a@]" Dhunk.pp
      instr.dba_block;
    Dhunk.outer_jumps instr.dba_block

  let linear instr =
    assert (not (Size.Byte.is_zero instr.size));
    let hwa = Virtual_address.add_int (instr.size :> int) instr.address in
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
    let rec loop program worklist =
      if W.is_empty worklist then program
      else
        let address, addresses = W.pop worklist in
        let p, wl =
          try
            let instr, _ = decode address in
            (* FIXME *)
            let succs = I.successors instr in
            step_fun program addresses instr succs
          with Invalid_argument msg ->
            Disasm_options.Logger.warning "%s" msg;
            (program, addresses)
        in
        loop p wl
    in
    loop program worklist

  let iter step_fun worklist =
    let step_fun' () wl instr succs = ((), step_fun wl instr succs) in
    fold step_fun' () worklist
end

(* Iterators *)
let fold step_fun program worklist =
  let rec loop program worklist =
    if W.is_empty worklist then program
    else
      let address, addresses = W.pop worklist in
      let instr, _ = decode address in
      (* FIXME *)
      let fsuccs =
        match Disassembly_mode.get () with
        | Linear -> Successors.linear
        | Linear_byte_wise -> Successors.linear_bytewise
        | Recursive -> Successors.recursive
        | Extended_linear -> Successors.extended_linear
      in
      let p, wl = step_fun program addresses instr (fsuccs instr) in
      loop p wl
  in
  loop program worklist

let iter step_fun worklist =
  let step_fun' () wl instr succs = ((), step_fun wl instr succs) in
  fold step_fun' () worklist

(* End iterators *)
