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

module type BasicInstruction = sig
  type mnemonic

  type t = private {
    size     : Basic_types.ByteSize.t;
    opcode   : string;
    mnemonic : mnemonic;
  }

  val create      : int -> string -> mnemonic ->  t
  val pp_opcode   : Format.formatter -> t -> unit
  val pp_mnemonic : Format.formatter -> t -> unit
end

module Make (P: Sigs.Printable) = struct
  type mnemonic = P.t

  type t = {
    size : Basic_types.ByteSize.t;
    opcode : string;
    mnemonic : mnemonic ;
  }

  let create size opcode mnemonic =
    let size = Basic_types.ByteSize.create size in
    { size; opcode; mnemonic; }

  let pp_opcode ppf t = Format.fprintf ppf "%s" t.opcode
  let pp_mnemonic ppf t = Format.fprintf ppf "%a" P.pp t.mnemonic
end



module Mnemonic = struct
  open Format
  type t =
    | Bad
    | Unhandled
    | Handled of string

  let bad = Bad
  let unhandled = Unhandled
  let handled v pp = Handled (asprintf "%a" pp v)

  let pp ppf = function
    | Handled v -> fprintf ppf "%s" v
    | Bad -> fprintf ppf "bad"
    | Unhandled -> fprintf ppf "unhandled"

  let to_string v = asprintf "%a" pp v

end

module GenericInstruction = Make(Mnemonic)

module Instruction = struct
  type t = {
    address : Dba_types.Virtual_address.t;
    size : Basic_types.ByteSize.t;
    opcode : Basic_types.Binstream.t;
    mnemonic : Mnemonic.t;
    dba_block : Dba_types.Block.t;
  }


  let create address size opcode mnemonic dba_block =
    { address; size; opcode; mnemonic; dba_block; }

  let of_generic_instruction address ginstr dba_block =
    create address
      ginstr.GenericInstruction.size
      (Basic_types.Binstream.of_nibbles ginstr.GenericInstruction.opcode)
      ginstr.GenericInstruction.mnemonic dba_block

  let of_dba_block address dba_block =
    let size = Basic_types.ByteSize.create 0 in
    let opcode = Basic_types.Binstream.empty in
    let mnemonic = Mnemonic.unhandled in
    create address size opcode mnemonic dba_block

  let empty address =
    let addr =
      Dba_types.Caddress.base_value address
      |> Bigint.int_of_big_int
      |> Dba_types.Virtual_address.create
    in
    of_dba_block addr Dba_types.Block.empty


  let to_generic_instruction e =
    GenericInstruction.create
      (Basic_types.ByteSize.to_int e.size)
      (Basic_types.Binstream.to_string e.opcode)
      e.mnemonic

  let set_dba_block t dba_block = { t with dba_block }


  let is_decoded t =
    not (
      Dba_types.Block.is_empty t.dba_block
    || Basic_types.ByteSize.is_zero t.size)


  let get_caddress t = Dba_types.Caddress.block_start_of_int (t.address:>int)


  let stop vaddr =
    let open Dba_types in
    let dba_block = Instruction.stop (Some Dba.OK) |> Block.singleton in
    of_dba_block vaddr dba_block

end


type pmap =
  (Dba.instruction * GenericInstruction.t option) Dba_types.Caddress.Map.t

(* Inherited function *)

(* FIXME : remove *)
let add_chained_instr chained_instr inst_map =
  match chained_instr with
  | [] -> inst_map
  | l ->
    let add_to acc (elem1, elem2) =
      Dba_types.Caddress.Map.add elem1 (elem2, None) acc
    in List.fold_left add_to inst_map l


module Program = struct
  type t = Instruction.t Dba_types.Virtual_address.Map.t

  let to_pmap t =
    Dba_types.Virtual_address.Map.fold
      (fun hw_address instr pmap ->
         let base =
           Bitvector.create
             (Bigint.big_int_of_int (hw_address:>int))
             (Machine.Word_size.get ()) in
         Dba_types.Block.fold_left
           (fun (pmap, j) i ->
              let caddr = Dba_types.Caddress.create base j in
              let ginstr =
                if j = 0 then Some (Instruction.to_generic_instruction instr)
                else None in
              Dba_types.Caddress.Map.add caddr (i, ginstr) pmap, j + 1
           ) (pmap, 0) instr.Instruction.dba_block
         |> fst
      ) t Dba_types.Caddress.Map.empty

end
