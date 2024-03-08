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

module type Basic = sig
  type mnemonic
  type t = private { size : Size.Byte.t; opcode : string; mnemonic : mnemonic }

  val create : int -> string -> mnemonic -> t
  val pp_opcode : Format.formatter -> t -> unit
  val pp_mnemonic : Format.formatter -> t -> unit
end

module Make (P : Sigs.PRINTABLE) = struct
  type mnemonic = P.t
  type t = { size : Size.Byte.t; opcode : string; mnemonic : mnemonic }

  let create size opcode mnemonic =
    let size = Size.Byte.create size in
    { size; opcode; mnemonic }

  let pp_opcode ppf t = Format.fprintf ppf "%s" t.opcode
  let pp_mnemonic ppf t = Format.fprintf ppf "%a" P.pp t.mnemonic
end

module Generic = Make (Mnemonic)

type t = {
  address : Virtual_address.t;
  size : Size.Byte.t;
  opcode : Binstream.t;
  mnemonic : Mnemonic.t;
  dba_block : Dhunk.t;
}

let hunk t = t.dba_block
let address t = t.address
let size t = t.size
let opcode t = t.opcode
let mnemonic t = t.mnemonic

let create address size opcode mnemonic dba_block =
  { address; size; opcode; mnemonic; dba_block }

let unsupported address size opcode mnemonic =
  assert (
    let open Mnemonic in
    match mnemonic with Unknown | Unsupported _ -> true | Supported _ -> false);
  create address size opcode mnemonic Dhunk.empty

let of_generic_instruction address ginstr dba_block =
  create address ginstr.Generic.size
    (Binstream.of_nibbles ginstr.Generic.opcode)
    ginstr.Generic.mnemonic dba_block

let of_dba_block ?(mnemonic = Mnemonic.unsupported ()) address dba_block =
  let size = Size.Byte.create 0 in
  let opcode = Binstream.empty in
  create address size opcode mnemonic dba_block

let empty address =
  let addr = Dba_types.Caddress.base_value address in
  of_dba_block addr Dhunk.empty

let to_generic_instruction e =
  Generic.create (Size.Byte.to_int e.size)
    (Binstream.to_string e.opcode)
    e.mnemonic

let set_dba_block t dba_block = { t with dba_block }
let set_mnemonic mnemonic t = { t with mnemonic }
let is_decoded t = not (Dhunk.is_empty t.dba_block || Size.Byte.is_zero t.size)
let get_caddress t = Dba_types.Caddress.block_start_of_int (t.address :> int)

let stop vaddr =
  let dba_block = Dba.Instr.stop (Some Dba.OK) |> Dhunk.singleton in
  of_dba_block vaddr dba_block

let start i = Dhunk.start i.dba_block

let pp ppf i =
  Format.fprintf ppf "@[<v 0>@[<h>%a/@ %a@]@ %a@]" Binstream.pp i.opcode
    Mnemonic.pp i.mnemonic Dhunk.pp i.dba_block
