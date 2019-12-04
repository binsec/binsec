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

type t =
  (Dba.Instr.t * Instruction.Generic.t option)
    Dba_types.Caddress.Map.t


module Cfg = Instr_cfg

let of_program p =
  let open Disasm.Program in
  Cfg.fold_vertex
    (fun v pmap ->
       match Cfg.V.inst v with
       | None -> pmap
       | Some instr ->
         let hw_address = Cfg.V.addr v in
         Dhunk.fold
           (fun (pmap, j) i ->
              let caddr = Dba_types.Caddress.create hw_address j in
              let ginstr =
                if j = 0 then
                  Some (Instruction.to_generic_instruction instr)
                else None in
              Dba_types.Caddress.Map.add caddr (i, ginstr) pmap, j + 1
           ) (pmap, 0) instr.Instruction.dba_block
         |> fst
    ) p.instructions Dba_types.Caddress.Map.empty

(* FIXME : remove *)
let add_chained_instr chained_instr inst_map =
  match chained_instr with
  | [] -> inst_map
  | l ->
    let add_to acc (elem1, elem2) =
      Dba_types.Caddress.Map.add elem1 (elem2, None) acc
    in List.fold_left add_to inst_map l
