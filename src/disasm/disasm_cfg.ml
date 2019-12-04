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

module H = Hashtbl.Make
    (struct
      type t = Virtual_address.t
      let hash (addr: Virtual_address.t) = (addr :> int)
      let equal a1 a2 = a1 = a2
    end)

let do_disasm entry =
  let htbl = H.create 257 in
  let callees = H.create 17 in
  let lo,hi =
    Loader_utils.(section_slice_by_name ".text" (Kernel_functions.get_img ())) in
  let cfg =
    Disasm_core.fold
      (fun cfg wlst inst set ->
         H.add htbl inst.Instruction.address inst;
         Instr_cfg.add_inst cfg inst.Instruction.address inst;
         let set =
           Virtual_address.Set.filter
             (fun ins -> (ins :> int) >= lo && (ins :> int) <= hi)
             set in
         Virtual_address.Set.iter (Instr_cfg.add_edge_a cfg inst.Instruction.address) set;
         let block_callees = Dhunk.callees inst.Instruction.dba_block in
         Virtual_address.Set.iter (fun vaddr -> H.add callees vaddr ()) block_callees;
         let set =
           Virtual_address.Set.filter
             (fun ins -> not (H.mem htbl ins))
             set in
         cfg, Disasm_core.W.add_set wlst set )
      (Instr_cfg.create 257)
      (Disasm_core.W.singleton entry)
  in
  cfg, H.fold (fun c _ l -> c :: l) callees []

let run () =
  let ventry =
    (match Kernel_functions.get_ep () with
     | Some e -> e
     | None ->
       Kernel_functions.get_img ()
       |> Loader.Img.entry
       |> Virtual_address.create )
  in
  let entry = Instr_cfg.V.of_addr ventry in
  try
    let cfg, callees = do_disasm ventry in
    let channel = open_out "cfg.dot" in
    Instr_cfg.output_graph channel cfg ~entry callees;
    close_out channel
  with
  | Unix.Unix_error (e,_,_) ->
    Disasm_options.Logger.error "%s" (Unix.error_message e)
