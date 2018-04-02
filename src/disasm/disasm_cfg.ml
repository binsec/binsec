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

module H = Hashtbl.Make
    (struct
      open Dba_types
      type t = Virtual_address.t
      let hash (addr: Virtual_address.t) = (addr :> int)
      let equal a1 a2 = a1 = a2
    end)

let find htbl key =
  try H.find htbl key
  with Not_found -> (H.add htbl key []; [])

let add htbl key value =
  let lst = find htbl key in
  H.replace htbl key (value :: lst)

let add_succs flow inst set =
  let open Dba_types in
  Virtual_address.Set.iter
    (fun addr -> add flow inst.Disasm_types.Instruction.address addr)
    set

let build_cfg cfg htbl flow =
  H.iter
    (fun addr succs ->
       try begin
         let inst = Cfg.Vertex.create (H.find htbl addr) in
         List.iter
           (fun succ ->
              try begin
                let succ = Cfg.Vertex.create (H.find htbl succ) in
                Cfg.add_edge cfg inst succ
              end
              with Not_found -> ())
           succs
       end
       with Not_found -> ())
    flow

let do_disasm entry =
  let open Dba_types in
  let flow = H.create 257 in
  let htbl = H.create 257 in
  let callees = H.create 17 in
  let cfg =
    Disasm_core.fold
      (fun cfg wlst inst set ->
         H.add htbl inst.Disasm_types.Instruction.address inst;
         add_succs flow inst set;
         let block_callees =
           Block.callees inst.Disasm_types.Instruction.dba_block in
         Virtual_address.Set.iter (fun vaddr -> H.add callees vaddr ()) block_callees;
         let set =
           Virtual_address.Set.filter
             (fun ins -> not (H.mem htbl ins)) set
         in
         cfg,
         Disasm_core.W.add_virtual_addresses set wlst)
      (Cfg.create ())
      (Disasm_core.W.singleton entry)
  in
  build_cfg cfg htbl flow; cfg, H.fold (fun c _ l -> c :: l) callees []

let run () =
  let entry =
    Bitvector.create
      (match Options.get_entry_point () with
       | Some e -> e
       | None ->
         Loader_utils.get_img ()
         |> Loader.Img.entry
         |> Bigint.big_int_of_int)
      (Machine.Word_size.get ())
  in
  let ventry =
    Dba_types.Virtual_address.of_int64 (
        Bigint.int64_of_big_int (Bitvector.value_of entry)) in
  try
    let cfg, callees = do_disasm ventry in
    let channel = open_out "cfg.dot" in
    Cfg.output_graph channel cfg callees;
    close_out channel
  with
    Unix.Unix_error (e,_,_) -> Logger.error "%s" (Unix.error_message e)
