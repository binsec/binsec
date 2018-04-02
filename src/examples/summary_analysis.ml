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

open Path_predicate
open Trace_type


type if_status = {
  left: int64 option;
  right: int64 option;
}

class summary_analyzer (input_config:Options.trace_analysis_config) =
  object inherit dse_analysis input_config

  val mutable nb_dyn_jump = 0
  val mutable nb_ret = 0
  val mutable nb_dyn_call = 0
  val mutable instruction_coverage = 0
  val mutable instr_mapping = Hashtbl.create 1500 (* for instr overlapping *)
  val mutable if_map = Basic_types.Addr64.Map.empty
  val mutable nb_smc = 0
  val mutable modules_loaded = 0
  val mutable instr_overlapping = Basic_types.Addr64.Set.empty
  val mutable instr_call = Basic_types.Addr64.Set.empty
  val mutable instr_ret = Basic_types.Addr64.Set.empty
  val mutable instr_djmp = Basic_types.Addr64.Set.empty


  method! private visit_metadata _inst metadata _env =
    match metadata with
    | Exception(_,_) -> ()
    | Module _ -> modules_loaded <- modules_loaded + 1
    | Layer _ -> nb_smc <- nb_smc + 1


  method! private visit_instr_before (_key:int) (tr_inst:trace_inst) _: trace_visit_action =
    if not(Hashtbl.mem instr_mapping tr_inst.location) then
      begin 
        instruction_coverage <- instruction_coverage + 1;
        Hashtbl.add instr_mapping tr_inst.location true; 
        let rec recurs i k = if i < k then (Hashtbl.add instr_mapping (Int64.add tr_inst.location (Int64.of_int i)) false; recurs (i+1) k) in
        recurs 1 (String.length tr_inst.opcode_bytes)
      end
    else
      begin
        if not(Hashtbl.mem instr_mapping tr_inst.location) then (* means jumping in the middle of an instruction *)
        instr_overlapping <- Basic_types.Addr64.Set.add tr_inst.location instr_overlapping;
      end;
    DoExec

  method! private visit_dbainstr_before (_key:int) (tr_inst:trace_inst) (dbainst:Dba_types.Statement.t) (_env:Path_pred_env.t): trace_visit_action =
    (match Dba_types.Statement.instruction dbainst with
    | Dba.IkIf(_,Dba.JOuter _,_) -> (* mettre à jour le if_status*)
      begin
        let next_l = get_next_address tr_inst.concrete_infos in
        if Basic_types.Addr64.Map.mem tr_inst.location if_map then
          let infos = Basic_types.Addr64.Map.find tr_inst.location if_map in
          match infos.left, infos.right with
          | Some a1, None ->
            if next_l = a1 then () else if_map <- Basic_types.Addr64.Map.add tr_inst.location ({infos with right=Some(next_l)}) if_map
          | _ -> ()
        else
          if_map <- Basic_types.Addr64.Map.add tr_inst.location ({left=Some(next_l); right=None}) if_map
      end
    | Dba.IkDJump(_,tgs) ->
      begin match tgs with
        | Some(Dba.Call(_)) -> instr_call <- Basic_types.Addr64.Set.add tr_inst.location instr_call
        | Some(Dba.Return) -> instr_ret <- Basic_types.Addr64.Set.add tr_inst.location instr_ret
        | None -> instr_djmp <- Basic_types.Addr64.Set.add tr_inst.location instr_djmp
      end
    | _ -> ()
    );
    DoExec


  method! private post_execution (_env:Path_pred_env.t): int =
    Logger.result "Trace length: %d" (cur_key_inst+1);
    Logger.result "Instruction coverage: %d" instruction_coverage;
    Logger.result "Modules loaded (shared libs): %d" modules_loaded;
    Logger.result "Self modification layers: %d" nb_smc;
    let addr_str = Basic_types.Addr64.Set.elements instr_overlapping |> List.map (fun i -> Printf.sprintf "%Lx" i) |> String.concat "," in
    Logger.result "Unique overlapping instructions: %d %s" (Basic_types.Addr64.Set.cardinal instr_overlapping) addr_str;
    Logger.result "Unique dynamic calls: %d" (Basic_types.Addr64.Set.cardinal instr_call);
    Logger.result "Unique ret: %d" (Basic_types.Addr64.Set.cardinal instr_ret);
    Logger.result "Unique dynamic jumps: %d" (Basic_types.Addr64.Set.cardinal instr_djmp);
    let partial, full = Basic_types.Addr64.Map.fold (fun _ i (p,f) -> match i.left, i.right with | Some _,None -> (p+1,f) | _ -> (p,f+1)) if_map (0,0) in
    Logger.result "Unique conditional jumps: %d, Coverage (partial: %d), (full: %d)" (Basic_types.Addr64.Map.cardinal if_map) partial full;
    0

  end;;
