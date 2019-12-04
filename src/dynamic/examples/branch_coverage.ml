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

open Dse_options
open Path_predicate
open Config_piqi
open Trace_type
open Configuration
open Formula
open Formula_pp


class branch_coverage_analyzer (input_config:Trace_config.t) =
  object(self) inherit dse_analysis input_config

    val mutable k_value = -1
    val mutable sat = 0
    val mutable unsat = 0
    val mutable pred_to = 0
    val mutable start_time = 0.0

    method! private pre_execution (_env:Path_predicate_env.t) : unit =
      let open Trace_config in
      start_time <- Unix.gettimeofday ();
      if input_config.configuration.ksteps <> 0l then
        k_value <- Int32.to_int input_config.configuration.ksteps

    method! private visit_instr_before _key _tr_inst _env =
      (*       if key = 100 then begin
               Logger.warning "Stop execution 20000 instr reached";
               StopExec
               end else *) DoExec

    method! private visit_dbainstr_before key tr_inst dba_inst env =
      let open Dba in
      let open Instr in
      let _addr = tr_inst.location in
      match dba_inst.Dba_types.Statement.instruction with
      | If (cond, JOuter address, _) ->
        let address = Virtual_address.to_bigint address.base in
        let next_l = get_next_address tr_inst.concrete_infos in
        let pred = self#build_cond_predicate cond env in
        let pred = if Bigint.eq_big_int address (Bigint.big_int_of_int64 next_l)
          then mk_bl_not pred else pred in
        begin
          let res, _, _t =
            self#solve_predicate pred
              ~push:true ~pop:true ~name:"" ~prek:k_value env in
          begin match res with
            | SAT -> sat <- sat + 1
            | UNSAT -> unsat <- unsat + 1
            | TIMEOUT -> pred_to <- pred_to + 1
            | UNKNOWN -> ()
          end;
          Logger.result "%d %Lx    %s %a"
            key tr_inst.location tr_inst.mnemonic pp_status res;
          DoExec
        end
      | _ -> DoExec

    method! private post_execution (_env:Path_predicate_env.t) : int =
      let total_time = (Unix.gettimeofday()) -. start_time in
      Logger.result "SAT:%d UNSAT:%d Timeout:%d Time:%.02f"
        sat unsat pred_to total_time;
      0

  end;;
