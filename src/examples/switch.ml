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

open Trace_type
open Formula
open Solver
open Smtlib2
open Options
open Config_piqi
open Path_predicate
open Path_pred_env
open Configuration

class switch_analysis (input_config:trace_analysis_config) =
  object(self) inherit dse_analysis input_config

    val smt_file = "/tmp/switch.smt2"
    val mutable targets = [];

    method! private visit_dbainstr_before (key:int) _
        dbainst (env:Path_pred_env.t): trace_visit_action =
      let open Dba in
      let open Dba_types.Statement in
      match dbainst with
      | { instruction = IkAssign(LhsVar(name1,_,_), ExprLoad(_,_,e) , _); _ }
        when name1 = "eax" && key = 27 ->
        Logger.debug ~level:1 "Dynamic assignment found";
        self#compute_possible_values_interactively e env ;
        DoExec
      | _ -> DoExec

    method private compute_possible_values e (env:Path_pred_env.t): unit =
      let _ = self#add_witness_variable "totem0" e env in
      let rec check_possible_jumps k f =
        let preds =
          List.map
            (fun e ->
               SmtNot
                 (self#build_witness_bitvector_comparison_predicate "totem0" env.addr_size e))
            targets
        in
        let pred = self#build_multiple_condition_predicate preds in
        let _ = build_formula_file f pred smt_file config.solver in
        (* let _ = pp_stat_formula new_f in *)
        let res, model = solve_model smt_file config.solver in
        match res with
        | SAT ->
          begin
            try
              let value = Smt_model.find_register model "totem0" in
              Logger.info "New value: %s" (Bitvector.to_hexstring value);
              targets <- value :: targets;
              if (k-1) != 0 then check_possible_jumps (k-1) env.formula
            with
            | Failure s -> Printf.printf "Error while processing model (stop recursion):%s\n" s
          end
        | UNSAT | TIMEOUT | UNKNOWN -> Logger.warning "UNSAT"
      in
      check_possible_jumps 10 env.formula


    method private compute_possible_values_interactively e (env:Path_pred_env.t): unit =
      self#add_witness_variable "totem0" e env;
      let solver = start_interactive ~file:smt_file config.solver in
      build_formula_incremental env.formula SmtTrue ~push:false solver config.solver |> ignore;
      let rec check_possible_jumps k prev =
        let res, model = solve_incremental_model solver ~file:smt_file ~f:(Some prev) config.solver in
        match res with
        | SAT ->
          let value = Smt_model.find_register model "totem0" in
          Logger.info "New value: %s" (Bitvector.to_hexstring value);
          flush stdout;
          targets <- value :: targets;
          let new_pred = SmtNot(self#build_witness_bitvector_comparison_predicate "totem0" env.addr_size value) in
          if (k-1) != 0 then check_possible_jumps (k-1) new_pred
        | UNSAT   -> Logger.error "UNSAT"
        | TIMEOUT -> Logger.warning "TIMEOUT"
        | UNKNOWN -> Logger.warning "UNKNOWN"
      in
      check_possible_jumps 10 SmtTrue;
      stop_interactive solver

    method! private post_execution _ =
      if List.length targets > 1 then 0 else 100

    method! private visit_instr_before (key:int) (tr_inst:trace_inst) _ : trace_visit_action =
      Logger.result "%d %Lx    %s" key tr_inst.location tr_inst.opcode;
      DoExec

  end;;
