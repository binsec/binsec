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
open Trace_type
open Path_predicate_formula
open Solver
open Formula
open Path_predicate
open Path_predicate_env

class switch_analysis (input_config:Trace_config.t) =
  object(self) inherit dse_analysis input_config

    val smt_file = "/tmp/switch.smt2"
    val mutable targets = [];

    method! private visit_dbainstr_before (key:int) _
        dbainst (env:Path_predicate_env.t): trace_visit_action =
      let open Dba in
      let open Dba_types.Statement in
      match dbainst with
      | { instruction = Instr.Assign(LValue.Var {Dba.name = "eax"; _},
                                     Expr.Load(_,_,e) , _); _ }
        when key = 27 ->
        Logger.debug ~level:1 "Dynamic assignment found";
        self#compute_possible_values_interactively e env ;
        DoExec
      | _ -> DoExec

    method private compute_possible_values e (env:Path_predicate_env.t): unit =
      let _ = self#add_witness_variable "totem0" e env in
      let rec check_possible_jumps k f =
        let preds =
          List.map
            (fun e ->
               mk_bl_not
                 (self#build_witness_bitvector_comparison_predicate "totem0" env.formula.addr_size e))
            targets
        in
        let pred = self#build_multiple_condition_predicate preds in
        let _ = build_formula_file f pred smt_file in
        (* let _ = pp_stat_formula new_f in *)
        let solver =
          Formula_options.Solver.of_piqi config.Config_piqi.Configuration.solver
        in
        match solve_model smt_file solver with
        | SAT, model ->
          begin
            match Smt_model.find_variable model "totem0" with
             | Some value ->
                Logger.info "New value: %a" Bitvector.pp_hex value;
                targets <- value :: targets;
                let k' = k - 1 in
                if k' <> 0 then check_possible_jumps k' env.formula
             | None ->
               Logger.error "Error while processing model (stop recursion)";
               exit 0
          end
        | (UNSAT | TIMEOUT | UNKNOWN), _ -> Logger.warning "UNSAT"
      in
      check_possible_jumps 10 env.formula


    method private compute_possible_values_interactively e (env:Path_predicate_env.t): unit =
      self#add_witness_variable "totem0" e env;
      let solver =
        Formula_options.Solver.of_piqi
          config.Config_piqi.Configuration.solver in
      let session = start_interactive ~file:smt_file solver in
      ignore @@
      build_formula_incremental env.formula mk_bl_true ~push:false session;
      let rec check_possible_jumps k prev =
        let res, model = solve_incremental_model session ~term:prev in
        match res with
        | SAT ->
           let vname = "totem0" in
           begin match Smt_model.find_variable model vname with
           | Some value ->
              Logger.info "New value: %a" Bitvector.pp_hex value;
              flush stdout;
              targets <- value :: targets;
              let new_pred =
                mk_bl_not
                  (self#build_witness_bitvector_comparison_predicate
                     vname env.formula.addr_size value) in
              let k' = k - 1 in
              if k' <> 0 then check_possible_jumps k' new_pred
           | None ->
              Logger.error
                "Error while processing model (stop recursion)";
              exit 1;
           end
        | UNSAT   -> Logger.error "UNSAT"
        | TIMEOUT -> Logger.warning "TIMEOUT"
        | UNKNOWN -> Logger.warning "UNKNOWN"
      in
      check_possible_jumps 10 mk_bl_true;
      stop_interactive session

    method! private post_execution _ =
      if List.length targets > 1 then 0 else 100

    method! private visit_instr_before (key:int) (tr_inst:trace_inst) _ : trace_visit_action =
      Logger.result "%d %Lx    %s" key tr_inst.location tr_inst.mnemonic;
      DoExec

  end;;
