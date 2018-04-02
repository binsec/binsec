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
open Path_pred_env
open Config_piqi
open Trace_type
open Formula
open Smtlib2
open Configuration

open Options
open Analysis_config_piqi
open Specific_parameters_t
open Generic_analysis

module Pp = Dba_printer.Unicode

let the = Utils.unsafe_get_opt

class generic_analyzer (input_config:Options.trace_analysis_config) =
  object(self) inherit dse_analysis input_config

    val mutable parameters = default_generic_analysis ()
    val mutable results = default_generic_analysis_results ()
    val mutable values = []
    val witness_var = "totem0"
    val mutable analysis_started = false
    val mutable from_addr = 0L
    val mutable to_addr   = 0L
    val mutable limit = 100
    val mutable first_res = UNKNOWN

    method! private pre_execution (_env:Path_pred_env.t) : unit =
      let tmp =
        match input_config.configuration.additional_parameters with
        | None ->
          Logger.fatal
            "No additional parameters found. Please provide a configuration file.";
          exit 3;
        | Some x -> x
      in
      parameters <- the tmp.generic_params;
      from_addr <- Utils.get_opt_or_default 0L parameters.from_addr;
      to_addr <- Utils.get_opt_or_default 0L parameters.to_addr;
      limit <- (match parameters.limit_values with | None -> 100 | Some x -> Int32.to_int x);
      if from_addr = 0L then
        analysis_started <- true

    method! private visit_instr_before (_key:int) (tr_inst:trace_inst) (env:Path_pred_env.t): trace_visit_action =
      if tr_inst.location = from_addr then analysis_started <- true;
      if tr_inst.location = to_addr then StopExec
      else
      if analysis_started then
        if tr_inst.location = parameters.target_addr then
          try
            let dba_instr = Parse_utils.instruction_of_string parameters.dba in
            match parameters.kind, dba_instr with
            | (`satisfiability, Dba.IkAssert(Dba.CondReif(e),_))
              when (Dba_utils.computesize_dbaexpr e) <> 1 ->
              Logger.error "Wrong expression size"
            | (`satisfiability, Dba.IkAssert(cond,_)) ->
              self#solve_satisfiability cond env
            | (`values, Dba.IkAssert(Dba.CondReif(e),_)) ->
              self#solve_values e env
            | (_,inst) ->
              Logger.error "Invalid DBA:%a" Pp.pp_instruction inst;
            ; StopExec
          with
          | Parse_utils.Invalid_dba_string s
          | Errors.Size_error s ->
            Logger.error "Wrong size:%s" s;
            StopExec
        else
          DoExec
      else
        SkipExec


    method private solve_satisfiability cond (env:Path_pred_env.t): unit =
      let pred = self#build_cond_predicate cond env in
      let res, _, _ = self#solve_predicate pred ~push:false ~pop:false ~get_model:false env in
      first_res <- res;
      results <- self#build_result res []


    method private solve_values e (env:Path_pred_env.t): unit =
      let size = Dba_utils.computesize_dbaexpr e in
      self#add_witness_variable witness_var e env;
      self#add_constraint_witness (Some SmtBvUge) size parameters.restrict_values_from env;
      self#add_constraint_witness (Some SmtBvUle) size parameters.restrict_values_to env;
      let rec check_possible_values k _ targets =
        (match targets with [] -> () | t:: _ -> self#add_constraint_witness None ~negate:true size (Some t) env);
        let res, model, _ = self#solve_predicate SmtTrue ~push:true ~pop:true env in
        if k = limit then first_res <- res;
        match res with
        | SAT ->
          begin try
              let value = Smt_model.find_register model witness_var in
              if k - 1 <> 0 then check_possible_values (k-1) env.formula
                  ((Bigint.int64_of_big_int (Bitvector.value_of value))::targets) else targets
            with
            | Failure s ->
              Logger.error
                "Error while processing model (stop recursion):%s\n" s;
              targets
          end
        | UNSAT | TIMEOUT | UNKNOWN -> targets
      in
      let tgs = check_possible_values limit env.formula [] in
      results <- self#build_result first_res tgs

    method private add_constraint_witness (op:smt_bv_binary option) ?(negate=false) (size:int) (value:int64 option) (env:Path_pred_env.t): unit =
      match value with
      | Some x ->
        let witness_f = SmtBvVar(witness_var, size) in
        let addr_f =
          SmtBvCst(Bitvector.create (Bigint.big_int_of_int64 x) env.addr_size) in
        let cst =
          begin match op with
            | None -> SmtComp(SmtBvExpr(witness_f), SmtBvExpr(addr_f))
            | Some o ->
              SmtComp(SmtBvExpr(SmtBvBinary(o, witness_f, addr_f)), one)
          end in
        let cst = if negate then SmtNot(cst) else cst in
        env.formula <- add_constraint env.formula cst
      | _ -> ()

    method! private post_execution _ : int =
      match input_config.trace_input with
      | Chunked (_,_) ->
        Logger.result "Result:%a" Smtlib2print.pp_smt_result first_res;
        List.iter
          (fun i ->
             Logger.result "val:%Lx" i) results.Generic_analysis_results.values;
        0
      | Stream _ ->
        let data = Piqirun.to_string (gen_generic_analysis_results results) in
        self#send_message "ANALYSIS_RESULTS" data; 0

    method private build_result (res:Smtlib2.smt_result) (values:int64 list): generic_analysis_results =
      let formula =
        match parameters.get_formula with
        | Some _ -> Some (File_utils.load default_formula_file)
        | None -> None
      in
      let res = match res with | SAT -> `sat | UNSAT -> `unsat | TIMEOUT -> `timeout | UNKNOWN -> `unknown in
      Generic_analysis_results.({result=res; values=values; smt_formula=formula})


  end;;
