(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
(*    VERIMAG                                                             *)
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
(**************************************************************************)

open Trace_type
open Config_piqi
open Configuration
open Path_pred_env
open Options

class check_trace (trace_config:Options.trace_analysis_config) =
  object(self) inherit InvertChild.invert_child trace_config

    method! visit_dbainstr_before (key:int) (inst:trace_inst) (dbainst:Dba_types.Statement.t) (env:Path_pred_env.t) =
      let addr = inst.location in
      let it = try Hashtbl.find instruction_counter addr with Not_found -> 0 in
      begin
      match Dba_types.Statement.instruction dbainst with
      | Dba.IkIf (cond, Dba.JOuter addr , _) ->
        let address = Dba_types.Caddress.base_value addr in
        let formula_file = "formula-if-check-"
                           ^ (Printf.sprintf "%04d-%04d-%02d" counter key it)
                           (*Uuidm.to_string @@ Uuidm.create `V4*) ^ ".smt2" in
        let next_addr = get_next_address inst.concrete_infos in
        let static_predicate = self#build_cond_predicate cond env in
        let predicate =
              if Bigint.compare_big_int address (Bigint.big_int_of_int64 next_addr) <> 0
              then Smtlib2.SmtNot static_predicate
              else static_predicate
        in
        ignore(Formula.build_formula_file env.formula predicate formula_file trace_config.configuration.solver);
        Logger.debug "Formula build, solving ...";
        begin
        try
                let result, _ = Solver.solve_model formula_file trace_config.configuration.solver in
                  match result with
                  | Smtlib2.SAT -> Logger.debug "SAT!" 
                  | Smtlib2.UNSAT
                  | Smtlib2.TIMEOUT
                  | Smtlib2.UNKNOWN ->
                    Logger.debug "input of conditional jump at 0x%Lx not found" inst.location
        with Failure _m -> ()
        end
      | Dba.IkDJump _
      | Dba.IkAssert _
      | Dba.IkAssign _
      | Dba.IkSJump _
      | Dba.IkStop _
      | Dba.IkIf _
      | Dba.IkAssume (_, _)
      | Dba.IkNondetAssume (_, _, _)
      | Dba.IkNondet (_, _, _)
      | Dba.IkUndef _
      | Dba.IkMalloc _
      | Dba.IkFree _
      | Dba.IkPrint _ -> ()
      end;
      Path_predicate.DoExec
end
