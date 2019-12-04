(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

open Dse_options
open Trace_type
open Config_piqi
open Configuration
open Path_predicate_env


class check_trace (trace_config:Trace_config.t) =
  object(self) inherit InvertChild.invert_child trace_config

    method! visit_dbainstr_before (key:int) inst (dbainst:Dba_types.Statement.t) (env:Path_predicate_env.t) =
      let addr = inst.location in
      let it = try Hashtbl.find instruction_counter addr with Not_found -> 0 in
      begin
        match Dba_types.Statement.instruction dbainst with
        | Dba.Instr.If (cond, Dba.JOuter addr , _) ->
          let address = Dba_types.Caddress.base_value addr in
          let formula_file = "formula-if-check-"
                             ^ (Printf.sprintf "%04d-%04d-%02d" counter key it)
                             (*Uuidm.to_string @@ Uuidm.create `V4*) ^ ".smt2" in
          let next_addr = get_next_address inst.concrete_infos in
          let static_predicate = self#build_cond_predicate cond env in
          let predicate =
            if Virtual_address.compare address
                 Virtual_address.(of_int64 next_addr) <> 0
            then Formula.mk_bl_not static_predicate
            else static_predicate
          in
          ignore(Path_predicate_formula.build_formula_file env.formula predicate formula_file);
          Logger.debug "Path_predicate_formula build, solving ...";
          begin
            try
              let result, _ =
                let open Trace_config in
                let solver = Formula_options.Solver.of_piqi
                               trace_config.configuration.solver in
                Solver.solve_model formula_file solver in
              match result with
              | Formula.SAT -> Logger.debug "SAT!"
              | Formula.UNSAT
              | Formula.TIMEOUT
              | Formula.UNKNOWN ->
                Logger.debug "input of conditional jump at 0x%Lx not found" inst.location
            with Failure _m -> ()
          end
        | Dba.Instr.DJump _
        | Dba.Instr.Assert _
        | Dba.Instr.Assign _
        | Dba.Instr.SJump _
        | Dba.Instr.Stop _
        | Dba.Instr.If _
        | Dba.Instr.Assume (_, _)
        | Dba.Instr.NondetAssume (_, _, _)
        | Dba.Instr.Nondet (_, _, _)
        | Dba.Instr.Undef _
        | Dba.Instr.Malloc _
        | Dba.Instr.Free _
        | Dba.Instr.Print _ -> ()
      end;
      Path_predicate.DoExec
  end
