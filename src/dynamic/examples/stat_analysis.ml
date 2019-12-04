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

open Path_predicate
open Path_predicate_env
open Config_piqi
open Trace_type
open Configuration
open Formula
open Analysis_config
open Standard_analysis
open Specific_parameters_t
open Dse_options

let is_flag (s:string): bool =
  List.mem s ["OF";"SF";"ZF";"AF";"PF";"CF"]

(* This is never set outside the CLI during execution *)
let pruning = Trace_options.Pruning.get ()

class stat_analyzer (input_config:Trace_config.t) =
  object(self) inherit dse_analysis input_config

    val! trace_name =
      let open Trace_config in
      Filename.basename input_config.trace_file |> Filename.chop_extension
    val mutable target = -1
    val mutable found = false
    val try_find_conflicting = true
    val mutable result = UNSAT

    val mutable nb_all = 0
    val mutable nb_flags = 0
    val mutable nb_if = 0
    val mutable start_time = 0.0

    method! private pre_execution _ =
      start_time <- Unix.gettimeofday ();
      let open Trace_config in
      match input_config.configuration.additional_parameters with
      | Some p ->
        begin match p.standard_params with
          | Some param ->
            begin match param.target_addr with
              | Some tgt ->
                Logger.debug "Set target to %Ld" tgt;
                target <- Int64.to_int tgt
              | None -> ()
            end
          | None -> ()
        end
      | None -> ()

    method private get_file_name (key:int) (env:Path_predicate_env.t): string =
      let opt = Path_predicate_formula.optim_str env.formula in
      let polname = Policy_utils.policy_file () in
      let solver_name =
        match input_config.Trace_config.configuration.solver with
        | `z3 -> "Z3"
        | `boolector -> "BOOLECTOR"
        | `cvc4 -> "CVC4"
        | `yices -> "YICES"
      in
      Format.sprintf "/tmp/%s_%d_%s_%s_%s.smt2"
        trace_name key polname solver_name opt


    method! private visit_instr_before key tr_inst _ =
      (* if key = 377147 then (Logger.result "Into instruction:"; Logger.set_verbosity 4); *)
      Logger.info ~level:1 "==== %d %Lx %s ===="
        key tr_inst.location tr_inst.mnemonic;
      (* if key = 20 then StopExec else *)
      DoExec

    method! private visit_dbainstr_before _ _ stmt _ =
      Logger.info ~level:2 "→ %a" Dba_types.Statement.pp stmt ;
      (match Dba_types.Statement.instruction stmt with
       | Dba.Instr.Assign(Dba.LValue.Var {Dba.name; _},_,_) ->
         if is_flag name then nb_flags <- nb_flags +1
       | Dba.Instr.If(_,_,_) -> nb_if <- nb_if + 1
       | _ -> ()
      );
      nb_all <- nb_all +1;
      DoExec

    method! private visit_instr_after key tr_inst env =
      (* if key = 377205 then
         (Logger.result 0 "Out instruction:"; Logger.verbosity := 0); *)
      if key = target then
        let name = self#get_file_name key env in
        let res, _, t =
          self#solve_predicate mk_bl_true
            ~push:true ~pop:true ~pruning ~get_model:false ~name:name env
        in
        result <- res;
        Logger.result "%d %Lx    %s:%a (%.04f)"
          key tr_inst.location tr_inst.mnemonic Formula_pp.pp_status res t;
        (match res with
         | UNSAT ->
            if try_find_conflicting then
              self#try_find_conflict key false key 0 key env
         | _ -> ());
        found <- true;
        StopExec
      else
        DoExec


    method! private post_execution (env:Path_predicate_env.t): int =
      let exec_time = (Unix.gettimeofday ()) -. start_time in
      let not_found, addr_not_found, not_cmp, addr_not_cmp, replaced =
        Trace_postprocessing.get_merge_stats () in
      let mk_fold x =
        Basic_types.Addr64.Set.fold
          (fun elt acc -> Printf.sprintf "%s %Lx" acc elt) x "" in
      let addr_not_found_str = mk_fold addr_not_found in
      let addr_not_cmp_str = mk_fold addr_not_cmp in
      Logger.info "Replaced:%d" replaced;
      Logger.info "Not found:%d(%d) %s" not_found
        (Basic_types.Addr64.Set.cardinal addr_not_found) addr_not_found_str;
      Logger.info "Not cmp:%d(%d) %s"
        not_cmp (Basic_types.Addr64.Set.cardinal addr_not_cmp) addr_not_cmp_str;
      Logger.info "Flags:%d(%d%%)  If:%d  Total:%d"
        nb_flags ((nb_flags*100)/nb_all) nb_if nb_all;
      Logger.info "Path predicate exec time:%.02f" exec_time;
      if not found then begin
        let name = self#get_file_name 0 env in
        let res, _, t =
          self#solve_predicate mk_bl_true ~print_stat:true ~push:true ~pop:true
            ~pruning ~get_model:false ~name:name env in
        result <- res;
        Logger.result "Final: %a (%.04f)" Formula_pp.pp_status res t
      end;
      self#print_stat_row env;
      status_to_exit_code result


    method private dichotomie_next_index more max_index min_index current=
      let curr =
        if more then current + (max_index - current) / 2
        else max_index - (max_index - min_index) / 2 in
      Logger.debug "Before [min:%d, max:%d, i:%d] -> %d"
        min_index max_index current curr;
      curr

    method private try_find_conflict (key:int) (more:bool) (max_i:int) (min_i:int) (curr_i:int) (env:Path_predicate_env.t): unit =
      let new_off = self#dichotomie_next_index more max_i min_i curr_i in
      if new_off = curr_i then
        Logger.result "Index from which prek unsat:%d" min_i
      else
        let k = key - new_off in
        let name = self#get_file_name k env in
        let res, _, _ =
          self#solve_predicate mk_bl_true ~push:true ~pop:true ~pruning:true
            ~prek:k ~get_model:false ~name:name env in
        Logger.result "Test k:%d  (%d) -> %a" k curr_i Formula_pp.pp_status res;
        match res with
        | UNSAT ->
          self#try_find_conflict key true  max_i new_off new_off env         (* new_off min_i new_off env *)
        | SAT -> self#try_find_conflict key false new_off min_i new_off env  (*  max_i new_off new_off env *)
        | _ -> Logger.warning "%a" Formula_pp.pp_status res



    method print_stat_row (env:Path_predicate_env.t): unit =
      let nb0 = List.length env.formula.hybrid_memory in
      let min, max, moy, found, rebase, disjoint = Path_predicate_optim.get_row_stats () in

      Logger.result
        "Size:%d\t\tMin:%d\tMax:%d\tMoy:%d\t\tFound:%d\tRebase:%d\tDisjoint:%d"
        nb0 min max moy found rebase disjoint
  end;;
