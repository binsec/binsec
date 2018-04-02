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
open Configuration
open Options
open Smtlib2
open Formula_type
open Analysis_config
open Standard_analysis
open Specific_parameters_t

let is_flag (s:string): bool =
  List.mem s ["OF";"SF";"ZF";"AF";"PF";"CF"]

class stat_analyzer (input_config:Options.trace_analysis_config) =
  object(self) inherit dse_analysis input_config

  val! trace_name = Filename.basename input_config.trace_file |> Filename.chop_extension
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

  method private get_file_name (key:int) (env:Path_pred_env.t): string =
    let opt = Formula.optim_str env.formula in
    let polname = Filename.basename !Options.pol_name |> Filename.chop_extension  in
    let solver_name =
      match input_config.configuration.solver with
      | `z3 -> "Z3"
      | `boolector -> "BOOLECTOR"
      | `cvc4 -> "CVC4"
      | `yices -> "YICES"
    in
    Format.sprintf "/tmp/%s_%d_%s_%s_%s.smt2" trace_name key polname solver_name opt


  method! private visit_instr_before (key:int) (tr_inst:trace_inst) _: trace_visit_action =
    (* if key = 377147 then (Logger.result "Into instruction:"; Logger.set_verbosity 4); *)
    Logger.info ~level:1 "==== %d %Lx %s ====" key tr_inst.location tr_inst.opcode;
    (* if key = 20 then StopExec else *)
    DoExec

  method! private visit_dbainstr_before (_key:int) (_tr_inst:trace_inst) (dbainst:Dba_types.Statement.t) (_env:Path_pred_env.t): trace_visit_action =
    Logger.info ~level:2 "→ %a" Dba_types.Statement.pp dbainst;
    (match Dba_types.Statement.instruction dbainst with
    | Dba.IkAssign(Dba.LhsVar(name,_,_),_,_) -> if is_flag name then nb_flags <- nb_flags +1
    | Dba.IkIf(_,_,_) -> nb_if <- nb_if +1
    | _ -> ()
    );
    nb_all <- nb_all +1;
    DoExec

  method! private visit_instr_after (key:int) (tr_inst:trace_inst)  (env:Path_pred_env.t): trace_visit_action =
    (* if key = 377205 then (Logger.result 0 "Out instruction:"; Logger.verbosity := 0); *)
    if key = target then
      let name = self#get_file_name key env in
      let res, _, t = self#solve_predicate (SmtTrue) ~push:true ~pop:true ~pruning:!Options.pruning ~get_model:false ~name:name env in
      result <- res;
      Logger.result "%d %Lx    %s:%a (%.04f)" key tr_inst.location tr_inst.opcode Smtlib2print.pp_smt_result res t;
      (match res with UNSAT -> if try_find_conflicting then self#try_find_conflict key false key 0 key env | _ -> ());
      found <- true;
      StopExec
    else
      DoExec


  method! private post_execution (env:Path_pred_env.t): int =
    let exec_time = (Unix.gettimeofday ()) -. start_time in      
    let not_found, addr_not_found, not_cmp, addr_not_cmp, replaced = Trace_postprocessing.get_merge_stats () in
    let addr_not_found_str = Basic_types.Addr64.Set.fold (fun elt acc -> Printf.sprintf "%s %Lx" acc elt) addr_not_found "" in
    let addr_not_cmp_str = Basic_types.Addr64.Set.fold (fun elt acc -> Printf.sprintf "%s %Lx" acc elt) addr_not_cmp "" in
    Logger.info "Replaced:%d" replaced;
    Logger.info "Not found:%d(%d) %s" not_found (Basic_types.Addr64.Set.cardinal addr_not_found) addr_not_found_str;
    Logger.info "Not cmp:%d(%d) %s" not_cmp (Basic_types.Addr64.Set.cardinal addr_not_cmp) addr_not_cmp_str;
    Logger.info "Flags:%d(%d%c)  If:%d  Total:%d" nb_flags ((nb_flags*100)/nb_all) '%' nb_if nb_all;
    Logger.info "Path predicate exec time:%.02f" exec_time;
    if not(found) then begin
      let name = self#get_file_name 0 env in
      let res, _, t = self#solve_predicate (SmtTrue) ~print_stat:true ~push:true ~pop:true ~pruning:!Options.pruning ~get_model:false ~name:name env in
      result <- res;
      Logger.result "Final: %a (%.04f)" Smtlib2print.pp_smt_result res t
    end;
    self#print_stat_row env;
    smt_result_to_exit_code result


  method private dichotomie_next_index (more:bool) (max_index:int) (min_index:int) (current:int): int=
    let curr =
    if more then
        current + ((max_index-current)/2)
    else
        max_index - ((max_index-min_index)/2)
    in
    Logger.debug "Before [min:%d, max:%d, i:%d] -> %d" min_index max_index current curr;
    curr

  method private try_find_conflict (key:int) (more:bool) (max_i:int) (min_i:int) (curr_i:int) (env:Path_pred_env.t): unit =
    let new_off = self#dichotomie_next_index more max_i min_i curr_i in
    if new_off = curr_i then
      Logger.result "Index from which prek unsat:%d" min_i
    else
      let k = key - new_off in
      let name = self#get_file_name k env in
      let res, _, _ = self#solve_predicate (SmtTrue) ~push:true ~pop:true ~pruning:true ~prek:k ~get_model:false ~name:name env in
      Logger.result "Test k:%d  (%d) -> %a" k curr_i Smtlib2print.pp_smt_result res;
      match res with
      | UNSAT ->
        self#try_find_conflict key true  max_i new_off new_off env         (* new_off min_i new_off env *)
      | SAT -> self#try_find_conflict key false new_off min_i new_off env  (*  max_i new_off new_off env *)
      | _ -> Logger.warning "%a" Smtlib2print.pp_smt_result res



  method print_stat_row (env:Path_pred_env.t): unit =
    let nb0 = List.length env.formula.hybrid_memory in
    let min, max, moy, found, rebase, disjoint = Formula_optim.get_row_stats () in

    Logger.result
        "Size:%d\t\tMin:%d\tMax:%d\tMoy:%d\t\tFound:%d\tRebase:%d\tDisjoint:%d"
        nb0 min max moy found rebase disjoint
  end;;
