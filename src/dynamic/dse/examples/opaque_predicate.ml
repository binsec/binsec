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
open Config_piqi
open Trace_type
open Dba
open Configuration
open Options
open Smtlib2

open Analysis_config_piqi
open Specific_parameters_t
open Po_analysis_results
open Standard_analysis

let smt_to_status res1 =
  match res1 with
  | SAT -> `unknown
  | UNSAT -> `opaque
  | _ -> `likely

let status_to_str status =
  match status with
  | `unknown -> "unknown"
  | `not_opaque -> "not opaque"
  | `opaque -> "opaque"
  | `likely -> "likely"

let rec is_sub_list l1 l2 acc = (* l1 is a sublist of l2 *)
  match l1,l2 with
  | h1 :: tl1, h2 :: tl2 ->
    if h1 <> h2 then false else is_sub_list tl1 tl2 acc
  | [], _ -> acc
  | _, [] -> false

let l_to_s l =
  let s = List.fold_left (fun acc i -> Printf.sprintf "%s,%Lx" acc i) "" l in
  String.sub s 1 ((String.length s)-1)

(* let p_to_s path =
  List.fold_left (fun acc i -> Printf.sprintf "%s%Lx," acc i) "" path
 *)

type path = int64 list
                                              (* current_ret * paths_to target   * solve time * k *)
type branch_state = Covered of int64 | Solved | Empty

type if_status = {
  left: branch_state;
  right: branch_state;
  paths: (int, path list) Hashtbl.t;
  status_map: (int, (smt_result * float)) Hashtbl.t;
}

class opaque_analyzer (input_config:Options.trace_analysis_config) =
  object(self) inherit dse_analysis input_config

    val mutable results = default_po_analysis_results ()
    val mutable target_addr = 0L
    val mutable target_exists = false
    val mutable gather_formula = false
    val mutable k_values = [2; 4; 6; 8; 10; 12; 14; 16; 18; 20; 22; 24; 26; 28; 30] (* 32; 34; 36; 38; 40; 42; 44; 46; 48; 50] *)

    val mutable if_map = Basic_types.Addr64.Map.empty
    val mutable do_multipath = true

    method! private pre_execution (_env:Path_pred_env.t) : unit =
      if !Options.experiment then (do_multipath <- false; Logger.warning "multi-path disabled !");
      if input_config.configuration.ksteps <> 0l then k_values <- [Int32.to_int input_config.configuration.ksteps];
      match input_config.configuration.additional_parameters with
      | None -> ()
      | Some params ->
        begin match params.standard_params with
          | Some params ->
            target_addr <- (match params.target_addr with | None -> 0L | Some x -> x);
            target_exists <- not (Utils.is_none params.target_addr);
            gather_formula <- (match params.get_formula with | None -> false | Some x -> x)
          | _ -> Logger.debug "No additional_parameters provided"
        end

    method private update_if_mapping (addr:int64) (next:int64): unit =
      if Basic_types.Addr64.Map.mem addr if_map then
        let state = Basic_types.Addr64.Map.find addr if_map in
        match state.left, state.right with
        | _, Covered _ -> ()
        | Covered(x), Solved ->
          if next <> x then
            begin
              Logger.result "Replace Solved status by Fully Covered: %Lx" addr;
              if_map <- Basic_types.Addr64.Map.add addr {state with right=Covered(next)} if_map
            end
        | Covered(x), Empty ->
          if next <> x then
            begin
              Logger.result "Replace status by Fully covered: %Lx" addr;
              if_map <- Basic_types.Addr64.Map.add addr {state with right=Covered(next)} if_map
            end
        | _ -> failwith "Impossible branch state"
      else
        let size = List.length k_values in
        let h = Hashtbl.create size in
        let _ = List.iter (fun i -> Hashtbl.add h i []) k_values in
        if_map <- Basic_types.Addr64.Map.add addr {left=Covered(next); right=Empty; paths=h; status_map=Hashtbl.create size} if_map

    method private is_partial_if_to_check (addr:int64) (_next:int64): bool =
      let status = Basic_types.Addr64.Map.find addr if_map in
      match status.left,status.right with
      | Covered _, Empty -> true (* and it is still not covered on both branches *)
      | _ -> false

    method! private visit_instr_before (_key:int) (tr_inst:trace_inst) (_env:Path_pred_env.t): trace_visit_action =
      (* Logger.result "%d %Lx    %s " key tr_inst.location tr_inst.opcode; *)
      let mask = 0xf0000000L in
      if Int64.logand tr_inst.location mask = mask then
        (Logger.warning "Enter library stop execution"; StopExec)
(*       else if key = 15 then begin
        Logger.warning "Stop execution 20000 instr reached";
        StopExec
      end *)
      else DoExec

    method! private visit_dbainstr_before (key:int) (tr_inst:trace_inst) (dba_inst:Dba_types.Statement.t) (env:Path_pred_env.t): trace_visit_action =
      let addr = tr_inst.location in
      match Dba_types.Statement.instruction dba_inst with
      | Dba.IkIf (cond, JOuter a , _) ->
        let address = Dba_types.Caddress.base_value a in
        if  (not(target_exists) && target_addr=0L) || (target_exists && target_addr=addr) then
          let next_l = get_next_address tr_inst.concrete_infos in
          self#update_if_mapping addr next_l;
          let status = Basic_types.Addr64.Map.find addr if_map in
          let pred = self#build_cond_predicate cond env in
          let pred = if Bigint.eq_big_int address (Bigint.big_int_of_int64 next_l) then SmtNot(pred) else pred in
          if self#is_partial_if_to_check addr next_l then
            begin
              Logger.result "%d %Lx    %s " key tr_inst.location tr_inst.opcode;
              self#try_complete_values status key pred env;
              if_map <- Basic_types.Addr64.Map.add addr {status with right=Solved} if_map;
              DoExec
            end
          else
            if do_multipath then
              begin
                self#try_complete_values status key pred env;
                DoExec
              end
            else DoExec
        else
          DoExec
      | _ -> DoExec


    method private compute_path (cur:int) (k:int): path =
      let filtered = InstrMap.filter (fun key _ -> key <= cur && key >= (cur-k)) trace.Trace_type.instrs in
      InstrMap.fold (fun _ inst acc -> inst.location::acc) filtered []   (* Return path from: 0(jmp) -> K *)


    method private try_complete_values (state:if_status) (key:int) (pred:smt_expr) (env:Path_pred_env.t): unit =
      let left l x =
        List.fold_left
          (fun (i,acc) elt ->
             if i <= x then (i+1,elt::acc) else (i+1,acc)) (0,[]) l
        |> snd in
      let iterate_k (k,k_path) =                      (* For each k along with its path *)
        (* let _ = Logger.debug "K %d: %s" k (List.fold_left (fun acc i -> Printf.sprintf "%s,%Lx" acc i) "" k_path) in *)
        let paths = Hashtbl.find state.paths  k in    (* Get the path already covered for this k *)
        let path_different = not(List.fold_left (fun acc path -> (is_sub_list k_path path true) || acc) false paths) in (* check if the path is different *)
        if path_different then
        begin
          let newpaths = List.filter (fun p -> not(is_sub_list p k_path true)) paths in (* Remove any path that is a subpath of the current*)
          Hashtbl.replace state.paths k (k_path::newpaths); (* Add the path to the covered paths *)
          if Hashtbl.mem state.status_map k then
            let st, _time = Hashtbl.find state.status_map k in                     (* Get the current status for this k *)
            match st with
            | UNSAT | TIMEOUT ->                                                  (* If not yet solved try again *)
              let new_st, new_t = self#solve_opaqueness key k pred env in           (* Compute *)
              Hashtbl.replace state.status_map k (new_st,new_t)
            | _ -> ()
          else
            self#solve_opaqueness key k pred env |> Hashtbl.replace state.status_map k (* Compute values for the first time *)
        end
        else
          ()
      in
      let max_k = List.fold_left (fun acc i -> max acc i) 0 k_values in   (* Max value in k values *)
      let path = self#compute_path key max_k in                           (* Compute the path for the k max *)
      List.fold_left (fun acc k ->
        let sub_path = left path k in
        (* if (List.length sub_path) = k+1 then *)
          (k,sub_path)::acc
        (* else *)
          (* acc *)
      ) [] k_values |>List.rev |> (* Create a list of all path for every k and iterate over it *)
      List.iter iterate_k


    method private solve_opaqueness (key:int) (k:int) (pred:smt_expr) (env:Path_pred_env.t) =
      let res, _, t = self#solve_predicate pred ~push:true ~pop:true ~name:"" ~prek:k env in
      (match res with
        | UNSAT when k = 2 ->
          let name = Printf.sprintf "/tmp/%s_%d_%d.smt2" trace_name key k in
          Logger.warning "suspicious formula unsat for k:%s" name;
          File_utils.copy default_formula_file name
        | _ -> ());
      (* Logger.result "k[%d]:%a(%.04f) " k_val Smtlib2print.pp_smt_result res t; *)
      res,t

    method private add_po_results arr (addr:int64) (if_state:if_status): unit =
      let to_i x = Int32.to_int x in
      let aux_add cov_addr addr status k t paths =
        let nb_paths = Some(List.length paths |> Int32.of_int) in
        let formula = if gather_formula then Some (File_utils.load default_formula_file) else None in
        if status = `opaque && k <= 20l then (let idx = to_i k in  arr.(idx) <- addr :: arr.(idx));
        let alive_branch = match status with `opaque -> Some cov_addr | _ -> None in
        (* let _ = Logger.warning "%Lx: k[%ld] paths:%d" addr k (List.length paths) in *)
        (* let _ = List.iter (fun p -> Logger.debug "%s" (p_to_s p)) paths in *)
        let newval =
          { Po_analysis_results_po_data.jmp_addr = addr;
            Po_analysis_results_po_data.status;
            Po_analysis_results_po_data.ksteps = k;
            Po_analysis_results_po_data.computation_time = t;
            Po_analysis_results_po_data.nb_paths;
            Po_analysis_results_po_data.alive_branch;
            Po_analysis_results_po_data.formula} in
        results.values <- newval :: results.values
      in
      match if_state.left, if_state.right with
      | Covered x, Covered _ ->
        aux_add x addr `not_opaque 0l 0. []
      | Covered(x), Solved ->
        Hashtbl.iter (fun k (res, t) ->
            aux_add x addr (smt_to_status res) (Int32.of_int k) t
              (Hashtbl.find if_state.paths k)) if_state.status_map
      | Covered(_), Empty -> failwith "Abnormal status Empty"
      | _, _ -> failwith "Abnormal status"


    method! private post_execution (_env:Path_pred_env.t) : int =
      let arr = Array.make 21 [] in (* Temporary storage to keep a list of PO ordered by k *)
      Basic_types.Addr64.Map.iter (fun addr st -> self#add_po_results arr addr st) if_map;
      if not(is_remote) then
      Array.iteri (fun k addr_l -> match addr_l with (* Print all the PO gathered *)
                                   | [] -> ()
                                   | _ -> Logger.result "k=%d(%d): %s" k (List.length addr_l) (l_to_s addr_l)) arr;

      let data = Piqirun.to_string (gen_po_analysis_results results) in
      match input_config.trace_input with
      | Chunked (_,_) ->
        let multi_str = if do_multipath then "_M" else "" in
        let filename = Printf.sprintf "%s/%s_opaque%s.pb" (Filename.dirname input_config.trace_file) trace_name multi_str in
        let ic = open_out filename in
        output_string ic data;
        close_out ic;
        0
      | Stream _ ->
        self#send_message "ANALYSIS_RESULTS" data;
        0

  end;;


class static_opaque_analyzer (input_config:Options.trace_analysis_config) =
  object(self) inherit dse_analysis input_config

    val mutable results = default_po_analysis_results ()
    val mutable target_addr = 0L
    val mutable target_exists = false
    val mutable k_value = 16
    val mutable if_map = Basic_types.Addr64.Map.empty
    val f_name = "/tmp/static_opaque.smt2"

    method! private pre_execution (_env:Path_pred_env.t) : unit =
      if input_config.configuration.ksteps <> 0l then k_value <- Int32.to_int input_config.configuration.ksteps;
      match input_config.configuration.additional_parameters with
      | None -> ()
      | Some params ->
        begin match params.standard_params with
          | Some params ->
            target_addr <- (match params.target_addr with | None -> 0L | Some x -> x);
            target_exists <- not (Utils.is_none params.target_addr)
          | _ -> Logger.debug "No additional_parameters provided"
        end

    method! private visit_dbainstr_before (_key:int) (tr_inst:trace_inst) (dba_inst) (env:Path_pred_env.t): trace_visit_action =
      let addr = tr_inst.location in
      try
        match dba_inst.Dba_types.Statement.instruction with
        | Dba.IkIf (cond, Dba.JOuter {base = address; _ }, off) ->
          let address1 = Bitvector.to_int64 address in
          let address2 =
            match (List.nth tr_inst.dbainstrs off).Dba_types.Statement.instruction with
            | Dba.IkSJump(JOuter {base = addr2; _},_) -> Bitvector.to_int64 addr2 | _ -> failwith "Goto not found" in
          if  (not(target_exists) && target_addr=0L) || (target_exists && target_addr=addr) then
            (let pred = self#build_cond_predicate cond env in
            let res1,_, t1 = self#solve_predicate pred ~name:f_name ~prek:k_value env in
            let res2,_, t2 = self#solve_predicate (SmtNot(pred)) ~prek:k_value env in
            let status, opt = self#get_final_status addr res1 res2 in
            let alive_branch = match opt with Some b -> Some(if b then address1 else address2) | None -> None in
            Logger.result "%Lx status:%s addr1:%Lx addr2:%Lx" addr (status_to_str status) address1 address2;
            self#add_po_results addr (t1 +. t2) status alive_branch);
          DoExec
        | _ -> DoExec
      with Failure s -> Logger.warning "%Lx skipped because of failure %s" addr s; DoExec

    method private get_final_status (addr:int64) (res1:smt_result) (res2:smt_result) =
      match res1, res2 with
      | SAT, SAT -> `not_opaque, None
      | UNSAT, UNSAT -> `likely, None
      | SAT, UNSAT -> `opaque, Some true
      | UNSAT, SAT -> `opaque, Some false
      | _ -> Logger.warning "%Lx final status Unknown:%s,%s" addr (Smtlib2print.smtres_to_string res1) (Smtlib2print.smtres_to_string res2); `unknown, None

    method private add_po_results (jmp_addr:int64) t status alive_branch: unit =
        let formula = Some (File_utils.load default_formula_file) in
        let newval =
          { Po_analysis_results_po_data.jmp_addr;
            Po_analysis_results_po_data.status;
            Po_analysis_results_po_data.ksteps = Int32.of_int k_value;
            Po_analysis_results_po_data.computation_time = t;
            Po_analysis_results_po_data.nb_paths = None;
            Po_analysis_results_po_data.alive_branch;
            Po_analysis_results_po_data.formula} in
        results.values <- newval :: results.values

    method! private post_execution (_env:Path_pred_env.t) : int =
      let data = Piqirun.to_string (gen_po_analysis_results results) in
      match input_config.trace_input with
      | Chunked (_,_) ->
        let filename = Printf.sprintf "%s/%s_opaque.pb" (Filename.dirname input_config.trace_file) "staticopaque" in
        let ic = open_out filename in output_string ic data; close_out ic; 0
      | Stream _ -> self#send_message "ANALYSIS_RESULTS" data; 0
  end
