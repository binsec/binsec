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
open Smtlib2print
open Decode_utils
open Configuration
open Options
open Smtlib2

open Analysis_config_piqi
open Specific_parameters_t
open Callret_analysis_results
open Standard_analysis


let compare_st e1 e2 = if e1 = e2 then 0 else -1
  
module StatusSet = Set.Make (struct type t = callret_analysis_results_callret_status let compare = compare_st end)

type labels = (* Violable | *) Aligned | Disaligned | Can_Return | Single | Multiple | Strong | Weak | Solver_Wrong | No_Call | Has_Returned

let compare_label e1 e2 =
  match e1,e2 with
  | x1, x2 when x1 = x2 -> 0
  | _ -> -1

module LabelSet = Set.Make (struct type t = labels let compare = compare_label end)

let label_to_variant lb =
  match lb with
  (* | Violable -> `violable *)
  | Aligned -> `aligned
  | Disaligned -> `disaligned
  | Can_Return -> `can_return
  | Single -> `single
  | Multiple -> `multiple
  | Strong -> `strong
  | Weak -> `weak
  | Solver_Wrong -> `solver_wrong
  | No_Call -> `no_call
  | Has_Returned -> `has_returned

let label_to_s x =
  match x with
  (* | Violable -> "violable" *)
  | Aligned -> "aligned"
  | Disaligned -> "disaligned"
  | Can_Return -> "can_return"
  | Single -> "single"
  | Multiple -> "multiple"
  | Strong -> "strong"
  | Weak -> "weak"
  | Solver_Wrong -> "solver_wrong"
  | No_Call -> "no_call"
  | Has_Returned -> "has_returned"

let smt_res_to_e res res_not =
  match res, res_not with
  | SAT, SAT -> `sat
  | SAT, UNSAT -> `valid
  | UNSAT, _ -> `unsat
  | _,_ -> `unknown

let pp_viol fmt res =
  match res with
  | `ok -> Format.fprintf fmt "@{<green>OK@}"
  | `viol -> Format.fprintf fmt "@{<red>VIOL@}"

let pp_smt_e fmt st =
  match st with
  | `sat -> Format.fprintf fmt "@{<blue>SAT@}"
  | `valid -> Format.fprintf fmt "@{<green>VALID@}"
  | `unsat -> Format.fprintf fmt "@{<red>UNSAT@}"
  | `unknown -> Format.fprintf fmt "@{<purple>TIMEOUT@}"

let status_to_final addr_st cnt_st is_egal additional_st: callret_analysis_results_callret_status * labels list =
  let st(* , lbs *) = if is_egal then `ok(* , [Violable; Weak] *) else `viol(* , [Can_Return; Weak] *) in
  match cnt_st with
  | `sat ->
    begin match addr_st with
    | `sat -> st, [](* lbs *)
    | `valid -> st, [Aligned] (* List.rev (Aligned::lbs) *)
    | `unsat -> st, [Disaligned](* List.rev (Disaligned::lbs) *)
    | `unknown -> st, []
    end
  | `valid ->
    begin match addr_st with
    | `unknown | `sat -> `ok, []
    | `valid -> `ok, [Strong; Aligned]
    | `unsat -> `ok, [Weak; Disaligned]
    end
  | `unsat ->
    if is_egal then
      `ok, [Solver_Wrong]
    else
      let card = match additional_st with | SAT -> [(*MULTIPLE*)] | UNSAT -> [Single] | _ -> [] in
      begin match addr_st with
      | `unknown | `sat -> `viol, card
      | `valid -> `viol, Aligned::card
      | `unsat -> `viol, Strong::Disaligned::card
      end
  | `unknown -> st, []


let labels_to_s lbs =
  List.fold_left (fun acc i -> Printf.sprintf "%s [%s]" acc (label_to_s i)) "" lbs

type ret_status = {
  status: callret_analysis_results_callret_status;
  labels: labels list;
  call_addr: int64 option;
  returnsite: int64;
}

class callret_analyzer (input_config:Options.trace_analysis_config) =
  object(self) inherit dse_analysis input_config

    val mutable results = default_callret_analysis_results ()
    val mutable uniq_match = false
    val mutable target_addr = 0L
    val mutable target_exists = false

    val mutable call_stack = [] (* (offset, addr, name, returnsite) list *)
    val mutable witness_counter = 0
    val addr_matched = Hashtbl.create 30
    val ret_map = Hashtbl.create 30

    method private fresh_witness_name (): string =
      let name = "witness"^(string_of_int witness_counter) in
      witness_counter <- witness_counter +1;
      name

    method! private pre_execution _ =
      match input_config.configuration.additional_parameters with
      | None -> ()
      | Some params ->
        begin match params.standard_params with
          | Some params ->
            uniq_match <- (match params.uniq with | None -> false | Some x -> x);
            target_addr <- (match params.target_addr with | None -> 0L | Some x -> x);
            target_exists <- not (Utils.is_none params.target_addr)
          | _ -> Logger.debug "No additional_parameters provided"
        end

    method private next_is_jmp_not_traced (key:int): bool =
      let trace_instrs = trace.Trace_type.instrs in
      InstrMap.mem (key+1) trace_instrs
      &&
      let next_i = InstrMap.find (key+1) trace_instrs in
      match next_i.Trace_type.dbainstrs with
      | { Dba_types.Statement.location = _; instruction = Dba.IkDJump(_, None) } :: _ ->
        not (is_traced next_i.Trace_type.concrete_infos)
      | _ :: _ | [] -> false

    method! private visit_dbainstr_before (key:int) (tr_inst:trace_inst)
        (dba_inst:Dba_types.Statement.t) (env:Path_pred_env.t): trace_visit_action =
      let addr = tr_inst.location in
      match Dba_types.Statement.instruction dba_inst with
      | Dba.IkSJump(_, Some(Dba.Call(_)))
      | Dba.IkDJump(_, Some(Dba.Call(_))) ->
        if is_traced tr_inst.concrete_infos && not(self#next_is_jmp_not_traced key) then
          let returnsite = get_store_content tr_inst.concrete_infos |> string_to_big_int |> Bigint.int64_of_big_int in
          let wit_addr = self#fresh_witness_name () in
          let addr_e = Dba.ExprVar("esp",32, None) in
          self#add_witness_variable wit_addr addr_e env;
          let wit_cnt = self#fresh_witness_name () in
          let cnt_e = Dba.ExprLoad(4, Dba.LittleEndian, Dba.ExprVar("esp",32, None)) in
          self#add_witness_variable wit_cnt cnt_e env;
          call_stack <- (key, addr, wit_addr, wit_cnt, returnsite)::call_stack;
          DoExec
        else
          DoExec
      | Dba.IkDJump(e, Some(Dba.Return)) ->
        let already_matched = Hashtbl.mem addr_matched addr in
        if  (not(uniq_match) && ((not(target_exists) && target_addr=0L) || (target_exists && target_addr=addr))) ||
            ((not(already_matched) && uniq_match) &&
              ((target_exists && target_addr = addr) || (not(target_exists) && target_addr =0L))) then
          let ret_value = self#concretize_expr_bv e env in
          begin match call_stack with
            | [] ->
              Logger.warning "Nothing in call stack";
              let f = self#build_address_comparison_predicate e ret_value env in
              let res,_,_ = self#solve_predicate (SmtNot(f)) ~push:true ~pop:false ~prek:25 env in
              let labels = match res with | UNSAT -> [No_Call; Single] | _ -> [No_Call] in
              self#update_result addr `viol labels None (Bitvector.value_of ret_value |> Bigint.int64_of_big_int);
              Logger.result "#%d Ret:0x%Lx [-,-,-] (v:-, k:-) -> %a: [%s]" key addr pp_viol `viol (labels_to_s labels);
              DoExec
            | (call_offset, call_addr, witness_addr, witness_cnt, returnsite)::tl ->
              let k_value = key-call_offset in
              let is_ok = Bigint.big_int_of_int64 returnsite |> Bigint.eq_big_int (Bitvector.value_of ret_value) in
              self#solve_violation key addr witness_addr witness_cnt is_ok k_value e call_addr ret_value env;
              call_stack <- tl;
              Hashtbl.add addr_matched addr false;
              if uniq_match && addr = target_addr then StopExec else DoExec
          end
        else
          (if call_stack <> [] then call_stack <- List.tl call_stack;
           DoExec)
      | Dba.IkSJump(_,None) when not(is_traced tr_inst.concrete_infos) -> (* Static jump not traced is tail call *)
        Logger.warning "Tail call detected #%d at:0x%Lx %s" key tr_inst.location tr_inst.opcode;
        if call_stack <> [] then call_stack <- List.tl call_stack; (* Pop the last call *)
        DoExec
      | _ -> DoExec

    method private solve_violation (key:int) (addr:int64) (witness_addr:string) (witness_cnt:string) (ok:bool) (k_val:int)
      (e:Dba.expr) (call_addr:int64) (ret_value:Bitvector.t) (env:Path_pred_env.t): unit =
      let k_val = if !Options.experiment then -1 else k_val in
      match e with
      | Dba.ExprLoad(_, _, e_addr) ->
        let f_addr =
          self#build_witness_expr_comparison_predicate witness_addr env.addr_size e_addr env in
        let f_cnt =
          self#build_witness_expr_comparison_predicate witness_cnt env.addr_size e env in
        (* let name = Printf.sprintf "/tmp/%d" key in *)
        let res_addr,_,_ = self#solve_predicate f_addr (* ~name:(name^"_step1.smt2") *) ~push:true ~pop:true ~prek:k_val env in
        let res_addr2,_,_ =
          self#solve_predicate (SmtNot(f_addr)) (* ~name:(name^"_step1.smt2") *) ~push:true ~pop:true ~prek:k_val env in
        let res_cnt,_,_ =
          self#solve_predicate f_cnt (* ~name:(name^"_step2.smt2") *) ~push:true ~pop:true ~prek:k_val env in
        let res_cnt2,_,_ =
          self#solve_predicate (SmtNot(f_cnt)) (* ~name:(name^"_step2.smt2") *) ~push:true ~pop:true ~prek:k_val env in
        let addr_status = smt_res_to_e res_addr res_addr2 in
        let cnt_status = smt_res_to_e res_cnt res_cnt2 in
        let res_additional =
          match res_cnt with
          | UNSAT ->
            let f = self#build_address_comparison_predicate e ret_value env in
            let res,_,_ =
              self#solve_predicate (SmtNot f) (* ~name:(name^"_step3.smt2") *) ~push:true ~pop:false ~prek:k_val env in res
          | _ -> UNKNOWN
        in
        let final_st, labels = status_to_final addr_status cnt_status ok res_additional in
        let label_s = labels_to_s labels in
        let ret_i64 = Bitvector.value_of ret_value |> Bigint.int64_of_big_int in
        let _ = self#update_result addr final_st labels (Some(call_addr)) ret_i64 in
        Logger.result "#%d Ret:0x%Lx [%a,%a,%a] (v:%b, k:%d) -> %a:%s"
          key addr pp_smt_e addr_status pp_smt_e cnt_status pp_smt_result
          res_additional (not ok) k_val pp_viol final_st label_s;
      | _ -> Logger.error "Ret not of the expected form !"


    method private update_result
        (addr:int64)
        (status:callret_analysis_results_callret_status)
        (labels:labels list)
        (call_addr:int64 option) (returnsite:int64) =
      let ret = {status; labels; returnsite; call_addr} in
      try
        let rets = Hashtbl.find ret_map addr in
        Hashtbl.replace ret_map addr (ret :: rets)
      with
      | Not_found -> Hashtbl.add ret_map addr [ret]


    method private add_ret_results (addr:int64) (rets:ret_status list): unit =
      let is_wrong ret = List.fold_left (fun acc i -> acc || i = Solver_Wrong) false ret.labels in
      let rec merge_labels acc_st new_st lbset lb_list =
        match lb_list with
        | [] -> lbset
        | lb::tl ->
          let _already_in = LabelSet.mem lb lbset in
          let newset =
          match lb with
          (* | Violable -> if acc_st = `ok && new_st = `ok then LabelSet.add lb lbset else lbset *)
          | Aligned -> if LabelSet.mem Disaligned lbset then LabelSet.remove Disaligned lbset else LabelSet.add lb lbset
          | Disaligned -> if LabelSet.mem Aligned lbset then LabelSet.remove Aligned lbset else LabelSet.add lb lbset
          | Can_Return -> LabelSet.add lb lbset
          | Single -> if LabelSet.mem Multiple lbset then LabelSet.remove Multiple lbset else LabelSet.add lb lbset
          | Multiple -> if LabelSet.mem Single lbset then LabelSet.remove Single lbset else LabelSet.add lb lbset
          | Strong -> if LabelSet.mem Weak lbset then LabelSet.remove Weak lbset else LabelSet.add lb lbset
          | Weak -> if LabelSet.mem Strong lbset then LabelSet.remove Strong lbset else LabelSet.add lb lbset
          | Solver_Wrong -> if acc_st <> new_st then lbset else LabelSet.add lb lbset
          | No_Call -> if acc_st = `viol && not(LabelSet.mem No_Call lbset) then lbset else LabelSet.add lb lbset
          | Has_Returned -> LabelSet.add lb (LabelSet.remove Can_Return lbset)
          in
          merge_labels acc_st new_st newset tl
      in
      let consolidate_status (st, lbset) ret =
      if is_wrong ret then
        (st, lbset)
      else
        match st, ret.status with
        | `ok, `viol ->
          ret.status, LabelSet.of_list ret.labels |> LabelSet.remove Can_Return |> LabelSet.add Has_Returned  (* Delete all previous labels *)
        | `viol, `ok ->
          st, LabelSet.add Has_Returned lbset             (* Ignore all labels of the current *)
        | `ok, `ok | `viol, `viol ->
          st, merge_labels st ret.status lbset ret.labels
      in
      let consolidate_call_addrs rets =
        let final_calls = List.fold_left (fun set ret ->
          match ret.call_addr with
          | None -> set
          | Some(addr) ->
            try Basic_types.Int64.Map.add addr
                  (StatusSet.add ret.status
                     (Basic_types.Int64.Map.find addr set)) set
            with Not_found ->
              Basic_types.Int64.Map.add addr
                (StatusSet.add ret.status StatusSet.empty) set
            ) Basic_types.Int64.Map.empty rets
        in
        Basic_types.Int64.Map.fold (fun addr sts acc ->
            (StatusSet.fold
               (fun st nacc ->
                 {addr; Callret_analysis_results_call_data.status = st} :: nacc
               ) sts []) @ acc)
          final_calls []
      in
      let first = List.hd rets in
      let final_status, labels = List.fold_left consolidate_status (first.status, LabelSet.of_list first.labels) rets in
      let final_callsites = List.fold_left (fun set ret ->
          Basic_types.Int64.Set.add ret.returnsite set) Basic_types.Int64.Set.empty rets |> Basic_types.Int64.Set.elements in
      let final_calls = consolidate_call_addrs rets in
      let res = ({ret_addr=addr;
                  Callret_analysis_results_ret_data.status = final_status;
                  Callret_analysis_results_ret_data.labels =
                    LabelSet.elements labels |> List.map label_to_variant;
                  returnsites = final_callsites;
                  solve_count = List.length rets |> Int32.of_int;
                  calls = final_calls})
      in
      results.values <- res::results.values

    method private print_stats (): unit =
      let count_ok, count_viol =
        List.fold_left
          (fun (ok, violated) e ->
             match e.Callret_analysis_results_ret_data.status with
             | `ok   -> ok + 1, violated
             | `viol -> ok, violated + 1
          ) (0, 0) results.values in
      Logger.result "Ok:%d Viol:%d" count_ok count_viol

    method! private post_execution _ =
      Hashtbl.iter (fun addr st -> self#add_ret_results addr st) ret_map;
      let data = Piqirun.to_string (gen_callret_analysis_results results) in
      match input_config.trace_input with
      | Chunked (_,_) ->
        self#print_stats ();
        let polname = Filename.basename !Options.pol_name |> Filename.chop_extension  in
        let trace_name = Filename.basename input_config.trace_file |> Filename.chop_extension  in
        let filename = Printf.sprintf "%s/%s_callret_%s.pb" (Filename.dirname input_config.trace_file) trace_name polname in
        let ic = open_out filename in
        output_string ic data;
        close_out ic;
        0
      | Stream _ ->
        self#send_message "ANALYSIS_RESULTS" data;
        0


  end;;
