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

open Options
open Config_piqi
open Configuration
open Dse


let set_verbosity config =
  let n = Int32.to_int config.verbosity in
  Logger.set_debug_level n;
  Logger.set_info_level n;
  Logger.set_warning_level n

let solver_to_string slv =
  match slv with
  | `z3 -> "z3"
  | `boolector -> "boolector"
  | `yices -> "yices"
  | `cvc4 -> "cvc4"

let check_solver_existence solver =
  let solver_s = solver_to_string solver in
  let path = Sys.getenv "PATH" in
  let dirs = Str.split (Str.regexp ":") path in
  let found =
    List.exists (fun p -> Sys.file_exists (Filename.concat p solver_s)) dirs in
  if not found then
    failwith ("Solver "^solver_s^" not found in path")


let has_pin_pinsec () =
  try
    ignore(Sys.getenv "PIN");
    ignore(Sys.getenv "PINSEC");
    true
  with
    Not_found ->
    Logger.error
      "@[<v 2>\
       Please define $PIN and $PINSEC:@ \
       export PIN=...@ \
       export PINSEC=../libpinsec.so\
       @]";
    false

let maybe_load_and_exit () =
  if Options.get_describe_binary () then begin
    Logger.result "%a" Loader_utils.pp_loader_summary (Options.ExecFile.get ());
    exit 0;
  end


let maybe_patch () =
  if Options.ExecFile.is_set () &&
     Binpatcher_options.PatchFile.is_set () then
    begin
      Binpatcher.run ~executable:(Options.ExecFile.get ());
      exit 0
    end


let set_machdep_on_need () =
  match Machine.ISA.get () with
  | Machine.Unknown -> Loader_utils.set_arch ()
  | _ -> ()


let pp_info_exec_file () =
  if Options.ExecFile.is_set () &&
     Sys.file_exists (Options.ExecFile.get ())
  then Logger.info "Binary file set to %s" (Options.ExecFile.get ())


let _ =
  Options.parse_command_line ();
  Options.check_options ();

  maybe_load_and_exit ();
  maybe_patch ();
  pp_info_exec_file ();

  match !Options.command with
  | Options.Disasm ->
    if !Options.decode then begin
      Disasm.decode (Options.ExecFile.get ())
    end
    else if !Options.decode_llvm then begin
      Disasm.decode_llvm (Options.ExecFile.get ())
    end
    else if !Options.cfgraph then begin
      set_machdep_on_need ();
      Disasm_cfg.run ()
    end
    else begin
      set_machdep_on_need ();
      let configuration_file = Options.get_dba_configuration_filename () in
      Disasm.run ~configuration_file ()
    end
  | Options.Analyzer ->
    begin
      let configuration_file = Options.get_dba_configuration_filename () in
      let dba_file = Options.get_dba_filename () in
      Ai.run ~configuration_file ~dba_file ()
    end

  | Options.Simulator ->
    (*********************dynamic simulation********************)
    let dba_file = Options.get_dba_filename () in
    let configuration_file = Options.get_dba_configuration_filename () in
    set_machdep_on_need ();
    Simulate.run ~dba_file ~configuration_file ()

  | Options.Trace ->
    begin
      try
        (*************** DSE analysis *****************)
        begin
          Format.set_margin 120;
          set_verbosity Options.config.configuration;
          match Options.config.trace_input with
          | Options.Chunked(_,rall) ->
            let name = Options.config.trace_file in
            if name = "" then
              failwith "No trace file provided (arg -trace)"
            else
              Options.config.trace_input <- Chunked(open_in_bin name, rall)
          | Options.Stream _ ->
            let ip = !Options.ip_address in
            if ip = "" then
              failwith "Trace format stream but no IP host defined"
            else
              (Network_io.connect_to_pin ip !Options.port;
              config.trace_input <- Stream("BINSEC");
              Logger.debug "Connected..")
        end;
        ignore (Libcall_stubs.check_libcall_policy_consistency
                  config.configuration.libcalls
                  config.configuration.default_action);
        check_solver_existence Options.config.configuration.solver;
        let res =
          match Options.config.configuration.analysis_name with
          | "sploit1" ->
            let analyzer = new Sploit1.sploit1 config in
            analyzer#compute
          | "switch" ->
            let analyzer = new Switch.switch_analysis config in
            analyzer#compute
          | "flareon" ->
            let analyzer = new Flareon.flare_one config in
            analyzer#compute
          | "generic" ->
            let analyzer = new Generic_analyse.generic_analyzer config in
            analyzer#compute
          | "callret" ->
            let analyzer = new Call_ret.callret_analyzer config in
            analyzer#compute
          | "opaque" ->
            let analyzer = new Opaque_predicate.opaque_analyzer config in
            analyzer#compute
          | "staticopaque" ->
            let analyzer = new Opaque_predicate.static_opaque_analyzer config in
            analyzer#compute
          | "stat" ->
            let analyzer = new Stat_analysis.stat_analyzer config in
            analyzer#compute
          | "branch" ->
            let analyzer = new Branch_coverage.branch_coverage_analyzer config in
            analyzer#compute
          | "invert" ->
            let analyzer = new InvertChild.invert_child config in
            analyzer#add_inst_key_to_invert  !Options.nth_to_invert;
            analyzer#init_entries ();
            if !Options.check_init then analyzer#set_check_init "tmp.json";
            begin
              try
                let ret = analyzer#compute in
                let new_conf_files = analyzer#get_new_conf_files () in
                List.iter (fun x -> Logger.info "New JSON file %s" x) new_conf_files;
                ret
              with InvertChild.SHOULD_INIT -> -1
            end
          | "check" ->
            let analyzer = new Check_trace.check_trace config in
            analyzer#init_entries ();
            let ret = analyzer#compute in
            let new_conf_files = analyzer#get_new_conf_files () in
            List.iter (fun x -> Logger.info "New JSON file %s" x) new_conf_files;
            ret
          | "eip" ->
            let analyzer = new EipRewrite.eip_rewrite config in
            analyzer#add_inst_key_to_invert !Options.nth_to_invert;
            analyzer#init_entries ();
            analyzer#compute
          | "uaf" ->
            let analyzer = new Uaf_detection.uaf_detection config in
            analyzer#set_nth_alloc  !Options.nth_alloc;
            analyzer#set_nth_free  !Options.nth_free;
            analyzer#set_nth_use !Options.nth_use;
            analyzer#init_entries ();
            analyzer#compute
          | "" ->
            if !Options.summary_only then
              let analyzer = new Summary_analysis.summary_analyzer config in
              analyzer#compute
            else
             (Trace_loader.print_trace config.trace_input; 0)
          | s ->
             Logger.fatal "Unknown analysis name %s" s;
             exit 2
        in
        let exit_value =
          match Options.config.trace_input with
          | Options.Chunked(_,_) -> res
          | Options.Stream _ ->
            Network_io.close_and_terminate_socket ();
            res
        in exit exit_value
      with Failure s ->
        Logger.error "Error: %s" s
    end

  | Options.Test ->
    begin
      (*********** Test unsupported opcodes in a pintool trace *******)
      set_verbosity Options.config.configuration;
      if !Options.experiment then
        Test.experiment ()
      else
        begin match config.configuration.policy with
          | _ :: _ -> exit (Test.test_policy config.configuration.policy)
          | [] -> exit 0
        end
    end
  | Options.Serve ->
    set_verbosity Options.config.configuration;
    Server.single_thread_server_loop()

  | Options.DSE ->
    if has_pin_pinsec () then
      begin
        match !Options.dse_type with
        | "dot" ->
          let dir = "./" in
          let children = Sys.readdir dir in
          let traces =
            List.filter
              (fun x -> try String.sub x 0 6 = "trace_" with _ -> false)
              (Array.to_list children)
          in
          TracesToTree.traces_to_dot traces
        | "bfs" ->
          BfsDSE.explore !Options.bin_to_explore
            !Options.arg_dse !Options.config_seed_file  !Options.trace_limit_length
            !Options.random_seed !Options.exploration_timeout false
        | "random" ->
          RandomDSE.explore !Options.bin_to_explore !Options.arg_dse !Options.config_seed_file
            !Options.trace_limit_length !Options.random_seed !Options.exploration_timeout false
         | "uaf" ->
          UAFDSE.set_score ("l_event","sp_alloc","sp_free","sp_use");
          UAFDSE.set_criteria "l_event";
          UAFDSE.explore !Options.bin_to_explore !Options.arg_dse !Options.config_seed_file
            !Options.trace_limit_length !Options.random_seed !Options.exploration_timeout false
        | "uaf-strcmp" ->
          UafStrcmpDSE.set_score ("l_event","sp_alloc","sp_free","sp_use");
          UafStrcmpDSE.set_criteria "l_event";
          UafStrcmpDSE.explore !Options.bin_to_explore !Options.arg_dse !Options.config_seed_file
            !Options.trace_limit_length !Options.random_seed !Options.exploration_timeout true
        | _ -> failwith "unknown exploration type"
    end
  | Options.NoMode ->
    Logger.error
      "@[<v 0>No command given: \
       @[<hov 0>use of one@ disasm,@ simulate,@ analyse,@ test,@ trace@]@]";
    exit 1
