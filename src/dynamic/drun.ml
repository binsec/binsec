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

open Config_piqi
open Configuration
open Trace_config

let run_sploit config =
  let analyzer = new Sploit1.sploit1 config in
  analyzer#compute

let run_switch config =
  let analyzer = new Switch.switch_analysis config in
  analyzer#compute

let run_flareon config =
  let analyzer = new Flareon.flare_one config in
  analyzer#compute

let run_opaque config =
  let analyzer = new Opaque_predicate.opaque_analyzer config in
  analyzer#compute

let run_generic config =
  let analyzer = new Generic_analyse.generic_analyzer config in
  analyzer#compute

let run_callret config =
  let analyzer = new Call_ret.callret_analyzer config in
  analyzer#compute

let run_stopaque config =
  let analyzer = new Opaque_predicate.static_opaque_analyzer config in
  analyzer#compute

let run_stat config =
  let analyzer = new Stat_analysis.stat_analyzer config in
  analyzer#compute

let run_branch config =
  let analyzer = new Branch_coverage.branch_coverage_analyzer config in
  analyzer#compute

let run_invert config =
  let analyzer = new InvertChild.invert_child config in
  let open Trace_options in
  analyzer#add_inst_key_to_invert  @@ Nth_to_invert.get ();
  analyzer#init_entries ();
  if Check_init.get () then analyzer#set_check_init "tmp.json";
  begin
    try
      let ret = analyzer#compute in
      let new_conf_files = analyzer#get_new_conf_files () in
      List.iter (fun x -> Logger.info "New JSON file %s" x) new_conf_files;
      ret
    with InvertChild.SHOULD_INIT -> -1
  end

let run_check config =
  let analyzer = new Check_trace.check_trace config in
  analyzer#init_entries ();
  let ret = analyzer#compute in
  let new_conf_files = analyzer#get_new_conf_files () in
  List.iter (fun x -> Dse_options.Logger.info "New JSON file %s" x) new_conf_files;
  ret

let run_eip config =
  let analyzer = new EipRewrite.eip_rewrite config in
  analyzer#add_inst_key_to_invert @@ Trace_options.Nth_to_invert.get ();
  analyzer#init_entries ();
  analyzer#compute

let run_uaf config =
  let open Trace_options in
  let analyzer = new Uaf_detection.uaf_detection config in
  analyzer#set_nth_alloc  @@ Nth_alloc.get ();
  analyzer#set_nth_free   @@ Nth_free.get ();
  analyzer#set_nth_use    @@ Nth_use.get ();
  analyzer#init_entries ();
  analyzer#compute

let run_default config =
  if Trace_options.Summary.get () then
    let analyzer = new Summary_analysis.summary_analyzer config in
    analyzer#compute
  else if Trace_options.AsCFG.get () then
    (Trace_loader.view_cfg config; 0)
  else begin
    Trace_loader.print_trace config.trace_input;
    0
  end

let load_policy_file configuration =
  if Trace_options.Policy_file.is_set () then
    let filename = Trace_options.Policy_file.get () in
    configuration.policy <- File_utils.readlines filename

let set_solver configuration =
  if not (Formula_options.Solver.is_default ()) then
    let solver = Formula_options.Solver.(get () |> to_piqi) in
    configuration.solver <- solver

let read_and_set_config_file config =
  if Trace_options.Config_file.is_set () then
    let raw = File_utils.load @@ Trace_options.Config_file.get () in
    let opts =
      Piqirun_ext.make_options ~json_omit_missing_fields:true () in
    config.configuration <-
      Config_piqi_ext.parse_configuration ~opts raw `json

let init_shared configuration =
  load_policy_file configuration;
  set_solver configuration;
  let open Trace_options in
  if not (Type.is_default ()) then
    configuration.direction <- Type.get ();
  if not (K.is_default ()) then
    configuration.ksteps <- Int32.of_int @@ K.get ();
  if not (Name.is_default ()) then
    configuration.analysis_name <- Name.get ();
  if not (Call_convention.is_default ()) then
       configuration.callcvt <- Call_convention.get ();
  if not (Default_action.is_default ()) then
    configuration.default_action <- Default_action.get ();
  if not (Incremental_solving.is_default ()) then
    configuration.incremental <- Incremental_solving.get ();
  if not (Formula_options.Solver.Timeout.is_default ()) then
    let timeout = Formula_options.Solver.Timeout.get () in
    configuration.timeout <- Int32.of_int timeout ;
  if not (Optimizations.RoW.is_default ()) then
    configuration.optim_row <- Optimizations.RoW.get ();
  if not (Optimizations.RoW_plus.is_default ()) then
    configuration.optim_rowplus <- Optimizations.RoW_plus.get ();
  if not (Optimizations.Equality_propagation.is_default ()) then
    configuration.optim_eqprop <- Optimizations.Equality_propagation.get ();
  if not (Optimizations.Constant_propagation.is_default ()) then
    configuration.optim_cstprop <- Optimizations.Constant_propagation.get ();
  if not (Optimizations.Rebase.is_default ()) then
    configuration.optim_rebase <- Optimizations.Rebase.get ()


let set_trace_file config =
  if Trace_options.Trace_file.is_set () then
    config.trace_file <- Trace_options.Trace_file.get ()

let set_trace_format config =
  let open Trace_options in
  config.trace_input <- Trace_format.get ();
  if Read_all.get () then
    config.trace_input <- Chunked(Pervasives.stdin, true)


let init c =
  (* First read configuration file if any then use command-line arguments if
  provided *)
  read_and_set_config_file c;
  set_trace_file c;
  set_trace_format c;
  init_shared c.configuration



let run () =
  try
    (*************** DSE analysis *****************)
    let config = Trace_config.default in
    init config;
    begin
      Format.set_margin 120;
      match config.trace_input with
      | Chunked(_,rall) ->
        let name = config.trace_file in
        if name = "" then
          failwith "No trace file provided (arg -tr-trace)"
        else
          config.trace_input <- Chunked(open_in_bin name, rall)
      | Stream _ ->
         let open Server_options in
        if Host_ip.is_set () then begin
          let ip = Host_ip.get () in
          let port = Server_port.get () in
          Network_io.connect_to_pin ip port;
          config.trace_input <- Stream("BINSEC");
          Logger.debug "Connected..";
          end
        else failwith "Trace format stream but no IP host defined"
    end;
    ignore (Libcall_stubs.check_libcall_policy_consistency
              config.configuration.libcalls
              config.configuration.default_action);
    let f =
      match config.configuration.analysis_name with
      | "sploit1" -> run_sploit
      | "switch"  -> run_switch
      | "flareon" -> run_flareon
      | "generic" -> run_generic
      | "callret" -> run_callret
      | "opaque"  -> run_opaque
      | "staticopaque" -> run_stopaque
      | "stat" -> run_stat
      | "branch" -> run_branch
      | "invert" -> run_invert
      | "check" -> run_check
      | "eip" -> run_eip
      | "uaf" -> run_uaf
      | "" -> run_default
      | s ->
        Dse_options.Logger.fatal "Unknown analysis name %s" s
    in let res = f config in
    let exit_value =
      match config.trace_input with
      | Chunked _  -> res
      | Stream _ ->
        Network_io.close_and_terminate_socket ();
        res
    in exit exit_value
  with Failure s ->
    Dse_options.Logger.error "Error: %s" s

let default_run () =
  if Trace_options.is_enabled () then run ()

let _ =
  Cli.Boot.enlist ~name:"trace/dse" ~f:default_run
