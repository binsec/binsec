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


type mode =
  | Analyzer
  | Simulator
  | Disasm
  | Trace
  | Test
  | NoMode
  | Serve
  | DSE


type trace_format =
  | Chunked of Pervasives.in_channel * bool
  | Stream of string

(* General options *)
let command = ref NoMode

(* Disassembly options *)
let opaque_predicates_file = ref ""
let violated_call_ret_file = ref ""
let cfgraph = ref false
let decode = ref false
let decode_llvm = ref false
let display_statistics = ref false

(* Global vars not assigned by command line
 * FIXME: Remove them!
 *)
let time_simpl = ref 0.
let time_disas = ref 0.
let time_nat_flag_recovery = ref 0.
let nb_conditional_cache_uses = ref 0
let nb_recovered_nat_predicates = ref 0
let nb_nat_predicate_recovery_tries = ref 0
let nb_failed_nat_predicate_recoveries = ref 0
let finalsize = ref 0
let initsize = ref 0
let itemps = ref 0
let iflags = ref 0
let ftemps = ref 0
let fflags = ref 0
let debug_file = ref ""
let cfg_file = ref "cfg_opcode.dot"
(* --------------- *)


(* Static analysis options *)
let nb_equalities_names = ref 0
let nb_equalities_classes = ref 0
let time_analysis = ref 0.
let time_equalities = ref 0.
let time_redundant_evals = ref 0.
let nb_equalities_refinement = ref 0
let nb_refined_lhs = ref 0
(* ------- *)


(* DSE Path exploration *)
let bin_to_explore = ref "/tmp/bin"
let arg_dse = ref "aa"
let config_seed_file = ref "/tmp/bin.json"
let strategy = ref "dfs"
let trace_limit_length = ref 5000
let exploration_timeout = ref (-1.0)
let random_seed = ref false
let score_file  = ref "score"
let score_alloc  = ref "score_alloc"
let score_free  = ref "score_free"
let score_use  = ref "score_use"
let l_event  = ref "l_event"
let dse_trace = ref "/tmp/t.tr"
let dse_type = ref "explore"

(* Verimag testing *)
let nth_to_invert = ref 0
let nth_alloc = ref 0
let nth_free = ref 0
let nth_use = ref 0
let check_init = ref false
(* ----------------- *)


(* DSE Options *)
type trace_analysis_config = {
  mutable configuration: Config_piqi.Configuration.t;
  mutable trace_file: string;
  mutable trace_input: trace_format;
}

let config =  {
  configuration = Config_piqi.default_configuration ();
  trace_file="";
  trace_input=Chunked(Pervasives.stdin,false);
}

let summary_only = ref false
let config_parsed = ref false
let pruning = ref true
let pol_name = ref ""
let natural_cond_enabled = ref false
let flatten_memory = ref false
(* ---------- *)

(* ---- Server options --- *)
let ip_address = ref ""
let port = ref 5570
let backend_port = ref 5580
let nb_workers = ref 2
(* -------------------- *)

(* Test options *)
let experiment = ref false
(* ---------- *)


let get_dba_configuration_filename , set_dba_configuration_file =
  let info_file = ref None in
  (fun () -> !info_file),
  (fun s -> info_file := Some s)


let set_dba_file, get_dba_filename =
  let filename = ref None in
  (fun fname -> filename := Some fname),
  (fun () -> !filename )


let get_entry_point, set_entry_point =
  let ep = ref None in
  (fun () -> !ep),
  (fun entry -> ep := Some (Bigint.big_int_of_string entry))


let set_describe_binary, get_describe_binary =
  let active = ref false in
  (fun b -> active := b),
  (fun () -> !active)


module ShareDirectory = struct
  let _name = "share"
  let doc = " Set share directory"

  let default_dirs =
    let dirs = [Config.sharedir] in
    try Sys.getenv "BINSEC_SHARE" :: dirs
    with Not_found -> dirs

  let search_dirs = ref default_dirs

  let add dirname =
    assert (Sys.is_directory dirname);
    search_dirs := dirname :: !search_dirs

  let pp ppf dirs =
    Format.fprintf ppf "@[<hov 0>";
    List.iter (fun dname -> Format.fprintf ppf "%s;@ " dname) dirs;
    Format.fprintf ppf "@]"

  let find_file filename =
    let dirs = !search_dirs in
    let rec loop = function
      | [] ->
        Logger.error "Could not find %s in %a" filename pp dirs;
        raise Not_found
      | d :: dirs ->
        let fname = Filename.concat d filename in
        if Sys.file_exists fname && not (Sys.is_directory fname) then fname
        else loop dirs
    in loop dirs

end

module ExecFile =
  Parameters.Builder.OptionalString(
      struct
        let name = "" (* unused *)
        let doc = ""  (* unused *)
      end
    )

module Machdep = struct
  let set s =
    match String.lowercase s with
    | "x86" -> Machine.set_x86 ()
    | "arm" -> Machine.set_arm_little ()
    | _ -> assert false


  let arch_options = ["x86"; "arm"]

  let cli_option =
    "-machdep", Arg.Symbol (arch_options, set),
    Format.asprintf " Set machdep [set by loader]"
end

let usage_message =
  Format.sprintf
  "@[<v 0> \
     Usage: binsec COMMAND [ARGUMENTS] file@ \
     @[<v 2> \
     COMMAND:@ \
       disasm:   disassembly operations@ \
       simulate: interpret dba@ \
       analyse:  launch a custom analysis@ \
       trace:    analyse trace (taint analysis)@ \
       test:     test undecoded instructions into a binary@ @]@ \
     ARGUMENTS: (binsec COMMAND -help)@]@."


let ep = "-entrypoint", Arg.String set_entry_point, " Set entry point"

       
let disasm_args =
  ep ::
    [
    ("-cfgraph", Arg.Set cfgraph,
     " Print the control flow graph");
    ("-decode", Arg.Set decode,
     " Decode the opcode given in argument (override other args)");
    ("-decode-llvm", Arg.Set decode_llvm,
     " Decode the opcode given in argument into LLVM (override other args)");
    ("-dmode", Disasm_options.DisassemblyMode.cli_handler,
     " Set disassembly mode");
    Disasm_options.DbaOutputFile.arg;
    Disasm_options.OpcodeOutputFile.arg;
    Disasm_options.ArmDecoder.arg;
    Disasm_options.ShowInstructionCount.arg;
    Disasm_options.Sections.arg;
  ]


let common_args =
  ep ::
    [
  ("-dba", Arg.String set_dba_file,
     " Set DBA file");

  Static_options.Disassembly.arg;
  Static_options.NaiveWidening.arg;
]

let simulation_args =
  common_args @
  [
    Simulate_options.FuzzerIterations.arg;
    Simulate_options.StepByStep.arg;
    Simulate_options.SemanticsMode.arg;
    "-on-conditional", Simulate_options.ConditionalStrategy.cli_handler,
    " Fuzz on else branches, then branches or fails";
  ]


let analysis_args =
  common_args @
  [
    Ai_options.KSetSize.arg;
    Ai_options.FailSoftMode.arg;
    Ai_options.X86FlagPatterns.arg;
    ("-abs-domain", Ai_options.Domain.cli_handler,
       " Define the abstract domain to use");
  ]


let solver_of_string = function
  | "boolector" ->  `boolector
  | "z3" -> `z3
  | "cvc4" -> `cvc4
  | "yices" -> `yices
  | _ -> assert false

let set_solver config solver_name =
  let open Config_piqi.Configuration in
  config.configuration.solver <- solver_of_string (String.lowercase solver_name)


let trace_args =
  let open Config_piqi.Configuration in
  [
    ("-summary",
     Arg.Unit (fun () -> summary_only := true),
     " shows only a trace summary");
    ("-type", Arg.Symbol (
        ["forward";"backward"],
        (fun s ->
           match (String.lowercase s) with
           | "forward" ->  config.configuration.direction <- `forward
           | "backward" -> config.configuration.direction <- `backward
           | _ -> assert false
        )), " Define the kind of trace analysis to perform.");
    ("-ksteps",
     Arg.Int (fun i -> config.configuration.ksteps <- Int32.of_int i),
     " Number of steps to perform before stopping the analyse");
    ("-trace", Arg.String (fun s -> config.trace_file <- s),
     " Indicates the trace file");
    ("-name", Arg.String (fun s -> config.configuration.analysis_name <- s),
     " Analysis name to perform");
    ("-nat-cond", Arg.Unit (fun () -> natural_cond_enabled := true),
     " Enable transformation of condition to natural conditions");
    ("-flat-mem", Arg.Unit (fun () -> flatten_memory := true),
     " Remove all read in memory if all indexes are constant");
    ("-config", Arg.String
       (fun s ->
         if not !config_parsed then begin
           config_parsed := true;
           let raw = File_utils.load s in
           let opts = Piqirun_ext.make_options ~json_omit_missing_fields:true () in
           config.configuration <- Config_piqi_ext.parse_configuration ~opts raw `json
         end)

    , " Configuration file");
    ("-solver",
     Arg.Symbol (["boolector";"z3";"cvc4";"Z3";"CVC4";"yices";"Yices"],
                 set_solver config),
     " Set solver to use");
    ("-callcvt", Arg.Symbol (
        ["cdecl";"stdcall";"fastcall";"thiscall"],
        (fun s ->
           let v =
           match (String.lowercase s) with
           | "cdecl" -> `cdecl
           | "stdcall" -> `stdcall
           | "fastcall" -> `fastcall
           | "thiscall" -> `thiscall
           | _ -> assert false
           in config.configuration.callcvt <- v
        )),
     " Set call convention used in the binary");
    ("-trace-format", Arg.Symbol (
        ["unsplitted";"chunked";"stream"],
        (fun s ->
           match (String.lowercase s) with
           | "chunked" -> config.trace_input <- Chunked(Pervasives.stdin, false)
           | "stream" -> config.trace_input <- Stream("")
           | _ -> assert false
        )),
     " Define the kind of trace analysis to perform.");
    ("-default-action", Arg.Symbol (
        ["symb";"conc";"ignore"],
        (fun s ->
           let v =
             match String.lowercase s with
             | "symb" -> `symb
             | "conc" ->  `conc
             | "ignore"-> `ignore
             | _ -> assert false
           in config.configuration.default_action <- v
        )), " Define de default action for default value in a policy");
    ("-read-all",
     Arg.Unit (fun () -> config.trace_input <- Chunked(Pervasives.stdin,true)),
     " Load full trace in memory at once");
    ("-policy",
     Arg.String
       (fun s ->
          config.configuration.policy <- File_utils.readlines s;
          pol_name:= s),
     " C/S policy file for concretisation/symbolisation");
    ("-incremental",
     Arg.Unit (fun () -> config.configuration.incremental <- true),
     " Activate incremental solving (if available)");
    ("-no-pruning", Arg.Unit (fun () -> pruning  := false),
     " Activate incremental solving (if available)");
    ("-timeout",
     Arg.Int (fun i -> config.configuration.timeout <- Int32.of_int i),
     " Timeout in seconds for solving queries");
    ("-optim-cstprop",
     Arg.Unit (fun () -> config.configuration.optim_cstprop <- true),
     " Enable constant propagation optimization");
    ("-optim-rebase",
     Arg.Unit (fun () -> config.configuration.optim_rebase <- true),
     " Enable rebase optimisation");
    ("-optim-row",
     Arg.Unit (fun () -> config.configuration.optim_row <- true),
     " Enable Read-Over-Write optimisation");
    ("-optim-rowplus",
     Arg.Unit (fun () -> config.configuration.optim_rowplus <- true),
     " Enable Memory ROW optimized");
    ("-optim-eqprop",
     Arg.Unit (fun () -> config.configuration.optim_eqprop <- true),
     " Enable Memory equality propagation optimization");
    ("-p", Arg.Set_int port, " Pinsec port");
    ("-host", Arg.Set_string ip_address, " IP address Pinsec server");
    ("-nth", Arg.Set_int nth_to_invert, "Nth inst to invert");
    ("-nth-alloc", Arg.Set_int nth_alloc, "Nth inst alloc");
    ("-nth-free", Arg.Set_int nth_free, "Nth inst free");
    ("-nth-use", Arg.Set_int nth_use, "Nth inst use");
    ("-check-init", Arg.Set check_init, "Check if input does not depend of init vals");
    ("-X", Arg.Set experiment, " Do not use (for development purposes only");
  ]


let test_args =
  let open Config_piqi.Configuration in
  [
    ("-X", Arg.Set experiment,
     " Do not use (for development purposes only");
    ("-policy",
     Arg.String (fun s ->
         config.configuration.policy <- File_utils.readlines s;
         pol_name:= s),
     "check the given policy file");
  ]

let server_args =
  [
    ("-p", Arg.Set_int port,
     " Indicates the port on which to listen");
    ("-solver", Arg.Symbol (
        ["boolector";"z3";"cvc4";"Z3";"CVC4";"yices";"Yices"],
        set_solver config)
    , " Specifies the solver to use");
    ("-w", Arg.Set_int nb_workers, " Number of workers")
  ]


let dse_args =
  [
    ("-bin",         Arg.Set_string bin_to_explore, " Binary to explore");
    ("-arg",         Arg.Set_string arg_dse,            " Argv");
    ("-config",      Arg.Set_string config_seed_file,
     " Bootstrap json configuration file");
    ("-strat",       Arg.Set_string strategy,
     " Exploration strategy: dfs (default) / bfs / random");

    (* added for code coverage *)
    ("-length",      Arg.Set_int trace_limit_length,
     " Limit length for traces (default 5000)" );
    ("-timeout",     Arg.Set_float exploration_timeout,
     " Timeout for exploration (if not set, no timeout)");

    (* added for UAF detection *)
    ("-random",      Arg.Set random_seed,
     " Use a random seed");
    ("-trace",       Arg.Set_string dse_trace,
     " Trace file");
    ("-type",        Arg.Set_string dse_type,
     " Type (default explore");
  ]


let common_args =
  [
    ("-info", Arg.String set_dba_configuration_file,
     " Set dba configuration file name");
    Disasm_options.NoLoaderMode.arg;
    ("-g", Arg.Set_string debug_file, " Set debugger output file");
    ("-simplify-level", Disasm_options.simplification_cli_handler,
     " Specify simplification level");
    Disasm_options.IgnoreUnhandledInstructions.arg;
    ("-display-statistics",
     Arg.Unit (fun () -> display_statistics := true),
     " Display simplification statistics");
    "-share", Arg.String ShareDirectory.add, ShareDirectory.doc;
    Machdep.cli_option;
  ]


let increase_verbosity () =
  let open Config_piqi.Configuration in
  config.configuration.verbosity <- Int32.succ config.configuration.verbosity


let output_args =
  [
  "-debug-level", Arg.Int Logger.set_debug_level,
  " Enable debug output from level <n> on";
  "-color", Arg.Unit (fun _ -> Logger.(set_color true; set_tagged_entry false)),
  " Enable color tags on outputs (might not work on your terminal)";
  "-loglevel", Logger.cli_handler,
  " Display log messages only above or equal to this level";
  "-quiet", Arg.Unit Logger.quiet,
  " Do not display anything";
  "-v", Arg.Unit increase_verbosity, " Increase verbosity level";
]


let generic_args =
  output_args @
  [ Disasm_options.ProtectedMode.arg;
    "-handle-seg", Arg.String Disasm_options.mark_ignored_segments,
    " Activate (comma-separated) segments";
    "-describe", Arg.Unit (fun () -> set_describe_binary true),
    " display a description of the contents of the binary and exits";
  ] @
    Binpatcher_options.cli


let opts = ref (Arg.align generic_args)


let subcommands =
  [ "analyse", Analyzer, analysis_args;
    "simulate", Simulator, simulation_args;
    "trace", Trace, trace_args;
    "test", Test, test_args;
    "disasm", Disasm, disasm_args;
    "serve", Serve, server_args;
    "dse", DSE, dse_args;
  ]


let subcommand_tbl =
  let h = Hashtbl.create (List.length subcommands) in
  List.iter
    (fun (name, mode_type, specific_args) ->
      Hashtbl.add h name (mode_type, specific_args)
    ) subcommands;
  h


let parse_command string =
  if !Arg.current == 1 then
    let mode_type, specific_options =
      try Hashtbl.find subcommand_tbl string
      with
      | Not_found ->
        Format.fprintf Format.str_formatter
          "@[<v 0>Unknown command : %s@ \
            Choose among: %a@]@.\
            "
          string
          (fun fmt h ->
             Format.fprintf fmt "@[<hov 0>";
             Hashtbl.iter (fun name _ -> Format.fprintf fmt "%s;@ " name) h;
             Format.fprintf fmt "@]"
          ) subcommand_tbl;
        let msg = Format.flush_str_formatter () in
        raise (Arg.Bad msg)
    in
    command := mode_type;
    opts := Arg.align (specific_options @ common_args @ generic_args)
  else if !Arg.current = Array.length Sys.argv - 1 then
    begin
      ExecFile.set string;
      Disasm_options.set_file string;
    end
  else
    let msg = Format.sprintf "Unknown command %s" string in
    raise (Arg.Bad msg)

let parse_command_line () =
  Arg.parse_dynamic opts parse_command usage_message;
  if !config_parsed then begin
    Arg.current := 0;
    config.configuration.Config_piqi.Configuration.verbosity <- 0l;
    (* Reparse *)
    Arg.parse_dynamic opts parse_command usage_message
  end

let check_options () =
  match !command with
  | Analyzer
  | Simulator
  | Disasm  ->
    if not (ExecFile.is_set ()) then begin
      Logger.fatal "No file specified (as last argument)";
      exit 2;
    end
  | Trace
  | Test
  | NoMode
  | DSE
  | Serve  -> ()
