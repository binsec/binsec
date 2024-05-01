(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
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

open Formula_pp
open Formula_options

module Command = struct
  type command =
    | PutEntry of Formula.entry
    | CheckSat
    | GetModel
    | GetValue of Formula.term

  let pp_command ppf command =
    let open Format in
    match command with
    | PutEntry en -> fprintf ppf "@[%a@]" pp_entry en
    | CheckSat -> fprintf ppf "(check-sat)"
    | GetModel -> fprintf ppf "(get-model)"
    | GetValue tm -> fprintf ppf "@[<hov 2>(get-value@ (%a))@]" pp_term tm

  let put_entry en = PutEntry en
  let check_sat = CheckSat
  let get_model = GetModel
  let get_value v = GetValue v
end

type solver_session = {
  id : int;
  solver : Prover.t;
  pid : Subprocess.t;
  stdin : out_channel;
  stdout : in_channel;
  stderr : in_channel;
  dump : out_channel option;
  combined : Format.formatter;
  incremental : bool;
}

let new_id =
  let n = ref 0 in
  fun () ->
    incr n;
    !n

let make_session ?file ?(incremental = false) solver pid =
  let stdin = Subprocess.stdin pid
  and stdout = Subprocess.stdout pid
  and stderr = Subprocess.stderr pid in
  let dump, outs =
    match file with
    | None -> (None, [| stdin |])
    | Some f ->
        assert (f <> "");
        Logger.debug "Will open %s" f;
        let fd = open_out f in
        (Some fd, [| stdin; fd |])
  in
  let out, flush =
    ( (fun buf pos len ->
        Array.iter (fun fd -> output_substring fd buf pos len) outs),
      fun () -> Array.iter flush outs )
  in
  let combined = Format.make_formatter out flush in
  {
    id = new_id ();
    solver;
    pid;
    stdin;
    stdout;
    stderr;
    dump;
    combined;
    incremental;
  }

let stop_interactive session =
  ignore @@ Subprocess.close session.pid;
  match session.dump with Some d -> close_out d | None -> ()

let get_formatter_of_session session = session.combined

let append_to_file ~file content =
  let fd = open_out_gen [ Open_append; Open_wronly; Open_text ] 644 file in
  output_string fd content;

  close_out fd

let patch_regexp = Str.regexp "(((let (\\([^(]\\)"

(* boolector sometimes forgets a paren or two *)
let patch_model_for_boolector solver s =
  match solver with
  | Boolector -> Str.replace_first patch_regexp "(((let ((\\1" s
  | _ -> s

let read_raw_model ~model solver fd =
  let b = Buffer.create 256 in
  let rec loop npar =
    let s = input_line fd |> patch_model_for_boolector solver in
    Buffer.add_char b '\n';
    Buffer.add_string b s;
    let lpar = String.length (String_utils.remove_char '(' s) in
    let rpar = String.length (String_utils.remove_char ')' s) in
    let npar = npar + (lpar - rpar) in
    let still_to_be_read =
      match (solver, model) with
      | Yices, true -> s <> "(((_ bv1 1) #b1))"
      | _ -> npar <> 0
    in
    if still_to_be_read then loop npar else Buffer.contents b
  in
  try loop 0 with End_of_file -> Buffer.contents b

let flush session = Format.pp_print_flush (get_formatter_of_session session) ()

let has_time_limit =
  let pat = Str.regexp ".*time limit.*" in
  fun (s : string) -> Str.string_match pat s 0

let get_result_and session f =
  flush session;
  try
    let first_line = input_line session.stdout in
    Logger.debug ~level:5 "Value read:%s" first_line;
    match first_line with
    | "sat" -> (Formula.SAT, Some (f session))
    | "unsat" -> (Formula.UNSAT, None)
    | "timeout" -> (Formula.TIMEOUT, None)
    | "unknown" -> (Formula.UNKNOWN, None)
    | s ->
        Logger.debug "Got status %s" s;
        if has_time_limit first_line then (
          ignore (input_line session.stdout);
          (Formula.TIMEOUT, None))
        else failwith ("Unknown status returned :" ^ first_line)
  with End_of_file ->
    Logger.error "Solver pipe closed unexpectedly (was the solver killed ?)";
    (Formula.UNKNOWN, None)

let get_return_value_ast parser raw_model =
  (* not for yices *)
  let lexbuf = Lexing.from_string raw_model in
  try parser Smtlib_lexer.token lexbuf
  with e ->
    let open Lexing in
    let pos = lexbuf.lex_curr_p.pos_cnum in
    let width = 30 in
    let lo = max 0 (pos - width)
    and hi = min (String.length raw_model) (pos + width) in
    let extract =
      String.sub raw_model lo (hi - lo)
      |> String.map (function '\n' -> ' ' | x -> x)
    in
    let underline = String.make (pos - lo) ' ' in
    Logger.error
      "Parsing of solver output failed approximately here :@ @[<v>%s@,%s^@]"
      extract underline;
    raise e

let extract_model_from_string s = function
  | Yices -> Smt_model.yices_extract s
  | _ ->
      let m = get_return_value_ast Smtlib_parser.model s in
      Smt_model.extract m

let parse_model session =
  Logger.debug "Parsing model";
  flush session;
  let solver = session.solver in
  let raw_model = read_raw_model ~model:true solver session.stdout in
  try extract_model_from_string raw_model solver
  with e ->
    Logger.error "@[<v 0>Could not parse %a model %s@]" Prover.pp solver
      raw_model;
    raise e

(* command (get-model) *)
let read_model session =
  Logger.debug "Read model";
  if session.incremental then (
    let open Format in
    let ppf = get_formatter_of_session session in
    fprintf ppf "(get-model)";
    pp_print_flush ppf ());
  parse_model session

(* Getting the value from the model *)
let read_value session =
  read_raw_model ~model:false session.solver session.stdout
  |> get_return_value_ast Smtlib_parser.value

let model_opt = function None -> Smt_model.empty | Some m -> m

let solve_model_time ?(timeout = 0) ?(get_model = false) ~file solver =
  let b = Buffer.create 24 in
  Buffer.add_string b "(check-sat)";
  if get_model then Buffer.add_string b "(get-model)";
  append_to_file ~file (Buffer.contents b);
  let args = Array.concat [ Prover.command timeout solver; [| file |] ] in
  let fullcmdline = Prover.Command.to_string args in
  Logger.debug "Solving %s" fullcmdline;
  let pid = Subprocess.spawn ~pdeathsig:Sys.sigkill args in
  let before = Unix.gettimeofday () and session = make_session solver pid in
  let r, m = get_result_and session read_model in
  Subprocess.close pid |> ignore;
  (r, model_opt m, Unix.gettimeofday () -. before)

let solve_model ?(timeout = 0) file solver =
  let res, model, _ = solve_model_time ~timeout ~get_model:true ~file solver in
  (res, model)

let solve ?(timeout = 0) file solver =
  let res, _, _ = solve_model_time ~timeout ~file solver in
  res

let start_interactive ?file ?(timeout = 0) solver =
  let incremental = true in
  let fullcmdline = Prover.command ~incremental timeout solver in
  let pid = Subprocess.spawn ~pdeathsig:Sys.sigkill fullcmdline in
  let session = make_session solver ~incremental pid ?file in
  Logger.debug ~level:4 "@[<hov>Starting session %d@ for %s@ with command %s@]"
    session.id (Prover.name_of solver)
    (Prover.Command.to_string fullcmdline);
  if file <> None then
    Logger.debug ~level:4 "@[<hov>Session transcript written to file@ %s@]"
      (Utils.unsafe_get_opt file);
  let ppf = get_formatter_of_session session in
  pp_header ppf ();
  Format.pp_print_flush ppf ();
  session

let pp_assert_check_sat ?term session =
  let open Format in
  let ppf = get_formatter_of_session session in
  if term <> None then
    fprintf ppf "(assert %a)" pp_bl_term (Utils.unsafe_get_opt term);
  pp_print_string ppf "(check_sat)@."

let solve_incremental_and ?term session func =
  let before = Unix.gettimeofday () in
  pp_assert_check_sat ?term session;
  let r, m = get_result_and session func in
  (r, m, Unix.gettimeofday () -. before)

let solve_incremental_model_time ?term ?(get_model = false) session =
  let read_result =
    if get_model then read_model else fun _ -> Smt_model.empty
  in
  let res, m, time = solve_incremental_and ?term session read_result in
  (res, model_opt m, time)

let solve_incremental_model ?term session =
  let res, m, _ = solve_incremental_model_time ?term ~get_model:true session in
  (res, m)

let solve_incremental ?term (session : solver_session) : Formula.status =
  let res, _, _ = solve_incremental_and ?term session ignore in
  res

let solve_incremental_value ?term expr session =
  let ppf = get_formatter_of_session session in
  pp_assert_check_sat ?term session;
  Format.fprintf ppf "(get-value %a)@." pp_bv_term expr;
  get_result_and session (fun session -> read_value session |> List.hd |> snd)

let push session = Format.fprintf (get_formatter_of_session session) "(push 1)"
let pop session = Format.fprintf (get_formatter_of_session session) "(pop 1)"

module Session = struct
  open Formula

  type t = {
    process : solver_session;
    mutable state : status option;
    mutable model : Smt_model.t option;
  }

  type output =
    | Nil
    | Model of Smt_model.t
    | Sat of status
    | Values of (Smtlib.term * Smtlib.term) list

  let create ?file ?(timeout = 0) solver =
    {
      process = start_interactive ?file ~timeout solver;
      state = None;
      model = None;
    }

  let pp ppf session =
    Format.fprintf ppf "Session %d with %s" session.process.id
      (Prover.name_of session.process.solver)

  let destroy session = stop_interactive session.process

  let rec write session command =
    Format.fprintf session.process.combined "@[<h>%a@]@." Command.pp_command
      command;
    (* yices outputs models without a deterministic way to mark the end of the
     * model. we send (get-value (1)) after (get-model) so that we know the
     * model is complete when we read ((1 1))
     * Awful. I know.
     *)
    if command = Command.get_model && session.process.solver = Yices then
      write session (Command.get_value (mk_bv_term mk_bv_one))

  let run session command =
    let open Command in
    write session command;
    match command with
    | PutEntry _ ->
        session.state <- None;
        session.model <- None;
        Nil
    | CheckSat ->
        let res, _ = get_result_and session.process ignore in
        session.state <- Some res;
        session.model <- None;
        Sat res
    | GetModel ->
        let model = parse_model session.process in
        session.model <- Some model;
        Model model
    | GetValue _ -> Values (read_value session.process)

  let put_entry session en =
    match run session (Command.put_entry en) with
    | Nil -> ()
    | _ -> assert false

  let check_sat session =
    match session.state with
    | None -> (
        match run session Command.check_sat with
        | Sat x -> x
        | _ -> assert false)
    | Some x -> x

  let get_model session =
    match session.model with
    | None -> (
        match run session Command.get_model with
        | Model m -> m
        | _ -> assert false)
    | Some x -> x

  let get_value session value =
    let res = Command.get_value value |> run session in
    match res with Values vs -> vs | _ -> assert false
end
