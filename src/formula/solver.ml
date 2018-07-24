(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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

open Common_piqi
open Formula_pp
open Formula_options

module Command =
struct

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
    | GetValue tm ->
      fprintf ppf "@[<hov 2>(get-value@ (%a))@]" pp_term tm

  let put_entry en = PutEntry en
  let check_sat = CheckSat
  let get_model = GetModel
  let get_value v = GetValue v
end

type solver_session = {
  solver: solver_t;
  stdin: out_channel;
  stdout: in_channel;
  stderr: in_channel;
  dump: out_channel option;
  combined: Format.formatter;
  incremental: bool
}

let make_session solver incremental stdin stdout stderr dump_file =
  let dump, outs = match dump_file with
    | "" -> None, [| stdin |]
    | f -> let fd = open_out f in Some fd, [| stdin; fd |]
  in
  let out, flush =
    (fun buf pos len -> Array.iter (fun fd -> Pervasives.output_substring fd buf pos len) outs),
    fun () -> Array.iter Pervasives.flush outs
  in
  let combined = Format.make_formatter out flush in
  { solver; stdin; stdout; stderr; dump; combined; incremental }

let stop_interactive (solver:solver_session): unit =
  ignore (Unix.close_process_full (solver.stdout, solver.stdin, solver.stderr));
  match solver.dump with | Some(d) -> close_out d | None -> ()

let default_session ?(file="") () = make_session (default_solver_t ()) false stdout stdin stdin file

let get_formatter_of_session solver_session = solver_session.combined

let is_boolector = function
  | `boolector -> true
  | _ -> false

let pp_solver ppf solver =
  let name = match solver with
    | `z3 -> "z3"
    | `yices -> "yices"
    | `boolector -> "boolector"
    | `cvc4 -> "CVC4"
  in
  Format.fprintf ppf "%s" name

let append_to_file (file:string) (content:string): unit =
  let fd = open_out_gen [Open_append;Open_wronly;Open_text] 644 file in
  output_string fd content;
  close_out fd

let patch_regexp = Str.regexp "(((let (\\([^(]\\)"
(* boolector sometimes forgets a paren or two *)
let patch_model_for_boolector solver s =
  if solver = `boolector then
    Str.replace_first patch_regexp "(((let ((\\1" s
  else
    s

let read_raw_model ~model solver fd =
  let rec loop npar acc =
    try
      let s = input_line fd |> patch_model_for_boolector solver in
      let lpar = String.length (String_utils.remove_char '(' s) in
      let rpar = String.length (String_utils.remove_char ')' s) in
      let npar = npar + (lpar - rpar) in
      let acc = acc ^ "\n" ^ s in
      let still_to_be_read = match solver, model with
        | `yices, true -> s <> "((#b1 #b1))"
        | _ -> npar <> 0
      in
      if still_to_be_read then
        loop npar acc
      else
        acc
    with
    | End_of_file -> acc
  in
  loop 0 ""


let has_time_limit =
  let pat = Str.regexp ".*time limit.*" in
  fun (s : string) -> Str.string_match pat s 0


let _or default = function
  | Some(x) -> x
  | None -> default

let get_result_and session f =
  try
    let first_line = input_line session.stdout in
    (* Logger.debug "is with model:%b" get_model; *)
    Logger.debug ~level:5 "Value read:%s" first_line;
    match first_line with
    | "sat" -> Formula.SAT, Some(f session)
    | "unsat" ->   Formula.UNSAT, None
    | "timeout" -> Formula.TIMEOUT, None
    | "unknown" -> Formula.UNKNOWN, None
    | _ ->
      if has_time_limit first_line then begin
        ignore (input_line session.stdout);
        Formula.TIMEOUT, None
      end
      else
        failwith ("Unknown status returned :"^first_line)
  with End_of_file ->
    Logger.error "Solver piped closed unexpected (got killed ?)";
    Formula.UNKNOWN, None

let get_return_value_ast parser raw_model =
  (* not for yices *)
  let lexbuf = Lexing.from_string raw_model in
  try
    parser Smtlib_lexer.token lexbuf
  with e ->
    let open Lexing in
    let pos = lexbuf.lex_curr_p.pos_cnum in
    let width = 30 in
    let lo = max 0 (pos - width)
    and hi = min (String.length raw_model) (pos + width) in
    let extract =
      String.sub raw_model lo (hi - lo)
      |> String.map (fun x -> if x = '\n' then ' ' else x)
    in
    let underline = String.make (pos - lo) ' ' in
    Logger.error
      "Parsing of solver output failed approximately here :@ @[<v>%s@,%s^@]"
      extract underline;
    raise e

let parse_model session =
  let raw_model = read_raw_model ~model:true session.solver session.stdout in
  match session.solver with
  | `yices ->
    Smt_model.yices_extract raw_model
  | _ -> get_return_value_ast Smtlib_parser.model raw_model |> Smt_model.extract

(* command (get-model) *)
let read_model session =
  if session.incremental
  then Format.fprintf (get_formatter_of_session session) "(get-model)\n%!";
  parse_model session

(* command (get-value) *)
let read_value session =
  read_raw_model ~model:false session.solver session.stdout
  |> get_return_value_ast Smtlib_parser.value |> Smt_model.extract_value

let solve_model_time ?(timeout=0) ?(get_model=false) file solver =
  let timeout = match solver with `cvc4 -> timeout*1000 | _ -> timeout in
  let do_timeout, tvalue =
    if timeout <> 0 then true, string_of_int timeout else false, "" in
  let cmdline, with_m, tvalue =
    match solver with
    | `boolector -> "boolector -m -x --smt2-model ", false,  " -t "^tvalue
    | `z3 -> "z3 --smt2 ", true, " -T:"^tvalue
    | `cvc4 ->
      (* Time in millisecond for CVC4 *)
      "cvc4 -L smt2 -m ", true, " --tlimit="^tvalue
    | `yices ->
      "yices-smt2 ", true, " --timeout="^tvalue
  in
  let s = Printf.sprintf "%s" ("\n(check-sat)\n"^(if with_m && get_model then "(get-model)\n" else "")) in
  append_to_file file s;
  let fullcmdline = cmdline^(if do_timeout then tvalue else "")^" "^file in
  let fdout, fdin, fderr = Unix.open_process_full fullcmdline (Unix.environment()) in
  let before = Unix.gettimeofday ()
  and session = make_session solver false fdin fdout fderr "" in
  let r, m = get_result_and session read_model in
  Unix.close_process_full (fdout, fdin, fderr) |> ignore;
  r, (_or Smt_model.empty m), (Unix.gettimeofday ()) -. before

let solve_model ?(timeout=0) (file:string) (solver:solver_t) =
  let res, model, _ = solve_model_time ~timeout ~get_model:true file solver in
  res, model

let solve ?(timeout=0) (file:string) (solver:solver_t)  =
  let res, _, _ = solve_model_time ~timeout ~get_model:false file solver in res

let start_interactive ?(file="") ?(timeout=0) (solver:solver_t): solver_session =
  let timeout = match solver with | `z3 | `cvc4 -> timeout*1000 | _ -> timeout in
  let do_timeout, tvalue = if timeout != 0 then true, string_of_int timeout else false, "" in
  let cmdline, tvalue =
    match solver with
    | `boolector -> "boolector -m -x --smt2-model -i", " -t "^tvalue
    | `z3 -> "z3 --smt2 -in", " -t:"^tvalue
    | `cvc4 -> "cvc4 -L smt2 -m -i", " --tlimit-per="^tvalue
    | `yices -> "yices-smt2 --incremental ", " --timeout="^tvalue
  in
  let fullcmdline = cmdline^(if do_timeout then tvalue else "") in
  let fdout, fdin, fderr = Unix.open_process_full fullcmdline (Unix.environment ()) in
  let session = make_session solver true fdin fdout fderr file in
  Logger.debug ~level:4 "Starting session for %a" pp_solver solver;
  begin
    match file with
    | "" -> ()
    | _ -> Logger.debug ~level:4 "Session transcript written to@ %s" file
  end;
  let out = get_formatter_of_session session in
  Format.fprintf out "%s\n\n%!" (print_header ());
  session

let solve_incremental_and ?(term=None) session func =
  let extra_pred =
    match term with
    | None -> ""
    | Some a -> Printf.sprintf "\n(assert %s)" (print_bl_term a) in
  let before = Unix.gettimeofday () in
  Format.fprintf (get_formatter_of_session session) "%s\n(check-sat)\n%!" extra_pred;
  let r,m = get_result_and session func in
  r, m, (Unix.gettimeofday ()) -. before

let solve_incremental_model_time ?(term=None) ?(get_model=false) session =
  let read_result =
    if get_model then read_model else fun _ -> Smt_model.empty
  in
  let res, m, time =
    solve_incremental_and ~term session read_result
  in res, (_or Smt_model.empty m), time

let solve_incremental_model ?(term=None) session =
  let res, m, _ =
    solve_incremental_model_time ~term ~get_model:true session
  in res, m

let solve_incremental
    ?(term=None) (session:solver_session):
  Formula.status =
  let res, _, _ =
    solve_incremental_and ~term session ignore
  in res

let solve_incremental_value ?(term=None) expr session  =
  let v_s = print_bv_term expr in
  let extra_pred =
    match term with
    | None -> ""
    | Some a -> Printf.sprintf "\n(assert %s)" (print_bl_term a) in
  Format.fprintf (get_formatter_of_session session)
    "%s\n(check-sat)\n(get-value (%s))\n%!" extra_pred v_s;
  get_result_and session read_value

let push (session:solver_session): unit =
  Format.fprintf (get_formatter_of_session session) "\n(push 1)\n%!"

let pop (session:solver_session): unit =
  Format.fprintf (get_formatter_of_session session) "\n(pop 1)\n%!"

module Session = struct
  open Formula

  type t = {
    process : solver_session;
    mutable state: status option;
    mutable model: Smt_model.t option;
  }

  type output =
    | Nil
    | Model of Smt_model.t
    | Sat of status
    | Value of Bitvector.t

  let create ?(file="") ?(timeout=0) solver = {
    process = start_interactive ~file ~timeout solver;
    state = None;
    model = None;
  }

  let destroy session = stop_interactive session.process

  let rec write session command =
    Format.fprintf session.process.combined "%a\n%!"
      Command.pp_command command;
    (* yices outputs models without a deterministic way to mark the end of the
     * model. we send (get-value (1)) after (get-model) so that we know the
     * model is complete when we read ((1 1))
     * Awful. I know.
    *)
    if (command, session.process.solver) = (Command.get_model, `yices) then
      write session (Command.get_value (mk_bv_term (mk_bv_one)))

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
    | GetValue _ ->
      let value = read_value session.process in
      Value value

  let put_entry session en =
    match run session (Command.put_entry en) with
    | Nil -> ()
    | _ -> assert false

  let check_sat session =
    match session.state with
    | None ->
      begin
        match run session Command.check_sat with
        | Sat x -> x
        | _ -> assert false
      end
    | Some x -> x

  let get_model session =
    match session.model with
    | None ->
      begin
        match run session Command.get_model with
        | Model m -> m
        | _ -> assert false
      end
    | Some x -> x

  let get_value session value =
    let res = Command.get_value value |> run session in
    match res with
    | Value v -> v
    | _ -> assert false
end
