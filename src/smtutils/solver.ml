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

open Common_piqi
open Smtlib2print

type solver_session = {
  stdin: out_channel;
  stdout: in_channel;
  stderr: in_channel;
}

let default_session () = {stdin=stdout; stdout=stdin; stderr=stdin}

let is_boolector = function
  | `boolector -> true
  | _ -> false


let append_to_file (file:string) (content:string): unit =
  let fd = open_out_gen [Open_append;Open_wronly;Open_text] 644 file in
  output_string fd content;
  close_out fd

let has_memory =
  let pat = Str.regexp "^(function.*" in
  fun (s : string) -> Str.string_match pat s 0

let rec read_raw_model (solver:solver_t) ?(_mem_found=false) (fd:in_channel) (cur:int) (acc:string): string =
  try
    let s = input_line fd in
    (* Logger.result "Raw line:%s" s; *)
    let mem_found = has_memory s in
    let lpar = String.length (String_utils.remove_char '(' s) in
    let rpar = String.length (String_utils.remove_char ')' s) in
    let num = cur + (lpar - rpar) in
    let newacc = acc ^ "\n" ^ s in
    if num <> 0 then
      read_raw_model solver ~_mem_found fd num newacc
    else
      match solver with
      | `yices when not mem_found ->
        (* Logger.result "recurse"; *)
        read_raw_model solver ~_mem_found fd num newacc
      | _ -> newacc
  with
  | End_of_file -> acc


let has_time_limit =
  let pat = Str.regexp ".*time limit.*" in
  fun (s : string) -> Str.string_match pat s 0


let get_result ?(inc=false) ?(get_model=false) solver session =
  try
    let first_line = input_line session.stdout in
    (* Logger.debug "is with model:%b" get_model; *)
    Logger.debug ~level:1 "Value read:%s" first_line;
    match first_line with
    | "sat" ->
      if get_model then
        begin
          if inc then Printf.fprintf session.stdin "(get-model)\n%!";
          let raw_model = read_raw_model solver session.stdout 0 "" in
          match solver with
          | `yices ->
            (* Logger.result "Raw model:\n%s" raw_model; *)
            Smtlib2.SAT, Smt_model.yices_extract raw_model
          | _ ->
            let lexbuf = Lexing.from_string raw_model in
            let smt_model = Smtlib_parser.model Smtlib_lexer.token lexbuf in
            begin
              match smt_model.Smtlib_ast.model_commands with
              | [] -> Logger.warning "Empty SMT model"; Smtlib2.SAT, Smt_model.empty
              | _ :: _->
                let model = Smt_model.extract smt_model in
                Smtlib2.SAT, model
            end
        end
      else Smtlib2.SAT, Smt_model.empty
    | "unsat" ->   Smtlib2.UNSAT,   Smt_model.empty
    | "timeout" -> Smtlib2.TIMEOUT, Smt_model.empty
    | "unknown" -> Smtlib2.UNKNOWN, Smt_model.empty
    | _ ->
      if has_time_limit first_line then begin
        ignore (input_line session.stdout);
        Smtlib2.TIMEOUT, Smt_model.empty
      end
      else
        failwith ("Unknown status returned :"^first_line)
  with End_of_file ->
    Logger.error "Solver piped closed unexpected (got killed ?)";
    Smtlib2.UNKNOWN, Smt_model.empty

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
    | `yices -> if timeout <> 0 then
        Logger.warning "Timeout not supported for Yices..";
      "yices-smt2 ", true, ""
  in
  let s = Printf.sprintf "%s" ("\n(check-sat)\n"^(if with_m && get_model then "(get-model)\n" else "")) in
  append_to_file file s;
  let fullcmdline = cmdline^(if do_timeout then tvalue else "")^" "^file in
  let fdout, fdin, fderr = Unix.open_process_full fullcmdline (Unix.environment()) in
  let before = Unix.gettimeofday () in
  let r, m = get_result ~get_model solver {stdin=fdin; stdout=fdout; stderr=fderr} in
  Unix.close_process_full (fdout, fdin, fderr) |> ignore;
  r, m, (Unix.gettimeofday ()) -. before

let solve_model ?(timeout=0) (file:string) (solver:solver_t) =
  let res, model, _ = solve_model_time ~timeout ~get_model:true file solver in
  res,model

let solve ?(timeout=0) (file:string) (solver:solver_t)  =
  let res, _, _ = solve_model_time ~timeout ~get_model:false file solver in res

let start_interactive ?(file="dump.smt2") ?(timeout=0) (solver:solver_t): solver_session =
  let timeout = match solver with | `z3 | `cvc4 -> timeout*1000 | _ -> timeout in
  let do_timeout, tvalue = if timeout != 0 then true, string_of_int timeout else false, "" in
  let cmdline, is_boolector, is_yices, tvalue =
    match solver with
    | `boolector -> "boolector -m -x --smt2-model -i", true, false, " -t "^tvalue
    | `z3 -> "z3 --smt2 -in", false, false, " -t:"^tvalue
    | `cvc4 -> "cvc4 -L smt2 -m -i", false, false, " --tlimit-per="^tvalue
    | `yices -> "yices-smt2 --incremental ", false, true, ""
  in
  let fullcmdline = cmdline^(if do_timeout then tvalue else "") in
  let fdout, fdin, fderr = Unix.open_process_full fullcmdline [||] in
  if is_yices && timeout <> 0 then Logger.warning  "Timeout not supported for Yices..";
  if is_yices then Logger.warning "Model parsing for Yices in incremental mode might hung eternally \
                                   if no memory present in the model";
  Printf.fprintf fdin "%s\n\n%s\n\n%!" (smt_header ()) (if is_boolector then "" else smt_functions ());
  let dump_fd = open_out file in
  Printf.fprintf dump_fd "%s\n\n%s\n\n%!" (smt_header ()) (if is_boolector then "" else smt_functions ());
  close_out dump_fd;
  {stdin=fdin; stdout=fdout; stderr=fderr}

let stop_interactive (solver:solver_session): unit =
  ignore (Unix.close_process_full (solver.stdout, solver.stdin, solver.stderr))

let solve_incremental_model_time
    ?(f=None) ?(file="dump.smt2") ?(get_model=false) (session:solver_session)
    (solver:solver_t)
  : Smtlib2.smt_result * Smt_model.t * float =
  let inline = is_boolector solver in
  let extra_pred =
    match f with
    | None -> ""
    | Some a -> Printf.sprintf "\n(assert %s)" (smtexpr_to_string ~inline a) in
  append_to_file file (Printf.sprintf "%s\n(check-sat)\n(get-model)\n%!" extra_pred);
  let before = Unix.gettimeofday () in
  Printf.fprintf session.stdin "%s\n(check-sat)\n%!" extra_pred;
  let r,m = get_result ~inc:true ~get_model solver session in
  r, m, (Unix.gettimeofday ()) -. before

let solve_incremental_model
    ?(f=None) ?(file="dump.smt2") (session:solver_session) (solver:solver_t):
  Smtlib2.smt_result * Smt_model.t=
  let res, m, _ =
    solve_incremental_model_time ~f ~file ~get_model:true session solver
  in res, m

let solve_incremental
    ?(f=None) ?(file="dump.smt2") (session:solver_session) (solver:solver_t):
  Smtlib2.smt_result =
  let res, _, _ =
    solve_incremental_model_time ~f ~file ~get_model:false session solver
  in res

let solve_incremental_value
    ?(f=None) (session:solver_session) solver ?(file="dump.smt2") value:
  Smtlib2.smt_result * Smt_model.t =
  let inline = is_boolector solver in
  let extra_pred =
    match f with None -> "" | Some a -> Printf.sprintf "\n(assert %s)" (smtexpr_to_string ~inline a) in
  let v_s = smtexpr_to_string ~inline value in
  Printf.fprintf session.stdin "%s\n(check-sat)\n(get-value (%s))\n%!" extra_pred v_s;
  append_to_file file (Printf.sprintf "%s\n(check-sat)\n(get-value (%s))\n%!" extra_pred v_s);
  get_result solver session

let push ?(file="dump.smt2") (session:solver_session): unit =
  Printf.fprintf session.stdin "\n(push 1)\n%!";
  append_to_file file (Printf.sprintf "\n(push 1)\n%!")

let pop ?(file="dump.smt2") (session:solver_session): unit =
  Printf.fprintf session.stdin "\n(pop 1)\n%!";
  append_to_file file (Printf.sprintf "\n(pop 1)\n%!")
