(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

module Command = struct
  type t = string array

  let pp ppf t =
    Format.pp_print_string ppf t.(0);
    for i = 1 to Array.length t - 1 do
      Format.pp_print_char ppf ' ';
      Format.pp_print_string ppf t.(i)
    done

  let to_string t =
    let b = Buffer.create 128 in
    Array.iter
      (fun sw ->
        Buffer.(
          add_char b ' ';
          add_string b sw))
      t;
    Buffer.contents b
end

type t = Boolector | Bitwuzla | Z3 | CVC4 | Yices

let pp ppf = function
  | Boolector -> Format.pp_print_string ppf "Boolector"
  | Bitwuzla -> Format.pp_print_string ppf "Bitwuzla"
  | Z3 -> Format.pp_print_string ppf "Z3"
  | CVC4 -> Format.pp_print_string ppf "CVC4"
  | Yices -> Format.pp_print_string ppf "Yices"

let is_boolector = function
  | Boolector | Bitwuzla -> true
  | Z3 | CVC4 | Yices -> false

let is_yices = function
  | Yices -> true
  | Z3 | CVC4 | Boolector | Bitwuzla -> false

let executable = function
  | Boolector -> "boolector"
  | Bitwuzla -> "bitwuzla"
  | CVC4 -> "cvc4"
  | Z3 -> "z3"
  | Yices -> "yices-smt2"

let name_of = executable

let rec any ic =
  match input_char ic with exception End_of_file -> None | _ -> any ic

let check_version = function
  | Bitwuzla ->
      fun version ->
        if
          try
            let ic = Scanf.Scanning.from_channel version in
            Scanf.bscanf ic "%d.%d.%d%s" (fun major minor _fix _ ->
                major > 0 || minor >= 2)
          with Scanf.Scan_failure _ | Failure _ | End_of_file -> false
        then None
        else
          Some
            (Format.asprintf
               "Found %a in the path, but the version does not match (expects \
                >=0.2)."
               pp Bitwuzla)
  | _ -> any
(* TODO ? *)

let ping solver =
  let exec = executable solver in
  match Unix.open_process_args_in exec [| exec; "--version" |] with
  | exception Unix.Unix_error (ENOENT, _, _) -> false
  | cout -> (
      try
        let mismatch = check_version solver cout in
        match Unix.close_process_in cout with
        | WEXITED 0 ->
            Option.fold ~none:true
              ~some:(fun msg ->
                Logger.warning "%s" msg;
                false)
              mismatch
        | _ -> false
      with End_of_file | Sys_error _ ->
        ignore (Unix.close_process_in cout);
        false)

let default_arguments = function
  | Boolector -> [ "-m"; "-x" ]
  | Bitwuzla -> [ "-m"; "--bv-output-format"; "16" ]
  | CVC4 -> [ "--lang=smt2"; "--produce-models" ]
  | Z3 -> [ "--smt2" ]
  | Yices -> []

let incremental_a = function
  | Bitwuzla -> []
  | Boolector -> [ "-i" ]
  | CVC4 -> [ "--incremental" ]
  | Z3 -> [ "-in" ]
  | Yices -> [ "--incremental" ]

let arguments ?options prover =
  match options with Some opts -> [ opts ] | None -> default_arguments prover

let timeout_s time = function
  | Boolector | Yices -> time
  | Bitwuzla | Z3 | CVC4 -> 1000 * time

let time_switch time prover =
  assert (time > 0);
  let t = timeout_s time prover |> string_of_int in
  match prover with
  | Boolector | Bitwuzla -> [ "-t"; t ]
  | CVC4 -> [ "--tlimit-per=" ^ t ]
  | Z3 -> [ "-t:" ^ t ]
  | Yices -> [ "--timeout=" ^ t ]

let add_switch_if p new_switches switches =
  if p then switches @ new_switches () else switches

let command ?(incremental = false) ?options timeout prover =
  executable prover :: arguments ?options prover
  |> add_switch_if incremental (fun () -> incremental_a prover)
  |> add_switch_if (timeout > 0) (fun () -> time_switch timeout prover)
  |> Array.of_list
