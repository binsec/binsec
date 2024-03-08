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

open Mcount_options

(* Extend the list below with the desired prefixes *)
let prefixes = [ "rep"; "repz"; "lock" ]
let is_prefix w = List.mem w prefixes

(* Heuristic function in charge of "guessing" the basename of the mnemonic we
 * are dealing with *)
let mnemo_basename s =
  let words = String.split_on_char ' ' s in
  let rec loop = function
    | [] -> "memo_basename_failure" (* This should not happen *)
    | w :: ws -> if is_prefix w then w ^ " " ^ loop ws else w
  in
  loop words

let instruction_class v =
  match Ida_cfg.C.V.inst v with
  | Some inst ->
      let prefix =
        let open Mnemonic in
        match Instruction.mnemonic inst with
        | Unknown -> "unknown"
        | Unsupported None -> "unsupported/unidentified"
        (* Whether the instruction has DBA semantics (Supported)
         * or not (Unsupported) does not matter for our purpose here as long as
         * we have the mnemonic as string (Unsupported (Some s)). *)
        | Unsupported (Some s) | Supported s -> mnemo_basename s
      in
      Some prefix
  | None ->
      Logger.debug "Unclassified instruction %@ %a" Virtual_address.pp
        (Ida_cfg.C.V.addr v);
      None

let display_mnemonic_classes ppf h =
  let ordered_list =
    Basic_types.String.Htbl.bindings h
    (* Comparison function is reversed to get the biggest items first *)
    |> List.sort (fun (_, v1) (_, v2) -> compare v2 v1)
    |>
    let lim = Limit.get () in
    List_utils.take_while (fun i _ -> i < lim)
  in
  List.iter
    (fun (iclass, nocc) -> Format.fprintf ppf "%s : %d;@," iclass nocc)
    ordered_list

let analyze_mnemonics cfg =
  Logger.info "Analyze CFG mnemonics";
  let g = Ida_cfg.G.graph cfg in
  (* Not all vertices embed a single new prefix, at most there are [nb_vertex] of them  *)
  let h = Basic_types.String.Htbl.create (Ida_cfg.C.nb_vertex g / 2) in
  Ida_cfg.C.iter_vertex
    (fun v ->
      let iclass =
        match instruction_class v with Some cl -> cl | None -> "unclassified"
      in
      let nocc =
        match Basic_types.String.Htbl.find h iclass with
        | n -> n
        | exception Not_found -> 0
      in
      Basic_types.String.Htbl.replace h iclass (nocc + 1))
    g;
  Logger.result "@[<v>%d most frequent mnemonics@,%a@]" (Limit.get ())
    display_mnemonic_classes h

(* Add for each vertex the semantics of the instruction *)
let _fill_semantics _ = assert false

let get_ida_cfg () =
  Logger.info "Getting IDA control-flow graph...";
  let ida_file = Ida_options.IdaOutputFile.get () in
  Ida.parse_cfg ~simple:false ~ida_file

let run () =
  if Mcount_options.is_enabled () then (
    Logger.info "Starting example plugin";
    let cfg = get_ida_cfg () in
    analyze_mnemonics cfg)

(*
 * (* Code for hello world example *)
 * let run () =
 *   if No1_options.is_enabled () then begin
 *       Logger.info "Hello World!";
 *     end
 * ;; *)

let _ = Cli.Boot.enlist ~f:run ~name:"Example plugin"
