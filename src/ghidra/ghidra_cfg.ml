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

open Ghidra_options

type kind =
  | Fallthrough
  | Branch
  | Call
  | Return of Virtual_address.t
  | Presumed

(* Main module : the graph representing the CFG *)
module G =
  Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
    (struct
      type t = Virtual_address.t

      let compare = Virtual_address.compare
      let equal = Virtual_address.equal
      let hash = Hashtbl.hash
    end)
    (struct
      type t = kind

      let default = Fallthrough
      let compare = compare
    end)

include G

let link_return =
  let rec walk cfg terminators call succ visited = function
    | [] -> ()
    | addr :: todo ->
        let visited = Virtual_address.Set.add addr visited in
        walk cfg terminators call succ visited
          (fold_succ_e
             (fun (_, kind, addr) todo ->
               if kind == Call || Virtual_address.Set.mem addr visited then todo
               else if Virtual_address.Set.mem addr terminators then (
                 add_edge_e cfg (E.create addr (Return call) succ);
                 todo)
               else addr :: todo)
             cfg addr todo)
  in
  fun cfg terminators succ addr ->
    iter_succ_e
      (function
        | _, Call, dest ->
            walk cfg terminators addr succ Virtual_address.Set.empty [ dest ]
        | _ -> ())
      cfg addr

let parse_cache ~path =
  let ic = open_in path in
  let cfg = create () in
  let mnc = Virtual_address.Htbl.create 0 in
  let lexbuf = Lexing.from_channel ic in
  try
    let calls, terminators =
      List.fold_left
        (fun (calls, terminators) (addr, size, mnemonic, kind, succs) ->
          Virtual_address.Htbl.add mnc addr mnemonic;
          match (kind, succs) with
          | "CALL_TERMINATOR", [ dest ] ->
              (* CALL_TERMINATOR is used when a function tail calls another one,
                 so it is in fact an UNCONDITIONAL_JUMP *)
              add_edge_e cfg (E.create addr Branch dest);
              (calls, terminators)
          | "COMPUTED_CALL", _ ->
              let succ = Virtual_address.add_int size addr in
              add_edge_e cfg (E.create addr Presumed succ);
              List.iter
                (fun succ -> add_edge_e cfg (E.create addr Call succ))
                succs;
              ((addr, succ) :: calls, terminators)
          | "COMPUTED_CALL_TERMINATOR", _ ->
              (* COMPUTED_CALL_TERMINATOR is used when a function tail calls
                  another one, so it is in fact a COMPUTED_JUMP *)
              List.iter
                (fun succ -> add_edge_e cfg (E.create addr Branch succ))
                succs;
              (calls, terminators)
          | "COMPUTED_JUMP", _ ->
              List.iter
                (fun succ -> add_edge_e cfg (E.create addr Branch succ))
                succs;
              (calls, terminators)
          | "CONDITIONAL_JUMP", [ dest ] ->
              add_edge_e cfg
                (E.create addr Fallthrough (Virtual_address.add_int size addr));
              add_edge_e cfg (E.create addr Branch dest);
              (calls, terminators)
          | "FALL_THROUGH", [] ->
              add_edge_e cfg
                (E.create addr Fallthrough (Virtual_address.add_int size addr));
              (calls, terminators)
          | "TERMINATOR", [] -> (calls, Virtual_address.Set.add addr terminators)
          | "UNCONDITIONAL_CALL", [ dest ] ->
              let succ = Virtual_address.add_int size addr in
              add_edge_e cfg (E.create addr Presumed succ);
              add_edge_e cfg (E.create addr Call dest);
              ((addr, succ) :: calls, terminators)
          | "UNCONDITIONAL_JUMP", [ dest ] ->
              add_edge_e cfg (E.create addr Branch dest);
              (calls, terminators)
          (* Skip jumps to external addresses *)
          | "UNCONDITIONAL_JUMP", [] -> (calls, terminators)
          | _ -> Errors.not_yet_implemented kind)
        ([], Virtual_address.Set.empty)
        (Parser_ghidra.instructions Lexer_ghidra.token lexbuf)
    in
    close_in ic;
    List.iter (fun (addr, succ) -> link_return cfg terminators succ addr) calls;
    (cfg, mnc)
  with Parser_ghidra.Error ->
    close_in ic;
    let p = Lexing.lexeme_start_p lexbuf in
    Logger.fatal "lexeme was %s at line %d" (Lexing.lexeme lexbuf)
      p.Lexing.pos_lnum

let run_ghidra ?(temp_dir = "/dev/shm") ?cache ~runner exe =
  let id = Filename.temp_file ~temp_dir "" "" in
  let base = Filename.basename id in
  let dir = base ^ ".d" and len = String.length base in
  let include_path =
    List.find
      (fun path -> Sys.file_exists (path ^ "/ghidra_export.java"))
      Runtime.Sites.utils
  in
  let command =
    Format.sprintf
      "%s %s %s -import %s -scriptPath %s -postScript ghidra_export.java \
       -scriptlog %s -deleteProject"
      runner temp_dir dir exe include_path id
  in
  Logger.debug "%s" command;
  if Sys.command command <> 0 then Logger.fatal "can not run ghidra";
  let readdir = Sys.readdir temp_dir in
  let log = match cache with None -> id ^ ".log" | Some p -> p in
  let oc = open_out_bin log in
  Basic_types.String.Set.iter (fun f ->
      let ic = open_in (Printf.sprintf "%s/%s" temp_dir f) in
      let n = in_channel_length ic in
      output_string oc @@ really_input_string ic n;
      close_in ic)
  @@ Array.fold_left
       (fun s f ->
         if String.length f > len && String.(equal base (sub f 0 len)) then
           Basic_types.String.Set.add f s
         else s)
       Basic_types.String.Set.empty readdir;
  let ic = open_in id in
  let n = in_channel_length ic in
  output_string oc @@ really_input_string ic n;
  close_in ic;
  close_out oc;
  let r = parse_cache ~path:log in
  let command = Format.sprintf "rm %s*" id in
  if Sys.command command <> 0 then Logger.fatal "can not remove tempory files";
  r

module L = Graph.Leaderlist.Make (G)

let is_conditional_jump g v =
  match G.succ_e g v with
  | [ (_, Branch, _); (_, Fallthrough, _) ]
  | [ (_, Fallthrough, _); (_, Branch, _) ] ->
      true
  | _ -> false

let pretty_dot ppf (g, m) =
  let leaders =
    L.leader_lists g (Loader_utils.entry_point (Kernel_functions.get_img ()))
  in
  let labels = Virtual_address.Htbl.create (nb_vertex g) in
  let g' = G.create ~size:(nb_vertex g) () in
  List.iter
    (fun bb ->
      let v = List.hd bb in
      Virtual_address.Htbl.add labels v
        (Format.asprintf "%a"
           (Format.pp_print_list
              ~pp_sep:(fun _ () -> ())
              (fun ppf v ->
                Format.fprintf ppf "%a: %s\\l" Virtual_address.pp v
                  (try Virtual_address.Htbl.find m v
                   with Not_found -> "extern")))
           bb);
      let e = List.fold_left (fun _ e -> e) v bb in
      G.iter_succ_e (fun (_, k, d) -> G.add_edge_e g' (v, k, d)) g e)
    leaders;
  let module D = Graph.Graphviz.Dot (struct
    include G

    let graph_attributes _ = [ `Orientation `Portrait; `Fontname "Courier" ]
    let default_vertex_attributes _ = [ `Shape `Box ]
    let vertex_name v = Format.asprintf "\"%a\"" Virtual_address.pp v
    let vertex_attributes v = [ `Label (Virtual_address.Htbl.find labels v) ]
    let get_subgraph _ = None
    let default_edge_attributes _ = []

    let edge_attributes = function
      | s, Fallthrough, _ when is_conditional_jump g' s ->
          [
            `Headport `N;
            `Tailport `S;
            `Taillabel "f";
            `Fontcolor 0xff5733;
            `Color 0xff5733;
          ]
      | _, Fallthrough, _ -> [ `Headport `N; `Tailport `S ]
      | s, Branch, d when s = d && is_conditional_jump g' s ->
          [
            `Constraint false;
            `Dir `Back;
            `Headlabel "t";
            `Fontcolor 0x25ac1e;
            `Color 0x25ac1e;
          ]
      | s, Branch, d when s = d -> [ `Constraint false; `Dir `Back ]
      | s, Branch, _ when is_conditional_jump g' s ->
          [
            `Constraint false;
            `Taillabel "t";
            `Fontcolor 0x25ac1e;
            `Color 0x25ac1e;
          ]
      | _, Branch, _ -> [ `Constraint false ]
      | _, Call, _ -> [ `Color 0x1e3aac ]
      | _, Return _, _ -> [ `Style `Invis; `Constraint false ]
      | _, Presumed, _ -> [ `Style `Dotted; `Headport `N; `Tailport `S ]
  end) in
  D.fprint_graph ppf g'

let import () =
  match (Cache.get_opt (), Runner.get_opt ()) with
  | Some path, None -> parse_cache ~path
  | cache, Some runner ->
      run_ghidra ?cache ~runner @@ Kernel_options.ExecFile.get ()
  | _, _ -> Logger.fatal "missing either cache or analyzeHeadless script"

let () =
  Cli.Boot.enlist ~name:"Ghidra" ~f:(fun () ->
      if is_enabled () then Logger.result "%a" pretty_dot (import ()))
