(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2021                                               *)
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

include Cli.Make (struct
  let shortname = "ghidra"

  let name = "Ghidra CFG import"
end)

module Runner = Builder.String_option (struct
  let name = "analyzeHeadless"

  let doc = "Path to the analyzeHeadless script"
end)

module Cache = Builder.String_option (struct
  let name = "cache"

  let doc = "Path to the file to load or save"
end)

module ToProcess = Builder.Integer_set (struct
  let name = "to-process"

  let doc = "Call addresses to process within the CFG"
end)

module StartingAddr = Builder.Integer (struct
  let name = "starting-addr"

  let default = 0

  let doc = "Address from where starts jumps lookup"
end)

module MaxInstr = Builder.Integer (struct
  let name = "max-instr"

  let default = 0

  let doc = "Number max of instruction to process while jumps lookup"
end)

module Vtbl = Hashtbl.Make (struct
  type t = Virtual_address.t

  let hash = Hashtbl.hash

  let equal = Virtual_address.equal
end)

module AddrSet = Set.Make (struct
  type t = Virtual_address.t

  let compare = Virtual_address.compare
end)

(* Main module : the graph representing the CFG *)
module G = Graph.Imperative.Digraph.ConcreteBidirectional (struct
  type t = Virtual_address.t

  let compare = Virtual_address.compare

  let equal = Virtual_address.equal

  let hash = Hashtbl.hash
end)

include G

type node = { vertex : G.V.t; mutable k : int }

type d = {
  cfg : G.t;
  ktable : string Vtbl.t;
  edges_to_remove : (G.V.t * G.V.t) Stack.t;
  edges_to_add : (G.V.t * G.V.t) Stack.t;
  next_table : G.V.t Vtbl.t;
  types_table : string Vtbl.t;
  to_process : ToProcess.t;
}

let rem_edge addr1 addr2 graph =
  let v1 = G.V.create addr1 in
  let v2 = G.V.create addr2 in

  G.remove_edge graph v1 v2

(* Adds v to set, returns it, and says if it was in or not *)
let add_to_set v set =
  let new_or_not = AddrSet.mem v set in
  let new_set = AddrSet.add v set in
  (new_or_not, new_set)

let add_edges stack g =
  let add_edges_aux edge = match edge with x, y -> G.add_edge g x y in
  Stack.iter add_edges_aux stack

let remove_edges stack g =
  let remove_edges_aux edge = match edge with x, y -> G.remove_edge g x y in
  Stack.iter remove_edges_aux stack

(* Gets the fall_through of a function call
 * Uses heuristic distance *)
let rec get_fall_through l call cfg dist best =
  match l with
  | [] -> best
  | v :: q -> (
      let new_dist =
        abs (Virtual_address.to_int call - Virtual_address.to_int v)
      in
      match new_dist < dist with
      | true -> get_fall_through q call cfg new_dist (Some v)
      | false -> get_fall_through q call cfg dist best)

(* Adds edges between certain vertex
 * like the return (TERMINATOR)
 * and the fall_through.
 * If an issue is encountered
 * the edge between Call <-> Fall_through is remade *)
let rec browse_succs g f call set v =
  let new_or_not, new_set = add_to_set v !set in
  set := new_set;
  match new_or_not with
  | true -> ()
  | false -> (
      match G.succ g.cfg v with
      | [] -> (
          match Vtbl.find_opt g.ktable v with
          | Some "TERMINATOR" -> Stack.push (v, f) g.edges_to_add
          | _ -> ())
      | p :: q -> (
          match Vtbl.find_opt g.ktable p with
          | Some "TERMINATOR" ->
              Stack.push (p, f) g.edges_to_add;
              List.iter (browse_succs g f call set) q
          | Some "UNCONDITIONAL_CALL" | Some "COMPUTED_CALL" -> (
              let fall_through =
                get_fall_through (G.succ g.cfg p) p g.cfg 10000 None
              in
              match fall_through with
              | None -> Stack.push (call, f) g.edges_to_add
              | Some fall -> browse_succs g f call set fall)
          | None -> Stack.push (call, f) g.edges_to_add
          | _ -> List.iter (browse_succs g f call set) (p :: q)))

(* Adapts the edges of the body of a function
 * to link the "return" nodes with the fall_through *)
let process_call_succs succs call g =
  (* We get the right next instruction after the call *)
  let fall_through = get_fall_through succs call g.cfg 10000 None in

  match fall_through with
  | None -> ()
  | Some f ->
      Stack.push (call, f) g.edges_to_remove;
      Vtbl.replace g.next_table f call;
      Vtbl.replace g.types_table f "Next";

      let rec process_call_succs_aux succs call ktable cfg =
        match succs with
        | [] -> ()
        | v :: q -> (
            match Virtual_address.equal v f with
            | false ->
                Vtbl.replace g.types_table v "Fall";
                let set = ref AddrSet.empty in
                browse_succs g f call set v;
                process_call_succs_aux q call ktable cfg
            | true -> process_call_succs_aux q call ktable cfg)
      in
      process_call_succs_aux succs call g.ktable g.cfg

(* Unprocessed calls must keep only one edge to avoid
 * bugs during CFG traversal
 * Also adds a default stub to ignore the function call*)
let disable_call succs call cfg =
  let fall_through = get_fall_through succs call cfg 10000 None in
  match fall_through with
  | None -> failwith "Should never happen [disable_call]"
  | Some f ->
      let temp_int = Virtual_address.to_bigint f in
      let size = Kernel_options.Machine.word_size () in
      let bv = Bitvector.create temp_int size in
      let addr = Bitvector.to_hexstring bv in
      (* A default stub to ignore the function call *)
      let str =
        "0: eax<32> := nondet; goto 1  1: ecx<32> := nondet; goto 2  2: \
         edx<32> := nondet; goto 3  3: goto (" ^ addr ^ ",0) // ret "
      in
      let dhunk =
        Parse_utils.read_string ~parser:Parser.dhunk_eof ~lexer:Lexer.token
          ~string:str
      in
      Disasm_core.add_replacement call dhunk;
      let remove_e f x =
        match Virtual_address.equal f x with
        | true -> ()
        | false -> G.remove_edge cfg call x
      in

      List.iter (remove_e f) succs

(* Main function to manage calls :
 * -> uses or not stubs
 * -> adds / deletes some edges *)
let process_call g addr str =
  let addr_int = Virtual_address.to_int addr in

  match str with
  | "UNCONDITIONAL_CALL" | "COMPUTED_CALL" -> (
      Vtbl.replace g.types_table addr "Call";
      match Basic_types.Int.Set.find_opt addr_int g.to_process with
      | None -> (
          let map = Disasm_core.get_decode_replacement () in
          (* Is this address hooked ? *)
          match Virtual_address.Map.find_opt addr map with
          | None -> disable_call (G.succ g.cfg addr) addr g.cfg
          | _ -> disable_call (G.succ g.cfg addr) addr g.cfg)
      | _ -> process_call_succs (G.succ g.cfg addr) addr g)
  | "CALL_TERMINATOR" -> (
      Vtbl.replace g.types_table addr "Call";
      match Basic_types.Int.Set.find_opt addr_int g.to_process with
      | None -> (
          let map = Disasm_core.get_decode_replacement () in
          (* Is this address to be hooked ? *)
          match Virtual_address.Map.find_opt addr map with
          | None ->
              List.iter (G.remove_edge g.cfg addr) (G.succ g.cfg addr);
              (* A general stub to avoid executing the function *)
              let str =
                "0: esp<32> := esp<32> + 4<32>; goto 1 1: eax<32> := nondet; \
                 goto 2  2: ecx<32> := nondet; goto 3  3: edx<32> := nondet; \
                 goto 4  4: goto @[esp<32> - 4<32>,4] // ret "
              in
              let dhunk =
                Parse_utils.read_string ~parser:Parser.dhunk_eof
                  ~lexer:Lexer.token ~string:str
              in
              Disasm_core.add_replacement addr dhunk
          | _ -> List.iter (G.remove_edge g.cfg addr) (G.succ g.cfg addr))
      | _ -> ())
  | _ -> ()

let process cfg ktable next_table types_table =
  let edges_to_remove = Stack.create () in
  let edges_to_add = Stack.create () in

  let to_process = ToProcess.get () in

  let g =
    {
      cfg;
      ktable;
      edges_to_remove;
      edges_to_add;
      next_table;
      types_table;
      to_process;
    }
  in

  Vtbl.iter (process_call g) ktable;
  (* Adds or removes edges only when all processes are done
   * Otherwise it generates bugs *)
  add_edges g.edges_to_add g.cfg;
  remove_edges g.edges_to_remove g.cfg

(* Is it a conditionnal jump ?
 * -> Node version *)
let is_jump node ktable jumps =
  match Vtbl.find_opt ktable node.vertex with
  | None -> failwith "Each instruction must have a kind"
  | Some str -> (
      match String.equal str "CONDITIONAL_JUMP" with
      | true -> Stack.push node.vertex jumps
      | false -> ())

(* Is it a conditionnal jump ?
 * -> Address version *)
let is_jump_addr vaddr ktable jumps jumps_done =
  match Vtbl.find_opt ktable vaddr with
  | None -> failwith "Each instruction must have a kind"
  | Some str -> (
      match
        (String.equal str "CONDITIONAL_JUMP", AddrSet.mem vaddr jumps_done)
      with
      | true, false -> Stack.push vaddr jumps
      | _, _ -> ())

let rec process_node cfg n_inst ktable stack set jumps =
  match Stack.is_empty stack with
  | true -> ()
  | false -> (
      let node = Stack.pop stack in
      is_jump node ktable jumps;
      match node.k >= n_inst with
      | true -> process_node cfg n_inst ktable stack set jumps
      | false ->
          let rec process_succs_node l =
            match l with
            | [] -> ()
            | p :: q -> (
                let new_node = { vertex = p; k = node.k + 1 } in
                let new_or_not, new_set = add_to_set p !set in
                set := new_set;
                match new_or_not with
                | false ->
                    Stack.push new_node stack;
                    process_succs_node q
                | true -> process_succs_node q)
          in
          process_succs_node (G.succ cfg node.vertex);
          process_node cfg n_inst ktable stack set jumps)

(* Gets the jumps of a program
 * Begining : from start_addr
 * End      : to start_addr + n_inst *)
let get_jumps cfg start_addr n_inst ktable jumps =
  match G.mem_vertex cfg start_addr with
  | false -> failwith "This address does not belong to the CFG"
  | true ->
      let stack = Stack.create () in
      let set = ref AddrSet.empty in

      let first_node = { vertex = start_addr; k = 0 } in
      Stack.push first_node stack;
      set := AddrSet.add first_node.vertex !set;
      process_node cfg n_inst ktable stack set jumps

let from_log log =
  (* Htable address <-> kind *)
  let ktable = Vtbl.create 10 in
  (* Htable next <-> call *)
  let next_table = Vtbl.create 10 in
  (* Htable address <-> type *)
  let types_table = Vtbl.create 10 in
  (* Htable address <-> mnemonics *)
  let mnem_table = Vtbl.create 10 in
  let jumps = Stack.create () in
  let jumps_done = ref AddrSet.empty in

  let ic = open_in log in
  let cfg = create () in
  let lexbuf = Lexing.from_channel ic in
  (try
     List.iter (fun (addr, succs, kind, mnemonic) ->
         Vtbl.replace ktable addr kind;
         Vtbl.replace mnem_table addr mnemonic;

         is_jump_addr addr ktable jumps !jumps_done;
         jumps_done := AddrSet.add addr !jumps_done;

         List.iter (fun succ -> add_edge cfg addr succ) succs)
     @@ Parser_ghidra.instructions Lexer_ghidra.token lexbuf
   with _ ->
     close_in ic;
     let p = Lexing.lexeme_start_p lexbuf in
     Logger.fatal "lexeme was %s at line %d" (Lexing.lexeme lexbuf)
       p.Lexing.pos_lnum);
  close_in ic;
  Logger.debug "Total jumps : %d" (Stack.length jumps);
  process cfg ktable next_table types_table;
  if Bbsse_options.ProcessAllJumps.get () then
    (cfg, next_table, types_table, Some jumps)
  else if (not (StartingAddr.is_default ())) && not (MaxInstr.is_default ())
  then (
    let jumps_to_do = Stack.create () in
    get_jumps cfg
      (Virtual_address.of_bigint (Z.of_int (StartingAddr.get ())))
      (MaxInstr.get ()) ktable jumps_to_do;
    (cfg, next_table, types_table, Some jumps_to_do))
  else (cfg, next_table, types_table, None)

module P = Basic_types.String.Set

let from_ghidra ?(temp_dir = "/dev/shm") ?cache runner exe =
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
  P.iter (fun f ->
      let ic = open_in (temp_dir ^ f) in
      let n = in_channel_length ic in
      output_string oc @@ really_input_string ic n;
      close_in ic)
  @@ Array.fold_left
       (fun s f ->
         if String.length f > len && String.(equal base (sub f 0 len)) then
           P.add f s
         else s)
       P.empty readdir;
  let ic = open_in id in
  let n = in_channel_length ic in
  output_string oc @@ really_input_string ic n;
  close_in ic;
  close_out oc;
  let r = from_log log in
  let command = Format.sprintf "rm %s*" id in
  if Sys.command command <> 0 then Logger.fatal "can not remove tempory files";
  r

let import () =
  match (Cache.get_opt (), Runner.get_opt ()) with
  | Some log, None -> from_log log
  | cache, Some runner ->
      from_ghidra ?cache runner @@ Kernel_options.ExecFile.get ()
  | _, _ -> Logger.fatal "missing either cache or analyzeHeadless script"

module Dot = Graph.Graphviz.Dot (struct
  include G

  let graph_attributes _ = []

  let default_vertex_attributes _ = [ `Shape `Box ]

  let vertex_name v = Format.asprintf "\"%a\"" Virtual_address.pp v

  let vertex_attributes _v = []

  let get_subgraph _ = None

  let default_edge_attributes _ = []

  let edge_attributes _ = []
end)

let () =
  Cli.Boot.enlist ~name:"Ghidra" ~f:(fun () ->
      if is_enabled () then
        let res = match import () with g, _, _, _ -> g in
        Logger.result "%a" Dot.fprint_graph res)
