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

open Dba
open Errors
open Format
open Disasm_options
module Cfg = Instr_cfg

module Program = struct
  type t = {
    instructions : Cfg.t;
    callsites : Virtual_address.Set.t;
    entrypoints : Virtual_address.Set.t;
    unresolved_jumps : Virtual_address.Set.t;
  }

  let empty =
    {
      instructions = Cfg.create 1;
      callsites = Virtual_address.Set.empty;
      entrypoints = Virtual_address.Set.empty;
      unresolved_jumps = Virtual_address.Set.empty;
    }

  let create ?(callsites = Virtual_address.Set.empty)
      ?(entrypoints = Virtual_address.Set.empty)
      ?(unresolved_jumps = Virtual_address.Set.empty) instructions =
    { instructions; callsites; entrypoints; unresolved_jumps }

  let on_instructions f p = { p with instructions = f p.instructions }
  let on_callsites f p = { p with callsites = f p.callsites }
  let is_callsite vaddr p = Virtual_address.Set.mem vaddr p.callsites

  let add_callsite p callsite =
    on_callsites (Virtual_address.Set.add callsite) p

  let add_callsites p callsites =
    Virtual_address.Set.fold (fun c p -> add_callsite p c) callsites p

  let add_unresolved_jump p address =
    {
      p with
      unresolved_jumps = Virtual_address.Set.add address p.unresolved_jumps;
    }

  let ppf_tag_functions ppf =
    let print_open_stag _ = ()
    and print_close_stag = function
      | String_tag "function" -> fprintf ppf "@;<8 0> ; <_fun>"
      | _ -> ()
    in
    let mark_open_stag = function
      | String_tag "function" ->
          if Logger.get_color () then "\027[0;36m" else ""
      | _ -> ""
    and mark_close_stag = function
      | String_tag "function" -> if Logger.get_color () then "\027[0m" else ""
      | _ -> ""
    in
    { mark_open_stag; mark_close_stag; print_open_stag; print_close_stag }

  let pp_no_dba ppf v =
    match Cfg.V.inst v with
    | None -> ()
    | Some inst ->
        let vaddr = Cfg.V.addr v in
        let binstr = Instruction.to_generic_instruction inst in
        let opcode_str =
          asprintf "%a" Instruction.Generic.pp_opcode binstr |> String.trim
        in
        (* The X86 standard says in 2.3.11:
             - The maximum length of an Intel 64 and IA-32 instruction
             remains 15 bytes.
             Assuming the opcode is made of groups of 2 nibbles (1 byte),
             separated by 1 space, the max string length is computed to be:
             2 * 15 + 15 / 2  = 38

             Adjust (upwards) the value whenever other assembly languages
             have higher requirements. *)
        fprintf ppf "%a@ %-38s@ %a" Virtual_address.pp vaddr opcode_str
          Instruction.Generic.pp_mnemonic binstr

  let pp ppf p =
    pp_set_formatter_stag_functions ppf (ppf_tag_functions ppf);
    pp_set_mark_tags ppf true;
    pp_set_print_tags ppf true;
    fprintf ppf "@[<v 0>";
    Cfg.iter_vertex_by_address
      (fun v ->
        let vaddr = Cfg.V.addr v in
        let tag_string = if is_callsite vaddr p then "function" else "" in
        fprintf ppf "@[<h>@{<%s>%a@}@]@ " tag_string pp_no_dba v)
      p.instructions;
    fprintf ppf "@]";
    pp_set_mark_tags ppf false;
    pp_set_print_tags ppf false

  let count_program_instructions p =
    let h = Hashtbl.create 107 in
    let increase_count mnemonic =
      match Hashtbl.find h mnemonic with
      | n -> Hashtbl.replace h mnemonic (n + 1)
      | exception Not_found -> Hashtbl.add h mnemonic 1
    in
    Cfg.iter_vertex
      (fun v ->
        match Cfg.V.inst v with
        | None -> ()
        | Some inst -> increase_count inst.Instruction.mnemonic)
      p.instructions;
    h

  let pp_mnemonic_summary ppf p =
    let tbl = count_program_instructions p in
    let ordered =
      Hashtbl.fold (fun mnemonic count l -> (mnemonic, count) :: l) tbl []
      |> (* Sorting in decreasing order *)
      List.sort (fun (_, c1) (_, c2) -> compare c2 c1)
    in
    fprintf ppf "@[<v 0>Different instruction count:%d@ %a@]"
      (Hashtbl.length tbl)
      (fun ppf l ->
        List.iter
          (fun (m, c) ->
            (* FIXME: Would be nicer to use tabulation boxes below *)
            let s = asprintf "%a" Mnemonic.pp m in
            fprintf ppf "@[<h>%-50s@ %d@]@ " s c)
          l)
      ordered

  let pp_dba ppf p =
    fprintf ppf "@[<v 0>%a@]"
      (fun ppf p ->
        Cfg.iter_vertex_by_address
          (fun v ->
            match Cfg.V.inst v with
            | None -> ()
            | Some inst ->
                let dhunk = inst.Instruction.dba_block in
                fprintf ppf "@[<v 0>@[<h># -- %a@]@ %a@]@ @ " pp_no_dba v
                  Dhunk.pp dhunk)
          p.instructions)
      p

  let pp_details ppf p =
    let pp_set title ppf s =
      if not (Virtual_address.Set.is_empty s) then (
        let size = Virtual_address.Set.cardinal s in
        pp_open_vbox ppf 2;
        fprintf ppf "## %s (%d)@," title size;
        pp_open_hovbox ppf 0;
        Virtual_address.Set.iter
          (fun vaddr -> fprintf ppf "%a;@ " Virtual_address.pp vaddr)
          s;
        pp_close_box ppf ();
        pp_close_box ppf ();
        pp_print_cut ppf ())
    in
    fprintf ppf "@[<v 0>%a%a%a@]" (pp_set "Entry points") p.entrypoints
      (pp_set "Functions") p.callsites
      (pp_set "Unresolved jumps")
      p.unresolved_jumps
end

open Program

(* Should it be here ? *)
let simplify_block = Dhunk.constant_propagation

(* Add a block to the program in construction.
   This block is simplified.
*)
let add_block instr p =
  let hw_address = instr.Instruction.address in
  let block = instr.Instruction.dba_block in
  let simplified_block = simplify_block block in
  let instr' = Instruction.set_dba_block instr simplified_block in
  Cfg.add_inst p hw_address instr';
  p

let join_wl wl1 wl2 b1 b2 =
  List.fold_left
    (fun acc a ->
      let bv = Virtual_address.to_bigint (Dba_types.Caddress.base_value a) in
      if Z.gt b1 bv || Z.gt bv b2 then a :: acc else acc)
    wl1 wl2

(* FIXME: Use new blocks *)
let extra_info dinstr =
  let rec aux = function
    | [] -> ([], None)
    | [ (_, Dba.Instr.SJump (JOuter dst, (Dba.Call add_ret as tag))) ] ->
        ([ dst; add_ret ], Some (None, Some dst, tag))
    | [ (_, Dba.Instr.SJump (JOuter dst, tag)) ] ->
        ([ dst ], Some (None, Some dst, tag))
    | [ (_, Dba.Instr.DJump (dst, (Dba.Call add_ret as tag))) ] ->
        ([ add_ret ], Some (Some dst, None, tag))
    | [ (_, Dba.Instr.DJump (dst, tag)) ] -> ([], Some (Some dst, None, tag))
    | [
        (_, Dba.Instr.If (_, JOuter thn, _));
        (_, Dba.Instr.SJump (JOuter nextaddr, _));
      ] ->
        ([ thn; nextaddr ], None)
    | [ (_, Dba.Instr.If _); (_, Dba.Instr.SJump (JOuter nextaddr, _)) ] ->
        ([ nextaddr ], None)
    | [ _ ] | [ _; (_, Dba.Instr.Stop _) ] ->
        ([], None) (* no recurive successors *)
    | _ :: insns -> aux insns
  in
  let instlist =
    let open Instruction in
    let addr = dinstr.address in
    let block = dinstr.dba_block in
    Dba_types.(
      Dhunk.to_stmts block addr
      |> List.map (fun locinstr ->
             (Statement.location locinstr, Statement.instruction locinstr)))
  in
  aux instlist

let find_calls instr jumps =
  let nexts, tag = extra_info instr in
  let calls =
    match tag with
    | None | Some (_, _, (Default | Dba.Return)) | Some (None, None, Dba.Call _)
      ->
        []
    | Some (None, Some a, Dba.Call _) -> [ a ]
    | Some (Some _, None, Dba.Call _) -> (
        let cur_addr =
          Dba_types.Caddress.block_start_of_int
            (instr.Instruction.address :> int)
        in
        match Dba_types.Caddress.Map.find cur_addr jumps with
        | l -> l
        | exception Not_found -> [])
    | Some (Some _, Some _, Dba.Call _) ->
        (* Never generated by extra_info *)
        failwith "Disasm: both static and dynamic jump targets provided"
  in
  (nexts, calls)

let get_call_targets instr = Dhunk.callees instr.Instruction.dba_block

(* Gather all successors made in block except if the user has specified them.

   Another desirable behaviors could be to make the union of both sets.
*)
let successors user_jumps fsucc instr =
  let caddr = Instruction.get_caddress instr in
  let open Dba_types in
  match Caddress.Map.find caddr user_jumps with
  | l -> List.map Caddress.to_virtual_address l |> Virtual_address.Set.of_list
  | exception Not_found -> fsucc instr

let get_instruction address stops =
  let caddress = Dba_types.Caddress.of_virtual_address address in
  if Dba_types.Caddress.Set.mem caddress stops then
    (Instruction.stop address, None)
  else Disasm_core.decode address

(* Insert a node from inst with edgest to its successors in the graph [g] *)
let insert g inst succs =
  let i_vaddr = inst.Instruction.address in
  (* Add the current instruction to the graph *)
  let src = Cfg.V.of_inst i_vaddr inst in
  Cfg.add_vertex g src;
  Virtual_address.Set.iter
    (fun vaddr -> Cfg.add_edge g src (Cfg.V.of_addr vaddr))
    succs

module Recursive = struct
  let level = 5

  let insert_successors succs worklist =
    Logger.debug ~level:3 "Inserting succs: @[<hov 0>%a@]"
      (fun ppf vaddr_set ->
        Virtual_address.Set.iter
          (fun vaddr -> fprintf ppf "%a;@ " Virtual_address.pp vaddr)
          vaddr_set)
      succs;
    Virtual_address.Set.fold Disasm_core.W.add succs worklist

  let aux_rec visited program worklist jumps stops =
    let rec loop program visited wl =
      if Disasm_core.W.is_empty wl then program
      else
        let address, addresses = Disasm_core.W.pop wl in
        if not (Virtual_address.Set.mem address visited) then
          let visited = Virtual_address.Set.add address visited in
          try
            Logger.debug ~level "Recursive decoding @%a with %a"
              Virtual_address.pp address Disasm_core.W.pp wl;
            let instr, _nextaddr = get_instruction address stops in
            (* Computing successors *)
            let call_targets = get_call_targets instr in
            let successors =
              successors jumps Disasm_core.Successors.recursive instr
            in
            let wl' = insert_successors successors addresses in
            let p' =
              add_callsites program call_targets
              |> on_instructions (add_block instr)
            in
            loop p' visited wl'
          with
          | Disasm_core.Decode_error s | Invalid_address s ->
              Logger.warning "@[%s %@ %a@]" s Virtual_address.pp address;
              loop program visited worklist
          | Invalid_argument s -> Logger.fatal "@[invalid argument (%s)@]" s
        else loop program visited addresses
    in
    loop program visited worklist

  let apply_aux = aux_rec Virtual_address.Set.empty

  let disassemble ?(jumps = Dba_types.Caddress.Map.empty)
      ?(stops = Dba_types.Caddress.Set.empty)
      ?(visited = Virtual_address.Set.empty) ?(worklist = Disasm_core.W.empty)
      program =
    aux_rec visited program worklist jumps stops

  let apply parameters =
    let open Infos in
    let wl = Disasm_core.W.of_set parameters.entry_points in
    let jmps = parameters.jumps in
    let stops = parameters.stops in
    apply_aux Program.empty wl jmps stops

  module D = Disasm_core.Make (struct
    let successors = Disasm_core.Successors.recursive
  end)

  let slice ~start ~stop =
    let open Disasm_core in
    Logger.debug "@[<hov>Recursive disassembly on slice [%a, %a]@]"
      Virtual_address.pp start Virtual_address.pp stop;
    let filter_p x = x <= stop in
    let f p wl inst vnexts =
      Logger.debug "@[<hov>Successors %@ %a : %a@]" Virtual_address.pp
        inst.Instruction.address Virtual_address.pp_set vnexts;
      let wl' = W.add_filtered_set filter_p wl vnexts in
      (* The instruction should be handled. Succs may be out of bounds *)
      insert p.Program.instructions inst vnexts;
      let p' =
        let hunk = inst.Instruction.dba_block in
        if Dhunk.has_indirect_jump hunk then
          Program.add_unresolved_jump p inst.Instruction.address
        else p
      in
      (p', wl')
    in
    W.singleton start |> D.fold f Program.empty
end

(* The default interval end is the section's end address *)
let compute_interval_end ~from_address ~img =
  let _, section_end =
    Loader_utils.section_slice_by_address ~address:from_address img
  in
  Logger.info "@[<h>Using section until %x@]" section_end;
  (Virtual_address.create from_address, Virtual_address.create section_end)

let compute_linear_disasm_intervals img parameters =
  let open Infos in
  if has_entry_points parameters then
    let open Virtual_address.Set in
    let rec loop acc eps =
      if is_empty eps then acc @ parameters.linear_addresses
      else
        let ep, eps = pop eps in
        let ep_interval = compute_interval_end ~from_address:(ep :> int) ~img in
        loop (ep_interval :: acc) eps
    in
    loop [] parameters.entry_points
  else parameters.linear_addresses

module Extended_linear = struct
  (* The recursive linear module implements the following disassembly strategy.

     Given a set of address intervals to disassemble linearly, it keeps track
     of static jumps. If a jump belongs to the current interval being
     disassembled, it is added to the worklist.

     The benefit is that we are able to find and disassemble overlapping
     instructions.

     Another possible strategy would be:
     - linearly disassemble intervals, keeping track of the jump targets.
     - in the end, gather all jump targets recognized. For those whose address
       has not been disassembled, start a recursive disassembly.
  *)

  let aux_reclinear addr iend program jumps wl visited stops =
    let initial_address = Virtual_address.to_bigint addr in
    let bigend = Virtual_address.to_bigint iend in
    let rec loop (addr : Virtual_address.t) program =
      if addr > iend then program
      else
        try
          Logger.debug ~level:4 "Disassembling %a" Virtual_address.pp addr;
          let open Dba_types in
          let instr, nextaddr = get_instruction addr stops in
          match nextaddr with
          | None -> program
          | Some succ_addr ->
              let wl', calls = find_calls instr jumps in
              (* Add all elemnts from [wl'] that are in the linear interval
                  [initial_address, iend] *)
              let wl =
                join_wl wl wl' initial_address bigend
                |> List.map Caddress.to_virtual_address
                |> Disasm_core.W.of_list
              in
              let vcalls =
                List.fold_left
                  (fun vset c ->
                    let vaddr = Dba_types.Caddress.to_virtual_address c in
                    Virtual_address.Set.add vaddr vset)
                  Virtual_address.Set.empty calls
              in
              let p = add_callsites program vcalls in
              let p =
                Recursive.aux_rec visited p wl jumps stops
                |> Program.on_instructions (add_block instr)
              in
              loop succ_addr p
        with
        | Disasm_core.Decode_error s | Invalid_address s ->
            Logger.error "%s %@ %a" s Virtual_address.pp addr;
            program
        | Invalid_argument s ->
            Logger.error "invalid argument (%s)" s;
            program
    in
    loop addr program

  let apply parameters =
    let open Infos in
    let jmps = parameters.jumps in
    let stops = parameters.stops in
    let f program (start, end_) =
      let visited = Virtual_address.Set.empty in
      aux_reclinear start end_ program jmps [] visited stops
    in
    compute_linear_disasm_intervals (Kernel_functions.get_img ()) parameters
    |> List.fold_left f Program.empty
end

module Linear = struct
  open Disasm_core

  module I = Make (struct
    let successors = Successors.linear
  end)

  let aux_linear worklist program (stop : Virtual_address.t) =
    let should_stop = ( < ) stop in
    let step p worklist instruction disasm_succs =
      assert (Virtual_address.Set.cardinal disasm_succs <= 1);
      (* The instruction should be handled. Succs may be out of bounds *)
      let g = program.Program.instructions in
      (* There are 2 types of successors:
         1. the linear one (aka disasm_succs) giving the next address to
         disassemble;
         2. flow ones (aka jumps, ...) which should be rendered as edges in the
         CFG;

         We first add the latter as successors in the CFG.
      *)
      let hunk = instruction.Instruction.dba_block in

      let flow_succs = Dhunk.outer_jumps hunk in
      insert g instruction flow_succs;
      let p' =
        if Dhunk.has_indirect_jump hunk then
          let i_vaddr = instruction.Instruction.address in
          Program.add_unresolved_jump p i_vaddr
        else p
      in
      ( p',
        Virtual_address.Set.fold
          (fun vaddr w ->
            let dst, w =
              if should_stop vaddr then
                let inst = Instruction.of_dba_block vaddr Dhunk.stop in
                (Cfg.V.of_inst vaddr inst, w)
              else
                (* The disassembled instruction will be added later on.
                   Just put in the addres vertex for now. *)
                (Cfg.V.of_addr vaddr, W.add vaddr w)
            in
            Cfg.add_vertex g dst;
            w)
          disasm_succs worklist )
    in
    I.fold step program worklist

  (* Inelegant solution to a real problem.
     15 bytes is the biggest x86 opcode. Thus it should be enough in the linear
     case to "catch" any computed successor of the upper bound of the desired
     linear interval.
     We always have [cur_address + increment] <= upper_bound + 15
  *)
  let _pad_fifteen_bytes from_caddr stops =
    let rec loop increment s =
      if increment = 15 then s
      else
        let caddr = Dba_types.Caddress.add_int from_caddr increment in
        loop (succ increment) (Dba_types.Caddress.Set.add caddr s)
    in
    loop 0 stops

  let apply ~(byte_wise : bool) (intervals : Virtual_address.t Interval.t list)
      =
    if byte_wise then Disassembly_mode.set Linear_byte_wise
    else Disassembly_mode.set Linear;
    let open Interval in
    let aux program ival =
      Logger.result "@[<h>Linear disassembly from %a to %a@]" Virtual_address.pp
        ival.lo Virtual_address.pp ival.hi;
      let worklist = Disasm_core.W.singleton ival.lo in
      aux_linear worklist program ival.hi
    in
    let approx_cfg_size =
      List.fold_left
        (fun sz ival ->
          let h = Virtual_address.to_int ival.hi
          and l = Virtual_address.to_int ival.lo in
          h - l + sz + 1)
        0 intervals
    in
    let cfg = Instr_cfg.create approx_cfg_size in
    let p = Program.create cfg in
    List.fold_left aux p intervals
end

(* FIXME: Yes I know program is unused *)
[@@@ocaml.warning "-27"]

let disassemble_slice ~program ~(slice_start : Virtual_address.t)
    ~(slice_end : Virtual_address.t) =
  match Disassembly_mode.get () with
  | Linear ->
      let ival = { Interval.hi = slice_end; Interval.lo = slice_start } in
      Linear.apply ~byte_wise:false [ ival ]
  | Recursive -> Recursive.slice ~start:slice_start ~stop:slice_end
  | _ -> assert false

let disassemble_section ?(program = Program.empty) img section_name =
  let sec_start, sec_end =
    Loader_utils.section_slice_by_name section_name img
  in
  Logger.debug "Disassembling section %s : [0x%x -- 0x%x]" section_name
    (sec_start :> int)
    (sec_end :> int);
  let slice_end = Virtual_address.create sec_end in
  let slice_start = Virtual_address.create sec_start in
  disassemble_slice ~program ~slice_start ~slice_end

let section = disassemble_section

let sections ?(program = Program.empty) img secs =
  Basic_types.String.Set.fold
    (fun section_name program ->
      try disassemble_section ~program img section_name
      with Not_found ->
        Logger.warning "Skipping unknown section %s" section_name;
        program)
    secs program

let disassemble_sections () =
  assert (Disasm_options.Sections.is_set ());
  (* force linear mode *)
  Disasm_options.Disassembly_mode.set Disasm_options.Linear;
  let img = Kernel_functions.get_img () in
  sections img @@ Disasm_options.Sections.get ()

module Basics = Basic_types

let disassemble_function g ~funcentry =
  let open Disasm_core in
  let wl = W.singleton funcentry in
  let module Dis = Make (struct
    let successors = Successors.linear
  end) in
  Dis.iter
    (fun wl i succs ->
      let src = i.Instruction.address in
      Cfg.add_inst g src i;
      if Dhunk.is_return i.Instruction.dba_block then wl
      else (
        Virtual_address.Set.iter (fun dst -> Cfg.add_edge_a g src dst) succs;
        W.add_set wl succs))
    wl;
  g

let do_functions g img funcnames =
  let function_addrs =
    Basics.String.Set.fold
      (fun funcname addrs ->
        match Loader_utils.address_of_symbol_by_name ~name:funcname img with
        | None ->
            Logger.warning "No function named %s. Skipping." funcname;
            addrs
        | Some vaddr ->
            Logger.debug ~level:5 "Add address %x for function %s" vaddr
              funcname;
            Virtual_address.Set.add (Virtual_address.create vaddr) addrs)
      funcnames Virtual_address.Set.empty
  in
  Virtual_address.Set.fold
    (fun funcentry g -> disassemble_function g ~funcentry)
    function_addrs g

exception Entry_found of Cfg.V.t

let pp_cfg ?(file = "function_cfg.dot") g =
  let oc = open_out_bin file in
  let entry =
    (* We just take the first vertex as given by iter as our entry point.
       This might not always be a good idea.
    *)
    try
      Cfg.iter_vertex (fun v -> raise (Entry_found v)) g;
      assert false
    with Entry_found v -> v
  in
  Cfg.output_graph oc g ~entry [];
  close_out oc

let handle_functions funcnames =
  let img = Kernel_functions.get_img () in
  let g = do_functions (Cfg.create 17) img funcnames in
  pp_cfg g;
  Program.create g

let pp_mode ppf = function
  | Disasm_options.Recursive -> Format.fprintf ppf "recursive"
  | Disasm_options.Extended_linear -> Format.fprintf ppf "extended linear"
  | Disasm_options.Linear -> Format.fprintf ppf "linear"
  | Disasm_options.Linear_byte_wise -> Format.fprintf ppf "linear byte wise"

(* Get the entry points from the parameters file if they exist,
   Otherwise, just take what the loader says is the one entry point.

   This function should be removed once we get rid of the Infos module.
*)
let get_initial_entry_points img parameters =
  let open Infos in
  if has_entry_points parameters then parameters.entry_points
  else
    let ep = Loader_utils.entry_point img in
    Logger.info "Starting from default entry point %a" Virtual_address.pp ep;
    Virtual_address.Set.singleton ep

let file ~filename =
  let img = Loader.load_file filename in
  let ep = Loader_utils.entry_point img in
  let slice_start, slice_end =
    compute_interval_end ~from_address:(ep :> int) ~img
  in
  disassemble_slice ~program:Program.empty ~slice_start ~slice_end

let disassemble parameters =
  let dba_file = DbaOutputFile.get ()
  and opcode_file =
    if OpcodeOutputFile.is_set () then OpcodeOutputFile.get () else "stdout"
  in
  Logger.debug "Disassembling mode %a (dba file=%s, opcode file=%s)" pp_mode
    (Disassembly_mode.get ()) dba_file opcode_file;
  if Functions.is_set () then
    let funcnames = Functions.get () in
    handle_functions funcnames
    (* Section disassembly has priority over specific entrypoints *)
  else if Sections.is_set () then (
    if Infos.has_entry_points parameters then
      Logger.warning "Section disassembly overrides entry points option";
    disassemble_sections ())
  else if Kernel_options.Dba_config.is_set () then
    let linear_p ~byte_wise p =
      let open Infos in
      let open Interval in
      let (intervals : Virtual_address.t Interval.t list) =
        List.map (fun (lo, hi) -> { lo; hi }) p.linear_addresses
      in
      Linear.apply ~byte_wise intervals
    in

    let disassembler =
      match Disassembly_mode.get () with
      | Recursive -> Recursive.apply
      | Extended_linear -> Extended_linear.apply
      | Linear -> linear_p ~byte_wise:false
      | Linear_byte_wise -> linear_p ~byte_wise:true
    in
    disassembler parameters
  else
    let img = Kernel_functions.get_img () in
    let rec disasm_eps program eps =
      if Virtual_address.Set.is_empty eps then program
      else
        let ep, eps = Virtual_address.Set.pop eps in
        let slice_start, slice_end =
          compute_interval_end ~from_address:(ep :> int) ~img
        in
        disasm_eps (disassemble_slice ~program ~slice_start ~slice_end) eps
    in
    let eps = get_initial_entry_points img parameters in
    Logger.debug ~level:2 "Entry points: @[%a@]"
      (fun ppf vset ->
        Virtual_address.Set.iter
          (fun e -> Format.fprintf ppf "%a;@ " Virtual_address.pp e)
          vset)
      eps;
    disasm_eps Program.empty eps

let run () =
  let parameters = Infos.default in
  Logger.result "Entry points: @[%a@]"
    (fun ppf vset ->
      Virtual_address.Set.iter
        (fun e -> Format.fprintf ppf "%a;@ " Virtual_address.pp e)
        vset)
    parameters.Infos.entry_points;
  let program = disassemble parameters in
  if OpcodeOutputFile.is_set () then
    Print_utils.pp_to_file ~filename:(OpcodeOutputFile.get ()) Program.pp
      program
  else
    Logger.result "@[<v 0>%a@ %a@]" Program.pp program Program.pp_details
      program;
  if ShowInstructionCount.get () then
    Logger.result "@[%a@]" Program.pp_mnemonic_summary program;
  Print_utils.pp_to_file ~filename:(DbaOutputFile.get ()) Program.pp_dba program

(* Other functionalities *)
let custom_pp_dbainstrs opc ppf dba_block =
  let open Dba_printer.EICUnicode in
  let opc = Mnemonic.to_string opc in
  let spaces = String.make (String.length opc) ' ' in
  pp_set_margin ppf 250;
  fprintf ppf "%a" Dhunk.pp dba_block;
  fprintf ppf "@[";
  let pp_ith ppf n =
    Print_utils.pp_opt pp_instruction ppf (Dhunk.inst dba_block n)
  in
  let mypp ppf i = fprintf ppf "@[<h>%2d: %a@]" i pp_ith i in
  (match Dhunk.length dba_block with
  | 0 -> ()
  | 1 -> fprintf ppf "@[<h>%s → %a@]" opc pp_ith 0
  | 2 ->
      fprintf ppf "@[<v 0> %s ⎧1: %a@  %s ⎩2: %a@ @]" opc pp_ith 0 spaces pp_ith
        1
  | nelts ->
      let middle = nelts / 2 in
      let pp_bar fmt i =
        if i = middle then fprintf fmt "%s ⎨" opc else fprintf fmt "%s ⎪" spaces
      in
      let rec aux i =
        if i = 0 then (
          fprintf ppf "@[<v 0>@[<h>%s ⎧%a@]@ " spaces mypp i;
          aux 1)
        else if i = nelts - 1 then fprintf ppf "@[<h>%s ⎩%a@]@]" spaces mypp i
        else (
          fprintf ppf "@[<h>%a%a@]@ " pp_bar i mypp i;
          aux (i + 1))
      in
      aux 0);
  fprintf ppf "@]"

let check_hex_string s =
  let open String_utils in
  match lfindi s (fun c -> not (is_hex_char c)) with
  | Some i ->
      Logger.fatal "Invalid hexadecimal character '%c' in opcode %s" s.[i] s
  | None -> ()

let _pp_pretty_utf8 i =
  let open Instruction in
  Logger.result "@[<v 0>%a@]" (custom_pp_dbainstrs i.mnemonic) i.dba_block

let inst_of_raw ?base raw =
  check_hex_string raw;
  Binstream.of_nibbles raw |> Disasm_core.decode_binstream ?base |> fst

let decode raw =
  try
    let base = Virtual_address.of_string (Disasm_at.get ()) in
    let i = inst_of_raw ~base raw in
    Logger.result "%a" Instruction.pp i
  with X86toDba.InstructionUnhandled s ->
    Logger.warning "Not decoded %s" s;
    exit 1

let main () =
  if Disasm_options.is_enabled () && Kernel_options.ExecFile.is_set () then
    if Disasm_options.CFG_graph.get () then Disasm_cfg.run ()
    else (
      Logger.info "Running disassembly";
      run ())

let run_decode () =
  if Disasm_options.Decode_instruction.is_set () then
    decode (Disasm_options.Decode_instruction.get ())

let _ =
  Cli.Boot.enlist ~name:"disassembly run" ~f:main;
  Cli.Boot.enlist ~name:"decode hex" ~f:run_decode
