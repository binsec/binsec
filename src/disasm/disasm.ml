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

open Dba
open Errors
open Format



let _opaque_predicates_map = ref Dba_types.Caddress.Map.empty
let violated_calls = ref Dba_types.Caddress.Set.empty
let _regular_calls = ref Dba_types.Caddress.Set.empty

let _check_opaque_predicates addr opaque_predicates_map nextaddrs =
  try
    let next_addr = Dba_types.Caddress.Map.find addr opaque_predicates_map in
    match next_addr with
    | None -> nextaddrs
    | Some a -> [a]
  with Not_found -> nextaddrs

module Program = struct
  type t = {
    instructions : Disasm_types.Program.t;
    callsites    : Dba_types.Caddress.Set.t
  }

  let empty = {
    instructions = Dba_types.Virtual_address.Map.empty;
    callsites = Dba_types.Caddress.Set.empty;
  }

  let create instructions callsites = { instructions; callsites; }

  let on_instructions f p = { p with instructions = f p.instructions }

  let on_callsites f p = { p with callsites = f p.callsites }

  let is_callsite caddr p = Dba_types.Caddress.Set.mem caddr p.callsites

  let add_callsite p callsite =
    on_callsites (Dba_types.Caddress.Set.add callsite) p

  let add_callsites = List.fold_left add_callsite

  let ppf_tag_functions ppf =
    let print_open_tag _ = ()
    and print_close_tag = function
      | "function" -> fprintf ppf "@;<8 0> ; <_fun>"
      | _ -> ()
    in
    let mark_open_tag = function
      | "function" ->
        if Logger.get_color () then "\027[0;36m" else ""
      | _ -> ""
    and mark_close_tag = function
      | "function" ->
        if Logger.get_color () then "\027[0m" else ""
      | _ -> ""
    in { mark_open_tag;  mark_close_tag;
         print_open_tag; print_close_tag; }


  let pp_no_dba vaddr ppf ginstr =
    let binstr = Disasm_types.Instruction.to_generic_instruction ginstr in
    let opcode_str =
      asprintf "%a" Disasm_types.GenericInstruction.pp_opcode binstr
      |> String.trim
    in
    (* The X86 standard says in 2.3.11:
       - The maximum length of an Intel 64 and IA-32 instruction
       remains 15 bytes.
       Assuming the opcode is made of groups of 2 nibbles (1 byte),
       separated by 1 space, the max string length is computed to be:
       2 * 15 + 15 / 2  = 38
       
       Adjust (upwards) the value whenever other assembly languages
       have higher requirements.
     *)
   
    fprintf ppf "%a@ %-38s@ %a"
            Dba_types.Virtual_address.pp vaddr
            opcode_str
            Disasm_types.GenericInstruction.pp_mnemonic binstr
    
  let pp ppf p =
    let open Dba_types.Virtual_address in
    pp_set_formatter_tag_functions ppf (ppf_tag_functions ppf);
    pp_set_mark_tags ppf true;
    pp_set_print_tags ppf true;
    fprintf ppf "@[<v 0>";
    Map.iter
      (fun vaddr ginstr ->
        let tag_string =
          if is_callsite (to_code_address vaddr) p then "function" else "" in
        fprintf ppf "@[<h>@{<%s>%a@}@]@ " tag_string (pp_no_dba vaddr) ginstr)
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
    Dba_types.Virtual_address.Map.iter
      (fun _ instr -> increase_count instr.Disasm_types.Instruction.mnemonic)
      p.instructions;
    h
    
  let pp_mnemonic_summary ppf p =
    let tbl = count_program_instructions p in
    let ordered =
      Hashtbl.fold (fun mnemonic count l -> (mnemonic, count) :: l) tbl []
      |> List.sort (fun (_, c1) (_, c2) -> Pervasives.compare c2 c1) (* decreasing order *)
    in
    fprintf ppf "@[<v 0>Different instruction count:%d@ %a@]"
    (Hashtbl.length tbl)
    (fun ppf l ->
      List.iter
        (fun (m, c) ->
          (* FIXME: Would be nicer to use tabulation boxes below *)
          let s = asprintf "%a" Disasm_types.Mnemonic.pp m in
          fprintf ppf "@[<h>%-50s@ %d@]@ " s c; 
    ) l) ordered

    
  let pp_dba ppf p =
    let open Dba_types.Virtual_address in
    fprintf
      ppf "@[<v 0>%a@]"
      (fun ppf p ->
        Map.iter
          (fun vaddr instr ->
            fprintf ppf
                    "@[<v 0>@[<h># -- %a@]@ %a@]@ @ "
                    (pp_no_dba vaddr) instr
                    Dba_types.Block.pp (instr.Disasm_types.Instruction.dba_block)
          )
          p.instructions
      ) p
    
end


open Program


(* Should it be here ? *)
let simplify_block b =
  Dba_types.Block.map Simplification_dba_instr.simplify_instruction b
  |> Simplification_dba_block.Constant_propagation.eval


(* Add a block to the program in construction.
   This block is simplified.
*)
let add_block instr p =
  let open Disasm_types in
  let hw_address = instr.Instruction.address in
  let block = instr.Instruction.dba_block in
  let simplified_block = simplify_block block in
  let instr' = Instruction.set_dba_block instr simplified_block in
  Dba_types.Virtual_address.Map.add hw_address instr' p


let join_wl wl1 wl2 b1 b2 =
  List.fold_left
    (fun acc a ->
      let bv = Dba_types.Caddress.base_value a in
      if Bigint.gt_big_int b1 bv || Bigint.gt_big_int bv b2 then a :: acc
      else acc
    ) wl1 wl2


(* FIXME: Use new blocks *)
let extra_info dinstr =
  let insert_ret addr ret_addr =
    if Disasm_dyn_infos_callrets.is_mem_call addr !violated_calls then []
    else [ret_addr]
  in
  let rec aux = function
    | [] -> [], None
    | [addr, Dba.IkSJump (JOuter dst, (Some (Dba.Call add_ret) as tag))] ->
      dst :: insert_ret addr add_ret,

      Some (None, Some dst, tag)

    | [_, Dba.IkSJump (JOuter dst, tag)] ->
      [dst], Some (None, Some dst, tag)

    | [addr, Dba.IkDJump (dst, (Some (Dba.Call add_ret) as tag))] ->
      insert_ret addr add_ret, Some (Some dst, None, tag)

    | [_, Dba.IkDJump (dst, tag)] ->
      [], Some (Some dst, None, tag)


    | (_, Dba.IkIf(_, JOuter thn, _))
      :: [_, Dba.IkSJump ((JOuter nextaddr), _)] ->
      [thn; nextaddr], None

    |  (_, Dba.IkIf _) :: [_, Dba.IkSJump ((JOuter nextaddr), _)] ->
      [nextaddr], None

    | [_]
    | _ :: [_, Dba.IkStop _] -> [], None (* no recurive successors *)
    | _ :: insns -> aux insns
  in
  let instlist =
    let open Disasm_types.Instruction in
    let addr = dinstr.address in
    let block = dinstr.dba_block in
    Dba_types.(
      Block.to_dbainstrs block addr
      |> List.map
        (fun locinstr -> Statement.location locinstr,
                         Statement.instruction locinstr))
  in aux instlist


let find_calls instr jumps =
  let nexts, tag = extra_info instr in
  let calls =
    match tag with
    | None
    | Some (_, _, (None | Some (Dba.Return)))
    | Some (None, None, Some (Dba.Call _)) -> []
    | Some (None, Some a, Some (Dba.Call _)) -> [a]
    | Some (Some _, None, Some (Dba.Call _)) ->
      begin
        let cur_addr =
          Dba_types.Caddress.block_start_of_int
            (instr.Disasm_types.Instruction.address:>int)
        in
        match Dba_types.Caddress.Map.find cur_addr jumps with
        | l -> l
        | exception Not_found -> []
      end
    | Some (Some _, Some _, Some (Dba.Call _)) ->
      (* Never generated by extra_info *)
      failwith
        "Disasm: both static and dynamic jump targets provided"
  in nexts, calls

let get_call_targets instr =
  Dba_types.Block.callees instr.Disasm_types.Instruction.dba_block

(* Gather all successors made in block except if the user has specified them.

   Another desirable behaviors could be to make the union of both sets.
*)
let successors user_jumps fsucc instr =
  let open Dba_types in
  let caddr = Disasm_types.Instruction.get_caddress instr in
  match Caddress.Map.find caddr user_jumps with
  | l ->
    List.map Virtual_address.of_code_address l |> Virtual_address.Set.of_list
  | exception Not_found -> fsucc instr


let get_instruction address stops =
  let open Dba_types in
  let caddress = Dba_types.Virtual_address.to_code_address address in
  if Caddress.Set.mem caddress stops
  then Disasm_types.Instruction.stop address, None
  else Disasm_core.decode address


module Recursive = struct
  open Dba_types
  let level = 5

  let insert_successors succs worklist =
    Logger.debug ~level:3 "Inserting succs: @[<hov 0>%a@]"
      (fun ppf vaddr_set ->
         Virtual_address.Set.iter
           (fun vaddr -> fprintf ppf "%a;@ " Virtual_address.pp vaddr)
           vaddr_set)
      succs;
    Virtual_address.Set.fold Disasm_core.W.add succs worklist


  let add_call_locations program call_vaddresses =
    let l =
      Virtual_address.Set.fold
        (fun vaddr l -> Virtual_address.to_code_address vaddr :: l)
        call_vaddresses []
    in add_callsites program l


  let aux_rec visited program worklist jumps stops =
    let rec loop program visited wl =
      if Disasm_core.W.is_empty wl then program
      else
        let address, addresses = Disasm_core.W.pop wl in
        if not (Virtual_address.Set.mem address visited) then begin
          let visited = Virtual_address.Set.add address visited in
          try
          Logger.debug ~level "Recursive decoding @%a with %a"
            Virtual_address.pp address Disasm_core.W.pp wl;
          let instr, _nextaddr = get_instruction address stops in
          (* Computing successors *)
          let call_targets = get_call_targets instr in
          let successors = successors jumps Disasm_core.Successors.recursive instr in
          let wl' = insert_successors successors addresses in
          let p' =
            add_call_locations program call_targets
            |> on_instructions (add_block instr) in
          loop p' visited wl'
          with
          | Disasm_core.Decode_error s
          | Invalid_address s ->
            Logger.warning "@[%s %@ %a@]"
              s Virtual_address.pp address;
            loop program visited worklist
          | Invalid_argument s ->
            Logger.fatal "@[invalid argument (%s)@]" s;
          exit 3
        end
        else loop program visited addresses
    in loop program visited worklist

  let apply_aux = aux_rec Dba_types.Virtual_address.Set.empty

  let disassemble
      ?(jumps=Dba_types.Caddress.Map.empty)
      ?(stops=Dba_types.Caddress.Set.empty)
      ?(visited=Dba_types.Virtual_address.Set.empty)
      ?(worklist=Disasm_core.W.empty) program =
    aux_rec visited program worklist jumps stops

  let apply parameters =
    let open Infos in
    let wl =
      Disasm_core.W.add_virtual_addresses parameters.entry_points Disasm_core.W.empty  in
    let jmps = parameters.jumps in
    let stops = parameters.stops in
    apply_aux Program.empty wl jmps stops

end


module Extended_linear = struct

(* The recursive linear module implements for now the following disassembly
   strategy.
   Given a set of address intervals to disassemble linearly, it also keeps track
   of jumps. If a jump belongs to the current interval being disassembled, it is
   added to the worklist.

   The benefit is that we are able to find and disassemble overlapping
   instructions.

   Another possible strategy would be:
   - linearly disassemble intervals, keeping track of the jump targets.
   - in the end, gather all jump targets recognized. For those whose address has
   not been disassembled, start a recursive disassembly.
*)

  let aux_reclinear addr iend program jumps wl visited stops =
    let open Dba_types in
    let initial_address = Virtual_address.to_bigint addr in
    let bigend = Virtual_address.to_bigint iend in
    let rec loop (addr:Dba_types.Virtual_address.t) program =
      if addr > iend then program
      else
        try
          Logger.debug ~level:4 "Disassembling %a" Virtual_address.pp addr;
          let instr, nextaddr = get_instruction addr stops in
          match nextaddr with
          | None -> program
          | Some succ_addr ->
            let wl', calls = find_calls instr jumps in
            (* Add all elemnts from [wl'] that are in the linear interval
                [initial_address, iend] *)
            let wl =
              join_wl wl wl' initial_address bigend
              |> List.map Virtual_address.of_code_address
              |> Disasm_core.W.of_list in
            let p = add_callsites program calls in
            let p =
              Recursive.aux_rec visited p wl jumps stops
              |> Program.on_instructions (add_block instr) in
            loop succ_addr p
        with
        | Disasm_core.Decode_error s
        | Invalid_address s ->
          Logger.error "%s %@ %a" s Virtual_address.pp addr;
          program
        | Invalid_argument s ->
          Logger.error "invalid argument (%s)" s;
          program
    in loop addr program


  let apply parameters =
    let open Infos in
    let jmps = parameters.jumps in
    let stops = parameters.stops in
    let linear_addresses = parameters.linear_addresses in
    let f program (start, end_) =
      let visited = Dba_types.Virtual_address.Set.empty in
      aux_reclinear start end_ program jmps [] visited stops
    in
    List.fold_left f Program.empty linear_addresses

end


module Linear = struct


  (* The upper bound of each linear interval is handled through stops.
     The end of intervals is thus pushed into stops prior to calling
     aux_linear.
  *)
  let aux_linear worklist program jumps stops =
    let open Dba_types in
    let is_stop instruction =
      let vaddr = instruction.Disasm_types.Instruction.address in
      Caddress.Set.mem (Virtual_address.to_code_address vaddr) stops in
    let step program worklist instruction succs =
      assert (Virtual_address.Set.cardinal succs <= 1);
      if is_stop instruction then
        (* If we need stop, we replace the code and the rest in the current
         * instruction *)
        let instruction = Disasm_types.Instruction.(stop instruction.address) in
        on_instructions (add_block instruction) program,
        worklist
      else
        let p =
          find_calls instruction jumps
          |> snd
          |> add_callsites (on_instructions (add_block instruction) program) in
        let w = Disasm_core.W.add_virtual_addresses succs worklist
        in p, w
    in Disasm_core.fold step program worklist

  (* Inelegant solution to a real problem.
     15 bytes is the biggest x86 opcode. Thus it should be enough in the linear
     case to "catch" any computed successor of the upper bound of the desired
     linear interval.
     We always have [cur_address + increment] <= upper_bound + 15
  *)
  let pad_fifteen_bytes from_caddr stops =
    let rec loop increment s =
      if increment = 15 then s
      else
        loop (succ increment)
          Dba_types.Caddress.(Set.add (add_int from_caddr increment) s)
    in loop 0 stops

  let apply ~(byte_wise:bool) parameters =
    let open Infos in
    if byte_wise then Disasm_options.DisassemblyMode.set_byte_wise_linear ()
    else Disasm_options.DisassemblyMode.set_linear ();
    let jmps = parameters.jumps in
    let stops = parameters.stops in
    let g program (start_addr, end_addr) =
      let stops = pad_fifteen_bytes (Dba_types.Virtual_address.to_code_address end_addr) stops in
      let worklist = Disasm_core.W.singleton start_addr in
      aux_linear worklist program jmps stops in
    List.fold_left g Program.empty parameters.linear_addresses
end

let disassemble_section ?(program=Program.empty) section_name =
  let open Dba_types in
  let img = Loader_utils.get_img () in
  let known = Hashtbl.create 7 in
  let sec_start, sec_end = Loader_utils.section_slice section_name img in
  Logger.debug "Disassembling section %s : [0x%x -- 0x%x]"
              section_name (sec_start:>int) (sec_end:>int);
  let sec_end = Virtual_address.create sec_end in
  let sec_start = Virtual_address.create sec_start in
  Disasm_core.fold
      (fun program wl instruction succs ->
        let p = on_instructions (add_block instruction) program in
        Hashtbl.add known instruction.Disasm_types.Instruction.address ();
        let valid_successors =
          Dba_types.Virtual_address.Set.filter
            (fun vaddr ->
              not (Hashtbl.mem known vaddr)
              && (vaddr:>int) < (sec_end:>int))
            succs
        in
        p, Disasm_core.W.add_virtual_addresses valid_successors wl
      )
  program
  (Disasm_core.W.singleton sec_start)
  

let disassemble_sections () =
  assert (Disasm_options.Sections.is_set ());
  Disasm_options.DisassemblyMode.set_linear (); (* force linear mode *)
  let sections = Disasm_options.Sections.get () in
  
  Basic_types.String.Set.fold
    (fun section_name program ->
      try disassemble_section ~program section_name
      with Not_found ->
        Disasm_options.Logger.warning "Skipping unknown section %s" section_name;
        program
    ) sections Program.empty

   
let pp_mode ppf = function
  | Disasm_options.DisassemblyMode.Recursive ->
    Format.fprintf ppf "recursive"
  | Disasm_options.DisassemblyMode.ExtendedLinear ->
    Format.fprintf ppf "extended linear"
  | Disasm_options.DisassemblyMode.Linear ->
    Format.fprintf ppf "linear"
  | Disasm_options.DisassemblyMode.Linear_byte_wise ->
    Format.fprintf ppf "linear byte wise"


let disassemble parameters =
  let open Disasm_options in
  if Sections.is_set () then disassemble_sections ()
  else 
    
  let dba_file = DbaOutputFile.get ()
  and opcode_file =
    if OpcodeOutputFile.is_set () then OpcodeOutputFile.get () else "stdout" in
  Disasm_options.Logger.debug "Disassembling mode %a (dba file=%s, opcode file=%s)"
    pp_mode (DisassemblyMode.get ()) dba_file opcode_file;
  let disassembler =
    match DisassemblyMode.get () with
    | DisassemblyMode.Recursive -> Recursive.apply
    | DisassemblyMode.ExtendedLinear -> Extended_linear.apply
    | DisassemblyMode.Linear -> Linear.apply ~byte_wise:false
    | DisassemblyMode.Linear_byte_wise -> Linear.apply ~byte_wise:true
  in disassembler parameters


let run ~configuration_file () =
  let open Disasm_options in
  let parameters = Binsec_utils.read_optional_config_file configuration_file in
  let program = disassemble parameters in
  (* let simplified_program =
     on_instructions Simplification_dba.simplify_dba program in *)
  if OpcodeOutputFile.is_set () then
    Print_utils.pp_to_file ~filename:(OpcodeOutputFile.get ())
                           Program.pp program 
  else 
    Logger.result "@[<v 0>Program@ %a@]" Program.pp program;
  if ShowInstructionCount.get () then
    Logger.result "@[%a@]" Program.pp_mnemonic_summary program;
  Print_utils.pp_to_file
    ~filename:(DbaOutputFile.get ())
    Program.pp_dba program 


(* Other functionalities *)
let custom_pp_dbainstrs opc ppf dba_block =
  let open Dba_printer.EICUnicode in
  let open Dba_types in
  let spaces = String.make (String.length opc) ' ' in
  pp_set_margin ppf 250;
  fprintf ppf "@[";
  let mypp i ppf instr = fprintf ppf "@[<h>%2d: %a@]" i pp_instruction instr in
  begin
    match Dba_types.Block.length dba_block with
    | 0 -> ()
    | 1 ->
      fprintf ppf "@[<h>%s → %a@]"
        opc
        pp_instruction (Block.get dba_block 0)
    | 2 ->
      let dbainstr1 = Block.get dba_block 0
      and dbainstr2 = Block.get dba_block 1 in
      fprintf ppf "@[<v 0> %s ⎧1: %a@  %s ⎩2: %a@ @]"
        opc
        pp_instruction dbainstr1 spaces
        pp_instruction dbainstr2

    | nelts ->
      let middle = nelts / 2 in
      let pp_bar fmt i =
        if i = middle then fprintf fmt "%s ⎨" opc
        else fprintf fmt "%s ⎪" spaces
      in
      let rec aux i =
        let e = Block.get dba_block i in
        if i = 0 then begin
          fprintf ppf "@[<v 0>@[<h>%s ⎧%a@]@ " spaces (mypp i) e;
          aux 1
        end
        else if i = nelts - 1 then
          fprintf ppf "@[<h>%s ⎩%a@]@]" spaces (mypp i) e
        else begin
          fprintf ppf "@[<h>%a%a@]@ " pp_bar i (mypp i) e;
          aux (i + 1)
        end
      in aux 0
  end;
  fprintf ppf "@]@."


let decode raw =
  try
    let opc, dba_block = Decode_utils.decode_hex_opcode raw in
    Logger.result "%a" (custom_pp_dbainstrs opc) dba_block;
    exit 0
  with X86toDba.InstructionUnhandled s ->
    Logger.warning "Not decoded %s" s;
    exit 1

let decode_llvm raw =
  try
    let _opc, dba_block = Decode_utils.decode_hex_opcode raw in
    Logger.result "%a" Llvm_decoder.pretty dba_block;
    exit 0
  with X86toDba.InstructionUnhandled s ->
    Logger.warning "Not decoded %s" s;
    exit 1

