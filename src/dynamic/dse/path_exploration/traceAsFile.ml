(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
(*    VERIMAG                                                             *)
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
(**************************************************************************)

open Dba
open TypeTraceDSE
open Conf_exploration
open Libcall_piqi
open Libcall_t
open Strcmp_t
open TracesToTree

module TraceAsFile : TypeTraceDSE =
struct

  (* trace from trace_type + useful info *)
  type trace_inst_next ={
    inst : Trace_type.trace_inst;
    it : int;
    is_loop : bool ;
    mutable call_stack : Int64.t list ;
  }

  type trace_t = {
    name : string ;
    conf : Conf_exploration.conf_t ;
    pinret : int ;
    mutable trace_raw : (trace_inst_next Trace_type.InstrMap.t) option;
    mutable call_stack_computed : bool }

  type child_t = {
    control      : control_t;
    location     : location_t;
    trace        : trace_t ; }

  (*****************************************************************************************)
  (* functions to access low-level representation of trace, trace configuration, and input *)
  (*****************************************************************************************)

  let name_of_trace trace = trace.name
  let config_of_trace trace = trace.conf
  let tracer_ret trace = trace.pinret

  let control_of_child (child:child_t) = child.control
  let location_of_child (child:child_t) = child.location
  let trace_of_child (child:child_t) = child.trace

  let pp_location l = Exploration_type.pp_location l

  let pp_control c =  match c with
    | ConJump l -> Printf.sprintf "Cond(%s)" (pp_location l)
    | DynJump l -> Printf.sprintf "Dyn(%s)" (pp_location l)

  let pp_child child =
    Printf.sprintf "%s: %s, %s"
      (child.trace.name) (pp_location child.location) (pp_control child.control)

  let _pp_cs cs = String.concat "-" (List.map (fun x -> Printf.sprintf "%Lx" x) cs)

  (*low addr first,, low iteration *)
  let compare_child (c1:child_t) (c2:child_t) =
    let l1,l2 = c1.location,c2.location in
    match l1,l2 with
    | Exploration_type.One_loc (addr1,it1), Exploration_type.One_loc (addr2,it2) ->
      begin
        match Int64.compare addr1 addr2 with
        | 0 ->
          begin
            match compare it2 it1 with
            | 0 -> compare c1.trace.name c2.trace.name
            | v -> v
          end
        | v -> v
      end
    | Exploration_type.All_loc(addra),Exploration_type.All_loc(addrb) -> Int64.compare addra addrb
    | Exploration_type.One_loc _ , _ | Exploration_type.All_loc _ , _ -> failwith "Error in child comparison\n"
  (*******************************************************************************)

  (*******************************************************************************)

  let loop_heuristic counter =
    let counter = Hashtbl.fold (fun key elem l -> (key,elem)::l) counter [] in
    let counter = List.sort (fun x y -> compare (snd x) (snd y)) counter in
    if (snd (List.hd (List.rev counter)) < 20) then []
    else
      let log_average = (float(List.fold_left (fun res x ->  res + (snd x) ) 0 counter ) )/. (float (List.length counter)) in
      List.map (fun x -> fst x ) (List.filter (fun x -> (float(snd x)) > (log_average)) counter)

  let clean trace =
    trace.call_stack_computed <- false;
    trace.trace_raw <- None

  let get_raw_trace trace =
    match trace.trace_raw with
    | Some trace_raw ->  trace_raw
    | None ->
      let filename = name_of_trace trace in
      Logger.debug "Get trace %s" filename;
      let ic = open_in_bin filename in
      let t = Trace_loader.load_partial_trace_from_file ic in
      close_in ic;
      let trace_raw = t.Trace_type.instrs in
      let instruction_counter = Hashtbl.create 4000 in
      let trace_raw =
        Trace_type.InstrMap.map
          (fun (x:Trace_type.trace_inst) ->
             let it = try Hashtbl.find instruction_counter x.Trace_type.location with Not_found -> 0 in
             Hashtbl.replace instruction_counter x.Trace_type.location (it + 1);
             {inst=x;it=it;is_loop=false; call_stack=[]}
          ) trace_raw in
      let key_loop = loop_heuristic instruction_counter in
      let trace_raw = Trace_type.InstrMap.map (fun elem ->
          if (List.exists (fun x -> (Int64.compare x elem.inst.Trace_type.location) = 0) key_loop)
          then { inst = elem.inst; it = elem.it; is_loop = true; call_stack = elem.call_stack}
          else elem) trace_raw in
      trace.trace_raw <- Some trace_raw;
      trace_raw

  let get_loop_instr trace =
    let trace_raw = get_raw_trace trace in
    let loop_inst = Trace_type.InstrMap.filter (fun _ x -> x.is_loop) trace_raw in
    Trace_type.InstrMap.fold (fun _ x l-> Exploration_type.All_loc(x.inst.Trace_type.location)::l) loop_inst []

  let get_conditional_loop_instr trace =
    let trace_raw = get_raw_trace trace in
    (*    let loop_inst = Trace_type.InstrMap.filter (fun key x -> x.is_loop) trace_raw in*) (* TODO should use it later ? *)
    let conditional_loop_inst = Trace_type.InstrMap.filter (fun _ x -> InstParsing.is_cond_jump x.inst) trace_raw in
    Trace_type.InstrMap.fold (fun _ x l-> Exploration_type.One_loc(x.inst.Trace_type.location,x.it)::l) conditional_loop_inst []

  let strcmp_vals inst =
    (* TODO this works only for jasper *)
    let addr_rodata = 0x808c5aL in
    let addr_rodata_end = 0x8090D4FL in
    let addr_data = 0x809a0e0L in
    let addr_data_end = 0x809C340L in
    let is_cst x =
      Int64.compare x addr_rodata > 0 && Int64.compare x addr_rodata_end < 0 ||
      Int64.compare x addr_data > 0 && Int64.compare x addr_data_end < 0
    in
    try
      let l =
        List.find
          (fun x -> match x with
            | Trace_type.Libcall l when l.ident  = `strcmp -> true
            | Trace_type.Libcall _
            | Trace_type.NextAddr _
            | Trace_type.RegRead _
            | Trace_type.RegWrite _
            | Trace_type.Syscall _
            | Trace_type.MemLoad _
            | Trace_type.MemStore _
            | Trace_type.Not_retrieved
            | Trace_type.Comment _
            | Trace_type.Wave _ -> false
        ) inst.Trace_type.concrete_infos in
      match l with
      | Trace_type.Libcall l when l.ident = `strcmp ->
        let strcmp = l.strcmp in
        begin
          match strcmp with
          | None -> []
          | Some strcmp ->
            let ret = if (is_cst strcmp.s1) then [strcmp.size_max_s1] else [] in
            if (is_cst strcmp.s2) then strcmp.size_max_s2::ret else ret
        end
      | Trace_type.Libcall _
      | Trace_type.NextAddr _
      | Trace_type.RegRead _
      | Trace_type.RegWrite _
      | Trace_type.Syscall _
      | Trace_type.MemLoad _
      | Trace_type.MemStore _
      | Trace_type.Not_retrieved
      | Trace_type.Comment _
      | Trace_type.Wave _ -> assert false

    with
      Not_found -> []

  let is_strcmp (inst:trace_inst_next) =
    let open Trace_type in
    let cc = inst.inst.concrete_infos in
    List.exists (fun x -> match x with
        | Libcall l when l.ident = `strcmp -> true
        | (NextAddr _|RegRead (_, _)|RegWrite (_, _)|Syscall _|MemLoad (_, _)
          | MemStore (_, _)|Not_retrieved|Comment _|Wave _) | Libcall _ -> false
      ) cc

  let find_instr_strcmp trace =
    let raw_trace = get_raw_trace trace in
    let raw_trace_bindings = Trace_type.InstrMap.bindings raw_trace in
    let raw_trace_bindings = List.map (fun (_,(x:trace_inst_next)) -> x ) raw_trace_bindings in
    let rec explore insts counter acc =
      match insts with
      | [] -> acc
      | hd::tl -> if(counter=0) then explore tl (-1) (hd::acc)
        else
        if(counter>0) then explore tl (counter-1) acc
        else
        if (is_strcmp hd) then explore tl  1 acc
        else explore tl (-1) acc
    in explore raw_trace_bindings (-1) []

  let strcmp_heuristic trace =
    let raw_trace = get_raw_trace trace in
    Trace_type.InstrMap.fold (fun _ x l ->
        match strcmp_vals x.inst with
        | [] -> l
        | vals -> (List.map (fun s -> (Exploration_type.All_loc(x.inst.Trace_type.location),s)) vals)@l
      ) raw_trace []

  let stack_to_list s =
    if Stack.is_empty s then [(-1L)]
    else begin
      let l = ref [] in
      Stack.iter (fun x -> l := x :: !l) s;
      !l
    end

  let rec compare_cs cs1 cs2 =
    match cs1,cs2 with
    | [] , [] -> 0
    | _ , [] | [] , _ -> 1
    | x::tl1, y::tl2 -> if (Int64.compare x y) = 0 then compare_cs tl1 tl2
      else 1

  let compute_call_stack trace =
    let is_libcall inst =
      let open Trace_type in
      List.exists (
        fun x -> match x with
        | Libcall _ -> true
        | NextAddr _|RegRead (_, _)|RegWrite (_, _)|Syscall _|MemLoad (_, _)| MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> false
      ) inst.concrete_infos in
    let trace_raw = get_raw_trace trace in
    let call_stack = Stack.create () in
    Stack.push 0L call_stack;
    let rec explore l_trace_inst =
      match l_trace_inst with
      | [] -> ()
      | (_, hd) :: tl ->
        hd.call_stack <- stack_to_list call_stack;
        let called = InstParsing.is_call hd.inst in
        if called && not (is_libcall hd.inst)
        then Stack.push hd.inst.Trace_type.location call_stack;
        let ret = InstParsing.is_ret hd.inst in
        let _ = try
            if (ret) then let _ = Stack.pop call_stack in ()
          with  Stack.Empty -> Logger.debug "############## Are you out ? \n"
        in
        explore tl
    in
    explore (Trace_type.InstrMap.bindings trace_raw);
    trace.call_stack_computed <- true


  let group f l =
    let rec grouping acc x=
      match x with
      | [] -> acc
      | hd::tl ->
        let l1,l2 = List.partition (f hd) tl in
        grouping ((hd::l1)::acc) l2
    in grouping [] l

  let same_callsite a b = (compare_cs (snd a).call_stack (snd b).call_stack) = 0
  let same_addr a b = (Int64.compare (snd a).inst.Trace_type.location (snd b).inst.Trace_type.location) = 0

  let get_instr_per_call_site trace =
    if not trace.call_stack_computed then compute_call_stack trace;
    let raw_trace = get_raw_trace trace in
    group same_callsite (Trace_type.InstrMap.bindings raw_trace)

  let rec extract_loop_it inst counter l_loc_size acc =
    match inst with
    | [] -> acc
    | hd::tl ->
      let new_elem = List.fold_left (fun l (loc,x) ->
          let l = if(Int64.compare counter x)=0 then (loc,hd)::l else l in
          if(Int64.compare counter (Int64.add 1L x))=0 then (loc,hd)::l else l
        ) [] l_loc_size
      in
      extract_loop_it tl (Int64.add counter 1L) l_loc_size new_elem@acc


  let get_child_loop instrs l_loc_size =
    let cond_loop_inst = List.filter (fun (_,x) -> InstParsing.is_cond_jump x.inst) instrs in
    let inst_by_addr = group same_addr cond_loop_inst in
    let inst_by_addr_ord = List.map (fun elem -> List.sort (fun (_,x) (_,y) -> compare x.it y.it) elem ) inst_by_addr in
    List.concat (List.map (fun x -> extract_loop_it x 0L l_loc_size []) inst_by_addr_ord)

  let get_instr_by_loc locs trace =
    let raw_trace = get_raw_trace trace in
    let raw_trace_bindings = Trace_type.InstrMap.bindings raw_trace in
    let raw_trace_bindings = List.map (fun (_,x) -> x ) raw_trace_bindings in
    List.filter ( fun x ->
        List.exists (fun loc ->
            match loc with
            | Exploration_type.One_loc (addr,it) -> ((Int64.compare addr x.inst.Trace_type.location) = 0) && ((compare it x.it) = 0)
            | Exploration_type.All_loc(_) -> false
          ) locs
      ) raw_trace_bindings

  let compare_loc a b =
    match a,b with
    | Exploration_type.One_loc(addra,ita),Exploration_type.One_loc(addrb,itb) ->
      begin
        match (Int64.compare addra addrb) with
        | 0 -> compare ita itb
        | v -> v
      end
    | Exploration_type.All_loc(addra),Exploration_type.All_loc(addrb) -> Int64.compare addra addrb
    | Exploration_type.All_loc(_),_ | _,Exploration_type.All_loc(_) -> failwith "Error during comparison loc \n"

  let build_cond_invert x =
    let rec explore dbas =
      match dbas with
      | [] -> failwith "Try to invert no condition that can be inverted \n"
      | dba::tl ->
        match  Dba_types.Statement.instruction dba with
        | IkIf(_, JOuter addr,_) -> addr.Dba.base
        | (IkAssign (_, _, _)|IkSJump (_, _)|IkDJump (_, _)|IkStop _|IkAssert (_, _)|IkIf (_, JInner _, _) |
           IkAssume (_, _)|IkNondetAssume (_, _, _)|IkNondet (_, _, _)|IkUndef (_, _)|
           IkMalloc (_, _, _)|IkFree (_, _)|IkPrint (_, _)) -> explore tl
    in
    let addr = explore x.inst.Trace_type.dbainstrs in
    let addr = Bitvector.value_of addr in
    let next_addr = Trace_type.(get_next_address x.inst.concrete_infos) in
    if(Bigint.compare_big_int addr (Bigint.big_int_of_int64 next_addr)) = 0 then
      ConJump(Exploration_type.One_loc(Int64.add x.inst.Trace_type.location 2L,x.it))
    else
      ConJump(Exploration_type.One_loc(Bigint.int64_of_big_int addr,x.it)) (* Todo do something cleaner *)

  let generate_child_in_loop trace l_loc_size=
    let instr_per_call_site = get_instr_per_call_site trace in
    let new_loc = List.concat (List.map (fun x -> get_child_loop x l_loc_size) instr_per_call_site) in
    let new_child_loop = List.map (fun (_,x) -> {trace=trace; control = build_cond_invert (snd x);  location = Exploration_type.One_loc((snd x).inst.Trace_type.location,(snd x).it)} ) new_loc in
    let uniq_loc_strcmp = List.sort_uniq compare_loc (List.map (fun (x,_) -> x ) l_loc_size) in
    let new_child_strcmp = get_instr_by_loc uniq_loc_strcmp trace in
    let new_child_loc = List.map (fun x -> {trace=trace; control = build_cond_invert x;  location = Exploration_type.One_loc(x.inst.Trace_type.location,x.it)} ) new_child_strcmp in
    new_child_loop,new_child_loc

  let generate_child_in_strcmp trace =
    let instr_strcmp = find_instr_strcmp  trace in
    List.map (fun x -> {trace=trace; control = build_cond_invert x;  location = Exploration_type.One_loc(x.inst.Trace_type.location,x.it)} ) instr_strcmp



  let call_stack_of_child child =
    let trace = child.trace in
    if not trace.call_stack_computed then compute_call_stack trace;
    let addr, it =
      match child.location with
      | Exploration_type.One_loc (addr, it) -> addr, it
      | Exploration_type.All_loc _ -> failwith "Call stack of child with multiple locations\n"
    in
    let raw_trace = get_raw_trace trace in
    let raw_trace_bindings = Trace_type.InstrMap.bindings raw_trace in
    let _, inst =
      List.find
        (fun (_,x) -> x.inst.Trace_type.location = addr && x.it = it)
        raw_trace_bindings
    in inst.call_stack

  let trace_to_list_addr trace =
    Printf.printf "Trace to list\n";
    let trace_raw = get_raw_trace trace in
    Printf.printf "size %d\n" (Trace_type.InstrMap.cardinal trace_raw);
    compute_call_stack trace;
    let prev_was_jmp = ref false in
    let inst_jmp =
      Trace_type.InstrMap.mapi
        (fun key elem ->
           let inst = elem.inst in
           if key =0 then (true, elem, NODE_EIP)
           else
             match (InstParsing.is_cond_jump inst,InstParsing.is_call inst,InstParsing.is_ret inst,!prev_was_jmp) with
             | (true,_,_,_) ->
               prev_was_jmp := true;
               true, elem, TracesToTree.NODE_JMP
             | (_,true,_,_) ->
               prev_was_jmp := false;
               let t = match (InstParsing.is_libcall inst) with
                 | true -> TracesToTree.NODE_LIBCALL
                 | false -> TracesToTree.NODE_CALL in (true, elem, t)
             | (_, _, true, _) ->
               prev_was_jmp := false;
               true, elem, TracesToTree.NODE_RET
             | (false, _, _, true) ->
               prev_was_jmp := false;
               true, elem, TracesToTree.NODE_OTHER
             | (false,_,_,false) -> (false,elem,TracesToTree.NODE_OTHER))
        trace_raw
    in
    Some (
      List.rev (
        Trace_type.InstrMap.fold
          (fun _ (keep, elem, t) l ->
             if keep then (elem.inst.Trace_type.location, t, elem.call_stack) :: l
             else l)
          inst_jmp []))

  let find_instr_cs trace list_addr_cs =
    if not trace.call_stack_computed then compute_call_stack trace;
    let trace_raw = get_raw_trace trace in
    List.filter
      (fun (x,cs) ->
         Trace_type.InstrMap.exists
           (fun _ y ->
              Int64.compare y.inst.Trace_type.location x =0 &&
              compare_cs y.call_stack cs = 0)
           trace_raw)
      list_addr_cs

  let exists_instr_cs trace list_addr_cs =
    if not trace.call_stack_computed then compute_call_stack trace;
    let trace_raw = get_raw_trace trace in
    List.exists (fun (x,cs) ->
        Trace_type.InstrMap.exists (fun _ y ->
            (Int64.compare y.inst.Trace_type.location x)=0 && (compare_cs y.call_stack cs)=0)
          trace_raw)
      list_addr_cs

  let split_trace_uaf trace alloc free use =
    if not trace.call_stack_computed then compute_call_stack trace;
    let trace_raw = get_raw_trace trace in
    let state = ref 0 in
    let exists l y  =
      List.exists (fun (x, cs) ->
          y.inst.Trace_type.location = x
          && compare_cs y.call_stack cs = 0)
        l in
    let trace_raw =
      Trace_type.InstrMap.map (fun y ->
          let list_addr = match !state with | 0 -> alloc | 1 -> free | _ -> use in
          if exists list_addr y then
            state := (match !state with | 0 -> 1 | 1 -> 2 | _ -> 3);
          (!state,y)
        ) trace_raw in
    let f (y:int)=  Trace_type.InstrMap.filter (fun _ (x,_) -> x=y) trace_raw in
    let g l =  Trace_type.InstrMap.map (fun (_,y) -> y) l in
    g (f 0), g (f 1),g (f 2)
  (*******************************************************************************)

  let get_trace (program_name) (program_args) (trace_config) trace_max_length =
    let args_list = Str.split (Str.regexp "[ \t]+") program_args in
    let conf_name = Conf_exploration.get_conf_name trace_config in
    let conf_basename = Filename.basename conf_name
    and conf_dirname = Filename.dirname conf_name in
    (* let trace_file      = "trace_" ^ (Printf.sprintf "%s" (Conf_exploration.get_conf_name trace_config )) ^ ".out" in *)
    let trace_file = Filename.concat conf_dirname "trace_" ^ conf_basename ^ ".out" in
    let log_file = program_name ^ ".log" in
    let log_file_descr  = Unix.openfile log_file [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666 in
    let pindir = Sys.getenv "PIN" in
    let pinsec = Sys.getenv "PINSEC" in
    let instrument_args =
      Array.append [| "-ifeellucky";
                      "-injection"; "child";
                      "-t"; pinsec;
                      "-config"; (Conf_exploration.get_conf_name trace_config);
                      "-out"; trace_file;
                      "-maxlength"; (string_of_int trace_max_length);
                      "-chunk-size"; "10000000";
                      "--"; program_name |] (Array.of_list (args_list))
    in
    let instrument_cmd = String.concat " " (Array.to_list instrument_args) in
    let conf_name = (Conf_exploration.get_conf_name trace_config) in
    List.iter (fun (_,name) ->
        let _ = Sys.command (Printf.sprintf "cp %s_input_%s %s" name conf_name name ) in
        ()
      ) (FdInput.list_fd ());
    let program_pid = Unix.create_process pindir instrument_args Unix.stdin log_file_descr log_file_descr
    in
    (
      Logger.debug "program name: %s\n" program_name;
      Logger.debug "instrumenting command: %s\n" instrument_cmd;
      Logger.debug "instrumented process id: %d\n" program_pid;
      flush_all ();
      Printf.printf "Test\n";

      let _, proc_status = Unix.waitpid [] program_pid in
      (
        Unix.close log_file_descr;
        match proc_status with
        | Unix.WEXITED ret_code ->
          (
            Logger.debug "Pintool exited with %d code\n" ret_code;
            { name      = trace_file;
              conf      = trace_config;
              pinret    = ret_code;
              trace_raw = None;
              call_stack_computed  = false };
          )
        | Unix.WSIGNALED _|Unix.WSTOPPED _ -> failwith "cannot generate trace"
      )
    )

  (* Error here, it of next_addr can be != it cur inst *)
  let inst_to_control_and_location (inst,it) =
    let next_addr = Trace_type.(get_next_address inst.concrete_infos) in
    let control = match (InstParsing.is_cond_jump inst) with
      | true -> ConJump (Exploration_type.One_loc(next_addr,it))
      | false ->
        match ( InstParsing.is_dyn_jump inst) with
        | true -> DynJump (Exploration_type.One_loc(next_addr,it))
        | false -> failwith "Should not happend !\n"
    in
    let location = Exploration_type.One_loc(inst.Trace_type.location,it) in
    (control,location)

  let get_all_cond_instr_raw raw_trace =
    let all_cond =
      Trace_type.InstrMap.filter (fun _key elem -> InstParsing.is_cond_jump elem.inst ) raw_trace in
    let all_cond = Trace_type.InstrMap.fold (fun _ elem l ->
        l@[(elem.inst,elem.it)]
      ) all_cond [] in
    List.map (fun x -> inst_to_control_and_location x) all_cond

  let get_all_cond_instr trace =
    let raw_trace = get_raw_trace trace in
    get_all_cond_instr_raw raw_trace


  let get_all_cond_instr_uaf trace alloc free use=
    let raw_trace_alloc,raw_trace_free,raw_trace_use = split_trace_uaf trace alloc free use in
    (get_all_cond_instr_raw raw_trace_alloc, get_all_cond_instr_raw raw_trace_free, get_all_cond_instr_raw raw_trace_use)


  let export_children (control,location) trace =
    {control=control;location=location;trace=trace}

  let filter cond_instr previous =
    let previous_saw = ref false in
    let ret =
      match previous with
      | Exploration_type.All_loc(_) -> failwith "Should not happend ? (filter traceasfilenext)"
      | Exploration_type.One_loc(prev_addr,prev_it) ->
        List.filter (fun (_,loc) ->
            match loc with
            | Exploration_type.All_loc(_) -> failwith "Should not happend ? (filter traceasfilenext)"
            | Exploration_type.One_loc(addr,it) ->
              if !previous_saw then true
              else if addr = prev_addr && it = prev_it then
                (previous_saw := true; false)
              else false
          ) cond_instr
    in (ret,!previous_saw)

  let get_children previous trace =
    let cond_instr = get_all_cond_instr trace in
    let cond_instr =
      match previous with
      | None -> cond_instr
      | Some previous -> fst (filter cond_instr previous) in
    List.map (fun x -> export_children x trace) cond_instr

  (* Should do something more generic, as a list of list of addr *)
  let get_children_uaf previous trace alloc free use =
    let (cond_instr_alloc,cond_instr_free,cond_instr_use) = get_all_cond_instr_uaf trace alloc free use in
    let () = Printf.printf "Size free %d usee %d (%d) \n" (List.length cond_instr_free) (List.length cond_instr_use) (List.length use) in
    (* remove conds belonging to the  previous trace, need to check if their belong to alloc / free / use part *)
    let f cond_instr already_saw =
      match previous,already_saw with
      | _ , true -> cond_instr,true
      | None, false -> cond_instr,true
      | Some previous,false -> filter cond_instr previous in
    let cond_instr_alloc,already_saw = f cond_instr_alloc false in
    let cond_instr_free,already_saw = f cond_instr_free already_saw in
    let cond_instr_use,_ = f cond_instr_use already_saw in
    let m cond_instr = List.map (fun x -> export_children x trace) cond_instr in
    m cond_instr_alloc, m cond_instr_free, m cond_instr_use

  let policy_cc () =
    ["* :: * :: esp :: * => Pc";
     "* :: * :: ebp :: * => Pc";
     "* :: @[?a] := _ :: !a :: * => C";
     "* :: _ := ?e    :: @[!$$] <: !e :: * => C";
     "* :: goto ?e    :: @[!$$] <: !e :: * => C";
     "default => P"]

  let config_to_cc (c:Config_piqi.configuration) =
    { c with Config_piqi.Configuration.policy = policy_cc () }


  let get_inputs check_init (child:child_t) polcc =
    let trace = trace_of_child child in
    let tracename = name_of_trace trace in
    let trace_config = config_of_trace trace in
    let config =
      Conf_exploration.build_analysis_configuration tracename trace_config in
    if polcc
    then
      config.Options.configuration <- config_to_cc config.Options.configuration;
    let trace_analyzer = new InvertChild.invert_child config in
    trace_analyzer#set_counter (get_conf_id trace_config);
    match child.location with
    | Exploration_type.One_loc (address, iteration) ->
      trace_analyzer#add_location_it_to_invert address iteration;
      trace_analyzer#init_entries ();
      if check_init then
        trace_analyzer#set_check_init (Conf_exploration.get_conf_name trace_config);
      let _ret = trace_analyzer#compute in
      let new_conf_files = trace_analyzer#get_new_conf_files() in
      List.map (fun x -> (x, Conf_exploration.counter_conf())) new_conf_files

    | Exploration_type.All_loc _ -> failwith "Child has wrong location type"



  (****************************************************************************)
  let build_analysis_configuration trace =
    Conf_exploration.build_analysis_configuration
      (name_of_trace trace) (config_of_trace trace)

end
