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
open Path_pred_env
open Config_piqi
open Configuration
open SymbolicInput
open InitMemInput
open Trace_type
open Options
open Smtlib2
open Libcall_piqi
open Common_piqi
open Libcall_t
open Input_t
open Memory_t
open Exploration_type

exception SHOULD_INIT
exception TIMEOUT_SOLVER


class invert_child (trace_config:Options.trace_analysis_config) =
  object(self) inherit Path_predicate.dse_analysis trace_config

    (* List of target to invert, can be location (addr), ou key (nth instruction) *)
    val mutable list_location_invert = []
    val mutable list_inst_key_invert = []

    val mutable new_conf_files = []

    val input_from_files= InputFromFiles.init ()

    val mutable check_init = ref false
    val mutable trace_config_name = ref ""

    method get_new_conf_files () = new_conf_files

    method set_check_init name =
      check_init := true;
      trace_config_name := name

    val mutable eip = 0L;

    val mutable counter = 0
    method set_counter c = counter <- c

    (* use as description of the trace, we construct the name with conditionals instructions seen *)
    val mutable name_trace = ""

    val instruction_counter = Hashtbl.create 4000 (* use to compute iteration*)

    val mutable input_entries = SymbolicInput.init ()
    val init_mem = InitMemInput.parse_inputs `conc trace_config.configuration.inputs

    method init_entries () = input_entries <- SymbolicInput.parse_inputs `patch trace_config.configuration.inputs
    method change_entries entries = input_entries <- entries

    method add_location_to_invert (addr:int64)  =
      list_location_invert <- All_loc (addr) :: list_location_invert (* TODO : should handle iteration *)

    method add_location_it_to_invert (addr:int64) (it:int) =
      list_location_invert <- One_loc (addr, it) :: list_location_invert (* TODO : should handle iteration *)

    method add_inst_key_to_invert (n:int) = list_inst_key_invert <- n :: list_inst_key_invert

    (* should rewrite these check *)
    method private check_fread cc =
        let open Fread_t in
        let libcall = List.filter (fun x -> match x with
                                         | Libcall _  -> true
                                         | NextAddr _|RegRead (_, _)|RegWrite (_, _)|Syscall _|MemLoad (_, _)|
                                            MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> false ) cc
        in
        let fread = List.find (fun l -> match l with 
                                            | Libcall l -> l.ident = `fread
                                            | NextAddr _|RegRead (_, _)|RegWrite (_, _)|Syscall _|MemLoad (_, _)|
                                               MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> false ) libcall in
        let fread = match fread with
                | Libcall fread -> fread
                | NextAddr _|RegRead (_, _)|RegWrite (_, _)|Syscall _|MemLoad (_, _)|
                  MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> failwith "Error libmatch check read  (childanalysis)"
        in
        let fread_t = match fread.fread with
                    | Some a -> a
                    | None -> failwith "Error check read (childanalysis)"
        in
        fread_t.ptr.addr,fread_t.size,fread_t.nmemb,fread_t.stream,fread_t.ret

    method check_read cc =
	try 
		let ptr, size, nmemb, stream, ret = self#check_fread cc in
		ptr, Int64.mul size nmemb,ret,stream
	with	
		_ ->
		let open Read_t in
		let libcall = List.filter (fun x -> match x with
						 | Libcall _  -> true
						 | NextAddr _|RegRead (_, _)|RegWrite (_, _)|Syscall _|MemLoad (_, _)|
						    MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> false ) cc 
		in
		let read = List.find (fun l -> match l with 
						    | Libcall l -> l.ident = `read
						    | NextAddr _|RegRead (_, _)|RegWrite (_, _)|Syscall _|MemLoad (_, _)|
						       MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> false ) libcall in
		let read = match read with
			| Libcall read -> read
			| NextAddr _|RegRead (_, _)|RegWrite (_, _)|Syscall _|MemLoad (_, _)|
			  MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> failwith "Error libmatch check read  (childanalysis)"
		in
		let read_t = match read.read with
			    | Some a -> a
			    | None -> failwith "Error check read (childanalysis)" 
		in
		read_t.buf.addr,read_t.count,read_t.ret,read_t.fd



    method check_mmap cc =
        let open Mmap_t in
        let libcall = List.filter (fun x -> match x with
                                         | Libcall _  -> true
                                         | NextAddr _|RegRead (_, _)|RegWrite (_, _)|Syscall _|MemLoad (_, _)|
                                            MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> false ) cc
        in
        let mmap = List.find (fun l -> match l with
                                            | Libcall l -> l.ident = `mmap
                                            | NextAddr _|RegRead (_, _)|RegWrite (_, _)|Syscall _|MemLoad (_, _)|
                                               MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> false ) libcall in
        let mmap = match mmap with
                | Libcall mmap -> mmap
                | NextAddr _|RegRead (_, _)|RegWrite (_, _)|Syscall _|MemLoad (_, _)|
                  MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> failwith "Error libmatch check mmap  (childanalysis)"
        in
        let mmap_t = match mmap.mmap with
                    | Some a -> a
                    | None -> failwith "Error check mmap (childanalysis)"
        in
        mmap_t.ret.Memory_t.addr,mmap_t.length,mmap_t.fd

    method check_fscanf cc =
        let open Fscanf_t in
        let libcall = List.filter (fun x -> match x with
                                         | Libcall _  -> true
                                         | NextAddr _|RegRead (_, _)|RegWrite (_, _)|Syscall _|MemLoad (_, _)|
                                            MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> false ) cc
        in
        let fscanf = List.find (fun l -> match l with
                                            | Libcall l -> l.ident = `fscanf
                                            | NextAddr _|RegRead (_, _)|RegWrite (_, _)|Syscall _|MemLoad (_, _)|
                                               MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> false ) libcall in
        let fscanf = match fscanf with
                | Libcall fscanf -> fscanf
                | NextAddr _|RegRead (_, _)|RegWrite (_, _)|Syscall _|MemLoad (_, _)|
                  MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> failwith "Error libmatch check fscanf (childanalysis)"
        in
        let (fscanf_elem:fscanf_t) = match fscanf.fscanf with
                    | Some a -> a
                    | None -> failwith "Error check fscanf (childanalysis)"
        in
        (List.map (fun (x:fscanf_elem_t) -> x.Fscanf_elem_t.addr ) fscanf_elem.elems),fscanf_elem.ret

    method private add_input_constraint input_entry env is_last =
      if(is_last) then
        let var_name = SymbolicInput.get_input_entry_name input_entry in
        let diff = SmtComp(SmtBvExpr(SmtBvVar(var_name, 8)), SmtBvExpr(SmtBvCst(Bitvector.create (Bigint.big_int_of_int 0x00) 8))) in
        let cond = diff in
        env.formula <- Formula.add_constraint env.formula cond
      else
        let var_name = SymbolicInput.get_input_entry_name input_entry in
        let diff = SmtComp(SmtBvExpr(SmtBvVar(var_name, 8)), SmtBvExpr(SmtBvCst(Bitvector.create (Bigint.big_int_of_int 0x00) 8))) in
        let cond = SmtNot(diff) in
        env.formula <- Formula.add_constraint env.formula cond

    method private add_alias a b env =
        let cond = SmtComp(SmtBvExpr(SmtBvVar(a, 8)), SmtBvExpr(SmtBvVar(b, 8))) in
        env.formula <-  Formula.add_constraint env.formula cond

    method private add_aliases l env =
        List.iter (fun (a,b) -> self#add_alias a b env) l

    method private add_input input_entry env =
      let var_name = SymbolicInput.get_input_entry_name input_entry
      and input_as_reg = SymbolicInput.get_input_entry_register input_entry
      and input_as_mem = SymbolicInput.get_input_entry_mem input_entry
      in
      match input_as_reg, input_as_mem with
      | Some reg_name, None -> Formula_utils.symbolize_register reg_name ~is_full_name:true var_name env
      | None, Some (mem_addr, mem_size) ->
        if (mem_size = 1) then Formula_utils.symbolize_memory_one_octet mem_addr var_name env
        else Logger.debug "try to symbolize mem with s > 1 octet(= %d) " mem_size
      | None, None | Some _ , Some _  -> failwith "malformed input entry"

    method private add_init_reg reg_name concrete_infos env =
      Logger.debug "Try %s" reg_name;
      List.iter (fun con_info ->
          match con_info with
          | RegRead (name, value) when name = reg_name ->
            let fexpr = SmtBvCst(Bitvector.create (Bigint.big_int_of_int64 value) 32) in
            Formula_utils.replace_register reg_name fexpr env
          | MemLoad _ | NextAddr _|RegRead (_, _)|RegWrite (_, _)|Libcall _|Syscall _|
            MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> ()
        ) concrete_infos

    method private add_init_mem mem_addr mem_size concrete_infos env =
      List.iter (fun x ->
          match x with
          | MemLoad (addr, str) when (((Int64.compare addr mem_addr)=0) && mem_size = String.length str) ->
            Formula_utils.replace_memory addr str env
          | RegRead _| NextAddr _|RegWrite (_, _)|Libcall _|Syscall _|MemLoad (_, _)|
            MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> ()
        ) concrete_infos

    method private add_init_values init_entry concrete_infos env =
      let entry_as_reg = InitMemInput.get_input_entry_register init_entry
      and entry_as_mem = InitMemInput.get_input_entry_mem init_entry
      in
      match entry_as_reg, entry_as_mem with
      | Some reg_name, None -> self#add_init_reg reg_name concrete_infos env
      | None, Some (mem_addr, mem_size) -> self#add_init_mem mem_addr mem_size concrete_infos env
      | _, _ -> failwith "malformed input entry"
      (* let () = *)
      (*   match InitMemInput.get_input_entry_register init_entry with *)
      (*   | Some reg_name -> self#add_init_reg reg_name concrete_infos env *)
      (*   | None -> () *)
      (* in *)
      (*   match InitMemInput.get_input_entry_mem init_entry with *)
      (*   | Some (mem_addr, mem_size) -> self#add_init_mem mem_addr mem_size concrete_infos env *)
      (*   | None -> () *)

    (* Update last inputs with new values, keep everything else *)
    (* if new_inputs contains inputs that are not in prev_inputs, add them *)
    method private update_inputs (prev_inputs:Config_piqi.input_t list) (new_inputs:Config_piqi.input_t list) =
        let same_input (x:Config_piqi.input_t) (y:Config_piqi.input_t) = x.typeid = y.typeid && x.address = y.address && x.iteration = y.iteration && x.action = y.action && x.when_ = y.when_ in
        let same_reg (x:Common_piqi.register_t) (y:Common_piqi.register_t) = x.Register_t.name = y.Register_t.name in
        let same_mem (x:Common_piqi.memory_t) (y:Common_piqi.memory_t) = x.addr = y.addr in
        let inputs =List.map (fun (x:Config_piqi.input_t) ->
            (* check if a there is new a value *)
            List.iter (fun (y:Config_piqi.input_t) ->
                match x.reg,y.reg,x.mem,y.mem with
                | Some rx,Some ry, None, None -> if (same_input x y && same_reg rx ry ) then x.reg <- y.reg
                | None,None,Some mx,Some my -> if (same_input x y && same_mem mx my ) then  x.mem <- y.mem
                | _ -> ()
            ) new_inputs;
         x
        ) prev_inputs in
        let missing_inputs = List.filter (fun (x:Config_piqi.input_t) ->
                        not (List.exists (fun (y:Config_piqi.input_t) ->
                                             match x.reg,y.reg,x.mem,y.mem with
                                            | Some rx,Some ry, None, None -> if (same_input x y && same_reg rx ry ) then true else false
                                            | None,None,Some mx,Some my -> if (same_input x y && same_mem mx my ) then true else false
                                            | _ -> false
                                         ) prev_inputs
                        )
        ) new_inputs in
        Logger.debug "Size of prev %d new %d and missing %d"
          (List.length prev_inputs)
          (List.length new_inputs)
          (List.length missing_inputs);
        inputs@missing_inputs

    method! private pre_execution _ = FdInput.reset ()

    method! private visit_dbainstr_before (key:int) (inst:trace_inst) (dbainst:Dba_types.Statement.t) (env:Path_pred_env.t) =
      let addr = inst.location in
      let it = try Hashtbl.find instruction_counter addr with Not_found -> 0 in
      let instruction = Dba_types.Statement.instruction dbainst in
      begin
      match  instruction with
        | IkIf _ | IkDJump _ -> name_trace <- Printf.sprintf "%s_0x%Lx-%d" name_trace addr it
        | (IkAssign (_, _, _)|IkSJump (_, _)|IkStop _|IkAssert (_, _)|
            IkAssume (_, _)|IkNondetAssume (_, _, _)|IkNondet (_, _, _)|
            IkUndef (_, _)|IkMalloc (_, _, _)|IkFree (_, _)|IkPrint (_, _))-> ()
      end;
      (* if inst is a target *)

      if List.exists (fun x ->
          match x with
          | All_loc a -> a = addr
          | One_loc (a, i) -> a = addr && i = it) list_location_invert ||
         List.exists (fun x -> x = key) list_inst_key_invert
      then begin
        match instruction with
        | IkIf (cond, JOuter addr , _) ->
          let address = Dba_types.Caddress.base_value addr in
          let formula_file = "formula-if_" ^ (Printf.sprintf "%04d-%04d-%02d" counter key it) (*Uuidm.to_string @@ Uuidm.create `V4*) ^ ".smt2" in
          (* let pred = self#build_cond_predicate cond env in *)
          let l  = InputFromFiles.get_aliases input_from_files in
          self#add_aliases l env;
          let next_addr = get_next_address inst.concrete_infos in
          let static_predicate = self#build_cond_predicate cond env in
          let predicate =
            if Bigint.compare_big_int address (Bigint.big_int_of_int64 next_addr) = 0
            then (Logger.debug "build negative predicate: %s\n" formula_file; SmtNot(static_predicate))
            else (Logger.debug "build normal predicate: %s\n" formula_file; static_predicate)
          in
          begin
            ignore (Formula.build_formula_file env.formula predicate formula_file trace_config.configuration.solver);
            Logger.debug "Formula build, lets solve";
            let timeout = Int32.to_int trace_config.configuration.timeout  in
            let result, model =
                try 
                        Solver.solve_model ~timeout:timeout formula_file trace_config.configuration.solver 
                with Failure _ -> UNKNOWN, Smt_model.empty
            in
            Logger.debug "Formula solved";
            match result with
            | SAT ->
              Logger.debug "SAT!";
              let new_inputs = SymbolicInput.update input_entries model in
              let should_init_register = Smt_model.registers model in
              let should_init_register = List.filter (fun k -> k = "esp" || k = "ebp" || k = "eax" || k = "ebx"  || k = "ecx" || k = "edx" || k = "esi" || k = "edi" ) should_init_register in
              let should_init_mem = Smt_model.memory_addresses model in
              let config = trace_config.configuration in
              if !check_init && (
                  should_init_register <> [] || should_init_mem <> []) then
                begin
                  Logger.debug
                    "Missed some init values: reg:%d mem:%d"
                    (List.length should_init_register)
                    (List.length should_init_mem);
                  let new_inputs_conc_export_reg =
                    List.map
                      (fun x ->
                         let value = default_register_value_t () in
                         value.Register_value_t.typeid <- `bit32;
                         value.Register_value_t.value_32 <- Some 0l;
                         { typeid = `reg;
                           address = eip;
                           iteration = 0l;
                           when_ = `before;
                           action = `conc;
                           reg = Some {Register_t.name=x; Register_t.value = value;};
                           mem = None;
                           indirect = None;
                         }
                      ) should_init_register in
                  let new_inputs_conc_export_mem =
                    List.map
                      (fun x ->
                         { typeid = `mem;
                           address = eip;
                           iteration = 0l;
                           when_ = `before;
                           action = `conc;
                           reg = None;
                           mem = Some {addr=x; value = "A"};
                           indirect = None;
                         }
                      ) should_init_mem in
                  config.inputs <- self#update_inputs config.inputs new_inputs_conc_export_reg;
                  config.inputs <- self#update_inputs config.inputs new_inputs_conc_export_mem;
                  let new_file = !trace_config_name in
                  Logger.debug "Create new conf files with more inputs %d %s" (List.length config.inputs) new_file;
                  let oc = open_out new_file in
                  let data = Config_piqi_ext.gen_configuration config `json_pretty in
                  output_string oc data;
                  close_out oc;
                  raise SHOULD_INIT
                end;
              let inputs = config.inputs in
              let new_file = "config_"^((*name_trace( *)Printf.sprintf "%04d-%04d-%02d" counter key it)^".json" in
              List.iter (fun (fd,name) ->
                      Logger.debug "Update file %s %Lx" name fd;
                      let vals = InputFromFiles.get_vals input_from_files fd in
                      let vals = List.map (fun (k,x) -> (k,Int64.to_int (SymbolicInput.get_value_of_input new_inputs x))) vals in
                      InputFromFiles.update_file name vals;
                      let _ = Sys.command (Printf.sprintf "cp %s %s_input_%s" name name new_file) in
                      ()
              ) (FdInput.list_fd ()) ;
              config.inputs <- inputs;
              let oc = open_out new_file in
              let data = Config_piqi_ext.gen_configuration config `json_pretty in
              output_string oc data;
              close_out oc;
              new_conf_files <- new_file::new_conf_files

            | UNSAT -> Logger.debug "inversed input of conditional jump at 0x%Lx: UNSAT\n" inst.location;
            | TIMEOUT -> Logger.debug "inversed input of conditional jump at 0x%Lx: TIMEOUT\n" inst.location;
                         raise TIMEOUT_SOLVER
            | UNKNOWN -> Logger.debug "inversed input of conditional jump at 0x%Lx not found (Unknow error)\n" inst.location;
          end
        | IkDJump (_, None) ->
          (
            (* implementation depending on the context *)
          )
        | (IkAssign (_, _, _)|IkSJump (_, _)|IkStop _|IkAssert (_, _)|IkIf (_, JInner _, _ ) |
           IkAssume (_, _)|IkNondetAssume (_, _, _)|IkNondet (_, _, _)|IkDJump (_, Some _) |
           IkUndef (_, _)|IkMalloc (_, _, _)|IkFree (_, _)|IkPrint (_, _)) -> ()
      end;
      Path_predicate.DoExec


    method! private visit_instr_before (key:int) (inst:trace_inst) (env:Path_pred_env.t) =
      if key = 0 then eip <- inst.location;
      let addr = inst.location in
      let it =
        try Hashtbl.find instruction_counter addr
        with Not_found -> 0
      in
      (* Check to add a symbolic input *)
      let elems = SymbolicInput.find_all_input_from_position input_entries (addr, it) false in
      let elems = SymbolicInput.to_list elems in
      List.iter (fun x -> self#add_input x env) elems;

      (* Check to add init memory *)
      let elems = InitMemInput.find_all_input_from_position init_mem (addr, it) false in
      let elems = InitMemInput.to_list elems in
      List.iter (fun x -> self#add_init_values x inst.concrete_infos env) elems;
      Path_predicate.DoExec

    method! private visit_instr_after (key:int) (inst:trace_inst) (env:Path_pred_env.t) =
      let addr = inst.location in
      let it = try Hashtbl.find instruction_counter addr with Not_found -> 0 in

      Hashtbl.replace instruction_counter addr (it + 1);

      (* Check to add a symbolic input *)
      let elems = SymbolicInput.find_all_input_from_position input_entries (addr, it) true in
      let elems = SymbolicInput.to_list elems in
      List.iter (fun x -> self#add_input x env) elems;
      begin
        if elems = [] then
          try
            (*            let next_is_already_read = SymbolicIinput.find_all_input_from_position input_entries (Int64.add addr 5L , it) false in*)
            try
              let buf,counter,ret_val,fd =
                self#check_read inst.concrete_infos in
              (* bug pintool in at same time patch + libcall, so dirty hack, if call .., +5 *)
              let next_addr = Int64.add inst.location 5L in
              (* need to clean this part. If this instruction is read, we need
                 to check if there already are symb input here, othewise we will have
                 doublon *)
              input_entries <- SymbolicInput.remove_all_input_from_position input_entries (Int64.add addr 5L , it) false;
              Logger.debug
                "Found read at %d, %Lx : %Lx (next %Lx %d), size arg %Lx size read %Lx"
                key buf counter next_addr it counter ret_val;
              let new_inputs = SymbolicInput.add_inputs_mem input_entries buf (Int64.to_int ret_val) next_addr it false in
              let () =match FdInput.get_name fd with
              | None -> FdInput.add_fd fd "test.plain"
              | Some _-> ()
              in
              InputFromFiles.add_read input_from_files fd (SymbolicInput.get_all_names new_inputs);
              Logger.debug "Add new inputs %d\n" (SymbolicInput.length new_inputs);
              List.iter (fun x -> self#add_input x env) (SymbolicInput.to_list_order_decr new_inputs);
              List.iteri
                (fun i x -> if(i=0 && ret_val<counter) then self#add_input_constraint x env true)
                (SymbolicInput.to_list_order_decr new_inputs);
              input_entries <- SymbolicInput.concat input_entries new_inputs ;
              let fexpr = SmtBvCst(Bitvector.create (Bigint.big_int_of_int64 ret_val) env.Path_pred_env.addr_size) in
              Formula_utils.replace_register "eax" fexpr env
            with
              _ ->
              try
                let buf,counter,fd = self#check_mmap inst.concrete_infos in
                let next_addr = Int64.add inst.location 5L in
                (* need to clean this part. If this instruction is read, we
                     need to check if there already are symb input here, othewise we will
                     have doublon *)
                input_entries <- SymbolicInput.remove_all_input_from_position input_entries (Int64.add addr 5L , it) false;
                Logger.debug "Found mmap at %d, %Lx : %Lx (next %Lx %d), size arg %Lx\n" key buf counter next_addr it counter;
                let new_inputs = SymbolicInput.add_inputs_mem
                    input_entries buf (Int64.to_int counter) next_addr it
                    false in
                InputFromFiles.add_mmap input_from_files fd (SymbolicInput.get_all_names new_inputs);
                Logger.debug "Add new inputs %d\n" (SymbolicInput.length new_inputs);
                List.iter (fun x -> self#add_input x env) (SymbolicInput.to_list_order_decr new_inputs);
                input_entries <- SymbolicInput.concat input_entries new_inputs
              with
                _ ->
                let addrs_fscanf,ret_val = self#check_fscanf inst.concrete_infos in
                if List.length addrs_fscanf > 0 then
                  let next_addr = Int64.add inst.location 5L in
                  input_entries <- SymbolicInput.remove_all_input_from_position input_entries (Int64.add addr 5L , it) false;
                  Logger.debug "Found fscanf at %d, new val %d" key (List.length addrs_fscanf);
                  let new_inputs =
                    List.fold_left (fun x y -> Logger.debug "Addr %Lx" y ;
                                     SymbolicInput.add_inputs_mem
                                       x y 4 next_addr it false )
                      input_entries  addrs_fscanf in
                  Logger.debug "Add new inputs %d\n" (SymbolicInput.length new_inputs);
                  List.iter (fun x -> self#add_input x env) (SymbolicInput.to_list_order_decr new_inputs);
                  input_entries <- SymbolicInput.concat  input_entries new_inputs;
                  let fexpr = SmtBvCst(Bitvector.create (Bigint.big_int_of_int64 ret_val) env.Path_pred_env.addr_size) in
                  Formula_utils.replace_register "eax" fexpr env
          with _ -> ()
      end;

      (* Check to add init memory *)
      let elems = InitMemInput.find_all_input_from_position init_mem (addr, it) true in
      let elems = InitMemInput.to_list elems in
      List.iter (fun x -> self#add_init_values x inst.concrete_infos env) elems;

      Path_predicate.DoExec

  end;;
