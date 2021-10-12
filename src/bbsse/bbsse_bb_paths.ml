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

open Bbsse_options

module Path_generator = struct
  type macro_node = {
    vertex : Ghidra_cfg.V.t;
    choices : bool Stack.t;
    dyn_jumps : Bitvector.t Stack.t;
    mutable k : int;
    mutable current_call : Virtual_address.t option;
    mutable prev_addr : Virtual_address.t;
  }

  type micro_node = {
    m_node : macro_node;
    node : Dhunk.Node.t;
    mutable prev_idx : int;
  }

  type t = {
    cfg : Ghidra_cfg.t;
    n : int;
    m : int;
    (* macro worklsit & micro worklist *)
    worklist : macro_node Stack.t;
    micro_worklist : micro_node Stack.t;
    (* Starting vertex *)
    start_vertex : Ghidra_cfg.V.t;
    mutable prev_idx : int;
    entrypoints : (Virtual_address.t * int) Stack.t;
    all_choices : bool Stack.t Stack.t;
    all_dyn_jumps : Bitvector.t Stack.t Stack.t;
    (* For cyclic paths *)
    cycle_entrypoints : (Virtual_address.t * int) Stack.t;
    cycle_all_choices : bool Stack.t Stack.t;
    cycle_all_dyn_jumps : Bitvector.t Stack.t Stack.t;
    next_table : Ghidra_cfg.vertex Ghidra_cfg.Vtbl.t;
    types_table : string Ghidra_cfg.Vtbl.t;
  }

  let compare_dhunk node1 node2 = Dhunk.Node.id node1 - Dhunk.Node.id node2

  module DhunkSet = Set.Make (struct
    type t = Dhunk.Node.t

    let compare = compare_dhunk
  end)

  let add_to_set_dhunk v set =
    let new_or_not = DhunkSet.mem v set in
    let new_set = DhunkSet.add v set in
    (new_or_not, new_set)

  (* vertex to address *)
  let v_to_addr v = Ghidra_cfg.V.label v

  let get_vertex node = node.vertex

  let get_choices node = node.choices

  let get_dyn_jumps node = node.dyn_jumps

  let get_k node = node.k

  (* Copy and return a macro node *)
  let copy_node node =
    let v = get_vertex node in
    let k = get_k node in
    let c = Stack.copy (get_choices node) in
    let d = Stack.copy (get_dyn_jumps node) in
    {
      vertex = v;
      choices = c;
      dyn_jumps = d;
      k;
      current_call = node.current_call;
      prev_addr = node.prev_addr;
    }

  let add_micro_nodes stack node = Stack.push node stack

  (* Return exits of a DBA bloc *)
  let rec find_exits block to_check exits =
    match Stack.is_empty to_check with
    | true -> exits
    | false -> (
        let node = Stack.pop to_check in
        let not_new, new_set = add_to_set_dhunk node !exits in
        exits := new_set;
        match not_new with
        | false -> (
            match Dhunk.succ block node with
            | [] -> find_exits block to_check exits
            | l ->
                List.iter (add_micro_nodes to_check) l;
                find_exits block to_check exits)
        | true -> find_exits block to_check exits)

  (* Compare 2 addresses *)
  let compare_addr addr1 addr2 = Virtual_address.equal addr1 addr2

  (* Filter exits to return only the good ones *)
  let rec check_exits exits good pg prev_addr already_done =
    match Stack.is_empty exits with
    | true -> good
    | false -> (
        let node = Stack.pop exits in
        match Dhunk.Node.inst node with
        | None -> failwith "Should not happen [check_exits]"
        | Some (Dba.Instr.SJump (jump_target, tag)) -> (
            match jump_target with
            | Dba.JOuter addr -> (
                match tag with
                | Some (Call _) ->
                    Stack.push node good;
                    check_exits exits good pg prev_addr already_done
                | _ -> (
                    (* Checks if it jumps to the right address *)
                    let prev_addr_int = Virtual_address.to_int prev_addr in

                    match prev_addr_int = 0 && already_done = false with
                    | true ->
                        Stack.push node good;
                        check_exits exits good pg prev_addr true
                    | false -> (
                        match compare_addr addr.base prev_addr with
                        | false ->
                            check_exits exits good pg prev_addr already_done
                        | true ->
                            Stack.push node good;
                            check_exits exits good pg prev_addr already_done)))
            | _ -> check_exits exits good pg prev_addr already_done)
        | Some (Dba.Instr.DJump (_, _)) ->
            Stack.push node good;
            check_exits exits good pg prev_addr already_done
        | Some (Dba.Instr.If (_, jump_target, _)) -> (
            match jump_target with
            | Dba.JInner _ -> check_exits exits good pg prev_addr already_done
            | Dba.JOuter addr -> (
                (* Checks if it jumps to the right address *)
                let prev_addr_int = Virtual_address.to_int prev_addr in

                match prev_addr_int = 0 && already_done = false with
                | true ->
                    Stack.push node good;
                    check_exits exits good pg prev_addr true
                | false -> (
                    match compare_addr addr.base prev_addr with
                    | false -> check_exits exits good pg prev_addr already_done
                    | true ->
                        Stack.push node good;
                        check_exits exits good pg prev_addr already_done)))
        | _ -> check_exits exits good pg prev_addr already_done)

  let rec add_good node good micro_worklist =
    match Stack.is_empty good with
    | true -> ()
    | false ->
        let micro_node = Stack.pop good in
        Stack.push
          {
            m_node = node;
            node = micro_node;
            prev_idx = Dhunk.Node.id micro_node;
          }
          micro_worklist;
        add_good (copy_node node) good micro_worklist

  (* Check if path is cyclic when
   * it browses m basic blocks backward *)
  let check_for_cycles pg v =
    let add_i i v = (v, i) in
    match pg.m with
    | 0 -> false
    | _ ->
        let already_done = ref Virtual_address.Set.empty in

        let is_cycle = ref true in

        let rec aux (v, i) =
          match Virtual_address.Set.mem (v_to_addr v) !already_done with
          | true -> ()
          | false -> (
              already_done :=
                Virtual_address.Set.add (v_to_addr v) !already_done;
              match Ghidra_cfg.pred pg.cfg v with
              | [] -> is_cycle := false
              | l when i <= pg.m -> (
                  match
                    Virtual_address.equal
                      (v_to_addr pg.start_vertex)
                      (v_to_addr v)
                  with
                  | true -> ()
                  | false -> (
                      match List.length l with
                      | x when x = 1 ->
                          let new_l = List.map (add_i i) l in
                          List.iter aux new_l
                      | _ ->
                          let new_l = List.map (add_i (i + 1)) l in
                          List.iter aux new_l))
              | _ -> is_cycle := false)
        in

        aux (v, 0);
        !is_cycle

  (* Saves a finished path *)
  let save_path micro_n pg =
    let n = micro_n.m_node in
    let entrypoint = v_to_addr (get_vertex n) in
    let choices = get_choices n in
    let dyn_jumps = get_dyn_jumps n in
    match check_for_cycles pg n.vertex with
    | true ->
        Stack.push (entrypoint, Dhunk.Node.id micro_n.node) pg.cycle_entrypoints;
        Stack.push choices pg.cycle_all_choices;
        Stack.push dyn_jumps pg.cycle_all_dyn_jumps
    | false ->
        Stack.push (entrypoint, Dhunk.Node.id micro_n.node) pg.entrypoints;
        Stack.push choices pg.all_choices;
        Stack.push dyn_jumps pg.all_dyn_jumps

  (* Saves an ended prematurely path
   * ex : when an instruction can't be decoded *)
  let save_undecoded vaddr n pg =
    let entrypoint = vaddr in
    let choices = get_choices n in
    let dyn_jumps = get_dyn_jumps n in
    let instruction = fst (Disasm_core.decode vaddr) in
    let block = instruction.Instruction.dba_block in
    let id = Dhunk.Node.id (Dhunk.start block) in
    match check_for_cycles pg n.vertex with
    | true ->
        Stack.push (entrypoint, id) pg.cycle_entrypoints;
        Stack.push choices pg.cycle_all_choices;
        Stack.push dyn_jumps pg.cycle_all_dyn_jumps
    | false ->
        Stack.push (entrypoint, id) pg.entrypoints;
        Stack.push choices pg.all_choices;
        Stack.push dyn_jumps pg.all_dyn_jumps

  (* Temporary save of cyclic paths *)
  let save_cyclic vaddr n pg =
    let entrypoint = vaddr in
    let choices = get_choices n in
    let dyn_jumps = get_dyn_jumps n in
    let instruction = fst (Disasm_core.decode vaddr) in
    let block = instruction.Instruction.dba_block in
    let id = Dhunk.Node.id (Dhunk.start block) in
    Stack.push (entrypoint, id) pg.cycle_entrypoints;
    Stack.push choices pg.cycle_all_choices;
    Stack.push dyn_jumps pg.cycle_all_dyn_jumps

  (* Process a micro DBA node *)
  let process_micro_node micro_node =
    match Dhunk.Node.inst micro_node.node with
    | None -> failwith "Should not happen [loop_on_dba_nodes]"
    | Some (Dba.Instr.If (_, jump_target, _)) -> (
        let node = micro_node.m_node in
        let choices = get_choices node in
        let null_addr = Virtual_address.of_string "0x00000000" in

        (match Virtual_address.equal null_addr micro_node.m_node.prev_addr with
        | true -> ()
        | false -> node.k <- node.k + 1);

        match jump_target with
        | JInner a -> (
            match a = micro_node.prev_idx with
            | true -> Stack.push true choices
            | false -> Stack.push false choices)
        | JOuter address -> (
            match address.base = micro_node.m_node.prev_addr with
            | true -> Stack.push true choices
            | false -> Stack.push false choices))
    | Some (Dba.Instr.DJump (_, _)) -> (
        Logger.debug "   DJump";
        let null_addr = Virtual_address.of_string "0x00000000" in

        match Virtual_address.equal null_addr micro_node.m_node.prev_addr with
        | true -> ()
        | false ->
            let node = micro_node.m_node in
            node.k <- node.k + 1;

            let dyn_jumps = get_dyn_jumps node in

            (* Converts an address to a bitvector *)
            let temp_int = Virtual_address.to_bigint node.prev_addr in
            let size = Kernel_options.Machine.word_size () in
            let bv = Bitvector.create temp_int size in
            Stack.push bv dyn_jumps)
    | _ -> ()

  let rec add_with_copy l node micro_w prev_idx =
    match l with
    | [] -> ()
    | n :: q ->
        Stack.push { m_node = node; node = n; prev_idx } micro_w;
        let new_node = copy_node node in
        add_with_copy q new_node micro_w prev_idx

  (* Main loop within the DBA bloc *)
  let rec loop_on_dba_nodes block micro_w all_paths pg =
    match Stack.is_empty micro_w with
    | true -> ()
    | false -> (
        let micro_node = Stack.pop micro_w in
        process_micro_node micro_node;
        match get_k micro_node.m_node >= pg.n with
        | true ->
            Stack.push micro_node all_paths;
            loop_on_dba_nodes block micro_w all_paths pg
        | false -> (
            match Dhunk.pred block micro_node.node with
            (* The path is finished, it is added to the macro worklist *)
            | [] ->
                Stack.push micro_node all_paths;
                loop_on_dba_nodes block micro_w all_paths pg
            | l ->
                let prev_idx = Dhunk.Node.id micro_node.node in
                add_with_copy l micro_node.m_node micro_w prev_idx;
                loop_on_dba_nodes block micro_w all_paths pg))

  (* Add the node's predecessors to the worklist *)
  let rec add_predecessors v_list micro_node worklist pg =
    let node = micro_node.m_node in
    match v_list with
    | [] -> ()
    | p :: q -> (
        (* Previous address *)
        let addr = Ghidra_cfg.V.label node.vertex in
        let new_addr = Ghidra_cfg.V.label p in

        (* All this processes the function calls to avoid bugs
         * It also increases k according to basic blocs *)
        match Ghidra_cfg.Vtbl.find_opt pg.types_table addr with
        | Some "Fall" -> (
            node.k <- node.k + 1;
            match node.k >= pg.n with
            | true -> save_path micro_node pg
            | false -> (
                match node.current_call with
                | Some call -> (
                    match Virtual_address.equal new_addr call with
                    | true ->
                        let new_node = copy_node node in
                        let c = get_choices new_node in
                        let d = get_dyn_jumps new_node in
                        let k = get_k new_node in
                        Stack.push
                          {
                            vertex = p;
                            choices = c;
                            dyn_jumps = d;
                            k;
                            current_call = None;
                            prev_addr = v_to_addr node.vertex;
                          }
                          worklist
                    | false -> add_predecessors q micro_node worklist pg)
                | None ->
                    let new_node = copy_node node in
                    let c = get_choices new_node in
                    let d = get_dyn_jumps new_node in
                    let k = get_k new_node in
                    Stack.push
                      {
                        vertex = p;
                        choices = c;
                        dyn_jumps = d;
                        k;
                        current_call = node.current_call;
                        prev_addr = v_to_addr node.vertex;
                      }
                      worklist;
                    add_predecessors q micro_node worklist pg))
        | Some "Call" -> (
            node.k <- node.k + 1;
            match node.k >= pg.n with
            | true -> save_path micro_node pg
            | false ->
                let new_node = copy_node node in
                let c = get_choices new_node in
                let d = get_dyn_jumps new_node in
                let k = get_k new_node in
                Stack.push
                  {
                    vertex = p;
                    choices = c;
                    dyn_jumps = d;
                    k;
                    current_call = node.current_call;
                    prev_addr = v_to_addr node.vertex;
                  }
                  worklist;
                add_predecessors q micro_node worklist pg)
        | _ -> (
            let null_addr = Virtual_address.of_string "0x00000000" in
            (match Virtual_address.equal null_addr node.prev_addr with
            | true -> ()
            | false -> (
                match
                  ( List.length q,
                    List.length (Ghidra_cfg.pred pg.cfg (get_vertex node)) )
                with
                | x, y when x >= 1 || y >= 2 -> node.k <- node.k + 1
                | _, _ -> ()));

            match node.k >= pg.n with
            | true -> save_path micro_node pg
            | false ->
                let new_node = copy_node node in
                let c = get_choices new_node in
                let d = get_dyn_jumps new_node in
                let k = get_k new_node in
                Stack.push
                  {
                    vertex = p;
                    choices = c;
                    dyn_jumps = d;
                    k;
                    current_call = node.current_call;
                    prev_addr = v_to_addr node.vertex;
                  }
                  worklist;
                add_predecessors q micro_node worklist pg))

  (* Saves paths when they reach limit condition
   * or add their predecessors to worklist *)
  let rec add_paths all_paths pg =
    match Stack.is_empty all_paths with
    | true -> ()
    | false -> (
        let node = Stack.pop all_paths in
        match get_k node.m_node >= pg.n with
        | true -> save_path node pg
        | false -> (
            match Ghidra_cfg.pred pg.cfg (get_vertex node.m_node) with
            (* If no more predecessors, stopping the current path *)
            | [] ->
                (match
                   Virtual_address.equal
                     (v_to_addr node.m_node.vertex)
                     (v_to_addr pg.start_vertex)
                 with
                | true -> ()
                | false -> save_path node pg);
                add_paths all_paths pg
            | l ->
                add_predecessors l node pg.worklist pg;
                add_paths all_paths pg))

  let set_to_stack set stack =
    let push x = Stack.push x stack in
    DhunkSet.iter push set;
    stack

  (* Process a (macro) node *)
  let process node pg =
    Logger.debug "Processing 0x%a ..." Virtual_address.pp
      (v_to_addr node.vertex);
    match
      ( Virtual_address.equal (v_to_addr pg.start_vertex) (v_to_addr node.vertex),
        Virtual_address.equal node.prev_addr
          (Virtual_address.of_string "0x00000000") )
    with
    | true, false ->
        (* A cycle on starting address detected *)
        save_cyclic node.prev_addr node pg
    | _ -> (
        let v = get_vertex node in
        let instruction = fst (Disasm_core.decode (v_to_addr v)) in
        let block = instruction.Instruction.dba_block in

        match Dhunk.inst block 0 with
        | None -> failwith "Block 0 not found ..."
        | Some (Dba.Instr.Stop _) -> (
            Logger.debug "DBA STOP instruction found";
            match
              Virtual_address.equal node.prev_addr (v_to_addr pg.start_vertex)
            with
            | true -> ()
            | false -> save_undecoded node.prev_addr node pg)
        | _ ->
            let to_check = Stack.create () in
            (* On donne l'adresse de départ *)
            Stack.push (Dhunk.start block) to_check;

            let set_exits = ref DhunkSet.empty in
            let new_set = find_exits block to_check set_exits in
            let exits = set_to_stack !new_set (Stack.create ()) in

            let good =
              check_exits exits (Stack.create ()) pg node.prev_addr false
            in

            add_good node good pg.micro_worklist;

            let all_paths = Stack.create () in
            loop_on_dba_nodes block pg.micro_worklist all_paths pg;

            add_paths all_paths pg)

  (* Main function *)
  let main_loop pg =
    let first_node =
      {
        vertex = pg.start_vertex;
        choices = Stack.create ();
        dyn_jumps = Stack.create ();
        k = 0;
        current_call = None;
        prev_addr = Virtual_address.of_string "0x00000000";
      }
    in

    Stack.push first_node pg.worklist;
    let worklist = pg.worklist in

    (* Recursive loop on vertexes *)
    let rec loop_on_nodes () =
      match Stack.is_empty worklist with
      | true -> ()
      | false -> (
          let open Ghidra_cfg in
          let node = Stack.pop worklist in

          match Vtbl.find_opt pg.types_table (V.label node.vertex) with
          | Some "Next" -> (
              match Vtbl.find_opt pg.next_table (v_to_addr node.vertex) with
              | Some v ->
                  node.current_call <- Some (v_to_addr v);
                  process node pg;
                  loop_on_nodes ()
              | None -> failwith "This node -Next- must have a caller ")
          | _ ->
              process node pg;
              loop_on_nodes ())
    in
    loop_on_nodes ();

    let print_entry x =
      Logger.debug "   Addr -> 0x%a" Virtual_address.pp (fst x)
    in
    Logger.debug "Entrypoints : ";
    Stack.iter print_entry pg.entrypoints;
    Logger.debug "Cyclic Entrypoints : ";
    Stack.iter print_entry pg.cycle_entrypoints;
    (match Stack.length pg.entrypoints = 0 with
    | true -> Logger.debug "Entrypoint empty !"
    | false -> ());
    ( (pg.entrypoints, pg.all_choices, pg.all_dyn_jumps),
      Some (pg.cycle_entrypoints, pg.cycle_all_choices, pg.cycle_all_dyn_jumps)
    )

  (* Initialization + return results  *)
  let init v =
    match Bbsse_options.MaxCondition.get_opt () with
    | None -> failwith "Please specify the number max of basic blocks"
    | Some max_cond -> (
        let g_cfg, next_table, types_table, _ = Ghidra_cfg.import () in

        match Ghidra_cfg.mem_vertex g_cfg v with
        | false -> failwith "Starting address is not correct"
        | true ->
            let val_m =
              match Bbsse_options.MaxConditionCycle.get_opt () with
              | None -> 0
              | Some m -> m
            in
            let pg =
              {
                cfg = g_cfg;
                (* Limit conditions *)
                n = max_cond;
                m = val_m;
                worklist = Stack.create ();
                micro_worklist = Stack.create ();
                (* Starting vertex *)
                start_vertex = v;
                prev_idx = 0;
                entrypoints = Stack.create ();
                all_choices = Stack.create ();
                all_dyn_jumps = Stack.create ();
                cycle_entrypoints = Stack.create ();
                cycle_all_choices = Stack.create ();
                cycle_all_dyn_jumps = Stack.create ();
                next_table;
                types_table;
              }
            in
            main_loop pg)

  (* Automatic reinitialisation*)
  let reinit v g_cfg next_table types_table =
    match Bbsse_options.MaxCondition.get_opt () with
    | None -> failwith "Please specify the number max of basic blocks"
    | Some max_cond -> (
        match Ghidra_cfg.mem_vertex g_cfg v with
        | false -> failwith "Starting address is not correct"
        | true ->
            let val_m =
              match Bbsse_options.MaxConditionCycle.get_opt () with
              | None -> 0
              | Some m -> m
            in
            let pg =
              {
                cfg = g_cfg;
                (* Limit conditions *)
                n = max_cond;
                m = val_m;
                worklist = Stack.create ();
                micro_worklist = Stack.create ();
                (* Starting vertex *)
                start_vertex = v;
                prev_idx = 0;
                entrypoints = Stack.create ();
                all_choices = Stack.create ();
                all_dyn_jumps = Stack.create ();
                cycle_entrypoints = Stack.create ();
                cycle_all_choices = Stack.create ();
                cycle_all_dyn_jumps = Stack.create ();
                next_table;
                types_table;
              }
            in
            main_loop pg)
end
