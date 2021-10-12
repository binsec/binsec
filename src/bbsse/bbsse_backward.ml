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

open Bbsse_types
open Bbsse_options

type entry_type = DJump | Opaque_Predicate | Other

module Backward (G : GLOBAL_ENV) = struct
  type t = {
    entrypoints : (Virtual_address.t * int) Stack.t;
    all_dyn_jumps : Bitvector.t Stack.t Stack.t;
    all_choices : bool Stack.t Stack.t;
    cycle_entrypoints : (Virtual_address.t * int) Stack.t;
    cycle_all_dyn_jumps : Bitvector.t Stack.t Stack.t;
    cycle_all_choices : bool Stack.t Stack.t;
    dest_addr : Virtual_address.t;
    mutable current_choices : bool Stack.t;
    (* current_choices exemple :
     * [ F;T;T;T;T;F;T;T;F ] *)
    mutable current_dyn_jumps : Bitvector.t Stack.t;
    mutable is_next_op : bool;
    is_jump_lookup : bool;
    gen_g_t : bool;
    symbols : Virtual_address.t list;
    mutable take_next : bool;
    mutable possible_directions : bool list;
  }

  (* Ground Truth detection for corutils + OLLVM programs *)
  let get_op_symbols tab =
    let add_op_symbols l x =
      let name = Loader.Symbol.name x in

      match String.length name = 1 with
      | true -> (
          match String.equal "x" name with
          | true ->
              let size = Kernel_options.Machine.word_size () in
              let bv = Bitvector.of_int ~size (Loader.Symbol.value x) in
              let addr = Virtual_address.of_bitvector bv in
              addr :: l
          | false -> l)
      | false -> (
          try
            let prefix = String.sub name 0 2 in

            match String.equal prefix "x." with
            | true ->
                let size = Kernel_options.Machine.word_size () in
                let bv = Bitvector.of_int ~size (Loader.Symbol.value x) in
                let addr = Virtual_address.of_bitvector bv in
                addr :: l
            | false -> l
          with Invalid_argument _ -> l)
    in

    Array.fold_left add_op_symbols [] tab

  (* Initialisation *)
  let init_backward v filename =
    match Bbsse_bb_paths.Path_generator.init v with
    | (e, c, d), None ->
        {
          entrypoints = e;
          dest_addr = Ghidra_cfg.V.label v;
          all_choices = c;
          current_choices = Stack.create ();
          all_dyn_jumps = d;
          current_dyn_jumps = Stack.create ();
          is_jump_lookup = false;
          is_next_op = false;
          symbols =
            get_op_symbols (Loader.Img.symbols (Loader.load_file filename));
          gen_g_t = Bbsse_options.GenGroundTruth.get ();
          take_next = false;
          cycle_entrypoints = Stack.create ();
          cycle_all_choices = Stack.create ();
          cycle_all_dyn_jumps = Stack.create ();
          possible_directions = [];
        }
    (* e,c,d returned from acyclic paths
     * f,g,h returned from cyclic paths *)
    | (e, c, d), Some (e1, c1, d1) ->
        {
          entrypoints = e;
          dest_addr = Ghidra_cfg.V.label v;
          all_choices = c;
          current_choices = Stack.create ();
          all_dyn_jumps = d;
          current_dyn_jumps = Stack.create ();
          is_jump_lookup = false;
          is_next_op = false;
          symbols =
            get_op_symbols (Loader.Img.symbols (Loader.load_file filename));
          gen_g_t = Bbsse_options.GenGroundTruth.get ();
          take_next = false;
          cycle_entrypoints = e1;
          cycle_all_choices = c1;
          cycle_all_dyn_jumps = d1;
          possible_directions = [];
        }

  (* Automatic reinitialisation*)
  let reinit_backward v g_cfg next_table types_table filename =
    match
      Bbsse_bb_paths.Path_generator.reinit v g_cfg next_table types_table
    with
    | (e, c, d), None ->
        {
          entrypoints = e;
          dest_addr = Ghidra_cfg.V.label v;
          all_choices = c;
          current_choices = Stack.create ();
          all_dyn_jumps = d;
          current_dyn_jumps = Stack.create ();
          is_jump_lookup = true;
          is_next_op = false;
          symbols =
            get_op_symbols (Loader.Img.symbols (Loader.load_file filename));
          gen_g_t = Bbsse_options.GenGroundTruth.get ();
          take_next = false;
          cycle_entrypoints = Stack.create ();
          cycle_all_choices = Stack.create ();
          cycle_all_dyn_jumps = Stack.create ();
          possible_directions = [];
        }
    | (e, c, d), Some (e1, c1, d1) ->
        {
          entrypoints = e;
          dest_addr = Ghidra_cfg.V.label v;
          all_choices = c;
          current_choices = Stack.create ();
          all_dyn_jumps = d;
          current_dyn_jumps = Stack.create ();
          is_jump_lookup = true;
          is_next_op = false;
          symbols =
            get_op_symbols (Loader.Img.symbols (Loader.load_file filename));
          gen_g_t = Bbsse_options.GenGroundTruth.get ();
          take_next = false;
          cycle_entrypoints = e1;
          cycle_all_choices = c1;
          cycle_all_dyn_jumps = d1;
          possible_directions = [];
        }

  (* Get entrypoint of current path *)
  let get_entrypoint b = Stack.pop b.entrypoints

  (* Get next choice of current path *)
  let get_next_choice b =
    match Stack.is_empty b.current_choices with
    | false -> Stack.pop b.current_choices
    | true -> failwith "current_choices queue should not be empty"

  (* Get next dynamic jump of current path *)
  let get_next_dyn_jump b =
    match Stack.is_empty b.current_dyn_jumps with
    | false -> Stack.pop b.current_dyn_jumps
    | true ->
        Logger.debug "Zero";
        Bitvector.zero

  (* Get the choices of current jump *)
  let get_choices b =
    match Stack.is_empty b.all_choices with
    | false -> Stack.pop b.all_choices
    | true -> failwith " Should not be empty [get_choices]"

  (* Get dynamic jumps of current jump *)
  let get_dyn_jumps b =
    match Stack.is_empty b.all_dyn_jumps with
    | false -> Stack.pop b.all_dyn_jumps
    | true -> failwith " Should not be empty [get_dyn_jumps]"

  type predicat_type = CLEAR | OPAQUE | UNKNOWN

  module PredicatSet = Set.Make (struct
    type t = Virtual_address.t

    let compare = Virtual_address.compare
  end)

  module Stats = struct
    (* A lot of stuff used for stats and benchmarks *)
    type t = {
      visited : int;
      (* Does not count the number uniquely visited paths but
       * should ... *)
      choices : int;
      asserts_unknown : int;
      asserts_failed : int;
      opaque_predicates : int;
      clear_predicates : int;
      unknown_predicates : int;
      unfeasible_predicates : int;
      unfeasible_paths : int;
      opaque_predicates_detected : int;
      other_opaque_predicates : int;
      false_negatives : int;
      nb_paths : int;
      all_predicates_opaque : PredicatSet.t;
      all_predicates_clear : PredicatSet.t;
      all_predicates_unknown : PredicatSet.t;
      all_predicates_unfeasible : PredicatSet.t;
      jumps_done : Virtual_address.Set.t;
      mutable all_jumps : Virtual_address.t Stack.t option;
      mutable cur_jmp_cond : Dba.Expr.t option;
      paths_per_jump : int Virtual_address.Htbl.t;
      must_be_opaque : Virtual_address.Set.t;
      (* Global Time *)
      times : float Virtual_address.Htbl.t;
      t_start : float;
      t_end : float;
      (* Time of entrypoints lookup *)
      times_e : float Virtual_address.Htbl.t;
      t_start_e : float;
      t_end_e : float;
      (* Time  in BB - SSE *)
      times_b : float Virtual_address.Htbl.t;
      t_start_b : float;
      t_end_b : float;
      (* Time in solver *)
      times_s : float Virtual_address.Htbl.t;
      t_start_s : float;
      t_end_s : float;
      entrypoint_per_jump : int Virtual_address.Htbl.t;
      jmp_entry : Virtual_address.t Virtual_address.Htbl.t;
      mutable entrypoint : Virtual_address.t;
      mutable nb_cur_instr : int;
      nb_instr : int Stack.t;
      g_t_set : Virtual_address.Set.t;
      right_left_conds : Bitvector.t Virtual_address.Htbl.t;
    }

    let empty =
      {
        visited = 0;
        choices = 0;
        asserts_unknown = 0;
        asserts_failed = 0;
        opaque_predicates = 0;
        clear_predicates = 0;
        unknown_predicates = 0;
        unfeasible_predicates = 0;
        unfeasible_paths = 0;
        all_predicates_opaque = PredicatSet.empty;
        all_predicates_clear = PredicatSet.empty;
        all_predicates_unknown = PredicatSet.empty;
        all_predicates_unfeasible = PredicatSet.empty;
        jumps_done = Virtual_address.Set.empty;
        all_jumps = None;
        cur_jmp_cond = None;
        must_be_opaque = Virtual_address.Set.empty;
        opaque_predicates_detected = 0;
        other_opaque_predicates = 0;
        false_negatives = 0;
        nb_paths = 0;
        times = Virtual_address.Htbl.create 7;
        t_start = 0.;
        t_end = 0.;
        times_e = Virtual_address.Htbl.create 7;
        t_start_e = 0.;
        t_end_e = 0.;
        times_b = Virtual_address.Htbl.create 7;
        t_start_b = 0.;
        t_end_b = 0.;
        times_s = Virtual_address.Htbl.create 7;
        t_start_s = 0.;
        t_end_s = 0.;
        paths_per_jump = Virtual_address.Htbl.create 7;
        entrypoint_per_jump = Virtual_address.Htbl.create 7;
        jmp_entry = Virtual_address.Htbl.create 7;
        right_left_conds = Virtual_address.Htbl.create 7;
        entrypoint = Virtual_address.create 0;
        nb_cur_instr = 0;
        nb_instr = Stack.create ();
        g_t_set = Virtual_address.Set.empty;
      }

    let add_visited s = { s with visited = s.visited + 1 }

    let add_choice s = { s with choices = s.choices + 1 }

    let add_assert_unknown s =
      { s with asserts_unknown = s.asserts_unknown + 1 }

    let add_assert_failed s = { s with asserts_failed = s.asserts_failed + 1 }

    let add_opaque_predicate s =
      { s with opaque_predicates = s.opaque_predicates + 1 }

    let add_clear_predicate s =
      { s with clear_predicates = s.clear_predicates + 1 }

    let add_unknown_predicate s =
      { s with unknown_predicates = s.unknown_predicates + 1 }

    let add_unfeasible_paths s =
      { s with unfeasible_paths = s.unfeasible_paths + 1 }

    let add_unfeasible_predicate s =
      { s with unfeasible_predicates = s.unfeasible_predicates + 1 }

    let add_predicate_opaque s p =
      {
        s with
        all_predicates_opaque = PredicatSet.add p s.all_predicates_opaque;
      }

    let add_predicate_unfeasible s p =
      {
        s with
        all_predicates_unfeasible =
          PredicatSet.add p s.all_predicates_unfeasible;
      }

    let add_predicate_clear s p =
      { s with all_predicates_clear = PredicatSet.add p s.all_predicates_clear }

    let add_predicate_unknown s p =
      {
        s with
        all_predicates_unknown = PredicatSet.add p s.all_predicates_unknown;
      }

    let add_jump_done s p =
      { s with jumps_done = Virtual_address.Set.add p s.jumps_done }

    let add_op_detected s =
      { s with opaque_predicates_detected = s.opaque_predicates_detected + 1 }

    let add_other_op s =
      { s with other_opaque_predicates = s.other_opaque_predicates + 1 }

    let add_fn s = { s with false_negatives = s.false_negatives + 1 }

    let add_path s = { s with nb_paths = s.nb_paths + 1 }

    let add_must_be_opaque s x =
      { s with must_be_opaque = Virtual_address.Set.add x s.must_be_opaque }

    let remove_opaque_predicate s =
      { s with opaque_predicates = s.opaque_predicates - 1 }

    let remove_clear_predicate s =
      { s with clear_predicates = s.clear_predicates - 1 }

    let remove_unknown_predicate s =
      { s with unknown_predicates = s.unknown_predicates - 1 }

    let remove_unfeasible_predicate s =
      { s with unfeasible_predicates = s.unfeasible_predicates - 1 }

    let set_all_jumps s stack = { s with all_jumps = stack }

    let set_cur_jmp_cond s cond = { s with cur_jmp_cond = cond }

    let get_cur_jmp_cond s = s.cur_jmp_cond

    let get_must_be_opaque s = s.must_be_opaque

    let get_predicates_opaque s = s.all_predicates_opaque

    let get_predicates_clear s = s.all_predicates_clear

    let get_predicates_unknown s = s.all_predicates_unknown

    let get_predicates_unfeasible s = s.all_predicates_unfeasible

    let get_times s = s.times

    let get_times_e s = s.times_e

    let get_times_b s = s.times_b

    let get_times_s s = s.times_s

    let get_nb_paths s = s.nb_paths

    let get_paths_per_jump s = s.paths_per_jump

    let get_right_left_conds s = s.right_left_conds

    let get_entrypoint_per_jump s = s.entrypoint_per_jump

    let get_jmp_entry s = s.jmp_entry

    let get_jumps_done s = s.jumps_done

    (*---------------------------------------------*)
    let get_t_start s = s.t_start

    let set_t_start s t = { s with t_start = t }

    let get_t_end s = s.t_end

    let set_t_end s t = { s with t_end = t }

    (*---------------------------------------------*)
    let get_t_start_e s = s.t_start_e

    let set_t_start_e s t = { s with t_start_e = t }

    let get_t_end_e s = s.t_end_e

    let set_t_end_e s t = { s with t_end_e = t }

    (*---------------------------------------------*)
    let get_t_start_b s = s.t_start_b

    let set_t_start_b s t = { s with t_start_b = t }

    let get_t_end_b s = s.t_end_b

    let set_t_end_b s t = { s with t_end_b = t }

    (*---------------------------------------------*)
    let get_t_start_s s = s.t_start_s

    let set_t_start_s s t = { s with t_start_s = t }

    let get_t_end_s s = s.t_end_s

    let set_t_end_s s t = { s with t_end_s = t }
    (*---------------------------------------------*)

    let get_entrypoint s = s.entrypoint

    let set_entrypoint s e = { s with entrypoint = e }

    let get_nb_cur_instr s = s.nb_cur_instr

    let set_nb_cur_instr s n = { s with nb_cur_instr = n }

    let get_nb_instr s = s.nb_instr

    let add_nb_instr s n =
      Stack.push n s.nb_instr;
      s

    let get_g_t_set s = s.g_t_set

    let add_g_t_set s x =
      { s with g_t_set = Virtual_address.Set.add x s.g_t_set }

    let pp ppf s =
      Format.fprintf ppf
        "@[<v 0>@[<h>selections %d@]@,\
         @[<h>choice %d@]@,\
         @[<h>unknown assertions %d@]@,\
         @[<h>failed assertions %d@]@]" s.visited s.choices s.asserts_unknown
        s.asserts_failed

    let pp_predicate ppf s =
      Format.fprintf ppf
        "@[<v 0>@[<h>opaque predicates detected %d@]@,\
         @[<h>other opaque predicates detected %d@]@,\
         @[<h>false negatives %d@]@,\
         @[<h>clear predicates %d@]@,\
         @[<h>unfeasible predicates %d@]@,\
         @[<h>unfeasible paths %d@]@,\
         @[<h>unknown predicates %d@]@]" s.opaque_predicates_detected
        s.other_opaque_predicates s.false_negatives s.clear_predicates
        s.unfeasible_predicates s.unfeasible_paths s.unknown_predicates

    module R = struct
      let value = ref empty

      let add_visited () = value := add_visited !value

      let add_choice () = value := add_choice !value

      let add_assert_unknown () = value := add_assert_unknown !value

      let add_assert_failed () = value := add_assert_failed !value

      let add_opaque_predicate () = value := add_opaque_predicate !value

      let add_clear_predicate () = value := add_clear_predicate !value

      let add_unknown_predicate () = value := add_unknown_predicate !value

      let add_predicate_opaque p = value := add_predicate_opaque !value p

      let add_predicate_clear p = value := add_predicate_clear !value p

      let add_predicate_unknown p = value := add_predicate_unknown !value p

      let add_predicate_unfeasible p =
        value := add_predicate_unfeasible !value p

      let add_unfeasible_paths () = value := add_unfeasible_paths !value

      let add_unfeasible_predicate () = value := add_unfeasible_predicate !value

      let add_jump_done j = value := add_jump_done !value j

      let add_op_detected () = value := add_op_detected !value

      let add_other_op () = value := add_other_op !value

      let add_fn () = value := add_fn !value

      let add_path () = value := add_path !value

      let get_nb_paths () = get_nb_paths !value

      let add_must_be_opaque x = value := add_must_be_opaque !value x

      let remove_opaque_predicate () = value := remove_opaque_predicate !value

      let remove_clear_predicate () = value := remove_clear_predicate !value

      let remove_unknown_predicate () = value := remove_unknown_predicate !value

      let remove_unfeasible_predicate () =
        value := remove_unfeasible_predicate !value

      let get_predicates_opaque () = get_predicates_opaque !value

      let get_predicates_clear () = get_predicates_clear !value

      let get_predicates_unknown () = get_predicates_unknown !value

      let get_predicates_unfeasible () = get_predicates_unfeasible !value

      let pp ppf () = pp ppf !value

      let pp_predicate ppf () = pp_predicate ppf !value

      let set_all_jumps stack = value := set_all_jumps !value stack

      let set_cur_jmp_cond cond = value := set_cur_jmp_cond !value cond

      let get_cur_jmp_cond () = get_cur_jmp_cond !value

      let get_must_be_opaque () = get_must_be_opaque !value

      let get_jumps_done () = get_jumps_done !value

      let get_right_left_conds () = get_right_left_conds !value

      let get_paths_per_jump () = get_paths_per_jump !value

      let get_entrypoint_per_jump () = get_entrypoint_per_jump !value

      let get_jmp_entry () = get_jmp_entry !value

      (*---------------------------------------------*)
      let get_times () = get_times !value

      let get_t_start () = get_t_start !value

      let set_t_start t = value := set_t_start !value t

      let get_t_end () = get_t_end !value

      let set_t_end t = value := set_t_end !value t

      (*---------------------------------------------*)
      let get_times_e () = get_times_e !value

      let get_t_start_e () = get_t_start_e !value

      let set_t_start_e t = value := set_t_start_e !value t

      let get_t_end_e () = get_t_end_e !value

      let set_t_end_e t = value := set_t_end_e !value t

      (*---------------------------------------------*)
      let get_times_b () = get_times_b !value

      let get_t_start_b () = get_t_start_b !value

      let set_t_start_b t = value := set_t_start_b !value t

      let get_t_end_b () = get_t_end_b !value

      let set_t_end_b t = value := set_t_end_b !value t

      (*---------------------------------------------*)
      let get_times_s () = get_times_s !value

      let get_t_start_s () = get_t_start_s !value

      let set_t_start_s t = value := set_t_start_s !value t

      let get_t_end_s () = get_t_end_s !value

      let set_t_end_s t = value := set_t_end_s !value t

      let get_entrypoint () = get_entrypoint !value

      let set_entrypoint e = value := set_entrypoint !value e

      let get_nb_cur_instr () = get_nb_cur_instr !value

      let set_nb_cur_instr n = value := set_nb_cur_instr !value n

      let get_nb_instr () = get_nb_instr !value

      let add_nb_instr n = value := add_nb_instr !value n

      let get_g_t_set () = get_g_t_set !value

      let add_g_t_set x = value := add_g_t_set !value x
    end

    include R
  end

  module Env = struct
    type t = {
      global : G.t;
      local : Path_state.t;
      cfg : C.t;
      entrypoint : Virtual_address.t;
    }

    let create ~global ~local =
      {
        global;
        local;
        cfg = Path_state.cfg local;
        entrypoint = Path_state.entrypoint local;
      }

    (* Get local / global *)
    let local e = e.local

    let global e = e.global

    (* Get next path *)
    let get_path g =
      let p = G.get_next_path g in
      let global, local = (g, p) in
      create ~global ~local
  end

  module Eval = struct
    (* Done *)
    let static_jump ~jump_target le =
      match jump_target with
      (* Jumping within the same DBA block : internal goto *)
      | Dba.JInner idx -> Some (Path_state.set_block_index idx le)
      (* Jumping to an other address : external goto *)
      | Dba.JOuter addr ->
          let vaddr = Dba_types.Caddress.to_virtual_address addr in
          Logger.debug ~level:5 "Jumping to new address %a" Virtual_address.pp
            vaddr;
          let open Bbsse_types.Path_state in
          Some (goto_vaddr vaddr le)

    let assignment ~lvalue ~rvalue e =
      (* generate the logical constraint, add it to the path predicate,
       * update symbolic_state.
       *)
      let open Bbsse_smt in
      Env.{ e with local = Translate.assignment lvalue rvalue e.local }

    let nondet ~lvalue ~region e =
      let _ = region in
      (* generate the logical constraint, add it to the path predicate,
       * update symbolic_state.
       *)
      let open Bbsse_smt in
      Env.{ e with local = Translate.nondet lvalue e.local }

    let ite ~condition ~jump_target ~local_target e b =
      (* True condition  -> jump_target
       * False condition -> local_target
       * create 2 states for the 2 conditionnal paths
       * expand path with assert condition and go to jump_target
       * push path with assert not condition and go to local_target *)
      let old_state = Env.local e in
      let raw_cond, state = Bbsse_smt.Translate.expr' old_state condition in
      let condition = Formula.(mk_bv_equal raw_cond mk_bv_one) in
      let alternate_condition = Formula.mk_bl_not condition in
      let consequent =
        Path_state.add_assertion condition state |> static_jump ~jump_target
      in
      let alternate_state = Path_state.branch state in
      let alternative =
        Path_state.add_assertion alternate_condition alternate_state
        |> Bbsse_types.Path_state.set_block_index local_target
      in
      match get_next_choice b with
      | true -> (
          (* True branch *)
          match consequent with
          | None -> failwith "Must not happen [ITE]"
          | Some consequent -> { e with local = consequent })
      | false ->
          (* False branch *)
          { e with local = alternative }

    let dynamic_jump ~jump_expr e b =
      let path_state = Env.local e in
      let target, path_state = Bbsse_smt.Translate.expr' path_state jump_expr in
      let bitvect = get_next_dyn_jump b in

      match bitvect with
      (* when bv == 0x00000000 it means that it is the dyn_jump to analyze
       * it is mostly used to detect implementation bugs
       * this should not happen *)
      | bv when bv = Bitvector.zero ->
          failwith "Should be the analyzed dyn_jump"
      | bv ->
          (* Add equality condition *)
          let condition = Formula.(mk_bv_equal (mk_bv_cst bv) target)
          and addr = Virtual_address.of_bitvector bv in
          Logger.debug ~level:4 "Dynamic Jump to : %a " Bitvector.pp_hex bv;
          let ps = Path_state.add_assertion condition path_state in
          (* On effectue le saut *)
          let ps = Bbsse_types.Path_state.goto_vaddr addr ps in
          let global = e.global in
          let local = ps in
          Env.create ~global ~local

    let skip instruction idx e =
      Logger.info ~level:3 "Skipping %a" Dba_printer.Ascii.pp_instruction
        instruction;
      Env.{ e with local = Path_state.set_block_index idx e.local }

    let check f cond idx e =
      match cond with
      | Dba.Expr.Cst _ -> failwith "Constant"
      | _ ->
          let state = Env.local e in
          let cond, symbols =
            Bbsse_smt.Translate.expr (Path_state.symbolic_state state) cond
          in
          let state = Path_state.set_symbolic_state symbols state in
          let state = f cond state in
          { e with Env.local = Path_state.set_block_index idx state }

    let assertion test state =
      Path_state.add_assertion
        Formula.(mk_bv_equal test Formula.mk_bv_one)
        state

    let maybe_add_comment ps =
      (* If comment is activated, this will add, for every formula entry, a
         comment about where it comes from.This can be usefule to debug the path predicate translation.*)
      if Sse_options.Comment.get () then
        let syst = Path_state.symbolic_state ps in
        let comment =
          Print_utils.string_from_pp
            (Formula_pp.pp_as_comment Path_state.pp_loc)
            ps
        in
        let symbolic_state = Sse_symbolic.State.comment comment syst in
        Path_state.set_symbolic_state symbolic_state ps
      else ps

    let assumption test state =
      Path_state.add_assertion
        Formula.(mk_bv_equal test Formula.mk_bv_one)
        state

    (* Execute an atomic step of BB-SSE *)
    let go b e =
      (*return e.local (= the new path_state) *)
      let path_state = maybe_add_comment @@ Env.local e in
      Logger.debug ~level:5 "@[Evaluating@ %a@]" Path_state.pp_loc path_state;
      match Path_state.dba_instruction path_state with
      (* LEFTvalue , RIGHTvalue , idx = next index *)
      | Dba.Instr.Assign (lvalue, rvalue, idx) ->
          let e = assignment ~lvalue ~rvalue e in
          Some Env.{ e with local = Path_state.set_block_index idx e.local }
      | Dba.Instr.Nondet (lvalue, region, idx) ->
          let e = nondet ~lvalue ~region e in
          Some Env.{ e with local = Path_state.set_block_index idx e.local }
      | Dba.Instr.SJump (jump_target, _) -> (
          match static_jump ~jump_target e.Env.local with
          | None ->
              failwith "Should never be taken [forbidden jump]"
              (* This jump has been forbidden *)
          | Some local -> Some { e with Env.local })
      | Dba.Instr.If (condition, jump_target, local_target) ->
          Some (ite ~condition ~jump_target ~local_target e b)
      | Dba.Instr.DJump (je, _) -> Some (dynamic_jump ~jump_expr:je e b)
      | Dba.Instr.Undef (_, idx) as instruction -> Some (skip instruction idx e)
      | Dba.Instr.Stop _ -> None
      | Dba.Instr.Assert (condition, idx) ->
          Some (check assertion condition idx e)
          (* All those are not yet implemented  : *)
      | Dba.Instr.Assume (condition, idx) ->
          Some (check assumption condition idx e)
      | Dba.Instr.NondetAssume (lvals, condition, idx) ->
          let e =
            List.fold_left
              (fun e lval ->
                let open Bbsse_smt in
                Env.{ e with local = Translate.nondet lval e.local })
              e lvals
          in
          Some (check assumption condition idx e)
      | (Dba.Instr.Malloc _ | Dba.Instr.Free _ | Dba.Instr.Print _) as
        dba_instruction ->
          let msg =
            Format.asprintf "%a" Dba_printer.Ascii.pp_instruction
              dba_instruction
          in
          Errors.not_yet_implemented msg
  end

  (* A bunch of functions used to visit recursively the
   * terms of a formula *)
  let rec visit_term env tm b =
    let open Formula in
    visit_term_desc env b tm.term_desc

  and visit_term_desc env b = function
    | BlTerm bl -> visit_bl_term env bl b
    | BvTerm bv -> visit_bv_term env bv b
    | AxTerm ax -> visit_ax_term env ax b

  (*-----------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
  and visit_bl_term env bl b = visit_bl_term_desc env b bl.bl_term_desc

  and visit_bl_term_desc env b = function
    | BlTrue -> ()
    | BlFalse -> ()
    | BlFun (_, ls) ->
        let rec aux l =
          match l with
          | [] -> ()
          | h :: q ->
              visit_term env h b;
              aux q
        in
        aux ls
    | BlLet (_, _) -> ()
    | BlUnop (_, bl) -> visit_bl_term env bl b
    | BlBnop (_, bl1, bl2) ->
        visit_bl_term env bl1 b;
        visit_bl_term env bl2 b
    | BlComp (_, bl1, bl2) ->
        visit_bl_term env bl1 b;
        visit_bl_term env bl2 b
    | BvComp (_, bv1, bv2) ->
        visit_bv_term env bv1 b;
        visit_bv_term env bv2 b
    | AxComp (_, ax1, ax2) ->
        visit_ax_term env ax1 b;
        visit_ax_term env ax2 b
    | BlIte (bl, bl1, bl2) ->
        visit_bl_term env bl b;
        visit_bl_term env bl1 b;
        visit_bl_term env bl2 b
  (*-----------------------------------------------------------------------------------------------------------------------------------------------------------------------*)

  and visit_bv_term env bv b = visit_bv_term_desc env b bv.bv_term_desc

  and visit_bv_term_desc env b = function
    | BvCst _ -> ()
    | BvFun (_, ls) ->
        let rec aux l =
          match l with
          | [] -> ()
          | h :: q ->
              visit_term env h b;
              aux q
        in
        aux ls
    | BvLet (_, _) -> ()
    | BvUnop (_, bv) -> visit_bv_term env bv b
    | BvBnop (_, bv1, bv2) ->
        visit_bv_term env bv1 b;
        visit_bv_term env bv2 b
    | BvIte (bl, bv1, bv2) ->
        visit_bl_term env bl b;
        visit_bv_term env bv1 b;
        visit_bv_term env bv2 b
    | Select (_, ax, bv) -> (
        visit_ax_term env ax b;
        visit_bv_term env bv b;

        match bv.bv_term_desc with
        | BvCst bitv -> (
            let addr = Virtual_address.of_bitvector bitv in
            (* This is the detection methode for the coreutils : *)
            match List.mem addr b.symbols with
            | true -> b.is_next_op <- true
            | false -> ()
            (* This is the detection method for X-Tunnel : *)
            (*
                     match Virtual_address.compare addr(Virtual_address.of_string "0x0059232c"),
                     Virtual_address.compare addr(Virtual_address.of_string "0x0058d9b4") with
                     | x,y when x <= 0 && y >= 0 ->b.is_next_op <- true
                     | _ ->()
                    end*)
            )
        | _ -> ())

  (*-----------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
  and visit_ax_term env array b =
    match array.ax_term_desc with
    | AxFun (_, ls) ->
        let rec aux l =
          match l with
          | [] -> ()
          | h :: q ->
              visit_term env h b;
              aux q
        in
        aux ls
    | AxLet (_, _) -> ()
    | AxIte (bl, ax1, ax2) ->
        visit_bl_term env bl b;
        visit_ax_term env ax1 b;
        visit_ax_term env ax2 b
    | Store (_, ax, bv1, bv2) ->
        visit_ax_term env ax b;
        visit_bv_term env bv1 b;
        visit_bv_term env bv2 b

  let visit_entry b v_set entry =
    match entry.Formula.entry_desc with
    | Declare decl -> (
        match decl.decl_desc with
        | BlDecl (bl_var, _) ->
            v_set := Formula.VarSet.remove (BlVar bl_var) !v_set
        | BvDecl (bv_var, _) ->
            v_set := Formula.VarSet.remove (BvVar bv_var) !v_set
        | AxDecl (ax_var, _) ->
            v_set := Formula.VarSet.remove (AxVar ax_var) !v_set)
    | Define def -> (
        match def.def_desc with
        | BlDef (v, _, bl_term) when Formula.VarSet.mem (BlVar v) !v_set ->
            v_set := Formula.VarSet.remove (BlVar v) !v_set;
            v_set :=
              Formula.VarSet.union
                (Formula_utils.bl_term_variables bl_term)
                !v_set;
            visit_bl_term None bl_term b
        | BvDef (v, _, bv_term) when Formula.VarSet.mem (BvVar v) !v_set ->
            v_set := Formula.VarSet.remove (BvVar v) !v_set;
            v_set :=
              Formula.VarSet.union
                (Formula_utils.bv_term_variables bv_term)
                !v_set;
            visit_bv_term None bv_term b
        | AxDef (v, _, ax_term) when Formula.VarSet.mem (AxVar v) !v_set ->
            v_set := Formula.VarSet.remove (AxVar v) !v_set;
            v_set :=
              Formula.VarSet.union
                (Formula_utils.ax_term_variables ax_term)
                !v_set;
            visit_ax_term None ax_term b
        | _ -> ())
    | Assert _ -> ()
    | Assume _ -> ()
    | Comment _ -> ()

  (* Checks, using heuristics, if a condition is an opaque predicate *)
  let is_opaque b e condition formula =
    let old_state = Env.local e in
    let raw_cond, _ = Bbsse_smt.Translate.expr' old_state condition in
    (* Makes the condition equals true *)
    let condition = Formula.(mk_bv_equal raw_cond mk_bv_one) in

    let v_set = ref (Formula_utils.bl_term_variables condition) in

    let opt_formula =
      Formula_transformation.optimize ?keep:(Some !v_set) formula
    in

    let old_v_set = ref !v_set in
    let leave = ref false in
    while Formula.VarSet.is_empty !v_set = false && !leave = false do
      Sequence.iter_backward (visit_entry b v_set) opt_formula.Formula.entries;
      match Formula.VarSet.equal !old_v_set !v_set with
      | true -> leave := true
      | false -> old_v_set := !v_set
    done

  (* When all paths are done, it terminates the current jump *)
  let halt e =
    Logger.info "@[<v 0>@[<v 2>SMT queries@,%a@]@,@[<v 2>Exploration@,%a@]@,@]"
      Bbsse_smt.Query_stats.pp () Stats.pp ();
    if Sse_options.Dot_filename_out.is_set () then (
      let filename = Sse_options.Dot_filename_out.get () in
      Logger.info "Outputting CFG in %s" filename;
      let oc = open_out_bin filename in
      let cfg = e.Env.cfg in
      (match C.mem_vertex_a cfg e.Env.entrypoint with
      | None -> ()
      | Some entry -> C.output_graph oc e.Env.cfg [] ~entry);
      close_out oc)

  let sat_status p = fst @@ Bbsse_smt.Solver.check_satistifiability p

  type path_directive = Continue | Discard

  let add_jump_enum g vaddr =
    let instruction = fst (Disasm_core.decode vaddr) in
    let instr = instruction.Instruction.dba_block in
    match Dhunk.inst instr 0 with
    | Some (Dba.Instr.If (condition, _, _)) -> (
        Logger.debug "     Adding Enumerate to 0x%a" Virtual_address.pp vaddr;
        let loc = Dba_utils.Expr.of_vaddr vaddr in
        let dir_enum = Directive.enumerate ?n:(Some 2) condition ~loc () in
        Stats.set_cur_jmp_cond (Some condition);
        G.Directives.incr_todo g;

        match G.Directives.at vaddr g with
        | None ->
            let q = Queue.create () in
            Queue.add dir_enum q;
            G.Directives.update vaddr q g
        | Some q ->
            Queue.add dir_enum q;
            G.Directives.update vaddr q g)
    | None -> failwith "Cannot get instruction [add_jump_enum]"
    | _ -> Logger.debug "%s" "   Not an IF ..."

  (* Time_e -> entrypoints lookup time
   * Time_b -> BB-SSE time
   * Time_s -> Solver time *)
  let get_time_e jump =
    Stats.set_t_end_e (Unix.gettimeofday ());

    let t_start_e = Stats.get_t_start_e () in
    let t_end_e = Stats.get_t_end_e () in

    let times_e = Stats.get_times_e () in
    Stats.set_t_start_e 0.;
    Stats.set_t_end_e 0.;

    match Virtual_address.Htbl.find_opt times_e jump with
    | Some t ->
        Logger.debug "%s" "   Replaced E";
        Virtual_address.Htbl.replace times_e jump (t +. (t_end_e -. t_start_e))
    | None ->
        Logger.debug "%s" "   First time E";
        Virtual_address.Htbl.add times_e jump (t_end_e -. t_start_e)

  let get_time_b jump =
    Stats.set_t_end_b (Unix.gettimeofday ());

    let t_start_b = Stats.get_t_start_b () in
    let t_end_b = Stats.get_t_end_b () in

    let times_b = Stats.get_times_b () in
    Stats.set_t_start_b 0.;
    Stats.set_t_end_b 0.;

    match Virtual_address.Htbl.find_opt times_b jump with
    | Some t ->
        Logger.debug "%s" "   Replaced B";
        Virtual_address.Htbl.replace times_b jump (t +. (t_end_b -. t_start_b))
    | None ->
        Logger.debug "%s" "   First time B";
        Virtual_address.Htbl.add times_b jump (t_end_b -. t_start_b)

  let get_time_s jump =
    Stats.set_t_end_s (Unix.gettimeofday ());

    let t_start_s = Stats.get_t_start_s () in
    let t_end_s = Stats.get_t_end_s () in

    let times_s = Stats.get_times_s () in
    Stats.set_t_start_s 0.;
    Stats.set_t_end_s 0.;

    match Virtual_address.Htbl.find_opt times_s jump with
    | Some t ->
        Logger.debug "%s" "   Replaced S";
        Virtual_address.Htbl.replace times_s jump (t +. (t_end_s -. t_start_s))
    | None ->
        Logger.debug "%s" "    First time S";
        Virtual_address.Htbl.add times_s jump (t_end_s -. t_start_s)

  let get_time jump =
    Stats.set_t_end (Unix.gettimeofday ());

    let t_start = Stats.get_t_start () in
    let t_end = Stats.get_t_end () in

    let times = Stats.get_times () in
    Stats.set_t_start 0.;
    Stats.set_t_end 0.;

    match Virtual_address.Htbl.find_opt times jump with
    | Some t ->
        Logger.debug "%s" "   Replaced";
        Virtual_address.Htbl.replace times jump (t +. (t_end -. t_start))
    | None ->
        Logger.debug "%s" "   First time";
        Virtual_address.Htbl.add times jump (t_end -. t_start)

  (* Entrypoints lookup timer *)
  let start_timer_e () =
    Stats.set_t_start_e (Unix.gettimeofday ());
    Logger.debug "%s" "Started timer E ..."

  (* BB-SSE timer *)
  let start_timer_b () =
    Stats.set_t_start_b (Unix.gettimeofday ());
    Logger.debug "%s" "Started timer B ..."

  (* Solver timer *)
  let start_timer_s () =
    Stats.set_t_start_s (Unix.gettimeofday ());
    Logger.debug "%s" "Started timer S ..."

  (* Global timer *)
  let start_timer () =
    Stats.set_t_start (Unix.gettimeofday ());
    Logger.debug "%s" "Started timer ..."

  (* Look for if then else jumps addresses :
     * True_addr : addr to jump when condition is true
     * False_addr : addr to jump when condition is false *)
  let get_ite dhunk =
    let start_node = Dhunk.start dhunk in
    let addresses = Dhunk.outer_jumps dhunk in
    let false_addr = ref (Virtual_address.of_string "0x00000000") in
    let true_addr = ref (Virtual_address.of_string "0x00000000") in

    let get_false_addr true_addr addr =
      match Virtual_address.equal true_addr addr with
      | true -> ()
      | false -> false_addr := addr
    in

    let aux_get_jump jump_target =
      match jump_target with
      | Dba.JInner _ -> failwith " Got JInner [get_ite]"
      | Dba.JOuter true_address -> (
          true_addr := true_address.base;
          match Virtual_address.Set.cardinal addresses with
          | 2 -> Virtual_address.Set.iter (get_false_addr !true_addr) addresses
          | _ -> failwith "Ite outer jumps must be 2")
    in

    let rec aux_find_ite node =
      match Dhunk.Node.inst node with
      | None -> failwith "Undecoded instruction [get_ite]"
      | Some dba_instr -> (
          match dba_instr with
          | Dba.Instr.If (_, jump_target, _) ->
              Logger.debug "%s" "ITE found !";
              aux_get_jump jump_target
          | _ -> List.iter aux_find_ite (Dhunk.succ dhunk node))
    in
    aux_find_ite start_node;

    match
      Virtual_address.equal !false_addr (Virtual_address.of_string "0x00000000")
      || Virtual_address.equal !false_addr
           (Virtual_address.of_string "0x00000000")
    with
    | true -> failwith "false_addr or true_addr not found [get_ite]"
    | false -> (!true_addr, !false_addr)

  (* Return the direction (true, flase) taken by a jump
   * For OPAQUE jumps, only 1 result ( true or false ) should be returned
   * Else, 2 results are returned *)
  let get_next_direction b =
    let is_true = ref false in
    let is_false = ref false in
    let aux x =
      match x with true -> is_true := true | false -> is_false := true
    in

    List.iter aux b.possible_directions;
    match (!is_true, !is_false) with
    | true, false -> (1, true)
    | true, true -> (2, true)
    | false, true -> (1, false)
    | false, false -> (2, false)

  let get_condition_value vs =
    match vs with
    | [] -> failwith "Should not happen [get_condition_value]"
    | [ b ] -> Bitvector.to_uint b = 1
    | _ -> failwith "Should not happen [get_condition_value]"

  let loop_until ~halt g b =
    let get_vaddr e = Path_state.virtual_address @@ Env.local e in
    (* Take a path *)
    let e = Env.get_path g in
    (* Previous address *)
    let last_vaddr = ref (get_vaddr e) in

    let rec loop_aux e =
      let vaddr = get_vaddr e in
      (* if it is a new address *)
      if vaddr <> !last_vaddr then (
        Logger.debug ~level:2 "%@%a %a" Virtual_address.pp vaddr Mnemonic.pp
          (Env.local e |> Path_state.inst |> Instruction.mnemonic);
        last_vaddr := vaddr;
        let nb_cur_instr = Stats.get_nb_cur_instr () in
        Stats.set_nb_cur_instr (nb_cur_instr + 1);

        do_directives vaddr e
        (* When the last virtual addresse has not changed, we are still in the
           same DBA block, hence no user action can have been performed.
           So, we just continue.
        *))
      else reloop e Continue
    (* Recursively reloop until the end of the analyzis, taking into account
     * the different directives *)
    and reloop e directive =
      (* Stopping the current analysis if no directive is present  *)
      if (not @@ G.Directives.has (Env.global e)) && not b.is_jump_lookup then
        halt e
      else
        match directive with
        | Continue -> (
            (* Execute the next atomic step *)
            match Eval.go b e with
            (* If everything is ok, continue *)
            | Some e -> loop_aux e
            | None -> halt e
            | exception G.Path.Empty_worklist ->
                failwith "exception G.Path.Empty_worklist")
        (* Finish this path *)
        | Discard -> halt e
    and do_directives vaddr e =
      let glob = Env.global e in

      match G.Directives.at vaddr g with
      (* If no more directives : reloop *)
      | None -> reloop e Continue
      | Some q ->
          let open Directive in
          (* q' is the new queue that will replace the old one.
             Uses side effects. *)
          let q' = Queue.create () in
          let rec handle_directives e path_directive =
            if Queue.is_empty q then (
              if Queue.is_empty q' then G.Directives.remove vaddr glob
                (* Update Queue *)
              else G.Directives.update vaddr q' glob;
              reloop e path_directive)
            else
              (* Take next directive from Queue and apply it *)
              let g = Queue.take q in
              let p = Env.local e in
              match directive g with
              | Reach (c, _, _) -> (
                  Logger.debug "-> Reach";
                  let check_if_opaque () =
                    let frml =
                      Sse_symbolic.State.formula
                        (Path_state.symbolic_state (Env.local e))
                    in

                    match Stats.get_cur_jmp_cond () with
                    | None -> ()
                    | Some cond -> is_opaque b e cond frml
                  in

                  check_if_opaque ();

                  match b.gen_g_t with
                  | true -> (
                      match b.is_next_op with
                      | true ->
                          Logger.info "This address 0x%a MUST be opaque"
                            Virtual_address.pp vaddr;
                          Stats.add_g_t_set vaddr
                      | false -> ())
                  | false ->
                      (* Stop BB-SSE timer then
                       * Start solver timer *)
                      get_time_b vaddr;
                      start_timer_s ();

                      let c' = Count.decr c in
                      Logger.result
                        "@[<h>Directive :: reached address %a (%a to go)@]"
                        Virtual_address.pp vaddr Count.pp c';

                      (* STOP timer *)
                      get_time_s vaddr;
                      (match c' with
                      (* Do not add it to q', this is done*)
                      | Count.Count 0 -> ()
                      | _ -> failwith "Reach must have n = 1, not more");

                      let conds = Stats.get_paths_per_jump () in

                      (* Increases jump's paths *)
                      (match Virtual_address.Htbl.find_opt conds vaddr with
                      | Some i ->
                          Virtual_address.Htbl.replace conds vaddr (i + 1)
                      | None -> Virtual_address.Htbl.replace conds vaddr 1);

                      let entrypoint = Stats.get_entrypoint () in

                      (* Counting number of jump's entrypoints *)
                      (match
                         List.mem entrypoint
                           (Virtual_address.Htbl.find_all
                              (Stats.get_jmp_entry ()) vaddr)
                       with
                      | true -> ()
                      | false -> (
                          Virtual_address.Htbl.add (Stats.get_jmp_entry ())
                            vaddr entrypoint;
                          match
                            Virtual_address.Htbl.find_opt
                              (Stats.get_entrypoint_per_jump ())
                              vaddr
                          with
                          | Some i ->
                              Virtual_address.Htbl.replace
                                (Stats.get_entrypoint_per_jump ())
                                vaddr (i + 1)
                          | None ->
                              Virtual_address.Htbl.replace
                                (Stats.get_entrypoint_per_jump ())
                                vaddr 1));

                      Stats.add_nb_instr (Stats.get_nb_cur_instr ());
                      handle_directives e Discard)
              (* ------------------------------------------------------------------------ *)
              | Enumerate (k, ex) ->
                  let e_fml, p = Bbsse_smt.Translate.expr' p ex in

                  let enumerate_at_most k =
                    (* Starting solver timer *)
                    start_timer_s ();
                    let bv_vs, _p =
                      Bbsse_smt.Solver.enumerate_values k e_fml p
                    in
                    get_time_s vaddr;

                    G.Directives.Enumeration.record vaddr ex bv_vs glob;

                    (* Number of value to enumerate *)
                    let n = G.Directives.Enumeration.count vaddr ex glob in

                    (* Values to enumerate *)
                    let vs = G.Directives.Enumeration.get vaddr ex glob in

                    Logger.result
                      "@[<hov 0>Directive :: enumerate@ possible values (%d) \
                       for %a %@ %a:@ @[<hov 0>%a@]@]"
                      n Dba_printer.EICAscii.pp_bl_term ex Virtual_address.pp
                      vaddr
                      (Print_utils.pp_list ~sep:",@ " Bitvector.pp)
                      vs;

                    let instr = Disasm_core.decode vaddr |> fst in
                    let mnemonic = Instruction.mnemonic instr in

                    let add_if_op () =
                      match b.is_next_op with
                      | true ->
                          Logger.info "[ENUM] This address 0x%a MUST be opaque"
                            Virtual_address.pp vaddr
                      | false -> ()
                    in
                    add_if_op ();

                    match (n, vaddr) with
                    | 1, addr when Virtual_address.equal addr b.dest_addr = true
                      -> (
                        (* Add kind of branch :
                         * Right / Left *)
                        let vtable = Stats.get_right_left_conds () in

                        match
                          ( Virtual_address.Htbl.find_opt vtable vaddr,
                            List.length vs = 1 )
                        with
                        (* If it is the same branch  *)
                        | Some x, true when Bitvector.equal x (List.hd vs) -> (
                            Logger.result "Predicate %a at 0x%a %s" Mnemonic.pp
                              mnemonic Virtual_address.pp vaddr
                              "is OPAQUE (BRANCHES) - same branch";
                            b.possible_directions <-
                              get_condition_value vs :: b.possible_directions;
                            match
                              PredicatSet.mem vaddr
                                (Stats.get_predicates_opaque ())
                            with
                            | true -> n
                            | false ->
                                Stats.add_opaque_predicate ();
                                Stats.add_predicate_opaque vaddr;
                                n)
                        (* If it is a new branch, both paths are doable *)
                        | Some x, true when not (Bitvector.equal x (List.hd vs))
                          -> (
                            Virtual_address.Htbl.add vtable vaddr (List.hd vs);
                            Logger.result "Predicate %a at 0x%a is %s"
                              Mnemonic.pp mnemonic Virtual_address.pp vaddr
                              "maybe CLEAR - other branch found ";
                            b.take_next <- true;
                            b.possible_directions <-
                              get_condition_value vs :: b.possible_directions;
                            match
                              PredicatSet.mem vaddr
                                (Stats.get_predicates_clear ())
                            with
                            | true -> n
                            | false ->
                                Stats.add_clear_predicate ();
                                Stats.add_predicate_clear vaddr;
                                n)
                        (* First time saying OPAQUE *)
                        | _, true -> (
                            Virtual_address.Htbl.add vtable vaddr (List.hd vs);
                            Logger.result "Predicate %a at 0x%a %s" Mnemonic.pp
                              mnemonic Virtual_address.pp vaddr
                              "is OPAQUE (BRANCHES) - first time";
                            b.possible_directions <-
                              get_condition_value vs :: b.possible_directions;
                            match
                              PredicatSet.mem vaddr
                                (Stats.get_predicates_opaque ())
                            with
                            | true -> n
                            | false ->
                                Stats.add_opaque_predicate ();
                                Stats.add_predicate_opaque vaddr;
                                n)
                        | _, false -> failwith "bitvector list length must be 1"
                        )
                    (* If 2 or more disctinct enumerations *)
                    | x, addr
                      when x > 1
                           && Virtual_address.equal addr b.dest_addr = true -> (
                        Logger.result "Predicate %a at 0x%a is %s" Mnemonic.pp
                          mnemonic Virtual_address.pp vaddr "maybe CLEAR ";
                        b.take_next <- true;
                        b.possible_directions <-
                          false :: true :: b.possible_directions;
                        match
                          PredicatSet.mem vaddr (Stats.get_predicates_clear ())
                        with
                        | true -> n
                        | false ->
                            Stats.add_clear_predicate ();
                            Stats.add_predicate_clear vaddr;
                            n)
                    (* If no enumeration, unreachable path *)
                    | x, addr
                      when x = 0
                           && Virtual_address.equal addr b.dest_addr = true -> (
                        Logger.result "Predicate %a @ %a is %s" Mnemonic.pp
                          mnemonic Virtual_address.pp vaddr "is UNREACHABLE ";
                        Logger.info "x : %d & addr : %a" x Virtual_address.pp
                          addr;
                        Stats.add_unfeasible_paths ();
                        match
                          PredicatSet.mem vaddr
                            (Stats.get_predicates_unfeasible ())
                        with
                        | true -> n
                        | false ->
                            Stats.add_unfeasible_predicate ();
                            Stats.add_predicate_unfeasible vaddr;
                            n)
                    | x, addr ->
                        Logger.debug "x : %d et addr : %a" x Virtual_address.pp
                          addr;
                        failwith
                          "Default case must not happen ( check options and \
                           config file)"
                  in

                  (let m = k - enumerate_at_most k in
                   if m > 0 then
                     let loc = Dba_utils.Expr.of_vaddr vaddr in
                     Queue.add (Directive.enumerate ~n:m ex ~loc ()) q');
                  handle_directives e path_directive
              (* ------------------------------------------------------------------------ *)
              | Assume expr ->
                  let p =
                    (* add comment *)
                    let comment =
                      Format.asprintf "@[<h>user constraint : assume %a @]"
                        Dba_printer.EICAscii.pp_bl_term expr
                    in
                    Logger.debug "Assume %@ %a" Virtual_address.pp vaddr;
                    let symb_state =
                      Path_state.symbolic_state p
                      |> Sse_symbolic.State.comment comment
                    in
                    Path_state.set_symbolic_state symb_state p
                  in
                  (* Adding the formula itself *)
                  let local = Bbsse_smt.Translate.assume expr p in
                  let e = Env.create ~global:glob ~local in
                  Queue.add g q';
                  handle_directives e path_directive
              (* ------------------------------------------------------------------------ *)
              | Cut _ ->
                  Queue.add g q';
                  Queue.clear q;
                  Logger.result "@[<h>Directive :: cut %@ %a@]"
                    Virtual_address.pp vaddr;
                  handle_directives e Discard
                  (* ------------------------------------------------------------------------ *)
              | _ -> failwith "Should not happen [Bad directive]"
          in
          handle_directives e Continue
    in

    loop_aux e

  let interval_or_set_to_cond expr is =
    let open Parse_helpers.Initialization in
    match is with
    | Nondet | Singleton _ -> assert false
    | Signed_interval (e1, e2) ->
        Dba.Expr.logand (Dba.Expr.sle e1 expr) (Dba.Expr.sle expr e2)
    | Unsigned_interval (e1, e2) ->
        Dba.Expr.logand (Dba.Expr.ule e1 expr) (Dba.Expr.ule expr e2)
    | Set l -> (
        match l with
        | [] -> assert false
        | a :: b ->
            let f = Dba.Expr.equal expr in
            List.fold_left (fun acc e -> Dba.Expr.logor acc @@ f e) (f a) b)

  let initialize_state ~filename ps =
    let ps =
      let cli_counters = Bbsse_options.Visit_address_counter.get () in
      match cli_counters with
      | [] -> ps
      | cs ->
          Logger.info "Found some address counters ... great";
          let m =
            let open! Virtual_address in
            List.fold_left
              (fun m c -> Map.add c.Bbsse_options.Address_counter.address c m)
              Map.empty cs
          in
          Path_state.set_address_counters m ps
    in
    if not (Sys.file_exists filename) then (
      Logger.warning "Cannot find sse configuration file %s" filename;
      ps)
    else
      let initials =
        Logger.debug "Reading initialization from %s" filename;
        let parser = Parser.initialization and lexer = Lexer.token in
        Parse_utils.read_file ~parser ~lexer ~filename
      in
      let f ps init =
        let open Parse_helpers.Initialization in
        match init.operation with
        | Mem_load (Dba.Expr.Cst addr, size) ->
            Path_state.with_init_mem_at ps ~addr ~size
        | Mem_load (_, _) -> assert false
        | Universal lval -> (
            match Dba_types.LValue.name_of lval with
            | Some name ->
                let symb = Path_state.symbolic_state ps in
                let size = Dba.LValue.size_of lval in
                let sort = Formula.BvSort size in
                let symb =
                  Sse_symbolic.State.declare ~wild:true name sort symb
                in
                Path_state.set_symbolic_state symb ps
            | None -> ps)
        | Assumption cond -> Bbsse_smt.Translate.assume cond ps
        | Assignment (lval, rval, naming_hint) -> (
            let wild = not init.controlled in
            match rval with
            | Singleton rv -> Bbsse_smt.Translate.assignment ~wild lval rv ps
            | x ->
                let state =
                  Bbsse_smt.Translate.nondet ~wild ?naming_hint lval ps
                in
                let e = Dba.LValue.to_expr lval in
                let cond = interval_or_set_to_cond e x in
                Bbsse_smt.Translate.assume cond state)
      in
      List.fold_left f ps initials

  let vertex_from_string str =
    let addr = Virtual_address.of_string str in
    Ghidra_cfg.V.create addr

  (* Add Reach directive to stop on it *)
  let stop_at vaddr g =
    let loc = Dba_utils.Expr.of_vaddr vaddr in
    let dir_reach = Directive.reach ?n:(Some 1) ~loc () in
    Logger.debug "     Adding Reach to 0x%a" Virtual_address.pp vaddr;
    G.Directives.incr_todo g;

    match G.Directives.at vaddr g with
    | None ->
        let q = Queue.create () in
        Queue.add dir_reach q;
        G.Directives.update vaddr q g
    | Some q ->
        Queue.add dir_reach q;
        G.Directives.update vaddr q g

  let dump_dyn_jumps stack =
    let print_elt x = Logger.debug "   %s" (Bitvector.to_hexstring x) in
    Stack.iter print_elt stack

  let dump_choices stack =
    let print_bool b =
      match b with
      | true -> Logger.debug "   %s" "True"
      | false -> Logger.debug "   %s" "False"
    in
    Stack.iter print_bool stack

  let do_bb_sse_on_jump jump g_cfg next_table types_table filename =
    let level = 3 in

    start_timer_e ();
    let v = Ghidra_cfg.V.create jump in
    (* Automated initialization *)
    let b = reinit_backward v g_cfg next_table types_table filename in
    get_time_e jump;
    b.possible_directions <- [];

    let aux_remove_dead_edges addr_1 addr_2 =
      Ghidra_cfg.rem_edge addr_1 addr_2 g_cfg
    in
    let aux_add_cycle_paths good_addr =
      while Stack.is_empty b.cycle_entrypoints = false do
        let entrypoint = Stack.pop b.cycle_entrypoints in
        let choices = Stack.pop b.cycle_all_choices in
        let dyn_jmps = Stack.pop b.cycle_all_dyn_jumps in

        match Virtual_address.equal good_addr (fst entrypoint) with
        | true ->
            Logger.debug "Taking into account %a" Virtual_address.pp
              (fst entrypoint);
            Stack.push entrypoint b.entrypoints;
            Stack.push choices b.all_choices;
            Stack.push dyn_jmps b.all_dyn_jumps
        | false -> ()
      done
    in

    let rec do_bb_sse_aux global =
      start_timer_b ();
      (match Stack.is_empty b.entrypoints with
      | false -> ()
      | true -> (
          (* Adding or not cylcic paths ... *)
          let addr = Ghidra_cfg.V.label v in
          let instr = Disasm_core.decode addr |> fst in
          let dhunk = instr.dba_block in
          let true_addr, false_addr = get_ite dhunk in

          Logger.debug "True_addr : %a" Virtual_address.pp true_addr;
          Logger.debug "False_addr : %a" Virtual_address.pp false_addr;
          match get_next_direction b with
          | 1, true ->
              Logger.debug "%s" "OP TRUE";
              aux_add_cycle_paths true_addr;
              aux_remove_dead_edges jump false_addr
          | 1, false ->
              Logger.debug "%s" "OP FALSE";
              aux_add_cycle_paths false_addr;
              aux_remove_dead_edges jump true_addr
          | _ -> Logger.debug "%s" "CLEAR or other ..."));
      if Stack.is_empty b.entrypoints = false && b.take_next = false then (
        Stats.add_path ();
        let entrypoint = get_entrypoint b in
        Stats.set_entrypoint (fst entrypoint);
        b.current_choices <- get_choices b;
        b.current_dyn_jumps <- get_dyn_jumps b;

        Logger.debug ~level "Starting BB-SSE from %a" Virtual_address.pp
          (fst entrypoint);
        Logger.debug "   Dynamic Jumps : ";
        dump_dyn_jumps b.current_dyn_jumps;
        Logger.debug "   Choices : ";
        dump_choices b.current_choices;

        let initialize_fun =
          initialize_state ~filename:(Sse_options.MemoryFile.get ())
        in
        Logger.debug ~level "Initialization done ...";
        Logger.debug ~level "Driver set ...";

        match global with
        | None ->
            let g =
              G.from_address ~block_index:(snd entrypoint) ~initialize_fun
                ~entrypoint:(fst entrypoint)
            in
            (* Adds
             * Reach directive
             * Enumerate directive *)
            stop_at jump g;
            add_jump_enum g jump;
            loop_until ~halt g b;
            do_bb_sse_aux (Some g)
        | Some glob ->
            let g =
              G.reinitialize ~block_index:(snd entrypoint) ~initialize_fun
                ~entrypoint:(fst entrypoint) glob
            in
            stop_at jump g;
            add_jump_enum g jump;
            loop_until ~halt g b;
            do_bb_sse_aux (Some g))
    in
    do_bb_sse_aux None

  (* Filtering results :
   * OPAQUE + CLEAR = CLEAR
   * UNREACHABLE + CLEAR = CLEAR
   * UNREACHABLE + OPAQUE = OPAQUE
   * UNKNOWN + * = UNKNOWN *)
  let filter_set set_o set_c set_unf =
    let filter_set_aux_unf addr =
      match
        (PredicatSet.find_opt addr !set_o, PredicatSet.find_opt addr !set_c)
      with
      | None, None -> ()
      | _, _ ->
          set_unf := PredicatSet.remove addr !set_unf;
          Stats.remove_unfeasible_predicate ();
          Logger.debug "[0x%a] UNREACHABLE deleted" Virtual_address.pp addr
    in

    let filter_set_aux_o addr =
      match
        (PredicatSet.find_opt addr !set_o, PredicatSet.find_opt addr !set_c)
      with
      | Some _, Some _ ->
          set_o := PredicatSet.remove addr !set_o;
          Logger.debug "[0x%a] OPAQUE -> CLEAR" Virtual_address.pp addr;
          Stats.remove_opaque_predicate ()
      | _, _ -> ()
    in

    PredicatSet.iter filter_set_aux_unf !set_unf;
    PredicatSet.iter filter_set_aux_o !set_o

  (* Pretty printing timers *)
  let pp_times times =
    let min = ref 100. in
    let max = ref 0. in
    let counter = ref 0. in
    let avg = ref 0. in
    let sum = ref 0. in

    let iter_aux _ t =
      counter := !counter +. 1.;
      sum := !sum +. t;
      (match Float.compare t !min with x when x < 0 -> min := t | _ -> ());

      match Float.compare t !max with x when x > 0 -> max := t | _ -> ()
    in

    Virtual_address.Htbl.iter iter_aux times;
    avg := !sum /. !counter;
    Logger.debug "Min : %f" !min;
    Logger.debug "Max : %f" !max;
    Logger.debug "Avg : %f" !avg

  let pp_conds_entries i_htbl =
    let min = ref 1000 in
    let max = ref 0 in
    let counter = ref 0 in
    let avg = ref 0. in
    let sum = ref 0 in

    let iter_aux _ i =
      counter := !counter + 1;
      sum := !sum + i;

      (match i > !max with true -> max := i | false -> ());

      match i < !min with true -> min := i | false -> ()
    in

    Virtual_address.Htbl.iter iter_aux i_htbl;
    avg := Float.of_int !sum /. Float.of_int !counter;
    Logger.debug "Min : %d" !min;
    Logger.debug "Max : %d" !max;
    Logger.debug "Avg : %f" !avg

  let pp_instr stack =
    let min = ref 1000 in
    let max = ref 0 in
    let counter = ref 0 in
    let avg = ref 0. in
    let sum = ref 0 in

    let iter_aux n =
      incr counter;
      sum := !sum + n;
      (match n > !max with true -> max := n | false -> ());

      match n < !min with true -> min := n | false -> ()
    in

    Stack.iter iter_aux stack;
    avg := Float.of_int !sum /. Float.of_int !counter;
    Logger.debug "Min : %d" !min;
    Logger.debug "Max : %d" !max;
    Logger.debug "Avg : %f" !avg

  (* Printing final results *)
  let final_printing () =
    let old_set_u = Stats.get_predicates_unknown () in
    let old_set_o = Stats.get_predicates_opaque () in
    let old_set_c = Stats.get_predicates_clear () in
    let old_set_unf = Stats.get_predicates_unfeasible () in

    let set_u = ref old_set_u in
    let set_o = ref old_set_o in
    let set_c = ref old_set_c in
    let set_unf = ref old_set_unf in

    filter_set set_o set_c set_unf;

    (* Temporary file used to save the final results
     * It can be commented or deleted if not used*)
    let oc = open_out "/tmp/result" in

    let print_predicates_opaque p =
      let str =
        match Bbsse_options.OPFile.get_opt () with
        | None ->
            Stats.add_op_detected ();
            ""
        | Some _ -> (
            match
              Virtual_address.Set.find_opt p (Stats.get_must_be_opaque ())
            with
            | Some _ ->
                Stats.add_op_detected ();
                " [ must be OPAQUE]"
            | None ->
                Stats.add_other_op ();
                " [maybe CLEAR]")
      in
      Logger.debug "0x%a is OPAQUE %s" Virtual_address.pp p str;

      let s = Print_utils.string_from_pp Virtual_address.pp p in
      Printf.fprintf oc "0x%s is OPAQUE %s\n" s str
    in

    let print_predicates_clear p =
      let str =
        match Bbsse_options.OPFile.get_opt () with
        | None -> ""
        | Some _ -> (
            match
              Virtual_address.Set.find_opt p (Stats.get_must_be_opaque ())
            with
            | Some _ ->
                Stats.add_fn ();
                " [ must be OPAQUE]"
            | None -> " [maybe CLEAR]")
      in

      Logger.debug "0x%a may be CLEAR %s" Virtual_address.pp p str;

      let s = Print_utils.string_from_pp Virtual_address.pp p in
      Printf.fprintf oc "0x%s may be CLEAR %s\n" s str
    in

    let print_predicates_unknown p =
      let str =
        match Bbsse_options.OPFile.get_opt () with
        | None -> ""
        | Some _ -> (
            match
              Virtual_address.Set.find_opt p (Stats.get_must_be_opaque ())
            with
            | Some _ -> " [ must be OPAQUE]"
            | None -> " [maybe CLEAR]")
      in

      Logger.debug "0x%a is UNKNOWN %s" Virtual_address.pp p str;

      let s = Print_utils.string_from_pp Virtual_address.pp p in
      Printf.fprintf oc "0x%s is UNKNOWN %s\n" s str
    in

    let print_predicates_unfeasible p =
      let str =
        match Bbsse_options.OPFile.get_opt () with
        | None -> ""
        | Some _ -> (
            match
              Virtual_address.Set.find_opt p (Stats.get_must_be_opaque ())
            with
            | Some _ -> " [ must be OPAQUE]"
            | None -> " [maybe CLEAR]")
      in

      Logger.debug "0x%a is UNREACHABLE %s" Virtual_address.pp p str;

      let s = Print_utils.string_from_pp Virtual_address.pp p in
      Printf.fprintf oc "0x%s is UNREACHABLE %s\n" s str
    in

    PredicatSet.iter print_predicates_opaque !set_o;
    PredicatSet.iter print_predicates_clear !set_c;
    PredicatSet.iter print_predicates_unfeasible !set_unf;
    PredicatSet.iter print_predicates_unknown !set_u;
    close_out oc;

    Logger.result "@[<v 0>@[<v 2>Detection - Opaque predicates@,%a@]@,@]"
      Stats.pp_predicate ();
    Logger.debug "%s" "Time / condition : ";
    pp_times (Stats.get_times ());
    Logger.debug "%s" "Time [Entrypoint] ";
    pp_times (Stats.get_times_e ());
    Logger.debug "%s" "Time [BB-SSE] ";
    pp_times (Stats.get_times_b ());
    Logger.debug "%s" "Time [Solver] ";
    pp_times (Stats.get_times_s ());
    (match Bbsse_options.GenGroundTruth.get () with
    | true ->
        Logger.info "%s" "Must be OP : ";

        let filename =
          match Kernel_options.ExecFile.get_opt () with
          | Some s -> s
          | None -> failwith "Please specify an Execfile"
        in

        let oc = open_out ("oracle/" ^ filename) in
        let pp_addr x =
          Logger.info " -> 0x%a" Virtual_address.pp x;
          let str = Print_utils.string_from_pp Virtual_address.pp x in
          Printf.fprintf oc "0x%s\n" str
        in
        Virtual_address.Set.iter pp_addr (Stats.get_g_t_set ());
        close_out oc
    | false -> ());
    Logger.debug "Total paths : %d" (Stats.get_nb_paths ());

    Logger.debug "Paths : ";
    pp_conds_entries (Stats.get_paths_per_jump ());

    Logger.debug "Entrypoints : ";
    pp_conds_entries (Stats.get_entrypoint_per_jump ());

    Logger.debug "Instructions :";
    pp_instr (Stats.get_nb_instr ())

  let stack_to_set stack =
    let set = ref Virtual_address.Set.empty in
    let aux_add x = set := Virtual_address.Set.add x !set in
    Stack.iter aux_add stack;
    !set

  let do_bb_sse ~filename =
    let level = 3 in
    Logger.debug ~level "Running BB-SSE on %s" filename;
    let str =
      match Kernel_options.Entry_point.is_set () with
      | false -> failwith "Please specify a start address"
      | true -> Kernel_options.Entry_point.get ()
    in

    let v = vertex_from_string str in
    (* Look for entrypoints, choices ...
     * Main initialization function *)
    let b = init_backward v filename in

    (* Add cyclic paths *)
    let aux_add_cycle_paths good_addr =
      while Stack.is_empty b.cycle_entrypoints = false do
        let entrypoint = Stack.pop b.cycle_entrypoints in
        let choices = Stack.pop b.cycle_all_choices in
        let dyn_jmps = Stack.pop b.cycle_all_dyn_jumps in

        match Virtual_address.equal good_addr (fst entrypoint) with
        | true ->
            Stack.push entrypoint b.entrypoints;
            Stack.push choices b.all_choices;
            Stack.push dyn_jmps b.all_dyn_jumps
        | false -> ()
      done
    in

    let rec do_bb_sse_aux global =
      (* entrypoint + données utiles
       * de ce chemin *)
      (match Stack.is_empty b.entrypoints with
      | false -> ()
      | true -> (
          (* When all of the jump's entrypoints are done
           * get which branch(es) (True / False) are doable by the jump *)
          let addr = Ghidra_cfg.V.label v in
          let instr = Disasm_core.decode addr |> fst in
          let dhunk = instr.dba_block in
          let true_addr, false_addr = get_ite dhunk in

          Logger.debug "True_addr : %a" Virtual_address.pp true_addr;
          Logger.debug "False_addr : %a" Virtual_address.pp false_addr;
          match get_next_direction b with
          | 1, true ->
              Logger.debug "%s" "OP TRUE";
              aux_add_cycle_paths true_addr
          | 1, false ->
              Logger.debug "%s" "OP FALSE";
              aux_add_cycle_paths false_addr
          | _ -> Logger.debug "%s" "CLEAR or other ..."));
      match Stack.is_empty b.entrypoints with
      | true -> ()
      | false -> (
          let entrypoint = get_entrypoint b in
          b.current_choices <- get_choices b;
          b.current_dyn_jumps <- get_dyn_jumps b;

          Logger.debug ~level "Starting BB-SSE from %a" Virtual_address.pp
            (fst entrypoint);
          Logger.debug "   %s" "Dynamic Jumps : ";
          dump_dyn_jumps b.current_dyn_jumps;
          Logger.debug "   %s" "Choices : ";
          dump_choices b.current_choices;

          let initialize_fun =
            initialize_state ~filename:(Sse_options.MemoryFile.get ())
          in
          Logger.debug ~level "Initialization done ...";
          Logger.debug ~level "Driver set ...";

          let jump = Ghidra_cfg.V.label v in

          match global with
          | None ->
              (* 'global' is totally initialized on first launch *)
              let g =
                G.from_address ~block_index:(snd entrypoint) ~initialize_fun
                  ~entrypoint:(fst entrypoint)
              in
              stop_at jump g;
              add_jump_enum g jump;
              loop_until ~halt g b;
              do_bb_sse_aux (Some g)
          | Some glob ->
              (* else, all fields are updated except enumerations *)
              let g =
                G.reinitialize ~block_index:(snd entrypoint) ~initialize_fun
                  ~entrypoint:(fst entrypoint) glob
              in
              stop_at jump g;
              add_jump_enum g jump;
              loop_until ~halt g b;
              do_bb_sse_aux (Some g))
    in
    do_bb_sse_aux None;
    final_printing ()

  let do_bb_sse_auto ~filename =
    let level = 3 in
    Logger.debug ~level "Running BB-SSE [AUTO] on %s" filename;

    (match Bbsse_options.OPFile.get_opt () with
    | Some file ->
        Logger.debug "Reading addresses from %s" file;
        let ic = open_in file in
        List.iter (fun addr -> Stats.add_must_be_opaque addr)
        @@ Bbsse_parser.addresses Bbsse_lexer.token
        @@ Lexing.from_channel ic;
        close_in ic
    | None -> ());

    let g_cfg, next_table, types_table, jumps_to_explore =
      Ghidra_cfg.import ()
    in

    Stats.set_all_jumps jumps_to_explore;

    match jumps_to_explore with
    | None -> failwith "Please give correct arguments to -ghidra"
    | Some jumps ->
        (* Sorting jump addresses by ascending order *)
        let ordered_jumps = stack_to_set jumps in

        Logger.debug "Total jumps : %d" (Stack.length jumps);
        let process_jumps jump =
          Logger.info "Processing jump : %a" Virtual_address.pp jump;
          let do_jmp = ref true in
          (match Bbsse_options.IgnoreAddr.get_opt () with
          | Some file ->
              Logger.debug "Reading addresses from %s" file;
              let ic = open_in file in
              List.iter (fun addr ->
                  match Virtual_address.equal addr jump with
                  | true -> do_jmp := false
                  | false -> ())
              @@ Bbsse_parser.addresses Bbsse_lexer.token
              @@ Lexing.from_channel ic;
              close_in ic
          | None -> ());

          try
            match !do_jmp with
            | false -> ()
            | true ->
                Stats.set_nb_cur_instr 0;
                start_timer ();
                do_bb_sse_on_jump jump g_cfg next_table types_table filename;
                get_time jump
          with (* If division by zero, take next jump *)
          | Division_by_zero -> ()
        in
        Virtual_address.Set.iter process_jumps ordered_jumps;
        final_printing ()

  let start =
    let filename = Kernel_options.ExecFile.get () in

    match Bbsse_options.FindJumps.get () with
    | false -> do_bb_sse ~filename
    | true -> do_bb_sse_auto ~filename
end

let run () =
  if Bbsse_options.is_enabled () && Kernel_options.ExecFile.is_set () then
    let (module H) = (module Dfs_global : GLOBAL_ENV) in
    let module S = Backward (H) in
    S.start

let _ = Cli.Boot.enlist ~name:"BB-SSE" ~f:run
