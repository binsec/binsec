(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2022                                               *)
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

open Sse_types
open Sse_options

exception Halt

module String = struct
  include String
  include Basic_types.String
end

module OMap = Map.Make (struct
  type t = string

  let compare =
    let rec iter s s' i =
      let d =
        Char.code (String.unsafe_get s i) - Char.code (String.unsafe_get s' i)
      in
      if d = 0 && i > 0 then iter s s' (i - 1) else d
    in
    fun s s' -> iter s s' (min (String.length s) (String.length s') - 1)
end)

(* Enumerate jumps targets *)
let get_entry_point () =
  match Kernel_functions.get_ep () with
  | Some v -> v
  | None ->
      Kernel_functions.get_img () |> Loader.Img.entry |> Virtual_address.create

let pp_value_as format ppf bv =
  match format with
  | Directive.Action.Bin ->
      Format.pp_print_string ppf @@ Bitvector.to_bitstring bv
  | Directive.Action.Dec -> Z.pp_print ppf @@ Bitvector.signed_of bv
  | Directive.Action.Hex ->
      Format.pp_print_string ppf @@ Bitvector.to_hexstring bv
  | Directive.Action.Ascii ->
      Format.fprintf ppf "%S" @@ Bitvector.to_asciistring bv

module Vtbl = Hashtbl.Make (struct
  type t = Dba.Var.t

  let hash = Hashtbl.hash

  let equal = ( = )
end)

module Etbl = Hashtbl.Make (struct
  type t = Dba.Expr.t

  let hash = Hashtbl.hash

  let equal = Dba.Expr.is_equal
end)

module type SSE_RUNNER = sig
  val start : unit -> unit
end

module Env_make (S : functor (QS : QUERY_STATISTICS) -> STATE)
(WF : WORKLIST_FACTORY) : SSE_RUNNER = struct
  type time = { mutable sec : float }

  module Exploration_stats = struct
    let paths = ref 1

    let completed_paths = ref 0

    let unknown_paths = ref 0

    let total_asserts = ref 0

    let failed_asserts = ref 0

    let branches = ref 0

    let max_depth = ref 0

    let instructions = ref 0

    let unique_insts = ref 0

    let init_time = { sec = Unix.gettimeofday () }

    let _reset () =
      paths := 1;
      completed_paths := 0;
      unknown_paths := 0;
      total_asserts := 0;
      failed_asserts := 0;
      branches := 0;
      max_depth := 0;
      instructions := 0;
      unique_insts := 0;
      init_time.sec <- Unix.gettimeofday ()

    let time () = Unix.gettimeofday () -. init_time.sec

    let get_time = time

    let get_paths () = !paths

    let get_completed_paths () = !completed_paths

    let get_unknown_paths () = !unknown_paths

    let get_total_asserts () = !total_asserts

    let get_failed_asserts () = !failed_asserts

    let get_branches () = !branches

    let get_max_depth () = !max_depth

    let get_instructions () = !instructions

    let get_unique_insts () = !unique_insts

    let add_path () = incr paths

    let terminate_path () = incr completed_paths

    let interrupt_path () = incr unknown_paths

    let add_assert () = incr total_asserts

    let add_failed_assert () = incr failed_asserts

    let add_branch () = incr branches

    let update_depth d = if !max_depth < d then max_depth := d

    let add_instruction () = incr instructions

    let add_unique_inst () = incr unique_insts

    let pp ppf () =
      Format.fprintf ppf
        "@[<v 0>@[<h>total paths                      %d@]@,\
         @[<h>completed/cut paths              %d@]@,\
         @[<h>pending paths                    %d@]@,\
         @[<h>stale paths                      %d@]@,\
         @[<h>failed assertions                %d@]@,\
         @[<h>branching points                 %d@]@,\
         @[<h>max path depth                   %d@]@,\
         @[<h>visited instructions (unrolled)  %d@]@,\
         @]"
        !paths !completed_paths
        (!paths - !completed_paths - !unknown_paths)
        !unknown_paths !failed_asserts !branches !max_depth !instructions

    let _to_toml () =
      Toml.Min.of_key_values
        [
          (Toml.Min.key "paths", Toml.Types.TInt !paths);
          (Toml.Min.key "completed_paths", Toml.Types.TInt !completed_paths);
          (Toml.Min.key "unknown_paths", Toml.Types.TInt !unknown_paths);
          (Toml.Min.key "total_asserts", Toml.Types.TInt !total_asserts);
          (Toml.Min.key "failed_asserts", Toml.Types.TInt !failed_asserts);
          (Toml.Min.key "branches", Toml.Types.TInt !branches);
          (Toml.Min.key "max_depth", Toml.Types.TInt !max_depth);
          (Toml.Min.key "instructions", Toml.Types.TInt !instructions);
          (Toml.Min.key "unique_insts", Toml.Types.TInt !unique_insts);
        ]
  end

  module Query_stats = struct
    module Preprocess = struct
      let sat = ref 0

      let unsat = ref 0

      let const = ref 0

      let get_sat () = !sat

      let get_unsat () = !unsat

      let get_const () = !const

      let total () = !sat + !unsat + !const

      let incr_sat () = incr sat

      let incr_unsat () = incr unsat

      let incr_const () = incr const

      let reset () =
        sat := 0;
        unsat := 0;
        const := 0

      let pp ppf () =
        let open Format in
        fprintf ppf
          "@[<v 2>@[<h>Preprocessing simplifications@]@,\
           @[<h>total          %d@]@,\
           @[<h>sat            %d@]@,\
           @[<h>unsat          %d@]@,\
           @[<h>constant enum  %d@]@]" (total ()) !sat !unsat !const

      let to_toml () =
        Toml.Min.of_key_values
          [
            (Toml.Min.key "sat", Toml.Types.TInt !sat);
            (Toml.Min.key "unsat", Toml.Types.TInt !unsat);
            (Toml.Min.key "const", Toml.Types.TInt !const);
          ]
    end

    module Solver = struct
      let sat = ref 0

      let unsat = ref 0

      let err = ref 0

      let get_sat () = !sat

      let get_unsat () = !unsat

      let get_err () = !err

      let incr_sat () = incr sat

      let incr_unsat () = incr unsat

      let incr_err () = incr err

      let total_time = { sec = 0. }

      let timer = { sec = 0. }

      let running = ref false

      let reset () =
        sat := 0;
        unsat := 0;
        err := 0;
        total_time.sec <- 0.;
        running := false

      let start_timer () =
        timer.sec <- Unix.gettimeofday ();
        running := true

      let stop_timer () =
        running := false;
        let elapsed = Unix.gettimeofday () -. timer.sec in
        total_time.sec <- total_time.sec +. elapsed

      let total () = !sat + !unsat + !err

      let total_time () =
        if !running then
          let elapsed = Unix.gettimeofday () -. timer.sec in
          total_time.sec +. elapsed
        else total_time.sec

      let get_time = total_time

      let pp ppf () =
        let open Format in
        let time = total_time () and queries = total () in
        fprintf ppf
          "@[<v 2>@[<h>Satisfiability queries@]@,\
           @[<h>total          %d@]@,\
           @[<h>sat            %d@]@,\
           @[<h>unsat          %d@]@,\
           @[<h>unknown        %d@]@,\
           @[<h>time           %.2f@]@,\
           @[<h>average        %.2f@]@]" (total ()) !sat !unsat !err time
          (time /. float queries)

      let to_toml () =
        Toml.Min.of_key_values
          [
            (Toml.Min.key "sat", Toml.Types.TInt !sat);
            (Toml.Min.key "unsat", Toml.Types.TInt !unsat);
            (Toml.Min.key "err", Toml.Types.TInt !err);
            (Toml.Min.key "time", Toml.Types.TFloat (total_time ()));
          ]
    end

    let _reset () =
      Preprocess.reset ();
      Solver.reset ()

    let pp ppf () =
      let open Format in
      fprintf ppf "@[<v 0>%a@,@,%a@,@]" Preprocess.pp () Solver.pp ()
  end

  module Screen = Sse_screen.Make (Exploration_stats) (Query_stats)
  module State = S (Query_stats)
  module Path_state = Path_state (State)
  module W = WF (Path_state)

  let assertion_failure session ps =
    Logger.error "@[<v 2> Assertion failed %@ %a@ %a@]" Path_state.pp_loc ps
      State.pp session;
    Exploration_stats.add_failed_assert ()

  module Env = struct
    type p = RX of { base : Virtual_address.t; content : Loader_buf.t } | RWX

    type t = {
      mutable client : Subprocess.t option;
      mutable xcode : p Imap.t;
      mutable entrypoint : Virtual_address.t;
      mutable prehook : Dhunk.t;
      rocfg : C.t;
      wcfg : Instruction.t OMap.t Virtual_address.Htbl.t;
      mutable worklist : W.t;
      cut : Dba.Expr.t list Virtual_address.Htbl.t;
      choice : Directive.Choice.t Virtual_address.Htbl.t;
      assume : Dba.Expr.t list Virtual_address.Htbl.t;
      dyn_assert : Dba.Expr.t list Virtual_address.Htbl.t;
      reach :
        (Directive.Count.t * Dba.Expr.t * Directive.Action.t list) Queue.t
        Virtual_address.Htbl.t;
      mutable reach_all : bool;
      enumerate : (int * Dba.Expr.t) Queue.t Virtual_address.Htbl.t;
      enumerations : Bitvector.t list Etbl.t Virtual_address.Htbl.t;
    }

    let create entrypoint =
      let img = Kernel_functions.get_img () in
      let transient = TransientEnum.get () > 0 in
      let approx_size, xcode =
        Array.fold_left
          (fun (sum, xcode) s ->
            if Loader.Section.has_flag Loader_types.Exec s then
              let { Loader_types.virt = pos; _ } = Loader.Section.pos s in
              let { Loader_types.virt = size; _ } = Loader.Section.size s in
              let virt, p =
                if Loader.Section.has_flag Loader_types.Write s then
                  ( 0,
                    if transient then (
                      Logger.debug ~level:4
                        "Section %S [%a, 0x%x] has both Write and Execute \
                         flags."
                        (Loader.Section.name s) Virtual_address.pp
                        (Virtual_address.create pos)
                        size;
                      RWX)
                    else (
                      Logger.warning
                        "Section %S [%a, 0x%x] has both Write and Execute \
                         flags.@ Self-modifying code is disabled and writes \
                         will be ignored.@ Use '-sse-self-written-enum N' to \
                         enable symbolic reasoning up to 'N - 1' forks."
                        (Loader.Section.name s) Virtual_address.pp
                        (Virtual_address.create pos)
                        size;
                      RX
                        {
                          base = Virtual_address.create pos;
                          content = Loader.Img.content img s;
                        }) )
                else
                  ( size,
                    RX
                      {
                        base = Virtual_address.create pos;
                        content = Loader.Img.content img s;
                      } )
              in
              (sum + virt, Imap.add ~base:(Z.of_int pos) size p xcode)
            else (sum, xcode))
          (0, Imap.empty) (Loader.Img.sections img)
      in
      {
        client = None;
        xcode;
        entrypoint;
        prehook = Dhunk.goto entrypoint;
        rocfg = C.create approx_size;
        wcfg = Virtual_address.Htbl.create 0;
        worklist = W.empty;
        cut = Virtual_address.Htbl.create 7;
        choice = Virtual_address.Htbl.create 7;
        assume = Virtual_address.Htbl.create 7;
        dyn_assert = Virtual_address.Htbl.create 7;
        reach = Virtual_address.Htbl.create 7;
        reach_all = false;
        enumerate = Virtual_address.Htbl.create 7;
        enumerations = Virtual_address.Htbl.create 7;
      }

    let llookup h v = try Virtual_address.Htbl.find h v with Not_found -> []

    let qlookup h v =
      try Virtual_address.Htbl.find h v
      with Not_found ->
        let q = Queue.create () in
        Virtual_address.Htbl.add h v q;
        q

    let add_cut e addr guard =
      Virtual_address.Htbl.replace e.cut addr (guard :: llookup e.cut addr)

    let add_choice e addr choice =
      Virtual_address.Htbl.replace e.choice addr choice

    let add_assume e addr pred =
      Virtual_address.Htbl.replace e.assume addr (pred :: llookup e.assume addr)

    let add_assert e addr pred =
      Virtual_address.Htbl.replace e.dyn_assert addr
        (pred :: llookup e.dyn_assert addr)

    let add_reach e addr count guard actions =
      Queue.add (count, guard, actions) (qlookup e.reach addr)

    let add_enumerate e addr count expr =
      Queue.add (count, expr) (qlookup e.enumerate addr)

    (* Initialize goal table from cli specification *)
    let add_directive e a =
      Logger.debug ~level:2 "Add action %a" Directive.pp a;
      let v = Directive.addr a in
      match Directive.directive a with
      | Cut g -> add_cut e v g
      | Choice c -> add_choice e v c
      | Assume p -> add_assume e v p
      | Assert p -> add_assert e v p
      | Reach (n, g, a) -> add_reach e v n g a
      | Enumerate (n, x) -> add_enumerate e v n x

    let update_from_cli e =
      List.iter (add_directive e) (Directives.get ());
      Virtual_address.Set.iter
        (fun v ->
          add_reach e v Directive.Count.once Dba.Expr.one
            [ Directive.Action.Print_model ])
        (Sse_utils.get_goal_addresses ());
      Virtual_address.Set.iter
        (fun v -> add_cut e v Dba.Expr.one)
        (Sse_utils.get_avoid_addresses ())

    let choose e =
      let rec pick_one w =
        match W.pop w with
        | path_state, worklist when Path_state.is_depth_ok path_state ->
            Logger.debug "Selecting path #%d (among %d)"
              (Path_state.id path_state) (W.length w);
            e.worklist <- worklist;
            path_state
        | path_state, worklist ->
            Logger.debug "Discarding path #%d (among %d)"
              (Path_state.id path_state) (W.length w);
            pick_one worklist
        | exception Not_found ->
            Logger.info "Empty path worklist: halting ...";
            raise Halt
      in
      pick_one e.worklist

    let add_path e path_state = e.worklist <- W.push path_state e.worklist

    let pick_path e = choose e

    let pick_alternative at ?consequent ?alternative e =
      let do_pick ~first ~second =
        add_path e first;
        add_path e second;
        pick_path e
      in
      match (consequent, alternative) with
      | None, None -> pick_path e
      | Some consequent, None -> consequent
      | None, Some alternative -> alternative
      | Some consequent, Some alternative -> (
          match Virtual_address.Htbl.find e.choice at with
          | exception Not_found ->
              let first, second =
                if Randomize.get () && Random.bool () then
                  (alternative, consequent)
                else (consequent, alternative)
              in
              do_pick ~first ~second
          | c ->
              let first, second =
                if Directive.Choice.is_alternative c then
                  (alternative, consequent)
                else (consequent, alternative)
              in
              Directive.Choice.do_alternate c;
              do_pick ~first ~second)

    let add_if_good e path_state =
      if Path_state.is_depth_ok path_state then add_path e path_state
      else
        Logger.warning "@[<hov>Cut (max depth) @@ %a@]" Path_state.pp_loc
          path_state
  end

  let halt (e : Env.t) =
    Screen.release ();
    Logger.info
      "@[<v 0>@[<v 2>SMT queries@,\
       %a@]@,\
       @[<v 2>Exploration@,\
       %a@]@[<h>visited instructions (static)    %d@]@,\
       @]"
      Query_stats.pp () Exploration_stats.pp ()
      (C.nb_vertex e.rocfg + Virtual_address.Htbl.length e.wcfg);
    if Dot_filename_out.is_set () then (
      let filename = Dot_filename_out.get () in
      Logger.info "Outputting CFG in %s" filename;
      let oc = open_out_bin filename in
      let cfg = e.Env.rocfg in
      (match C.mem_vertex_a cfg e.Env.entrypoint with
      | None -> ()
      | Some entry -> C.output_graph oc e.Env.rocfg [] ~entry);
      close_out oc)

  module Eval = struct
    let goto ps addr = Path_state.set_next_address addr ps

    let static_jump ~jump_target le =
      match jump_target with
      | Dba.JInner idx -> Some (Path_state.set_block_index idx le)
      | Dba.JOuter addr -> (
          let vaddr = Dba_types.Caddress.to_virtual_address addr in
          Logger.debug ~level:5 "Jumping to new address %a" Virtual_address.pp
            vaddr;
          match Path_state.counter vaddr le with
          | Some c -> (
              match Address_counter.check_and_decr c with
              | Some c -> Some (goto (Path_state.set_counter vaddr c le) vaddr)
              | None ->
                  Logger.debug
                    "Cutting path at address %a : we reached the limit ..."
                    Virtual_address.pp vaddr;
                  None (* Could not decrement counter: should stop *))
          | None -> Some (goto le vaddr))

    let assign ~lvalue ~rvalue ss =
      match lvalue with
      | Dba.LValue.Var { name; _ } -> State.assign name rvalue ss
      | Dba.LValue.Restrict (var, { lo; hi }) ->
          State.assign var.name
            (Dba_utils.Expr.complement rvalue ~hi ~lo var)
            ss
      | Dba.LValue.Store (_, dir, addr) -> State.write ~addr rvalue dir ss

    (* lvalue <- e *)
    let assignment ~lvalue ~rvalue ps =
      (* generate the logical constraint, add it to the path predicate,
       * update symbolic_state.
       *)
      ps
      |> Path_state.set_symbolic_state @@ assign ~lvalue ~rvalue
         @@ Path_state.symbolic_state ps

    let havoc ~lvalue ss =
      match lvalue with
      | Dba.LValue.Var { name; size; _ } -> State.fresh name size ss
      | Dba.LValue.Restrict (var, { lo; hi }) ->
          let size = hi - lo + 1 in
          let nondet = Dba.Expr.var "bs_unknown" size in
          State.assign var.name
            (Dba_utils.Expr.complement nondet ~hi ~lo var)
            (State.fresh "bs_unknown" size ss)
      | Dba.LValue.Store (bytesize, dir, addr) ->
          let bitsize = 8 * bytesize in
          let nondet = Dba.Expr.var "bs_unknown" bitsize in
          State.write ~addr nondet dir (State.fresh "bs_unknown" bitsize ss)

    let nondet ~lvalue ps =
      (* generate the logical constraint, add it to the path predicate,
       * update symbolic_state.
       *)
      ps
      |> Path_state.set_symbolic_state
         @@ havoc ~lvalue (Path_state.symbolic_state ps)

    let ite ~condition ~jump_target ~local_target e ps =
      Exploration_stats.add_branch ();
      let addr = Path_state.virtual_address ps in
      match State.test condition (Path_state.symbolic_state ps) with
      | exception Unknown ->
          Exploration_stats.interrupt_path ();
          Env.pick_path e
      | True symbolic_state ->
          let consequent =
            static_jump ~jump_target
              (Path_state.set_symbolic_state symbolic_state ps)
          in
          Env.pick_alternative addr ?consequent e
      | False symbolic_state ->
          let alternative =
            Path_state.set_block_index local_target
              (Path_state.set_symbolic_state symbolic_state ps)
          in
          Env.pick_alternative addr ~alternative e
      | Both { t = symbolic_state; f = symbolic_state' } ->
          let consequent =
            static_jump ~jump_target
              (Path_state.branch
                 (Path_state.set_symbolic_state symbolic_state ps))
          in
          let alternative =
            Path_state.set_block_index local_target
              (Path_state.set_symbolic_state symbolic_state' ps)
          in
          Exploration_stats.add_path ();
          Env.pick_alternative addr ?consequent ~alternative e

    let dynamic_jump ~jump_expr e ps =
      Exploration_stats.add_branch ();
      let n = JumpEnumDepth.get () in
      match State.split_on jump_expr ~n (Path_state.symbolic_state ps) with
      | [] | (exception Unknown) ->
          Exploration_stats.interrupt_path ();
          Env.pick_path e
      | x :: bx ->
          let handle f (bv, symbolic_state) =
            Logger.debug ~level:4 "@[<hov>Dynamic jump@ %a@ could lead to@ %a@]"
              Path_state.pp_loc ps Bitvector.pp_hex bv;
            try
              let addr = Virtual_address.of_bitvector bv in
              let ps = f ps in
              Env.add_if_good e
                (goto (Path_state.set_symbolic_state symbolic_state ps) addr)
            with Virtual_address.Non_canonical_form ->
              Logger.warning
                "@[<hov>Jump@ %a@ could have led to invalid address %a;@ \
                 skipping@]"
                Path_state.pp_loc ps Bitvector.pp_hex bv
          in

          handle Fun.id x;
          List.iter
            (handle (fun ps ->
                 Exploration_stats.add_path ();
                 Path_state.branch ps))
            bx;
          Env.pick_path e

    let skip instruction idx ps =
      Logger.info ~level:3 "Skipping %a" Dba_printer.Ascii.pp_instruction
        instruction;
      Path_state.set_block_index idx ps

    let assertion cond idx e ps =
      Exploration_stats.add_assert ();
      match State.test cond (Path_state.symbolic_state ps) with
      | exception Unknown ->
          Exploration_stats.interrupt_path ();
          Env.pick_path e
      | True symbolic_state ->
          Path_state.set_block_index idx
            (Path_state.set_symbolic_state symbolic_state ps)
      | False symbolic_state ->
          assertion_failure symbolic_state ps;
          Exploration_stats.terminate_path ();
          Env.pick_path e
      | Both { t = symbolic_state; f = symbolic_state' } ->
          assertion_failure symbolic_state' ps;
          Path_state.set_block_index idx
            (Path_state.set_symbolic_state symbolic_state ps)

    let assumption cond idx e ps =
      match State.assume cond (Path_state.symbolic_state ps) with
      | exception Unknown ->
          Exploration_stats.interrupt_path ();
          Env.pick_path e
      | None ->
          Logger.info "@[<h>Unsatifiable assumption %@ %a@]" Path_state.pp_loc
            ps;
          Exploration_stats.interrupt_path ();
          Env.pick_path e
      | Some symbolic_state ->
          Path_state.set_block_index idx
            (Path_state.set_symbolic_state symbolic_state ps)

    let go e ps =
      Logger.debug ~level:5 "@[Evaluating@ %a@]" Path_state.pp_loc ps;
      match Path_state.dba_instruction ps with
      | Dba.Instr.Assign (lvalue, rvalue, idx) ->
          let ps = assignment ~lvalue ~rvalue ps in
          Path_state.set_block_index idx ps
      | Dba.Instr.Nondet (lvalue, _, idx) ->
          let ps = nondet ~lvalue ps in
          Path_state.set_block_index idx ps
      | Dba.Instr.SJump (jump_target, _) -> (
          match static_jump ~jump_target ps with
          | None ->
              (* This jump has been forbidden *)
              Exploration_stats.interrupt_path ();
              Env.pick_path e
          | Some local -> local)
      | Dba.Instr.If (condition, jump_target, local_target) ->
          ite ~condition ~jump_target ~local_target e ps
      | Dba.Instr.DJump (je, _) -> dynamic_jump ~jump_expr:je e ps
      | Dba.Instr.Undef (_, idx) as instruction -> skip instruction idx ps
      | Dba.Instr.Stop (Some (Dba.Unsupported _ | Dba.KO)) ->
          (* Discard current path, choose a new one *)
          Logger.warning "@[<hov>Cut (error) @@ %a@]" Path_state.pp_loc ps;
          Exploration_stats.interrupt_path ();
          Env.pick_path e
      | Dba.Instr.Stop _ ->
          (* Discard current path, choose a new one *)
          Exploration_stats.terminate_path ();
          Env.pick_path e
      | Dba.Instr.Assert (condition, idx) -> assertion condition idx e ps
      | Dba.Instr.Assume (condition, idx) -> assumption condition idx e ps
      | ( Dba.Instr.NondetAssume _ | Dba.Instr.Malloc _ | Dba.Instr.Free _
        | Dba.Instr.Print _ ) as dba_instruction ->
          let msg =
            Format.asprintf "%a" Dba_printer.Ascii.pp_instruction
              dba_instruction
          in
          Errors.not_yet_implemented msg
  end

  let loop_until ~halt e ps =
    let rec loop_aux ps =
      match Path_state.next_address ps with
      | Some vaddr -> do_directives vaddr e ps
      (* When the last virtual addresse has not changed, when are still in the
         same DBA block, hence no user action can have been performed.
         So, we just continue.
      *)
      | None -> loop_aux (Eval.go e ps)
    and handle_assumptions vaddr e ps =
      match Virtual_address.Htbl.find e.Env.assume vaddr with
      | exception Not_found -> Some ps
      | assumptions -> (
          let assumption =
            List.fold_left
              (fun e a -> Dba.Expr.logand e a)
              (List.hd assumptions) (List.tl assumptions)
          in
          Logger.debug "Assume %a %@ %a" Dba_printer.Ascii.pp_bl_term assumption
            Virtual_address.pp vaddr;
          match State.assume assumption (Path_state.symbolic_state ps) with
          | exception Unknown ->
              Exploration_stats.interrupt_path ();
              None
          | None ->
              Logger.result
                "@[<h>Directive :: unsatifiable assumption for path %d %@ %a@]"
                (Path_state.id ps) Virtual_address.pp vaddr;
              Exploration_stats.interrupt_path ();
              None
          | Some symbolic_state ->
              Some (Path_state.set_symbolic_state symbolic_state ps))
    and handle_assertion vaddr e ps =
      match Virtual_address.Htbl.find e.Env.dyn_assert vaddr with
      | exception Not_found -> Some ps
      | assertions -> (
          Exploration_stats.add_assert ();
          let assertions =
            List.fold_left
              (fun e a -> Dba.Expr.logand e a)
              (List.hd assertions) (List.tl assertions)
          in
          Logger.debug "Assert %a %@ %a" Dba_printer.Ascii.pp_bl_term assertions
            Virtual_address.pp vaddr;
          match State.test assertions (Path_state.symbolic_state ps) with
          | exception Unknown ->
              Exploration_stats.interrupt_path ();
              None
          | True symbolic_state ->
              Some (Path_state.set_symbolic_state symbolic_state ps)
          | False symbolic_state ->
              assertion_failure symbolic_state ps;
              Exploration_stats.terminate_path ();
              None
          | Both { t = symbolic_state; f = symbolic_state' } ->
              assertion_failure symbolic_state' ps;
              Some (Path_state.set_symbolic_state symbolic_state ps))
    and handle_reach vaddr e ps =
      match Virtual_address.Htbl.find e.Env.reach vaddr with
      | exception Not_found -> ()
      | reachs ->
          let reachs' = Queue.create () in
          Queue.iter
            (fun ((k, guard, actions) as r) ->
              Logger.debug "Reach";
              match State.assume guard (Path_state.symbolic_state ps) with
              | exception Unknown ->
                  Logger.warning
                    "@[<h>Directive :: path %d reached address %a with unknown \
                     condition (%a to go)@]"
                    (Path_state.id ps) Virtual_address.pp vaddr
                    Directive.Count.pp k;
                  Queue.add r reachs'
              | None ->
                  Logger.debug
                    "@[<h>Directive :: path %d reached address %a with unsat \
                     condition (%a to go)@]"
                    (Path_state.id ps) Virtual_address.pp vaddr
                    Directive.Count.pp k;
                  Queue.add r reachs'
              | Some symbolic_state -> (
                  let k' = Directive.Count.decr k in
                  Logger.result
                    "@[<h>Directive :: path %d reached address %a (%a to go)@]"
                    (Path_state.id ps) Virtual_address.pp vaddr
                    Directive.Count.pp k';
                  List.iter
                    (function
                      | Directive.Action.Print_formula slice ->
                          Logger.result "Formula@[<hov>%a@] %@ %a@\n%a"
                            (fun ppf -> function
                              | None -> ()
                              | Some l ->
                                  Format.pp_print_space ppf ();
                                  Format.pp_print_string ppf "for";
                                  List.iter
                                    (fun (_, n) ->
                                      Format.pp_print_space ppf ();
                                      Format.pp_print_string ppf n)
                                    l)
                            slice Virtual_address.pp vaddr (State.pp_smt ?slice)
                            symbolic_state
                      | Directive.Action.Print_model ->
                          Logger.result "@[<v 0>Model %@ %a@ %a@]"
                            Virtual_address.pp vaddr State.pp symbolic_state
                      | Directive.Action.Print_value (format, expr) ->
                          let bv =
                            fst
                              (List.hd
                                 (State.split_on ~n:1 expr symbolic_state))
                          in
                          Logger.result "@[<v 0>Value %a : %a@]"
                            Dba_printer.Ascii.pp_bl_term expr
                            (pp_value_as format) bv
                      | Directive.Action.Print_stream name ->
                          Logger.result "@[<v 0>Ascii stream %s : %S@]" name
                            (try State.as_ascii name symbolic_state
                             with Not_found -> ""))
                    actions;
                  match k' with
                  | Directive.Count.Count 0 -> ()
                  | _ -> Queue.add (k', guard, actions) reachs'))
            reachs;
          if Queue.length reachs' = 0 then
            Virtual_address.Htbl.remove e.Env.reach vaddr
          else Virtual_address.Htbl.replace e.Env.reach vaddr reachs'
    and handle_enumerate vaddr e ps =
      match Virtual_address.Htbl.find e.Env.enumerate vaddr with
      | exception Not_found -> ()
      | enumerations ->
          let enumerations' = Queue.create () in
          Queue.iter
            (fun (k, x) ->
              let bvs =
                try
                  Etbl.find
                    (Virtual_address.Htbl.find e.Env.enumerations vaddr)
                    x
                with Not_found -> []
              in
              let n = List.length bvs in
              let bvs' =
                State.split_on x ~n:k ~except:bvs (Path_state.symbolic_state ps)
              in
              let n' = List.length bvs' in
              let bvs =
                List.sort_uniq Bitvector.compare
                  (List.fold_left (fun bvs (bv, _) -> bv :: bvs) bvs bvs')
              in
              Logger.result
                "@[<hov 0>Directive :: enumerate@ possible values (%d) for %a \
                 %@ %a:@ @[<hov 0>%a@]@]"
                (n + n') Dba_printer.EICAscii.pp_bl_term x Virtual_address.pp
                vaddr
                (Print_utils.pp_list ~sep:",@ " Bitvector.pp_hex_or_bin)
                bvs;
              if n' < k then Queue.add (k - n', x) enumerations';
              try
                Etbl.replace
                  (Virtual_address.Htbl.find e.Env.enumerations vaddr)
                  x bvs
              with Not_found ->
                let tbl = Etbl.create 7 in
                Etbl.add tbl x bvs;
                Virtual_address.Htbl.add e.Env.enumerations vaddr tbl)
            enumerations;
          if Queue.length enumerations' = 0 then
            Virtual_address.Htbl.remove e.Env.enumerate vaddr
          else Virtual_address.Htbl.replace e.Env.enumerate vaddr enumerations'
    and handle_cut vaddr e ps =
      match Virtual_address.Htbl.find e.Env.cut vaddr with
      | exception Not_found -> Some ps
      | cuts -> (
          let guard =
            Dba.Expr.lognot
              (List.fold_left
                 (fun e g -> Dba.Expr.logor e g)
                 (List.hd cuts) (List.tl cuts))
          in
          match State.assume guard (Path_state.symbolic_state ps) with
          | exception Unknown ->
              Logger.warning
                "@[<h>Directive :: cut %@ %a with unknown condition@]"
                Virtual_address.pp vaddr;
              Exploration_stats.interrupt_path ();
              None
          | None ->
              Logger.result "@[<h>Directive :: cut path %d %@ %a@]"
                (Path_state.id ps) Virtual_address.pp vaddr;
              Exploration_stats.terminate_path ();
              None
          | Some symbolic_state ->
              Logger.debug "@[<h>Directive :: cut %@ %a with unsat condition@]"
                Virtual_address.pp vaddr;
              Some (Path_state.set_symbolic_state symbolic_state ps))
    and do_directives vaddr e ps =
      match handle_assumptions vaddr e ps with
      | None -> loop_aux (Env.pick_path e)
      | Some ps -> (
          match handle_assertion vaddr e ps with
          | None -> loop_aux (Env.pick_path e)
          | Some ps -> (
              handle_reach vaddr e ps;
              handle_enumerate vaddr e ps;
              if
                (not e.Env.reach_all)
                && Virtual_address.Htbl.length e.Env.reach = 0
                && Virtual_address.Htbl.length e.Env.enumerate = 0
              then halt e
              else
                match handle_cut vaddr e ps with
                | None -> loop_aux (Env.pick_path e)
                | Some ps -> check_decode_and_eval vaddr e ps))
    and check_decode_and_eval vaddr (e : Env.t) ps =
      match C.mem_vertex_a e.rocfg vaddr with
      | Some v ->
          (* C.add_edge_a e.Env.rocfg (Path_state.virtual_address ps) vaddr; *)
          let ps = Path_state.set_instruction (Option.get (C.V.inst v)) ps in
          eval vaddr e ps
      | None -> (
          match Imap.find (Virtual_address.to_bigint vaddr) e.xcode with
          | exception Not_found ->
              Logger.warning
                "@[<hov>Jump@ %a@ could have led to invalid address %a;@ \
                 skipping@]"
                Path_state.pp_loc ps Virtual_address.pp vaddr;
              Exploration_stats.interrupt_path ();
              loop_aux (Env.pick_path e)
          | RX { base; content } ->
              let reader =
                Lreader.of_zero_extend_buffer
                  ~at:(Virtual_address.diff vaddr base)
                  content
              in
              let i = fst (Disasm_core.decode_from reader vaddr) in
              Exploration_stats.add_unique_inst ();
              C.add_inst e.rocfg vaddr i;
              (* C.add_edge_a e.Env.rocfg (Path_state.virtual_address ps) vaddr; *)
              let ps = Path_state.set_instruction i ps in
              eval vaddr e ps
          | RWX -> transient_instruction vaddr e ps)
    and transient_instruction vaddr e ps =
      let n = TransientEnum.get () in
      let omap =
        try Virtual_address.Htbl.find e.wcfg vaddr
        with Not_found -> OMap.empty
      in
      let opcode =
        Dba.Expr.load
          (Isa_helper.max_instruction_len ())
          Machine.LittleEndian
          (Dba_utils.Expr.of_vaddr vaddr)
      in
      match State.split_on opcode ~n (Path_state.symbolic_state ps) with
      | [] | (exception Unknown) ->
          Exploration_stats.interrupt_path ();
          loop_aux (Env.pick_path e)
      | x :: bx ->
          let fork ps =
            Exploration_stats.add_path ();
            Path_state.branch ps
          in
          let handle f vaddr omap (bv, symbolic_state) =
            let opcode = Bitvector.to_asciistring bv in
            let omap, i =
              try (omap, OMap.find opcode omap)
              with Not_found ->
                let reader = Lreader.of_bytes opcode in
                let i = fst (Disasm_core.decode_from reader vaddr) in
                Exploration_stats.add_unique_inst ();
                let s = (Instruction.size i :> int) in
                let omap =
                  if s = 0 then omap
                  else
                    OMap.add
                      (String.sub opcode 0 (Instruction.size i :> int))
                      i omap
                in
                (omap, i)
            in
            Logger.debug ~level:4
              "@[<hov>Self-written instruction @@ %a could be %a [ %a ]@]"
              Virtual_address.pp vaddr Mnemonic.pp (Instruction.mnemonic i)
              Binstream.pp (Instruction.opcode i);
            ( omap,
              Path_state.set_instruction i
                (Path_state.set_symbolic_state symbolic_state (f ps)) )
          in
          let omap =
            List.fold_left
              (fun omap x ->
                let omap, ps = handle fork vaddr omap x in
                Env.add_path e ps;
                omap)
              omap bx
          in
          let omap, ps = handle Fun.id vaddr omap x in
          Virtual_address.Htbl.replace e.wcfg vaddr omap;
          eval vaddr e ps
    and eval vaddr e ps =
      Logger.debug ~level:2 "%@%a %a" Virtual_address.pp vaddr Mnemonic.pp
        (Instruction.mnemonic (Path_state.inst ps));
      Exploration_stats.add_instruction ();
      Exploration_stats.update_depth (Path_state.depth ps);
      loop_aux (Eval.go e ps)
    in
    try
      Sys.catch_break true;
      Screen.init ();
      Option.iter
        (fun timeout ->
          Sys.set_signal Sys.sigalrm
            (Sys.Signal_handle
               (fun s ->
                 assert (s = Sys.sigalrm);
                 raise_notrace Halt));
          ignore (Unix.alarm timeout))
        (Timeout.get_opt ());
      loop_aux ps
    with
    | Halt | Sys.Break -> halt e
    | err ->
        Screen.release ();
        raise err

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

  let initialize_state (e : Env.t) state =
    let to_load = LoadSections.get () and ro_load = LoadROSections.get () in
    let addr_size = Kernel_options.Machine.word_size ()
    and img = Kernel_functions.get_img () in
    let symbols : (Dba.VarTag.attribute * Bitvector.t) String.Htbl.t =
      String.Htbl.create 100
    in
    (match img with
    | ELF img ->
        let open Loader_elf in
        Array.iter
          (fun sym ->
            match Symbol.header sym with
            | { kind = SECTION; sh = SEC { name; addr; size; _ }; _ }
            | { sh = SEC _; name; value = addr; size; _ } ->
                String.Htbl.add symbols name
                  (Value, Bitvector.of_int ~size:addr_size addr);
                String.Htbl.add symbols name
                  (Size, Bitvector.of_int ~size:addr_size size);
                String.Htbl.add symbols name
                  (Last, Bitvector.of_int ~size:addr_size (addr + size - 1))
            | _ -> ())
          (Img.symbols img)
    | _ ->
        let open Loader in
        Array.iter
          (fun sym ->
            String.Htbl.add symbols (Symbol.name sym)
              (Value, Bitvector.of_int ~size:addr_size (Symbol.value sym)))
          (Img.symbols img);
        Array.iter
          (fun sec ->
            let name = Section.name sec in
            let { Loader_types.virt = addr; _ } = Section.pos sec in
            String.Htbl.add symbols name
              (Value, Bitvector.of_int ~size:addr_size addr);
            let { Loader_types.virt = size; _ } = Section.size sec in
            String.Htbl.add symbols name
              (Size, Bitvector.of_int ~size:addr_size size);
            String.Htbl.add symbols name
              (Last, Bitvector.of_int ~size:addr_size (addr + size - 1)))
          (Img.sections img));
    let state =
      if (not ro_load) && String.Set.is_empty to_load then state
      else
        Array.fold_left
          (fun state section ->
            let name = Loader.Section.name section in
            if
              (ro_load
              && Loader.Section.(
                   has_flag Loader_types.Read section
                   && not (has_flag Loader_types.Write section)))
              || String.Set.mem name to_load
            then (
              let addr =
                Bitvector.of_int ~size:addr_size
                  (Loader.Section.pos section).virt
              and size = (Loader.Section.size section).virt in
              Logger.info "Load section %s" name;
              State.memcpy ~addr size (Loader.Img.content img section) state)
            else state)
          state
        @@ Loader.Img.sections img
    in
    let rec copy_from addr size state =
      let section =
        match
          Loader_utils.find_section_by_address ~address:(Bitvector.to_int addr)
            img
        with
        | None ->
            Logger.fatal "address %a does not belong to file" Virtual_address.pp
              (Virtual_address.of_bitvector addr)
        | Some section -> section
      in
      let { Loader_types.virt; _ } = Loader.Section.size section in
      if virt >= size then
        State.memcpy ~addr size (Loader.Img.content img section) state
      else
        copy_from
          (Bitvector.add_int addr virt)
          (size - virt)
          (State.memcpy ~addr virt (Loader.Img.content img section) state)
    in
    let set state init =
      let open Parse_helpers.Initialization in
      match init.operation with
      | Mem_load (addr, size) -> (
          match State.split_on addr ~n:2 state with
          | [ (bv, _) ] ->
              Logger.debug ~level:40
                "the memory initializer address %a resolves to %a"
                Dba_printer.Ascii.pp_bl_term addr Bitvector.pp bv;
              copy_from bv size state
          | _ | (exception Unknown) ->
              Logger.fatal
                "the memory initializer address %a does not resolve to a \
                 unique value"
                Dba_printer.Ascii.pp_bl_term addr)
      | Universal lval -> (
          match Dba_types.LValue.name_of lval with
          | Some name ->
              let size = Dba.LValue.size_of lval in
              State.fresh name size state
          | None -> state)
      | Assumption cond -> (
          match State.assume cond state with
          | exception Unknown ->
              Logger.fatal "the initial assumption %a is unknown"
                Dba_printer.Ascii.pp_bl_term cond
          | None ->
              Logger.fatal "the initial assumption %a is false"
                Dba_printer.Ascii.pp_bl_term cond
          | Some state -> state)
      | Assignment (lval, rval, name_opt) -> (
          match (rval, name_opt) with
          | Nondet, Some name ->
              let size = Dba.LValue.size_of lval in
              let evar = Dba.Expr.var name size in
              let state = State.fresh name size state in
              Eval.assign ~lvalue:lval ~rvalue:evar state
          | Nondet, None -> Eval.havoc ~lvalue:lval state
          | Singleton rv, Some name ->
              let bitsize = Size.Bit.create (Dba.LValue.size_of lval) in
              let var = Dba.LValue.var ~bitsize name in
              let evar = Dba.LValue.to_expr var in
              Eval.assign ~lvalue:var ~rvalue:rv state
              |> Eval.assign ~lvalue:lval ~rvalue:evar
          | Singleton rv, None -> Eval.assign ~lvalue:lval ~rvalue:rv state
          | x, Some name ->
              let size = Dba.LValue.size_of lval in
              let evar = Dba.Expr.var name size in
              let state = State.fresh name size state in
              let state = Eval.assign ~lvalue:lval ~rvalue:evar state in
              let cond = interval_or_set_to_cond evar x in
              Option.get (State.assume cond state)
          | x, None ->
              let state = Eval.havoc ~lvalue:lval state in
              let e = Dba.LValue.to_expr lval in
              let cond = interval_or_set_to_cond e x in
              Option.get (State.assume cond state))
    in
    let state =
      match MemoryFile.get_opt () with
      | None -> state
      | Some filename ->
          if not (Sys.file_exists filename) then
            Logger.fatal "Cannot find sse configuration file %s" filename;
          let initials =
            Logger.debug "Reading initialization from %s" filename;
            let parser = Parser.initialization and lexer = Lexer.token in
            Parse_utils.read_file ~parser ~lexer ~filename
          in
          List.fold_left set state initials
    in
    let from_core prehook state =
      match Kernel_functions.get_img () with
      | Loader.Raw _ | Loader.PE _ -> Logger.fatal "Binary is not an ELF file."
      | Loader.ELF img' ->
          let hdr = Loader_elf.Img.header img' in
          if hdr.Loader_elf.Ehdr.kind <> Loader_elf.Ehdr.ET.CORE then
            Logger.fatal "Binary is not a core file";
          let vmap, state =
            Array.fold_left
              (fun (vmap, state) section ->
                let open Loader_elf in
                let hdr = Section.header section in
                if Section.has_flag Loader_types.Read section then
                  let addr =
                    Bitvector.of_int
                      ~size:(Kernel_options.Machine.word_size ())
                      hdr.Shdr.addr
                  in
                  ( Imap.add ~base:(Bitvector.value_of addr) hdr.Shdr.size true
                      vmap,
                    State.memcpy ~addr hdr.Shdr.size (Img.content img' section)
                      state )
                else (vmap, state))
              (Imap.empty, state)
              (Loader_elf.Img.sections img')
          in
          let transient = TransientEnum.get () > 0 in
          let _, _, xcode, state =
            Array.fold_left
              (fun (vmap, fmap, xcode, state)
                   { Loader_elf.addresses = { lo; hi }; offset; name } ->
                if String.Set.mem name fmap then (vmap, fmap, xcode, state)
                else
                  let img = Loader_elf.load_file name in
                  let size = Virtual_address.diff hi lo + 1 in
                  let section =
                    Option.get
                      (Loader_utils.find_section
                         ~p:(fun s ->
                           let { Loader_types.raw; _ } = Loader.Section.pos s in
                           Loader.Section.has_flag Loader_types.Read s
                           && offset <= raw
                           && raw < offset + size)
                         (Loader.ELF img))
                  in
                  let { Loader_types.raw; virt } = Loader.Section.pos section in
                  let base =
                    Virtual_address.diff
                      (Virtual_address.add_int (raw - offset) lo)
                      (Virtual_address.create virt)
                  in
                  Logger.debug "%08x :: %a-%a %08x %s" base Virtual_address.pp
                    lo Virtual_address.pp hi offset name;
                  let sections = Loader_elf.Img.sections img in
                  let vmap, xcode, state =
                    Array.fold_left
                      (fun (vmap, xcode, state) s ->
                        let open Loader_elf in
                        let hdr = Section.header s in
                        let pos = hdr.Shdr.addr and size = hdr.Shdr.size in
                        let addr =
                          Bitvector.of_int ~size:addr_size (pos + base)
                        in
                        let base = Bitvector.value_of addr in
                        if
                          (hdr.kind = RELA || hdr.kind = REL)
                          && String_utils.start_with ~prefix:".got"
                               (Section.name (Array.get sections hdr.Shdr.info))
                        then (
                          Array.iter
                            (fun Rel.
                                   {
                                     offset;
                                     kind = _;
                                     symbol = { name; _ };
                                     addend;
                                   } ->
                              let addend = Option.value ~default:0 addend in
                              let reader =
                                Lreader.create
                                  ~at:(Z.to_int base + offset - pos)
                                  Loader_elf.read_address img'
                              in
                              let value =
                                Bitvector.add_int
                                  (Lreader.Read.read reader (addr_size lsr 3))
                                  (-addend)
                              in
                              Logger.debug ~level:4 "symbol %S resolved at %a"
                                name Virtual_address.pp
                                (Virtual_address.of_bitvector value);
                              String.Htbl.add symbols name (Value, value))
                            (Rel.read img hdr);
                          (vmap, xcode, state))
                        else if
                          (not (Section.has_flag Loader_types.Read s))
                          || Imap.mem base vmap
                        then (vmap, xcode, state)
                        else
                          let vmap = Imap.add ~base size true vmap in
                          let xcode =
                            if Section.has_flag Loader_types.Exec s then
                              let p =
                                if Section.has_flag Loader_types.Write s then
                                  if transient then Env.RWX
                                  else (
                                    Logger.warning
                                      "Section %S [%a, 0x%x] has both Write \
                                       and Execute flags.@ Self-modifying code \
                                       is disabled and writes will be \
                                       ignored.@ Use '-sse-self-written-enum \
                                       N' to enable symbolic reasoning up to \
                                       'N - 1' forks."
                                      (Section.name s) Virtual_address.pp
                                      (Virtual_address.create pos)
                                      size;
                                    Env.RX
                                      {
                                        base = Virtual_address.create pos;
                                        content = Img.content img s;
                                      })
                                else
                                  Env.RX
                                    {
                                      base = Virtual_address.of_bitvector addr;
                                      content = Img.content img s;
                                    }
                              in
                              Imap.add ~base size p xcode
                            else xcode
                          in
                          let state =
                            State.memcpy ~addr size (Img.content img s) state
                          in
                          (vmap, xcode, state))
                      (vmap, xcode, state) sections
                  in
                  (vmap, String.Set.add name fmap, xcode, state))
              (vmap, String.Set.empty, e.xcode, state)
              (Loader_elf.files img')
          in
          let entrypoint, initializations = Isa_helper.core img' in
          let state =
            State.assign "$ENTRYPOINT"
              (Dba_utils.Expr.of_vaddr entrypoint)
              state
          in
          e.entrypoint <- entrypoint;
          e.prehook <- prehook;
          e.xcode <- xcode;
          let state =
            List.fold_left
              (fun state ({ Dba.name; _ }, value) ->
                State.assign name value state)
              state initializations
          in
          state
    in
    let state =
      match ScriptFiles.get () with
      | [] -> state
      | files ->
          let module M : Astbuilder.Env = struct
            let wordsize = Kernel_options.Machine.word_size ()

            let endianness = Kernel_options.Machine.endianness ()

            let tbl = String.Htbl.create 128;;

            List.iter
              (fun (name, var) ->
                String.Htbl.add tbl (String.lowercase_ascii name) var)
              (Isa_helper.get_defs ())

            let lookup name size =
              let ci_name = String.lowercase_ascii name in
              try Basic_types.String.Htbl.find tbl ci_name
              with Not_found ->
                if size = -1 then
                  Logger.fatal "size is missing for variable %s" name;
                let bitsize = Size.Bit.create size in
                let var = Dba.LValue.var ~bitsize name in
                String.Htbl.add tbl ci_name var;
                var

            let tbl = String.Htbl.create 128

            let lookup_symbol name (attr : Dba.VarTag.attribute) =
              try List.assoc attr (String.Htbl.find_all tbl name)
              with Not_found ->
                let value =
                  lazy
                    (try List.assoc attr (String.Htbl.find_all symbols name)
                     with Not_found ->
                       Logger.fatal "Can not resolve symbol <%s%a>" name
                         Dba.VarTag.pp_attribute attr)
                in
                let tag = Dba.VarTag.Symbol (attr, value) in
                let sym = Dba.Expr.var ~tag name wordsize in
                String.Htbl.add tbl name (attr, sym);
                sym
          end in
          let module P = Sse_parser.Make (M) in
          List.fold_left
            (fun state filename ->
              if not (Sys.file_exists filename) then
                Logger.fatal "Cannot find sse configuration file %s" filename;
              let script =
                Logger.debug "Reading script from %s" filename;
                let parser = P.script and lexer = Sse_lexer.token in
                Parse_utils.read_file ~parser ~lexer ~filename
              in
              List.fold_left
                (fun state -> function
                  | Script.Init i -> set state i
                  | Script.Goal g -> (
                      let loc = Directive.loc g in
                      match State.split_on loc ~n:2 state with
                      | [ (bv, _) ] ->
                          Logger.debug ~level:40
                            "the directive address %a resolves to %a"
                            Dba_printer.Ascii.pp_bl_term loc Bitvector.pp bv;
                          let addr = Dba.Expr.constant bv in
                          let g = Directive.reloc addr g in
                          Env.add_directive e g;
                          state
                      | _ | (exception Unknown) ->
                          Logger.fatal
                            "the directive address %a does not resolve to a \
                             unique value"
                            Dba_printer.Ascii.pp_bl_term loc)
                  | Script.Stub (a, b) ->
                      List.iter
                        (fun a ->
                          match State.split_on a ~n:2 state with
                          | [ (bv, _) ] ->
                              Logger.debug ~level:40
                                "the stub address %a resolves to %a"
                                Dba_printer.Ascii.pp_bl_term a Bitvector.pp bv;
                              Logger.debug ~level:10
                                "@[<v 2> replace address %a by@ %a@]"
                                Dba_printer.Ascii.pp_bl_term a Dhunk.pp b;
                              let addr = Virtual_address.of_bitvector bv in
                              let mnemonic =
                                Mnemonic.supported a (fun ppf e ->
                                    Format.fprintf ppf "stub for %a"
                                      Dba_printer.Ascii.pp_bl_term e)
                              in
                              C.add_inst e.rocfg addr
                                (Instruction.of_dba_block ~mnemonic addr b)
                          | _ | (exception Unknown) ->
                              Logger.fatal
                                "the stub address %a does not resolve to a \
                                 unique value"
                                Dba_printer.Ascii.pp_bl_term a)
                        a;
                      state
                  | Script.Pragma (Start_from (a, prehook)) -> (
                      match State.split_on a ~n:2 state with
                      | [ (bv, _) ] ->
                          Logger.debug ~level:40
                            "the entrypoint address %a resolves to %a"
                            Dba_printer.Ascii.pp_bl_term a Bitvector.pp bv;
                          e.entrypoint <- Virtual_address.of_bitvector bv;
                          e.prehook <- prehook;
                          state
                      | _ | (exception Unknown) ->
                          Logger.fatal
                            "the entrypoint address %a does not resolve to a \
                             unique value"
                            Dba_printer.Ascii.pp_bl_term a)
                  | Script.Pragma (Start_from_core prehook) ->
                      from_core prehook state
                  | Script.Pragma (Load_sections names) ->
                      List.fold_left
                        (fun ss name ->
                          let section =
                            Loader_utils.find_section_by_name name img
                          in
                          let addr =
                            Bitvector.of_int ~size:addr_size
                              (Loader.Section.pos section).virt
                          and size = (Loader.Section.size section).virt in
                          Logger.info "Load section %s (%a, %#x)" name
                            Bitvector.pp_hex_or_bin addr size;
                          State.memcpy ~addr size
                            (Loader.Img.content img section)
                            ss)
                        state names
                  | Script.Pragma Reach_all ->
                      e.Env.reach_all <- true;
                      state)
                state script)
            state files
    in
    state

  let do_sse ~filename =
    let level = 3 in
    Logger.debug ~level "Running SSE on %s" filename;
    let entrypoint = get_entry_point () in
    Logger.debug ~level "Starting from %a" Virtual_address.pp entrypoint;
    let e = Env.create entrypoint in
    Logger.debug ~level "@[<h>Initializing SSE goals ...@]";
    Env.update_from_cli e;
    Logger.debug ~level "Driver set ...";
    Logger.debug ~level "Creating symbolic store ...";
    let ss = initialize_state e (State.empty ()) in
    let ps =
      Path_state.create ss (Instruction.of_dba_block e.entrypoint e.prehook)
    in
    let ps =
      let cli_counters = Visit_address_counter.get () in
      match cli_counters with
      | [] -> ps
      | cs ->
          Logger.info "Found some address counters ... great";
          let m =
            let open! Virtual_address in
            List.fold_left
              (fun m c -> Map.add c.Address_counter.address c m)
              Map.empty cs
          in
          Path_state.set_address_counters m ps
    in
    Logger.debug ~level "Initialization done ...";
    loop_until ~halt e ps

  let start () =
    let filename = Kernel_options.ExecFile.get () in
    do_sse ~filename
end

let get_worklist () =
  match Search_heuristics.get () with
  | Dfs -> (module Dfs : WORKLIST_FACTORY)
  | Bfs -> (module Bfs : WORKLIST_FACTORY)
  | Nurs ->
      let seed =
        match Seed.get_opt () with
        | Some s -> s
        | None ->
            let v = Utils.random_max_int () in
            Logger.info "Random search seed is %d" v;
            Seed.set v;
            v
      in
      Random.init seed;
      (module Nurs : WORKLIST_FACTORY)

let get_state () =
  if AlternativeEngine.get () then
    (module Senv.State ((val Senv.get_solver_factory ())) : STATE_FACTORY)
  else
    (module Sse_symbolic.State ((val Smt_solver.get_solver ())) : STATE_FACTORY)

let run () =
  if is_enabled () && Kernel_options.ExecFile.is_set () then
    let module S = Env_make ((val get_state ())) ((val get_worklist ())) in
    S.start ()

let _ = Cli.Boot.enlist ~name:"SSE" ~f:run
