(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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

(* Enumerate jumps targets *)
let get_entry_point () =
  match Kernel_functions.get_ep () with
  | Some v -> v
  | None ->
     let img = Kernel_functions.get_img () in
     Loader.Img.entry img |> Virtual_address.create


module type SSE_RUNNER = sig val start: unit -> unit end
module Env_make(G: GLOBAL_ENV): SSE_RUNNER =
struct
  module Stats = struct
    type t = {
        visited : int;
        choices : int;
      }

    let empty = { visited = 0; choices = 0; }
    let add_visited s = { s with visited = s.visited + 1 }
    let add_choice  s = { s with choices = s.choices + 1 }

    let pp ppf s =
      Format.fprintf ppf
      "@[<v 0>\
       @[<h>visited paths %d@]@,\
       @[<h>choice        %d@]\
       @]"
      s.visited
      s.choices
    ;;

    module R = struct
      let value = ref empty
      let add_visited () = value := add_visited !value
      let add_choice  () = value := add_choice !value
      let pp ppf () = pp ppf !value
    end
    include R
  end

  module Env = struct
    type t = {
        global : G.t;
        local  : Path_state.t
      }

    let create ~global ~local = { global; local; }

    let local e = e.local
    let global e = e.global

    let of_global g =
      let global, local = G.Path.choose g in
      create ~global ~local

    let current_address e =
      Path_state.virtual_address @@ local e

    let goals_here e =
      G.Goals.at (current_address e) (global e)

    let check_sat e =
      let sat_status, _ = Sse_smt.Solver.check_satistifiability e.local in
      sat_status

    let rec pick_path e =
      Stats.add_visited ();
      let e = of_global e.global in
      check_sat_or_choose_another e

    and check_sat_or_choose_another e =
      let freq = Solver_call_frequency.get () in
      let n = Path_state.solver_calls e.local in
      if n = freq then
        match check_sat e with
        | Formula.SAT ->
           (* After a call, let's get back to the initial state *)
           let local = Path_state.reset_solver_calls e.local in
           { e with local }
        | _ -> pick_path e
      else
        let local = Path_state.incr_solver_calls e.local in
        { e with local }


    let is_good = function
      | None -> None
      | Some p ->
         if Path_state.leads_to_goal p then Some p
         else None

    let pick_alternative ~consequent ~alternative e =
      Stats.add_choice ();
      let do_pick ~first ~second =
        let global = G.(Path.add first e.global |> Path.add second) in
        pick_path { e with global }
      in
      match consequent, alternative with
      | None, None -> pick_path e
      | Some consequent, None -> {e with local = consequent}
      | None, Some alternative -> {e with local = alternative}
      | Some consequent, Some alternative ->
         let open Action in
         match goals_here e with
         | Some { goal = Choice Alternative; _ } ->
            do_pick ~first:alternative ~second:consequent
         | Some { goal = Choice Consequent; _} ->
            do_pick ~second:alternative ~first:consequent
         | None | Some _ ->
            let first, second =
              if Sse_options.Randomize.get () && Random.bool ()
              then alternative, consequent
              else consequent, alternative in
            do_pick ~first ~second


    let add_if_good e path_state =
      match is_good (Some path_state) with
      | None ->
         Logger.info
           "Goal is unreachable from@ %a" Path_state.pp_loc path_state;
         e
      | Some path_state ->
         {e with global = G.Path.add path_state e.global}

  end

  let halt _e =
    Logger.info
      "@[<v 0>\
       @[<v 2>SMT queries@,%a@]@,\
       @[<v 2>Exploration@,%a@]@,\
       @]"
      Sse_smt.Query_stats.pp ()
      Stats.pp ()

  module Eval = struct

    let static_jump ~jump_target le =
      match jump_target with
      | Dba.JInner idx ->
         Some (Path_state.set_block_index idx le)
      | Dba.JOuter addr ->
         let vaddr = Dba_types.Caddress.to_virtual_address addr in
         Logger.debug ~level:5 "Jumping to new address %a"
           Virtual_address.pp vaddr;
         let open Sse_types.Path_state in
         match Path_state.counter vaddr le with
         | Some c ->
            begin match Address_counter.check_and_decr c with
            | Some c ->
               let p =
                 Path_state.set_counter vaddr c le
                 |> goto_vaddr vaddr in
               Some p
            | None ->
               Logger.debug "Cutting path at address %a : we reached the limit ..."
                 Virtual_address.pp vaddr;
               None
                 (* Could not decrement counter: should stop *)
            end
         | None -> Some (goto_vaddr vaddr le)

    (* lvalue <- e *)
    let assignment ~lvalue ~rvalue e =
      (* generate the logical constraint, add it to the path predicate,
     update symbolic_state
       *)
      let open Sse_smt in
      Env.{ e with local = Translate.assignment lvalue rvalue e.local }

    let nondet ~lvalue ~region e =
      let _ = region in
      (* generate the logical constraint, add it to the path predicate,
     update symbolic_state
       *)
      let open Sse_smt in
      Env.{ e with local = Translate.nondet lvalue e.local }


    (* pick a new branch in the worklist and check its satisfiability *)

    let ite ~condition ~jump_target ~local_target e =
      (* expand path with assert condition and go to jump_target *)
      (* push path with assert not condition and go to local_target *)
      let open Sse_smt in
      let state = Env.local e in
      let condition =
        let open Formula in
        let syst = Path_state.symbolic_state state in
        mk_bv_equal (Translate.expr syst condition) mk_bv_one
      in
      let alternate_condition = Formula.mk_bl_not condition in
      let consequent =
        Path_state.add_assertion condition state |> static_jump ~jump_target
      in
      let alternate_state = Path_state.branch state in
      let alternative =
        Some (
            Path_state.add_assertion alternate_condition alternate_state
            |> Sse_types.Path_state.set_block_index local_target)
      in Env.pick_alternative ~consequent ~alternative e

    let dynamic_jump ~jump_expr e =
      let img = Kernel_functions.get_img () in
      let path_state = Env.local e in
      let target =
        let symb_st = Path_state.symbolic_state path_state in
        Sse_smt.Translate.expr symb_st jump_expr in
      let n = Sse_options.JumpEnumDepth.get () in
      let concretes, path_state =
        Sse_smt.Solver.enumerate_values n target path_state in
      let with_bv env bv =
        let condition = Formula.(mk_bv_equal (mk_bv_cst bv) target)
        and addr = Virtual_address.of_bitvector bv
        and invalid bv =
          Logger.warning
            "@[<hov>Dynamic jump@ %a@ could have led to invalid address %a;@ \
             skipping@]"
            Path_state.pp_loc path_state Bitvector.pp_hex bv;
          env
        in
        Logger.debug ~level:4 "@[<hov>Dynamic jump@ %a@ could lead to@ %a@]"
          Path_state.pp_loc path_state Bitvector.pp_hex bv;
        let address = Virtual_address.to_int64 addr |> Int64.to_int in
        let section = Loader_utils.find_section_by_address ~address img in
        match section with
        | Some s when
               Loader.Section.has_flag Loader_types.Read s &&
                 Loader.Section.has_flag Loader_types.Exec s ->
           let ps = Path_state.add_assertion condition path_state in
           let ps = Sse_types.Path_state.goto_vaddr addr ps in
           Env.add_if_good env ps
        | Some _ | None -> invalid bv
      in
      let env = List.fold_left with_bv e concretes in
      Env.pick_path env

    let skip instruction idx e =
      Logger.info ~level:3 "Skipping %a"
        Dba_printer.Ascii.pp_instruction instruction;
      Env.{ e with local = Path_state.set_block_index idx e.local}

    (* If no-comment is not activated add for every formula entry a comment
       about where it comes from. This is in particular used to debug the path
       predicate translation.
     *)
    let maybe_add_comment ps =
      if not (Sse_options.NoComment.get ()) then
        let comment =
          Print_utils.string_from_pp
            (Formula_pp.pp_as_comment Path_state.pp_loc) ps
          |> Formula.mk_comment
        and syst = Path_state.symbolic_state ps in
        Sse_symbolic.Store.add_entry syst.Sse_symbolic.State.store comment


    let go e =
      let path_state = Env.local e in
      maybe_add_comment path_state;
      Logger.debug ~level:5 "@[Evaluating@ %a@]" Path_state.pp_loc path_state;
      match Path_state.dba_instruction path_state with
      | Dba.Instr.Assign (lvalue, rvalue, idx) ->
         let e = assignment ~lvalue ~rvalue e in
         Env.{ e with local = Path_state.set_block_index idx e.local }
      | Dba.Instr.Nondet(lvalue,region,idx) ->
        let e = nondet ~lvalue ~region e in
        Env.{ e with local = Path_state.set_block_index idx e.local }

      | Dba.Instr.SJump (jump_target, _) -> begin
          match static_jump ~jump_target e.Env.local with
          | None -> (* This jump has been forbidden *)
             Env.pick_path e
          | Some local -> {e with Env.local}
        end
      | Dba.Instr.If (condition, jump_target, local_target) ->
         ite ~condition ~jump_target ~local_target e

      | Dba.Instr.DJump (je, _) -> dynamic_jump ~jump_expr:je e
      | Dba.Instr.Undef(_, idx) as instruction -> skip instruction idx e
      | Dba.Instr.Stop _ ->
         (* Discard current path, choose a new one *)
         Env.pick_path e
      | Dba.Instr.Assert _
      | Dba.Instr.Assume _
      | Dba.Instr.NondetAssume _
      | Dba.Instr.Malloc _
      | Dba.Instr.Free _
      | Dba.Instr.Print _ as dba_instruction ->
         let msg =
           Format.asprintf "%a" Dba_printer.Ascii.pp_instruction dba_instruction in
         Errors.not_yet_implemented msg
  end

  let is_sat p =
    let sat_status, _ = Sse_smt.Solver.check_satistifiability p in
    sat_status = Formula.SAT

  type directive =
    | Continue
    | Change

  let loop_until ~halt g =
    let e = Env.of_global g in
    let last_vaddr = ref (Path_state.virtual_address @@ Env.local e) in
    let rec loop_aux e =
      let p = Env.local e in
      let vaddr = Path_state.virtual_address p in
      if vaddr <> !last_vaddr then begin
          last_vaddr := vaddr;
          let glob = Env.global e in
          let gopt = G.Goals.at vaddr g in
          match gopt with
          | None -> reloop e Continue
          | Some g ->
             let open Action in
             match goal g with
             (* Branch choice is handled later on the DBA instruction itself *)
             | Choice _ -> reloop e Continue
             | Cut ->
                Logger.result "@[<h>Action :: cut %@ %a@]"
                  Virtual_address.pp vaddr;
                reloop e Change
             | Reach c ->
                if is_sat p then begin
                    let c' = Count.decr c in
                    Logger.result "@[<h>Action :: reached address %a (%a to go)@]"
                      Virtual_address.pp vaddr Count.pp c';
                    (match Sse_smt.Solver.get_model p with
                    | Some m ->
                       Logger.result "@[<v 0>Model %@ %a@ %a@]"
                         Virtual_address.pp vaddr
                         Smt_model.pp m;
                    | None ->
                       Logger.result
                         "@[<h>No model %@ %a@]" Virtual_address.pp vaddr);
                    (match c' with
                     | Count.Count 0 -> G.Goals.remove vaddr glob
                     | Count.Count n ->
                        G.Goals.update vaddr (Action.reach ~n vaddr) glob
                     | Count.Unlimited -> ());
                    reloop e Continue
                  end
                else
                  (* It's not SAT - not counted as a reach *)
                  reloop e Change
             | Enumerate (k, ex) ->
                let e_fml =
                  Sse_smt.Translate.expr (Path_state.symbolic_state p) ex in
                let enumerate_at_most k =
                   let bv_vs, _p =
                     Sse_smt.Solver.enumerate_values k e_fml p in
                   G.Goals.Enumeration.record vaddr bv_vs glob;
                   let n = G.Goals.Enumeration.count vaddr glob in
                   let vs = G.Goals.Enumeration.get vaddr glob in
                   Logger.result
                     "@[<hov 0>Action :: enumerate@ \
                      possible values (%d) for %a %@ %a:@ @[<hov 0>%a@]@]"
                     n
                     Dba_printer.EICAscii.pp_bl_term ex
                     Virtual_address.pp vaddr
                     (Print_utils.pp_list ~sep:",@ " Bitvector.pp) vs;
                   n in
                (match k with
                 | Count.Count k ->
                    let m = k - enumerate_at_most k in
                    if m <= 0 then G.Goals.remove vaddr glob
                 | Count.Unlimited ->
                    ignore @@ enumerate_at_most max_int
                );
                reloop e Continue
             | Restrict (e, v) ->
                let symb_state = Path_state.symbolic_state p in
                let e1 = Sse_smt.Translate.expr symb_state e
                and e2 = Sse_smt.Translate.expr symb_state v in
                let a = Formula.mk_bv_equal e1 e2 in
                let local = Path_state.add_assertion a p in
                let e = Env.create ~global:glob ~local in
                reloop e Continue
        end
      else reloop e Continue
      and reloop e directive =
        if not @@ G.Goals.has (Env.global e) then halt e
        else
          match directive with
          | Continue -> loop_aux @@ Eval.go e
          | Change -> loop_aux @@ Env.pick_path e
    in loop_aux @@ Env.of_global g


let interval_or_set_to_cond expr is =
  let open Parse_helpers.Initialization in
  match is with
  | SignedInterval (e1, e2) ->
    Dba.Expr.logand (Dba.Expr.sle e1 expr) (Dba.Expr.sle expr e2)
  | UnsignedInterval (e1, e2) ->
    Dba.Expr.logand (Dba.Expr.ule e1 expr) (Dba.Expr.ule expr e2)
  | Set l ->
    match l with
    | [] -> assert false
    | a :: b ->
      let f = Dba.Expr.eq expr in
      List.fold_left (fun acc e -> Dba.Expr.logor acc @@ f e) (f a) b


let initialize_state ~filename ps =
  let ps =
    let cli_counters = Visit_address_counter.get () in
    match cli_counters with
    | [] -> ps
    | cs ->
       Logger.info "Found some address counters ... great";
       let m =
         let open Virtual_address in
         List.fold_left
           (fun m c -> Map.add c.Address_counter.address c m) Map.empty cs in
         Path_state.set_address_counters m ps
    in

  if not (Sys.file_exists filename) then begin
    Logger.warning "Cannot find sse configuration file %s" filename;
    ps
    end
  else
    let initials =
      Logger.debug "Reading initialization from %s" filename;
      let parser = Parser.initialization
      and lexer = Lexer.token in
      Parse_utils.read_file ~parser ~lexer ~filename
    in
    let f ps = function
      | Parse_helpers.Initialization.Assignment (lval, rval) ->
        Sse_smt.Translate.assignment lval rval ps
      | Parse_helpers.Initialization.MemLoad (addr, size) ->
        Path_state.with_init_mem_at ps ~addr ~size
      | Parse_helpers.Initialization.NondeterministicAssignment(lval,x) ->
        let state = Sse_smt.Translate.nondet lval ps in
        let cond = interval_or_set_to_cond (Dba.LValue.to_expr lval) x  in
        let state = Sse_smt.Translate.assume cond state in
        state
    in List.fold_left f ps initials


let do_sse ~filename =
  let level = 3 in
  Logger.debug ~level "Running SSE on %s" filename;
  let entrypoint = get_entry_point () in
  Logger.debug ~level "Starting from %a" Virtual_address.pp entrypoint;
  let initialize_fun =
    initialize_state ~filename:(Sse_options.MemoryFile.get ()) in
  Logger.debug ~level "Initialization done ...";
  Logger.debug ~level "Driver set ...";
  try loop_until ~halt (G.from_address ~initialize_fun ~entrypoint)
  with G.Path.Empty_worklist -> ()

let start () =
    let filename = Kernel_options.ExecFile.get () in
    do_sse ~filename

end

let run () =
  if Sse_options.is_enabled () && Kernel_options.ExecFile.is_set () then
    let (module H) =
      match Search_heuristics.get () with
      | Dfs -> (module Dfs_global:GLOBAL_ENV)
      | Bfs -> (module Bfs_global:GLOBAL_ENV)
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
         (module Nurs_global:GLOBAL_ENV)
    in let module S = Env_make(H) in S.start ()


let _ =
  Cli.Boot.enlist ~name:"SSE" ~f:run
