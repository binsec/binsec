(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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
    Kernel_functions.get_img ()
    |> Loader.Img.entry
    |> Virtual_address.create

module type SSE_RUNNER = sig val start: unit -> unit end

module Env_make(G: GLOBAL_ENV): SSE_RUNNER =
struct
  module Stats = struct
    type t = {
      visited : int; (* Does not count the number uniquely visited paths but
                      * should ... *)
      choices : int;
    }

    let empty = { visited = 0; choices = 0; }
    let add_visited s = { s with visited = s.visited + 1 }
    let add_choice  s = { s with choices = s.choices + 1 }

    let pp ppf s =
      Format.fprintf ppf
        "@[<v 0>\
         @[<h>selections %d@]@,\
         @[<h>choice     %d@]\
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
      local  : Path_state.t;
      cfg : C.t;
      entrypoint : Virtual_address.t ;
    }

    let create ~global ~local =
      { global;
        local;
        cfg = Path_state.cfg local;
        entrypoint = Path_state.entrypoint local;
      }

    let local e = e.local
    let global e = e.global

    let of_global g =
      let global, local = G.Path.choose g in
      create ~global ~local

    let current_address e =
      Path_state.virtual_address @@ local e

    let goals_here e =
      G.Directives.at (current_address e) (global e)

    let choice_goals_here e =
      match goals_here e with
      | None -> []
      | Some q ->
        Queue.fold
          (fun l d -> if Directive.is_choice d then d :: l else l) [] q

    let check_sat e =
      let sat_status, _ = Sse_smt.Solver.check_satistifiability e.local in
      sat_status

    let rec pick_path e =
      Stats.add_visited ();
      let e = of_global (global e) in
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
        if Path_state.may_lead_to_goal p then Some p
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
        let open Directive in
        match choice_goals_here e with
        | d :: ds ->
          if ds <> [] then
            Logger.warning "@[<h>%@ Ignoring %d choice directives@]"
              (List.length ds);
          begin match directive d with
            | Choice c ->
              let first, second =
                if Choice.is_alternative c then alternative, consequent
                else consequent, alternative in
              Choice.do_alternate c;
              do_pick ~first ~second
            | _ -> assert false
          end
        | [] ->
          let first, second =
            if Sse_options.Randomize.get () && Random.bool ()
            then alternative, consequent
            else consequent, alternative in
          do_pick ~first ~second


    let add_if_good e path_state =
      match is_good (Some path_state) with
      | None ->
        Logger.info "Goal unreachable from@ %a" Path_state.pp_loc path_state;
        e
      | Some path_state ->
        { e with global = G.Path.add path_state e.global }

  end

  let halt e =
    Logger.info
      "@[<v 0>\
       @[<v 2>SMT queries@,%a@]@,\
       @[<v 2>Exploration@,%a@]@,\
       @]"
      Sse_smt.Query_stats.pp ()
      Stats.pp ();
    if Sse_options.Dot_filename_out.is_set () then
      let filename = Sse_options.Dot_filename_out.get () in
      Logger.info "Outputting CFG in %s" filename;
      let oc = open_out_bin filename in
      let cfg = e.Env.cfg in
      begin
        match C.mem_vertex_a cfg e.Env.entrypoint with
        | None -> ()
        | Some entry -> C.output_graph oc e.Env.cfg [] ~entry
      end;
      close_out oc

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
       * update symbolic_state.
       *)
      let open Sse_smt in
      Env.{ e with local = Translate.assignment lvalue rvalue e.local }

    let nondet ~lvalue ~region e =
      let _ = region in
      (* generate the logical constraint, add it to the path predicate,
       * update symbolic_state.
       *)
      let open Sse_smt in
      Env.{ e with local = Translate.nondet lvalue e.local }


    let ite ~condition ~jump_target ~local_target e =
      (* expand path with assert condition and go to jump_target *)
      (* push path with assert not condition and go to local_target *)
      let state = Env.local e in
      let condition =
        let open Formula in
        let syst = Path_state.symbolic_state state in
        mk_bv_equal (Sse_smt.Translate.expr syst condition) mk_bv_one
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
        let address = Virtual_address.to_int addr in
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

    (* If comment is activated, this will add, for every formula entry, a
       comment about where it comes from.

       This can be usefule to debug the path predicate translation.  *)
    let maybe_add_comment ps =
      if Sse_options.Comment.get () then
        let syst = Path_state.symbolic_state ps in
        let comment =
          Print_utils.string_from_pp
            (Formula_pp.pp_as_comment Path_state.pp_loc) ps
        in
        let symbolic_state = Sse_symbolic.State.comment comment syst in
        Path_state.set_symbolic_state symbolic_state ps
      else ps


    let go e =
      let path_state = maybe_add_comment @@ Env.local e in
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

  let sat_status p =
    fst @@ Sse_smt.Solver.check_satistifiability p

  type path_directive =
    | Continue
    | Discard

  let loop_until ~halt g =
    let get_vaddr e = Path_state.virtual_address @@ Env.local e in
    let e = Env.of_global g in
    let last_vaddr = ref (get_vaddr e) in

    let rec loop_aux e =
      let vaddr = get_vaddr e in
      if vaddr <> !last_vaddr then begin
        Logger.debug ~level:2 "%@%a %a"
          Virtual_address.pp vaddr
          Mnemonic.pp (Env.local e |> Path_state.inst |> Instruction.mnemonic)
        ;
        last_vaddr := vaddr;
        do_directives vaddr e
      end
      (* When the last virtual addresse has not changed, when are still in the
         same DBA block, hence no user action can have been performed.
         So, we just continue.
      *)
      else reloop e Continue

    and reloop e directive =
      if not @@ G.Directives.has (Env.global e) then halt e
      else
        let e_action =
          match directive with
          | Continue -> Eval.go
          | Discard  -> Env.pick_path in
        match e_action e with
        | e -> loop_aux e
        | exception G.Path.Empty_worklist -> halt e

    and do_directives vaddr e =
      let glob = Env.global e in
      match G.Directives.at vaddr g with
      | None -> reloop e Continue
      | Some q ->
        let open Directive in
        (* q' is the new queue that will replace the old one.
           Uses side effects. *)
        let q' = Queue.create () in
        let rec handle_directives e path_directive =
          if Queue.is_empty q then begin
            if Queue.is_empty q' then G.Directives.remove vaddr glob
            else G.Directives.update vaddr q' glob;
            reloop e path_directive
          end
          else
            let g = Queue.take q in
            let p = Env.local e in
            match directive g with
            | Choice _ ->
              (* Branch choice is handled later on the DBA instruction itself *)
              Queue.add g q';
              handle_directives e path_directive
            | Cut ->
              Queue.add g q';
              Queue.clear q;
              Logger.result "@[<h>Directive :: cut %@ %a@]"
                Virtual_address.pp vaddr;
              handle_directives e Discard
            | Reach c ->
              Logger.debug "Reach";
              begin
                match sat_status p with
                | Formula.SAT ->
                   begin
                     let c' = Count.decr c in
                     Logger.result
                       "@[<h>Directive :: reached address %a (%a to go)@]"
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
                      (* Do not add it to q', this is done*)
                      | Count.Count 0 -> ()
                      | Count.Count n ->
                         let loc = Loader_utils.Binary_loc.address vaddr in
                         Queue.add (Directive.reach ~n loc) q'
                      | Count.Unlimited ->
                         Queue.add g q') ;
                     handle_directives e path_directive
                   end
                | status ->
                   begin
                     Logger.warning
                       "@[<h>Directive :: reach \
                        reached address %a with %a \
                        (still %a to go)@]"
                       Virtual_address.pp vaddr Formula_pp.pp_status status
                       Count.pp c ;
                     Queue.add g q';
                     (* It's not SAT - not counted as a reach *)
                     handle_directives e Discard
                   end
              end
            | Enumerate (k, ex) ->
              let e_fml =
                Sse_smt.Translate.expr (Path_state.symbolic_state p) ex in
              let enumerate_at_most k =
                let bv_vs, _p =
                  Sse_smt.Solver.enumerate_values k e_fml p in
                G.Directives.Enumeration.record vaddr ex bv_vs glob;
                let n = G.Directives.Enumeration.count vaddr ex glob in
                let vs = G.Directives.Enumeration.get vaddr ex glob in
                Logger.result
                  "@[<hov 0>Directive :: enumerate@ \
                   possible values (%d) for %a %@ %a:@ @[<hov 0>%a@]@]"
                  n
                  Dba_printer.EICAscii.pp_bl_term ex
                  Virtual_address.pp vaddr
                  (Print_utils.pp_list ~sep:",@ " Bitvector.pp) vs;
                n in
              (match k with
               | Count.Count k ->
                 let m = k - enumerate_at_most k in
                 if m > 0 then
                   let loc = Loader_utils.Binary_loc.address vaddr in
                   Queue.add (Directive.enumerate ~n:m ex loc) q'
               | Count.Unlimited ->
                 Queue.add g q';
                 ignore @@ enumerate_at_most max_int
              );
              handle_directives e path_directive
            | Assume expr ->

              let p =
                (* add comment *)
                let comment =
                  Format.asprintf
                    "@[<h>user constraint : assume %a @]"
                    Dba_printer.EICAscii.pp_bl_term expr in
                Logger.debug "Assume %@ %a" Virtual_address.pp vaddr;
                let symb_state =
                  Path_state.symbolic_state p
                  |> Sse_symbolic.State.comment comment in
                Path_state.set_symbolic_state symb_state p in
              (* Adding the formula itself *)
              let local = Sse_smt.Translate.assume expr p in
              let e = Env.create ~global:glob ~local in
              Queue.add g q';
              handle_directives e path_directive
        in handle_directives e Continue

    in
    let e = Env.of_global g in
    loop_aux e



  let interval_or_set_to_cond expr is =
    let open Parse_helpers.Initialization in
    match is with
    | Singleton _ -> assert false
    | Signed_interval (e1, e2) ->
      Dba.Expr.logand (Dba.Expr.sle e1 expr) (Dba.Expr.sle expr e2)
    | Unsigned_interval (e1, e2) ->
      Dba.Expr.logand (Dba.Expr.ule e1 expr) (Dba.Expr.ule expr e2)
    | Set l ->
      match l with
      | [] -> assert false
      | a :: b ->
        let f = Dba.Expr.equal expr in
        List.fold_left (fun acc e -> Dba.Expr.logor acc @@ f e) (f a) b


  let initialize_state ~filename ps =
    let ps =
      let cli_counters = Visit_address_counter.get () in
      match cli_counters with
      | [] -> ps
      | cs ->
        Logger.info "Found some address counters ... great";
        let m =
          let open! Virtual_address in
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
      let f ps init =
        let open Parse_helpers.Initialization in
        match init.operation with
        | Mem_load (addr, size) ->
          Path_state.with_init_mem_at ps ~addr ~size
        | Universal lval ->
          begin
            match Dba_types.LValue.name_of lval with
            | Some name ->
              let symb = Path_state.symbolic_state ps in
              let size = Dba.LValue.size_of lval in
              let sort = Formula.BvSort size in
              let symb = Sse_symbolic.State.declare ~wild:true name sort symb in
              Path_state.set_symbolic_state symb ps
            | None -> ps
          end
        | Assignment (lval, rval, naming_hint) ->
          let wild = not init.controlled in
          match rval with
          | Singleton rv ->
            Sse_smt.Translate.assignment ~wild lval rv ps
          | x ->
            let state = Sse_smt.Translate.nondet ~wild ?naming_hint lval ps in
            let e = Dba.LValue.to_expr lval in
            let cond = interval_or_set_to_cond e x in
            Sse_smt.Translate.assume cond state
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
    loop_until ~halt (G.from_address ~initialize_fun ~entrypoint)


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
