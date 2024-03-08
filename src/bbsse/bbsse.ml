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

open Bbsse_types
open Bbsse_options
module V = Virtual_address

module A = struct
  include Dba_types.Caddress

  let of_jump_target a = function Dba.JInner n -> reid a n | Dba.JOuter t -> t
end

module I = Dba.Instr
module E = Dba.Expr
module D = Directive
module G = Bbsse_path_generator
module T = Smt_external.Translate
module P = Smt_solver
module N = Basic_types.Int

module S = struct
  include Status

  let pp_expecting ppf = function
    | Unknown -> ()
    | t ->
        Format.pp_print_string ppf " expecting ";
        pp ppf t

  let pp_expected ppf = function
    | Unknown -> ()
    | t ->
        Format.pp_print_string ppf " (expected ";
        pp ppf t;
        Format.pp_print_char ppf ')'
end

module Stat = struct
  let ground_truth = ref false
  let all_predicates = ref 0
  let deemed_opaques = ref 0
  let true_positives = ref 0
  let false_positives = ref 0
  let wrong_positives = ref 0
  let deemed_clear = ref 0
  let true_negatives = ref 0
  let false_negatives = ref 0
  let deemed_unreachable = ref 0
  let true_unreachable = ref 0
  let false_unreachable = ref 0
  let failure = ref 0
  let all_paths = ref 0

  type time = { mutable sec : float }

  let search_time = { sec = 0. }
  let eval_time = { sec = 0. }
  let preprocess_time = { sec = 0. }
  let total_time = { sec = 0. }
end

module Predicate = struct
  type t = {
    addr : A.t;
    mnemonic : string;
    test : Dba.Expr.t;
    mutable status : S.t;
    expect : S.t;
  }
end

module Eval = struct
  let rec loop state sym a c j =
    match A.Htbl.find state.G.dis a with
    | I.Assign (r, e, n) -> loop state (T.assign r e sym) (A.reid a n) c j
    | I.Undef (r, n) | I.Nondet (r, n) ->
        loop state (T.havoc r sym) (A.reid a n) c j
    | I.SJump (t, _) -> loop state sym (A.of_jump_target a t) c j
    | I.If (e, t, n) -> (
        match c with
        | [] -> T.expr sym e
        | true :: c -> loop state (T.assume e sym) (A.of_jump_target a t) c j
        | false :: c -> loop state (T.assume (E.lognot e) sym) (A.reid a n) c j)
    | I.DJump (e, _) -> (
        match j with
        | [] -> assert false
        | v :: j ->
            loop state
              (T.assume
                 (E.equal e
                    (E.constant
                       (Bitvector.create
                          (Virtual_address.to_bigint v)
                          (E.size_of e))))
                 sym)
              (A.of_virtual_address v) c j)
    | I.Stop _ -> assert false
    | I.Assert (e, n) | I.Assume (e, n) ->
        loop state (T.assume e sym) (A.reid a n) c j

  let run state (a, c, j) =
    incr Stat.all_paths;
    let t = Unix.gettimeofday () in
    let r = loop state (Smt_symbolic.State.create ()) a c j in
    Stat.eval_time.sec <- Stat.eval_time.sec +. Unix.gettimeofday () -. t;
    r
end

let do_optimization ?(keep = Formula.VarSet.empty) fm =
  let level = 3 in
  if Formula.VarSet.is_empty keep then Logger.debug ~level "Optimize"
  else
    Logger.debug ~level "@[<v 2>Optimize but keep intact these variables:@ %a@]"
      Formula_pp.pp_varset keep;
  let t = Unix.gettimeofday () in
  let r = Formula_transformation.optimize ~keep fm in
  Stat.preprocess_time.sec <-
    Stat.preprocess_time.sec +. Unix.gettimeofday () -. t;
  r

module Runner (Solver : Smt_sig.Solver) = struct
  let rec loop state status = function
    | [] -> status
    | p :: paths -> (
        let e, sym = Eval.run state p in
        match status with
        | S.Unreachable -> (
            let keep = Formula_utils.bv_term_variables e in
            let formula =
              do_optimization ~keep (Smt_symbolic.State.formula sym)
            in
            let solver = Solver.open_session () in
            Formula.iter_forward (Solver.put solver) formula;
            match Solver.check_sat solver with
            | SAT -> (
                let bv = Solver.get_bv_value solver e in
                Solver.put solver
                  Formula.(mk_assert (mk_bv_distinct (mk_bv_cst bv) e));
                match Solver.check_sat_and_close solver with
                | SAT -> S.Clear
                | UNSAT -> loop state (S.Opaque (Bitvector.to_bool bv)) paths
                | TIMEOUT | UNKNOWN -> S.Unknown)
            | UNSAT ->
                Solver.close_session solver;
                loop state S.Unreachable paths
            | TIMEOUT | UNKNOWN ->
                Solver.close_session solver;
                S.Unknown)
        | S.Unknown | S.Clear -> assert false
        | S.Opaque b -> (
            let sym =
              Smt_symbolic.State.constrain
                Formula.(mk_bv_distinct (mk_bv_cst (Bitvector.of_bool b)) e)
                sym
            in
            let formula = do_optimization (Smt_symbolic.State.formula sym) in
            let solver = Solver.open_session () in
            Formula.iter_forward (Solver.put solver) formula;
            match Solver.check_sat_and_close solver with
            | SAT -> S.Clear
            | UNSAT -> loop state status paths
            | TIMEOUT | UNKNOWN -> S.Unknown))

  let run state n (p : Predicate.t) =
    let k = Unix.gettimeofday () in
    Logger.debug "Running BB-SSE at %a (%a)%a" A.pp_base p.addr
      Dba_printer.Ascii.pp_bl_term p.test S.pp_expecting p.expect;
    let t = Unix.gettimeofday () in
    let paths = G.enumerate_path state n p.addr in
    Logger.debug "Found %a paths"
      (fun ppf paths -> Format.pp_print_int ppf (List.length paths))
      paths;
    Stat.search_time.sec <- Stat.search_time.sec +. Unix.gettimeofday () -. t;
    (match loop state S.Unreachable paths with
    | (exception _) | S.Unknown ->
        p.status <- S.Unknown;
        Logger.debug "Failed to handle predicate %a at %a%a"
          Dba_printer.Ascii.pp_bl_term p.test A.pp_base p.addr S.pp_expected
          p.expect
    | status ->
        p.status <- status;
        Logger.debug "Predicate %a at %a deemed %a%a"
          Dba_printer.Ascii.pp_bl_term p.test A.pp_base p.addr S.pp status
          S.pp_expected p.expect);
    Stat.total_time.sec <- Stat.total_time.sec +. Unix.gettimeofday () -. k

  let pp ppf () =
    let open Stat in
    Format.pp_open_vbox ppf 2;
    Format.fprintf ppf "total predicate                      %d" !all_predicates;
    Format.pp_print_space ppf ();
    Format.pp_open_vbox ppf 2;
    Format.fprintf ppf "total opaques predicates           %d"
      (!deemed_opaques + !true_positives + !false_positives + !wrong_positives);
    if !ground_truth then (
      Format.pp_print_space ppf ();
      Format.fprintf ppf "predicates deemed opaque         %d" !deemed_opaques;
      Format.pp_print_space ppf ();
      Format.fprintf ppf "true positives                   %d" !true_positives;
      Format.pp_print_space ppf ();
      Format.fprintf ppf "false positives                  %d" !false_positives;
      Format.pp_print_space ppf ();
      Format.fprintf ppf "wrong positives                  %d" !wrong_positives);
    Format.pp_close_box ppf ();
    Format.pp_print_space ppf ();
    Format.pp_open_vbox ppf 2;
    Format.fprintf ppf "total unreachable predicates       %d"
      (!deemed_unreachable + !true_unreachable + !false_unreachable);
    if !ground_truth then (
      Format.pp_print_space ppf ();
      Format.fprintf ppf "predicates deemed unreachable    %d"
        !deemed_unreachable;
      Format.pp_print_space ppf ();
      Format.fprintf ppf "true positives                   %d" !true_unreachable;
      Format.pp_print_space ppf ();
      Format.fprintf ppf "false positives                  %d"
        !false_unreachable);
    Format.pp_close_box ppf ();
    Format.pp_print_space ppf ();
    Format.pp_open_vbox ppf 2;
    Format.fprintf ppf "total clear predicates             %d"
      (!deemed_clear + !true_negatives + !false_negatives);
    if !ground_truth then (
      Format.pp_print_space ppf ();
      Format.fprintf ppf "predicates deemed clear          %d" !deemed_clear;
      Format.pp_print_space ppf ();
      Format.fprintf ppf "true negatives                   %d" !true_negatives;
      Format.pp_print_space ppf ();
      Format.fprintf ppf "false negatives                  %d" !false_negatives);
    Format.pp_close_box ppf ();
    Format.pp_print_space ppf ();
    Format.fprintf ppf "total unfinished (timeout)         %d" !failure;
    Format.pp_close_box ppf ();
    Format.pp_print_space ppf ();
    Format.pp_open_vbox ppf 0;
    Format.pp_print_space ppf ();
    if !ground_truth then
      Format.fprintf ppf "@[<h>        search cumulative time       %fs@]@,"
        search_time.sec;
    Format.fprintf ppf "@[<h>total explored paths                 %d@]@,"
      !all_paths;
    if !ground_truth then
      Format.fprintf ppf "@[<h>    evaluation cumulative time       %fs@]@,"
        eval_time.sec;
    Format.fprintf ppf "@[<h>total satisfiability queries         %d@]@,"
      (Solver.query_stat ());
    if !ground_truth then
      Format.fprintf ppf
        " @[<h> preprocessing cumulative time       %fs@]@,\
         @[<h>       solving cumulative time       %fs@]@,"
        preprocess_time.sec (Solver.time_stat ());
    Format.fprintf ppf "@[<h>         total cumulative time       %fs@]@,@]"
      total_time.sec

  let rec loop state bounds n predicates i =
    let s = Array.length predicates in
    if i < s then (
      let p : Predicate.t = Array.get predicates i in
      (match p.status with
      | S.Unknown | S.Clear ->
          Logger.debug ~level:0 "Analyzing the target %d / %d (%d step%s)"
            (i + 1) s (n + 1)
            (if n = 0 then "" else "s");
          run state n p
      | S.Opaque _ | S.Unreachable -> ());
      loop state bounds n predicates (i + 1))
    else if not (N.Set.is_empty bounds) then
      let n = N.Set.min_elt bounds in
      loop state (N.Set.remove n bounds) (n - 1) predicates 0
    else
      let open Stat in
      Array.iteri
        (fun i (p : Predicate.t) ->
          incr all_predicates;
          Logger.result "%d / %d: Predicate %s at %a deemed %a%a" (i + 1) s
            p.mnemonic A.pp_base p.addr S.pp p.status S.pp_expected p.expect;
          match (p.expect, p.status) with
          | S.Unknown, S.Unknown
          | S.Clear, S.Unknown
          | S.Opaque _, S.Unknown
          | S.Unreachable, S.Unknown ->
              incr failure
          | S.Unknown, S.Unreachable -> incr deemed_unreachable
          | S.Unknown, S.Opaque _ -> incr deemed_opaques
          | S.Unknown, S.Clear -> incr deemed_clear
          | S.Clear, S.Clear -> incr true_negatives
          | S.Clear, S.Opaque _ -> incr false_positives
          | S.Clear, S.Unreachable -> incr false_unreachable
          | S.Opaque _, S.Clear | S.Unreachable, S.Clear -> incr false_negatives
          | S.Opaque true, S.Opaque true | S.Opaque false, S.Opaque false ->
              incr true_positives
          | S.Unreachable, S.Unreachable -> incr true_unreachable
          | S.Opaque _, S.Opaque _ | S.Unreachable, S.Opaque _ ->
              incr wrong_positives
          | S.Opaque _, S.Unreachable -> incr false_unreachable)
        predicates;
      Logger.info "%a" pp ()
end

let find_jumps f cfg =
  let jump_targets =
    Array.of_list
      (Ghidra_cfg.fold_vertex
         (fun v jumps ->
           if f v then
             match Ghidra_cfg.succ_e cfg v with
             | [ (_, Branch, _); (_, Fallthrough, _) ]
             | [ (_, Fallthrough, _); (_, Branch, _) ] ->
                 v :: jumps
             | _ -> jumps
           else jumps)
         cfg [])
  in
  Array.sort V.compare jump_targets;
  jump_targets

let run () =
  if is_enabled () || FindAllJumps.get () || FindJumpsBetween.is_set () then (
    let cfg, ims = Ghidra_cfg.import () in
    (* calls to process, jumps to skip, ground truth *)
    let ctp, jts, gt =
      match Directives.get_opt () with
      | None -> (V.Set.empty, V.Set.empty, V.Map.empty)
      | Some path ->
          Logger.debug "Reading directives from %s" path;
          let ic = open_in path in
          List.fold_left
            (fun (ctp, jts, gt) -> function
              | D.ExpectAt (v, x) -> (ctp, jts, V.Map.add v x gt)
              | D.SkipJump v -> (ctp, V.Set.add v jts, gt)
              | D.ProcessCall v -> (V.Set.add v ctp, jts, gt))
            (V.Set.empty, V.Set.empty, V.Map.empty)
            (Bbsse_parser.directives Bbsse_lexer.token (Lexing.from_channel ic))
    in
    let ctp =
      Basic_types.Int.Set.fold
        (fun a ctp -> V.Set.add (V.create a) ctp)
        (CallsToProceed.get ()) ctp
    in
    Stat.ground_truth := not (V.Map.is_empty gt);

    let jump_targets =
      if FindAllJumps.get () then
        find_jumps (fun v -> not (V.Set.mem v jts)) cfg
      else
        match FindJumpsBetween.get () with
        | [] -> (
            match Kernel_functions.get_ep () with
            | None -> Logger.fatal "Please specify a start address"
            | Some v -> [| v |])
        | [ lo; hi ] ->
            let start, stop =
              (Virtual_address.create lo, Virtual_address.create hi)
            in
            find_jumps
              (fun v ->
                V.compare v start > 0
                && V.compare v stop < 0
                && not (V.Set.mem v jts))
              cfg
        | _ -> Logger.fatal "Find-jumps expects exactly two addresses."
    in
    let dis = A.Htbl.create (Ghidra_cfg.nb_vertex cfg) (* rough estimation *)
    and dap = A.Htbl.create (Ghidra_cfg.nb_edges cfg) (* rough estimation *)
    and opa = A.Htbl.create (Array.length jump_targets) in

    (* Corner case: add a default source if executable entry point
        has a predecessor *)
    let entry =
      Virtual_address.create (Loader.Img.entry (Kernel_functions.get_img ()))
    in
    if Ghidra_cfg.in_degree cfg entry <> 0 then (
      let root = Virtual_address.create 0 in
      assert (not (V.equal root entry));
      Ghidra_cfg.add_edge_e cfg (root, Fallthrough, entry);
      let root = A.of_virtual_address root
      and entry = A.of_virtual_address entry in
      A.Htbl.add dap entry [ root ];
      A.Htbl.add dis root (I.static_jump (JOuter entry)));

    let state = { G.cfg; ims; ctp; dis; dap; opa } in

    let bounds =
      match MaxBB.get () with [] -> N.Set.singleton 1 | l -> N.Set.of_list l
    in
    let n = N.Set.min_elt bounds in
    if n < 1 then
      Logger.fatal "The number of basic blocks should be greater or equal to 1.";
    let bounds = N.Set.remove n bounds in

    let predicates =
      Array.map
        (fun v ->
          try
            let addr, test = G.find_condition state v in
            Predicate.
              {
                addr;
                mnemonic = V.Htbl.find state.ims v;
                test;
                status = S.Unknown;
                expect = (try V.Map.find v gt with Not_found -> S.Unknown);
              }
          with Not_found ->
            Logger.fatal "Can not find a conditional jump at %a." V.pp v)
        jump_targets
    in

    let module R = Runner ((val P.get_solver () : Smt_sig.Solver)) in
    R.loop state bounds (n - 1) predicates 0)

let _ = Cli.Boot.enlist ~name:"BB-SSE" ~f:run
