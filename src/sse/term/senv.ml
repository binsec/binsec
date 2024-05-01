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

let solvers =
  let open Formula_options in
  [ Bitwuzla; Boolector; Z3; CVC4; Yices ]

let map =
  let open Formula_options in
  let open Smt_options in
  function
  | Auto | Bitwuzla_builtin | Bitwuzla_legacy -> assert false
  | Bitwuzla_smtlib -> Bitwuzla
  | Boolector_smtlib -> Boolector
  | Z3_smtlib -> Z3
  | CVC4_smtlib -> CVC4
  | Yices_smtlib -> Yices

let get_solver () : (module Solver.OPEN) =
  let module Logger = Smt_options.Logger in
  match Smt_options.SMTSolver.get () with
  | (Smt_options.Auto | Smt_options.Bitwuzla_builtin)
    when Option.is_some Libsolver.bitwuzla_cxx ->
      Logger.debug "Use native Bitwuzla binding (cxx).";
      let module Api = Api_solver.Make ((val Option.get Libsolver.bitwuzla_cxx)) in
      (module Api.Open)
  | Smt_options.Auto | Smt_options.Bitwuzla_builtin
  | Smt_options.Bitwuzla_legacy
    when Option.is_some Libsolver.bitwuzla_c ->
      Logger.debug "Use native Bitwuzla binding (c).";
      let module Api = Api_solver.Make ((val Option.get Libsolver.bitwuzla_c)) in
      (module Api.Open)
  | Auto -> (
      try
        let solver = List.find Prover.ping solvers in
        Logger.info "Found %a in the path." Prover.pp solver;
        Formula_options.Solver.set solver;
        (module Smt2_solver.Solver)
      with Not_found -> Logger.fatal "No SMT solver found.")
  | Bitwuzla_builtin | Bitwuzla_legacy ->
      Logger.fatal "Native bitwuzla binding is required but not available."
  | solver when Prover.ping (map solver) ->
      Smt_options.Logger.debug "Found %a in the path." Prover.pp (map solver);
      Formula_options.Solver.set (map solver);
      (module Smt2_solver.Solver)
  | solver ->
      Logger.fatal "%a is required but not available in path." Prover.pp
        (map solver)

exception Undef = Types.Undef
exception Uninterp = Types.Uninterp
exception Unknown = Types.Unknown
exception Non_unique = Types.Non_unique
exception Non_mergeable = Types.Non_mergeable

type 'a test = 'a Types.test =
  | True of 'a
  | False of 'a
  | Both of { t : 'a; f : 'a }

(* utils *)

module BiMap = Basic_types.BigInt.Map
module NiTbl = Basic_types.Int.Htbl
open Sexpr
module BiItM = Imap
module S = Basic_types.String.Map
module I = Basic_types.Int.Map
module R = Basic_types.Int.Htbl
module K = Basic_types.Int.Set

type _ Types.value += Term : Sexpr.Expr.t Types.value
type lazy_memory = Solver.lazy_memory

module State
    (D : Domains.S)
    (Solver : Solver.GET_MODEL_WITH_STATS)
    (QS : Types.QUERY_STATISTICS) =
struct
  module Uid = struct
    type t = Suid.t

    let zero = Suid.incr Suid.zero
    (* zero is reserved for initial memory *)

    let succ = Suid.incr
    let compare = Suid.compare
  end

  module Solver = Solver (QS)

  let addr_space = Kernel_options.Machine.word_size ()

  let timeout =
    match Formula_options.Solver.Timeout.get () with
    | 0 -> None
    | n -> Some (float_of_int n)

  type t = {
    constraints : Expr.t list;
    (* reversed sequence of assertions *)
    mutable deps : BvSet.t BvMap.t;
    mutable domains : D.t BvMap.t;
    mutable anchors : K.t;
    vsymbols : Expr.t I.t;
    (* collection of visible symbols *)
    varrays : Memory.t S.t;
    (* collection of visible arrays *)
    vmemory : Memory.t;
    (* visible memory *)
    lmem : lazy_memory;
    (* set of lazily initialized memory locations *)
    model : Model.t; (* a model that satisfy constraints *)
  }

  module C : Ai.CONTEXT with type t = t and type v := D.t = struct
    type nonrec t = t

    let add_dependency t ~parent e =
      t.deps <-
        BvMap.add e
          (BvSet.add parent
             (try BvMap.find e t.deps with Not_found -> BvSet.empty))
          t.deps

    let find_dependency t e = BvMap.find e t.deps
    let add t e v = t.domains <- BvMap.add e v t.domains
    let find t e = BvMap.find e t.domains
  end

  module Overapprox : Memory_manager.CONTEXT with type t = t and type v := D.t =
  struct
    include Ai.Make (D) (C)

    let anchor t (m : Memory.t) =
      match m with
      | Root | Symbol _ -> ()
      | Layer { id; _ } -> t.anchors <- K.add id t.anchors

    let anchored t (m : Memory.t) =
      match m with
      | Root | Symbol _ -> true
      | Layer { id; _ } -> K.mem id t.anchors
  end

  module MMU = Memory_manager.Make (D) (Overapprox)

  let pp ppf state = Model.pp ppf state.model

  let empty () =
    {
      constraints = [];
      deps = BvMap.empty;
      domains = BvMap.empty;
      anchors = K.empty;
      vsymbols = I.empty;
      varrays = S.empty;
      vmemory = Memory.root;
      lmem = { content = BiItM.empty; lemmas = []; addr_space };
      model = Model.empty addr_space;
    }

  let alloc ~array state =
    let symbol = Memory.fresh array in
    { state with varrays = S.add array symbol state.varrays }

  let assign ({ id; _ } : Types.Var.t) value state =
    { state with vsymbols = I.add id value state.vsymbols }

  let write ~addr value dir state =
    let vmemory = MMU.write state ~addr value dir state.vmemory in
    { state with vmemory }

  let store name ~addr value dir state =
    try
      let ar = S.find name state.varrays in
      let varrays =
        S.add name (MMU.write state ~addr value dir ar) state.varrays
      in
      { state with varrays }
    with Not_found -> raise_notrace (Uninterp name)

  let lookup ({ id; _ } as var : Types.Var.t) t =
    try I.find id t.vsymbols with Not_found -> raise_notrace (Undef var)

  let read ~addr bytes dir state =
    let bytes = MMU.read state ~addr bytes dir state.vmemory in
    (bytes, state)

  let select name ~addr bytes dir state =
    try
      let array = S.find name state.varrays in
      let bytes = MMU.read state ~addr bytes dir array in
      (bytes, state)
    with Not_found -> raise_notrace (Uninterp name)

  let memcpy ~addr len orig state =
    let base = Bv.value_of addr in
    let lmem =
      {
        state.lmem with
        content =
          BiItM.add ~base len (Bv.value_of addr, orig) state.lmem.content;
      }
    in
    let vmemory =
      MMU.source state ~addr:(Expr.constant addr) ~len orig state.vmemory
    in
    { state with lmem; vmemory }

  let assume cond state =
    if Expr.is_equal cond Expr.one then (
      QS.Preprocess.incr_true ();
      Some state)
    else if Expr.is_equal cond Expr.zero then (
      QS.Preprocess.incr_false ();
      None)
    else
      match D.is_zero (Overapprox.eval state cond) with
      | True ->
          QS.Preprocess.incr_false ();
          None
      | False ->
          QS.Preprocess.incr_true ();
          Some state
      | Unknown ->
          let constraints = cond :: state.constraints in
          if Bitvector.zero = Model.eval state.model cond then (
            QS.Solver.start_timer ();
            let r = Solver.check_sat ?timeout state.lmem constraints in
            QS.Solver.stop_timer ();
            match r with
            | Unknown -> raise Unknown
            | Unsat -> None
            | Sat model ->
                Overapprox.refine state cond D.one;
                Some { state with constraints; model })
          else (
            QS.Preprocess.incr_true ();
            Overapprox.refine state cond D.one;
            Some { state with constraints })

  let test cond state =
    if Expr.is_equal cond Expr.one then (
      QS.Preprocess.incr_true ();
      True state)
    else if Expr.is_equal cond Expr.zero then (
      QS.Preprocess.incr_false ();
      False state)
    else
      match D.is_zero (Overapprox.eval state cond) with
      | True ->
          QS.Preprocess.incr_false ();
          False state
      | False ->
          QS.Preprocess.incr_true ();
          True state
      | Unknown -> (
          let tcons = cond :: state.constraints
          and fcons = Expr.lognot cond :: state.constraints in
          let e = Model.eval state.model cond in
          let to_check, constraints =
            if Bv.is_zero e then (tcons, fcons) else (fcons, tcons)
          in
          QS.Solver.start_timer ();
          let r = Solver.check_sat ?timeout state.lmem to_check in
          QS.Solver.stop_timer ();
          match r with
          | Unknown -> raise Unknown
          | Unsat ->
              if Bv.is_zero e then (
                Overapprox.refine state cond D.zero;
                False { state with constraints })
              else (
                Overapprox.refine state cond D.one;
                True { state with constraints })
          | Sat model ->
              let t, f =
                if Bv.is_zero e then
                  ( { state with constraints = to_check; model },
                    { state with constraints } )
                else
                  ( { state with constraints },
                    { state with constraints = to_check; model } )
              in
              Overapprox.refine t cond D.one;
              Overapprox.refine f cond D.zero;
              Both { t; f })

  let enumerate =
    let with_solver state e n enum except =
      QS.Solver.start_timer ();
      match
        Solver.fold_values ?timeout state.lmem state.constraints e ~n ~except
          (fun bv model enum ->
            let cond = Expr.equal e (Expr.constant bv) in
            let state =
              { state with constraints = cond :: state.constraints; model }
            in
            ignore (Overapprox.eval state cond);
            Overapprox.refine state cond D.one;
            (bv, state) :: enum)
          enum
      with
      | exception Unknown ->
          QS.Solver.stop_timer ();
          raise Unknown
      | enum ->
          QS.Solver.stop_timer ();
          enum
    in
    fun e ?(n = 1) ?(except = []) state ->
      match e with
      | Expr.Cst bv when List.mem bv except = false ->
          QS.Preprocess.incr_const ();
          [ (bv, state) ]
      | Expr.Cst _ ->
          QS.Preprocess.incr_const ();
          []
      | _ -> (
          let bv = Model.eval state.model e in
          let n, enum, except =
            if List.mem bv except then (n, [], except)
            else (
              QS.Preprocess.incr_const ();
              let cond = Expr.equal e (Expr.constant bv) in
              let state =
                { state with constraints = cond :: state.constraints }
              in
              ignore (Overapprox.eval state cond);
              Overapprox.refine state cond D.one;
              (n - 1, [ (bv, state) ], bv :: except))
          in
          if n = 0 then enum
          else
            let size = Expr.sizeof e and d = Overapprox.eval state e in
            match D.project ~size d with
            | Point _ -> enum
            | Top | Seq _ -> with_solver state e n enum except)

  let merge ~parent t t' =
    if t == t' then t
    else if t.lmem == t'.lmem then
      match (t.constraints, t'.constraints) with
      | c :: constraints, c' :: constraints'
        when constraints == constraints' && Expr.is_equal c (Expr.lognot c') ->
          let domains = parent.domains
          and anchors = K.union t.anchors t'.anchors
          and deps =
            BvMap.merge
              (fun _ o o' ->
                match (o, o') with
                | None, None -> assert false
                | None, Some _ -> o'
                | Some _, None -> o
                | Some d, Some d' -> Some (BvSet.union d d'))
              t.deps t'.deps
          and vsymbols =
            if t.vsymbols == t'.vsymbols then t.vsymbols
            else
              I.merge
                (fun _ o0 o1 ->
                  match (o0, o1) with
                  | Some e0, Some e1 ->
                      if Expr.is_equal e0 e1 then o0
                      else Some (Expr.ite c e0 e1)
                  | (Some _ | None), (Some _ | None) ->
                      raise_notrace Non_mergeable)
                t.vsymbols t'.vsymbols
          and varrays =
            if t.varrays == t'.varrays then t.varrays
            else
              S.merge
                (fun _ o0 o1 ->
                  match (o0, o1) with
                  | Some a0, Some a1 -> Some (MMU.merge parent c a0 a1)
                  | (Some _ | None), (Some _ | None) ->
                      raise_notrace Non_mergeable)
                t.varrays t'.varrays
          and vmemory = MMU.merge parent c t.vmemory t'.vmemory
          and lmem = t.lmem
          and model = t.model in
          {
            constraints;
            deps;
            domains;
            anchors;
            vsymbols;
            varrays;
            vmemory;
            lmem;
            model;
          }
      | _ -> raise_notrace Non_mergeable
    else raise_notrace Non_mergeable

  module Value = struct
    type t = Expr.t

    let kind = Term
    let constant = Expr.constant
    let var id name size = Expr.var (name ^ Suid.to_string id) size name
    let unary = Expr.unary
    let binary = Expr.binary
    let ite = Expr.ite
  end

  let assertions t = t.constraints

  let get_value (e : Expr.t) _ =
    match e with Cst bv -> bv | _ -> raise_notrace Non_unique

  let get_a_value (e : Expr.t) t = Model.eval t.model e

  let pp_smt (target : Expr.t Types.target) ppf t =
    let module P = Smt2_solver.Printer in
    let ctx = P.create ~next_id:Uid.zero () in
    (* visit assertions *)
    List.iter (P.visit_bl ctx) t.constraints;
    (* visit terms *)
    let defs =
      match target with
      | Some defs ->
          List.iter (fun (e, _) -> P.visit_bv ctx e) defs;
          defs
      | None ->
          P.visit_ax ctx t.vmemory;
          List.rev
            (I.fold
               (fun id expr defs ->
                 match Dba.Var.from_id id with
                 | exception Not_found -> defs
                 | { name; _ } ->
                     P.visit_bv ctx expr;
                     (expr, name) :: defs)
               t.vsymbols [])
    in
    Format.pp_open_vbox ppf 0;
    (* print definitions *)
    P.pp_print_defs ppf ctx;
    List.iter
      (fun (bv, name) ->
        Format.fprintf ppf "@[<h>(define-fun %s () (_ BitVec %d)@ " name
          (Expr.sizeof bv);
        P.pp_print_bv ctx ppf bv;
        Format.fprintf ppf ")@]@ ")
      defs;
    if target = None then
      Format.fprintf ppf
        "@[<h>(define-fun memory () (Array (_ BitVec %d) (_ BitVec 8))@ %a)@]"
        (Kernel_options.Machine.word_size ())
        (P.pp_print_ax ctx) t.vmemory;
    (* print assertions *)
    List.iter
      (fun bl ->
        Format.pp_open_hbox ppf ();
        Format.pp_print_string ppf "(assert ";
        P.pp_print_bl ctx ppf bl;
        Format.pp_print_char ppf ')';
        Format.pp_close_box ppf ();
        Format.pp_print_space ppf ())
      t.constraints;
    Format.pp_close_box ppf ()

  let to_formula t =
    let module C = Smt2_solver.Cross in
    let ctx = C.create ~next_id:Uid.zero () in
    List.iter (C.assert_bl ctx) t.constraints;
    C.define_ax ctx "memory" t.vmemory;
    I.iter
      (fun id expr -> C.define_bv ctx (Dba.Var.from_id id).name expr)
      t.vsymbols;
    C.to_formula ctx

  let downcast _ = None
end

type Options.Engine.t += Vanilla | Multi_checks

let () =
  Options.Engine.register "vanilla" Vanilla (fun () ->
      let module Solver = Solver.Once ((val get_solver ())) in
      (module State (Domains.Interval) (Solver)))

let () =
  Options.Engine.register "multi-checks" Multi_checks (fun () ->
      let module Solver = Solver.MultiChecks ((val get_solver ())) in
      (module State (Domains.Interval) (Solver)))
