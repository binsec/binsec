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

let solvers =
  let open Formula_options in
  [ Bitwuzla; Boolector; Z3; CVC4; Yices ]

let map =
  let open Formula_options in
  let open Smt_options in
  function
  | Auto | Bitwuzla_native -> assert false
  | Bitwuzla_smtlib -> Bitwuzla
  | Boolector_smtlib -> Boolector
  | Z3_smtlib -> Z3
  | CVC4_smtlib -> CVC4
  | Yices_smtlib -> Yices

let get_solver_factory () =
  let open Formula_options in
  let open Smt_options in
  match Smt_options.SMTSolver.get () with
  | (Smt_options.Auto | Smt_options.Bitwuzla_native) when Smt_bitwuzla.available
    ->
      (module Native_solver.Solver : Solver_sig.FACTORY)
  | Auto -> (
      try
        let solver = List.find Prover.ping solvers in
        Logger.info "Found %a in the path." Prover.pp solver;
        Solver.set solver;
        (module Smt2_solver.Solver : Solver_sig.FACTORY)
      with Not_found -> Logger.fatal "No SMT solver found.")
  | Bitwuzla_native ->
      Logger.fatal "Native bitwuzla binding is required but not available."
  | solver when Prover.ping (map solver) ->
      Solver.set (map solver);
      (module Smt2_solver.Solver : Solver_sig.FACTORY)
  | solver ->
      Logger.fatal "%a is required but not available in path." Prover.pp
        (map solver)

exception Unknown = Sse_types.Unknown

type 'a test = 'a Sse_types.test =
  | True of 'a
  | False of 'a
  | Both of { t : 'a; f : 'a }

(* utils *)
let pp_int_as_bv ppf x = function
  | 1 -> Format.fprintf ppf "#b%d" x
  | 4 -> Format.fprintf ppf "#x%01x" x
  | 8 -> Format.fprintf ppf "#x%02x" x
  | 12 -> Format.fprintf ppf "#x%03x" x
  | 16 -> Format.fprintf ppf "#x%04x" x
  | 20 -> Format.fprintf ppf "#x%05x" x
  | 24 -> Format.fprintf ppf "#x%06x" x
  | 28 -> Format.fprintf ppf "#x%07x" x
  | 32 -> Format.fprintf ppf "#x%08x" x
  | 64 when x >= 0 -> Format.fprintf ppf "#x%016x" x
  | sz -> Format.fprintf ppf "(_ bv%d %d)" x sz

let pp_bv ppf value size =
  try pp_int_as_bv ppf (Z.to_int value) size
  with Z.Overflow -> Format.fprintf ppf "(_ bv%a %d)" Z.pp_print value size

let byte_size = Natural.to_int Basic_types.Constants.bytesize

module BiTbl = Basic_types.BigInt.Htbl
module BiMap = Basic_types.BigInt.Map
module NiTbl = Basic_types.Int.Htbl
module Sname = Suid
open Sexpr
module BiItM = Imap
module BvSet = Set.Make (Expr)
module S = Basic_types.String.Map

module I = Map.Make (struct
  type t = Z.t

  let compare x y = -Z.compare x y
end)

module Model = struct
  type t = Bv.t BvTbl.t * char BiTbl.t

  let empty () = (BvTbl.create 0, BiTbl.create 0)

  let maybe_pp_char ppf c =
    if String_utils.is_char_printable c then Format.fprintf ppf " (%c)" c

  let pp_variables ppf vars values =
    if S.is_empty vars = false then (
      Format.pp_print_string ppf "# Variables";
      Format.pp_print_cut ppf ();
      S.iter
        (fun name list ->
          let list = List.rev list in
          Format.fprintf ppf "%s : @[<hov>%a@]@ " name
            (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf var ->
                 match BvTbl.find values var with
                 | exception Not_found -> Format.pp_print_string ppf "--"
                 | bv -> Bitvector.pp_hex_or_bin ppf bv))
            list;
          match list with
          | var :: _ :: _ when Expr.sizeof var = 8 ->
              Format.pp_print_string ppf "  [as ascii] ";
              List.iter
                (fun var ->
                  match BvTbl.find values var with
                  | exception Not_found -> Format.pp_print_string ppf "."
                  | bv -> Format.pp_print_char ppf (Bitvector.to_char bv))
                list;
              Format.pp_print_space ppf ()
          | _ -> ())
        vars)

  let pp_memory ppf memory addr_space =
    if BiTbl.length memory = 0 then
      Format.pp_print_string ppf "-- empty memory --"
    else (
      Format.pp_print_string ppf "# Memory";
      Format.pp_print_cut ppf ();
      let img = Kernel_functions.get_img () in
      let noname = "" in
      let section_name addr =
        let address = Virtual_address.to_int (Virtual_address.of_bigint addr) in
        match Loader_utils.find_section_by_address ~address img with
        | None -> noname
        | Some section -> Loader.Section.name section
      in
      let pp_section ppf name =
        if name == noname then Format.pp_print_string ppf "unamed section"
        else Format.fprintf ppf "section %s" name
      in
      let last_section = ref "--" in
      I.iter (fun addr byte ->
          let name = section_name addr in
          if name <> !last_section then (
            Format.fprintf ppf "; %a@ " pp_section name;
            last_section := name);
          pp_bv ppf addr addr_space;
          Format.fprintf ppf " : %02x %a@ " (Char.code byte) maybe_pp_char byte)
      @@ BiTbl.fold I.add memory I.empty)

  let pp ppf vars addr_space (values, memory) =
    if S.is_empty vars && BiTbl.length memory = 0 then
      Format.fprintf ppf "@[<h>--- Empty model ---@]"
    else (
      Format.fprintf ppf "@[<v 0>--- Model ---@ ";
      pp_variables ppf vars values;
      Format.pp_print_space ppf ();
      pp_memory ppf memory addr_space;
      Format.pp_close_box ppf ())

  let rec eval ((vars, _) as m) = function
    | Expr.Cst bv -> bv
    | e -> (
        try BvTbl.find vars e
        with Not_found ->
          let size = Expr.sizeof e in
          let value =
            match e with
            | Expr.Cst _ -> assert false
            | Expr.Var _ -> Bitvector.create (Z.of_int (Expr.hash e)) size
            | Expr.Load { addr; len; dir; label; _ } ->
                eval_load m (eval m addr) len dir label
            | Expr.Unary { f; x; _ } -> Term.Bv.unary f (eval m x)
            | Expr.Binary { f; x; y; _ } ->
                Term.Bv.binary f (eval m x) (eval m y)
            | Expr.Ite { c; t; e; _ } ->
                if Bv.zero = eval m c then eval m e else eval m t
          in
          BvTbl.add vars e value;
          value)

  and eval_load =
    let concat dir buf =
      let size = Bytes.length buf in
      let size' = size - 1 in
      if dir = Term.BigEndian then
        for i = 0 to (size / 2) - 1 do
          let j = size' - i in
          let x = Bytes.get buf i and y = Bytes.get buf j in
          Bytes.set buf i y;
          Bytes.set buf j x
        done;
      Bitvector.create
        (Z.of_bits (Bytes.unsafe_to_string buf))
        (byte_size * size)
    in
    let fill memory ptr map buf =
      let map = ref map in
      while !map <> Z.zero do
        let x = Z.trailing_zeros !map in
        let ptr = Bitvector.value_of (Bitvector.add_int ptr x) in
        let byte =
          match BiTbl.find memory ptr with
          | exception Not_found ->
              let byte =
                Char.unsafe_chr (Z.to_int (Z.logand ptr (Z.of_int 0xff)))
              in
              BiTbl.add memory ptr byte;
              byte
          | c -> c
        in
        Bytes.set buf x byte;
        map := Z.(!map lxor (one lsl x))
      done
    in
    let rec lookup ((_, memory) as m) ptr map buf = function
      | Memory.Unknown -> fill memory ptr map buf
      | Memory.Source { addr; len; orig; over; _ } ->
          let offset = Bv.sub ptr addr in
          let map = ref map and map' = ref Z.zero in
          while !map <> Z.zero do
            let x = Z.trailing_zeros !map in
            (try
               let y = Z.to_int (Bv.value_of (Bv.add_int offset x)) in
               if y < len then
                 if y < Bigarray.Array1.dim orig then
                   Bytes.set buf x
                     (Char.unsafe_chr (Bigarray.Array1.unsafe_get orig y))
                 else Bytes.set buf x '\x00'
               else map' := Z.(!map' lor (one lsl x))
             with Z.Overflow -> map' := Z.(!map' lor (one lsl x)));
            map := Z.(!map lxor (one lsl x))
          done;
          if !map' <> Z.zero then lookup m ptr !map' buf over
      | Layer { addr; bytes; over; _ } ->
          let addr = eval m addr in
          let offset = Bv.signed_of (Bv.sub ptr addr) in
          let map = ref map and map' = ref Z.zero in
          while !map <> Z.zero do
            let x = Z.trailing_zeros !map in
            let y = Z.add offset (Z.of_int x) in
            (match BiMap.find y bytes with
            | byte -> Bytes.set buf x (Bitvector.to_char (eval m byte))
            | exception Not_found -> map' := Z.(!map' lor (one lsl x)));
            map := Z.(!map lxor (one lsl x))
          done;
          if !map' <> Z.zero then lookup m ptr !map' buf over
    in
    fun m ptr len dir memory ->
      let buf = Bytes.make len '\x00' (* no value *) in
      lookup m ptr (Z.pred (Z.shift_left Z.one len)) buf memory;
      concat dir buf
end

module State (F : Solver_sig.FACTORY) (QS : Sse_types.QUERY_STATISTICS) = struct
  type t = {
    constraints : Expr.t list;
    (* reversed sequence of assertions *)
    constset : BvSet.t;
    vsymbols : Expr.t S.t;
    (* collection of visible symbols *)
    vmemory : Memory.t;
    (* visible memory *)
    fid : Sname.t;
    (* unique indice counter *)
    fvariables : Expr.t list S.t;
    (* collection of free variables *)
    ilocs : (Z.t * Loader_buf.t) BiItM.t;
    (* set of initialized memory locations *)
    model : Model.t; (* a model that satisfy constraints *)
  }

  let pp ppf state =
    Model.pp ppf state.fvariables
      (Kernel_options.Machine.word_size ())
      state.model

  let empty () =
    {
      constraints = [];
      constset = BvSet.empty;
      vsymbols = S.empty;
      vmemory = Memory.Unknown;
      fid = Sname.(incr zero);
      (* zero is reserved for initial memory *)
      fvariables = S.empty;
      ilocs = BiItM.empty;
      model = Model.empty ();
    }

  let fresh name size state =
    let v = Expr.var (Sname.to_string state.fid) size name in
    let fid = Sname.incr state.fid in
    let h =
      match S.find name state.fvariables with
      | exception Not_found -> [ v ]
      | h -> v :: h
    in
    let fvariables = S.add name h state.fvariables in
    let vsymbols = S.add name v state.vsymbols in
    { state with vsymbols; fid; fvariables }

  let assign name value state =
    { state with vsymbols = S.add name value state.vsymbols }

  let write ~addr value dir state =
    { state with vmemory = Memory.write ~addr value dir state.vmemory }

  let rec lookup name size state =
    match S.find name state.vsymbols with
    | exception Not_found -> lookup name size (fresh name size state)
    | bv -> (bv, state)

  let read ~addr bytes dir state = Memory.read ~addr bytes dir state.vmemory

  let memcpy ~addr len orig state =
    let base = Bv.value_of addr in
    let ilocs = BiItM.add ~base len (Bv.value_of addr, orig) state.ilocs in
    let vmemory = Memory.source ~addr ~len orig state.vmemory in
    { state with ilocs; vmemory }

  module Engine (Solver : Solver_sig.S) = struct
    type result = Unsat | Sat of t

    let extract_memory state =
      match Solver.get_memory () with
      | exception Not_found -> (BiTbl.create 0, state.constraints)
      | array, history ->
          let dirty = BiTbl.create 32 and memory = BiTbl.create 32 in
          let addr_space = Kernel_options.Machine.word_size () in
          let constraints =
            Queue.fold
              (fun constraints (access : Solver.access) ->
                match access with
                | Select (index, len) ->
                    let z = Solver.get_value index in
                    let rec fold z index len memory constraints =
                      if len = 0 then constraints
                      else if BiTbl.mem dirty z then
                        fold
                          Z.(z + one)
                          (Solver.succ index) (len - 1) memory constraints
                      else
                        let k = Solver.get_at array index in
                        let v = Z.to_int k in
                        let constraints =
                          match BiItM.find z state.ilocs with
                          | exception Not_found ->
                              BiTbl.add memory z (Char.unsafe_chr v);
                              constraints
                          | base, img ->
                              let y = Z.to_int (Z.sub z base) in
                              let v' =
                                if y < Bigarray.Array1.dim img then
                                  Bigarray.Array1.get img y
                                else 0
                              in
                              if v <> v' then
                                Expr.(
                                  equal
                                    (load 1 LittleEndian
                                       (constant (Bv.create z addr_space))
                                       Memory.Unknown)
                                    (constant (Bv.of_int ~size:byte_size v')))
                                :: constraints
                              else constraints
                        in
                        fold
                          Z.(z + one)
                          (Solver.succ index) (len - 1) memory constraints
                    in
                    fold z index len memory constraints
                | Store index ->
                    let z = Solver.get_value index in
                    BiTbl.replace dirty z ();
                    constraints)
              state.constraints history
          in
          (memory, constraints)

    let extract_vars state =
      let vars = BvTbl.create 32 in
      S.iter
        (fun _ ->
          List.iter (fun bv ->
              match Solver.get bv with
              | exception Not_found -> ()
              | x ->
                  BvTbl.add vars bv
                    (Bitvector.create (Solver.get_value x) (Expr.sizeof bv))))
        state.fvariables;
      vars

    let rec force_lazy_init constraints state =
      if constraints == state.constraints = false then
        match constraints with
        | [] -> ()
        | eq :: constraints ->
            let addr, value =
              match eq with
              | Binary
                  { f = Eq; x = Load { addr = Cst addr; _ }; y = Cst value; _ }
                ->
                  (Bitvector.value_of addr, Bitvector.value_of value)
              | _ -> assert false
            in
            Solver.set_memory ~addr value;
            force_lazy_init constraints state

    let enumerate =
      let rec iter state e expr size n enum =
        if n = 0 then enum
        else
          match Solver.check_sat () with
          | Unknown ->
              QS.Solver.incr_err ();
              raise Unknown
          | Unsat ->
              QS.Solver.incr_unsat ();
              enum
          | Sat ->
              QS.Solver.incr_sat ();
              let memory, constraints = extract_memory state in
              if constraints == state.constraints = false then (
                force_lazy_init constraints state;
                iter { state with constraints } e expr size n enum)
              else
                let x = Solver.get_value expr in
                let b = Bv.create x size in
                let cond = Expr.equal e (Expr.constant b) in
                let state' =
                  {
                    state with
                    constraints = cond :: constraints;
                    constset = BvSet.add cond state.constset;
                    model = (extract_vars state, memory);
                  }
                in
                Solver.neq expr x;
                iter state e expr size (n - 1) ((b, state') :: enum)
      in
      fun e ?(n = (1 lsl Expr.sizeof e) - 1) ?(except = []) state ->
        let size = Expr.sizeof e in
        let expr = Solver.bind state.fid e state.constraints in
        let init =
          let bv = Model.eval state.model e in
          if List.mem bv except then []
          else (
            QS.Preprocess.incr_const ();
            Solver.neq expr (Bitvector.value_of bv);
            let cond = Expr.equal e (Expr.constant bv) in
            [
              ( bv,
                {
                  state with
                  constraints = cond :: state.constraints;
                  constset = BvSet.add cond state.constset;
                } );
            ])
        in
        List.iter (fun bv -> Solver.neq expr (Bitvector.value_of bv)) except;
        iter state e expr size (n - 1) init

    let check_sat =
      let rec check_sat_true state =
        match Solver.check_sat () with
        | Unknown -> raise Unknown
        | Unsat -> Unsat
        | Sat ->
            let memory, constraints = extract_memory state in
            if constraints == state.constraints = false then (
              force_lazy_init constraints state;
              check_sat_true { state with constraints })
            else Sat { state with model = (extract_vars state, memory) }
      in
      fun state ->
        Solver.put state.fid state.constraints;
        check_sat_true state

    let close () = Solver.close ()
  end

  let assume cond state =
    if Expr.is_equal cond Expr.one then (
      QS.Preprocess.incr_sat ();
      Some state)
    else if Expr.is_equal cond Expr.zero then (
      QS.Preprocess.incr_unsat ();
      None)
    else if BvSet.mem cond state.constset then (
      QS.Preprocess.incr_sat ();
      Some state)
    else if BvSet.mem (Expr.lognot cond) state.constset then (
      QS.Preprocess.incr_unsat ();
      None)
    else
      let state =
        {
          state with
          constraints = cond :: state.constraints;
          constset = BvSet.add cond state.constset;
        }
      in
      if Bitvector.zero = Model.eval state.model cond then (
        QS.Solver.start_timer ();
        let open Engine (F ()) in
        let r =
          match check_sat state with
          | exception Unknown ->
              QS.Solver.incr_err ();
              raise Unknown
          | Unsat ->
              QS.Solver.incr_unsat ();
              None
          | Sat state ->
              QS.Solver.incr_sat ();
              Some state
        in
        close ();
        QS.Solver.stop_timer ();
        r)
      else (
        QS.Preprocess.incr_sat ();
        Some state)

  let test cond state =
    if Expr.is_equal cond Expr.one then (
      QS.Preprocess.incr_sat ();
      True state)
    else if Expr.is_equal cond Expr.zero then (
      QS.Preprocess.incr_unsat ();
      False state)
    else if BvSet.mem cond state.constset then (
      QS.Preprocess.incr_sat ();
      True state)
    else if BvSet.mem (Expr.lognot cond) state.constset then (
      QS.Preprocess.incr_unsat ();
      False state)
    else
      let t =
        {
          state with
          constraints = cond :: state.constraints;
          constset = BvSet.add cond state.constset;
        }
      in
      let ncond = Expr.lognot cond in
      let f =
        {
          state with
          constraints = ncond :: state.constraints;
          constset = BvSet.add ncond state.constset;
        }
      in
      let e = Model.eval state.model cond in
      let s = if Bv.is_zero e then t else f in
      QS.Solver.start_timer ();
      let open Engine (F ()) in
      let r =
        match check_sat s with
        | exception Unknown ->
            QS.Solver.incr_err ();
            raise Unknown
        | Unsat ->
            QS.Solver.incr_unsat ();
            if Bv.is_zero e then False f else True t
        | Sat state ->
            QS.Solver.incr_sat ();
            if Bv.is_zero e then Both { t = state; f }
            else Both { t; f = state }
      in
      close ();
      QS.Solver.stop_timer ();
      r

  let enumerate =
    let with_solver e ?n ?except state =
      Sse_options.Logger.debug ~level:5 "Call solver";
      QS.Solver.start_timer ();
      let open Engine (F ()) in
      let r = enumerate e ?n ?except state in
      close ();
      QS.Solver.stop_timer ();
      r
    in
    fun e ?n ?(except = []) state ->
      match (e, n) with
      | Expr.Cst bv, _ when List.mem bv except = false ->
          QS.Preprocess.incr_const ();
          [ (bv, state) ]
      | Expr.Cst _, _ ->
          QS.Preprocess.incr_const ();
          []
      | _, Some 1 ->
          let bv = Model.eval state.model e in
          if List.mem bv except then with_solver e ?n ~except state
          else (
            QS.Preprocess.incr_const ();
            let cond = Expr.equal e (Expr.constant bv) in
            [
              ( bv,
                {
                  state with
                  constraints = cond :: state.constraints;
                  constset = BvSet.add cond state.constset;
                } );
            ])
      | _, _ -> with_solver e ?n ~except state

  module Translate = struct
    let unary e = function
      | Dba.Unary_op.Not -> Term.Not
      | Dba.Unary_op.UMinus -> Term.Minus
      | Dba.Unary_op.Sext n -> Term.Sext (n - Dba.Expr.size_of e)
      | Dba.Unary_op.Uext n -> Term.Uext (n - Dba.Expr.size_of e)
      | Dba.Unary_op.Restrict interval -> Term.Restrict interval

    let binary op =
      let open Dba.Binary_op in
      match op with
      | Plus -> Term.Plus
      | Minus -> Term.Minus
      | Mult -> Term.Mul
      | DivU -> Term.Udiv
      | DivS -> Term.Sdiv
      | ModU -> Term.Umod
      | ModS -> Term.Smod
      | Eq -> Term.Eq
      | Diff -> Term.Diff
      | LeqU -> Term.Ule
      | LtU -> Term.Ult
      | GeqU -> Term.Uge
      | GtU -> Term.Ugt
      | LeqS -> Term.Sle
      | LtS -> Term.Slt
      | GeqS -> Term.Sge
      | GtS -> Term.Sgt
      | Xor -> Term.Xor
      | And -> Term.And
      | Or -> Term.Or
      | Concat -> Term.Concat
      | LShift -> Term.Lsl
      | RShiftU -> Term.Lsr
      | RShiftS -> Term.Asr
      | LeftRotate -> Term.Rol
      | RightRotate -> Term.Ror

    let rec expr symbolic_state e =
      match e with
      | Dba.Expr.Var { info = Symbol (_, (lazy bv)); _ } | Dba.Expr.Cst bv ->
          (Expr.constant bv, symbolic_state)
      | Dba.Expr.Var { name; size; _ } -> lookup name size symbolic_state
      | Dba.Expr.Load (bytes, endianness, e) ->
          let addr, symbolic_state = expr symbolic_state e in
          (read ~addr bytes endianness symbolic_state, symbolic_state)
      | Dba.Expr.Binary (bop, lop, rop) ->
          let lop, symbolic_state = expr symbolic_state lop in
          let rop, symbolic_state = expr symbolic_state rop in
          (Expr.binary (binary bop) lop rop, symbolic_state)
      | Dba.Expr.Unary (uop, e) ->
          let v, symbolic_state = expr symbolic_state e in
          (Expr.unary (unary e uop) v, symbolic_state)
      | Dba.Expr.Ite (c, then_e, else_e) -> (
          let cond, symbolic_state = expr symbolic_state c in
          match cond with
          | Expr.Cst bv when Bv.is_zero bv -> expr symbolic_state else_e
          | Expr.Cst _ -> expr symbolic_state then_e
          | _ ->
              let then_smt, symbolic_state = expr symbolic_state then_e in
              let else_smt, symbolic_state = expr symbolic_state else_e in
              (Expr.ite cond then_smt else_smt, symbolic_state))
  end

  let assume e t =
    let e, t = Translate.expr t e in
    assume e t

  let test e t =
    let e, t = Translate.expr t e in
    test e t

  let split_on e ?n ?except t =
    let e, t = Translate.expr t e in
    enumerate e ?n ?except t

  let assign name e t =
    let e, t = Translate.expr t e in
    assign name e t

  let write ~addr value dir t =
    let addr, t = Translate.expr t addr in
    let value, t = Translate.expr t value in
    write ~addr value dir t

  let pp_smt ?slice ppf t =
    let module P = Smt2_solver.Printer in
    let ctx =
      P.create ~debug:(fun ~name ~label -> label ^ name) ~next_id:t.fid ()
    in
    (* visit assertions *)
    List.iter (P.visit_bl ctx) t.constraints;
    (* visit terms *)
    let defs =
      match slice with
      | Some defs ->
          List.map
            (fun (expr, name) ->
              let expr, _ = Translate.expr t expr in
              P.visit_bv ctx expr;
              (expr, name))
            defs
      | None ->
          P.visit_ax ctx t.vmemory;
          List.rev
            (S.fold
               (fun name expr defs ->
                 P.visit_bv ctx expr;
                 (expr, name) :: defs)
               t.vsymbols [])
    in
    Format.pp_open_vbox ppf 0;
    (* print declarations *)
    P.pp_print_decls ppf ctx;
    (* print definitions *)
    P.pp_print_defs ppf ctx;
    List.iter
      (fun (bv, name) ->
        Format.fprintf ppf "@[<h>(define-fun %s () (_ BitVec %d)@ " name
          (Expr.sizeof bv);
        P.pp_print_bv ctx ppf bv;
        Format.fprintf ppf ")@]@ ")
      defs;
    if slice == None then
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

  let as_ascii name t =
    let buf = Buffer.create 16 in
    List.iter (fun var ->
        assert (Expr.sizeof var mod byte_size = 0);
        let rec iter bv =
          let size = Bitvector.size_of bv in
          if size = byte_size then Buffer.add_char buf (Bitvector.to_char bv)
          else
            let byte = Bitvector.extract bv { Interval.lo = 0; hi = 7 } in
            Buffer.add_char buf (Bitvector.to_char byte);
            iter (Bitvector.extract bv { Interval.lo = 8; hi = size - 1 })
        in
        iter (Model.eval t.model var))
    @@ List.rev @@ S.find name t.fvariables;
    Buffer.contents buf
end
