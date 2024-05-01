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

open Sexpr

exception Unknown = Types.Unknown

module type S = sig
  val visit_formula : Expr.t list -> unit
  val iter_free_variables : (string -> Expr.t -> unit) -> unit
  val iter_free_arrays : (string -> Memory.t -> unit) -> unit
  val assert_formula : Expr.t -> unit
  val check_sat : ?timeout:float -> unit -> Libsolver.status
  val check_sat_assuming : ?timeout:float -> Expr.t -> Libsolver.status
  val get_value : Expr.t -> Z.t
  val fold_array_values : (Z.t -> Z.t -> 'a -> 'a) -> Memory.t -> 'a -> 'a
  val push : unit -> unit
  val pop : unit -> unit
  val close : unit -> unit
end

module type OPEN = functor () -> S

type lazy_memory = {
  addr_space : int;
  content : (Z.t * Loader_buf.t) Imap.t;
  mutable lemmas : Expr.t list;
}

type result = Sat of Model.t | Unsat | Unknown

let deadline_of_timeout =
  Option.map (fun timeout -> Unix.gettimeofday () +. timeout)

let timeout_of_deadline =
  Option.map (fun deadline -> deadline -. Unix.gettimeofday ())

let extract_memory solver lmem =
  let module Solver = (val solver : S) in
  let memory = BiTbl.create 64 in
  let alocs =
    Solver.fold_array_values
      (fun addr value alocs ->
        match Imap.find addr lmem.content with
        | exception Not_found ->
            BiTbl.add memory addr (Char.unsafe_chr (Z.to_int value));
            alocs
        | base, img ->
            let offset = Z.to_int (Z.sub addr base) in
            let value' =
              Z.of_int
                (if offset < Bigarray.Array1.dim img then
                   Bigarray.Array1.get img offset
                 else 0)
            in
            if value <> value' then
              Expr.equal
                (Expr.load 1 LittleEndian
                   (Expr.constant (Bv.create addr lmem.addr_space))
                   Memory.root)
                (Expr.constant (Bv.create value' 8))
              :: alocs
            else alocs)
      Memory.root lmem.lemmas
  in
  (memory, alocs)

let extract_array solver name =
  let module Solver = (val solver : S) in
  let array = BiTbl.create 64 in
  Solver.fold_array_values
    (fun addr value array ->
      BiTbl.add array addr (Char.unsafe_chr (Z.to_int value));
      array)
    name array

let extract_arrays solver =
  let module Solver = (val solver : S) in
  let arrays = StTbl.create 5 in
  Solver.iter_free_arrays (fun name symbol ->
      StTbl.add arrays name (extract_array solver symbol));
  arrays

let extract_vars solver =
  let module Solver = (val solver : S) in
  let vars = StTbl.create 8 and values = BvTbl.create 32 in
  Solver.iter_free_variables (fun name bv ->
      StTbl.add vars name bv;
      BvTbl.add values bv
        (Bitvector.create (Solver.get_value bv) (Expr.sizeof bv)));
  (vars, values)

let rec assert_lazy_lemmas solver lmem lemmas =
  let module Solver = (val solver : S) in
  if lemmas != lmem.lemmas then
    match lemmas with
    | [] -> ()
    | e :: lemmas ->
        Solver.assert_formula e;
        assert_lazy_lemmas solver lmem lemmas

let assert_lazy_lemmas solver lmem lemmas =
  assert_lazy_lemmas solver lmem lemmas;
  lmem.lemmas <- lemmas

module Common (QS : Types.QUERY_STATISTICS) = struct
  let rec check_sat solver deadline lmem =
    let module Solver = (val solver : S) in
    match Solver.check_sat ?timeout:(timeout_of_deadline deadline) () with
    | Unknown ->
        QS.Solver.incr_err ();
        Unknown
    | Unsat ->
        QS.Solver.incr_unsat ();
        Unsat
    | Sat ->
        QS.Solver.incr_sat ();
        let memory, lemmas = extract_memory solver lmem in
        if lemmas != lmem.lemmas then (
          assert_lazy_lemmas solver lmem lemmas;
          check_sat solver deadline lmem)
        else
          let vars, values = extract_vars solver in
          Sat (vars, values, memory, extract_arrays solver, lmem.addr_space)

  let rec check_sat_assuming solver deadline lmem e =
    let module Solver = (val solver : S) in
    match
      Solver.check_sat_assuming ?timeout:(timeout_of_deadline deadline) e
    with
    | Unknown ->
        QS.Solver.incr_err ();
        Unknown
    | Unsat ->
        QS.Solver.incr_unsat ();
        Unsat
    | Sat ->
        QS.Solver.incr_sat ();
        let memory, lemmas = extract_memory solver lmem in
        if lemmas != lmem.lemmas then (
          assert_lazy_lemmas solver lmem lemmas;
          check_sat_assuming solver deadline lmem e)
        else
          let vars, values = extract_vars solver in
          Sat (vars, values, memory, extract_arrays solver, lmem.addr_space)

  let rec fold_values solver deadline lmem e size n f acc =
    if n = 0 then acc
    else
      let module Solver = (val solver : S) in
      match Solver.check_sat ?timeout:(timeout_of_deadline deadline) () with
      | Unknown ->
          QS.Solver.incr_err ();
          raise Unknown
      | Unsat ->
          QS.Solver.incr_unsat ();
          acc
      | Sat ->
          QS.Solver.incr_sat ();
          let memory, lemmas = extract_memory solver lmem in
          if lemmas != lmem.lemmas then (
            assert_lazy_lemmas solver lmem lemmas;
            fold_values solver deadline lmem e size n f acc)
          else
            let x = Solver.get_value e in
            let bv = Bv.create x size in
            let vars, values = extract_vars solver in
            let model =
              (vars, values, memory, extract_arrays solver, lmem.addr_space)
            in
            Solver.assert_formula (Expr.diff e (Expr.constant bv));
            fold_values solver deadline lmem e size (n - 1) f (f bv model acc)
end

module type GET_MODEL = sig
  val check_sat : ?timeout:float -> lazy_memory -> Expr.t list -> result

  val fold_values :
    ?timeout:float ->
    lazy_memory ->
    Expr.t list ->
    Expr.t ->
    n:int ->
    except:Bv.t list ->
    (Bv.t -> Model.t -> 'a -> 'a) ->
    'a ->
    'a
end

module type GET_MODEL_WITH_STATS = functor (QS : Types.QUERY_STATISTICS) ->
  GET_MODEL

module Once (Session : OPEN) (QS : Types.QUERY_STATISTICS) : GET_MODEL = struct
  include Common (QS)

  let check_sat ?timeout lmem formula =
    let module Solver = Session () in
    Solver.visit_formula formula;
    List.iter Solver.assert_formula formula;
    List.iter Solver.assert_formula lmem.lemmas;
    let r = check_sat (module Solver : S) (deadline_of_timeout timeout) lmem in
    Solver.close ();
    r

  let fold_values ?timeout lmem formula e ~n ~except f acc =
    let module Solver = Session () in
    Solver.visit_formula formula;
    List.iter Solver.assert_formula formula;
    List.iter Solver.assert_formula lmem.lemmas;
    List.iter
      (fun bv -> Solver.assert_formula (Expr.diff e (Expr.constant bv)))
      except;
    match
      fold_values
        (module Solver : S)
        (deadline_of_timeout timeout)
        lmem e (Expr.sizeof e) n f acc
    with
    | exception Unknown ->
        Solver.close ();
        raise Unknown
    | acc ->
        Solver.close ();
        acc
end

module MultiChecks (Session : OPEN) (QS : Types.QUERY_STATISTICS) : GET_MODEL =
struct
  include Common (QS)

  let close solver =
    let module Solver = (val solver : S) in
    Solver.close ()

  type cache = {
    mutable assertions : Expr.t list;
    mutable lemmas : Expr.t list;
    mutable solver : (module S) option;
  }

  let cache = { assertions = []; lemmas = []; solver = None }

  let reset () =
    cache.assertions <- [];
    cache.lemmas <- [];
    Option.iter close cache.solver;
    cache.solver <- None

  let assert_formula solver formula =
    let module Solver = (val solver : S) in
    Solver.visit_formula formula;
    List.iter Solver.assert_formula formula

  let open_solver lemmas formula =
    Option.iter close cache.solver;
    let module Solver = Session () in
    let solver = (module Solver : S) in
    cache.solver <- Some solver;
    assert_formula solver formula;
    List.iter Solver.assert_formula lemmas;
    solver

  let rec update_lemmas solver lemmas =
    if lemmas = cache.lemmas then ()
    else
      match lemmas with
      | [] -> assert false
      | e :: lemmas ->
          let module Solver = (val solver : S) in
          Solver.assert_formula e;
          update_lemmas solver lemmas

  let rec search_prefix solver lemmas formula to_assert =
    if formula == cache.assertions then (
      assert_formula solver to_assert;
      update_lemmas solver lemmas;
      solver)
    else
      match formula with
      | [] -> open_solver lemmas to_assert
      | e :: formula -> search_prefix solver lemmas formula (e :: to_assert)

  let set_context lemmas formula =
    let solver =
      match cache.solver with
      | None -> open_solver lemmas formula
      | Some solver -> search_prefix solver lemmas formula []
    in
    cache.assertions <- formula;
    cache.lemmas <- lemmas;
    solver

  let check_sat ?timeout (lmem : lazy_memory) formula =
    match formula with
    | [] -> Sat (Model.empty lmem.addr_space)
    | e :: formula ->
        let solver = set_context lmem.lemmas formula in
        let module Solver = (val solver) in
        check_sat_assuming
          (module Solver : S)
          (deadline_of_timeout timeout)
          lmem e

  let fold_values ?timeout (lmem : lazy_memory) formula e ~n ~except f acc =
    let solver = set_context lmem.lemmas formula in
    let module Solver = (val solver) in
    Solver.push ();
    List.iter
      (fun bv -> Solver.assert_formula (Expr.diff e (Expr.constant bv)))
      except;
    match
      fold_values
        (module Solver : S)
        (deadline_of_timeout timeout)
        lmem e (Expr.sizeof e) n f acc
    with
    | exception Unknown ->
        reset ();
        raise Unknown
    | acc ->
        Solver.pop ();
        acc
end
