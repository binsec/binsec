(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

module BiTbl = Basic_types.Integers.Bigint.Htbl
open Types
include Session

exception Unknown = State.Unknown

type lazy_memory = {
  contents : Loader_types.buffer option Zmap.t AsMap.t;
  mutable lemmas : Expr.t list;
}

type result = Sat of Model.t | Unsat | Unknown

let deadline_of_timeout =
  Option.map (fun timeout -> Unix.gettimeofday () +. timeout)

let timeout_of_deadline =
  Option.map (fun deadline -> deadline -. Unix.gettimeofday ())

let extract_lazy_array :
    (module S) ->
    Loader_types.buffer option Zmap.t ->
    Memory.symbol ->
    Expr.t list ->
    char BiTbl.t * Expr.t list =
 fun solver contents (Symbol { index; _ } as symbol) lemmas ->
  let module Solver = (val solver : S) in
  let memory = BiTbl.create 64 in
  let lemmas =
    Solver.fold_array_values
      (fun addr value lemmas ->
        match Zmap.find addr contents with
        | exception Not_found ->
            BiTbl.add memory addr (Char.unsafe_chr (Z.to_int value));
            lemmas
        | Item { lo = base; elt = content; _ } ->
            BiTbl.add memory addr (Char.unsafe_chr (Z.to_int value));
            let offset = Z.to_int (Z.sub addr base) in
            let value' =
              match content with
              | None -> Z.zero
              | Some data ->
                  Z.of_int
                    (if offset < Bigarray.Array1.dim data then
                       Bigarray.Array1.get data offset
                     else 0)
            in
            if value <> value' then
              Expr.equal
                (Expr.load 1 LittleEndian
                   (Expr.constant (Bv.create addr index))
                   symbol)
                (Expr.constant (Bv.create value' 8))
              :: lemmas
            else lemmas)
      symbol lemmas
  in
  (memory, lemmas)

let extract_array : (module S) -> Memory.symbol -> char BiTbl.t =
 fun solver (Symbol _ as symbol) ->
  let module Solver = (val solver : S) in
  let array = BiTbl.create 64 in
  Solver.fold_array_values
    (fun addr value array ->
      BiTbl.add array addr (Char.unsafe_chr (Z.to_int value));
      array)
    symbol array

let extract_arrays :
    (module S) -> lazy_memory -> (char BiTbl.t AsTbl.t, Expr.t list) Result.t =
 fun solver lmem ->
  let module Solver = (val solver : S) in
  let arrays = AsTbl.create 5 and lemmas = ref lmem.lemmas in
  Solver.iter_free_arrays (fun _ symbol ->
      match AsMap.find symbol lmem.contents with
      | exception Not_found ->
          if !lemmas == lmem.lemmas then
            AsTbl.add arrays symbol (extract_array solver symbol)
      | contents ->
          let bytes, new_lemmas =
            extract_lazy_array solver contents symbol !lemmas
          in
          lemmas := new_lemmas;
          AsTbl.add arrays symbol bytes);
  if !lemmas == lmem.lemmas then Ok arrays else Error !lemmas

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

let rec check_sat solver ?deadline lmem =
  let module Solver = (val solver : S) in
  match Solver.check_sat ?timeout:(timeout_of_deadline deadline) () with
  | Unknown -> Unknown
  | Unsat -> Unsat
  | Sat -> (
      match extract_arrays solver lmem with
      | Error lemmas ->
          assert_lazy_lemmas solver lmem lemmas;
          check_sat solver ?deadline lmem
      | Ok arrays ->
          let symbols, values = extract_vars solver in
          Sat { symbols; values; arrays })

let rec check_sat_assuming solver ?deadline lmem e =
  let module Solver = (val solver : S) in
  match Solver.check_sat_assuming ?timeout:(timeout_of_deadline deadline) e with
  | Unknown -> Unknown
  | Unsat -> Unsat
  | Sat -> (
      match extract_arrays solver lmem with
      | Error lemmas ->
          assert_lazy_lemmas solver lmem lemmas;
          check_sat_assuming solver ?deadline lmem e
      | Ok arrays ->
          let symbols, values = extract_vars solver in
          Sat { symbols; values; arrays })

let open_session :
    ?carbon_copy:(unit -> string) -> Smtlib.Solver.backend -> unit -> (module S)
    =
 fun ?carbon_copy backend ->
  match (carbon_copy, backend) with
  | None, None -> fun () -> (module Dummy)
  | None, Text { session; arg } -> fun () -> Text.make session arg
  | None, Binding { factory; complete_fold_ax_values = true } ->
      let module Factory = Binding.Make ((val factory)) in
      fun () -> (module Factory.Open ())
  | None, Binding { factory; complete_fold_ax_values = false } ->
      let module Factory = Binding.SafeArray ((val factory)) in
      fun () -> (module Factory.Open ())
  | Some filename_gen, None ->
      fun () -> Text.make (module Smtlib.Solver.Session.Dump) (filename_gen ())
  | Some filename_gen, Text { session; arg } ->
      let module Carbon_copy = Smtlib.Solver.Session.Carbon_copy ((val session)) in
      fun () -> Text.make (module Carbon_copy) (arg, filename_gen ())
  | Some filename_gen, Binding { factory; complete_fold_ax_values = true } ->
      let module Factory = Binding.Make ((val factory)) in
      fun () -> Text.make_carbon_copy (module Factory.Open ()) (filename_gen ())
  | Some filename_gen, Binding { factory; complete_fold_ax_values = false } ->
      let module Factory = Binding.SafeArray ((val factory)) in
      fun () -> Text.make_carbon_copy (module Factory.Open ()) (filename_gen ())

type cache = {
  mutable assertions : Expr.t list;
  mutable lemmas : Expr.t list;
  mutable solver : (module S) option;
}

let close solver =
  let module Solver = (val solver : S) in
  Solver.close ()

let empty_cache () =
  let cache = { assertions = []; lemmas = []; solver = None } in
  Gc.finalise (fun cache -> Option.iter close cache.solver) cache;
  cache

let clear_cache cache =
  cache.assertions <- [];
  cache.lemmas <- [];
  Option.iter close cache.solver

type mode = One_shot | Multi_checks of cache

module Multi_checks = struct
  let assert_formula solver formula =
    let module Solver = (val solver : S) in
    Solver.visit_formula formula;
    List.iter Solver.assert_formula formula

  let open_solver cache open_session lemmas formula =
    Option.iter close cache.solver;
    let solver = open_session () in
    cache.solver <- Some solver;
    assert_formula solver formula;
    let module Solver = (val solver : S) in
    List.iter Solver.assert_formula lemmas;
    solver

  let rec update_lemmas cache solver lemmas =
    if lemmas = cache.lemmas then ()
    else
      match lemmas with
      | [] -> assert false
      | e :: lemmas ->
          let module Solver = (val solver : S) in
          Solver.assert_formula e;
          update_lemmas cache solver lemmas

  let rec search_prefix cache solver lemmas formula to_assert =
    if formula == cache.assertions then (
      assert_formula solver to_assert;
      update_lemmas cache solver lemmas;
      solver)
    else
      match formula with
      | [] -> raise_notrace Not_found
      | e :: formula ->
          search_prefix cache solver lemmas formula (e :: to_assert)

  let set_context cache open_session lemmas formula =
    let solver =
      try
        match cache.solver with
        | None -> raise_notrace Not_found
        | Some solver -> search_prefix cache solver lemmas formula []
      with Not_found -> open_solver cache open_session lemmas formula
    in
    cache.assertions <- formula;
    cache.lemmas <- lemmas;
    solver
end

type enumeration = {
  open_session : unit -> (module S);
  timeout : float option;
  memory : lazy_memory;
  target : Expr.t;
  assertions : Expr.t list;
  mutable except : Bv.t list;
  mutable solver : (module S) option;
  mutable completed : bool;
}

let release_enumeration : enumeration -> unit =
 fun state ->
  Option.iter close state.solver;
  state.solver <- None

let next_value : enumeration -> (Bv.t * Model.t) option =
 fun state ->
  if state.completed then None
  else
    let solver =
      match state.solver with
      | None ->
          let solver = state.open_session () in
          state.solver <- Some solver;
          let module Solver = (val solver) in
          Solver.visit_formula state.assertions;
          List.iter Solver.assert_formula state.assertions;
          List.iter Solver.assert_formula state.memory.lemmas;
          List.iter
            (fun bv -> Solver.assert_distinct state.target (Expr.constant bv))
            state.except;
          solver
      | Some solver -> solver
    in
    match
      check_sat solver
        ?deadline:(deadline_of_timeout state.timeout)
        state.memory
    with
    | Unknown ->
        release_enumeration state;
        raise Unknown
    | Unsat ->
        release_enumeration state;
        state.completed <- true;
        None
    | Sat model ->
        let module Solver = (val solver) in
        let x = Solver.get_value state.target in
        let bv = Bv.create x (Expr.sizeof state.target) in
        Solver.assert_distinct state.target (Expr.constant bv);
        state.except <- bv :: state.except;
        Some (bv, model)

let check_sat :
    (unit -> (module S)) ->
    mode ->
    ?timeout:float ->
    lazy_memory ->
    Expr.t list ->
    result =
 fun open_session mode ->
  match mode with
  | One_shot ->
      fun ?timeout lmem formula ->
        let module Solver = (val open_session ()) in
        Solver.visit_formula formula;
        List.iter Solver.assert_formula formula;
        List.iter Solver.assert_formula lmem.lemmas;
        let r =
          check_sat
            (module Solver : S)
            ?deadline:(deadline_of_timeout timeout)
            lmem
        in
        Solver.close ();
        r
  | Multi_checks cache -> (
      fun ?timeout lmem formula ->
        match formula with
        | [] -> Sat (Model.empty ())
        | e :: formula ->
            check_sat_assuming
              (Multi_checks.set_context cache open_session lmem.lemmas formula)
              ?deadline:(deadline_of_timeout timeout)
              lmem e)

let enumerate_values :
    (unit -> (module S)) ->
    ?timeout:float ->
    lazy_memory ->
    Expr.t list ->
    Expr.t ->
    except:Bv.t list ->
    enumeration =
 fun open_session ?timeout memory assertions target ~except ->
  let state =
    {
      open_session;
      timeout;
      memory;
      target;
      assertions;
      except;
      solver = None;
      completed = false;
    }
  in
  Gc.finalise (fun state -> Option.iter close state.solver) state;
  state
