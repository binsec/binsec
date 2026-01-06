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

exception Undefined = State.Undefined
exception Undeclared = State.Undeclared
exception Unknown = State.Unknown
exception Non_mergeable = State.Non_mergeable

(* utils *)

module NiTbl = Basic_types.Integers.Int.Htbl
open Types
module StrMap = Basic_types.String.Map
module IntMap = Basic_types.Integers.Int.Map
module IntTbl = Basic_types.Integers.Int.Htbl
module IntSet = Basic_types.Integers.Int.Set
module AxHmap = Hmap.Make (Memory)

module AsHmap = Hmap.Make (struct
  type t = Memory.symbol

  let compare : t -> t -> int =
   fun (Symbol _ as s0) (Symbol _ as s1) -> Memory.compare s0 s1

  let hash : t -> int = fun (Symbol _ as s) -> Memory.hash s
end)

module VarHmap = Hmap.Make (struct
  type t = ([ `Var ], string, Memory.t) Term.t

  let hash var = Expr.hash (Term.to_exp var)
  let compare var var' = Expr.compare (Term.to_exp var) (Term.to_exp var')
end)

module IntHmap = Hmap.Make (struct
  type t = int

  let hash = Fun.id
  let compare = ( - )
end)

type 'a State.value_kind += Term : Expr.t State.value_kind

type 'a predicate = {
  clauses : int;
  constraints : Expr.t list (* reversed sequence of assertions *);
  mutable reverse_dependencies : BvSet.t BvHmap.t;
  domain : (module Domains.S with type t = 'a);
  mutable values : 'a BvHmap.t;
  token : Solver.lazy_memory;
}

type 'a state = {
  visible_symbols : Expr.t IntMap.t;
  (* collection of visible symbols *)
  visible_arrays : Memory.t StrMap.t;
  (* collection of visible arrays *)
  visible_memory : [ `Any ] Memory.node; (* visible memory *)
  mutable anchors : IntSet.t;
  predicate : 'a predicate;
}

type t = Q : 'a state -> t [@@unboxed]

type ('value, 'state, 'cookie, 'a) State.feature +=
  | VisibleSymbols :
      ( 'value,
        'state,
        'cookie,
        'state -> 'value Dba_types.Var.Map.t )
      State.feature
  | VisibleMemory : ('value, 'state, 'cookie, 'state -> Memory.t) State.feature
  | VisibleArrays :
      ('value, 'state, 'cookie, 'state -> Memory.t StrMap.t) State.feature
  | Downcast : ('value, 'state, 'cookie, 'state -> t) State.feature
  | Upcast : ('value, 'state, 'cookie, t -> 'state) State.feature

module Uid : State.UID with type t = Suid.t = struct
  type t = Suid.t

  let zero = Suid.incr Suid.zero
  (* zero is reserved for initial memory *)

  let succ = Suid.incr
  let compare = Suid.compare
end

module Value : State.VALUE with type id = Uid.t and type t = Expr.t = struct
  type t = Expr.t
  type id = Uid.t

  let zero = Expr.zero
  let constant = Expr.constant
  let var id name size = Expr.var (name ^ Suid.to_string id) size name
  let unary = Expr.unary
  let binary = Expr.binary
  let ite = Expr.ite
  let is_symbolic : t -> bool = function Cst _ -> false | _ -> true

  let is_zero : t -> trilean = function
    | Cst bv -> if Bv.is_zeros bv then True else False
    | _ -> Unknown

  let sizeof : t -> int = Expr.sizeof
end

module Model : State.MODEL with type t = Model.t and type value := Value.t =
struct
  type t = Model.t

  let empty = Model.empty
  let eval e t = Model.eval t e
  let pp = Model.pp
  let pp_with_sections = Model.pp_with_sections
end

module Cookie : sig
  include State.COOKIE

  val set_backend : t -> Smtlib.Solver.backend -> unit
  val set_timeout : t -> float -> unit
  val set_dump_dir : t -> string -> unit
  val set_multi_checks : t -> bool -> unit
  val check_sat : t -> Solver.lazy_memory -> Expr.t list -> Solver.result

  val enumerate_values :
    t ->
    Solver.lazy_memory ->
    Expr.t list ->
    Expr.t ->
    except:Bv.t list ->
    Solver.enumeration
end = struct
  type t = {
    n : int ref;
    mutable dir : string option;
    mutable backend : Smtlib.Solver.backend option;
    mutable open_session : (unit -> (module Solver.S)) option;
    mutable mode : Solver.mode;
    mutable timeout : float option;
    mutable check_sat :
      ?timeout:float -> Solver.lazy_memory -> Expr.t list -> Solver.result;
    mutable enumerate_values :
      ?timeout:float ->
      Solver.lazy_memory ->
      Expr.t list ->
      Expr.t ->
      except:Bv.t list ->
      Solver.enumeration;
  }

  let incr cookie f () =
    incr cookie.n;
    f ()

  let resolve_session cookie =
    match cookie.open_session with
    | Some open_session -> open_session
    | None ->
        let carbon_copy =
          Option.map
            (fun dir () ->
              if not (Sys.file_exists dir) then Unix.mkdir dir 0o777;
              Filename.concat dir (Format.sprintf "query_%d.smt2" !(cookie.n)))
            cookie.dir
        in
        let backend =
          match cookie.backend with
          | Some backend -> backend
          | None ->
              let backend = Smtlib.Solver.default_backend () in
              cookie.backend <- Some backend;
              backend
        in
        let open_session =
          incr cookie (Solver.open_session ?carbon_copy backend)
        in
        cookie.open_session <- Some open_session;
        open_session

  let resolve_check_sat cookie ?timeout token formula =
    let check_sat = Solver.check_sat (resolve_session cookie) cookie.mode in
    cookie.check_sat <- check_sat;
    check_sat ?timeout token formula

  let resolve_enumerate cookie ?timeout token formula target ~except =
    let enumerate_values = Solver.enumerate_values (resolve_session cookie) in
    cookie.enumerate_values <- enumerate_values;
    enumerate_values ?timeout token formula target ~except

  let default () =
    let rec cookie =
      {
        n = ref 0;
        dir = None;
        backend = None;
        timeout = None;
        open_session = None;
        mode = One_shot;
        check_sat =
          (fun ?timeout token formula ->
            resolve_check_sat cookie ?timeout token formula);
        enumerate_values =
          (fun ?timeout token formula target ~except ->
            resolve_enumerate cookie ?timeout token formula target ~except);
      }
    in
    cookie

  let reset_open_session cookie =
    cookie.open_session <- None;
    (match cookie.mode with
    | One_shot -> ()
    | Multi_checks cache -> Solver.clear_cache cache);
    cookie.check_sat <- resolve_check_sat cookie;
    cookie.enumerate_values <- resolve_enumerate cookie

  let set_backend cookie backend =
    cookie.backend <- Some backend;
    reset_open_session cookie

  let set_dump_dir cookie dir =
    cookie.dir <- Some dir;
    reset_open_session cookie

  let set_timeout cookie timeout =
    cookie.timeout <- (if Float.is_infinite timeout then None else Some timeout)

  let set_multi_checks cookie enable =
    match (cookie.mode, enable) with
    | One_shot, false | Multi_checks _, true -> ()
    | Multi_checks cache, false ->
        Solver.clear_cache cache;
        cookie.mode <- One_shot;
        cookie.check_sat <- resolve_check_sat cookie
    | One_shot, true ->
        cookie.mode <- Multi_checks (Solver.empty_cache ());
        cookie.check_sat <- resolve_check_sat cookie

  let check_sat cookie token formula =
    cookie.check_sat ?timeout:cookie.timeout token formula

  let enumerate_values cookie token formula target ~except =
    cookie.enumerate_values ?timeout:cookie.timeout token formula target ~except
end

module Enumeration :
  State.ENUMERATION
    with type t = Solver.enumeration
     and type value := Bitvector.t * Model.t = struct
  type t = Solver.enumeration

  let next : t -> (Bitvector.t * Model.t) option = Solver.next_value
  let suspend : t -> unit = Solver.release_enumeration
end

module Ai : sig
  include Abstract_interpretation.CONTEXT with type 'a t = 'a predicate
  include Abstract_interpretation.S with type 'a t = 'a t
end = struct
  module Context :
    Abstract_interpretation.CONTEXT with type 'a t = 'a predicate = struct
    type 'a t = 'a predicate

    let domain { domain; _ } = domain

    let add_dependency state ~parent e =
      state.reverse_dependencies <-
        BvHmap.add e
          (BvSet.add parent
             (try BvHmap.find e state.reverse_dependencies
              with Not_found -> BvSet.empty))
          state.reverse_dependencies

    let find_dependency { reverse_dependencies; _ } e =
      BvHmap.find e reverse_dependencies

    let add_value state e v = state.values <- BvHmap.add e v state.values
    let find_value { values; _ } e = BvHmap.find e values
  end

  include Context
  include Abstract_interpretation.Make (Context)
end

module MMU = Memory_manager.Make (struct
  type 'a t = 'a state

  include Abstract_interpretation.Make (Ai)

  let domain { predicate; _ } = domain predicate

  let anchor state (m : Memory.t) =
    state.anchors <- IntSet.add (Memory.hash m) state.anchors

  let anchored { anchors; _ } (m : Memory.t) =
    IntSet.mem (Memory.hash m) anchors

  let eval { predicate; _ } e = eval predicate e
  let refine { predicate; _ } e d = refine predicate e d
end)

let lookup : Dba.Var.t -> t -> Expr.t =
 fun var (Q { visible_symbols; _ }) ->
  try IntMap.find var.id visible_symbols
  with Not_found -> raise (Undefined var)

let read : addr:Expr.t -> int -> Expr.endianness -> t -> Expr.t * t =
 fun ~addr bytes dir (Q state) ->
  match state.visible_memory with
  | None -> raise (Undeclared None)
  | (Symbol _ | Layer _) as visible_memory ->
      let bytes = MMU.read state ~addr bytes dir visible_memory in
      (bytes, Q state)

let select : string -> addr:Expr.t -> int -> Expr.endianness -> t -> Expr.t * t
    =
 fun name ~addr bytes dir (Q state) ->
  try
    let array = StrMap.find name state.visible_arrays in
    let bytes = MMU.read state ~addr bytes dir array in
    (bytes, Q state)
  with Not_found -> raise (Undeclared (Some name))

let empty : unit -> t =
 fun () ->
  Q
    {
      visible_symbols = IntMap.empty;
      visible_arrays = StrMap.empty;
      visible_memory = Memory.none;
      anchors = IntSet.empty;
      predicate =
        {
          clauses = 0;
          constraints = [];
          reverse_dependencies = BvHmap.empty;
          domain = (module Domains.Interval);
          values = BvHmap.empty;
          token = { contents = AsMap.empty; lemmas = [] };
        };
    }

let declare : array:string option -> int -> t -> t =
 fun ~array index (Q state) ->
  match array with
  | None -> Q { state with visible_memory = Memory.any (Memory.root index) }
  | Some name ->
      Q
        {
          state with
          visible_arrays =
            StrMap.add name (Memory.symbol name index) state.visible_arrays;
        }

let assign : Dba.Var.t -> Expr.t -> t -> t =
 fun { id; _ } value (Q state) ->
  Q { state with visible_symbols = IntMap.add id value state.visible_symbols }

let write : addr:Expr.t -> Expr.t -> Expr.endianness -> t -> t =
 fun ~addr value dir (Q state) ->
  match state.visible_memory with
  | None -> raise (Undeclared None)
  | (Symbol _ | Layer _) as visible_memory ->
      Q
        {
          state with
          visible_memory =
            Memory.any (MMU.write state ~addr value dir visible_memory);
        }

let store : string -> addr:Expr.t -> Expr.t -> Expr.endianness -> t -> t =
 fun name ~addr value dir (Q state) ->
  try
    Q
      {
        state with
        visible_arrays =
          StrMap.add name
            (MMU.write state ~addr value dir
               (StrMap.find name state.visible_arrays))
            state.visible_arrays;
      }
  with Not_found -> raise (Undeclared (Some name))

let memcpy :
    string option -> addr:Expr.t -> int -> Loader_types.buffer -> t -> t =
  let crop :
      lo:Z.t ->
      hi:Z.t ->
      Loader_types.buffer option ->
      Loader_types.buffer option =
   fun ~lo ~hi array ->
    match array with
    | None -> None
    | Some data ->
        let lo = Z.to_int lo and hi = Z.to_int hi in
        Some (Bigarray.Array1.sub data lo (hi - lo + 1))
  in
  let blit :
      base:Z.t ->
      int ->
      Loader_types.buffer ->
      Loader_types.buffer option Zmap.t =
   fun ~base len data ->
    let size = Bigarray.Array1.dim data in
    if len <= size then
      Zmap.singleton ~lo:base
        ~hi:(Z.add base (Z.of_int (len - 1)))
        (Some (Bigarray.Array1.sub data 0 len))
    else if size = 0 then
      Zmap.singleton ~lo:base ~hi:(Z.add base (Z.of_int (len - 1))) None
    else
      Zmap.union_left
        (Zmap.singleton ~lo:base
           ~hi:(Z.add base (Z.of_int (size - 1)))
           (Some data))
        (Zmap.singleton
           ~lo:(Z.add base (Z.of_int size))
           ~hi:(Z.add base (Z.of_int (len - 1)))
           None)
  in
  fun base ~addr len orig (Q state) ->
    let array =
      match base with
      | None -> (
          match state.visible_memory with
          | None -> raise (Undeclared None)
          | (Symbol _ | Layer _) as visible_memory -> visible_memory)
      | Some name -> (
          try StrMap.find name state.visible_arrays
          with Not_found -> raise (Undeclared base))
    in
    let predicate =
      match (addr, array) with
      | ( Cst addr,
          ( (Symbol { id; _ } as symbol)
          | Layer { addr = Cst _; over = Symbol { id; _ } as symbol; _ } ) )
        when not (IntSet.mem id state.anchors) ->
          let base = Bv.value_of addr in
          {
            state.predicate with
            token =
              {
                lemmas = state.predicate.token.lemmas;
                contents =
                  AsMap.add symbol
                    (Zmap.union_left ~crop (blit ~base len orig)
                       (try AsMap.find symbol state.predicate.token.contents
                        with Not_found -> Zmap.empty))
                    state.predicate.token.contents;
              };
          }
      | _ -> state.predicate
    in
    match base with
    | None ->
        Q
          {
            state with
            visible_memory = Memory.any (MMU.source state ~addr ~len orig array);
            predicate;
          }
    | Some name ->
        Q
          {
            state with
            visible_arrays =
              StrMap.add name
                (MMU.source state ~addr ~len orig array)
                state.visible_arrays;
          }

let rec zip c0 n0 c1 n1 c0' c1' =
  if n0 < n1 then
    zip c0 n0 (List.tl c1) (n1 - 1) c0' (Expr.logand c1' (List.hd c1))
  else if n1 < n0 then
    zip (List.tl c0) (n0 - 1) c1 n1 (Expr.logand c0' (List.hd c0)) c1'
  else if c0 == c1 then (c0', c1', c0, n0)
  else
    zip (List.tl c0) (n0 - 1) (List.tl c1) (n1 - 1)
      (Expr.logand c0' (List.hd c0))
      (Expr.logand c1' (List.hd c1))

let zip c0 n0 c1 n1 =
  let c0', c1', common, n = zip c0 n0 c1 n1 Expr.one Expr.one in
  (c0', c1', common, n)

let merge_predicate :
    type a b. a predicate -> b predicate -> Expr.t * a predicate =
 fun predicate0 predicate1 ->
  if predicate0.token != predicate1.token then raise Non_mergeable
  else
    let module Domain = (val predicate0.domain) in
    let module Domain' = (val predicate1.domain) in
    match Domain.T with
    | Domain'.T ->
        let cond, cond', common, n =
          zip predicate0.constraints predicate0.clauses predicate1.constraints
            predicate1.clauses
        in
        let union_cond = Expr.logor cond cond' in
        let values =
          BvHmap.union_map_eq
            (fun e d0 d1 -> Domain.union ~size:(Expr.sizeof e) d0 d1)
            (fun e _ -> Domain.top (Expr.sizeof e))
            predicate0.values predicate1.values
        and reverse_dependencies =
          BvHmap.union_eq
            (fun _ d d' -> BvSet.union d d')
            predicate0.reverse_dependencies predicate1.reverse_dependencies
        in
        let predicate =
          {
            clauses = n + 1;
            constraints = union_cond :: common;
            reverse_dependencies;
            domain = predicate0.domain;
            values;
            token = predicate0.token;
          }
        in
        ignore (Ai.eval predicate union_cond);
        Ai.refine predicate union_cond Domain.one;
        (cond', predicate)
    | _ -> raise Non_mergeable

let merge : t -> t -> t =
  let root : Memory.t -> Memory.t =
   fun array -> match Memory.base array with Symbol _ as symbol -> symbol
  in
  fun (Q state0 as t0) (Q state1 as t1) ->
    if t0 == t1 then t0
    else
      let pivot, predicate =
        merge_predicate state0.predicate state1.predicate
      in
      let visible_symbols =
        if state0.visible_symbols == state1.visible_symbols then
          state0.visible_symbols
        else
          IntMap.merge
            (fun _ o0 o1 ->
              match (o0, o1) with
              | None, None -> assert false
              | Some e0, Some e1 ->
                  if Expr.is_equal e0 e1 then o0
                  else Some (Expr.ite pivot e1 e0)
              | Some _, None -> o0
              | None, Some _ -> o1)
            state0.visible_symbols state1.visible_symbols
      and anchors = IntSet.union state0.anchors state1.anchors in
      let state =
        {
          visible_symbols;
          visible_arrays = state0.visible_arrays;
          visible_memory = state0.visible_memory;
          anchors;
          predicate;
        }
      in
      let visible_arrays =
        if state0.visible_arrays == state1.visible_arrays then
          state0.visible_arrays
        else
          StrMap.merge
            (fun _ o0 o1 ->
              match (o0, o1) with
              | None, None -> assert false
              | Some a0, None -> Some (MMU.merge state pivot (root a0) a0)
              | None, Some a1 -> Some (MMU.merge state pivot a1 (root a1))
              | Some a0, Some a1 -> Some (MMU.merge state pivot a1 a0))
            state0.visible_arrays state1.visible_arrays
      and visible_memory : [ `Any ] Memory.node =
        match (state0.visible_memory, state1.visible_memory) with
        | None, None -> None
        | ((Symbol _ | Layer _) as visible_memory0), None ->
            Memory.any
              (MMU.merge state pivot (root visible_memory0) visible_memory0)
        | None, ((Symbol _ | Layer _) as visible_memory1) ->
            Memory.any
              (MMU.merge state pivot visible_memory1 (root visible_memory1))
        | ( ((Symbol _ | Layer _) as visible_memory0),
            ((Symbol _ | Layer _) as visible_memory1) ) ->
            Memory.any (MMU.merge state pivot visible_memory1 visible_memory0)
      in
      Q { state with visible_arrays; visible_memory }

let add_condition : type a. a predicate -> Expr.t -> a predicate =
  let rec refine : type a. a predicate -> Expr.t -> a predicate =
   fun ({ clauses; constraints; reverse_dependencies; domain; values; token } as
        predicate) cond ->
    let open (val predicate.domain) in
    match cond with
    | Binary { f = Or; x; y; _ } -> (
        BvHmap.freeze values;
        match
          refine
            {
              clauses;
              constraints;
              reverse_dependencies;
              domain;
              values;
              token;
            }
            y
        with
        | exception Domains.Empty ->
            Ai.refine predicate x one;
            predicate
        | predicate' -> (
            try
              Ai.refine predicate x one;
              predicate.values <-
                BvHmap.union_map_eq
                  (fun e d0 d1 -> union ~size:(Expr.sizeof e) d0 d1)
                  (fun e _ -> top (Expr.sizeof e))
                  predicate.values predicate'.values;
              predicate
            with Domains.Empty -> predicate'))
    | _ ->
        Ai.refine predicate cond one;
        predicate
  in
  fun { clauses; constraints; reverse_dependencies; domain; values; token } cond ->
    BvHmap.freeze reverse_dependencies;
    BvHmap.freeze values;
    refine
      {
        clauses = clauses + 1;
        constraints = cond :: constraints;
        reverse_dependencies;
        domain;
        values;
        token;
      }
      cond

let assume : Expr.t -> t -> t option =
 fun cond (Q state) ->
  let module Domain = (val state.predicate.domain) in
  match Domain.is_zero (Ai.eval state.predicate cond) with
  | True -> None
  | False -> Some (Q state)
  | Unknown -> (
      match add_condition state.predicate cond with
      | exception Domains.Empty -> None
      | predicate -> Some (Q { state with predicate }))

let is_symbolic : Expr.t -> t -> bool =
 fun e (Q state) ->
  let open (val state.predicate.domain) in
  not (is_point ~size:(Expr.sizeof e) (Ai.eval state.predicate e))

let is_zero : Expr.t -> t -> trilean =
 fun e (Q state) ->
  let open (val state.predicate.domain) in
  is_zero (Ai.eval state.predicate e)

let predicate : t -> Expr.t list =
 fun (Q { predicate = { constraints; _ }; _ }) -> constraints

let check_sat : Cookie.t -> t -> Model.t option =
 fun cookie (Q state) ->
  match
    Cookie.check_sat cookie state.predicate.token state.predicate.constraints
  with
  | Sat model -> Some model
  | Unsat -> None
  | Unknown -> raise Unknown

let enumerate : Cookie.t -> Expr.t -> ?except:Bv.t list -> t -> Enumeration.t =
 fun cookie target ?(except = []) (Q state) ->
  Cookie.enumerate_values cookie state.predicate.token
    state.predicate.constraints target ~except

let print_smtlib :
    ?slice:(Value.t * string) list -> Format.formatter -> t -> unit =
 fun ?slice ppf (Q state) ->
  let ctx = Printer.create ~next_id:Uid.zero () in
  (* visit assertions *)
  List.iter (Printer.visit_bl ctx) state.predicate.constraints;
  (* visit terms *)
  let defs =
    match slice with
    | Some defs ->
        List.iter (fun (e, _) -> Printer.visit_bv ctx e) defs;
        defs
    | None ->
        (match state.visible_memory with
        | None -> ()
        | (Symbol _ | Layer _) as visible_memory ->
            Printer.visit_ax ctx visible_memory);
        StrMap.iter (fun _ ax -> Printer.visit_ax ctx ax) state.visible_arrays;
        List.rev
          (IntMap.fold
             (fun id expr defs ->
               match Dba.Var.from_id id with
               | exception Not_found -> defs
               | { name; _ } ->
                   Printer.visit_bv ctx expr;
                   (expr, name) :: defs)
             state.visible_symbols [])
  in
  Format.pp_open_vbox ppf 0;
  (* print definitions *)
  Printer.pp_print_defs ppf ctx;
  List.iter
    (fun (bv, name) ->
      Format.fprintf ppf "@[<h>(define-fun %s () (_ BitVec %d)@ " name
        (Expr.sizeof bv);
      Printer.pp_print_bv ctx ppf bv;
      Format.fprintf ppf ")@]@ ")
    defs;
  if slice = None then (
    (match state.visible_memory with
    | None -> ()
    | (Symbol _ | Layer _) as visible_memory ->
        Format.fprintf ppf
          "@[<h>(define-fun memory () (Array (_ BitVec %d) (_ BitVec 8))@ %a)@]"
          (match Memory.base visible_memory with Symbol { index; _ } -> index)
          (Printer.pp_print_ax ctx) visible_memory);
    StrMap.iter
      (fun name ax ->
        Format.fprintf ppf
          "@[<h>(define-fun %s () (Array (_ BitVec %d) (_ BitVec 8))@ %a)@]"
          name
          (match Memory.base ax with Symbol { index; _ } -> index)
          (Printer.pp_print_ax ctx) ax)
      state.visible_arrays);
  (* print assertions *)
  List.iter
    (fun bl ->
      Format.pp_open_hbox ppf ();
      Format.pp_print_string ppf "(assert ";
      Printer.pp_print_bl ctx ppf bl;
      Format.pp_print_char ppf ')';
      Format.pp_close_box ppf ();
      Format.pp_print_space ppf ())
    state.predicate.constraints;
  Format.pp_close_box ppf ()

let pp : Format.formatter -> t -> unit =
 fun ppf t -> print_smtlib ?slice:None ppf t

let to_formula : t -> Smtlib.Formula.formula =
 fun (Q state) ->
  let ctx = To_formula.create ~next_id:Uid.zero () in
  List.iter (To_formula.assert_bl ctx) state.predicate.constraints;
  (match state.visible_memory with
  | None -> ()
  | (Symbol _ | Layer _) as visible_memory ->
      To_formula.define_ax ctx "memory" visible_memory);
  StrMap.iter
    (fun name ax -> To_formula.define_ax ctx name ax)
    state.visible_arrays;
  IntMap.iter
    (fun id expr ->
      match Dba.Var.from_id id with
      | exception Not_found -> ()
      | { name; _ } -> To_formula.define_bv ctx name expr)
    state.visible_symbols;
  To_formula.to_formula ctx

let visible_symbols : t -> Expr.t Dba_types.Var.Map.t =
 fun (Q state) ->
  IntMap.fold
    (fun id expr map ->
      match Dba.Var.from_id id with
      | exception Not_found -> map
      | var -> Dba_types.Var.Map.add var expr map)
    state.visible_symbols Dba_types.Var.Map.empty

let visible_memory : t -> Memory.t =
 fun (Q { visible_memory; _ }) ->
  match visible_memory with
  | None -> raise Not_found
  | (Symbol _ | Layer _) as visible_memory -> visible_memory

let visible_arrays : t -> Memory.t StrMap.t =
 fun (Q { visible_arrays; _ }) -> visible_arrays

let more : type a. (Value.t, t, Cookie.t, a) State.feature -> a option =
  function
  | State.ValueKind -> Some Term
  | State.SetSMTSolver -> Some Cookie.set_backend
  | State.SetSMTSolverTimeout -> Some Cookie.set_timeout
  | State.SetSMTDumpDir -> Some Cookie.set_dump_dir
  | State.SetSMTSolverMultiChecks -> Some Cookie.set_multi_checks
  | State.ToFormula -> Some to_formula
  | VisibleSymbols -> Some visible_symbols
  | VisibleMemory -> Some visible_memory
  | VisibleArrays -> Some visible_arrays
  | Downcast -> Some Fun.id
  | Upcast -> Some Fun.id
  | _ -> None
