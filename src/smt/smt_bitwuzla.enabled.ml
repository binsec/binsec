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

module Logger = Logger.Make (struct
  let name = "bitwuzla"
end)

let available = true
let queries = ref 0

type time = { mutable sec : float }

let cumulated_time = { sec = 0. }
let query_stat () = !queries
let time_stat () = cumulated_time.sec

module type Session = sig
  val put_entry : Formula.entry -> unit
  val bv_lookup : Formula.bv_term -> Bitvector.t
  val ax_lookup : Formula.ax_term -> (Bitvector.t * Bitvector.t) array
  val check_sat : timeout:int -> Formula.status
  val close : unit -> unit
end

module Make () : Session = struct
  include Bitwuzla.Incremental ()
  module BlH = Formula.BlTermHashtbl
  module BvH = Formula.BvTermHashtbl
  module AxH = Formula.AxTermHashtbl
  module I = Basic_types.Int.Htbl

  module P = Hashtbl.Make (struct
    type t = int * int

    let equal = ( = )
    let hash = Hashtbl.hash
  end)

  let bl_mapping = BlH.create 64
  let bv_mapping = BvH.create 64
  let ax_mapping = AxH.create 64
  let bv_sort = I.create 32
  let ax_sort = P.create 8

  let bv_sort sz =
    try I.find bv_sort sz
    with Not_found ->
      let sort = Sort.bv sz in
      I.add bv_sort sz sort;
      sort

  let ax_sort idx elt =
    let p = (idx, elt) in
    try P.find ax_sort p
    with Not_found ->
      let sort = Sort.ar (bv_sort idx) (bv_sort elt) in
      P.add ax_sort p sort;
      sort

  let bl_bnop_map : Formula.bl_bnop -> bv Term.t -> bv Term.t -> bv Term.t =
    function
    | BlImply -> Term.Bl.implies
    | BlAnd -> Term.Bl.logand
    | BlOr -> Term.Bl.logor
    | BlXor -> Term.Bl.logxor

  let bl_comp_map : Formula.bl_comp -> bv Term.t -> bv Term.t -> bv Term.t =
    function
    | BlEqual -> Term.equal
    | BlDistinct -> Term.distinct

  let rec repeat n bv_term acc =
    if n = 0 then acc else repeat (n - 1) bv_term (Term.Bv.append bv_term acc)

  let bv_unop_map : Formula.bv_unop -> bv Term.t -> bv Term.t = function
    | BvNot -> Term.Bv.lognot
    | BvNeg -> Term.Bv.neg
    | BvRepeat n -> fun bv_term -> repeat (n - 1) bv_term bv_term
    | BvZeroExtend n -> Term.Bv.zero_extend n
    | BvSignExtend n -> Term.Bv.sign_extend n
    | BvRotateLeft n -> (Fun.flip Term.Bv.rotate_lefti) n
    | BvRotateRight n -> (Fun.flip Term.Bv.rotate_righti) n
    | BvExtract { hi; lo } -> Term.Bv.extract ~hi ~lo

  let bv_bnop_map : Formula.bv_bnop -> bv Term.t -> bv Term.t -> bv Term.t =
    function
    | BvConcat -> Term.Bv.append
    | BvAnd -> Term.Bv.logand
    | BvNand -> Term.Bv.lognand
    | BvOr -> Term.Bv.logor
    | BvNor -> Term.Bv.lognor
    | BvXor -> Term.Bv.logxor
    | BvXnor -> Term.Bv.logxnor
    | BvCmp -> Term.equal
    | BvAdd -> Term.Bv.add
    | BvSub -> Term.Bv.sub
    | BvMul -> Term.Bv.mul
    | BvUdiv -> Term.Bv.udiv
    | BvSdiv -> Term.Bv.sdiv
    | BvUrem -> Term.Bv.urem
    | BvSrem -> Term.Bv.srem
    | BvSmod -> Term.Bv.smod
    | BvShl -> Term.Bv.shift_left
    | BvAshr -> Term.Bv.shift_right
    | BvLshr -> Term.Bv.shift_right_logical

  let bv_comp_map : Formula.bv_comp -> bv Term.t -> bv Term.t -> bv Term.t =
    function
    | BvEqual -> Term.equal
    | BvDistinct -> Term.distinct
    | BvUlt -> Term.Bv.ult
    | BvUle -> Term.Bv.ule
    | BvUgt -> Term.Bv.ugt
    | BvUge -> Term.Bv.uge
    | BvSlt -> Term.Bv.slt
    | BvSle -> Term.Bv.sle
    | BvSgt -> Term.Bv.sgt
    | BvSge -> Term.Bv.sge

  let ax_comp_map :
      Formula.ax_comp -> (bv, bv) ar Term.t -> (bv, bv) ar Term.t -> bv Term.t =
    function
    | AxEqual -> Term.equal
    | AxDistinct -> Term.distinct

  let rec mk_select sz ax_term bv_term acc =
    if sz = 0 then acc
    else
      mk_select (sz - 1) ax_term (Term.Bv.succ bv_term)
        (Term.Bv.append (Term.Ar.select ax_term bv_term) acc)

  let mk_select sz ax_term bv_term =
    mk_select (sz - 1) ax_term (Term.Bv.succ bv_term)
      (Term.Ar.select ax_term bv_term)

  let rec mk_store bindings elt_size sz ax_term bv_term0 bv_term1 =
    if elt_size = sz then
      Term.Ar.store ax_term bv_term0 (bv_map bindings bv_term1)
    else
      mk_store bindings elt_size (sz - elt_size)
        (Term.Ar.store ax_term bv_term0
           (bv_map bindings
              (Formula.mk_bv_extract
                 { Interval.hi = elt_size - 1; lo = 0 }
                 bv_term1)))
        (Term.Bv.succ bv_term0)
        (Formula.mk_bv_extract { Interval.hi = sz - 1; lo = elt_size } bv_term1)

  and bind defs bindings =
    let bl_mapping = BlH.create 8
    and bv_mapping = BvH.create 8
    and ax_mapping = AxH.create 8 in
    List.iter (put_define [ (bl_mapping, bv_mapping, ax_mapping) ]) defs;
    (bl_mapping, bv_mapping, ax_mapping) :: bindings

  and bl_lookup bl_term = function
    | [] -> BlH.find bl_mapping bl_term
    | (bl_mapping, _, _) :: bindings -> (
        try BlH.find bl_mapping bl_term
        with Not_found -> bl_lookup bl_term bindings)

  and bv_lookup bv_term = function
    | [] -> BvH.find bv_mapping bv_term
    | (_, bv_mapping, _) :: bindings -> (
        try BvH.find bv_mapping bv_term
        with Not_found -> bv_lookup bv_term bindings)

  and ax_lookup ax_term = function
    | [] -> AxH.find ax_mapping ax_term
    | (_, _, ax_mapping) :: bindings -> (
        try AxH.find ax_mapping ax_term
        with Not_found -> ax_lookup ax_term bindings)

  and bl_map bindings (bl_term : Formula.bl_term) =
    try bl_lookup bl_term bindings
    with Not_found ->
      let bl_term' =
        match bl_term.bl_term_desc with
        | BlTrue -> Term.Bl.true'
        | BlFalse -> Term.Bl.false'
        | BlFun _ -> assert false
        | BlLet (defs, bl_term) -> bl_map (bind defs bindings) bl_term
        | BlUnop (BlNot, bl_term) -> Term.Bl.lognot (bl_map bindings bl_term)
        | BlBnop (op, bl_term0, bl_term1) ->
            (bl_bnop_map op) (bl_map bindings bl_term0)
              (bl_map bindings bl_term1)
        | BlComp (op, bl_term0, bl_term1) ->
            (bl_comp_map op) (bl_map bindings bl_term0)
              (bl_map bindings bl_term1)
        | BvComp (op, bv_term0, bv_term1) ->
            (bv_comp_map op) (bv_map bindings bv_term0)
              (bv_map bindings bv_term1)
        | AxComp (op, ax_term0, ax_term1) ->
            (ax_comp_map op) (ax_map bindings ax_term0)
              (ax_map bindings ax_term1)
        | BlIte (bl_term0, bl_term1, bl_term2) ->
            Term.ite (bl_map bindings bl_term0) (bl_map bindings bl_term1)
              (bl_map bindings bl_term2)
      in
      BlH.add bl_mapping bl_term bl_term';
      bl_term'

  and bv_map bindings (bv_term : Formula.bv_term) =
    try bv_lookup bv_term bindings
    with Not_found ->
      let bv_term' =
        match bv_term.bv_term_desc with
        | BvCst bv ->
            Term.Bv.of_z
              (bv_sort (Bitvector.size_of bv))
              (Bitvector.value_of bv)
        | BvFun ({ bv_name; bv_size; _ }, []) ->
            Term.const (bv_sort bv_size) bv_name
        | BvFun _ -> assert false
        | BvLet (defs, bv_term) -> bv_map (bind defs bindings) bv_term
        | BvUnop (op, bv_term) -> (bv_unop_map op) (bv_map bindings bv_term)
        | BvBnop (op, bv_term0, bv_term1) ->
            (bv_bnop_map op) (bv_map bindings bv_term0)
              (bv_map bindings bv_term1)
        | BvIte (bl_term, bv_term0, bv_term1) ->
            Term.ite (bl_map bindings bl_term) (bv_map bindings bv_term0)
              (bv_map bindings bv_term1)
        | Select (1, ax_term, bv_term) ->
            Term.Ar.select (ax_map bindings ax_term) (bv_map bindings bv_term)
        | Select (sz, ax_term, bv_term) ->
            mk_select sz (ax_map bindings ax_term) (bv_map bindings bv_term)
      in
      BvH.add bv_mapping bv_term bv_term';
      bv_term'

  and ax_map bindings (ax_term : Formula.ax_term) =
    try ax_lookup ax_term bindings
    with Not_found ->
      let ax_term' =
        match ax_term.ax_term_desc with
        | AxFun _ -> assert false
        | AxLet (defs, ax_term) -> ax_map (bind defs bindings) ax_term
        | AxIte (bl_term, ax_term0, ax_term1) ->
            Term.ite (bl_map bindings bl_term) (ax_map bindings ax_term0)
              (ax_map bindings ax_term1)
        | Store (1, ax_term, bv_term0, bv_term1) ->
            Term.Ar.store (ax_map bindings ax_term) (bv_map bindings bv_term0)
              (bv_map bindings bv_term1)
        | Store (sz, ax_term, bv_term0, bv_term1) ->
            mk_store bindings ax_term.elt_term_size
              (sz * ax_term.elt_term_size)
              (ax_map bindings ax_term) (bv_map bindings bv_term0) bv_term1
      in
      AxH.add ax_mapping ax_term ax_term';
      ax_term'

  and[@warning "-8"] put_define
      ((bl_mapping, bv_mapping, ax_mapping) :: _ as bindings)
      (def : Formula.def) =
    match def.def_desc with
    | BlDef (bl_var, _, bl_term) ->
        BlH.add bl_mapping (Formula.mk_bl_var bl_var) (bl_map bindings bl_term)
    | BvDef (bv_var, _, bv_term) ->
        BvH.add bv_mapping (Formula.mk_bv_var bv_var) (bv_map bindings bv_term)
    | AxDef (ax_var, _, ax_term) ->
        AxH.add ax_mapping (Formula.mk_ax_var ax_var) (ax_map bindings ax_term)

  let put_declare : Formula.decl -> unit = function
    | { decl_desc = BlDecl (({ bl_name; _ } as bl_var), _); _ } ->
        BlH.add bl_mapping (Formula.mk_bl_var bl_var)
          (Term.const Sort.bool bl_name)
    | { decl_desc = BvDecl (({ bv_name; bv_size; _ } as bv_var), _); _ } ->
        BvH.add bv_mapping (Formula.mk_bv_var bv_var)
          (Term.const (bv_sort bv_size) bv_name)
    | {
        decl_desc = AxDecl (({ ax_name; idx_size; elt_size; _ } as ax_var), _);
        _;
      } ->
        AxH.add ax_mapping (Formula.mk_ax_var ax_var)
          (Term.const (ax_sort idx_size elt_size) ax_name)

  let put_define = put_define [ (bl_mapping, bv_mapping, ax_mapping) ]

  let put_entry (entry : Formula.entry) =
    match entry.entry_desc with
    | Declare decl -> put_declare decl
    | Define def -> put_define def
    | Assert bl_term | Assume bl_term -> assert' (bl_map [] bl_term)
    | Comment _ -> ()

  let bv_lookup e =
    Bitvector.create
      (Term.Bv.assignment (get_value (bv_map [] e)))
      e.bv_term_size

  let ax_lookup e =
    let addr_size, elem_size = Formula_utils.ax_size e in
    Array.map
      (fun (addr, value) ->
        ( Bitvector.create (Term.Bv.assignment addr) addr_size,
          Bitvector.create (Term.Bv.assignment value) elem_size ))
      (fst (Term.Ar.assignment (get_value (ax_map [] e))))

  let check_sat ~timeout =
    match Smt_bitwuzla_utils.watchdog ~timeout check_sat () with
    | Sat -> Formula.SAT
    | Unsat -> Formula.UNSAT
    | Unknown -> Formula.UNKNOWN

  let check_sat ~timeout =
    match Utils.time (fun () -> check_sat ~timeout) with
    | time, r ->
        Logger.debug "solver returned %a in %fs" Formula_pp.pp_status r time;
        r

  let close = unsafe_close
end

type t = (module Session) * float

let open_session () =
  let t = Unix.gettimeofday () in
  ((module Make () : Session), t)

let check_sat (solver, _) =
  incr queries;
  let timeout = Formula_options.Solver.Timeout.get () in
  let module S = (val solver : Session) in
  S.check_sat ~timeout

let get_bv_value (solver, _) e =
  let module S = (val solver : Session) in
  S.bv_lookup e

let get_ax_values (solver, _) e =
  let module S = (val solver : Session) in
  S.ax_lookup e

let put (solver, _) e =
  let module S = (val solver : Session) in
  S.put_entry e

let close_session (solver, t) =
  let module S = (val solver : Session) in
  S.close ();
  cumulated_time.sec <- cumulated_time.sec +. Unix.gettimeofday () -. t;
  Logger.debug "solver session closed in %fs" (Unix.gettimeofday () -. t)

let check_sat_and_close solver =
  let res = check_sat solver in
  close_session solver;
  res
