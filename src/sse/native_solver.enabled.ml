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

module Bv = Sexpr.Bv
module Expr = Sexpr.Expr
module Memory = Sexpr.Memory
module BvTbl = Sexpr.BvTbl
module AxTbl = Sexpr.AxTbl
module NiTbl = Basic_types.Int.Htbl
module BiMap = Basic_types.BigInt.Map

let byte_size = Natural.to_int Basic_types.Constants.bytesize

module Solver () : Solver_sig.S = struct
  module Solver = Bitwuzla.Incremental ()

  module T = Term
  open Solver

  type nonrec result = result = Sat | Unsat | Unknown

  type memory = (bv, bv) ar term

  type nonrec term = bv term

  type nonrec value = bv value

  type access = Select of term * int | Store of term

  module Context = struct
    type t = {
      st_cons : bv sort NiTbl.t;
      bv_cons : term BvTbl.t;
      ax_cons : memory AxTbl.t;
      history : access Queue.t;
    }

    let create () =
      {
        st_cons = NiTbl.create 8;
        bv_cons = BvTbl.create 128;
        ax_cons = AxTbl.create 64;
        history = Queue.create ();
      }
  end

  open Context

  let visit_sort ctx sz =
    try NiTbl.find ctx.st_cons sz
    with Not_found ->
      let st = Sort.bv sz in
      NiTbl.add ctx.st_cons sz st;
      st

  and visit_select =
    let rec iter len concat index array res =
      if len = 0 then res
      else
        iter (len - 1) concat (Term.Bv.succ index) array
          (concat (Term.Ar.select array index) res)
    in
    fun len dir index array ->
      let concat =
        match dir with
        | Machine.LittleEndian -> Term.Bv.append
        | Machine.BigEndian -> assert false
        (* TODO *)
      in
      iter (len - 1) concat (Term.Bv.succ index) array
        (Term.Ar.select array index)

  and visit_binop =
    let open Expr in
    function
    | Plus -> Term.Bv.add
    | Minus -> Term.Bv.sub
    | Mul -> Term.Bv.mul
    | Udiv -> Term.Bv.udiv
    | Sdiv -> Term.Bv.sdiv
    | Umod -> Term.Bv.urem
    | Smod -> Term.Bv.srem
    | Or -> Term.Bv.logor
    | And -> Term.Bv.logand
    | Xor -> Term.Bv.logxor
    | Concat -> Term.Bv.append
    | Lsl -> Term.Bv.shift_left
    | Lsr -> Term.Bv.shift_right_logical
    | Asr -> Term.Bv.shift_right
    | Rol -> Term.Bv.rotate_left
    | Ror -> Term.Bv.rotate_right
    | Eq -> Term.equal
    | Diff -> Term.distinct
    | Ule -> Term.Bv.ule
    | Ult -> Term.Bv.ult
    | Uge -> Term.Bv.uge
    | Ugt -> Term.Bv.ugt
    | Sle -> Term.Bv.sle
    | Slt -> Term.Bv.slt
    | Sge -> Term.Bv.sge
    | Sgt -> Term.Bv.sgt

  let rec visit_bv ctx bv =
    try BvTbl.find ctx.bv_cons bv
    with Not_found ->
      let e =
        match bv with
        | Var { name; size; _ } -> Term.const (visit_sort ctx size) name
        | Load { len; dir; addr; label; _ } ->
            let sort = visit_sort ctx (Expr.sizeof addr) in
            let index = visit_bv ctx addr and array = visit_ax ctx sort label in
            Queue.push (Select (index, len)) ctx.history;
            visit_select len dir index array
        | Cst bv ->
            let size = Bv.size_of bv and value = Bv.value_of bv in
            let st = visit_sort ctx size in
            Term.Bv.of_z st value
        | Unary { f = Not; x; _ } -> Term.Bv.lognot (visit_bv ctx x)
        | Unary { f = Minus; x; _ } -> Term.Bv.neg (visit_bv ctx x)
        | Unary { f = Uext n; x; _ } -> Term.Bv.zero_extend n (visit_bv ctx x)
        | Unary { f = Sext n; x; _ } -> Term.Bv.sign_extend n (visit_bv ctx x)
        | Unary { f = Restrict { lo; hi }; x; _ } ->
            Term.Bv.extract ~hi ~lo (visit_bv ctx x)
        | Binary { f; x; y; _ } ->
            (visit_binop f) (visit_bv ctx x) (visit_bv ctx y)
        | Ite { c; t; e; _ } ->
            Term.ite (visit_bv ctx c) (visit_bv ctx t) (visit_bv ctx e)
      in
      BvTbl.add ctx.bv_cons bv e;
      e

  and visit_ax ctx index ax =
    try AxTbl.find ctx.ax_cons ax
    with Not_found ->
      let a =
        match ax with
        | Memory.Unknown ->
            Term.const
              (Sort.ar index (visit_sort ctx byte_size))
              Suid.(to_string zero)
        | Memory.Source { over; _ } -> visit_ax ctx index over
        | Memory.Layer { addr; bytes; over; _ } ->
            let base = visit_bv ctx addr and array = visit_ax ctx index over in
            BiMap.fold
              (fun i byte array ->
                let x = visit_bv ctx byte
                and index = Term.Bv.(add base (of_z index i)) in
                Queue.push (Store index) ctx.history;
                Term.Ar.store array index x)
              bytes array
      in
      AxTbl.add ctx.ax_cons ax a;
      a

  let ctx = create ()

  let put _ constraints =
    List.iter (fun bl -> assert' (visit_bv ctx bl)) constraints

  let bind _ e constraints =
    let e = visit_bv ctx e in
    put () constraints;
    e

  let get e = BvTbl.find ctx.bv_cons e

  let set_memory ~addr value =
    let sort =
      match Queue.top ctx.history with Select (t, _) | Store t -> Term.sort t
    in
    assert'
      (Term.equal
         (Term.Ar.select
            (AxTbl.find ctx.ax_cons Memory.Unknown)
            (Term.Bv.of_z sort addr))
         (Term.Bv.of_z (visit_sort ctx byte_size) value))

  let neq e x = assert' (Term.distinct e (Term.Bv.of_z (Term.sort e) x))

  let get_memory () = (AxTbl.find ctx.ax_cons Memory.Unknown, ctx.history)

  let get_at ar (x : value) =
    Term.Bv.assignment @@ get_value (Term.Ar.select ar (x :> term))

  let get_value x = get_value x

  let assignment = Term.Bv.assignment

  let succ (x : value) = get_value (Term.Bv.succ (x :> term))

  let timeout = Formula_options.Solver.Timeout.get ()

  let check_sat () = Smt_bitwuzla_utils.watchdog ~timeout check_sat ()

  let close () = unsafe_close ()

  (* let check_binop f x y e =
   *   let x' = visit_bv ctx x and y' = visit_bv ctx y and e' = visit_bv ctx e in
   *   assert' (Term.distinct (visit_binop f x' y') e');
   *   match check_sat () with
   *   | Sat ->
   *       Sse_options.Logger.fatal "(%a %a %a) <> %a" T.pp_op f T.pp x T.pp y T.pp
   *         e
   *   | _ -> close ()
   *
   * let visit_unop =
   *   let open T in
   *   function
   *   | Not -> Term.Bv.lognot
   *   | Minus -> Term.Bv.neg
   *   | Uext n -> Term.Bv.zero_extend n
   *   | Sext n -> Term.Bv.sign_extend n
   *   | Restrict { lo; hi } -> Term.Bv.extract ~hi ~lo
   *
   * let check_unop f x e =
   *   let x' = visit_bv ctx x and e' = visit_bv ctx e in
   *   assert' (Term.distinct (visit_unop f x') e');
   *   match check_sat () with
   *   | Sat -> Sse_options.Logger.fatal "(%a %a) <> %a" T.pp_op f T.pp x T.pp e
   *   | _ -> close () *)
end
