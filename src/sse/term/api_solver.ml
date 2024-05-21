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

module Bv = Sexpr.Bv
module Expr = Sexpr.Expr
module Store = Sexpr.Store
module Memory = Sexpr.Memory
module StTbl = Sexpr.StTbl
module BvTbl = Sexpr.BvTbl
module AxTbl = Sexpr.AxTbl
module NiTbl = Basic_types.Int.Htbl
module BiMap = Basic_types.BigInt.Map

let byte_size = Natural.to_int Basic_types.Constants.bytesize

module type CONTEXT = sig
  type bl
  type bv
  type ax

  val addr_space : int
  val visit_bl : Expr.t -> bl
  val visit_bv : Expr.t -> bv
  val visit_ax : Memory.t -> ax
  val iter_free_variables : (string -> Expr.t -> unit) -> unit
  val iter_free_arrays : (string -> Memory.t -> unit) -> unit
  val get : Expr.t -> bv
end

type ('bl, 'bv, 'ax) context =
  (module CONTEXT with type bl = 'bl and type bv = 'bv and type ax = 'ax)

module type AXHOOK = sig
  val visit_load : ([ `Mem ], string, Memory.t) Term.t -> unit
  val visit_ax : Memory.t -> unit
end

module Context (H : AXHOOK) (S : Libsolver.S) :
  CONTEXT with type bl = S.Bl.t and type bv = S.Bv.t and type ax = S.Ax.t =
struct
  open S

  type bl = Bl.t
  type bv = Bv.t
  type ax = Ax.t

  let fvariables = StTbl.create 16
  let farrays = StTbl.create 4
  let bl_cons = BvTbl.create 128
  let bv_cons = BvTbl.create 128
  let ax_cons = AxTbl.create 64
  let addr_space = Kernel_options.Machine.word_size ()
  let array_sort = Ax.sort ~idx:addr_space byte_size

  let visit_select =
    let rec iter len concat index array res =
      if len = 0 then res
      else
        iter (len - 1) concat (Bv.succ index) array
          (concat (Ax.select array index) res)
    in
    fun len dir index array ->
      let concat =
        match dir with
        | Machine.LittleEndian -> Bv.append
        | Machine.BigEndian -> Fun.flip Bv.append
      in
      iter (len - 1) concat (Bv.succ index) array (Ax.select array index)

  let rec unroll_store array index x s =
    if s = 8 then Ax.store array index x
    else
      unroll_store
        (Ax.store array index (Bv.extract ~hi:7 ~lo:0 x))
        (Bv.succ index)
        (Bv.extract ~hi:(s - 1) ~lo:8 x)
        (s - 8)

  let rec visit_bl bl =
    try BvTbl.find bl_cons bl
    with Not_found ->
      let e =
        match bl with
        | Cst bv -> if Term.Bv.is_zero bv then Bl.bot else Bl.top
        | Load _ (* cannot be a bl<1> *) -> assert false
        | Unary { f = Not; x; _ } -> Bl.lognot (visit_bl x)
        | Binary { f = And; x; y; _ } -> Bl.logand (visit_bl x) (visit_bl y)
        | Binary { f = Or; x; y; _ } -> Bl.logor (visit_bl x) (visit_bl y)
        | Binary { f = Xor; x; y; _ } -> Bl.logxor (visit_bl x) (visit_bl y)
        | Binary { f = Eq; x; y; _ } -> Bv.equal (visit_bv x) (visit_bv y)
        | Binary { f = Diff; x; y; _ } -> Bv.diff (visit_bv x) (visit_bv y)
        | Binary { f = Uge; x; y; _ } -> Bv.uge (visit_bv x) (visit_bv y)
        | Binary { f = Ule; x; y; _ } -> Bv.ule (visit_bv x) (visit_bv y)
        | Binary { f = Ugt; x; y; _ } -> Bv.ugt (visit_bv x) (visit_bv y)
        | Binary { f = Ult; x; y; _ } -> Bv.ult (visit_bv x) (visit_bv y)
        | Binary { f = Sge; x; y; _ } -> Bv.sge (visit_bv x) (visit_bv y)
        | Binary { f = Sle; x; y; _ } -> Bv.sle (visit_bv x) (visit_bv y)
        | Binary { f = Sgt; x; y; _ } -> Bv.sgt (visit_bv x) (visit_bv y)
        | Binary { f = Slt; x; y; _ } -> Bv.slt (visit_bv x) (visit_bv y)
        | Ite { c; t; e; _ } -> Bl.ite (visit_bl c) (visit_bl t) (visit_bl e)
        | Var _ | Unary _ | Binary _ -> Bv.to_bl (visit_bv bl)
      in
      BvTbl.add bl_cons bl e;
      e

  and visit_bv bv =
    try BvTbl.find bv_cons bv
    with Not_found ->
      let e =
        match bv with
        | Var { name; size; _ } ->
            StTbl.add fvariables name bv;
            Bv.const size name
        | Load { len; dir; addr; label; _ } ->
            let index = visit_bv addr and array = visit_ax label in
            H.visit_load (Term.to_mem_exn bv);
            visit_select len dir index array
        | Cst bv -> Bv.value (Bitvector.size_of bv) (Bitvector.value_of bv)
        | Unary { f = Not; x; _ } -> Bv.lognot (visit_bv x)
        | Unary { f = Minus; x; _ } -> Bv.neg (visit_bv x)
        | Unary { f = Uext n; x; _ } -> Bv.uext n (visit_bv x)
        | Unary { f = Sext n; x; _ } -> Bv.sext n (visit_bv x)
        | Unary { f = Restrict { lo; hi }; x; _ } ->
            Bv.extract ~hi ~lo (visit_bv x)
        | Binary { f = Plus; x; y; _ } -> Bv.add (visit_bv x) (visit_bv y)
        | Binary { f = Minus; x; y; _ } -> Bv.sub (visit_bv x) (visit_bv y)
        | Binary { f = Mul; x; y; _ } -> Bv.mul (visit_bv x) (visit_bv y)
        | Binary { f = Udiv; x; y; _ } -> Bv.udiv (visit_bv x) (visit_bv y)
        | Binary { f = Sdiv; x; y; _ } -> Bv.sdiv (visit_bv x) (visit_bv y)
        | Binary { f = Umod; x; y; _ } -> Bv.umod (visit_bv x) (visit_bv y)
        | Binary { f = Smod; x; y; _ } -> Bv.smod (visit_bv x) (visit_bv y)
        | Binary { f = Or; x; y; _ } -> Bv.logor (visit_bv x) (visit_bv y)
        | Binary { f = And; x; y; _ } -> Bv.logand (visit_bv x) (visit_bv y)
        | Binary { f = Xor; x; y; _ } -> Bv.logxor (visit_bv x) (visit_bv y)
        | Binary { f = Concat; x; y; _ } -> Bv.append (visit_bv x) (visit_bv y)
        | Binary { f = Lsl; x; y; _ } -> Bv.shift_left (visit_bv x) (visit_bv y)
        | Binary { f = Lsr; x; y; _ } ->
            Bv.shift_right (visit_bv x) (visit_bv y)
        | Binary { f = Asr; x; y; _ } ->
            Bv.shift_right_signed (visit_bv x) (visit_bv y)
        | Binary { f = Rol; x; y; _ } ->
            Bv.rotate_left (visit_bv x) (visit_bv y)
        | Binary { f = Ror; x; y; _ } ->
            Bv.rotate_right (visit_bv x) (visit_bv y)
        | Binary
            { f = Eq | Diff | Ule | Ult | Uge | Ugt | Sle | Slt | Sge | Sgt; _ }
          ->
            Bl.to_bv (visit_bl bv)
        | Ite { c; t; e; _ } -> Bv.ite (visit_bl c) (visit_bv t) (visit_bv e)
      in

      BvTbl.add bv_cons bv e;
      e

  and visit_ax (ax : Memory.t) =
    try AxTbl.find ax_cons ax
    with Not_found ->
      let a =
        match ax with
        | Root -> Ax.const array_sort Suid.(to_string zero)
        | Symbol name ->
            StTbl.add farrays name ax;
            Ax.const array_sort name
        | Layer { addr; store; over; _ } ->
            let base = visit_bv addr and array = visit_ax over in
            Store.fold_term
              (fun i value array ->
                let s = Expr.sizeof value in
                let x = visit_bv value
                and index = Bv.add base (Bv.value addr_space i) in
                unroll_store array index x s)
              array store
      in
      H.visit_ax ax;
      AxTbl.add ax_cons ax a;
      a

  let iter_free_variables f = StTbl.iter f fvariables
  let iter_free_arrays f = StTbl.iter f farrays
  let get e = BvTbl.find bv_cons e
end

module NoHook = struct
  let visit_load _ = ()
  let visit_ax _ = ()
end

module Make (F : Libsolver.F) = struct
  module Open () : Solver.S = struct
    module Solver = F ()
    module Context = Context (NoHook) (Solver)

    let visit_formula _ = ()
    let iter_free_variables = Context.iter_free_variables
    let iter_free_arrays = Context.iter_free_arrays
    let assert_formula bl = Solver.assert_formula (Context.visit_bl bl)
    let check_sat = Solver.check_sat

    let check_sat_assuming ?timeout bl =
      Solver.check_sat_assuming ?timeout (Context.visit_bl bl)

    let get_value bv = Solver.get_bv_value (Context.visit_bv bv)

    let fold_array_values f ar x =
      Solver.fold_ax_values f (Context.visit_ax ar) x

    let push = Solver.push
    let pop = Solver.pop
    let close = Solver.close
  end
end

module BiTbl = Sexpr.BiTbl

module SafeArray (F : Libsolver.F) = struct
  (* Some SMT solvers use a default value when they build array models.
     Yet, the default value is not necessarily handled by the 'fold_ax_values'
     function.
     Moreover, the default value conflicts with the lazy initialization of
     large segments of memory because, either we ignore it and thus, we can
     miss an address that is constrained but share the same value than default
     one, either we will have to check for all initialized addresses -- that
     is what lazy initialization tries to avoid.

     This module proposes an alternative way to query the array models based
     on the one used in 'Smt2_solver'.
  *)

  module Open () : Solver.S = struct
    module Solver = F ()

    type access = Select of Expr.t * int | Store of Expr.t * int

    let ax_root : Memory.t AxTbl.t = AxTbl.create 64
    let ordered_mem : access Queue.t AxTbl.t = AxTbl.create 4

    module Hook = struct
      let visit_load
          (Load { len; addr; label; _ } : ([ `Mem ], string, Memory.t) Term.t) =
        let root = AxTbl.find ax_root label in
        let ordered_mem = AxTbl.find ordered_mem root in
        Queue.push (Select (addr, len)) ordered_mem

      let visit_ax (ax : Memory.t) =
        match ax with
        | Root | Symbol _ ->
            AxTbl.add ax_root ax ax;
            AxTbl.add ordered_mem ax (Queue.create ())
        | Layer { addr; store; over; _ } ->
            let root = AxTbl.find ax_root over in
            AxTbl.add ax_root ax root;
            let ordered_mem = AxTbl.find ordered_mem root in
            Store.iter_term
              (fun i bv ->
                Queue.push
                  (Store (Expr.addz addr i, Expr.sizeof bv lsr 3))
                  ordered_mem)
              store
    end

    module Context = Context (Hook) (Solver)

    let visit_formula _ = ()
    let iter_free_variables = Context.iter_free_variables
    let iter_free_arrays = Context.iter_free_arrays
    let assert_formula bl = Solver.assert_formula (Context.visit_bl bl)
    let check_sat = Solver.check_sat

    let check_sat_assuming ?timeout bl =
      Solver.check_sat_assuming ?timeout (Context.visit_bl bl)

    let get_value bv = Solver.get_bv_value (Context.visit_bv bv)

    let fold_array_values f ar x =
      match AxTbl.find ordered_mem ar with
      | exception Not_found -> x
      | history ->
          if Queue.is_empty history then x
          else
            let dirty = BiTbl.create (Queue.length history) in
            Queue.fold
              (fun x access ->
                match access with
                | Select (index, len) ->
                    let index = get_value index in
                    let rec fold index len x =
                      if len = 0 then x
                      else if BiTbl.mem dirty index then
                        fold (Z.succ index) (len - 1) x
                      else
                        let k =
                          get_value
                            (Expr.load 1 LittleEndian
                               (Expr.constant
                                  (Bitvector.create index Context.addr_space))
                               ar)
                        in
                        fold (Z.succ index) (len - 1) (f index k x)
                    in
                    fold index len x
                | Store (index, len) ->
                    let index = get_value index in
                    let rec loop index len =
                      if len <> 0 then (
                        BiTbl.replace dirty index ();
                        loop (Z.succ index) (len - 1))
                    in
                    loop index len;
                    x)
              x history

    let push = Solver.push
    let pop = Solver.pop
    let close = Solver.close
  end
end
