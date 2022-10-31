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

module S = Basic_types.String
module I = Basic_types.Int

module A = struct
  type t = Root | Symbol of string

  let equal t t' =
    match (t, t') with
    | Root, Root -> true
    | Symbol name, Symbol name' -> String.equal name name'
    | (Root | Symbol _), (Root | Symbol _) -> false

  let compare t t' =
    match (t, t') with
    | Root, Root -> 0
    | Symbol name, Symbol name' -> String.compare name name'
    | Root, Symbol _ -> -1
    | Symbol _, Root -> 1

  let hash = function Root -> 129913994 | Symbol name -> Hashtbl.hash name

  let default = Root
end

module Expr =
  Term.Make
    (struct
      type t = int

      let equal = ( == )

      let compare = ( - )

      let hash = Fun.id
    end)
    (A)

module Var = struct
  type t = ([ `Var ], int, A.t) Term.t
end

module Action = struct
  type format = Bin | Dec | Hex | Ascii

  type t =
    | Print_formula of (Expr.t * string) list option
    | Print_model
    | Print_value of format * Expr.t
    | Print_stream of string
end

module Output = struct
  type t =
    | Model
    | Formula
    | Slice of (Expr.t * string) list
    | Value of (Action.format * Expr.t)
    | Stream of string
    | String of string
end

(* micro-operation *)
module Uop = struct
  type t =
    | Symbol of Var.t
    | Define of { var : Var.t; rval : Expr.t }
    | Forget of Var.t
    | Store of {
        base : A.t;
        dir : Machine.endianness;
        addr : Expr.t;
        rval : Expr.t;
      }
    | Assume of Expr.t
    | Assert of Expr.t
    | Print of Output.t
    | Enumerate of {
        enum : Expr.t;
        id : int;
        format : Action.format;
        n : int;
        mutable k : int;
        mutable values : Bitvector.t list;
      }
end

type _ t =
  | Hook : {
      addr : Virtual_address.t;
      info : string;
      mutable succ : [ `All ] t;
    }
      -> [< `Label | `Pred | `All ] t
  | Exec : {
      addr : Virtual_address.t;
      info : string;
      mutable succ : [ `All ] t;
    }
      -> [< `Label | `Pred | `All ] t
  | Step : { uop : Uop.t; mutable succ : [ `All ] t } -> [< `Pred | `All ] t
  | Branch : {
      test : Expr.t;
      mutable taken : [ `All ] t;
      mutable fallthrough : [ `All ] t;
    }
      -> [< `Pred | `All ] t
  | Goto : {
      addr : Virtual_address.t;
      mutable preds : [ `Pred ] t list;
    }
      -> [ `All ] t
  | Jump : Expr.t -> [ `All ] t
  | Reach : {
      id : int;
      mutable n : int;
      guard : Expr.t;
      mutable actions : [ `All ] t;
      mutable succ : [ `All ] t;
    }
      -> [ `All ] t
  | Cut : [ `All ] t
  | Halt : [ `All ] t
  | Die : string -> [ `All ] t

let addr (t : [ `Label ] t) =
  match t with Hook { addr; _ } | Exec { addr; _ } -> addr

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

  let var ((ctx : Var.t S.Htbl.t), _, rev) name size : Var.t =
    try
      let (Var { size = size'; _ } as var) = S.Htbl.find ctx name in
      if size' = size then var
      else
        List.find
          (fun (Var { size = size'; _ } : Var.t) -> size' = size)
          (S.Htbl.find_all ctx name)
    with Not_found ->
      let id = S.Htbl.length ctx in
      let var = Term.to_var_exn (Expr.var name size id) in
      S.Htbl.add ctx name var;
      I.Htbl.add rev id name;
      var

  let evar ctx name size = Term.to_exp (var ctx name size)

  let rec expr ctx (e : Dba.Expr.t) =
    match e with
    | Var { info = Symbol (_, (lazy bv)); _ } | Dba.Expr.Cst bv ->
        Expr.constant bv
    | Var { name; size; _ } -> evar ctx name size
    | Load (bytes, endianness, addr) ->
        Expr.load bytes endianness (expr ctx addr) A.default
    | Unary (uop, e) -> Expr.unary (unary e uop) (expr ctx e)
    | Binary (bop, lop, rop) ->
        Expr.binary (binary bop) (expr ctx lop) (expr ctx rop)
    | Ite (c, then_e, else_e) ->
        Expr.ite (expr ctx c) (expr ctx then_e) (expr ctx else_e)

  let complement e ~lo ~hi var =
    let size = Expr.sizeof var in
    if lo = 0 then Expr.append (Expr.restrict ~lo:(hi + 1) ~hi:(size - 1) var) e
    else if hi = size - 1 then
      Expr.append e (Expr.restrict ~lo:0 ~hi:(lo - 1) var)
    else
      Expr.append
        (Expr.append (Expr.restrict ~lo:(hi + 1) ~hi:(size - 1) var) e)
        (Expr.restrict ~lo:0 ~hi:(lo - 1) var)

  let array (_, ctx, _) name =
    try S.Htbl.find ctx name
    with Not_found ->
      let ar = A.Symbol name in
      S.Htbl.add ctx name ar;
      ar

  let expr' ctx e =
    Expr.map
      (fun name size (label : Dba.VarTag.t) ->
        match label with
        | Symbol (_, (lazy bv)) -> Expr.constant bv
        | Flag | Temp | Register | Empty -> evar ctx name size)
      (fun len dir addr label -> Expr.load len dir addr (array ctx label))
      e
end

let assign ctx (loc : Dba.LValue.t) (value : Dba.Expr.t) succ =
  match loc with
  | Var { name; size; _ } ->
      let var = Translate.var ctx name size
      and rval = Translate.expr ctx value in
      Step { uop = Define { var; rval }; succ }
  | Restrict ({ name; size; _ }, { lo; hi }) ->
      let var = Translate.var ctx name size
      and rval =
        Translate.(complement (expr ctx value) ~hi ~lo (evar ctx name size))
      in
      Step { uop = Define { var; rval }; succ }
  | Store (_, dir, addr) ->
      let base = A.default
      and addr = Translate.expr ctx addr
      and rval = Translate.expr ctx value in
      Step { uop = Store { base; dir; addr; rval }; succ }

let nondet =
  let entropy = Printf.sprintf "%%entropy%%%d" in
  fun ctx (loc : Dba.LValue.t) succ ->
    match loc with
    | Var { name; size; _ } ->
        Step { uop = Symbol (Translate.var ctx name size); succ }
    | Restrict ({ name; size; _ }, { lo; hi }) ->
        let size' = hi - lo + 1 in
        let name' = entropy size' in
        let var' = Translate.var ctx name' size' in
        let var = Translate.var ctx name size in
        let rval =
          Translate.complement (Term.to_exp var') ~lo ~hi (Term.to_exp var)
        in
        Step
          {
            uop = Symbol var';
            succ = Step { uop = Define { var; rval }; succ };
          }
    | Store (bytes, dir, addr) ->
        let size' = 8 * bytes in
        let name' = entropy size' in
        let var' = Translate.var ctx name' size' in
        let rval = Term.to_exp var' in
        let base = A.default and addr = Translate.expr ctx addr in
        Step
          {
            uop = Symbol var';
            succ = Step { uop = Store { base; dir; addr; rval }; succ };
          }

let of_dhunk : Var.t S.Htbl.t * A.t S.Htbl.t * string I.Htbl.t -> Dhunk.t -> _ t
    =
  let rec lookup d a i =
    match Array.get a i with
    | Halt -> (
        match Dhunk.inst_exn d i with
        | SJump (JInner i, _) -> lookup d a i
        | _ -> Halt)
    | t -> t
  in
  fun ctx d ->
    let n = Dhunk.length d in
    let a = Array.make n Halt in
    for i = 0 to n - 1 do
      Array.set a i
        (match Dhunk.inst_exn d i with
        | Assign (loc, value, _) -> assign ctx loc value Halt
        | Undef (Var { name; size; _ }, _) ->
            Step { uop = Forget Translate.(var ctx name size); succ = Halt }
        | Nondet (loc, _, _) -> nondet ctx loc Halt
        | Assume (test, _) ->
            Step { uop = Assume (Translate.expr ctx test); succ = Halt }
        | Assert (test, _) ->
            Step { uop = Assert (Translate.expr ctx test); succ = Halt }
        | If (test, JInner _, _) ->
            Branch
              {
                test = Translate.expr ctx test;
                taken = Halt;
                fallthrough = Halt;
              }
        | If (test, JOuter { base; _ }, _) ->
            Branch
              {
                test = Translate.expr ctx test;
                taken = Goto { addr = base; preds = [] };
                fallthrough = Halt;
              }
        | DJump (target, _) -> Jump (Translate.expr ctx target)
        | SJump (JOuter { base; _ }, _) -> Goto { addr = base; preds = [] }
        | SJump (JInner _, _) -> Halt
        | Stop (None | Some OK) -> Halt
        | Stop (Some (Undecoded msg | Unsupported msg)) -> Die msg
        | ( Undef _ | NondetAssume _ | Malloc _ | Free _ | Print _
          | Stop (Some KO) ) as ins ->
            Options.Logger.fatal "unexpected instruction kind %a"
              Dba_printer.Ascii.pp_instruction ins)
    done;
    for i = 0 to n - 1 do
      match (Dhunk.inst_exn d i, Array.get a i) with
      | Assign (_, _, i'), Step t -> t.succ <- lookup d a i'
      | Undef (_, i'), Step t -> t.succ <- lookup d a i'
      | Nondet (Var _, _, i'), Step t -> t.succ <- lookup d a i'
      | Nondet (Restrict _, _, i'), Step { succ = Step t; _ } ->
          t.succ <- lookup d a i'
      | Nondet (Store _, _, i'), Step { succ = Step t; _ } ->
          t.succ <- lookup d a i'
      | Assume (_, i'), Step t -> t.succ <- lookup d a i'
      | Assert (_, i'), Step t -> t.succ <- lookup d a i'
      | If (_, JInner i', f'), Branch t ->
          t.taken <- lookup d a i';
          t.fallthrough <- lookup d a f'
      | If (_, JOuter _, i'), Branch t -> t.fallthrough <- lookup d a i'
      | SJump (JOuter _, _), Goto t ->
          t.preds <-
            List.fold_left
              (fun preds i' ->
                match Array.get a i' with
                | (Hook _ | Exec _ | Step _ | Branch _) as p -> p :: preds
                | Goto _ | Jump _ | Reach _ | Cut | Halt | Die _ -> assert false)
              [] (Dhunk.pred d i)
      | DJump _, _ | SJump _, _ | Stop _, _ -> ()
      | _ -> assert false
    done;
    lookup d a 0
