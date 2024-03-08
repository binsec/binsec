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

module type CONTEXT = sig
  type t
  (** context *)

  type v
  (** domain abstract value *)

  val add_dependency : t -> parent:Expr.t -> Expr.t -> unit
  val find_dependency : t -> Expr.t -> BvSet.t
  val add : t -> Expr.t -> v -> unit
  val find : t -> Expr.t -> v
end

module type S = sig
  type t
  type v

  val eval : t -> Expr.t -> v
  val refine : t -> Expr.t -> v -> unit
end

module Make (D : Domains.S) (C : CONTEXT with type v := D.t) :
  S with type t = C.t and type v := D.t = struct
  type t = C.t

  open D

  let unary (f : Term.unary Term.operator) ~size x =
    match f with
    | Not -> lognot ~size x
    | Minus -> uminus ~size x
    | Sext n -> sext n ~size x
    | Uext n -> uext n ~size x
    | Restrict { hi; lo } -> restrict ~hi ~lo ~size x

  let binary (f : Term.binary Term.operator) =
    match f with
    | Plus -> add
    | Minus -> sub
    | Mul -> mul
    | Udiv -> udiv
    | Umod -> umod
    | Sdiv -> sdiv
    | Smod -> smod
    | Or -> logor
    | And -> logand
    | Xor -> logxor
    | Eq -> equal
    | Diff -> diff
    | Ule -> ule
    | Ult -> ult
    | Uge -> uge
    | Ugt -> ugt
    | Sle -> sle
    | Slt -> slt
    | Sge -> sge
    | Sgt -> sgt
    | Lsl -> shift_left
    | Lsr -> shift_right
    | Asr -> shift_right_signed
    | Rol -> rotate_left
    | Ror -> rotate_right
    | Concat -> assert false

  let rec eval ctx (e : Expr.t) =
    try C.find ctx e
    with Not_found ->
      let d =
        match e with
        | Cst bv ->
            constant ~size:(Bitvector.size_of bv) (Bitvector.value_of bv)
        | Var { size; _ } -> top size
        | Load { len; _ } -> top (len lsl 3)
        | Unary { f; x; _ } ->
            C.add_dependency ctx x ~parent:e;
            unary f ~size:(Expr.sizeof x) (eval ctx x)
        | Binary { f = Concat; x; y; _ } ->
            C.add_dependency ctx x ~parent:e;
            C.add_dependency ctx y ~parent:e;
            append ~size1:(Expr.sizeof x) (eval ctx x) ~size2:(Expr.sizeof y)
              (eval ctx y)
        | Binary { f; x; y; _ } ->
            C.add_dependency ctx x ~parent:e;
            C.add_dependency ctx y ~parent:e;
            (binary f) ~size:(Expr.sizeof x) (eval ctx x) (eval ctx y)
        | Ite { c; t; e = r; _ } ->
            C.add_dependency ctx c ~parent:e;
            C.add_dependency ctx t ~parent:e;
            C.add_dependency ctx r ~parent:e;
            let c' = eval ctx c in
            if included ~size:1 c' one then eval ctx t
            else if included ~size:1 c' zero then eval ctx r
            else union ~size:(Expr.sizeof t) (eval ctx t) (eval ctx r)
      in
      C.add ctx e d;
      d

  let unary_feedback (f : Term.unary Term.operator) x d =
    match f with
    | Not -> lognot_feedback x d
    | Minus -> uminus_feedback x d
    | Sext n -> sext_feedback n x d
    | Uext n -> uext_feedback n x d
    | Restrict { hi; lo } -> restrict_feedback ~hi ~lo x d

  let binary_feedback (f : Term.binary Term.operator) =
    match f with
    | Plus -> add_feedback
    | Minus -> sub_feedback
    | Mul -> mul_feedback
    | Udiv -> udiv_feedback
    | Umod -> umod_feedback
    | Sdiv -> sdiv_feedback
    | Smod -> smod_feedback
    | Or -> logor_feedback
    | And -> logand_feedback
    | Xor -> logxor_feedback
    | Eq -> equal_feedback
    | Diff -> diff_feedback
    | Ule -> ule_feedback
    | Ult -> ult_feedback
    | Uge -> uge_feedback
    | Ugt -> ugt_feedback
    | Sle -> sle_feedback
    | Slt -> slt_feedback
    | Sge -> sge_feedback
    | Sgt -> sgt_feedback
    | Lsl -> shift_left_feedback
    | Lsr -> shift_right_feedback
    | Asr -> shift_right_signed_feedback
    | Rol -> rotate_left_feedback
    | Ror -> rotate_right_feedback
    | Concat -> assert false

  let refine =
    let rec loop_up todo ctx locked =
      if BvSet.is_empty todo then ()
      else
        let e = BvSet.choose todo in
        let todo = BvSet.remove e todo in
        if BvSet.mem e locked then loop_up todo ctx locked
        else
          (* let locked = BvSet.add e locked in *)
          let o = C.find ctx e in
          let n =
            match e with
            | Cst _ | Var _ | Load _ -> assert false
            | Unary { f; x; _ } -> unary f ~size:(Expr.sizeof x) (C.find ctx x)
            | Binary { f = Concat; x; y; _ } ->
                append ~size1:(Expr.sizeof x) (C.find ctx x)
                  ~size2:(Expr.sizeof y) (C.find ctx y)
            | Binary { f; x; y; _ } ->
                binary f ~size:(Expr.sizeof x) (C.find ctx x) (C.find ctx y)
            | Ite { c; t; e = r; _ } ->
                let c' = C.find ctx c in
                if included ~size:1 c' one then C.find ctx t
                else if included ~size:1 c' zero then C.find ctx r
                else union ~size:(Expr.sizeof t) (C.find ctx t) (C.find ctx r)
          in
          if included ~size:(Expr.sizeof e) o n then loop_up todo ctx locked
          else
            let todo =
              try BvSet.union (C.find_dependency ctx e) todo
              with Not_found -> todo
            in
            C.add ctx e n;
            loop_up todo ctx locked
    in
    let rec loop_down todo ctx dirty locked =
      if Queue.is_empty todo then loop_up dirty ctx locked
      else
        let (e : Expr.t), (d : t) = Queue.pop todo in
        let size = Expr.sizeof e in
        let o = C.find ctx e in
        if included ~size o d then loop_down todo ctx dirty locked
        else
          let n = inter ~size o d in
          let locked = BvSet.add e locked in
          C.add ctx e n;
          let dirty =
            try BvSet.union (C.find_dependency ctx e) dirty
            with Not_found -> dirty
          in
          (match e with
          | Cst _ | Var _ | Load _ -> ()
          | Unary { f; x; _ } ->
              let x' =
                unary_feedback f ~size:(Expr.sizeof x) (C.find ctx x) n
              in
              Queue.add (x, x') todo
          | Binary { f = Concat; x; y; _ } ->
              let x', y' =
                append_feedback ~size1:(Expr.sizeof x) (C.find ctx x)
                  ~size2:(Expr.sizeof y) (C.find ctx y) n
              in
              Queue.add (x, x') todo;
              Queue.add (y, y') todo
          | Binary { f; x; y; _ } ->
              let x', y' =
                (binary_feedback f) ~size:(Expr.sizeof x) (C.find ctx x)
                  (C.find ctx y) n
              in
              Queue.add (x, x') todo;
              Queue.add (y, y') todo
          | Ite { c; t; e; _ } ->
              let c' = C.find ctx c in
              if included ~size:1 c' one then Queue.add (t, n) todo
              else if included ~size:1 c' zero then Queue.add (e, n) todo
              else if disjoint ~size (C.find ctx t) n then
                Queue.add (c, zero) todo
              else if disjoint ~size (C.find ctx e) n then
                Queue.add (c, one) todo);
          loop_down todo ctx dirty locked
    in
    fun ctx e d ->
      let todo = Queue.create () in
      Queue.add (e, d) todo;
      loop_down todo ctx BvSet.empty BvSet.empty
end
