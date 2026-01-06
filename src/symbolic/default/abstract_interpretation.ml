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

open Types

module type CONTEXT = sig
  type 'a t
  (** context *)

  val domain : 'a t -> (module Domains.S with type t = 'a)
  val add_dependency : 'a t -> parent:Expr.t -> Expr.t -> unit
  val find_dependency : 'a t -> Expr.t -> BvSet.t
  val add_value : 'a t -> Expr.t -> 'a -> unit
  val find_value : 'a t -> Expr.t -> 'a
end

module type S = sig
  type 'a t

  val domain : 'a t -> (module Domains.S with type t = 'a)
  val eval : 'a t -> Expr.t -> 'a
  val refine : 'a t -> Expr.t -> 'a -> unit
end

let unary :
    type a.
    (module Domains.S with type t = a) -> unary operator -> size:int -> a -> a =
 fun domain f ~size x ->
  let open (val domain) in
  match f with
  | Not -> lognot ~size x
  | Minus -> uminus ~size x
  | Sext n -> sext n ~size x
  | Uext n -> uext n ~size x
  | Restrict { hi; lo } -> restrict ~hi ~lo ~size x

and binary :
    type a.
    (module Domains.S with type t = a) ->
    binary operator ->
    size:int ->
    a ->
    a ->
    a =
 fun domain f ->
  let open (val domain) in
  match f with
  | Plus -> add
  | Minus -> sub
  | Mul -> mul
  | Udiv -> udiv
  | Urem -> urem
  | Sdiv -> sdiv
  | Srem -> srem
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

and unary_feedback :
    type a.
    (module Domains.S with type t = a) ->
    unary operator ->
    size:int ->
    a ->
    a ->
    a =
 fun domain f ~size x d ->
  let open (val domain) in
  match f with
  | Not -> lognot_feedback ~size x d
  | Minus -> uminus_feedback ~size x d
  | Sext n -> sext_feedback ~size n x d
  | Uext n -> uext_feedback ~size n x d
  | Restrict { hi; lo } -> restrict_feedback ~size ~hi ~lo x d

and binary_feedback :
    type a.
    (module Domains.S with type t = a) ->
    binary operator ->
    size:int ->
    a ->
    a ->
    a ->
    a * a =
 fun domain f ->
  let open (val domain) in
  match f with
  | Plus -> add_feedback
  | Minus -> sub_feedback
  | Mul -> mul_feedback
  | Udiv -> udiv_feedback
  | Urem -> urem_feedback
  | Sdiv -> sdiv_feedback
  | Srem -> srem_feedback
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

module Make (C : CONTEXT) : S with type 'a t := 'a C.t = struct
  let domain = C.domain

  let eval : type a. a C.t -> Expr.t -> a =
    let rec eval :
        type a. a C.t -> Expr.t -> (module Domains.S with type t = a) -> a =
     fun ctx e domain ->
      try C.find_value ctx e
      with Not_found ->
        let open (val domain) in
        let d =
          match e with
          | Cst bv ->
              constant ~size:(Bitvector.size_of bv) (Bitvector.value_of bv)
          | Var { size; _ } -> top size
          | Load { len; _ } -> top (len lsl 3)
          | Unary { f; x; _ } ->
              C.add_dependency ctx x ~parent:e;
              unary domain f ~size:(Expr.sizeof x) (eval ctx x domain)
          | Binary { f = Concat; x; y; _ } ->
              C.add_dependency ctx x ~parent:e;
              C.add_dependency ctx y ~parent:e;
              append ~size1:(Expr.sizeof x) (eval ctx x domain)
                ~size2:(Expr.sizeof y) (eval ctx y domain)
          | Binary { f; x; y; _ } ->
              C.add_dependency ctx x ~parent:e;
              C.add_dependency ctx y ~parent:e;
              binary domain f ~size:(Expr.sizeof x) (eval ctx x domain)
                (eval ctx y domain)
          | Ite { c; t; e = r; _ } -> (
              C.add_dependency ctx c ~parent:e;
              C.add_dependency ctx t ~parent:e;
              C.add_dependency ctx r ~parent:e;
              let c' = eval ctx c domain in
              match is_zero c' with
              | False -> eval ctx t domain
              | True -> eval ctx r domain
              | Unknown ->
                  union ~size:(Expr.sizeof t) (eval ctx t domain)
                    (eval ctx r domain))
        in
        C.add_value ctx e d;
        d
    in
    fun ctx e ->
      try C.find_value ctx e with Not_found -> eval ctx e (C.domain ctx)

  let refine : type a. a C.t -> Expr.t -> a -> unit =
    let rec loop_up :
        type a.
        a C.t ->
        BvSet.t ->
        BvSet.t ->
        (module Domains.S with type t = a) ->
        unit =
     fun ctx todo locked domain ->
      if BvSet.is_empty todo then ()
      else
        let open (val domain) in
        let e = BvSet.choose todo in
        let todo = BvSet.remove e todo in
        if BvSet.mem e locked then loop_up ctx todo locked domain
        else
          (* let locked = BvSet.add e locked in *)
          let o = eval ctx e in
          let n =
            match e with
            | Cst _ | Var _ | Load _ -> assert false
            | Unary { f; x; _ } ->
                unary domain f ~size:(Expr.sizeof x) (eval ctx x)
            | Binary { f = Concat; x; y; _ } ->
                append ~size1:(Expr.sizeof x) (eval ctx x)
                  ~size2:(Expr.sizeof y) (eval ctx y)
            | Binary { f; x; y; _ } ->
                binary domain f ~size:(Expr.sizeof x) (eval ctx x) (eval ctx y)
            | Ite { c; t; e = r; _ } -> (
                let c' = eval ctx c in
                match is_zero c' with
                | False -> eval ctx t
                | True -> eval ctx r
                | Unknown ->
                    union ~size:(Expr.sizeof t) (eval ctx t) (eval ctx r))
          in
          if included ~size:(Expr.sizeof e) o n then
            loop_up ctx todo locked domain
          else
            let todo =
              try BvSet.union (C.find_dependency ctx e) todo
              with Not_found -> todo
            in
            C.add_value ctx e n;
            loop_up ctx todo locked domain
    in
    let rec loop_down :
        type a.
        a C.t ->
        (Expr.t * a) Queue.t ->
        BvSet.t ->
        BvSet.t ->
        (module Domains.S with type t = a) ->
        unit =
     fun ctx todo dirty locked domain ->
      if Queue.is_empty todo then loop_up ctx dirty locked domain
      else
        let open (val domain) in
        let e, d = Queue.pop todo in
        let size = Expr.sizeof e in
        let o = eval ctx e in
        if included ~size o d then loop_down ctx todo dirty locked domain
        else
          let n = inter ~size o d in
          let locked = BvSet.add e locked in
          C.add_value ctx e n;
          let dirty =
            try BvSet.union (C.find_dependency ctx e) dirty
            with Not_found -> dirty
          in
          (match e with
          | Cst _ | Var _ | Load _ -> ()
          | Unary { f; x; _ } ->
              let x' =
                unary_feedback domain f ~size:(Expr.sizeof x) (eval ctx x) n
              in
              Queue.add (x, x') todo
          | Binary { f = Concat; x; y; _ } ->
              let x', y' =
                append_feedback ~size1:(Expr.sizeof x) (eval ctx x)
                  ~size2:(Expr.sizeof y) (eval ctx y) n
              in
              Queue.add (x, x') todo;
              Queue.add (y, y') todo
          | Binary { f; x; y; _ } ->
              let x', y' =
                binary_feedback domain f ~size:(Expr.sizeof x) (eval ctx x)
                  (eval ctx y) n
              in
              Queue.add (x, x') todo;
              Queue.add (y, y') todo
          | Ite { c; t; e; _ } -> (
              let c' = eval ctx c in
              match is_zero c' with
              | False -> Queue.add (t, n) todo
              | True -> Queue.add (e, n) todo
              | Unknown ->
                  if disjoint ~size (eval ctx t) n then Queue.add (c, zero) todo
                  else if disjoint ~size (eval ctx e) n then
                    Queue.add (c, one) todo));
          loop_down ctx todo dirty locked domain
    in
    fun ctx e d ->
      let todo = Queue.create () in
      Queue.add (e, d) todo;
      loop_down ctx todo BvSet.empty BvSet.empty (C.domain ctx)
end
