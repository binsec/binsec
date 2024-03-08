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

module type S = sig
  type 'a t

  val empty : 'a t
  val length : 'a t -> int
  val push_front : 'a -> 'a t -> 'a t
  val push_back : 'a -> 'a t -> 'a t
  val peek_front : 'a t -> 'a
  val peek_back : 'a t -> 'a
  val pop_front : 'a t -> 'a t
  val pop_back : 'a t -> 'a t
  val map_forward : ('a -> 'b) -> 'a t -> 'b t
  val map_backward : ('a -> 'b) -> 'a t -> 'b t
  val iter_forward : ('a -> unit) -> 'a t -> unit
  val iter_backward : ('a -> unit) -> 'a t -> unit
  val fold_forward : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val fold_backward : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val to_seq_forward : 'a t -> 'a Seq.t
  val to_seq_backward : 'a t -> 'a Seq.t
end

(** concatenates a list of Seq.t *)
let rec and_then : 'a. 'a Seq.t list -> 'a Seq.t =
 fun l () ->
  let open Seq in
  match l with
  | [] -> Nil
  | [ x ] -> x ()
  | x :: tl -> (
      match x () with
      | Nil -> (and_then tl) ()
      | Cons (a, next) -> Cons (a, and_then (next :: tl)))

(** concatenates a Seq.t of Seq.t *)
let rec flatten_seq : 'a. 'a Seq.t Seq.t -> 'a Seq.t =
 fun s () ->
  let open Seq in
  match s () with
  | Nil -> Nil
  | Cons (x, tl) -> (
      match x () with
      | Nil -> (flatten_seq tl) ()
      | Cons (a, next) -> Cons (a, flatten_seq (fun () -> Cons (next, tl))))

module D : S = struct
  type 'a digit = Zero | One of 'a | Two of 'a * 'a | Three of 'a * 'a * 'a

  type 'a queue =
    | Shallow of 'a digit
    | Deep of 'a digit * ('a * 'a) queue * 'a digit

  type 'a t = { length : int; queue : 'a queue }

  let qempty = Shallow Zero

  let dpush_front x = function
    | Zero -> One x
    | One a -> Two (x, a)
    | Two (a, b) -> Three (x, a, b)
    | Three _ -> assert false

  let dpush_back x = function
    | Zero -> One x
    | One a -> Two (a, x)
    | Two (a, b) -> Three (a, b, x)
    | Three _ -> assert false

  let dpeek_front = function
    | Zero -> raise Not_found
    | One a -> a
    | Two (a, _) -> a
    | Three (a, _, _) -> a

  let dpeek_back = function
    | Zero -> raise Not_found
    | One a -> a
    | Two (_, a) -> a
    | Three (_, _, a) -> a

  let dpop_front = function
    | Zero -> raise Not_found
    | One _ -> Zero
    | Two (_, a) -> One a
    | Three (_, a, b) -> Two (a, b)

  let dpop_back = function
    | Zero -> raise Not_found
    | One _ -> Zero
    | Two (a, _) -> One a
    | Three (a, b, _) -> Two (a, b)

  let dmap_forward f = function
    | Zero -> Zero
    | One a ->
        let a = f a in
        One a
    | Two (a, b) ->
        let b = f b in
        let a = f a in
        Two (a, b)
    | Three (a, b, c) ->
        let c = f c in
        let b = f b in
        let a = f a in
        Three (a, b, c)

  let dmap_backward f = function
    | Zero -> Zero
    | One a ->
        let a = f a in
        One a
    | Two (a, b) ->
        let a = f a in
        let b = f b in
        Two (a, b)
    | Three (a, b, c) ->
        let a = f a in
        let b = f b in
        let c = f c in
        Three (a, b, c)

  let diter_forward f = function
    | Zero -> ()
    | One a -> f a
    | Two (a, b) ->
        f b;
        f a
    | Three (a, b, c) ->
        f c;
        f b;
        f a

  let diter_backward f = function
    | Zero -> ()
    | One a -> f a
    | Two (a, b) ->
        f a;
        f b
    | Three (a, b, c) ->
        f a;
        f b;
        f c

  let rec dto_seq_forward f =
    let open Seq in
    fun () ->
      match f with
      | Zero -> Nil
      | One a -> Cons (a, dto_seq_forward Zero)
      | Two (a, b) -> Cons (b, dto_seq_forward (One a))
      | Three (a, b, c) -> Cons (c, dto_seq_forward (Two (a, b)))

  let rec dto_seq_backward f =
    let open Seq in
    fun () ->
      match f with
      | Zero -> Nil
      | One a -> Cons (a, dto_seq_backward Zero)
      | Two (a, b) -> Cons (a, dto_seq_backward (One b))
      | Three (a, b, c) -> Cons (a, dto_seq_backward (Two (b, c)))

  let dfold_forward f t acc =
    match t with
    | Zero -> acc
    | One a -> f a acc
    | Two (a, b) -> f a (f b acc)
    | Three (a, b, c) -> f a (f b (f c acc))

  let dfold_backward f t acc =
    match t with
    | Zero -> acc
    | One a -> f a acc
    | Two (a, b) -> f b (f a acc)
    | Three (a, b, c) -> f c (f b (f a acc))

  let rec qpush_front : 'a. 'a -> 'a queue -> 'a queue =
   fun x q ->
    match q with
    | Shallow d -> (
        match d with
        | Zero | One _ | Two _ -> Shallow (dpush_front x d)
        | Three (a, b, c) -> Deep (Two (x, a), qempty, Two (b, c)))
    | Deep (f, m, r) -> (
        match f with
        | Zero | One _ | Two _ -> Deep (dpush_front x f, m, r)
        | Three (a, b, c) -> Deep (Two (x, a), qpush_front (b, c) m, r))

  let rec qpush_back : 'a. 'a -> 'a queue -> 'a queue =
   fun x q ->
    match q with
    | Shallow d -> (
        match d with
        | Zero | One _ | Two _ -> Shallow (dpush_back x d)
        | Three (a, b, c) -> Deep (Two (a, b), qempty, Two (c, x)))
    | Deep (f, m, r) -> (
        match r with
        | Zero | One _ | Two _ -> Deep (f, m, dpush_back x r)
        | Three (a, b, c) -> Deep (f, qpush_back (a, b) m, Two (c, x)))

  let qpeek_front = function
    | Shallow d -> dpeek_front d
    | Deep (f, _, _) -> dpeek_front f

  let qpeek_back = function
    | Shallow d -> dpeek_back d
    | Deep (_, _, r) -> dpeek_back r

  let rec qpop_front : 'a. 'a queue -> 'a queue =
   fun t ->
    match t with
    | Shallow d -> Shallow (dpop_front d)
    | Deep (f, m, r) -> (
        match f with
        | Zero -> assert false
        | One _ ->
            if m = qempty then Shallow r
            else
              let a, b = qpeek_front m in
              Deep (Two (a, b), qpop_front m, r)
        | Two _ | Three _ -> Deep (dpop_front f, m, r))

  let rec qpop_back : 'a. 'a queue -> 'a queue =
   fun t ->
    match t with
    | Shallow d -> Shallow (dpop_back d)
    | Deep (f, m, r) -> (
        match r with
        | Zero -> assert false
        | One _ ->
            if m = qempty then Shallow f
            else
              let a, b = qpeek_back m in
              Deep (f, qpop_back m, Two (a, b))
        | Two _ | Three _ -> Deep (f, m, dpop_back r))

  let rec qmap_forward : 'a 'b. ('a -> 'b) -> 'a queue -> 'b queue =
   fun g t ->
    match t with
    | Shallow d -> Shallow (dmap_forward g d)
    | Deep (f, m, r) ->
        let r = dmap_forward g r in
        let m =
          qmap_forward
            (fun (x, y) ->
              let y = g y in
              let x = g x in
              (x, y))
            m
        in
        let f = dmap_forward g f in
        Deep (f, m, r)

  let rec qmap_backward : 'a 'b. ('a -> 'b) -> 'a queue -> 'b queue =
   fun g t ->
    match t with
    | Shallow d -> Shallow (dmap_backward g d)
    | Deep (f, m, r) ->
        let f = dmap_backward g f in
        let m =
          qmap_backward
            (fun (x, y) ->
              let x = g x in
              let y = g y in
              (x, y))
            m
        in
        let r = dmap_backward g r in
        Deep (f, m, r)

  let rec qiter_forward : 'a. ('a -> unit) -> 'a queue -> unit =
   fun g t ->
    match t with
    | Shallow d -> diter_forward g d
    | Deep (f, m, r) ->
        diter_forward g r;
        qiter_forward
          (fun (x, y) ->
            g y;
            g x)
          m;
        diter_forward g f

  let rec qiter_backward : 'a. ('a -> unit) -> 'a queue -> unit =
   fun g t ->
    match t with
    | Shallow d -> diter_backward g d
    | Deep (f, m, r) ->
        diter_backward g f;
        qiter_backward
          (fun (x, y) ->
            g x;
            g y)
          m;
        diter_backward g r

  let rec qto_seq_forward : type b. b queue -> b Seq.t = function
    | Shallow d -> dto_seq_forward d
    | Deep (f, m, r) ->
        let seq_of_pairs = qto_seq_forward m in
        let seq_of_seqs =
          Seq.map (fun (a, b) () -> Seq.(Cons (b, return a))) seq_of_pairs
        in
        and_then
          [ dto_seq_forward r; flatten_seq seq_of_seqs; dto_seq_forward f ]

  let rec qto_seq_backward : type b. b queue -> b Seq.t = function
    | Shallow d -> dto_seq_backward d
    | Deep (f, m, r) ->
        let seq_of_pairs = qto_seq_backward m in
        let seq_of_seqs =
          Seq.map (fun (a, b) () -> Seq.(Cons (a, return b))) seq_of_pairs
        in
        and_then
          [ dto_seq_backward f; flatten_seq seq_of_seqs; dto_seq_backward r ]

  let rec qfold_forward : 'a 'b. ('a -> 'b -> 'b) -> 'a queue -> 'b -> 'b =
   fun g t acc ->
    match t with
    | Shallow d -> dfold_forward g d acc
    | Deep (f, m, r) ->
        dfold_forward g f
          (qfold_forward
             (fun (x, y) acc -> g x (g y acc))
             m (dfold_forward g r acc))

  let rec qfold_backward : 'a 'b. ('a -> 'b -> 'b) -> 'a queue -> 'b -> 'b =
   fun g t acc ->
    match t with
    | Shallow d -> dfold_backward g d acc
    | Deep (f, m, r) ->
        dfold_backward g r
          (qfold_backward
             (fun (x, y) acc -> g y (g x acc))
             m (dfold_backward g f acc))

  let empty = { length = 0; queue = qempty }
  let length t = t.length
  let push_front x t = { length = t.length + 1; queue = qpush_front x t.queue }
  let push_back x t = { length = t.length + 1; queue = qpush_back x t.queue }
  let peek_front t = qpeek_front t.queue
  let peek_back t = qpeek_back t.queue
  let pop_front t = { length = t.length - 1; queue = qpop_front t.queue }
  let pop_back t = { length = t.length - 1; queue = qpop_back t.queue }
  let map_forward f t = { t with queue = qmap_forward f t.queue }
  let map_backward f t = { t with queue = qmap_backward f t.queue }
  let iter_forward f t = qiter_forward f t.queue
  let iter_backward f t = qiter_backward f t.queue
  let to_seq_forward t = qto_seq_forward t.queue
  let to_seq_backward t = qto_seq_backward t.queue
  let fold_forward f t acc = qfold_forward f t.queue acc
  let fold_backward f t acc = qfold_backward f t.queue acc
end

type 'a compound =
  | Simple of 'a D.t
  | Compound of 'a D.t * 'a compound queue * 'a D.t

and 'a queue =
  | Shallow of 'a D.t
  | Deep of 'a D.t * 'a compound queue * 'a D.t * 'a compound queue * 'a D.t

type 'a t = { length : int; queue : 'a queue }

let qempty = Shallow D.empty

let qpush_front x = function
  | Shallow q -> Shallow (D.push_front x q)
  | Deep (f, a, m, b, r) -> Deep (D.push_front x f, a, m, b, r)

let qpush_back x = function
  | Shallow q -> Shallow (D.push_back x q)
  | Deep (f, a, m, b, r) -> Deep (f, a, m, b, D.push_back x r)

let qpeek_front = function
  | Shallow q -> D.peek_front q
  | Deep (f, _, _, _, _) -> D.peek_front f

let qpeek_back = function
  | Shallow q -> D.peek_back q
  | Deep (_, _, _, _, r) -> D.peek_back r

let share f r =
  ( D.pop_back f,
    D.push_front (D.peek_back f) (D.push_front (D.peek_front r) D.empty),
    D.pop_front r )

let rec append_left t1 t2 =
  if t1 = D.empty then t2
  else append_left (D.pop_back t1) (D.push_front (D.peek_back t1) t2)

let rec append_right t1 t2 =
  if t2 = D.empty then t1
  else append_right (D.push_back (D.peek_front t2) t1) (D.pop_front t2)

let qappend t1 t2 =
  match (t1, t2) with
  | Shallow q1, Shallow q2 ->
      if D.length q1 < 4 then Shallow (append_left q1 q2)
      else if D.length q2 < 4 then Shallow (append_right q1 q2)
      else
        let f, m, r = share q1 q2 in
        Deep (f, qempty, m, qempty, r)
  | Shallow q, Deep (f, a, m, b, r) ->
      if D.length q < 3 then Deep (append_left q f, a, m, b, r)
      else Deep (q, qpush_front (Simple f) a, m, b, r)
  | Deep (f, a, m, b, r), Shallow q ->
      if D.length q < 3 then Deep (f, a, m, b, append_right r q)
      else Deep (f, a, m, qpush_back (Simple r) b, q)
  | Deep (f1, a1, m1, b1, r1), Deep (f2, a2, m2, b2, r2) ->
      let r, m, f = share r1 f2 in
      Deep
        ( f1,
          qpush_back (Compound (m1, b1, r)) a1,
          m,
          qpush_front (Compound (f, a2, m2)) b2,
          r2 )

let replace_front x = function
  | Shallow q -> Shallow (D.push_front x (D.pop_front q))
  | Deep (f, a, m, b, r) -> Deep (D.push_front x (D.pop_front f), a, m, b, r)

let rec qpop_front : 'a. 'a queue -> 'a queue =
 fun t ->
  match t with
  | Shallow q -> Shallow (D.pop_front q)
  | Deep (f, a, m, b, r) ->
      if D.length f > 3 then Deep (D.pop_front f, a, m, b, r)
      else if a <> qempty then
        match qpeek_front a with
        | Simple q -> Deep (append_left (D.pop_front f) q, qpop_front a, m, b, r)
        | Compound (f', a', r') ->
            Deep
              ( append_left (D.pop_front f) f',
                qappend a' (replace_front (Simple r') a),
                m,
                b,
                r )
      else if b <> qempty then
        match qpeek_front b with
        | Simple q ->
            Deep (append_left (D.pop_front f) m, qempty, q, qpop_front b, r)
        | Compound (f', a', r') ->
            Deep
              ( append_left (D.pop_front f) m,
                qpush_front (Simple f') a',
                r',
                qpop_front b,
                r )
      else qappend (Shallow (append_left (D.pop_front f) m)) (Shallow r)

let replace_back x = function
  | Shallow q -> Shallow (D.push_back x (D.pop_back q))
  | Deep (f, a, m, b, r) -> Deep (f, a, m, b, D.push_back x (D.pop_back r))

let rec qpop_back : 'a. 'a queue -> 'a queue =
 fun t ->
  match t with
  | Shallow q -> Shallow (D.pop_back q)
  | Deep (f, a, m, b, r) ->
      if D.length r > 3 then Deep (f, a, m, b, D.pop_back r)
      else if b <> qempty then
        match qpeek_back b with
        | Simple q -> Deep (f, a, m, qpop_back b, append_right q (D.pop_back r))
        | Compound (f', a', r') ->
            Deep
              ( f,
                a,
                m,
                qappend (replace_back (Simple f') b) a',
                append_right r' (D.pop_back r) )
      else if a <> qempty then
        match qpeek_back a with
        | Simple q ->
            Deep (f, qpop_back a, q, qempty, append_right m (D.pop_back r))
        | Compound (f', a', r') ->
            Deep
              ( f,
                qpop_back a,
                f',
                qpush_back (Simple r') a',
                append_right m (D.pop_back r) )
      else qappend (Shallow f) (Shallow (append_right m (D.pop_back r)))

let rec cmap_forward : 'a 'b. ('a -> 'b) -> 'a compound -> 'b compound =
 fun g t ->
  match t with
  | Simple q -> Simple (D.map_forward g q)
  | Compound (f, a, r) ->
      let r = D.map_forward g r in
      let a = qmap_forward (cmap_forward g) a in
      let f = D.map_forward g f in
      Compound (f, a, r)

and qmap_forward : 'a 'b. ('a -> 'b) -> 'a queue -> 'b queue =
 fun g t ->
  match t with
  | Shallow q -> Shallow (D.map_forward g q)
  | Deep (f, a, m, b, r) ->
      let r = D.map_forward g r in
      let b = qmap_forward (cmap_forward g) b in
      let m = D.map_forward g m in
      let a = qmap_forward (cmap_forward g) a in
      let f = D.map_forward g f in
      Deep (f, a, m, b, r)

let rec cmap_backward : 'a 'b. ('a -> 'b) -> 'a compound -> 'b compound =
 fun g t ->
  match t with
  | Simple q -> Simple (D.map_backward g q)
  | Compound (f, a, r) ->
      let f = D.map_backward g f in
      let a = qmap_backward (cmap_backward g) a in
      let r = D.map_backward g r in
      Compound (f, a, r)

and qmap_backward : 'a 'b. ('a -> 'b) -> 'a queue -> 'b queue =
 fun g t ->
  match t with
  | Shallow q -> Shallow (D.map_backward g q)
  | Deep (f, a, m, b, r) ->
      let f = D.map_backward g f in
      let a = qmap_backward (cmap_backward g) a in
      let m = D.map_backward g m in
      let b = qmap_backward (cmap_backward g) b in
      let r = D.map_backward g r in
      Deep (f, a, m, b, r)

let rec citer_forward : 'a. ('a -> unit) -> 'a compound -> unit =
 fun g t ->
  match t with
  | Simple q -> D.iter_forward g q
  | Compound (f, a, r) ->
      D.iter_forward g r;
      qiter_forward (citer_forward g) a;
      D.iter_forward g f

and qiter_forward : 'a. ('a -> unit) -> 'a queue -> unit =
 fun g t ->
  match t with
  | Shallow q -> D.iter_forward g q
  | Deep (f, a, m, b, r) ->
      D.iter_forward g r;
      qiter_forward (citer_forward g) b;
      D.iter_forward g m;
      qiter_forward (citer_forward g) a;
      D.iter_forward g f

let rec citer_backward : 'a. ('a -> unit) -> 'a compound -> unit =
 fun g t ->
  match t with
  | Simple q -> D.iter_backward g q
  | Compound (f, a, r) ->
      D.iter_backward g f;
      qiter_backward (citer_backward g) a;
      D.iter_backward g r

and qiter_backward : 'a. ('a -> unit) -> 'a queue -> unit =
 fun g t ->
  match t with
  | Shallow q -> D.iter_backward g q
  | Deep (f, a, m, b, r) ->
      D.iter_backward g f;
      qiter_backward (citer_backward g) a;
      D.iter_backward g m;
      qiter_backward (citer_backward g) b;
      D.iter_backward g r

let rec cto_seq_forward : type c. c compound -> c Seq.t = function
  | Simple q -> D.to_seq_forward q
  | Compound (f, a, r) ->
      and_then
        [
          D.to_seq_forward r;
          flatten_seq (Seq.map cto_seq_forward (qto_seq_forward a));
          D.to_seq_forward f;
        ]

and qto_seq_forward : type b. b queue -> b Seq.t = function
  | Shallow q -> D.to_seq_forward q
  | Deep (f, a, m, b, r) ->
      and_then
        [
          D.to_seq_forward r;
          flatten_seq (Seq.map cto_seq_forward (qto_seq_forward b));
          D.to_seq_forward m;
          flatten_seq (Seq.map cto_seq_forward (qto_seq_forward a));
          D.to_seq_forward f;
        ]

let rec cto_seq_backward : type c. c compound -> c Seq.t = function
  | Simple q -> D.to_seq_backward q
  | Compound (f, a, r) ->
      and_then
        [
          D.to_seq_backward f;
          flatten_seq (Seq.map cto_seq_backward (qto_seq_backward a));
          D.to_seq_backward r;
        ]

and qto_seq_backward : type b. b queue -> b Seq.t = function
  | Shallow q -> D.to_seq_backward q
  | Deep (f, a, m, b, r) ->
      and_then
        [
          D.to_seq_backward f;
          flatten_seq (Seq.map cto_seq_backward (qto_seq_backward a));
          D.to_seq_backward m;
          flatten_seq (Seq.map cto_seq_backward (qto_seq_backward b));
          D.to_seq_backward r;
        ]

let rec cfold_forward : 'a 'b. ('a -> 'b -> 'b) -> 'a compound -> 'b -> 'b =
 fun g t acc ->
  match t with
  | Simple q -> D.fold_forward g q acc
  | Compound (f, a, r) ->
      D.fold_forward g f
        (qfold_forward (cfold_forward g) a (D.fold_forward g r acc))

and qfold_forward : 'a 'b. ('a -> 'b -> 'b) -> 'a queue -> 'b -> 'b =
 fun g t acc ->
  match t with
  | Shallow q -> D.fold_forward g q acc
  | Deep (f, a, m, b, r) ->
      D.fold_forward g f
        (qfold_forward (cfold_forward g) a
           (D.fold_forward g m
              (qfold_forward (cfold_forward g) b (D.fold_forward g r acc))))

let rec cfold_backward : 'a 'b. ('a -> 'b -> 'b) -> 'a compound -> 'b -> 'b =
 fun g t acc ->
  match t with
  | Simple q -> D.fold_backward g q acc
  | Compound (f, a, r) ->
      D.fold_backward g r
        (qfold_backward (cfold_backward g) a (D.fold_backward g f acc))

and qfold_backward : 'a 'b. ('a -> 'b -> 'b) -> 'a queue -> 'b -> 'b =
 fun g t acc ->
  match t with
  | Shallow q -> D.fold_backward g q acc
  | Deep (f, a, m, b, r) ->
      D.fold_backward g r
        (qfold_backward (cfold_backward g) b
           (D.fold_backward g m
              (qfold_backward (cfold_backward g) a (D.fold_backward g f acc))))

let empty = { length = 0; queue = qempty }
let length t = t.length

let append t1 t2 =
  { length = t1.length + t2.length; queue = qappend t1.queue t2.queue }

let push_front x t = { length = t.length + 1; queue = qpush_front x t.queue }
let push_back x t = { length = t.length + 1; queue = qpush_back x t.queue }
let peek_front t = try Some (qpeek_front t.queue) with Not_found -> None
let peek_back t = try Some (qpeek_back t.queue) with Not_found -> None

let pop_front t =
  try Some { length = t.length - 1; queue = qpop_front t.queue }
  with Not_found -> None

let pop_back t =
  try Some { length = t.length - 1; queue = qpop_back t.queue }
  with Not_found -> None

let map_forward f t = { t with queue = qmap_forward f t.queue }
let map_backward f t = { t with queue = qmap_backward f t.queue }
let iter_forward f t = qiter_forward f t.queue
let iter_backward f t = qiter_backward f t.queue
let to_seq_forward t = qto_seq_forward t.queue
let to_seq_backward t = qto_seq_backward t.queue
let fold_forward f t acc = qfold_forward f t.queue acc
let fold_backward f t acc = qfold_backward f t.queue acc
