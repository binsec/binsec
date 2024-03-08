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

type 'a t = 'a Basic_types.interval = { lo : 'a; hi : 'a }

let belongs cmp x t = cmp x t.lo >= 0 && cmp x t.hi <= 0
let intersects cmp t s = not (cmp t.hi s.lo < 0 || cmp t.lo s.hi > 0)

module type S = sig
  type point
  type interval
  type t

  val empty : t
  val singleton : interval -> t
  val add : interval -> t -> t
  val remove : interval -> t -> t
  val is_empty : t -> bool
  val cardinal : t -> int
  val mem : interval -> t -> bool
  val min : t -> point option
  val max : t -> point option
  val is_point : t -> point option
  val is_interval : t -> interval option
  val belongs : point -> t -> interval list
  val intersects : interval -> t -> interval list
  val map : (interval -> interval) -> t -> t
  val iter : (interval -> unit) -> t -> unit
  val fold : (interval -> 'a -> 'a) -> t -> 'a -> 'a
  val union : t -> t -> t
  val inter : t -> t -> t
  val print : (point -> string) -> t -> string
end

module type T = sig
  type point
  type interval

  type t = private
    | Empty
    | Black of int * t * interval * t * interval
    | Red of int * t * interval * t * interval

  val empty : t
  val singleton : interval -> t
  val add : interval -> t -> t
  val remove : interval -> t -> t
  val cardinal : t -> int
end

module Base (Ord : Sigs.COMPARABLE) :
  T with type point = Ord.t and type interval = Ord.t t = struct
  type point = Ord.t
  type interval = Ord.t t

  type t =
    | Empty
    | Black of int * t * interval * t * interval
    | Red of int * t * interval * t * interval

  let empty = Empty

  let cardinal = function
    | Empty -> 0
    | Black (i, _, _, _, _) | Red (i, _, _, _, _) -> i

  let extend x y =
    {
      lo = (if Ord.compare x.lo y.lo > 0 then y.lo else x.lo);
      hi = (if Ord.compare x.hi y.hi < 0 then y.hi else x.hi);
    }

  let black l v r =
    let i, ml =
      match l with
      | Empty -> (0, v)
      | Black (i, _, _, _, m) | Red (i, _, _, _, m) -> (i, m)
    in
    let j, mr =
      match r with
      | Empty -> (0, v)
      | Black (j, _, _, _, m) | Red (j, _, _, _, m) -> (j, m)
    in
    Black (i + j + 1, l, v, r, extend v (extend ml mr))

  let red l v r =
    let i, ml =
      match l with
      | Empty -> (0, v)
      | Black (i, _, _, _, m) | Red (i, _, _, _, m) -> (i, m)
    in
    let j, mr =
      match r with
      | Empty -> (0, v)
      | Black (j, _, _, _, m) | Red (j, _, _, _, m) -> (j, m)
    in
    Red (i + j + 1, l, v, r, extend v (extend ml mr))

  let singleton x = black empty x empty

  (* Insertion *)

  let lbalance x1 x2 x3 =
    match (x1, x2, x3) with
    | Red (_, Red (_, a, x, b, _), y, c, _), z, d ->
        red (black a x b) y (black c z d)
    | Red (_, a, x, Red (_, b, y, c, _), _), z, d ->
        red (black a x b) y (black c z d)
    | a, x, b -> black a x b

  let rbalance x1 x2 x3 =
    match (x1, x2, x3) with
    | a, x, Red (_, Red (_, b, y, c, _), z, d, _) ->
        red (black a x b) y (black c z d)
    | a, x, Red (_, b, y, Red (_, c, z, d, _), _) ->
        red (black a x b) y (black c z d)
    | a, x, b -> black a x b

  let add x t =
    let rec ins = function
      | Empty -> red Empty x Empty
      | Red (_, a, y, b, _) as t ->
          let cmp_lo = Ord.compare x.lo y.lo in
          let cmp_hi = Ord.compare x.hi y.hi in
          if cmp_lo < 0 || (cmp_lo = 0 && cmp_hi < 0) then red (ins a) y b
          else if cmp_lo > 0 || (cmp_lo = 0 && cmp_hi > 0) then red a y (ins b)
          else t
      | Black (_, a, y, b, _) as t ->
          let cmp_lo = Ord.compare x.lo y.lo in
          let cmp_hi = Ord.compare x.hi y.hi in
          if cmp_lo < 0 || (cmp_lo = 0 && cmp_hi < 0) then lbalance (ins a) y b
          else if cmp_lo > 0 || (cmp_lo = 0 && cmp_hi > 0) then
            rbalance a y (ins b)
          else t
    in
    match ins t with
    | Black _ as s -> s
    | Red (_, a, y, b, _) -> black a y b
    | Empty -> assert false

  (* Deletion *)

  let lunbalanced = function
    | Red (_, Black (_, a, x, b, _), y, c, _) ->
        (lbalance (red a x b) y c, false)
    | Black (_, Black (_, a, x, b, _), y, c, _) ->
        (lbalance (red a x b) y c, true)
    | Black (_, Red (_, a, x, Black (_, b, y, c, _), _), z, d, _) ->
        (black a x (lbalance (red b y c) z d), false)
    | _ -> assert false

  let runbalanced = function
    | Red (_, a, x, Black (_, b, y, c, _), _) ->
        (rbalance a x (red b y c), false)
    | Black (_, a, x, Black (_, b, y, c, _), _) ->
        (rbalance a x (red b y c), true)
    | Black (_, a, x, Red (_, Black (_, b, y, c, _), z, d, _), _) ->
        (black (rbalance a x (red b y c)) z d, false)
    | _ -> assert false

  let rec remove_min = function
    | Empty -> assert false
    | Black (_, Empty, x, Empty, _) -> (Empty, x, true)
    | Black (_, Empty, x, Red (_, l, y, r, _), _) -> (black l y r, x, false)
    | Black (_, Empty, _, Black _, _) -> assert false
    | Red (_, Empty, x, r, _) -> (r, x, false)
    | Black (_, l, x, r, _) ->
        let l, m, d = remove_min l in
        let t = black l x r in
        if d then
          let t, d = runbalanced t in
          (t, m, d)
        else (t, m, false)
    | Red (_, l, x, r, _) ->
        let l, m, d = remove_min l in
        let t = red l x r in
        if d then
          let t, d = runbalanced t in
          (t, m, d)
        else (t, m, false)

  let remove x t =
    let rec remove_aux = function
      | Empty -> (Empty, false)
      | Black (_, l, y, r, m) -> (
          if not (intersects Ord.compare x m) then (t, false)
          else
            let cmp_lo = Ord.compare x.lo y.lo in
            let cmp_hi = Ord.compare x.hi y.hi in
            if cmp_lo < 0 || (cmp_lo = 0 && cmp_hi < 0) then
              let l, d = remove_aux l in
              let t = black l y r in
              if d then runbalanced t else (t, false)
            else if cmp_lo > 0 || (cmp_lo = 0 && cmp_hi > 0) then
              let r, d = remove_aux r in
              let t = black l y r in
              if d then lunbalanced t else (t, false)
            else
              (* x = y *)
              match r with
              | Empty -> (
                  match l with
                  | Red (_, l, x, r, _) -> (black l x r, false)
                  | t -> (t, true))
              | _ ->
                  let r, m, d = remove_min r in
                  let t = black l m r in
                  if d then lunbalanced t else (t, false))
      | Red (_, l, y, r, m) -> (
          if not (intersects Ord.compare x m) then (t, false)
          else
            let cmp_lo = Ord.compare x.lo y.lo in
            let cmp_hi = Ord.compare x.hi y.hi in
            if cmp_lo < 0 || (cmp_lo = 0 && cmp_hi < 0) then
              let l, d = remove_aux l in
              let t = red l y r in
              if d then runbalanced t else (t, false)
            else if cmp_lo > 0 || (cmp_lo = 0 && cmp_hi > 0) then
              let r, d = remove_aux r in
              let t = red l y r in
              if d then lunbalanced t else (t, false)
            else
              (* x = y *)
              match r with
              | Empty -> (l, false)
              | _ ->
                  let r, m, d = remove_min r in
                  let t = red l m r in
                  if d then lunbalanced t else (t, false))
    in
    fst (remove_aux t)
end

module Core
    (Ord : Sigs.COMPARABLE)
    (Base : T with type point = Ord.t and type interval = Ord.t t) =
struct
  include Base

  let restrict x y =
    {
      lo = (if Ord.compare x.lo y.lo < 0 then y.lo else x.lo);
      hi = (if Ord.compare x.hi y.hi > 0 then y.hi else x.hi);
    }

  let is_empty = function Empty -> true | _ -> false

  let is_interval = function
    | Red (_, Empty, v, Empty, _) | Black (_, Empty, v, Empty, _) -> Some v
    | _ -> None

  let is_point t =
    match is_interval t with
    | None -> None
    | Some t -> if Ord.compare t.lo t.hi = 0 then Some t.lo else None

  let min = function
    | Empty -> None
    | Red (_, _, _, _, m) | Black (_, _, _, _, m) -> Some m.lo

  let max = function
    | Empty -> None
    | Red (_, _, _, _, m) | Black (_, _, _, _, m) -> Some m.hi

  let rec mem x = function
    | Empty -> false
    | Black (_, l, v, r, m) | Red (_, l, v, r, m) ->
        Ord.compare x.lo m.lo >= 0
        && Ord.compare x.hi m.hi <= 0
        &&
        let cmp_lo = Ord.compare x.lo v.lo in
        let cmp_hi = Ord.compare x.hi v.hi in
        (cmp_lo = 0 && cmp_hi = 0) || mem x (if cmp_lo < 0 then l else r)

  let rec belongs_aux acc x = function
    | Empty -> acc
    | Black (_, a, y, b, m) | Red (_, a, y, b, m) ->
        if not (belongs Ord.compare x m) then acc
        else
          let acc =
            if belongs Ord.compare x y then y :: belongs_aux acc x a
            else belongs_aux acc x a
          in
          if Ord.compare x y.lo < 0 then acc else belongs_aux acc x b

  let belongs x t = belongs_aux [] x t

  let rec intersects_aux acc x = function
    | Empty -> acc
    | Black (_, a, y, b, m) | Red (_, a, y, b, m) ->
        if not (intersects Ord.compare x m) then acc
        else
          let acc =
            if intersects Ord.compare x y then y :: intersects_aux acc x a
            else intersects_aux acc x a
          in
          if Ord.compare x.hi y.lo < 0 then acc else intersects_aux acc x b

  let intersects x t = intersects_aux [] x t

  let rec iter f = function
    | Empty -> ()
    | Black (_, l, v, r, _) | Red (_, l, v, r, _) ->
        iter f l;
        f v;
        iter f r

  let rec fold f t acc =
    match t with
    | Empty -> acc
    | Black (_, l, v, r, _) | Red (_, l, v, r, _) ->
        fold f r (f v (fold f l acc))

  let map f t = fold (fun i t -> add (f i) t) t empty

  let union t1 t2 =
    let t1, t2 = if cardinal t1 > cardinal t2 then (t2, t1) else (t1, t2) in
    fold add t1 t2

  let inter t1 t2 =
    let t1, t2 = if cardinal t1 > cardinal t2 then (t2, t1) else (t1, t2) in
    fold
      (fun interval acc ->
        List.fold_left
          (fun acc i -> add (restrict interval i) acc)
          acc (intersects interval t2))
      t1 empty

  let print pr t =
    fold
      (fun i acc -> Printf.sprintf "[%s ; %s]" (pr i.lo) (pr i.hi) :: acc)
      t []
    |> List.rev |> String.concat " "
end

module Make (Ord : Sigs.COMPARABLE) = Core (Ord) (Base (Ord))

module Flat (Ord : Sigs.ITERABLE) =
  Core
    (Ord)
    (struct
      include Base (Ord)

      let adjacent x y = y = Ord.succ x || y = Ord.pred x

      let adjacent x y =
        intersects Ord.compare x y || adjacent x.hi y.lo || adjacent x.lo y.hi

      let rec adjacent_aux acc x = function
        | Empty -> acc
        | Black (_, a, y, b, m) | Red (_, a, y, b, m) ->
            if not (adjacent x m) then acc
            else
              let acc =
                if adjacent x y then y :: adjacent_aux acc x a
                else adjacent_aux acc x a
              in
              if Ord.compare x.hi y.lo < 0 then acc else adjacent_aux acc x b

      let adjacent x t = adjacent_aux [] x t

      let add interval t =
        let list = adjacent interval t in
        add
          (List.fold_left
             (fun acc i ->
               {
                 lo = (if Ord.compare i.lo acc.lo < 0 then i.lo else acc.lo);
                 hi = (if Ord.compare i.hi acc.hi > 0 then i.hi else acc.hi);
               })
             interval list)
          (List.fold_left (fun t i -> remove i t) t list)

      let remove interval t =
        let list = adjacent interval t in
        List.fold_left
          (fun acc i ->
            ( acc |> fun acc ->
              if Ord.compare i.lo interval.lo < 0 then
                add { lo = i.lo; hi = Ord.pred interval.lo } acc
              else acc )
            |> fun acc ->
            if Ord.compare i.hi interval.hi > 0 then
              add { lo = Ord.succ interval.hi; hi = i.hi } acc
            else acc)
          (List.fold_left (fun t i -> remove i t) t list)
          list
    end)

module Int = Make (struct
  type t = int

  let compare (x : int) (y : int) = compare x y
end)

module IntFlat = Flat (struct
  type t = int

  let compare (x : int) (y : int) = compare x y
  let succ x = succ x
  let pred x = pred x
end)

module Float = Make (struct
  type t = float

  let compare (x : float) (y : float) = compare x y
end)

module FloatFlat = Flat (struct
  type t = float

  let compare (x : float) (y : float) = compare x y
  let succ x = x +. epsilon_float
  let pred x = x -. epsilon_float
end)

module BV
    (Make : S with type point = Bitvector.t and type interval = Bitvector.t t) =
struct
  include Make

  let top n = singleton Bitvector.{ lo = zeros n; hi = max_ubv n }
  let bot _ = empty
  let ule bv = singleton Bitvector.{ lo = zeros (size_of bv); hi = bv }
  let uge bv = singleton Bitvector.{ lo = bv; hi = max_ubv (size_of bv) }
  let ult bv = if Bitvector.is_zeros bv then empty else ule (Bitvector.pred bv)

  let ugt bv =
    if Bitvector.is_max_ubv bv then empty else uge (Bitvector.succ bv)

  let sle bv =
    let sz = Bitvector.size_of bv in
    if Bitvector.is_neg bv then singleton Bitvector.{ lo = min_sbv sz; hi = bv }
    else
      empty
      |> add Bitvector.{ lo = zeros sz; hi = bv }
      |> add Bitvector.{ lo = min_sbv sz; hi = max_ubv sz }

  let sge bv =
    let sz = Bitvector.size_of bv in
    if Bitvector.is_pos bv then singleton Bitvector.{ lo = bv; hi = max_sbv sz }
    else
      empty
      |> add Bitvector.{ lo = zeros sz; hi = max_sbv sz }
      |> add Bitvector.{ lo = bv; hi = max_ubv sz }

  let slt bv =
    if Bitvector.is_min_sbv bv then empty else sle (Bitvector.pred bv)

  let sgt bv =
    if Bitvector.is_max_sbv bv then empty else sge (Bitvector.succ bv)

  let equal bv = singleton { lo = bv; hi = bv }
  let distinct bv = union (ult bv) (ugt bv)

  let size_of t =
    match min t with None -> None | Some bv -> Some (Bitvector.size_of bv)

  let zero_extend i t =
    match size_of t with
    | None -> empty
    | Some sz ->
        map
          (fun t ->
            {
              lo = Bitvector.extend t.lo (sz + i);
              hi = Bitvector.extend t.hi (sz + i);
            })
          t

  let sign_extend i t =
    match size_of t with
    | None -> empty
    | Some sz ->
        map
          (fun t ->
            {
              lo = Bitvector.extend_signed t.lo (sz + i);
              hi = Bitvector.extend_signed t.hi (sz + i);
            })
          t

  let extract i t =
    let sz = i.hi - i.lo + 1 in
    fold
      (fun t acc ->
        let shr_lo = Bitvector.shift_right t.lo i.lo in
        let shr_hi = Bitvector.shift_right t.hi i.lo in
        let length = Bitvector.fill ~hi:(sz - 1) (Bitvector.size_of t.lo) in
        if Bitvector.ugt (Bitvector.sub shr_hi shr_lo) length then
          add { lo = Bitvector.zeros sz; hi = Bitvector.fill sz } acc
        else if Bitvector.ule shr_hi (Bitvector.logor shr_lo length) then
          add
            { lo = Bitvector.extract t.lo i; hi = Bitvector.extract t.hi i }
            acc
        else
          acc
          |> add { lo = Bitvector.extract t.lo i; hi = Bitvector.fill sz }
          |> add { lo = Bitvector.zeros sz; hi = Bitvector.extract t.hi i })
      t empty

  let concat t1 t2 =
    fold
      (fun t1 acc ->
        fold
          (fun t2 acc ->
            add
              {
                lo = Bitvector.append t1.lo t2.lo;
                hi = Bitvector.append t1.hi t2.hi;
              }
              acc)
          t2 acc)
      t1 empty

  let bvand t1 t2 =
    match (size_of t1, size_of t2) with
    | None, None | None, Some _ | Some _, None -> empty
    | Some sz1, Some sz2 -> (
        assert (sz1 = sz2);
        match is_point t2 with
        | Some bv -> map (fun t -> { t with lo = Bitvector.logand bv t.lo }) t1
        | None -> (
            match is_point t1 with
            | Some bv ->
                map (fun t -> { t with lo = Bitvector.logand bv t.lo }) t2
            | None -> top sz1))

  let bvor t1 t2 =
    match (size_of t1, size_of t2) with
    | None, None | None, Some _ | Some _, None -> empty
    | Some sz1, Some sz2 -> (
        assert (sz1 = sz2);
        match is_point t2 with
        | Some bv -> map (fun t -> { t with hi = Bitvector.logor bv t.hi }) t1
        | None -> (
            match is_point t1 with
            | Some bv ->
                map (fun t -> { t with hi = Bitvector.logor bv t.hi }) t2
            | None -> top sz1))

  let bvadd t1 t2 =
    fold
      (fun t1 acc ->
        let mx = Bitvector.max_ubv (Bitvector.size_of t1.lo) in
        fold
          (fun t2 acc ->
            let lo = Bitvector.add t1.lo t2.lo in
            let hi = Bitvector.add t1.hi t2.hi in
            if
              Bitvector.ult (Bitvector.sub mx t1.hi) t2.hi
              && Bitvector.uge (Bitvector.sub mx t1.lo) t2.lo
            then union acc (union (uge lo) (ule hi))
            else union acc (inter (uge lo) (ule hi)))
          t2 acc)
      t1 empty

  let bvsub t1 t2 =
    fold
      (fun t1 acc ->
        fold
          (fun t2 acc ->
            let lo = Bitvector.sub t1.lo t2.hi in
            let hi = Bitvector.sub t1.hi t2.lo in
            if Bitvector.ult t1.lo t2.hi && Bitvector.uge t1.hi t2.lo then
              union acc (union (uge lo) (ule hi))
            else union acc (inter (uge lo) (ule hi)))
          t2 acc)
      t1 empty
end

module BitVec = BV (Make (struct
  type t = Bitvector.t

  let compare x y = Bitvector.compare x y
end))

module BitVecFlat = BV (Flat (struct
  type t = Bitvector.t

  let compare x y = Bitvector.compare x y
  let succ x = if Bitvector.is_max_ubv x then x else Bitvector.succ x
  let pred x = if Bitvector.is_zeros x then x else Bitvector.pred x
end))
