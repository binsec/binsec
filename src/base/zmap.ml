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

type ('a, _) node =
  | Empty : ('a, [< `Empty | `Item_or_empty | `Any ]) node
  | Item : {
      lo : Z.t;
      hi : Z.t;
      elt : 'a;
    }
      -> ('a, [< `Non_empty | `Item | `Item_or_empty | `Any ]) node
  | Node : {
      lo : Z.t;
      hi : Z.t;
      mask : Z.t;
      zero : 'a tree;
      one : 'a tree;
    }
      -> ('a, [< `Non_empty | `Node | `Any ]) node

and 'a tree = ('a, [ `Non_empty ]) node
and 'a t = ('a, [ `Any ]) node
and 'a item = ('a, [ `Item ]) node
and 'a item_opt = ('a, [ `Item_or_empty ]) node
and 'a branch = ('a, [ `Node ]) node

type ('a, 'b) continuation =
  | Root : ('a, [< `Root | `Any ]) continuation
  | Right : 'a path * 'a tree -> ('a, [< `Right | `Any ]) continuation

and 'a path = ('a, [ `Any ]) continuation
and 'a right = ('a, [ `Right ]) continuation

(* most significant bit of difference betweeb two key *)
let diff k k' = Z.numbits (Z.logxor k k')
let any : 'a tree -> 'a t = function (Item _ | Node _) as t -> t

let iter : ('a item -> unit) -> 'a t -> unit =
  let rec iter : ('a item -> unit) -> 'a tree -> unit =
   fun f t ->
    match t with
    | Item _ as item -> f item
    | Node { zero; one; _ } ->
        iter f zero;
        iter f one
  in
  fun f t ->
    match t with Empty -> () | (Item _ | Node _) as tree -> iter f tree

let rev_iter : ('a item -> unit) -> 'a t -> unit =
  let rec rev_iter : ('a item -> unit) -> 'a tree -> unit =
   fun f t ->
    match t with
    | Item _ as item -> f item
    | Node { zero; one; _ } ->
        rev_iter f one;
        rev_iter f zero
  in
  fun f t ->
    match t with Empty -> () | (Item _ | Node _) as tree -> rev_iter f tree

let fold : ('a item -> 'b -> 'b) -> 'b -> 'a t -> 'b =
  let rec fold : ('a item -> 'b -> 'b) -> 'b -> 'a tree -> 'b =
   fun f acc t ->
    match t with
    | Item _ as item -> f item acc
    | Node { zero; one; _ } -> fold f (fold f acc zero) one
  in
  fun f acc t ->
    match t with Empty -> acc | (Item _ | Node _) as tree -> fold f acc tree

let rev_fold : ('a item -> 'b -> 'b) -> 'b -> 'a t -> 'b =
  let rec rev_fold : ('a item -> 'b -> 'b) -> 'b -> 'a tree -> 'b =
   fun f acc t ->
    match t with
    | Item _ as item -> f item acc
    | Node { zero; one; _ } -> rev_fold f (rev_fold f acc one) zero
  in
  fun f acc t ->
    match t with
    | Empty -> acc
    | (Item _ | Node _) as tree -> rev_fold f acc tree

let is_empty : 'a t -> bool = function
  | Empty -> true
  | Item _ | Node _ -> false

let lower_bound : 'a tree -> Z.t = function
  | Item { lo; _ } | Node { lo; _ } -> lo

let upper_bound : 'a tree -> Z.t = function
  | Item { hi; _ } | Node { hi; _ } -> hi

let rec is_empty_between_left : Z.t -> Z.t -> 'a tree -> bool =
 fun lo' hi' tree ->
  match tree with
  | Item _ -> false
  | Node { lo; zero; one; _ } ->
      Z.lt lo lo' && is_empty_between_choice lo' hi' zero one

and is_empty_between_right : Z.t -> Z.t -> 'a tree -> bool =
 fun lo' hi' tree ->
  match tree with
  | Item _ -> false
  | Node { hi; zero; one; _ } ->
      Z.lt hi' hi && is_empty_between_choice lo' hi' zero one

and is_empty_between_choice : Z.t -> Z.t -> 'a tree -> 'a tree -> bool =
 fun lo' hi' zero one ->
  let ub = upper_bound zero in
  if Z.leq hi' ub then is_empty_between_left lo' hi' zero
  else
    let lb = lower_bound one in
    if Z.leq lb lo' then is_empty_between_right lo' hi' one
    else Z.lt ub lo' && Z.lt hi' lb

let is_empty_between : Z.t -> Z.t -> 'a t -> bool =
 fun lo' hi' t ->
  match t with
  | Empty -> true
  | Item { lo; hi; _ } -> Z.lt hi' lo || Z.lt hi lo'
  | Node { lo; hi; zero; one; _ } ->
      Z.lt hi' lo || Z.lt hi lo' || is_empty_between_choice lo' hi' zero one

let map : ('a item -> 'b) -> 'a t -> 'b t =
  let rec map : ('a item -> 'b) -> 'a tree -> 'b tree =
   fun f t ->
    match t with
    | Item { lo; hi; _ } as item -> Item { lo; hi; elt = f item }
    | Node { lo; hi; mask; zero; one } ->
        Node { lo; hi; mask; zero = map f zero; one = map f one }
  in
  fun f t ->
    match t with
    | Empty -> Empty
    | (Item _ | Node _) as tree -> any (map f tree)

let bindings : 'a t -> 'a item list =
  let rec bindings : 'a tree -> 'a item list -> 'a item list =
   fun t acc ->
    match t with
    | Item _ as item -> item :: acc
    | Node { zero; one; _ } -> bindings zero (bindings one acc)
  in
  function Empty -> [] | (Item _ | Node _) as tree -> bindings tree []

let mem : Z.t -> 'a t -> bool =
  let rec mem : Z.t -> 'a tree -> bool =
   fun index tree ->
    match tree with
    | Item _ -> true
    | Node { zero; one; _ } -> mem_choice index zero one
  and mem_choice : Z.t -> 'a tree -> 'a tree -> bool =
   fun index zero one ->
    let ub = upper_bound zero in
    if Z.leq index ub then mem index zero
    else
      let lb = lower_bound one in
      Z.leq lb index && mem index one
  in
  fun index t ->
    match t with
    | Empty -> false
    | Item { lo; hi; _ } -> Z.leq lo index && Z.leq index hi
    | Node { lo; hi; zero; one; _ } ->
        Z.leq lo index && Z.leq index hi && mem_choice index zero one

let find_opt : Z.t -> 'a t -> 'a item_opt =
  let rec find : Z.t -> 'a tree -> 'a item_opt =
   fun index tree ->
    match tree with
    | Item _ as item -> item
    | Node { zero; one; _ } -> find_choice index zero one
  and find_choice : Z.t -> 'a tree -> 'a tree -> 'a item_opt =
   fun index zero one ->
    let ub = upper_bound zero in
    if Z.leq index ub then find index zero
    else
      let lb = lower_bound one in
      if Z.leq lb index then find index one else Empty
  in
  fun index t ->
    match t with
    | Empty -> Empty
    | Item { lo; hi; _ } as item ->
        if Z.lt index lo || Z.lt hi index then Empty else item
    | Node { lo; hi; zero; one; _ } ->
        if Z.lt index lo || Z.lt hi index then Empty
        else find_choice index zero one

let none : 'a item_opt = Empty

let find : Z.t -> 'a t -> 'a item =
 fun index t ->
  match find_opt index t with
  | Empty -> raise Not_found
  | Item _ as item -> item

let choose : 'a t -> 'a item =
  let rec choose : 'a tree -> 'a item = function
    | Item _ as item -> item
    | Node { zero; _ } -> choose zero
  in
  function Empty -> raise Not_found | (Item _ | Node _) as tree -> choose tree

let empty : 'a t = Empty

let singleton : lo:Z.t -> hi:Z.t -> 'a -> 'a t =
 fun ~lo ~hi elt ->
  if Z.lt hi lo then raise (Invalid_argument "singleton");
  Item { lo; hi; elt }

let rec join :
    stich:('a item -> 'a item -> 'a option) option ->
    'a tree ->
    'a tree ->
    'a tree =
  let join_make_nodes :
      stich:('a item -> 'a item -> 'a option) option ->
      lo:Z.t ->
      hi:Z.t ->
      mask:Z.t ->
      zero:'a tree ->
      one:'a tree ->
      'a tree =
    let rec union_eq_make_nodes_rebuilt_left :
        'a branch list -> 'a tree -> Z.t -> 'a tree =
     fun path0 rightmost1 hi1 ->
      match path0 with
      | [] -> rightmost1
      | Node { lo = lo0; mask = mask0; zero = zero0; _ } :: path0 ->
          union_eq_make_nodes_rebuilt_left path0
            (Node
               {
                 lo = lo0;
                 hi = hi1;
                 mask = mask0;
                 zero = zero0;
                 one = rightmost1;
               })
            hi1
    in
    let rec join_make_nodes_rebuilt_right :
        'a tree -> Z.t -> 'a branch list -> 'a tree =
     fun leftmost0 lo0 path1 ->
      match path1 with
      | [] -> leftmost0
      | Node { hi = hi1; mask = mask1; one = one1; _ } :: path1 ->
          join_make_nodes_rebuilt_right
            (Node
               {
                 lo = lo0;
                 hi = hi1;
                 mask = mask1;
                 zero = leftmost0;
                 one = one1;
               })
            lo0 path1
    in
    let rec join_make_nodes_leftmost :
        stich:('a item -> 'a item -> 'a option) ->
        lo:Z.t ->
        hi:Z.t ->
        mask:Z.t ->
        zero:'a tree ->
        one:'a tree ->
        'a item ->
        'a branch list ->
        'a tree ->
        'a branch list ->
        'a tree =
     fun ~stich ~lo ~hi ~mask ~zero ~one (Item { lo = lo0; _ } as rightmost0)
         path0 leftmost1 path1 ->
      match leftmost1 with
      | Item { hi = hi1; _ } as leftmost1 -> (
          match stich rightmost0 leftmost1 with
          | None -> Node { lo; hi; mask; zero; one }
          | Some elt -> (
              let zero =
                union_eq_make_nodes_rebuilt_left path0
                  (Item { lo = lo0; hi = hi1; elt })
                  hi1
              in
              match path1 with
              | [] -> zero
              | Node { one = one1; _ } :: path1 ->
                  Node
                    {
                      lo;
                      hi;
                      mask;
                      zero;
                      one =
                        join_make_nodes_rebuilt_right one1 (lower_bound one1)
                          path1;
                    }))
      | Node { zero = zero0; _ } as node1 ->
          join_make_nodes_leftmost ~stich ~lo ~hi ~mask ~zero ~one rightmost0
            path0 zero0 (node1 :: path1)
    in
    let rec join_make_nodes_rightmost :
        stich:('a item -> 'a item -> 'a option) ->
        lo:Z.t ->
        hi:Z.t ->
        mask:Z.t ->
        zero:'a tree ->
        one:'a tree ->
        'a tree ->
        'a branch list ->
        'a tree =
     fun ~stich ~lo ~hi ~mask ~zero ~one rightmost0 path0 ->
      match rightmost0 with
      | Item _ as item0 ->
          join_make_nodes_leftmost ~stich ~lo ~hi ~mask ~zero ~one item0 path0
            one []
      | Node { one = one1; _ } as node0 ->
          join_make_nodes_rightmost ~stich ~lo ~hi ~mask ~zero ~one one1
            (node0 :: path0)
    in
    fun ~stich ~lo ~hi ~mask ~zero ~one ->
      match stich with
      | Some stich
        when Z.equal Z.one (Z.sub (lower_bound one) (upper_bound zero)) ->
          join_make_nodes_rightmost ~stich ~lo ~hi ~mask ~zero ~one zero []
      | Some _ | None -> Node { lo; hi; mask; zero; one }
  in
  let join_disjoint_items :
      stich:('a item -> 'a item -> 'a option) option ->
      'a item ->
      'a item ->
      'a tree =
   fun ~stich (Item { lo = lo0; _ } as item0)
       (Item { lo = lo1; hi = hi1; _ } as item1) ->
    join_make_nodes ~stich ~lo:lo0 ~hi:hi1
      ~mask:(Z.logand lo1 (Z.shift_left Z.minus_one (diff lo0 lo1 - 1)))
      ~zero:item0 ~one:item1
  and join_disjoint_item_node :
      stich:('a item -> 'a item -> 'a option) option ->
      'a item ->
      'a branch ->
      'a tree =
   fun ~stich (Item { lo = lo0; _ } as item0)
       (Node { hi = hi1; mask = mask1; zero = zero1; one = one1; _ } as node1) ->
    let n = diff lo0 mask1 in
    let z' = Z.trailing_zeros mask1 in
    if z' + 1 >= n then
      join_make_nodes ~stich ~lo:lo0 ~hi:hi1 ~mask:mask1
        ~zero:(join ~stich item0 zero1) ~one:one1
    else
      join_make_nodes ~stich ~lo:lo0 ~hi:hi1
        ~mask:(Z.logand mask1 (Z.shift_left Z.minus_one (n - 1)))
        ~zero:item0 ~one:node1
  and join_disjoint_node_item :
      stich:('a item -> 'a item -> 'a option) option ->
      'a branch ->
      'a item ->
      'a tree =
   fun ~stich
       (Node { lo = lo0; mask = mask0; zero = zero0; one = one0; _ } as node0)
       (Item { lo = lo1; hi = hi1; _ } as item1) ->
    let n = diff mask0 lo1 and z = Z.trailing_zeros mask0 in
    if z >= n then
      join_make_nodes ~stich ~lo:lo0 ~hi:hi1 ~mask:mask0 ~zero:zero0
        ~one:(join ~stich one0 item1)
    else
      join_make_nodes ~stich ~lo:lo0 ~hi:hi1
        ~mask:(Z.logand lo1 (Z.shift_left Z.minus_one (n - 1)))
        ~zero:node0 ~one:item1
  and join_disjoint_nodes :
      stich:('a item -> 'a item -> 'a option) option ->
      'a branch ->
      'a branch ->
      'a tree =
   fun ~stich
       (Node { lo = lo0; mask = mask0; zero = zero0; one = one0; _ } as node0)
       (Node { hi = hi1; mask = mask1; zero = zero1; one = one1; _ } as node1) ->
    let n = diff mask0 mask1 and z = Z.trailing_zeros mask0 in
    if z >= n then
      join_make_nodes ~stich ~lo:lo0 ~hi:hi1 ~mask:mask0 ~zero:zero0
        ~one:(join ~stich one0 node1)
    else
      let z' = Z.trailing_zeros mask1 in
      if z' + 1 >= n then
        join_make_nodes ~stich ~lo:lo0 ~hi:hi1 ~mask:mask1
          ~zero:(join ~stich node0 zero1) ~one:one1
      else
        join_make_nodes ~stich ~lo:lo0 ~hi:hi1
          ~mask:(Z.logand mask1 (Z.shift_left Z.minus_one (n - 1)))
          ~zero:node0 ~one:node1
  in
  fun ~stich tree0 tree1 ->
    match (tree0, tree1) with
    | (Item _ as item0), (Item _ as item1) ->
        join_disjoint_items ~stich item0 item1
    | (Item _ as item0), (Node _ as node1) ->
        join_disjoint_item_node ~stich item0 node1
    | (Node _ as node0), (Item _ as item1) ->
        join_disjoint_node_item ~stich node0 item1
    | (Node _ as node0), (Node _ as node1) ->
        join_disjoint_nodes ~stich node0 node1

let join_left :
    stich:('a item -> 'a item -> 'a option) option -> 'a tree -> 'a t -> 'a tree
    =
 fun ~stich tree0 t1 ->
  match t1 with
  | Empty -> tree0
  | (Item _ | Node _) as tree1 -> join ~stich tree0 tree1

let join_right :
    stich:('a item -> 'a item -> 'a option) option -> 'a t -> 'a tree -> 'a tree
    =
 fun ~stich t0 tree1 ->
  match t0 with
  | Empty -> tree1
  | (Item _ | Node _) as tree0 -> join ~stich tree0 tree1

(* (* type mode =
      | Left_right  (** Left and right *)
      | Left_acc  (** Left and accumulator *)
      | Acc_right  (** Accumulator and right *) *)

   type 'a overlap =
     | Ll_Rl_Ru_Lu : [ `Four_endpoints ] overlap
         (** Left starts before and ends after right.
             \[     left      \]
                \[  right  \]
         *)
     | Rl_Ll_Lu_Ru : [ `Four_endpoints ] overlap
         (** Right starts before and ends after left.
                \[  left    \]
             \[     right     \]
         *)
     | Ll_Rl_Lu_Ru : [ `Four_endpoints ] overlap
         (** Right starts and ends after left.
             \[  left  \]
                \[  right  \]
         *)
     | Rl_Ll_Ru_Lu : [ `Four_endpoints ] overlap
         (** Left starts and ends after right.
                 \[  left  \]
             \[  right  \]
         *)
     | LRl_Ru_Lu : [ `Three_endpoints ] overlap
         (** Left ends after right.
             \[  left        \]
             \[  right    \]
         *)
     | LRl_Lu_Ru : [ `Three_endpoints ] overlap
         (** Right ends after left.
             \[  left     \]
             \[  right      \]
         *)
     | Ll_Rl_LRu : [ `Three_endpoints ] overlap
         (** Left starts before right.
             \[  left        \]
                \[  right    \]
         *)
     | Rl_Ll_LRu : [ `Three_endpoints ] overlap
         (** Right starts before left.
                  \[  left  \]
             \[  right      \]
         *)
     | LRl_LRu : [ `Two_endpoints ] overlap
         (** Left and right are equal.
             \[  left  \]
             \[  right \]
         *)

   type ('a, 'b) update =
     | Left_overlay
         : ('a, [< `Two_endpoints | `Three_endpoints | `Four_endpoints ]) update
     | Right_overlay
         : ('a, [< `Two_endpoints | `Three_endpoints | `Four_endpoints ]) update
     | Merge_overlay :
         'a
         -> ('a, [< `Two_endpoints | `Three_endpoints | `Four_endpoints ]) update
         (** Insert a single item ranging from first to last endpoints *)
     | Split_FSL : 'a * 'a -> ('a, [< `Three_endpoints | `Four_endpoints ]) update
         (** Insert one item between first and second endpoints, then one item between second and last endpoints. *)
     | Split_FPL : 'a * 'a -> ('a, [< `Three_endpoints | `Four_endpoints ]) update
         (** Insert one item between first and penultimate endpoints, then one item between penultimate and last endpoints. *)
     | Split_FSTL : 'a * 'a * 'a -> ('a, [< `Four_endpoints ]) update
         (** Insert three items between the four endpoints.  *)

   type 'a update_handler = {
     f : 'k. 'k overlap -> 'a item -> 'a item -> ('a, 'k) update;
   }
   [@@unboxed]

   let union_update : 'a update_handler -> 'a t -> 'a t -> 'a t =
     let union_update_overlap_items_ordered :
         'a update_handler -> 'a item -> 'a item -> 'a tree =
      fun { f = update } (Item { lo = lo0; hi = hi0; elt = elt0 } as item0)
          (Item { lo = lo1; hi = hi1; elt = elt1 } as item1) ->
       if Z.equal lo0 lo1 then
         if Z.equal hi0 hi1 then
           match update LRl_LRu item0 item1 with
           | Left_overlay -> item0
           | Right_overlay -> item1
           | Merge_overlay elt -> Item { lo = lo0; hi = hi0; elt }
         else if Z.lt hi0 hi1 then
           match update LRl_Lu_Ru item0 item1 with
           | Left_overlay -> Item { lo = lo0; hi = hi1; elt = elt0 }
           | Right_overlay -> item1
           | Merge_overlay elt -> Item { lo = lo0; hi = hi1; elt }
           | Split_FSL (eltf, elts) | Split_FPL (eltf, elts) ->
               join
                 (Item { lo = lo0; hi = Z.pred hi0; elt = eltf })
                 (Item { lo = hi0; hi = hi1; elt = elts })
         else
           match update LRl_Ru_Lu item0 item1 with
           | Left_overlay -> item0
           | Right_overlay -> Item { lo = lo0; hi = hi0; elt = elt1 }
           | Merge_overlay elt -> Item { lo = lo0; hi = hi0; elt }
           | Split_FSL (eltf, elts) | Split_FPL (eltf, elts) ->
               join
                 (Item { lo = lo0; hi = Z.pred hi1; elt = eltf })
                 (Item { lo = hi1; hi = hi0; elt = elts })
       else if Z.equal hi0 hi1 then
         match update Ll_Rl_LRu item0 item1 with
         | Left_overlay -> item0
         | Right_overlay -> Item { lo = lo0; hi = hi0; elt = elt1 }
         | Merge_overlay elt -> Item { lo = lo0; hi = hi0; elt }
         | Split_FSL (eltf, elts) | Split_FPL (eltf, elts) ->
             join
               (Item { lo = lo0; hi = Z.pred lo1; elt = eltf })
               (Item { lo = lo1; hi = hi1; elt = elts })
       else if Z.lt hi0 hi1 then
         match update Ll_Rl_Lu_Ru item0 item1 with
         | Left_overlay -> Item { lo = lo0; hi = hi1; elt = elt0 }
         | Right_overlay -> Item { lo = lo0; hi = hi1; elt = elt1 }
         | Merge_overlay elt -> Item { lo = lo0; hi = hi1; elt }
         | Split_FSL (eltf, elts) ->
             join
               (Item { lo = lo0; hi = Z.pred lo1; elt = eltf })
               (Item { lo = lo1; hi = hi1; elt = elts })
         | Split_FPL (eltf, elts) ->
             join
               (Item { lo = lo0; hi = Z.pred hi0; elt = eltf })
               (Item { lo = hi0; hi = hi1; elt = elts })
         | Split_FSTL (eltf, eltm, eltl) ->
             join
               (join
                  (Item { lo = lo0; hi = Z.pred lo1; elt = eltf })
                  (Item { lo = lo1; hi = Z.pred hi0; elt = eltm }))
               (Item { lo = hi0; hi = hi1; elt = eltl })
       else
         match update Ll_Rl_Ru_Lu item0 item1 with
         | Left_overlay -> item0
         | Right_overlay -> Item { lo = lo0; hi = hi0; elt = elt1 }
         | Merge_overlay elt -> Item { lo = lo0; hi = hi0; elt }
         | Split_FSL (eltf, elts) ->
             join
               (Item { lo = lo0; hi = Z.pred lo1; elt = eltf })
               (Item { lo = lo1; hi = hi0; elt = elts })
         | Split_FPL (eltf, elts) ->
             join
               (Item { lo = lo0; hi = Z.pred hi1; elt = eltf })
               (Item { lo = hi1; hi = hi0; elt = elts })
         | Split_FSTL (eltf, eltm, eltl) ->
             join
               (join
                  (Item { lo = lo0; hi = Z.pred lo1; elt = eltf })
                  (Item { lo = lo1; hi = Z.pred hi0; elt = eltm }))
               (Item { lo = hi0; hi = hi1; elt = eltl })
     in
     let union_update_overlap_items_reverted :
         'a update_handler -> 'a item -> 'a item -> 'a tree =
      fun { f = update } (Item { lo = lo0; hi = hi0; elt = elt0 } as item0)
          (Item { lo = lo1; hi = hi1; elt = elt1 } as item1) ->
       if Z.equal lo0 lo1 then
         if Z.equal hi0 hi1 then
           match update LRl_LRu item0 item1 with
           | Left_overlay -> item0
           | Right_overlay -> item1
           | Merge_overlay elt -> Item { lo = lo0; hi = hi0; elt }
         else if Z.lt hi0 hi1 then
           match update LRl_Lu_Ru item0 item1 with
           | Left_overlay -> Item { lo = lo0; hi = hi1; elt = elt0 }
           | Right_overlay -> item1
           | Merge_overlay elt -> Item { lo = lo0; hi = hi1; elt }
           | Split_FSL (eltf, elts) | Split_FPL (eltf, elts) ->
               join
                 (Item { lo = lo0; hi = Z.pred hi0; elt = eltf })
                 (Item { lo = hi0; hi = hi1; elt = elts })
         else
           match update LRl_Ru_Lu item0 item1 with
           | Left_overlay -> item0
           | Right_overlay -> Item { lo = lo0; hi = hi0; elt = elt1 }
           | Merge_overlay elt -> Item { lo = lo0; hi = hi0; elt }
           | Split_FSL (eltf, elts) | Split_FPL (eltf, elts) ->
               join
                 (Item { lo = lo0; hi = Z.pred hi1; elt = eltf })
                 (Item { lo = hi1; hi = hi0; elt = elts })
       else if Z.equal hi0 hi1 then
         match update Rl_Ll_LRu item0 item1 with
         | Left_overlay -> Item { lo = lo1; hi = hi1; elt = elt0 }
         | Right_overlay -> item1
         | Merge_overlay elt -> Item { lo = lo1; hi = hi1; elt }
         | Split_FSL (eltf, elts) | Split_FPL (eltf, elts) ->
             join
               (Item { lo = lo1; hi = Z.pred lo0; elt = eltf })
               (Item { lo = lo0; hi = hi0; elt = elts })
       else if Z.lt hi0 hi1 then
         match update Rl_Ll_Lu_Ru item0 item1 with
         | Left_overlay -> Item { lo = lo1; hi = hi1; elt = elt0 }
         | Right_overlay -> item1
         | Merge_overlay elt -> Item { lo = lo1; hi = hi1; elt }
         | Split_FSL (eltf, elts) ->
             join
               (Item { lo = lo1; hi = Z.pred lo0; elt = eltf })
               (Item { lo = lo0; hi = hi1; elt = elts })
         | Split_FPL (eltf, elts) ->
             join
               (Item { lo = lo1; hi = Z.pred hi0; elt = eltf })
               (Item { lo = hi0; hi = hi1; elt = elts })
         | Split_FSTL (eltf, eltm, eltl) ->
             join
               (join
                  (Item { lo = lo1; hi = Z.pred lo0; elt = eltf })
                  (Item { lo = lo0; hi = Z.pred hi0; elt = eltm }))
               (Item { lo = hi0; hi = hi1; elt = eltl })
       else
         match update Rl_Ll_Ru_Lu item0 item1 with
         | Left_overlay -> Item { lo = lo1; hi = hi0; elt = elt0 }
         | Right_overlay -> Item { lo = lo1; hi = hi0; elt = elt1 }
         | Merge_overlay elt -> Item { lo = lo1; hi = hi0; elt }
         | Split_FSL (eltf, elts) ->
             join
               (Item { lo = lo1; hi = Z.pred lo0; elt = eltf })
               (Item { lo = lo0; hi = hi0; elt = elts })
         | Split_FPL (eltf, elts) ->
             join
               (Item { lo = lo1; hi = Z.pred hi1; elt = eltf })
               (Item { lo = hi1; hi = hi0; elt = elts })
         | Split_FSTL (eltf, eltm, eltl) ->
             join
               (join
                  (Item { lo = lo1; hi = Z.pred lo0; elt = eltf })
                  (Item { lo = lo0; hi = Z.pred hi1; elt = eltm }))
               (Item { lo = hi1; hi = hi0; elt = eltl })
     in
     let union_update_overlap_items :
         'a update_handler -> 'a item -> 'a item -> bool -> 'a tree =
      fun update item0 item1 swap ->
       if swap then union_update_overlap_items_reverted update item1 item0
       else union_update_overlap_items_ordered update item0 item1
     in
     let rec union_update_ordered :
         'a update_handler -> 'a tree -> 'a tree -> bool -> 'a tree =
      fun update tree0 tree1 swap ->
       if tree0 == tree1 then tree0
       else
         let ub0 = upper_bound tree0 and lb1 = lower_bound tree1 in
         if Z.lt ub0 lb1 then join tree0 tree1
         else union_update_overlap update tree0 tree1 swap
     and union_update_overlap :
         'a update_handler -> 'a tree -> 'a tree -> bool -> 'a tree =
      fun update tree0 tree1 swap ->
       match (tree0, tree1) with
       | (Item _ as item0), (Item _ as item1) ->
           union_update_overlap_items update item0 item1 swap
       | ( (Item { hi = hi0; _ } as item0),
           (Node { zero = zero1; one = one1; _ } as node1) ) ->
           if Z.leq hi0 (upper_bound zero1) then
             join (union_update_overlap update item0 zero1 swap) one1
           else union_update_destruct_left update Root Root item0 node1 swap
       | ( (Node { zero = zero0; one = one0; _ } as node0),
           (Item { lo = lo1; _ } as item1) ) ->
           if Z.leq (upper_bound one0) lo1 then
             join zero0 (union_update_overlap update one0 item1 swap)
           else union_update_destruct_left update Root Root node0 item1 swap
       | ( (Node { hi = hi0; zero = zero0; one = one0; _ } as node0),
           (Node { lo = lo1; zero = zero1; one = one1; _ } as node1) ) ->
           if Z.leq hi0 (upper_bound zero1) then
             join (union_update_overlap update node0 zero1 swap) one1
           else if Z.leq (lower_bound one0) lo1 then
             join zero0 (union_update_overlap update one0 node1 swap)
           else union_update_destruct_left update Root Root node0 node1 swap
     and union_update_destruct_left :
         'a update_handler ->
         'a path ->
         'a path ->
         'a tree ->
         'a tree ->
         bool ->
         'a tree =
      fun update path0 path1 tree0 tree1 swap ->
       match (tree0, tree1) with
       | (Item _ as item0), (Item _ as item1) ->
           union_update_destruct_right update path0 path1
             (union_update_overlap_items update item0 item1 swap)
             swap
       | (Item _ as item0), Node { zero = zero1; one = one1; _ } ->
           union_update_destruct_left update path0
             (Right (path1, one1))
             item0 zero1 swap
       | Node { zero = zero0; one = one0; _ }, (Item { lo = lo1; _ } as item1) ->
           if Z.leq (lower_bound one0) lo1 then
             union_update_destruct_right update path0 path1
               (join zero0 (union_update_overlap update one0 item1 swap))
               swap
           else if Z.lt (upper_bound zero0) lo1 then
             union_update_destruct_right update path0 path1
               (join zero0 (union_update_ordered update item1 one0 (not swap)))
               swap
           else
             union_update_destruct_left update
               (Right (path0, one0))
               path1 zero0 item1 swap
       | ( Node { zero = zero0; one = one0; _ },
           (Node { lo = lo1; zero = zero1; one = one1; _ } as node1) ) ->
           if Z.leq (lower_bound one0) lo1 then
             union_update_destruct_right update path0 path1
               (join zero0 (union_update_overlap update one0 node1 swap))
               swap
           else if Z.lt (upper_bound zero0) lo1 then
             union_update_destruct_right update path0 path1
               (join zero0 (union_update_ordered update node1 one0 (not swap)))
               swap
           else
             union_update_destruct_left update
               (Right (path0, one0))
               (Right (path1, one1))
               zero0 zero1 swap
     and union_update_destruct_right :
         'a update_handler -> 'a path -> 'a path -> 'a tree -> bool -> 'a tree =
       let rec union_update_destruct_right_single :
           'a update_handler -> 'a right -> 'a tree -> bool -> 'a tree =
        fun update (Right (path0, tree0)) acc swap ->
         if Z.lt (upper_bound acc) (lower_bound tree0) then
           union_update_destruct_right_single_disjoint path0 (join acc tree0)
         else
           match path0 with
           | Root -> union_update_overlap update acc tree0 (not swap)
           | Right _ as right0 ->
               union_update_destruct_right_single update right0
                 (union_update_overlap update acc tree0 (not swap))
                 swap
       and union_update_destruct_right_single_disjoint :
           'a path -> 'a tree -> 'a tree =
        fun path0 acc ->
         match path0 with
         | Root -> acc
         | Right (path0, tree0) ->
             union_update_destruct_right_single_disjoint path0 (join acc tree0)
       and union_update_destruct_right_double :
           'a update_handler -> 'a right -> 'a right -> 'a tree -> bool -> 'a tree
           =
        fun update (Right (_, tree0) as right0) (Right (_, tree1) as right1) acc
            swap ->
         if Z.lt (lower_bound tree1) (lower_bound tree0) then
           union_update_destruct_right_double_ordered update right1 right0 acc
             (not swap)
         else
           union_update_destruct_right_double_ordered update right0 right1 acc swap
       and union_update_destruct_right_double_ordered :
           'a update_handler -> 'a right -> 'a right -> 'a tree -> bool -> 'a tree
           =
        fun update (Right (path0, tree0)) (Right (path1, tree1) as right1) acc swap ->
         if Z.lt (upper_bound tree0) (lower_bound tree1) then
           union_update_destruct_right update path0 right1
             (union_update_ordered update acc tree0 (not swap))
             swap
         else
           union_update_destruct_left update path0 path1
             (union_update_ordered update acc tree0 (not swap))
             tree1 swap
       in
       fun update path0 path1 acc swap ->
         match (path0, path1) with
         | Root, Root -> acc
         | Root, (Right _ as right1) ->
             union_update_destruct_right_single update right1 acc (not swap)
         | (Right _ as right0), Root ->
             union_update_destruct_right_single update right0 acc swap
         | (Right _ as right0), (Right _ as right1) ->
             union_update_destruct_right_double update right0 right1 acc swap
     in
     fun update t0 t1 ->
       match (t0, t1) with
       | Empty, t1 -> t1
       | t0, Empty -> t0
       | ( ((Item { lo = lo0; _ } | Node { lo = lo0; _ }) as tree0),
           ((Item { lo = lo1; _ } | Node { lo = lo1; _ }) as tree1) ) ->
           any
             (if Z.lt lo1 lo0 then union_update_ordered update tree1 tree0 true
              else union_update_ordered update tree0 tree1 false) *)

let union_update :
    ?stich:('a item -> 'a item -> 'a option) ->
    ('a item -> 'a item -> 'a t) ->
    'a t ->
    'a t ->
    'a t =
  let union_update_overlap_items :
      ('a item -> 'a item -> 'a t) -> 'a item -> 'a item -> bool -> 'a t =
   fun update item0 item1 swap ->
    if swap then update item1 item0 else update item0 item1
  in
  let rec union_update_ordered :
      stich:('a item -> 'a item -> 'a option) option ->
      ('a item -> 'a item -> 'a t) ->
      'a tree ->
      'a tree ->
      bool ->
      'a t =
   fun ~stich update tree0 tree1 swap ->
    if tree0 == tree1 then any tree0
    else
      let ub0 = upper_bound tree0 and lb1 = lower_bound tree1 in
      if Z.lt ub0 lb1 then any (join ~stich tree0 tree1)
      else union_update_overlap ~stich update tree0 tree1 swap
  and union_update_overlap :
      stich:('a item -> 'a item -> 'a option) option ->
      ('a item -> 'a item -> 'a t) ->
      'a tree ->
      'a tree ->
      bool ->
      'a t =
   fun ~stich update tree0 tree1 swap ->
    match (tree0, tree1) with
    | (Item _ as item0), (Item _ as item1) ->
        union_update_overlap_items update item0 item1 swap
    | ( (Item { hi = hi0; _ } as item0),
        (Node { zero = zero1; one = one1; _ } as node1) ) ->
        if Z.leq hi0 (upper_bound zero1) then
          any
            (join_right ~stich
               (union_update_overlap ~stich update item0 zero1 swap)
               one1)
        else union_update_destruct_left ~stich update Root Root item0 node1 swap
    | ( (Node { zero = zero0; one = one0; _ } as node0),
        (Item { lo = lo1; _ } as item1) ) ->
        if Z.leq (upper_bound one0) lo1 then
          any
            (join_left ~stich zero0
               (union_update_overlap ~stich update one0 item1 swap))
        else union_update_destruct_left ~stich update Root Root node0 item1 swap
    | ( (Node { hi = hi0; zero = zero0; one = one0; _ } as node0),
        (Node { lo = lo1; zero = zero1; one = one1; _ } as node1) ) ->
        if Z.leq hi0 (upper_bound zero1) then
          any
            (join_right ~stich
               (union_update_overlap ~stich update node0 zero1 swap)
               one1)
        else if Z.leq (lower_bound one0) lo1 then
          any
            (join_left ~stich zero0
               (union_update_overlap ~stich update one0 node1 swap))
        else union_update_destruct_left ~stich update Root Root node0 node1 swap
  and union_update_destruct_left :
      stich:('a item -> 'a item -> 'a option) option ->
      ('a item -> 'a item -> 'a t) ->
      'a path ->
      'a path ->
      'a tree ->
      'a tree ->
      bool ->
      'a t =
   fun ~stich update path0 path1 tree0 tree1 swap ->
    match (tree0, tree1) with
    | (Item _ as item0), (Item _ as item1) ->
        union_update_destruct_right ~stich update path0 path1
          (union_update_overlap_items update item0 item1 swap)
          swap
    | (Item _ as item0), Node { zero = zero1; one = one1; _ } ->
        union_update_destruct_left ~stich update path0
          (Right (path1, one1))
          item0 zero1 swap
    | Node { zero = zero0; one = one0; _ }, (Item { lo = lo1; _ } as item1) ->
        if Z.leq (lower_bound one0) lo1 then
          union_update_destruct_right_acc ~stich update path0 path1
            (join_left ~stich zero0
               (union_update_overlap ~stich update one0 item1 swap))
            swap
        else if Z.lt (upper_bound zero0) lo1 then
          union_update_destruct_right_acc ~stich update path0 path1
            (join_left ~stich zero0
               (union_update_ordered ~stich update item1 one0 (not swap)))
            swap
        else
          union_update_destruct_left ~stich update
            (Right (path0, one0))
            path1 zero0 item1 swap
    | ( Node { zero = zero0; one = one0; _ },
        (Node { lo = lo1; zero = zero1; one = one1; _ } as node1) ) ->
        if Z.leq (lower_bound one0) lo1 then
          union_update_destruct_right_acc ~stich update path0 path1
            (join_left ~stich zero0
               (union_update_overlap ~stich update one0 node1 swap))
            swap
        else if Z.lt (upper_bound zero0) lo1 then
          union_update_destruct_right_acc ~stich update path0 path1
            (join_left ~stich zero0
               (union_update_ordered ~stich update node1 one0 (not swap)))
            swap
        else
          union_update_destruct_left ~stich update
            (Right (path0, one0))
            (Right (path1, one1))
            zero0 zero1 swap
  and union_update_destruct_right :
      stich:('a item -> 'a item -> 'a option) option ->
      ('a item -> 'a item -> 'a t) ->
      'a path ->
      'a path ->
      'a t ->
      bool ->
      'a t =
   fun ~stich update path0 path1 acc swap ->
    match (path0, path1) with
    | Root, Root -> acc
    | Root, (Right _ as right1) ->
        union_update_destruct_right_single ~stich update right1 acc (not swap)
    | (Right _ as right0), Root ->
        union_update_destruct_right_single ~stich update right0 acc swap
    | (Right _ as right0), (Right _ as right1) ->
        union_update_destruct_right_double ~stich update right0 right1 acc swap
  and union_update_destruct_right_acc :
      stich:('a item -> 'a item -> 'a option) option ->
      ('a item -> 'a item -> 'a t) ->
      'a path ->
      'a path ->
      'a tree ->
      bool ->
      'a t =
   fun ~stich update path0 path1 acc swap ->
    match (path0, path1) with
    | Root, Root -> any acc
    | Root, (Right _ as right1) ->
        union_update_destruct_right_single_acc ~stich update right1 acc
          (not swap)
    | (Right _ as right0), Root ->
        union_update_destruct_right_single_acc ~stich update right0 acc swap
    | (Right _ as right0), (Right _ as right1) ->
        union_update_destruct_right_double ~stich update right0 right1 (any acc)
          swap
  and union_update_destruct_right_single :
      stich:('a item -> 'a item -> 'a option) option ->
      ('a item -> 'a item -> 'a t) ->
      'a right ->
      'a t ->
      bool ->
      'a t =
   fun ~stich update (Right (path0, tree0) as right0) acc swap ->
    match acc with
    | Empty -> union_update_destruct_right_single_disjoint ~stich path0 tree0
    | (Item _ | Node _) as acc ->
        union_update_destruct_right_single_acc ~stich update right0 acc swap
  and union_update_destruct_right_single_acc :
      stich:('a item -> 'a item -> 'a option) option ->
      ('a item -> 'a item -> 'a t) ->
      'a right ->
      'a tree ->
      bool ->
      'a t =
   fun ~stich update (Right (path0, tree0)) acc swap ->
    if Z.lt (upper_bound acc) (lower_bound tree0) then
      union_update_destruct_right_single_disjoint ~stich path0
        (join ~stich acc tree0)
    else
      match path0 with
      | Root -> union_update_overlap ~stich update acc tree0 (not swap)
      | Right _ as right0 ->
          union_update_destruct_right_single ~stich update right0
            (union_update_overlap ~stich update acc tree0 (not swap))
            swap
  and union_update_destruct_right_single_disjoint :
      stich:('a item -> 'a item -> 'a option) option ->
      'a path ->
      'a tree ->
      'a t =
   fun ~stich path0 acc ->
    match path0 with
    | Root -> any acc
    | Right (path0, tree0) ->
        union_update_destruct_right_single_disjoint ~stich path0
          (join ~stich acc tree0)
  and union_update_destruct_right_double :
      stich:('a item -> 'a item -> 'a option) option ->
      ('a item -> 'a item -> 'a t) ->
      'a right ->
      'a right ->
      'a t ->
      bool ->
      'a t =
   fun ~stich update (Right (_, tree0) as right0) (Right (_, tree1) as right1)
       acc swap ->
    if Z.lt (lower_bound tree1) (lower_bound tree0) then
      union_update_destruct_right_double_ordered ~stich update right1 right0 acc
        (not swap)
    else
      union_update_destruct_right_double_ordered ~stich update right0 right1 acc
        swap
  and union_update_destruct_right_double_ordered :
      stich:('a item -> 'a item -> 'a option) option ->
      ('a item -> 'a item -> 'a t) ->
      'a right ->
      'a right ->
      'a t ->
      bool ->
      'a t =
   fun ~stich update (Right (path0, tree0)) (Right (path1, tree1) as right1) acc
       swap ->
    if Z.lt (upper_bound tree0) (lower_bound tree1) then
      match acc with
      | Empty ->
          union_update_destruct_right_acc ~stich update path0 right1 tree0 swap
      | (Item _ | Node _) as acc ->
          union_update_destruct_right ~stich update path0 right1
            (union_update_ordered ~stich update acc tree0 (not swap))
            swap
    else
      match acc with
      | Empty ->
          union_update_destruct_left ~stich update path0 path1 tree0 tree1 swap
      | (Item _ | Node _) as acc -> (
          match union_update_ordered ~stich update acc tree0 (not swap) with
          | Empty ->
              union_update_destruct_right ~stich update path0 right1 Empty swap
          | (Item _ | Node _) as acc ->
              union_update_destruct_left ~stich update path0 path1 acc tree1
                swap)
  in
  fun ?stich update t0 t1 ->
    match (t0, t1) with
    | Empty, t1 -> t1
    | t0, Empty -> t0
    | ( ((Item { lo = lo0; _ } | Node { lo = lo0; _ }) as tree0),
        ((Item { lo = lo1; _ } | Node { lo = lo1; _ }) as tree1) ) ->
        if Z.lt lo1 lo0 then union_update_ordered ~stich update tree1 tree0 true
        else union_update_ordered ~stich update tree0 tree1 false

(* let union_eq : ('a item -> 'a item -> 'a) -> 'a t -> 'a t -> 'a t =
   let union_eq_overlap_items :
       ('a item -> 'a item -> 'a) -> 'a item -> 'a item -> 'a tree =
    fun merge (Item { lo = lo0; hi = hi0; elt = elt0 } as item0)
        (Item { hi = hi1; elt = elt1; _ } as item1) ->
     if Z.leq hi1 hi0 && elt0 == elt1 then item0
     else Item { lo = lo0; hi = Z.max hi0 hi1; elt = merge item0 item1 }
   in
   let rec union_eq_ordered :
       ('a item -> 'a item -> 'a) -> 'a tree -> 'a tree -> 'a tree =
    fun merge tree0 tree1 ->
     (* invariant: lower_bound(tree0) <= lower_bound(tree1) *)
     (* assert (Z.leq (lower_bound tree0) (lower_bound tree1)); *)
     if tree0 == tree1 then tree0
     else
       let ub0 = upper_bound tree0 and lb1 = lower_bound tree1 in
       if Z.lt ub0 lb1 then join tree0 tree1
       else union_eq_overlap merge tree0 tree1
   and union_eq_overlap :
       ('a item -> 'a item -> 'a) -> 'a tree -> 'a tree -> 'a tree =
    fun merge tree0 tree1 ->
     match (tree0, tree1) with
     | (Item _ as item0), (Item _ as item1) ->
         union_eq_overlap_items merge item0 item1
     | ( (Item { hi = hi0; _ } as item0),
         (Node { zero = zero1; one = one1; _ } as node1) ) ->
         if Z.leq hi0 (upper_bound zero1) then
           join (union_eq_overlap merge item0 zero1) one1
         else union_eq_destruct_left merge Root Root item0 node1
     | ( (Node { zero = zero0; one = one0; _ } as node0),
         (Item { lo = lo1; _ } as item1) ) ->
         if Z.leq (upper_bound one0) lo1 then
           join zero0 (union_eq_overlap merge one0 item1)
         else union_eq_destruct_left merge Root Root node0 item1
     | ( (Node { hi = hi0; zero = zero0; one = one0; _ } as node0),
         (Node { lo = lo1; zero = zero1; one = one1; _ } as node1) ) ->
         if Z.leq hi0 (upper_bound zero1) then
           join (union_eq_overlap merge node0 zero1) one1
         else if Z.leq (lower_bound one0) lo1 then
           join zero0 (union_eq_overlap merge one0 node1)
         else union_eq_destruct_left merge Root Root node0 node1
   and union_eq_destruct_left :
       ('a item -> 'a item -> 'a) ->
       'a path ->
       'a path ->
       'a tree ->
       'a tree ->
       'a tree =
    fun merge path0 path1 tree0 tree1 ->
     match (tree0, tree1) with
     | (Item _ as item0), (Item _ as item1) ->
         union_eq_destruct_right merge path0 path1
           (union_eq_overlap_items merge item0 item1)
     | (Item _ as item0), Node { zero = zero1; one = one1; _ } ->
         union_eq_destruct_left merge path0 (Right (path1, one1)) item0 zero1
     | Node { zero = zero0; one = one0; _ }, (Item { lo = lo1; _ } as item1) ->
         if Z.leq (lower_bound one0) lo1 then
           union_eq_destruct_right merge path0 path1
             (join zero0 (union_eq_overlap merge one0 item1))
         else if Z.lt (upper_bound zero0) lo1 then
           union_eq_destruct_right merge path0 path1
             (join zero0 (union_eq_ordered merge item1 one0))
         else
           union_eq_destruct_left merge (Right (path0, one0)) path1 zero0 item1
     | ( Node { zero = zero0; one = one0; _ },
         (Node { lo = lo1; zero = zero1; one = one1; _ } as node1) ) ->
         if Z.leq (lower_bound one0) lo1 then
           union_eq_destruct_right merge path0 path1
             (join zero0 (union_eq_overlap merge one0 node1))
         else if Z.lt (upper_bound zero0) lo1 then
           union_eq_destruct_right merge path0 path1
             (join zero0 (union_eq_ordered merge node1 one0))
         else
           union_eq_destruct_left merge
             (Right (path0, one0))
             (Right (path1, one1))
             zero0 zero1
   and union_eq_destruct_right :
       ('a item -> 'a item -> 'a) -> 'a path -> 'a path -> 'a tree -> 'a tree =
     let rec union_eq_destruct_right_single :
         ('a item -> 'a item -> 'a) -> 'a right -> 'a tree -> 'a tree =
      fun merge (Right (path0, tree0)) acc ->
       if Z.lt (upper_bound acc) (lower_bound tree0) then
         union_eq_destruct_right_single_disjoint path0 (join acc tree0)
       else
         match path0 with
         | Root -> union_eq_overlap merge acc tree0
         | Right _ as right0 ->
             union_eq_destruct_right_single merge right0
               (union_eq_overlap merge acc tree0)
     and union_eq_destruct_right_single_disjoint : 'a path -> 'a tree -> 'a tree
         =
      fun path0 acc ->
       match path0 with
       | Root -> acc
       | Right (path0, tree0) ->
           union_eq_destruct_right_single_disjoint path0 (join acc tree0)
     and union_eq_destruct_right_double :
         ('a item -> 'a item -> 'a) -> 'a right -> 'a right -> 'a tree -> 'a tree
         =
      fun merge (Right (_, tree0) as right0) (Right (_, tree1) as right1) acc ->
       if Z.lt (lower_bound tree1) (lower_bound tree0) then
         union_eq_destruct_right_double_ordered merge right1 right0 acc
       else union_eq_destruct_right_double_ordered merge right0 right1 acc
     and union_eq_destruct_right_double_ordered :
         ('a item -> 'a item -> 'a) -> 'a right -> 'a right -> 'a tree -> 'a tree
         =
      fun merge (Right (path0, tree0)) (Right (path1, tree1) as right1) acc ->
       if Z.lt (upper_bound tree0) (lower_bound tree1) then
         union_eq_destruct_right merge path0 right1
           (union_eq_ordered merge acc tree0)
       else
         union_eq_destruct_left merge path0 path1
           (union_eq_ordered merge acc tree0)
           tree1
     in
     fun merge path0 path1 acc ->
       match (path0, path1) with
       | Root, Root -> acc
       | Root, (Right _ as right1) ->
           union_eq_destruct_right_single merge right1 acc
       | (Right _ as right0), Root ->
           union_eq_destruct_right_single merge right0 acc
       | (Right _ as right0), (Right _ as right1) ->
           union_eq_destruct_right_double merge right0 right1 acc
   in
   fun merge t0 t1 ->
     match (t0, t1) with
     | Empty, t1 -> t1
     | t0, Empty -> t0
     | ( ((Item { lo = lo0; _ } | Node { lo = lo0; _ }) as tree0),
         ((Item { lo = lo1; _ } | Node { lo = lo1; _ }) as tree1) ) ->
         any
           (if Z.lt lo1 lo0 then union_eq_ordered merge tree1 tree0
            else union_eq_ordered merge tree0 tree1) *)

let union_eq :
    ?stich:('a item -> 'a item -> 'a option) ->
    ('a item -> 'a item -> 'a) ->
    'a t ->
    'a t ->
    'a t =
  let union : ('a item -> 'a item -> 'a) -> 'a item -> 'a item -> 'a t =
   fun merge (Item { lo = lo0; hi = hi0; elt = elt0 } as item0)
       (Item { lo = lo1; hi = hi1; elt = elt1; _ } as item1) ->
    if elt0 == elt1 then
      if Z.leq lo0 lo1 then
        if Z.leq hi1 hi0 then item0 else Item { lo = lo0; hi = hi1; elt = elt0 }
      else if Z.leq hi0 hi1 then item1
      else Item { lo = lo1; hi = hi0; elt = elt0 }
    else
      Item { lo = Z.min lo0 lo1; hi = Z.max hi0 hi1; elt = merge item0 item1 }
  in
  fun ?stich merge t0 t1 -> union_update ?stich (union merge) t0 t1

let union_left :
    ?stich:('a item -> 'a item -> 'a option) ->
    ?crop:(lo:Z.t -> hi:Z.t -> 'a -> 'a) ->
    'a t ->
    'a t ->
    'a t =
  let keep_left : (lo:Z.t -> hi:Z.t -> 'a -> 'a) -> 'a item -> 'a item -> 'a t =
   fun crop (Item { lo = lo0; hi = hi0; _ } as item0)
       (Item { lo = lo1; hi = hi1; elt = elt1 }) ->
    if Z.leq lo0 lo1 then
      if Z.leq hi1 hi0 then (* Ll Rl Rh Lh  *)
        item0
      else
        (* Ll Rl Lh Rh *)
        let loa = Z.succ hi0 in
        any
          (join ~stich:None item0
             (Item
                {
                  lo = loa;
                  hi = hi1;
                  elt = crop ~lo:(Z.sub loa lo1) ~hi:(Z.sub hi1 lo1) elt1;
                }))
    else
      let hib = Z.pred lo0 in
      if Z.leq hi1 hi0 then
        (* Rl Ll Rh Lh  *)
        any
          (join ~stich:None
             (Item
                {
                  lo = lo1;
                  hi = hib;
                  elt = crop ~lo:Z.zero ~hi:(Z.sub hib lo1) elt1;
                })
             item0)
      else
        (* Rl Ll Lh Rh *)
        let loa = Z.succ hi0 in
        any
          (join ~stich:None
             (join ~stich:None
                (Item
                   {
                     lo = lo1;
                     hi = hib;
                     elt = crop ~lo:Z.zero ~hi:(Z.sub hib lo1) elt1;
                   })
                item0)
             (Item
                {
                  lo = loa;
                  hi = hi1;
                  elt = crop ~lo:(Z.sub loa lo1) ~hi:(Z.sub hi1 lo1) elt1;
                }))
  in
  fun ?stich ?(crop = fun ~lo:_ ~hi:_ a -> a) t0 t1 ->
    union_update ?stich (keep_left crop) t0 t1

(* let union_split :
     ?crop:(lo:Z.t -> hi:Z.t -> 'a -> 'a) ->
     ('a item -> 'a item -> 'a) ->
     'a t ->
     'a t ->
     'a t =
   let split :
       (lo:Z.t -> hi:Z.t -> 'a -> 'a) ->
       ('a item -> 'a item -> 'a) ->
       'a item ->
       'a item ->
       'a t =
    fun crop merge (Item { lo = lo0; hi = hi0; elt = elt0 } as item0)
        (Item { lo = lo1; hi = hi1; elt = elt1 } as item1) ->
     if Z.leq lo0 lo1 then
       if Z.equal lo0 lo1 then
         if Z.leq hi0 hi1 then
           if Z.equal hi0 hi1 then
           (Item
                    {
                      lo = lo0;
                      hi = hi0;
                      elt = merge item0 item1;
                    })
           else assert false
         else assert false
       else if Z.leq hi1 hi0 then
         (* Ll Rl Rh Lh  *)
         let hib = Z.pred lo1 and loa = Z.succ hi1 in
         any
           (join
              (join
                 (Item
                    {
                      lo = lo0;
                      hi = hib;
                      elt = crop ~lo:Z.zero ~hi:(Z.sub hib lo0) elt0;
                    })
                 (Item { lo = lo1; hi = hi1; elt = merge item0 item1 }))
              (Item
                 {
                   lo = loa;
                   hi = hi0;
                   elt = crop ~lo:(Z.sub loa lo0) ~hi:(Z.sub hi0 lo0) elt0;
                 }))
       else
         (* Ll Rl Lh Rh *)
         let loa = Z.succ hi0 in
         any
           (join item0
              (Item
                 {
                   lo = loa;
                   hi = hi1;
                   elt = crop ~lo:(Z.sub loa lo1) ~hi:(Z.sub hi1 lo1) elt1;
                 }))
     else
       let hib = Z.pred lo0 in
       if Z.leq hi1 hi0 then
         (* Rl Ll Rh Lh  *)
         any
           (join
              (Item
                 {
                   lo = lo1;
                   hi = hib;
                   elt = crop ~lo:Z.zero ~hi:(Z.sub hib lo1) elt1;
                 })
              item0)
       else
         (* Rl Ll Lh Rh *)
         let loa = Z.succ hi0 in
         any
           (join
              (join
                 (Item
                    {
                      lo = lo1;
                      hi = hib;
                      elt = crop ~lo:Z.zero ~hi:(Z.sub hib lo1) elt1;
                    })
                 item0)
              (Item
                 {
                   lo = loa;
                   hi = hi1;
                   elt = crop ~lo:(Z.sub loa lo1) ~hi:(Z.sub hi1 lo1) elt1;
                 }))
   in
   fun ?(crop = fun ~lo:_ ~hi:_ a -> a) merge t0 t1 ->
     union_update (split crop merge) t0 t1 *)

let disjoint : type a b. a t -> b t -> bool =
  let rec disjoint_ordered : type a b. a tree -> b tree -> bool =
   fun tree0 tree1 ->
    let ub0 = upper_bound tree0 and lb1 = lower_bound tree1 in
    Z.lt ub0 lb1
    ||
    match (tree0, tree1) with
    | Item _, Item _ -> false
    | Item { lo = lb0; _ }, Node { zero = zero1; one = one1; _ } ->
        is_empty_between_choice lb0 ub0 zero1 one1
    | Node { zero = zero0; one = one0; _ }, Item { hi = ub1; _ } ->
        is_empty_between_choice lb1 ub1 zero0 one0
    | ( (Node { zero = zero0; one = one0; _ } as node0),
        (Node { zero = zero1; one = one1; _ } as node1) ) ->
        if Z.leq (lower_bound one0) lb1 then disjoint_ordered one0 node1
        else if Z.lt ub0 (lower_bound one1) then disjoint_ordered node0 zero1
        else disjoint_ordered zero0 node1 && disjoint_ordered node1 one0
  in
  fun t0 t1 ->
    match (t0, t1) with
    | Empty, _ | _, Empty -> true
    | ( ((Item { lo = lo0; _ } | Node { lo = lo0; _ }) as tree0),
        ((Item { lo = lo1; _ } | Node { lo = lo1; _ }) as tree1) ) ->
        if Z.lt lo1 lo0 then disjoint_ordered tree1 tree0
        else disjoint_ordered tree0 tree1

let substract :
    type a b. ?crop:(lo:Z.t -> hi:Z.t -> a -> a) -> a t -> b t -> a t =
  let union_disjoint : type a. a t -> a t -> a t =
   fun t0 t1 ->
    match (t0, t1) with
    | Empty, t1 -> t1
    | t0, Empty -> t0
    | ((Item _ | Node _) as tree0), ((Item _ | Node _) as tree1) ->
        any (join ~stich:None tree0 tree1)
  in
  let rec substract :
      type a b. crop:(lo:Z.t -> hi:Z.t -> a -> a) -> a tree -> b tree -> a t =
   fun ~crop tree0 tree1 ->
    let ub0 = upper_bound tree0 and lb1 = lower_bound tree1 in
    if Z.lt ub0 lb1 then any tree0
    else
      let lb0 = lower_bound tree0 and ub1 = upper_bound tree1 in
      if Z.lt ub1 lb0 then any tree0
      else
        match (tree0, tree1) with
        | ( Item { lo = lo0; hi = hi0; elt = elt0 },
            Item { lo = lo1; hi = hi1; _ } ) ->
            if Z.leq lo1 lo0 then
              if Z.leq hi0 hi1 then Empty
              else
                let loa = Z.succ hi1 in
                Item
                  {
                    lo = loa;
                    hi = hi0;
                    elt = crop ~lo:(Z.sub loa lo0) ~hi:(Z.sub hi0 lo0) elt0;
                  }
            else if Z.leq hi0 hi1 then
              let hib = Z.pred lo1 in
              Item
                {
                  lo = lo0;
                  hi = hib;
                  elt = crop ~lo:Z.zero ~hi:(Z.sub hib lo0) elt0;
                }
            else
              let loa = Z.succ hi1 and hib = Z.pred lo1 in
              any
                (join ~stich:None
                   (Item
                      {
                        lo = lo0;
                        hi = hib;
                        elt = crop ~lo:Z.zero ~hi:(Z.sub hib lo0) elt0;
                      })
                   (Item
                      {
                        lo = loa;
                        hi = hi0;
                        elt = crop ~lo:(Z.sub loa lo0) ~hi:(Z.sub hi0 lo0) elt0;
                      }))
        | (Item _ as item0), Node { zero = zero1; one = one1; _ } -> (
            let item0' = substract ~crop item0 zero1 in
            match item0' with
            | Empty -> Empty
            | (Item _ | Node _) as tree0' -> substract ~crop tree0' one1)
        | ( (Node { zero = zero0; one = one0; _ } as node0),
            ((Item _ | Node _) as tree1) ) ->
            let zero0' = substract ~crop zero0 tree1
            and one0' = substract ~crop one0 tree1 in
            if any zero0 == zero0' && any one0 == one0' then node0
            else union_disjoint zero0' one0'
  in
  fun ?(crop = fun ~lo:_ ~hi:_ a -> a) t0 t1 ->
    match (t0, t1) with
    | Empty, _ -> Empty
    | t0, Empty -> t0
    | ((Item _ | Node _) as tree0), ((Item _ | Node _) as tree1) ->
        substract ~crop tree0 tree1

let fold_inter :
    type a b. (a item -> b item -> 'c -> 'c) -> 'c -> a t -> b t -> 'c =
  let rec fold_inter :
      type a b. (a item -> b item -> 'c -> 'c) -> 'c -> a tree -> b tree -> 'c =
   fun f acc tree0 tree1 ->
    let ub0 = upper_bound tree0 and lb1 = lower_bound tree1 in
    if Z.lt ub0 lb1 then acc
    else
      let lb0 = lower_bound tree0 and ub1 = upper_bound tree1 in
      if Z.lt ub1 lb0 then acc
      else
        match (tree0, tree1) with
        | (Item _ as item0), (Item _ as item1) -> f item0 item1 acc
        | (Item _ as item0), Node { zero = zero1; one = one1; _ } ->
            fold_inter f (fold_inter f acc item0 zero1) item0 one1
        | Node { zero = zero0; one = one0; _ }, ((Item _ | Node _) as tree1) ->
            fold_inter f (fold_inter f acc zero0 tree1) one0 tree1
  in
  fun f acc t0 t1 ->
    match (t0, t1) with
    | Empty, _ | _, Empty -> acc
    | ((Item _ | Node _) as tree0), ((Item _ | Node _) as tree1) ->
        fold_inter f acc tree0 tree1
