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

type ('a, _) node = private
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

val empty : 'a t
val is_empty : 'a t -> bool
val is_empty_between : Z.t -> Z.t -> 'a t -> bool
val disjoint : 'a t -> 'b t -> bool
val singleton : lo:Z.t -> hi:Z.t -> 'a -> 'a t

val union_eq :
  ?stich:('a item -> 'a item -> 'a option) ->
  ('a item -> 'a item -> 'a) ->
  'a t ->
  'a t ->
  'a t

val union_left :
  ?stich:('a item -> 'a item -> 'a option) ->
  ?crop:(lo:Z.t -> hi:Z.t -> 'a -> 'a) ->
  'a t ->
  'a t ->
  'a t

val union_update :
  ?stich:('a item -> 'a item -> 'a option) ->
  ('a item -> 'a item -> 'a t) ->
  'a t ->
  'a t ->
  'a t

val find : Z.t -> 'a t -> 'a item
val find_opt : Z.t -> 'a t -> 'a item_opt
val none : 'a item_opt
val mem : Z.t -> 'a t -> bool
val iter : ('a item -> unit) -> 'a t -> unit
val rev_iter : ('a item -> unit) -> 'a t -> unit
val fold : ('a item -> 'b -> 'b) -> 'b -> 'a t -> 'b
val rev_fold : ('a item -> 'b -> 'b) -> 'b -> 'a t -> 'b
val map : ('a item -> 'b) -> 'a t -> 'b t
val choose : 'a t -> 'a item
val bindings : 'a t -> 'a item list
val substract : ?crop:(lo:Z.t -> hi:Z.t -> 'a -> 'a) -> 'a t -> 'b t -> 'a t
val fold_inter : ('a item -> 'b item -> 'c -> 'c) -> 'c -> 'a t -> 'b t -> 'c
