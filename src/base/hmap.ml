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

module type S = sig
  type key
  type !'a t

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val union_eq : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  val union_map_eq :
    (key -> 'a -> 'a -> 'a) -> (key -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  val freeze : 'a t -> unit
  val bindings : 'a t -> (key * 'a) list
  val choose : 'a t -> key * 'a
  val mem : key -> 'a t -> bool
  val find : key -> 'a t -> 'a
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val is_empty : 'a t -> bool
end

module Make (H : sig
  type t

  val hash : t -> int
  val compare : t -> t -> int
end) : S with type key = H.t = struct
  type key = H.t

  type !'a bucketlist =
    | Empty
    | Cons of {
        mutable frozen : bool;
        mutable key : key;
        mutable data : 'a;
        mutable next : 'a bucketlist;
      }

  type !'a t =
    | Empty
    | Node of {
        mutable frozen : bool;
        mutable left : 'a t;
        mutable right : 'a t;
        mutable bucket : 'a bucketlist;
        mutable hash : int;
      }

  (* most significant bit of difference betweeb two hash *)
  let diff h h' = Z.numbits (Z.of_int (h lxor h'))
  let trailing_zeros h = Z.trailing_zeros (Z.of_int h)
  let empty : 'a t = Empty

  let b_freeze : 'a bucketlist -> unit = function
    | Empty -> ()
    | Cons c -> c.frozen <- true

  let freeze : 'a t -> unit = function
    | Empty -> ()
    | Node n -> n.frozen <- true

  let rec b_i_add : key -> 'a -> 'a bucketlist -> 'a bucketlist =
   fun key' data' t ->
    match t with
    | Empty -> Cons { frozen = false; key = key'; data = data'; next = Empty }
    | Cons { key; data; next; _ } ->
        let i = H.compare key key' in
        if i >= 0 then (
          let next' = if i = 0 then next else t in
          b_freeze next';
          Cons { frozen = false; key = key'; data = data'; next = next' })
        else Cons { frozen = false; key; data; next = b_i_add key' data' next }

  let rec b_m_add : key -> 'a -> 'a bucketlist -> 'a bucketlist =
   fun key' data' t ->
    match t with
    | Empty | Cons { frozen = true; _ } -> b_i_add key' data' t
    | Cons r ->
        let i = H.compare r.key key' in
        if i = 0 then (
          r.data <- data';
          t)
        else if i > 0 then
          Cons { frozen = false; key = key'; data = data'; next = t }
        else (
          r.next <- b_m_add key' data' r.next;
          t)

  let rec n_i_add : key -> 'a -> 'a t -> int -> 'a t =
   fun key' data' t hash' ->
    match t with
    | Empty ->
        Node
          {
            frozen = false;
            bucket =
              Cons { frozen = false; key = key'; data = data'; next = Empty };
            hash = hash';
            left = Empty;
            right = Empty;
          }
    | Node { left; right; bucket; hash; _ } ->
        b_freeze bucket;
        freeze left;
        freeze right;
        if hash = hash' then
          Node
            {
              frozen = false;
              left;
              right;
              bucket = b_i_add key' data' bucket;
              hash;
            }
        else if hash > hash' then
          let n = diff hash hash' and z' = trailing_zeros hash' in
          if z' >= n then
            Node
              {
                frozen = false;
                left = Empty;
                right = t;
                bucket =
                  Cons
                    { frozen = false; key = key'; data = data'; next = Empty };
                hash = hash';
              }
          else
            let z = trailing_zeros hash in
            if z + 1 >= n then
              Node
                {
                  frozen = false;
                  left = n_i_add key' data' left hash';
                  right;
                  bucket;
                  hash;
                }
            else
              Node
                {
                  frozen = false;
                  left =
                    Node
                      {
                        frozen = false;
                        left = Empty;
                        right = Empty;
                        bucket =
                          Cons
                            {
                              frozen = false;
                              key = key';
                              data = data';
                              next = Empty;
                            };
                        hash = hash';
                      };
                  right = t;
                  bucket = Empty;
                  hash = hash land (-1 lsl (n - 1));
                }
        else
          let n = diff hash hash' and z = trailing_zeros hash in
          if z >= n then
            Node
              {
                frozen = false;
                left;
                right = n_i_add key' data' right hash';
                bucket;
                hash;
              }
          else
            let z' = trailing_zeros hash' in
            if z' + 1 >= n then
              Node
                {
                  frozen = false;
                  left = t;
                  right = Empty;
                  bucket =
                    Cons
                      { frozen = false; key = key'; data = data'; next = Empty };
                  hash = hash';
                }
            else
              Node
                {
                  frozen = false;
                  left = t;
                  right =
                    Node
                      {
                        frozen = false;
                        left = Empty;
                        right = Empty;
                        bucket =
                          Cons
                            {
                              frozen = false;
                              key = key';
                              data = data';
                              next = Empty;
                            };
                        hash = hash';
                      };
                  bucket = Empty;
                  hash = hash' land (-1 lsl (n - 1));
                }

  let rec n_m_add : key -> 'a -> 'a t -> int -> 'a t =
   fun key' data' t hash' ->
    match t with
    | Empty | Node { frozen = true; _ } -> n_i_add key' data' t hash'
    | Node ({ bucket; hash; _ } as r) ->
        if r.hash = hash' then (
          r.bucket <- b_m_add key' data' bucket;
          t)
        else if hash > hash' then
          let n = diff hash hash' and z' = trailing_zeros hash' in
          if z' >= n then
            Node
              {
                frozen = false;
                left = Empty;
                right = t;
                bucket =
                  Cons
                    { frozen = false; key = key'; data = data'; next = Empty };
                hash = hash';
              }
          else
            let z = trailing_zeros hash in
            if z + 1 >= n then (
              r.left <- n_m_add key' data' r.left hash';
              t)
            else
              Node
                {
                  frozen = false;
                  left =
                    Node
                      {
                        frozen = false;
                        left = Empty;
                        right = Empty;
                        bucket =
                          Cons
                            {
                              frozen = false;
                              key = key';
                              data = data';
                              next = Empty;
                            };
                        hash = hash';
                      };
                  right = t;
                  bucket = Empty;
                  hash = hash land (-1 lsl (n - 1));
                }
        else
          let n = diff hash hash' and z = trailing_zeros hash in
          if z >= n then (
            r.right <- n_m_add key' data' r.right hash';
            t)
          else
            let z' = trailing_zeros hash' in
            if z' + 1 >= n then
              Node
                {
                  frozen = false;
                  left = t;
                  right = Empty;
                  bucket =
                    Cons
                      { frozen = false; key = key'; data = data'; next = Empty };
                  hash = hash';
                }
            else
              Node
                {
                  frozen = false;
                  left = t;
                  right =
                    Node
                      {
                        frozen = false;
                        left = Empty;
                        right = Empty;
                        bucket =
                          Cons
                            {
                              frozen = false;
                              key = key';
                              data = data';
                              next = Empty;
                            };
                        hash = hash';
                      };
                  bucket = Empty;
                  hash = hash' land (-1 lsl (n - 1));
                }

  let add : key -> 'a -> 'a t -> 'a t =
   fun key data t -> n_m_add key data t (H.hash key)

  let singleton key data =
    Node
      {
        frozen = false;
        left = Empty;
        right = Empty;
        bucket = Cons { frozen = false; key; data; next = Empty };
        hash = H.hash key;
      }

  let rec b_union_eq :
      (key -> 'a -> 'a -> 'a) -> 'a bucketlist -> 'a bucketlist -> 'a bucketlist
      =
   fun f t t' ->
    if t == t' then (
      b_freeze t;
      t)
    else
      match (t, t') with
      | Empty, (Empty (* unreachable *) | Cons _) ->
          b_freeze t';
          t'
      | Cons _, Empty ->
          b_freeze t;
          t
      | ( Cons { key; data; next; _ },
          Cons { key = key'; data = data'; next = next'; _ } ) ->
          let i = H.compare key key' in
          if i = 0 then
            Cons
              {
                frozen = false;
                key;
                data = f key data data';
                next = b_union_eq f next next';
              }
          else if i < 0 then
            Cons { frozen = false; key; data; next = b_union_eq f next t' }
          else
            Cons
              {
                frozen = false;
                key = key';
                data = data';
                next = b_union_eq f t next';
              }

  let rec union_eq : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t =
   fun f t t' ->
    if t == t' then (
      freeze t;
      t)
    else
      match (t, t') with
      | Empty, (Empty (* unreachable *) | Node _) ->
          freeze t';
          t'
      | Node _, Empty ->
          freeze t;
          t
      | ( Node { left; right; bucket; hash; _ },
          Node
            { left = left'; right = right'; bucket = bucket'; hash = hash'; _ }
        ) ->
          if hash = hash' then
            Node
              {
                frozen = false;
                left = union_eq f left left';
                right = union_eq f right right';
                bucket = b_union_eq f bucket bucket';
                hash;
              }
          else if hash > hash' then
            let n = diff hash hash' and z' = trailing_zeros hash' in
            if z' >= n then
              Node
                {
                  frozen = false;
                  left = left';
                  right = union_eq f t right';
                  bucket = bucket';
                  hash = hash';
                }
            else
              let z = trailing_zeros hash in
              if z + 1 >= n then
                Node
                  {
                    frozen = false;
                    left = union_eq f left t';
                    right;
                    bucket;
                    hash;
                  }
              else (
                freeze t;
                freeze t';
                Node
                  {
                    frozen = false;
                    left = t';
                    right = t;
                    bucket = Empty;
                    hash = hash land (-1 lsl (n - 1));
                  })
          else
            let n = diff hash hash' and z = trailing_zeros hash in
            if z >= n then
              Node
                {
                  frozen = false;
                  left;
                  right = union_eq f right t';
                  bucket;
                  hash;
                }
            else
              let z' = trailing_zeros hash' in
              if z' + 1 >= n then
                Node
                  {
                    frozen = false;
                    left = union_eq f t left';
                    right = right';
                    bucket = bucket';
                    hash = hash';
                  }
              else (
                freeze t;
                freeze t';
                Node
                  {
                    frozen = false;
                    left = t;
                    right = t';
                    bucket = Empty;
                    hash = hash' land (-1 lsl (n - 1));
                  })

  let rec b_bindings : (key * 'a) list -> 'a bucketlist -> (key * 'a) list =
   fun acc t ->
    match t with
    | Empty -> acc
    | Cons { key; data; next; _ } -> (key, data) :: b_bindings acc next

  let rec n_bindings : (key * 'a) list -> 'a t -> (key * 'a) list =
   fun acc t ->
    match t with
    | Empty -> acc
    | Node { left; bucket; right; _ } ->
        n_bindings (b_bindings (n_bindings acc right) bucket) left

  let bindings : 'a t -> (key * 'a) list = fun t -> n_bindings [] t

  let rec choose : 'a t -> key * 'a = function
    | Empty -> raise Not_found
    | Node { bucket = Cons { key; data; _ }; _ } -> (key, data)
    | Node { left = Empty; right; _ } -> choose right
    | Node { left; _ } -> choose left

  let rec b_mem : key -> 'a bucketlist -> bool =
   fun key' t ->
    match t with
    | Empty -> false
    | Cons { key; next; _ } ->
        let i = H.compare key key' in
        i = 0 || (i < 0 && b_mem key' next)

  let rec n_mem : key -> 'a t -> int -> bool =
   fun key' t hash' ->
    match t with
    | Empty -> false
    | Node { left; right; bucket; hash; _ } ->
        if hash = hash' then b_mem key' bucket
        else
          let n = diff hash hash' in
          if hash < hash' then
            let z = trailing_zeros hash in
            z >= n && n_mem key' right hash'
          else
            let z' = trailing_zeros hash' in
            (not (z' >= n))
            &&
            let z = trailing_zeros hash in
            z + 1 >= n && n_mem key' left hash'

  let mem : key -> 'a t -> bool = fun key t -> n_mem key t (H.hash key)

  let rec b_find : key -> 'a bucketlist -> 'a =
   fun key' t ->
    match t with
    | Empty -> raise Not_found
    | Cons { key; data; next; _ } ->
        let i = H.compare key key' in
        if i = 0 then data
        else if i < 0 then b_find key' next
        else raise Not_found

  let rec n_find : key -> 'a t -> int -> 'a =
   fun key' t hash' ->
    match t with
    | Empty -> raise Not_found
    | Node { left; right; bucket; hash; _ } ->
        if hash = hash' then b_find key' bucket
        else
          let n = diff hash hash' in
          if hash < hash' then
            let z = trailing_zeros hash in
            if z >= n then n_find key' right hash' else raise Not_found
          else
            let z' = trailing_zeros hash' in
            if z' >= n then raise Not_found
            else
              let z = trailing_zeros hash in
              if z + 1 >= n then n_find key' left hash' else raise Not_found

  let find : key -> 'a t -> 'a = fun key t -> n_find key t (H.hash key)

  let rec b_iter : (key -> 'a -> unit) -> 'a bucketlist -> unit =
   fun f t ->
    match t with
    | Empty -> ()
    | Cons { key; data; next; _ } ->
        f key data;
        b_iter f next

  let rec iter : (key -> 'a -> unit) -> 'a t -> unit =
   fun f t ->
    match t with
    | Empty -> ()
    | Node { left; right; bucket; _ } ->
        iter f left;
        b_iter f bucket;
        iter f right

  let rec b_fold : (key -> 'a -> 'acc -> 'acc) -> 'a bucketlist -> 'acc -> 'acc
      =
   fun f t acc ->
    match t with
    | Empty -> acc
    | Cons { key; data; next; _ } -> b_fold f next (f key data acc)

  let rec fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc =
   fun f t acc ->
    match t with
    | Empty -> acc
    | Node { left; right; bucket; _ } ->
        fold f right (b_fold f bucket (fold f left acc))

  let rec b_map : ('a -> 'b) -> 'a bucketlist -> 'b bucketlist =
   fun f t ->
    match t with
    | Empty -> Empty
    | Cons { key; data; next; _ } ->
        Cons { frozen = false; key; data = f data; next = b_map f next }

  let rec map : ('a -> 'b) -> 'a t -> 'b t =
   fun f t ->
    match t with
    | Empty -> Empty
    | Node { left; right; bucket; hash; _ } ->
        Node
          {
            frozen = false;
            left = map f left;
            right = map f right;
            bucket = b_map f bucket;
            hash;
          }

  let rec b_mapi : (key -> 'a -> 'b) -> 'a bucketlist -> 'b bucketlist =
   fun f t ->
    match t with
    | Empty -> Empty
    | Cons { key; data; next; _ } ->
        Cons { frozen = false; key; data = f key data; next = b_mapi f next }

  let rec mapi : (key -> 'a -> 'b) -> 'a t -> 'b t =
   fun f t ->
    match t with
    | Empty -> Empty
    | Node { left; right; bucket; hash; _ } ->
        Node
          {
            frozen = false;
            left = mapi f left;
            right = mapi f right;
            bucket = b_mapi f bucket;
            hash;
          }

  let rec b_union_map_eq :
      (key -> 'a -> 'a -> 'a) ->
      (key -> 'a -> 'a) ->
      'a bucketlist ->
      'a bucketlist ->
      'a bucketlist =
   fun f g t t' ->
    if t == t' then (
      b_freeze t;
      t)
    else
      match (t, t') with
      | Empty, (Empty (* unreachable *) | Cons _) -> b_mapi g t'
      | Cons _, Empty -> b_mapi g t
      | ( Cons { key; data; next; _ },
          Cons { key = key'; data = data'; next = next'; _ } ) ->
          let i = H.compare key key' in
          if i = 0 then
            Cons
              {
                frozen = false;
                key;
                data = f key data data';
                next = b_union_map_eq f g next next';
              }
          else if i < 0 then
            Cons
              {
                frozen = false;
                key;
                data = g key data;
                next = b_union_map_eq f g next t';
              }
          else
            Cons
              {
                frozen = false;
                key = key';
                data = g key' data';
                next = b_union_map_eq f g t next';
              }

  let rec union_map_eq :
      (key -> 'a -> 'a -> 'a) -> (key -> 'a -> 'a) -> 'a t -> 'a t -> 'a t =
   fun f g t t' ->
    if t == t' then (
      freeze t;
      t)
    else
      match (t, t') with
      | Empty, (Empty (* unreachable *) | Node _) -> mapi g t'
      | Node _, Empty -> mapi g t
      | ( Node { left; right; bucket; hash; _ },
          Node
            { left = left'; right = right'; bucket = bucket'; hash = hash'; _ }
        ) ->
          if hash = hash' then
            Node
              {
                frozen = false;
                left = union_map_eq f g left left';
                right = union_map_eq f g right right';
                bucket = b_union_map_eq f g bucket bucket';
                hash;
              }
          else if hash > hash' then
            let n = diff hash hash' and z' = trailing_zeros hash' in
            if z' >= n then
              Node
                {
                  frozen = false;
                  left = mapi g left';
                  right = union_map_eq f g t right';
                  bucket = b_mapi g bucket';
                  hash = hash';
                }
            else
              let z = trailing_zeros hash in
              if z + 1 >= n then
                Node
                  {
                    frozen = false;
                    left = union_map_eq f g left t';
                    right = mapi g right;
                    bucket = b_mapi g bucket;
                    hash;
                  }
              else
                Node
                  {
                    frozen = false;
                    left = mapi g t';
                    right = mapi g t;
                    bucket = Empty;
                    hash = hash land (-1 lsl (n - 1));
                  }
          else
            let n = diff hash hash' and z = trailing_zeros hash in
            if z >= n then
              Node
                {
                  frozen = false;
                  left = mapi g left;
                  right = union_map_eq f g right t';
                  bucket = b_mapi g bucket;
                  hash;
                }
            else
              let z' = trailing_zeros hash' in
              if z' + 1 >= n then
                Node
                  {
                    frozen = false;
                    left = union_map_eq f g t left';
                    right = mapi g right';
                    bucket = b_mapi g bucket';
                    hash = hash';
                  }
              else
                Node
                  {
                    frozen = false;
                    left = mapi g t;
                    right = mapi g t';
                    bucket = Empty;
                    hash = hash' land (-1 lsl (n - 1));
                  }

  let is_empty : 'a t -> bool = function Empty -> true | Node _ -> false
end
