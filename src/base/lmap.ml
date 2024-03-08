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

module type Value = sig
  type t

  val equal : t -> t -> bool
  val len : t -> int
  val crop : lo:int -> hi:int -> t -> t
  val concat : t -> t -> t
end

module type S = sig
  type t
  type v

  val empty : t
  val is_empty : t -> bool
  val is_empty_between : Z.t -> Z.t -> t -> bool
  val singleton : Z.t -> v -> t
  val store : Z.t -> v -> t -> t
  val select : (Z.t -> int -> v) -> Z.t -> int -> t -> v
  val iter : (Z.t -> v -> unit) -> t -> unit
  val rev_iter : (Z.t -> v -> unit) -> t -> unit
  val fold : (Z.t -> v -> 'a -> 'a) -> 'a -> t -> 'a
  val rev_fold : (Z.t -> v -> 'a -> 'a) -> 'a -> t -> 'a
  val map : (Z.t -> v -> v) -> t -> t
  val merge : (Z.t -> v option -> v option -> v option) -> t -> t -> t
  val choose : t -> Z.t * v
  val bindings : t -> (Z.t * v) list
end

module Make (E : Value) : S with type v := E.t = struct
  type empty
  type nonempty

  type _ tree =
    | Z : empty tree
    | N : { k : Z.t; e : E.t } -> nonempty tree  (** leaf *)
    | LN : { k : Z.t; e : E.t; l : nonempty tree } -> nonempty tree
        (** node with left sub-tree  *)
    | NR : { k : Z.t; e : E.t; r : nonempty tree } -> nonempty tree
        (** node with right sub-tree *)
    | LNR : {
        k : Z.t;
        e : E.t;
        l : nonempty tree;
        r : nonempty tree;
      }
        -> nonempty tree  (** node with left and right sub-trees *)
    | LR : { k : Z.t; l : nonempty tree; r : nonempty tree } -> nonempty tree
        (** branch with left and right sub-trees *)

  exception Overflow of { mutable t : nonempty tree; k : Z.t; e : E.t }
  exception Return of nonempty tree
  exception Hole of E.t

  type t = T : _ tree -> t [@@unboxed]

  (* most significant bit of difference betweeb two key *)
  let diff k k' = Z.numbits (Z.logxor k k')
  let upper_bound k e = Z.add k (Z.of_int (E.len e))
  let distance k' u' = Z.to_int (Z.sub u' k')

  let rec iter : type a. (Z.t -> E.t -> unit) -> a tree -> unit =
   fun f t ->
    match t with
    | Z -> ()
    | N { k; e } -> f k e
    | LN { k; e; l } ->
        iter f l;
        f k e
    | NR { k; e; r } ->
        f k e;
        iter f r
    | LNR { k; e; l; r } ->
        iter f l;
        f k e;
        iter f r
    | LR { l; r; _ } ->
        iter f l;
        iter f r

  let rec rev_iter : type a. (Z.t -> E.t -> unit) -> a tree -> unit =
   fun f t ->
    match t with
    | Z -> ()
    | N { k; e } -> f k e
    | LN { k; e; l } ->
        f k e;
        rev_iter f l
    | NR { k; e; r } ->
        rev_iter f r;
        f k e
    | LNR { k; e; l; r } ->
        rev_iter f r;
        f k e;
        rev_iter f l
    | LR { l; r; _ } ->
        rev_iter f r;
        rev_iter f l

  let rec fold : type a. (Z.t -> E.t -> 'c -> 'c) -> 'c -> a tree -> 'c =
   fun f x t ->
    match t with
    | Z -> x
    | N { k; e } -> f k e x
    | LN { k; e; l } -> f k e (fold f x l)
    | NR { k; e; r } -> fold f (f k e x) r
    | LNR { k; e; l; r } -> fold f (f k e (fold f x l)) r
    | LR { l; r; _ } -> fold f (fold f x l) r

  let rec rev_fold : type a. (Z.t -> E.t -> 'c -> 'c) -> 'c -> a tree -> 'c =
   fun f x t ->
    match t with
    | Z -> x
    | N { k; e } -> f k e x
    | LN { k; e; l } -> rev_fold f (f k e x) l
    | NR { k; e; r } -> f k e (rev_fold f x r)
    | LNR { k; e; l; r } -> rev_fold f (f k e (rev_fold f x r)) l
    | LR { l; r; _ } -> rev_fold f (rev_fold f x r) l

  let rec d_is_empty_between : type a. Z.t -> Z.t -> a tree -> bool =
   fun k' u' t ->
    match t with
    | Z -> true
    | N { k; e } -> n_is_empty_between k' u' k e Z Z
    | LN { k; e; l } -> n_is_empty_between k' u' k e l Z
    | NR { k; e; r } -> n_is_empty_between k' u' k e Z r
    | LNR { k; e; l; r } -> n_is_empty_between k' u' k e l r
    | LR { k; l; r; _ } -> b_is_empty_between k' u' k l r

  and n_is_empty_between :
      type a b. Z.t -> Z.t -> Z.t -> E.t -> a tree -> b tree -> bool =
   fun k' u' k e l r ->
    if Z.lt u' k then d_is_empty_between k' u' l
    else Z.leq (upper_bound k e) k' && d_is_empty_between k' u' r

  and b_is_empty_between :
      type a b. Z.t -> Z.t -> Z.t -> a tree -> b tree -> bool =
   fun k' u' k l r ->
    d_is_empty_between k' u' l && (Z.leq u' k || d_is_empty_between k' u' r)

  let rec map : type a. (Z.t -> E.t -> E.t) -> a tree -> a tree =
   fun f t ->
    match t with
    | Z -> Z
    | N { k; e } -> N { k; e = f k e }
    | LN { k; e; l } -> LN { k; e = f k e; l = map f l }
    | NR { k; e; r } -> NR { k; e = f k e; r = map f r }
    | LNR { k; e; l; r } -> LNR { k; e = f k e; l = map f l; r = map f r }
    | LR { k; l; r } -> LR { k; l = map f l; r = map f r }

  let bindings t = rev_fold (fun k e b -> (k, e) :: b) [] t

  external nonempty : _ tree -> nonempty tree = "%identity"

  let create : type a b. Z.t -> E.t -> a tree -> b tree -> nonempty tree =
   fun k e l r ->
    match (l, r) with
    | Z, Z -> N { k; e }
    | ((N _ | LN _ | NR _ | LNR _ | LR _) as l), Z -> LN { k; e; l }
    | Z, ((N _ | LN _ | NR _ | LNR _ | LR _) as r) -> NR { k; e; r }
    | ( ((N _ | LN _ | NR _ | LNR _ | LR _) as l),
        ((N _ | LN _ | NR _ | LNR _ | LR _) as r) ) ->
        LNR { k; e; l; r }

  let branch : Z.t -> nonempty tree -> nonempty tree -> nonempty tree =
   fun k l r -> LR { k; l; r }

  let maybe_branch : type a b. Z.t -> a tree -> b tree -> nonempty tree =
   fun k l r ->
    match r with
    | Z -> nonempty l
    | (N _ | LN _ | NR _ | LNR _ | LR _) as r -> (
        match l with
        | Z -> r
        | (N _ | LN _ | NR _ | LNR _ | LR _) as l -> branch k l r)

  (* dispatch add *)
  let rec d_add : type a. Z.t -> E.t -> a tree -> Z.t -> nonempty tree =
   fun k' e' t u' ->
    match t with
    | Z -> N { k = k'; e = e' }
    | N { k; e } -> n_add k' e' t u' k e Z Z
    | LN { k; e; l } -> n_add k' e' t u' k e l Z
    | NR { k; e; r } -> n_add k' e' t u' k e Z r
    | LNR { k; e; l; r } -> n_add k' e' t u' k e l r
    | LR { k; l; r } -> b_add k' e' t u' k l r

  (* node add *)
  and n_add :
      type a b.
      Z.t ->
      E.t ->
      nonempty tree ->
      Z.t ->
      Z.t ->
      E.t ->
      a tree ->
      b tree ->
      nonempty tree =
   fun k' e' t u' k e l r ->
    if Z.equal k k' then (
      if E.equal e e' then t
      else
        let s = E.len e and s' = E.len e' in
        let d = s - s' in
        if d = 0 then
          (* case 1                     *
           *       [ k  .. u  ]         *
           *       [ k' .. u' ]         *
           *)
          create k' e' l r
        else if d > 0 then
          let ea = E.crop ~hi:(s - 1) ~lo:s' e in
          let n = diff k' u' in
          let z' = Z.trailing_zeros k' in
          if z' >= n then
            let r' = d_add u' ea r (Z.add u' (Z.of_int d)) in
            create k' e' l r'
          else raise_notrace (Overflow { t = create k' e' l Z; k = u'; e = ea })
        else
          try
            let (T r') = d_remove_below u' r in
            create k' e' l r'
          with Overflow c as o ->
            c.t <- create k' e' l Z;
            raise_notrace o)
    else if Z.geq k u' then
      (* case 2                     *
       *               [ k  .. u  ] *
       * [ k' .. u' ]               *
       *)
      let n = diff k k' and z' = Z.trailing_zeros k' in
      if z' >= n then
        let (T r') = d_remove_below u' t in
        create k' e' Z r'
      else
        let z = Z.trailing_zeros k in
        if z + 1 >= n then
          let l' =
            try d_add k' e' l u'
            with Overflow { t = l'; k = k''; e = e'' } ->
              d_add k'' e'' l' (upper_bound k'' e'')
          in
          create k e l' r
        else
          let kb = Z.logand k (Z.shift_left Z.minus_one (n - 1)) in
          let l' = create k' e' Z Z in
          let (T r') = d_remove_below u' t in
          branch kb l' (nonempty r')
    else
      let s = E.len e in
      let u = Z.add k (Z.of_int s) in
      if Z.leq u k' then (
        (* case 3                     *
         * [ k  .. u  ]               *
         *               [ k' .. u' ] *
         *)
        let n = diff k k'
        and z = Z.trailing_zeros k in
        if z >= n then (
          try
            let r' = d_add k' e' r u' in
            create k e l r'
          with Overflow c as o ->
            c.t <- create k e l c.t;
            raise_notrace o)
        else
          let z' = Z.trailing_zeros k' in
          try
            let l' = d_remove_between k' u' t in
            if z' + 1 >= n then create k' e' l' Z
            else
              let k'' = Z.logand k' (Z.shift_left Z.minus_one (n - 1)) in
              let r'' = create k' e' Z Z in
              branch k'' l' r''
          with Overflow c as o ->
            c.t <- d_add k' e' c.t u';
            raise_notrace o)
      else
        (* overlap *)
        let d = Z.sub k k' in
        if Z.geq k k' then (
          if Z.gt u u' then
            (* case 4                     *
             *         [ k  .. u  ]       *
             *     [ k' .. u' ]           *
             *)
            let s' = E.len e' in
            let ea = E.crop ~hi:(s - 1) ~lo:(s' - Z.to_int d) e in
            let n = diff k k' and z' = Z.trailing_zeros k' in
            let n' = diff k' u' in
            if z' >= n then
              if z' >= n' then
                let r' = d_add u' ea r u in
                create k' e' Z r'
              else
                raise_notrace
                  (Overflow { t = create k' e' Z Z; k = u'; e = ea })
            else
              let z = Z.trailing_zeros k in
              let n'' = diff k u' in
              if z + 1 >= n then
                if z >= n'' then
                  let r' = d_add u' ea r u in
                  let l' =
                    try d_add k' e' l u'
                    with Overflow { t = l'; k = k''; e = e'' } ->
                      let l'' = d_add k'' e'' l' (upper_bound k'' e'') in
                      branch k l'' r'
                  in
                  branch k l' r'
                else
                  raise_notrace
                    (Overflow { t = d_add k' e' l u'; k = u'; e = ea })
              else
                let l' = create k' e' Z Z in
                if z >= n'' then
                  let k'' = Z.logand k (Z.shift_left Z.minus_one (n - 1)) in
                  let r' = d_add u' ea r u in
                  branch k'' l' r'
                else raise_notrace (Overflow { t = l'; k = u'; e = ea })
          else
            (* case 5                     *
             *       [ k  .. u  ]         *
             *    [ k'    ..    u' ]      *
             *)
            let n = diff k k' and z' = Z.trailing_zeros k' in
            if z' >= n then (
              try
                let (T r') = d_remove_below u' r in
                create k' e' Z r'
              with Overflow c as o ->
                c.t <- create k' e' Z Z;
                raise_notrace o)
            else
              let z = Z.trailing_zeros k in
              let kb, l' =
                if z + 1 >= n then (k, d_add k' e' l u')
                else
                  ( Z.logand k (Z.shift_left Z.minus_one (n - 1)),
                    create k' e' Z Z )
              in
              try
                let (T r') = d_remove_below u' r in
                maybe_branch kb l' r'
              with Overflow c as o ->
                c.t <- l';
                raise_notrace o)
        else if Z.gt u u' then
          (* case 6                     *
           *    [ k     ..    u  ]      *
           *       [ k' .. u' ]         *
           *)
          let eb = E.crop ~hi:(-Z.to_int d - 1) ~lo:0 e in
          let s' = E.len e' in
          let ea = E.crop ~hi:(s - 1) ~lo:(s' - Z.to_int d) e in
          let z = Z.trailing_zeros k and z' = Z.trailing_zeros k' in
          let n = diff k k' in
          let n' = diff k' u' in
          if z >= n then
            if z' >= n' then (
              try
                let r'' = d_add u' ea r u in
                let r' = d_add k' e' r'' u' in
                create k eb l r'
              with Overflow c as o ->
                c.t <- create k' e' Z c.t;
                raise_notrace o)
            else
              let z'' = Z.trailing_zeros u' in
              if z'' + 1 >= n' then
                let n'' = diff k u' in
                if z >= n'' then
                  let r' = d_add u' ea r u in
                  let r'' = d_add k' e' r' u' in
                  create k eb l r''
                else
                  raise_notrace
                    (Overflow
                       { t = create k eb l (create k' e' Z Z); k = u'; e = ea })
              else
                let n'' = diff k u' in
                if z >= n'' then
                  let r' = d_add k' e' r u' in
                  let r'' = d_add u' ea r' u in
                  create k eb l r''
                else
                  let r' = create k' e' Z Z in
                  raise_notrace
                    (Overflow { t = create k eb l r'; k = u'; e = ea })
          else if z' + 1 >= n then
            let l' = create k eb l Z in
            if z' >= n then
              let r' = create u' ea Z Z in
              create k' e' l' r'
            else
              raise_notrace (Overflow { t = create k' e' l' Z; k = u'; e = ea })
          else
            let t' = create k eb l Z in
            raise_notrace (Overflow { t = d_add k' e' t' u'; k = u'; e = ea })
        else
          (* case 7                     *
           *     [ k  .. u  ]           *
           *         [ k' .. u' ]       *
           *)
          let e'' = E.crop ~hi:(-Z.to_int d - 1) ~lo:0 e in
          let n = diff k k' and z = Z.trailing_zeros k in
          if z >= n then (
            try
              let r' = d_add k' e' r u' in
              create k e'' l r'
            with Overflow c as o ->
              c.t <- create k e'' l c.t;
              raise_notrace o)
          else
            let z' = Z.trailing_zeros k' in
            let l' = create k e'' l Z in
            if z' + 1 >= n then create k' e' l' Z
            else
              let k'' = Z.logand k' (Z.shift_left Z.minus_one (n - 1)) in
              let r'' = create k' e' Z Z in
              branch k'' l' r''

  (* branch add *)
  and b_add :
      Z.t ->
      E.t ->
      nonempty tree ->
      Z.t ->
      Z.t ->
      nonempty tree ->
      nonempty tree ->
      nonempty tree =
   fun k' e' t u' k l r ->
    if Z.equal k k' then
      try
        let l' =
          try d_remove_between k' u' l
          with Overflow { t = l'; k = ka; e = ea } ->
            let r' = d_add ka ea r (upper_bound ka ea) in
            raise_notrace (Return (create k' e' l' r'))
        in
        let (T r') =
          try d_remove_below u' r
          with Overflow c as o ->
            c.t <- create k' e' l' Z;
            raise_notrace o
        in
        create k' e' l' r'
      with Return t -> t
    else if Z.geq k u' then
      let n = diff k k' and z' = Z.trailing_zeros k' in
      if z' >= n then
        let (T r') = d_remove_below u' t in
        create k' e' Z r'
      else
        let z = Z.trailing_zeros k in
        if z + 1 >= n then
          try
            let l' = d_add k' e' l u' in
            branch k l' r
          with Overflow { t = l'; k = k''; e = e'' } ->
            if Z.equal k k'' then create k'' e'' l' r
            else
              let u'' = upper_bound k'' e'' in
              let l'' = d_add k'' e'' l' u'' in
              branch k l'' r
        else
          let k'' = Z.logand k (Z.shift_left Z.minus_one (n - 1)) in
          let l'' = create k' e' Z Z in
          let (T r'') = d_remove_below u' t in
          branch k'' l'' (nonempty r'')
    else if Z.lt k k' then
      let n = diff k k' and z = Z.trailing_zeros k in
      if z >= n then
        try
          let l' =
            try d_remove_between k' u' l
            with Overflow { t = l'; k = ka; e = ea } ->
              let r' = d_add ka ea r (upper_bound ka ea) in
              let r'' = d_add k' e' r' u' in
              raise_notrace (Return (branch k l' r''))
          in
          let r' =
            try d_add k' e' r u'
            with Overflow c as o ->
              c.t <- branch k l' c.t;
              raise_notrace o
          in
          branch k l' r'
        with Return t -> t
      else
        let z' = Z.trailing_zeros k' in
        if z' + 1 >= n then
          try
            let t' = d_remove_between k' u' t in
            create k' e' t' Z
          with Overflow c as o ->
            (* assert (c.k = u'); *)
            let n' = diff k' u' in
            if z' >= n' then
              let r' = create c.k c.e Z Z in
              create k' e' c.t r'
            else (
              c.t <- create k' e' c.t Z;
              raise_notrace o)
        else
          let k'' = Z.logand k' (Z.shift_left Z.minus_one (n - 1)) in
          let r'' = create k' e' Z Z in
          try
            let l'' = d_remove_between k' u' t in
            branch k'' l'' r''
          with Overflow c as o ->
            (* assert (c.k = u'); *)
            let n' = diff k'' u' in
            if n >= n' then
              let r3 = d_add c.k c.e r'' (upper_bound c.k c.e) in
              branch k'' c.t r3
            else (
              c.t <- branch k'' c.t r'';
              raise_notrace o)
    else
      (* overwrite *)
      let n = diff k k' and z' = Z.trailing_zeros k' in
      if z' >= n then
        try
          (try ignore (d_remove_below u' l)
           with Overflow { k = k''; e = e''; _ } ->
             let r' = d_add k'' e'' r (upper_bound k'' e'') in
             let n' = diff k' u' in
             if z' >= n' then raise_notrace (Return (create k' e' Z r'))
             else
               let l' = create k' e' Z Z in
               raise_notrace (Return (branch k l' r')));
          try
            let (T r') = d_remove_below u' r in
            create k' e' Z r'
          with Overflow c as o ->
            c.t <- create k' e' Z Z;
            raise_notrace o
        with Return t -> t
      else
        let z = Z.trailing_zeros k in
        if z + 1 >= n then
          try
            let l' =
              try d_add k' e' l u'
              with Overflow { t = l'; k = k''; e = e'' } ->
                let u'' = upper_bound k'' e'' in
                let r' = d_add k'' e'' r u'' in
                raise_notrace (Return (branch k l' r'))
            in
            let (T r') =
              try d_remove_below u' r
              with Overflow c as o ->
                c.t <- l';
                raise_notrace o
            in
            maybe_branch k l' r'
          with Return t -> t
        else
          let k'' = Z.logand k (Z.shift_left Z.minus_one (n - 1)) in
          let l'' = create k' e' Z Z in
          try
            let (T r'') = d_remove_below u' t in
            maybe_branch k'' l'' r''
          with Overflow c as o ->
            c.t <- l'';
            raise_notrace o

  (* dispatch remove below *)
  and d_remove_below : type a. Z.t -> a tree -> t =
   fun u' t ->
    match t with
    | Z -> T Z
    | N { k; e } -> n_remove_below u' t k e Z Z
    | LN { k; e; l } -> n_remove_below u' t k e l Z
    | NR { k; e; r } -> n_remove_below u' t k e Z r
    | LNR { k; e; l; r } -> n_remove_below u' t k e l r
    | LR { k; l; r } -> b_remove_below u' t k l r

  (* node remove below *)
  and n_remove_below :
      type a b. Z.t -> nonempty tree -> Z.t -> E.t -> a tree -> b tree -> t =
   fun u' t k e l r ->
    if Z.leq u' k then
      let (T l') =
        try d_remove_below u' l
        with Overflow { k = k'; e = e'; _ } -> T (create k' e' Z Z)
      in
      if T l' == T l then T t else T (create k e l' r)
    else
      let s = E.len e in
      let u = Z.add k (Z.of_int s) in
      if Z.geq u' u then d_remove_below u' r
      else
        let d = Z.to_int (Z.sub u' k) in
        let e' = E.crop ~hi:(s - 1) ~lo:d e in
        let n = diff k u' and z = Z.trailing_zeros k in
        if z >= n then T (d_add u' e' r (upper_bound u' e'))
        else raise_notrace (Overflow { t; k = u'; e = e' })

  (* branch remove below *)
  and b_remove_below :
      Z.t -> nonempty tree -> Z.t -> nonempty tree -> nonempty tree -> t =
   fun u' t k l r ->
    if Z.leq u' k then
      try
        let (T l') = d_remove_below u' l in
        if T l' == T l then T t else T (maybe_branch k l' r)
      with Overflow { k = k''; e = e''; _ } ->
        if Z.equal k'' k then T (create k e'' Z r)
        else
          let l' = create k'' e'' Z Z in
          T (branch k l' r)
    else
      let (T r') = d_remove_below u' r in
      if T r' == T r then
        try
          ignore (d_remove_below u' l);
          T r'
        with Overflow { k = k'; e = e'; _ } ->
          T (d_add k' e' r' (upper_bound k' e'))
      else T r'

  (* remove between *)
  and d_remove_between : type a. Z.t -> Z.t -> a tree -> a tree =
   fun k' u' t ->
    match t with
    | Z -> Z
    | N { k; e } -> n_remove_between k' u' t k e Z
    | LN { k; e; l } -> n_remove_between k' u' t k e l
    | NR { k; e; r } -> r_remove_between k' u' t k e Z r
    | LNR { k; e; l; r } -> r_remove_between k' u' t k e l r
    | LR { k; l; r } -> (
        try
          let r' = d_remove_between k' u' r in
          if T r' == T r then t else branch k l r'
        with Overflow c as o ->
          c.t <- branch k l c.t;
          raise_notrace o)

  and n_remove_between :
      type a.
      Z.t -> Z.t -> nonempty tree -> Z.t -> E.t -> a tree -> nonempty tree =
   fun k' u' t k e l ->
    let s = E.len e in
    let u = Z.add k (Z.of_int s) in
    if Z.leq u k' then t
    else
      let eb = E.crop ~hi:(Z.to_int (Z.sub k' k) - 1) ~lo:0 e in
      let t' = create k eb l Z in
      if Z.gt u u' then
        let ea = E.crop ~hi:(s - 1) ~lo:(Z.to_int (Z.sub u' k)) e in
        raise_notrace (Overflow { t = t'; k = u'; e = ea })
      else t'

  and r_remove_between :
      type a.
      Z.t ->
      Z.t ->
      nonempty tree ->
      Z.t ->
      E.t ->
      a tree ->
      nonempty tree ->
      nonempty tree =
   fun k' u' t k e l r ->
    try
      let r' = d_remove_between k' u' r in
      if T r' == T r then t else create k e l r'
    with Overflow c as o ->
      c.t <- create k e l c.t;
      raise_notrace o

  let rec d_seek_rightmost :
      type a. a tree -> (Z.t -> int -> E.t) -> Z.t -> Z.t -> E.t =
   fun t f k' u' ->
    match t with
    | Z -> raise_notrace Not_found
    | N { k; e } | LN { k; e; _ } -> n_seek_rightmost Z f k' u' k e
    | NR { k; e; r } | LNR { k; e; r; _ } -> n_seek_rightmost r f k' u' k e
    | LR { r; _ } -> d_seek_rightmost r f k' u'

  and n_seek_rightmost :
      type a. a tree -> (Z.t -> int -> E.t) -> Z.t -> Z.t -> Z.t -> E.t -> E.t =
   fun r f k' u' k e ->
    let u = upper_bound k e in
    if Z.leq u k' then d_seek_rightmost r f k' u'
    else
      let d = distance u u' in
      let s = E.len e and s' = distance k' u' in
      let lo = distance k k' in
      if d = 0 then E.crop ~hi:(s - 1) ~lo e
      else if d > 0 then raise_notrace (Hole (E.crop ~hi:(s - 1) ~lo e))
      else E.crop ~hi:(lo + s' - 1) ~lo e

  let rec d_seek :
      type p a. p tree -> (Z.t -> int -> E.t) -> Z.t -> Z.t -> a tree -> E.t =
   fun p f k' u' t ->
    match t with
    | Z -> d_seek_rightmost p f k' u' (* raise_notrace Not_found *)
    | N { k; e } -> n_seek p f k' u' k e Z Z
    | LN { k; e; l } -> n_seek p f k' u' k e l Z
    | NR { k; e; r } -> n_seek p f k' u' k e Z r
    | LNR { k; e; l; r } -> n_seek p f k' u' k e l r
    | LR { k; l; r } -> b_seek p f k' u' k l r

  and n_seek :
      type p a b.
      p tree ->
      (Z.t -> int -> E.t) ->
      Z.t ->
      Z.t ->
      Z.t ->
      E.t ->
      a tree ->
      b tree ->
      E.t =
   fun p f k' u' k e l r ->
    if Z.equal k k' then
      let s = E.len e and s' = distance k' u' in
      let d = s - s' in
      if d = 0 then e
      else if d > 0 then E.crop ~hi:(s' - 1) ~lo:0 e
      else d_fill f (Z.add k (Z.of_int s)) u' e r
    else if Z.geq k u' then d_seek p f k' u' l
    else
      let u = upper_bound k e in
      if Z.leq u k' then d_seek Z f k' u' r
      else
        let d = distance u u' in
        if Z.gt k k' then
          let e' =
            try d_seek p f k' k l with
            | Not_found -> f k' (distance k' k)
            | Hole e' ->
                let kh = upper_bound k' e' in
                E.concat (f kh (distance kh k)) e'
          in
          if d = 0 then E.concat e e'
          else if d > 0 then d_fill f u u' (E.concat e e') r
          else E.concat (E.crop ~hi:(distance k u' - 1) ~lo:0 e) e'
        else
          let s = E.len e and s' = distance k' u' in
          let lo = distance k k' in
          if d = 0 then E.crop ~hi:(s - 1) ~lo e
          else if d > 0 then d_fill f u u' (E.crop ~hi:(s - 1) ~lo e) r
          else E.crop ~hi:(lo + s' - 1) ~lo e

  and b_seek :
      type p.
      p tree ->
      (Z.t -> int -> E.t) ->
      Z.t ->
      Z.t ->
      Z.t ->
      nonempty tree ->
      nonempty tree ->
      E.t =
   fun p f k' u' k l r ->
    if Z.geq k u' then d_seek p f k' u' l
    else if Z.lt k k' then d_seek l f k' u' r (* d_seek_rightmost f k' u' l *)
    else
      try d_seek p f k' u' l with
      | Not_found -> d_seek Z f k' u' r
      | Hole e' -> d_fill f (upper_bound k' e') u' e' r

  and d_fill : type a. (Z.t -> int -> E.t) -> Z.t -> Z.t -> E.t -> a tree -> E.t
      =
   fun f k' u' e' t ->
    match t with
    | Z -> raise_notrace (Hole e')
    | N { k; e } -> n_fill f k' u' e' k e Z Z
    | LN { k; e; l } -> n_fill f k' u' e' k e l Z
    | NR { k; e; r } -> n_fill f k' u' e' k e Z r
    | LNR { k; e; l; r } -> n_fill f k' u' e' k e l r
    | LR { l; r; _ } -> b_fill f k' u' e' l r

  and n_fill :
      type a b.
      (Z.t -> int -> E.t) ->
      Z.t ->
      Z.t ->
      E.t ->
      Z.t ->
      E.t ->
      a tree ->
      b tree ->
      E.t =
   fun f k' u' e' k e l r ->
    if Z.geq k u' then
      try d_fill f k' u' e' l
      with Hole eh ->
        let kh = Z.add k' (Z.of_int (E.len eh - E.len e')) in
        E.concat (f kh (distance kh u')) eh
    else if Z.equal k k' then
      let s = E.len e and s' = distance k' u' in
      let d = s - s' in
      if d = 0 then E.concat e e'
      else if d > 0 then E.concat (E.crop ~hi:(s' - 1) ~lo:0 e) e'
      else d_fill f (Z.add k (Z.of_int s)) u' (E.concat e e') r
    else
      let u = upper_bound k e in
      let e' =
        try d_fill f k' k e' l
        with Hole eh ->
          let kh = Z.add k' (Z.of_int (E.len eh - E.len e')) in
          E.concat (f kh (distance kh k)) eh
      in
      let d = distance u u' in
      if d = 0 then E.concat e e'
      else if d > 0 then d_fill f u u' (E.concat e e') r
      else E.concat (E.crop ~hi:(distance k u' - 1) ~lo:0 e) e'

  and b_fill :
      (Z.t -> int -> E.t) ->
      Z.t ->
      Z.t ->
      E.t ->
      nonempty tree ->
      nonempty tree ->
      E.t =
   fun f k' u' e' l r ->
    try d_fill f k' u' e' l
    with Hole eh ->
      let kh = Z.add k' (Z.of_int (E.len eh - E.len e')) in
      d_fill f kh u' eh r

  (* let rec merge :
   *     type a b c.
   *     (Z.t -> E.t option -> E.t option -> E.t option) -> a tree -> b tree -> t =
   *  fun f t t' ->
   *   match (t, t') with
   *   | Z, Z -> T Z
   *   | N { k; e }, Z -> (
   *       match f k (Some e) None with
   *       | None -> T Z
   *       | Some x -> if e == x then T t else T (N { k; e = x }))
   *   | LN { k; e; l }, Z -> (
   *       match (f k (Some e) None, merge f l Z) with
   *       | None, T Z -> T Z
   *       | None, T y -> T y
   *       | Some x, T Z -> T (N { k; e = x })
   *       | Some x, T ((N _ | LN _ | NR _ | LNR _ | LR _) as y) ->
   *           if e == x && T l == T y then T t else T (LN { k; e = x; l = y }))
   *   | NR { k; e; r }, Z -> (
   *       match (f k (Some e) None, merge f r Z) with
   *       | None, T Z -> T Z
   *       | None, T y -> T y
   *       | Some x, T Z -> T (N { k; e = x })
   *       | Some x, T ((N _ | LN _ | NR _ | LNR _ | LR _) as z) ->
   *           if e == x && T r == T z then T t else T (NR { k; e = x; r = z }))
   *   | LNR { k; e; l; r }, Z -> (
   *       match (f k (Some e) None, merge f l Z, merge f r Z) with
   *       | Some x, T Z, T Z -> T (N { k; e = x })
   *       | Some x, T ((N _ | LN _ | NR _ | LNR _ | LR _) as y), T Z ->
   *           T (LN { k; e = x; l = y })
   *       | Some x, T Z, T ((N _ | LN _ | NR _ | LNR _ | LR _) as z) ->
   *           T (NR { k; e = x; r = z })
   *       | ( Some x,
   *           T ((N _ | LN _ | NR _ | LNR _ | LR _) as y),
   *           T ((N _ | LN _ | NR _ | LNR _ | LR _) as z) ) ->
   *           if e == x && T l == T y && T r == T z then T t
   *           else T (LNR { k; e = x; l = y; r = z })
   *       | None, T Z, T Z -> T Z
   *       | None, T y, T Z -> T y
   *       | None, T Z, T z -> T z
   *       | None, T ((N _ | LN _ | NR _ | LNR _ | LR _) as y), T z ->
   *           T (fold (fun k e t -> d_add k e t (upper_bound k e)) y z))
   *   | LR { k; l; r }, Z -> (
   *       match (merge f l Z, merge f r Z) with
   *       | T Z, T Z -> T Z
   *       | T y, T Z -> T y
   *       | T Z, T z -> T z
   *       | ( T ((N _ | LN _ | NR _ | LNR _ | LR _) as y),
   *           T ((N _ | LN _ | NR _ | LNR _ | LR _) as z) ) ->
   *           if T l == T y && T r == T z then T t else T (LR { k; l = y; r = z })
   *       )
   *   | Z, t' -> merge (fun k o o' -> f k o' o) t' Z
   *   | N { k; e }, N { k = k'; e = e' } when Z.equal k k' && E.len e = E.len e'
   *     -> (
   *       match f k (Some e) (Some e') with
   *       | Some x ->
   *           if e == x then T t else if e' == x then T t' else T (N { k; e = x })
   *       | None -> T Z)
   *   | LN { k; e; l }, N { k = k'; e = e' }
   *     when Z.equal k k' && E.len e = E.len e' -> (
   *       match (f k (Some e) (Some e'), merge f l Z) with
   *       | None, T Z -> T Z
   *       | None, T y -> T y
   *       | Some x, T Z -> if e' == x then T t' else T (N { k; e = x })
   *       | Some x, T ((N _ | LN _ | NR _ | LNR _ | LR _) as y) ->
   *           T (LN { k; e = x; l = y }))
   *   | NR { k; e; r }, N { k = k'; e = e' }
   *     when Z.equal k k' && E.len e = E.len e' -> (
   *       match (f k (Some e) (Some e'), merge f r Z) with
   *       | None, T Z -> T Z
   *       | None, T z -> T z
   *       | Some x, T Z -> if e' == x then T t' else T (N { k; e = x })
   *       | Some x, T ((N _ | LN _ | NR _ | LNR _ | LR _) as z) ->
   *           T (NR { k; e = x; r = z }))
   *   | LNR { k; e; l; r }, N { k = k'; e = e' }
   *     when Z.equal k k' && E.len e = E.len e' -> (
   *       match (f k (Some e) (Some e'), merge f l Z, merge f r Z) with
   *       | None, T Z, T Z -> T Z
   *       | None, T y, T Z -> T y
   *       | None, T Z, T z -> T z
   *       | None, T ((N _ | LN _ | NR _ | LNR _ | LR _) as y), T z ->
   *           T (fold (fun k e t -> d_add k e t (upper_bound k e)) y z)
   *       | Some x, T Z, T Z -> if e' == x then T t' else T (N { k; e = x })
   *       | Some x, T ((N _ | LN _ | NR _ | LNR _ | LR _) as y), T Z ->
   *           T (LN { k; e = x; l = y })
   *       | Some x, T Z, T ((N _ | LN _ | NR _ | LNR _ | LR _) as z) ->
   *           T (NR { k; e = x; r = z })
   *       | ( Some x,
   *           T ((N _ | LN _ | NR _ | LNR _ | LR _) as y),
   *           T ((N _ | LN _ | NR _ | LNR _ | LR _) as z) ) ->
   *           if e == x && T l == T y && T r == T z then T t
   *           else T (LNR { k; e = x; l = y; r = z }))
   *   | t, N _ -> slow_merge f t t'
   *   | N _, t' -> merge (fun k o o' -> f k o' o) t' t
   *   | _ -> assert false *)

  let empty = T Z
  let is_empty t = t == empty
  let is_empty_between i j (T t) = d_is_empty_between i j t
  let singleton k e = T (N { k; e })

  let store k' e' (T t) =
    let u' = upper_bound k' e' in
    try T (d_add k' e' t u')
    with Overflow { t; k; e } ->
      let u = upper_bound k e in
      T (d_add k e t u)

  let select f k s (T t) =
    let u = Z.add k (Z.of_int s) in
    try d_seek Z f k u t with
    | Not_found -> f k s
    | Hole e ->
        let kh = upper_bound k e in
        E.concat (f kh (distance kh u)) e

  let iter f (T t) = iter f t
  let rev_iter f (T t) = rev_iter f t
  let fold f x (T t) = fold f x t
  let rev_fold f x (T t) = rev_fold f x t
  let map f (T t) = T (map f t)

  let rec choose (T t) =
    match t with
    | Z -> raise Not_found
    | N { k; e } | LN { k; e; _ } | NR { k; e; _ } | LNR { k; e; _ } -> (k, e)
    | LR { l; _ } -> choose (T l)

  let rec merge_bindings f bindings bindings' t =
    match (bindings, bindings') with
    | [], [] -> t
    | (k, e) :: remaining, [] ->
        let t =
          match f k (Some e) None with None -> t | Some x -> store k x t
        in
        merge_bindings f remaining bindings' t
    | [], (k', e') :: remaining' ->
        let t =
          match f k' None (Some e') with None -> t | Some x -> store k' x t
        in
        merge_bindings f bindings remaining' t
    | (k, e) :: remaining, (k', e') :: remaining' ->
        let s = E.len e and s' = E.len e' in
        let u = Z.add k (Z.of_int s) and u' = Z.add k' (Z.of_int s') in
        if Z.equal k k' then
          if s = s' then
            let t =
              match f k (Some e) (Some e') with
              | None -> t
              | Some x -> store k x t
            in
            merge_bindings f remaining remaining' t
          else if s < s' then
            let el' = E.crop ~lo:0 ~hi:(s - 1) e'
            and eh' = E.crop ~lo:s ~hi:(s' - 1) e' in
            let t =
              match f k (Some e) (Some el') with
              | None -> t
              | Some x -> store k x t
            in
            merge_bindings f remaining ((u, eh') :: remaining') t
          else
            let el = E.crop ~lo:0 ~hi:(s' - 1) e
            and eh = E.crop ~lo:s' ~hi:(s - 1) e in
            let t =
              match f k (Some el) (Some e') with
              | None -> t
              | Some x -> store k x t
            in
            merge_bindings f ((u', eh) :: remaining) remaining' t
        else if Z.leq u k' then
          let t =
            match f k (Some e) None with None -> t | Some x -> store k x t
          in
          merge_bindings f remaining bindings' t
        else if Z.leq u' k then
          let t =
            match f k None (Some e') with None -> t | Some x -> store k x t
          in
          merge_bindings f bindings remaining' t
        else
          let d = distance k k' in
          if d > 0 then
            let el = E.crop ~lo:0 ~hi:(d - 1) e
            and eh = E.crop ~lo:d ~hi:(s - 1) e in
            let t =
              match f k (Some el) None with None -> t | Some x -> store k x t
            in
            merge_bindings f ((k', eh) :: remaining) bindings' t
          else
            let el' = E.crop ~lo:0 ~hi:(d - 1) e'
            and eh' = E.crop ~lo:d ~hi:(s' - 1) e' in
            let t =
              match f k None (Some el') with None -> t | Some x -> store k x t
            in
            merge_bindings f bindings ((k, eh') :: remaining') t

  let slow_merge :
      type a b.
      (Z.t -> E.t option -> E.t option -> E.t option) -> a tree -> b tree -> t =
   fun f t t' ->
    let bindings = bindings t and bindings' = bindings t' in
    merge_bindings f bindings bindings' (T Z)

  let merge f (T t) (T t') = slow_merge f t t'
  let bindings (T t) = bindings t
end
