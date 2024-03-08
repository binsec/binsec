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

module Bv = Bitvector
module BiMap = Basic_types.BigInt.Map

module rec Expr : (Term.S with type a := string and type b := Memory.t) =
  Term.Make
    (struct
      type t = string

      let compare _ _ = 0
      let equal _ _ = true
      let hash _ = 0
    end)
    (Memory)

and Chunk : sig
  include Lmap.Value

  type hunk =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type kind = Hunk of hunk | Term of Expr.t

  val inspect : t -> kind
  val of_hunk : hunk -> t
  val of_term : Expr.t -> t
  val to_term : t -> Expr.t

  (** low level API *)

  val is_hunk : t -> bool
  val is_term : t -> bool
  val unsafe_to_hunk : t -> hunk
  val unsafe_to_term : t -> Expr.t
end = struct
  type t

  type hunk =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type kind = Hunk of hunk | Term of Expr.t

  external is_unboxed : t -> bool = "%obj_is_int"
  external still_unboxed : Bv.t -> bool = "%obj_is_int"
  external to_bv : t -> Bv.t = "%identity"
  external of_bv : Bv.t -> t = "%identity"
  external unsafe_to_term : t -> Expr.t = "%identity"
  external of_term : Expr.t -> t = "%identity"
  external unsafe_to_hunk : t -> hunk = "%identity"
  external of_hunk : hunk -> t = "%identity"

  let is_hunk x = Obj.tag (Obj.repr x) = Obj.custom_tag
  let is_term x = is_unboxed x || not (is_hunk x)

  let of_bv bv =
    if still_unboxed bv then of_bv bv else of_term (Expr.constant bv)

  let inspect x =
    if is_unboxed x then Term (Expr.constant (to_bv x))
    else if is_hunk x then Hunk (unsafe_to_hunk x)
    else Term (unsafe_to_term x)

  let equal x y =
    x == y
    || (not (is_unboxed x))
       && (not (is_hunk x))
       && (not (is_unboxed y))
       && (not (is_hunk y))
       && Expr.is_equal (unsafe_to_term x) (unsafe_to_term y)

  let len x =
    if is_unboxed x then Bv.size_of (to_bv x) lsr 3
    else if is_hunk x then Bigarray.Array1.dim (unsafe_to_hunk x)
    else Expr.sizeof (unsafe_to_term x) lsr 3

  let crop ~lo ~hi x =
    if is_hunk x then
      of_hunk (Bigarray.Array1.sub (unsafe_to_hunk x) lo (hi - lo + 1))
    else
      let lo = lo lsl 3 and hi = (hi lsl 3) + 7 in
      if is_unboxed x then of_bv (Bv.extract (to_bv x) { lo; hi })
      else of_term (Expr.restrict ~lo ~hi (unsafe_to_term x))

  let hunk_to_bv x =
    let x = unsafe_to_hunk x in
    let s = Bigarray.Array1.dim x in
    Bv.create
      (Z.of_bits
         (String.init s (fun i ->
              Char.unsafe_chr (Bigarray.Array1.unsafe_get x i))))
      (s lsl 3)

  let concat x y =
    if is_unboxed x then
      if is_unboxed y then of_bv (Bv.append (to_bv x) (to_bv y))
      else if is_hunk y then of_bv (Bv.append (to_bv x) (hunk_to_bv y))
      else of_term (Expr.append (Expr.constant (to_bv x)) (unsafe_to_term y))
    else if is_hunk x then
      if is_unboxed y then of_bv (Bv.append (hunk_to_bv x) (to_bv y))
      else if is_hunk y then of_bv (Bv.append (hunk_to_bv x) (hunk_to_bv y))
      else
        of_term (Expr.append (Expr.constant (hunk_to_bv x)) (unsafe_to_term y))
    else if is_unboxed y then
      of_term (Expr.append (unsafe_to_term x) (Expr.constant (to_bv y)))
    else if is_hunk y then
      of_term (Expr.append (unsafe_to_term x) (Expr.constant (hunk_to_bv y)))
    else of_term (Expr.append (unsafe_to_term x) (unsafe_to_term y))

  let to_term x =
    if is_unboxed x then Expr.constant (to_bv x)
    else if is_hunk x then Expr.constant (hunk_to_bv x)
    else unsafe_to_term x

  let of_term (x : Expr.t) = match x with Cst bv -> of_bv bv | x -> of_term x
end

and Store : sig
  include Lmap.S with type v := Chunk.t

  val singleton : Bv.t -> Chunk.t -> t
  val store : Bv.t -> Chunk.t -> t -> t
  val select : (Z.t -> int -> Chunk.t) -> Bv.t -> int -> t -> Chunk.t
  val iter_term : (Z.t -> Expr.t -> unit) -> t -> unit
  val fold_term : (Z.t -> Expr.t -> 'a -> 'a) -> 'a -> t -> 'a
end = struct
  include Lmap.Make (Chunk)

  let singleton k v =
    let z = Bv.value_of k and s = Chunk.len v in
    let u = Z.add z (Z.of_int s) in
    let n = Bv.size_of k in
    if Z.numbits u > n && Z.popcount u > 1 then
      let o = Z.to_int (Z.extract u 0 n) in
      store z
        (Chunk.crop ~hi:(s - o - 1) ~lo:0 v)
        (singleton Z.zero (Chunk.crop ~hi:(s - 1) ~lo:(s - o) v))
    else singleton z v

  let store k v t =
    let z = Bv.value_of k and s = Chunk.len v in
    let u = Z.add z (Z.of_int s) in
    let n = Bv.size_of k in
    if Z.numbits u > n && Z.popcount u > 1 then
      let o = Z.to_int (Z.extract u 0 n) in
      store z
        (Chunk.crop ~hi:(s - o - 1) ~lo:0 v)
        (store Z.zero (Chunk.crop ~hi:(s - 1) ~lo:(s - o) v) t)
    else store z v t

  let select f k s t =
    let z = Bv.value_of k in
    let u = Z.add z (Z.of_int s) in
    let n = Bv.size_of k in
    if Z.numbits u > n && Z.popcount u > 1 then
      let o = Z.to_int (Z.extract u 0 n) in
      Chunk.concat (select f Z.zero o t) (select f z (s - o) t)
    else select f z s t

  let iter_term f t =
    iter (fun k v -> if not (Chunk.is_hunk v) then f k (Chunk.to_term v)) t

  let fold_term f b t =
    fold
      (fun k v b ->
        if not (Chunk.is_hunk v) then f k (Chunk.to_term v) b else b)
      b t
end

and Memory : sig
  type t =
    | Root
    | Symbol of string
    | Layer of { id : int; over : t; addr : Expr.t; store : Store.t }

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val root : t
  val fresh : string -> t
  val layer : Expr.t -> Store.t -> t -> t
end = struct
  type t =
    | Root
    | Symbol of string
    | Layer of { id : int; over : t; addr : Expr.t; store : Store.t }

  let id = ref 0

  let hash = function
    | Root -> 0
    | Symbol name -> Hashtbl.hash name
    | Layer { id; _ } -> id

  let compare t t' = hash t - hash t'

  let equal t t' =
    match (t, t') with
    | Root, Root -> true
    | Symbol id, Symbol id' -> id = id'
    | Layer { id; _ }, Layer { id = id'; _ } -> id = id'
    | (Root | Symbol _ | Layer _), (Root | Symbol _ | Layer _) -> false

  let root = Root
  let fresh name = Symbol name

  let layer addr store over =
    incr id;
    Layer { id = !id; over; addr; store }
end

module BvTbl = Hashtbl.Make (struct
  include Expr

  let equal = is_equal
end)

module AxTbl = Hashtbl.Make (Memory)
module BiTbl = Basic_types.BigInt.Htbl
module StTbl = Basic_types.String.Htbl
module S = Basic_types.String.Map

module I = Map.Make (struct
  type t = Z.t

  let compare x y = -Z.compare x y
end)

let bswap =
  let rec iter e i r =
    if i = 0 then r
    else
      iter e (i - 8) (Expr.append (Expr.restrict ~hi:(i - 1) ~lo:(i - 8) e) r)
  in
  fun e ->
    let size = Expr.sizeof e in
    assert (size land 0x7 = 0);
    iter e (size - 8) (Expr.restrict ~hi:(size - 1) ~lo:(size - 8) e)

module Model = struct
  type t =
    Expr.t StTbl.t * Bv.t BvTbl.t * char BiTbl.t * char BiTbl.t StTbl.t * int

  let empty addr_space =
    (StTbl.create 0, BvTbl.create 0, BiTbl.create 0, StTbl.create 0, addr_space)

  let maybe_pp_char ppf c =
    if String_utils.is_char_printable c then Format.fprintf ppf " (%c)" c

  let pp_variables ppf vars values =
    if StTbl.length vars > 0 then (
      Format.pp_print_string ppf "# Variables";
      Format.pp_print_cut ppf ();
      StTbl.iter
        (fun name value ->
          Format.fprintf ppf "%s : %a@ " name Bitvector.pp_hex_or_bin
            (BvTbl.find values value))
        vars)

  let pp_int_as_bv ppf x = function
    | 1 -> Format.fprintf ppf "#b%d" x
    | 4 -> Format.fprintf ppf "#x%01x" x
    | 8 -> Format.fprintf ppf "#x%02x" x
    | 12 -> Format.fprintf ppf "#x%03x" x
    | 16 -> Format.fprintf ppf "#x%04x" x
    | 20 -> Format.fprintf ppf "#x%05x" x
    | 24 -> Format.fprintf ppf "#x%06x" x
    | 28 -> Format.fprintf ppf "#x%07x" x
    | 32 -> Format.fprintf ppf "#x%08x" x
    | 64 when x >= 0 -> Format.fprintf ppf "#x%016x" x
    | sz -> Format.fprintf ppf "(_ bv%d %d)" x sz

  let pp_bv ppf value size =
    try pp_int_as_bv ppf (Z.to_int value) size
    with Z.Overflow -> Format.fprintf ppf "(_ bv%a %d)" Z.pp_print value size

  let pp_memory ppf memory addr_space =
    if BiTbl.length memory = 0 then
      Format.pp_print_string ppf "-- empty memory --"
    else (
      Format.pp_print_string ppf "# Memory";
      Format.pp_print_cut ppf ();
      let img = Kernel_functions.get_img () in
      let noname = "" in
      let section_name addr =
        try
          let address =
            Virtual_address.to_int (Virtual_address.of_bigint addr)
          in
          match Loader_utils.find_section_by_address ~address img with
          | None -> noname
          | Some section -> Loader.Section.name section
        with Virtual_address.Non_canonical_form -> noname
      in
      let pp_section ppf name =
        if name == noname then Format.pp_print_string ppf "unamed section"
        else Format.fprintf ppf "section %s" name
      in
      let last_section = ref "--" in
      I.iter (fun addr byte ->
          let name = section_name addr in
          if name <> !last_section then (
            Format.fprintf ppf "; %a@ " pp_section name;
            last_section := name);
          pp_bv ppf addr addr_space;
          Format.fprintf ppf " : %02x %a@ " (Char.code byte) maybe_pp_char byte)
      @@ BiTbl.fold I.add memory I.empty)

  let pp_array ppf name array addr_space =
    Format.pp_print_string ppf "# Array ";
    Format.pp_print_string ppf name;
    Format.pp_print_cut ppf ();
    I.iter (fun addr byte ->
        pp_bv ppf addr addr_space;
        Format.fprintf ppf " : %02x %a@ " (Char.code byte) maybe_pp_char byte)
    @@ BiTbl.fold I.add array I.empty

  let pp ppf (vars, values, memory, arrays, addr_space) =
    if
      StTbl.length vars = 0
      && BiTbl.length memory = 0
      && StTbl.length arrays = 0
    then Format.fprintf ppf "@[<h>--- Empty model ---@]"
    else (
      Format.fprintf ppf "@[<v 0>--- Model ---@ ";
      pp_variables ppf vars values;
      Format.pp_print_space ppf ();
      pp_memory ppf memory addr_space;
      StTbl.iter
        (fun name array ->
          if BiTbl.length array <> 0 then (
            Format.pp_print_space ppf ();
            pp_array ppf name array addr_space))
        arrays;
      Format.pp_close_box ppf ())

  let rec eval
      ?(symbols =
        fun e -> Bitvector.create (Z.of_int (Expr.hash e)) (Expr.sizeof e))
      ?(memory = fun _ _ -> '\x00') ((vars, values, _, _, _) as m) = function
    | Expr.Cst bv -> bv
    | e -> (
        try BvTbl.find values e
        with Not_found ->
          let value =
            match e with
            | Expr.Cst _ -> assert false
            | Expr.Var { name; _ } ->
                StTbl.add vars name e;
                symbols e
            | Expr.Load { addr; len; dir; label; _ } ->
                eval_load ~symbols ~memory m
                  (eval ~symbols ~memory m addr)
                  len dir label
            | Expr.Unary { f; x; _ } ->
                Term.Bv.unary f (eval ~symbols ~memory m x)
            | Expr.Binary { f; x; y; _ } ->
                Term.Bv.binary f
                  (eval ~symbols ~memory m x)
                  (eval ~symbols ~memory m y)
            | Expr.Ite { c; t; e; _ } ->
                if Bv.zero = eval ~symbols ~memory m c then
                  eval ~symbols ~memory m e
                else eval ~symbols ~memory m t
          in
          BvTbl.add values e value;
          value)

  and eval_load ~symbols ~memory ((_, _, cache, arrays, addr_size) as t) ptr len
      dir (memory_term : Memory.t) =
    match memory_term with
    | Root ->
        let index =
          match dir with
          | LittleEndian -> Bv.add_int ptr
          | BigEndian ->
              let hi = Bv.add_int ptr (len - 1) in
              fun i -> Bv.add_int hi (-i)
        in
        let bits =
          String.init len (fun i ->
              let x = Bv.value_of (index i) in
              try BiTbl.find cache x
              with Not_found ->
                let byte = memory memory_term (Bv.create x addr_size) in
                BiTbl.add cache x byte;
                byte)
        in
        Bv.create (Z.of_bits bits) (len lsl 3)
    | Symbol n ->
        let index =
          match dir with
          | LittleEndian -> Bv.add_int ptr
          | BigEndian ->
              let hi = Bv.add_int ptr (len - 1) in
              fun i -> Bv.add_int hi (-i)
        in
        let bits =
          String.init len (fun i ->
              let x = Bv.value_of (index i)
              and arr =
                try StTbl.find arrays n
                with Not_found ->
                  let arr = BiTbl.create 16 in
                  StTbl.add arrays n arr;
                  arr
              in
              try BiTbl.find arr x
              with Not_found ->
                let byte = memory memory_term (Bv.create x addr_size) in
                BiTbl.add arr x byte;
                byte)
        in
        Bv.create (Z.of_bits bits) (len lsl 3)
    | Layer { addr; store; over; _ } ->
        let addr = eval ~symbols ~memory t addr in
        let size = Bv.size_of addr in
        let offset = Bv.sub ptr addr in
        let miss i s =
          Chunk.of_term
            (Expr.load s Expr.LittleEndian
               (Expr.constant (Bv.add addr (Bv.create i size)))
               over)
        in
        let bytes = Chunk.to_term (Store.select miss offset len store) in
        let bytes =
          match dir with LittleEndian -> bytes | BigEndian -> bswap bytes
        in
        eval ~symbols ~memory t bytes
end

module BvSet = Set.Make (Expr)
module BvMap = Map.Make (Expr)
