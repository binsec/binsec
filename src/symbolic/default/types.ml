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

type trilean = Basic_types.Ternary.t = False | True | Unknown
type unary = Term.unary
type binary = Term.binary
type 'a operator = 'a Term.operator

module Bv = Bitvector
module BiMap = Basic_types.Integers.Bigint.Map

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
  val len : t -> int

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
      if is_unboxed x then of_bv (Bv.extract ~hi ~lo (to_bv x))
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

  let unsafe_to_term x =
    if is_unboxed x then Expr.constant (to_bv x) else unsafe_to_term x
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
    iter
      (fun k v ->
        if Chunk.is_hunk v then
          let h = Chunk.unsafe_to_hunk v in
          for i = 0 to Bigarray.Array1.dim h - 1 do
            f
              (Z.add k (Z.of_int i))
              (Expr.constant
                 (Bv.of_int ~size:8 (Bigarray.Array1.unsafe_get h i)))
          done
        else f k (Chunk.unsafe_to_term v))
      t

  let rec fold_hunk f b h k i len =
    if i < len then
      fold_hunk f
        (f
           (Z.add k (Z.of_int i))
           (Expr.constant (Bv.of_int ~size:8 (Bigarray.Array1.unsafe_get h i)))
           b)
        h k (i + 1) len
    else b

  let fold_term f b t =
    fold
      (fun k v b ->
        if Chunk.is_hunk v then
          let h = Chunk.unsafe_to_hunk v in
          fold_hunk f b h k 0 (Bigarray.Array1.dim h)
        else f k (Chunk.unsafe_to_term v) b)
      b t
end

and Memory : sig
  type 'a node =
    | None : [< `None | `Any ] node
    | Symbol : {
        id : int;
        name : string;
        index : int;
      }
        -> [< `Some | `Symbol | `Any ] node
    | Layer : {
        id : int;
        over : t;
        addr : Expr.t;
        store : Store.t;
      }
        -> [< `Some | `Any ] node

  and symbol = [ `Symbol ] node
  and nullable = [ `Any ] node
  and t = [ `Some ] node

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val none : nullable
  val any : t -> nullable
  val root : int -> t
  val symbol : string -> int -> t
  val layer : Expr.t -> Store.t -> t -> t
  val base : t -> symbol
end = struct
  type 'a node =
    | None : [< `None | `Any ] node
    | Symbol : {
        id : int;
        name : string;
        index : int;
      }
        -> [< `Some | `Symbol | `Any ] node
    | Layer : {
        id : int;
        over : t;
        addr : Expr.t;
        store : Store.t;
      }
        -> [< `Some | `Any ] node

  and symbol = [ `Symbol ] node
  and nullable = [ `Any ] node
  and t = [ `Some ] node

  let id = ref 0
  let hash : t -> int = function Symbol { id; _ } | Layer { id; _ } -> id
  let compare : t -> t -> int = fun t t' -> hash t - hash t'
  let equal : t -> t -> bool = fun t t' -> hash t = hash t'
  let none : nullable = None
  let any : t -> nullable = function (Symbol _ | Layer _) as node -> node

  let root : int -> t =
   fun index ->
    incr id;
    Symbol { id = !id; name = "@"; index }

  let symbol : string -> int -> t =
   fun name index ->
    incr id;
    Symbol { id = !id; name; index }

  let layer : Expr.t -> Store.t -> t -> t =
   fun addr store over ->
    incr id;
    Layer { id = !id; over; addr; store }

  let rec base : t -> symbol = function
    | Symbol _ as symbol -> symbol
    | Layer { over; _ } -> base over
end

module BvTbl = Hashtbl.Make (struct
  include Expr

  let equal = is_equal
end)

module AxTbl = Hashtbl.Make (Memory)

module AsMap = Map.Make (struct
  type t = Memory.symbol

  let compare : t -> t -> int =
   fun (Symbol _ as s0) (Symbol _ as s1) -> Memory.compare s0 s1
end)

module AsTbl = Hashtbl.Make (struct
  type t = Memory.symbol

  let equal : t -> t -> bool =
   fun (Symbol _ as s0) (Symbol _ as s1) -> Memory.equal s0 s1

  let hash : t -> int = fun (Symbol _ as s) -> Memory.hash s
end)

module BiTbl = Basic_types.Integers.Bigint.Htbl
module StTbl = Basic_types.String.Htbl
module S = Basic_types.String.Map

module IntMap = Map.Make (struct
  type t = Z.t

  let compare x y = -Z.compare x y
end)

module Model = struct
  type t = {
    symbols : Expr.t StTbl.t;
    values : Bv.t BvTbl.t;
    arrays : char BiTbl.t AsTbl.t;
  }

  let empty : unit -> t =
   fun () ->
    {
      symbols = StTbl.create 0;
      values = BvTbl.create 0;
      arrays = AsTbl.create 0;
    }

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

  let pp_memory_with_sections section_name ppf memory addr_space =
    Format.pp_print_string ppf "# Memory";
    Format.pp_print_cut ppf ();
    let pp_section ppf name =
      match name with
      | None -> Format.pp_print_string ppf "unamed section"
      | Some name -> Format.fprintf ppf "section %s" name
    in
    let last_section = ref (Some "--") in
    IntMap.iter (fun addr byte ->
        let name = section_name (Virtual_address.of_bigint addr) in
        if name <> !last_section then (
          Format.fprintf ppf "; %a@ " pp_section name;
          last_section := name);
        pp_bv ppf addr addr_space;
        Format.fprintf ppf " : %02x %a@ " (Char.code byte) maybe_pp_char byte)
    @@ BiTbl.fold IntMap.add memory IntMap.empty

  let pp_array ppf name array addr_space =
    Format.pp_print_string ppf "# Array ";
    Format.pp_print_string ppf name;
    Format.pp_print_cut ppf ();
    IntMap.iter (fun addr byte ->
        pp_bv ppf addr addr_space;
        Format.fprintf ppf " : %02x %a@ " (Char.code byte) maybe_pp_char byte)
    @@ BiTbl.fold IntMap.add array IntMap.empty

  let pp_with_sections :
      (Virtual_address.t -> string option) -> Format.formatter -> t -> unit =
   fun section_name ppf model ->
    if StTbl.length model.symbols = 0 && AsTbl.length model.arrays = 0 then
      Format.fprintf ppf "@[<h>--- Empty model ---@]"
    else (
      Format.fprintf ppf "@[<v 0>--- Model ---@ ";
      pp_variables ppf model.symbols model.values;
      AsTbl.iter
        (fun (Symbol { name; index; _ }) array ->
          if BiTbl.length array <> 0 then (
            Format.pp_print_space ppf ();
            if name = "@" then
              pp_memory_with_sections section_name ppf array index
            else pp_array ppf name array index))
        model.arrays;
      Format.pp_close_box ppf ())

  let pp : Format.formatter -> t -> unit =
   fun ppf t -> pp_with_sections (Fun.const None) ppf t

  let rec eval :
      ?symbols:(Expr.t -> Bitvector.t) ->
      ?memory:(Memory.symbol -> Bitvector.t -> char) ->
      t ->
      Expr.t ->
      Bv.t =
   fun ?(symbols =
         fun e -> Bitvector.create (Z.of_int (Expr.hash e)) (Expr.sizeof e))
       ?(memory = fun _ _ -> '\x00') model -> function
    | Cst bv -> bv
    | e -> (
        try BvTbl.find model.values e
        with Not_found ->
          let value =
            match e with
            | Expr.Cst _ -> assert false
            | Expr.Var { name; _ } ->
                StTbl.add model.symbols name e;
                symbols e
            | Expr.Load { addr; len; dir; label; _ } ->
                eval_load ~symbols ~memory model
                  (eval ~symbols ~memory model addr)
                  len dir label
            | Expr.Unary { f; x; _ } ->
                Term.Bv.unary f (eval ~symbols ~memory model x)
            | Expr.Binary { f; x; y; _ } ->
                Term.Bv.binary f
                  (eval ~symbols ~memory model x)
                  (eval ~symbols ~memory model y)
            | Expr.Ite { c; t; e; _ } ->
                if Bv.zero = eval ~symbols ~memory model c then
                  eval ~symbols ~memory model e
                else eval ~symbols ~memory model t
          in
          BvTbl.add model.values e value;
          value)

  and eval_load :
      symbols:(Expr.t -> Bv.t) ->
      memory:(Memory.symbol -> Bv.t -> char) ->
      t ->
      Bv.t ->
      int ->
      Machine.endianness ->
      Memory.t ->
      Bv.t =
   fun ~symbols ~memory model ptr len dir memory_term ->
    match memory_term with
    | Symbol { index; _ } as symbol ->
        let offset =
          match dir with
          | LittleEndian -> Bv.add_int ptr
          | BigEndian ->
              let hi = Bv.add_int ptr (len - 1) in
              fun i -> Bv.add_int hi (-i)
        in
        let ar =
          try AsTbl.find model.arrays symbol
          with Not_found ->
            let arr = BiTbl.create 16 in
            AsTbl.add model.arrays symbol arr;
            arr
        in
        let bits =
          String.init len (fun i ->
              let x = Bv.value_of (offset i) in
              try BiTbl.find ar x
              with Not_found ->
                let byte = memory symbol (Bv.create x index) in
                BiTbl.add ar x byte;
                byte)
        in
        Bv.create (Z.of_bits bits) (len lsl 3)
    | Layer { addr; store; over; _ } ->
        let addr = eval ~symbols ~memory model addr in
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
          match dir with
          | LittleEndian -> bytes
          | BigEndian -> Expr.byte_swap bytes
        in
        eval ~symbols ~memory model bytes

  let complement : from:t -> t -> unit =
   fun ~from partial_model ->
    AsTbl.iter
      (fun arr store ->
        match AsTbl.find partial_model.arrays arr with
        | exception Not_found -> AsTbl.add partial_model.arrays arr store
        | partial_store ->
            BiTbl.iter
              (fun idx value ->
                if not (BiTbl.mem partial_store idx) then
                  BiTbl.add partial_store idx value)
              store)
      from.arrays;
    StTbl.iter
      (fun name value ->
        if not (StTbl.mem partial_model.symbols name) then (
          StTbl.add partial_model.symbols name value;
          BvTbl.add partial_model.values value (BvTbl.find from.values value)))
      from.symbols
end

module BvSet = Set.Make (Expr)
module BvMap = Map.Make (Expr)
module BvHmap = Hmap.Make (Expr)
