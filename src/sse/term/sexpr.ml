(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2022                                               *)
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

exception Non_mergeable = Types.Non_mergeable

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

  type buffer =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  val of_buf : buffer -> t

  val of_expr : Expr.t -> t

  val to_expr : t -> Expr.t

  val is_exported : t -> bool

  val equal : t -> t -> bool
end = struct
  type t

  type buffer =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  external is_unboxed : t -> bool = "%obj_is_int"

  external still_unboxed : Bv.t -> bool = "%obj_is_int"

  external to_bv : t -> Bv.t = "%identity"

  external of_bv : Bv.t -> t = "%identity"

  external to_expr : t -> Expr.t = "%identity"

  external of_expr : Expr.t -> t = "%identity"

  external to_buf : t -> buffer = "%identity"

  external of_buf : buffer -> t = "%identity"

  let is_buf x = Obj.tag (Obj.repr x) = Obj.custom_tag

  let of_bv bv =
    if still_unboxed bv then of_bv bv else of_expr (Expr.constant bv)

  let equal x y =
    x == y
    || (not (is_unboxed x))
       && (not (is_buf x))
       && (not (is_unboxed y))
       && (not (is_buf y))
       && Expr.is_equal (to_expr x) (to_expr y)

  let len x =
    if is_unboxed x then Bv.size_of (to_bv x) lsr 3
    else if is_buf x then Bigarray.Array1.dim (to_buf x)
    else Expr.sizeof (to_expr x) lsr 3

  let crop ~lo ~hi x =
    if is_buf x then of_buf (Bigarray.Array1.sub (to_buf x) lo (hi - lo + 1))
    else
      let lo = lo lsl 3 and hi = (hi lsl 3) + 7 in
      if is_unboxed x then of_bv (Bv.extract (to_bv x) { lo; hi })
      else of_expr (Expr.restrict ~lo ~hi (to_expr x))

  let buf_to_bv x =
    let x = to_buf x in
    let s = Bigarray.Array1.dim x in
    Bv.create
      (Z.of_bits
         (String.init (Bigarray.Array1.dim x) (fun i ->
              Char.unsafe_chr (Bigarray.Array1.unsafe_get x i))))
      (s lsl 3)

  let concat x y =
    if is_unboxed x then
      if is_unboxed y then of_bv (Bv.append (to_bv x) (to_bv y))
      else if is_buf y then of_bv (Bv.append (to_bv x) (buf_to_bv y))
      else of_expr (Expr.append (Expr.constant (to_bv x)) (to_expr y))
    else if is_buf x then
      if is_unboxed y then of_bv (Bv.append (buf_to_bv x) (to_bv y))
      else if is_buf y then of_bv (Bv.append (buf_to_bv x) (buf_to_bv y))
      else of_expr (Expr.append (Expr.constant (buf_to_bv x)) (to_expr y))
    else if is_unboxed y then
      of_expr (Expr.append (to_expr x) (Expr.constant (to_bv y)))
    else if is_buf y then
      of_expr (Expr.append (to_expr x) (Expr.constant (buf_to_bv y)))
    else of_expr (Expr.append (to_expr x) (to_expr y))

  let to_expr x =
    if is_unboxed x then Expr.constant (to_bv x)
    else if is_buf x then Expr.constant (buf_to_bv x)
    else to_expr x

  let of_expr (x : Expr.t) = match x with Cst bv -> of_bv bv | x -> of_expr x

  let is_exported x = not (is_buf x)
end

and Store : sig
  type t

  val empty : t

  val singleton : Bv.t -> Chunk.t -> t

  val store : Bv.t -> Chunk.t -> t -> t

  val select : (Z.t -> int -> Chunk.t) -> Bv.t -> int -> t -> Chunk.t

  val iter : (Z.t -> Expr.t -> unit) -> t -> unit

  val fold : (Z.t -> Expr.t -> 'a -> 'a) -> 'a -> t -> 'a

  val map : (Z.t -> Chunk.t -> Chunk.t) -> t -> t

  val merge :
    (Z.t -> Chunk.t option -> Chunk.t option -> Chunk.t option) -> t -> t -> t
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

  let iter f t =
    iter (fun k v -> if Chunk.is_exported v then f k (Chunk.to_expr v)) t

  let fold f b t =
    fold
      (fun k v b -> if Chunk.is_exported v then f k (Chunk.to_expr v) b else b)
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

  val source : addr:Expr.t -> len:int -> Loader_buf.t -> t -> t

  val write : addr:Expr.t -> Expr.t -> Expr.endianness -> t -> t

  val read : addr:Expr.t -> int -> Expr.endianness -> t -> Expr.t

  val merge : Expr.t -> t -> t -> t

  val bswap : Expr.t -> Expr.t
end = struct
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

  let rebase (addr : Expr.t) =
    match addr with
    | Cst bv -> (Expr.zeros (Bv.size_of bv), bv)
    | Binary { f = Plus; x; y = Cst bv; _ } -> (x, bv)
    | Binary { f = Minus; x; y = Cst bv; _ } -> (x, Bv.neg bv)
    | _ -> (addr, Bv.zeros (Expr.sizeof addr))

  let blit offset buf len over =
    let s = Bigarray.Array1.dim buf in
    if len <= s then
      let buf = Bigarray.Array1.sub buf 0 len in
      Store.store offset (Chunk.of_buf buf) over
    else
      let buf' =
        Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.C_layout (len - s)
      in
      Bigarray.Array1.fill buf' 0;
      if s = 0 then Store.store offset (Chunk.of_buf buf') over
      else
        Store.store (Bv.add_int offset s) (Chunk.of_buf buf')
          (Store.store offset (Chunk.of_buf buf) over)

  let fill addr len orig over =
    let addr, offset = rebase addr in
    incr id;
    Layer { id = !id; over; addr; store = blit offset orig len Store.empty }

  let source ~addr ~len orig over =
    match over with
    | Root | Symbol _ -> fill addr len orig over
    | Layer { addr = addr'; store = store'; over = over'; _ } -> (
        match Expr.sub addr addr' with
        | Expr.Cst bv ->
            let store = blit bv orig len store' in
            incr id;
            Layer { id = !id; over = over'; addr = addr'; store }
        | _ -> fill addr len orig over)

  let layer addr value over =
    let addr, offset = rebase addr in
    incr id;
    Layer
      {
        id = !id;
        over;
        addr;
        store = Store.singleton offset (Chunk.of_expr value);
      }

  let write ~addr value (dir : Expr.endianness) over =
    let value =
      match dir with LittleEndian -> value | BigEndian -> bswap value
    in
    match over with
    | Root | Symbol _ -> layer addr value over
    | Layer { addr = addr'; store = store'; over = over'; _ } -> (
        match Expr.sub addr addr' with
        | Expr.Cst bv ->
            let store = Store.store bv (Chunk.of_expr value) store' in
            incr id;
            Layer { id = !id; over = over'; addr = addr'; store }
        | _ -> layer addr value over)

  let read ~addr bytes (dir : Expr.endianness) memory =
    match memory with
    | Root | Symbol _ -> Expr.load bytes dir addr memory
    | Layer { addr = addr'; store; over; _ } -> (
        match Expr.sub addr addr' with
        | Expr.Cst bv -> (
            let miss i s =
              Chunk.of_expr
                (Expr.load s Expr.LittleEndian (Expr.addz addr' i) over)
            in
            let bytes = Chunk.to_expr (Store.select miss bv bytes store) in
            match dir with LittleEndian -> bytes | BigEndian -> bswap bytes)
        | _ -> Expr.load bytes dir addr memory)

  let rec merge c t t' =
    if t == t' then t
    else
      match (t, t') with
      | Layer { over; addr; store; _ }, t' when over == t' ->
          incr id;
          let id = !id
          and store =
            Store.map
              (fun offset chunk ->
                if Chunk.is_exported chunk then
                  let value = Chunk.to_expr chunk in
                  let size = Expr.sizeof value in
                  Chunk.of_expr
                    (Expr.ite c value
                       (read ~addr:(Expr.addz addr offset) (size / 8)
                          LittleEndian over))
                else raise_notrace Non_mergeable)
              store
          in
          Layer { id; over; addr; store }
      | t, Layer { over; _ } when t == over -> merge (Expr.lognot c) t' t
      | ( Layer { over; addr; store; _ },
          Layer { over = over'; addr = addr'; store = store'; _ } )
        when Expr.is_equal addr addr' && over == over' ->
          incr id;
          let id = !id
          and store =
            Store.merge
              (fun offset o0 o1 ->
                match (o0, o1) with
                | Some c0, Some c1 ->
                    if Chunk.equal c0 c1 then o0
                    else
                      Some
                        (Chunk.of_expr
                           (Expr.ite c (Chunk.to_expr c0) (Chunk.to_expr c1)))
                | Some c0, None ->
                    let value = Chunk.to_expr c0 in
                    let size = Expr.sizeof value in
                    Some
                      (Chunk.of_expr
                         (Expr.ite c value
                            (read ~addr:(Expr.addz addr offset) (size / 8)
                               LittleEndian over)))
                | None, Some c1 ->
                    let value = Chunk.to_expr c1 in
                    let size = Expr.sizeof value in
                    Some
                      (Chunk.of_expr
                         (Expr.ite c
                            (read ~addr:(Expr.addz addr offset) (size / 8)
                               LittleEndian over)
                            value))
                | None, None -> None)
              store store'
          in
          Layer { id; over; addr; store }
      | (Root | Symbol _ | Layer _), (Root | Symbol _ | Layer _) ->
          raise_notrace Non_mergeable
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

module Model = struct
  type t = Bv.t BvTbl.t * char BiTbl.t * char BiTbl.t StTbl.t

  let empty () = (BvTbl.create 0, BiTbl.create 0, StTbl.create 0)

  let maybe_pp_char ppf c =
    if String_utils.is_char_printable c then Format.fprintf ppf " (%c)" c

  let pp_variables ppf vars values =
    if S.is_empty vars = false then (
      Format.pp_print_string ppf "# Variables";
      Format.pp_print_cut ppf ();
      S.iter
        (fun name list ->
          let list = List.rev list in
          Format.fprintf ppf "%s : @[<hov>%a@]@ " name
            (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf var ->
                 match BvTbl.find values var with
                 | exception Not_found -> Format.pp_print_string ppf "--"
                 | bv -> Bitvector.pp_hex_or_bin ppf bv))
            list;
          match list with
          | var :: _ :: _ when Expr.sizeof var = 8 ->
              Format.pp_print_string ppf "  [as ascii] ";
              List.iter
                (fun var ->
                  match BvTbl.find values var with
                  | exception Not_found -> Format.pp_print_string ppf "."
                  | bv -> Format.pp_print_char ppf (Bitvector.to_char bv))
                list;
              Format.pp_print_space ppf ()
          | _ -> ())
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
        let address = Virtual_address.to_int (Virtual_address.of_bigint addr) in
        match Loader_utils.find_section_by_address ~address img with
        | None -> noname
        | Some section -> Loader.Section.name section
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

  let pp ppf vars addr_space (values, memory, arrays) =
    if S.is_empty vars && BiTbl.length memory = 0 && StTbl.length arrays = 0
    then Format.fprintf ppf "@[<h>--- Empty model ---@]"
    else (
      Format.fprintf ppf "@[<v 0>--- Model ---@ ";
      pp_variables ppf vars values;
      Format.pp_print_space ppf ();
      pp_memory ppf memory addr_space;
      Format.pp_close_box ppf ())

  let rec eval ((vars, _, _) as m) = function
    | Expr.Cst bv -> bv
    | e -> (
        try BvTbl.find vars e
        with Not_found ->
          let size = Expr.sizeof e in
          let value =
            match e with
            | Expr.Cst _ -> assert false
            | Expr.Var _ -> Bitvector.create (Z.of_int (Expr.hash e)) size
            | Expr.Load { addr; len; dir; label; _ } ->
                eval_load m (eval m addr) len dir label
            | Expr.Unary { f; x; _ } -> Term.Bv.unary f (eval m x)
            | Expr.Binary { f; x; y; _ } ->
                Term.Bv.binary f (eval m x) (eval m y)
            | Expr.Ite { c; t; e; _ } ->
                if Bv.zero = eval m c then eval m e else eval m t
          in
          BvTbl.add vars e value;
          value)

  and eval_load ((_, cache, arrays) as t) ptr len dir (memory : Memory.t) =
    match memory with
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
              try BiTbl.find cache (Bv.value_of (index i))
              with Not_found -> '\x00')
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
              try BiTbl.find (StTbl.find arrays n) (Bv.value_of (index i))
              with Not_found -> '\x00')
        in
        Bv.create (Z.of_bits bits) (len lsl 3)
    | Layer { addr; store; over; _ } ->
        let addr = eval t addr in
        let size = Bv.size_of addr in
        let offset = Bv.sub ptr addr in
        let miss i s =
          Chunk.of_expr
            (Expr.load s Expr.LittleEndian
               (Expr.constant (Bv.add addr (Bv.create i size)))
               over)
        in
        let bytes = Chunk.to_expr (Store.select miss offset len store) in
        let bytes =
          match dir with
          | LittleEndian -> bytes
          | BigEndian -> Memory.bswap bytes
        in
        eval t bytes
end
