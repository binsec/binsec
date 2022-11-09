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

let byte_size = Natural.to_int Basic_types.Constants.bytesize

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

and Memory : sig
  type t =
    | Unknown
    | Source of {
        id : int;
        over : t;
        addr : Bv.t;
        orig : Loader_buf.t;
        len : int;
      }
    | Layer of {
        id : int;
        over : t;
        addr : Expr.t;
        bytes : Expr.t BiMap.t;
        pop : int;
      }

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val hash : t -> int

  val source : addr:Bv.t -> len:int -> Loader_buf.t -> t -> t

  val write : addr:Expr.t -> Expr.t -> Expr.endianness -> t -> t

  val read : addr:Expr.t -> int -> Expr.endianness -> t -> Expr.t
end = struct
  type t =
    | Unknown
    | Source of {
        id : int;
        over : t;
        addr : Bv.t;
        orig : Loader_buf.t;
        len : int;
      }
    | Layer of {
        id : int;
        over : t;
        addr : Expr.t;
        bytes : Expr.t BiMap.t;
        pop : int;
      }

  let hash = function Unknown -> 0 | Source { id; _ } | Layer { id; _ } -> id

  let compare t t' = hash t - hash t'

  let equal t t' = hash t = hash t'

  let source ~addr ~len orig over =
    Source { id = hash over + 1; orig; addr; len; over }

  let byte n value =
    Expr.restrict ~lo:(byte_size * n) ~hi:((byte_size * (n + 1)) - 1) value

  let split dir value offset =
    let len = Expr.sizeof value / byte_size in
    match dir with
    | Expr.LittleEndian ->
        let rec fold n value offset map =
          if n = 0 then map
          else
            let n = n - 1 in
            fold n value offset
              (BiMap.add (Z.add offset (Z.of_int n)) (byte n value) map)
        in
        (fold len value offset BiMap.empty, len)
    | Expr.BigEndian ->
        let rec fold i n value offset map =
          if i = n then map
          else
            fold (i + 1) n value offset
              (BiMap.add
                 (Z.add offset (Z.of_int i))
                 (byte (n - i - 1) value)
                 map)
        in
        (fold 0 len value offset BiMap.empty, len)

  let layer addr value dir over =
    let bytes, pop = split dir value Z.zero in
    Layer { id = hash over + 1; over; addr; bytes; pop }

  let write ~addr value dir over =
    match over with
    | Unknown | Source _ -> layer addr value dir over
    | Layer { id; addr = addr'; bytes = bytes'; pop = pop'; over = over' } -> (
        match Expr.sub addr addr' with
        | Expr.Cst bv ->
            let offset = Bv.signed_of bv in
            let bytes, pop = split dir value offset in
            let cnt = ref (pop' + pop) in
            let bytes =
              BiMap.union
                (fun _ _ b ->
                  decr cnt;
                  Some b)
                bytes' bytes
            in
            Layer { id = id + 1; over = over'; addr = addr'; bytes; pop = !cnt }
        | _ -> layer addr value dir over)

  let read =
    let concat dir buf =
      match dir with
      | Expr.LittleEndian ->
          let value = ref Array.(get buf (length buf - 1)) in
          for i = Array.length buf - 2 downto 0 do
            value := Expr.append !value (Array.get buf i)
          done;
          !value
      | Expr.BigEndian ->
          let value = ref Array.(get buf 0) in
          for i = 1 to Array.length buf - 1 do
            value := Expr.append !value (Array.get buf i)
          done;
          !value
    in
    let fill dir addr map buf memory =
      let map = ref map in
      let load = Expr.load (Array.length buf) dir addr memory in
      while !map <> Z.zero do
        let x = Z.trailing_zeros !map in
        Array.set buf x (byte x load);
        map := Z.(!map lxor (one lsl x))
      done
    in
    let rec lookup dir addr map buf memory =
      match memory with
      | Memory.Unknown -> fill dir addr map buf memory
      | Memory.Source { addr = base; len; orig; over; _ } -> (
          match addr with
          | Expr.Cst bv ->
              let offset = Bv.sub bv base in
              let map = ref map and map' = ref Z.zero in
              while !map <> Z.zero do
                let x = Z.trailing_zeros !map in
                (try
                   let y = Z.to_int (Bv.value_of (Bv.add_int offset x)) in
                   if y < len then
                     let v =
                       if y < Bigarray.Array1.dim orig then
                         Bigarray.Array1.unsafe_get orig y
                       else 0
                     in
                     Array.set buf x
                       (Expr.constant (Bv.of_int ~size:byte_size v))
                   else map' := Z.(!map' lor (one lsl x))
                 with Z.Overflow -> map' := Z.(!map' lor (one lsl x)));
                map := Z.(!map lxor (one lsl x))
              done;
              if !map' <> Z.zero then lookup dir addr !map' buf over
          | _ -> fill dir addr map buf memory)
      | Layer { addr = addr'; bytes; over; _ } -> (
          match Expr.sub addr addr' with
          | Expr.Cst bv ->
              let offset = Bv.signed_of bv in
              let map = ref map and map' = ref Z.zero in
              while !map <> Z.zero do
                let x = Z.trailing_zeros !map in
                let y = Z.add offset (Z.of_int x) in
                (match BiMap.find y bytes with
                | byte -> Array.set buf x byte
                | exception Not_found -> map' := Z.(!map' lor (one lsl x)));
                map := Z.(!map lxor (one lsl x))
              done;
              if !map' <> Z.zero then lookup dir addr !map' buf over
          | _ -> fill dir addr map buf memory)
    in
    fun ~addr bytes dir memory ->
      let buf = Array.make bytes Expr.zero (* no value *) in
      lookup dir addr (Z.pred (Z.shift_left Z.one bytes)) buf memory;
      concat dir buf
end

module BvTbl = Hashtbl.Make (struct
  include Expr

  let equal = is_equal
end)

module AxTbl = Hashtbl.Make (Memory)
