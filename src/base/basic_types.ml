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

type endianness = LittleEndian | BigEndian

module MapSetMaker (C : Sigs.COMPARABLE) = struct
  module Map = struct
    include Map.Make (C)

    let pop m =
      let ((k, _) as elt) = choose m in
      (elt, remove k m)

    let keys m = fold (fun k _ acc -> k :: acc) m [] |> List.rev
    let values m = fold (fun _ v acc -> v :: acc) m [] |> List.rev
  end

  module Set = struct
    include Set.Make (C)

    let pop set =
      let a = choose set in
      (a, remove a set)
  end
end

module Collection_make = struct
  module Hashed (C : Sigs.HASHABLE) = struct
    include C
    include MapSetMaker (C)

    module Htbl = struct
      include Hashtbl.Make (C)

      let filter p h =
        let h' = create (length h) in
        iter (fun k v -> if p k v then add h' k v) h;
        h'

      let bindings h = fold (fun k v acc -> (k, v) :: acc) h []
    end
  end

  module Auto (C : Sigs.COMPARABLE_EXT) = Hashed (struct
    include C

    let hash = Hashtbl.hash
  end)

  module Default (C : Sigs.COMPARABLE) = Auto (struct
    include C

    let equal a b = compare a b = 0
  end)
end

type 'a interval = { lo : 'a; hi : 'a }

module Integers = struct
  type uint8 = int
  type uint16 = int
  type uint32 = int32
  type uint64 = int64
  type int8 = int
  type int16 = int

  module type S = sig
    type t

    val to_uint8 : t -> uint8
    val to_uint16 : t -> uint16
    val to_uint32 : t -> uint32
    val to_uint64 : t -> uint64
    val to_int8 : t -> int8
    val to_int16 : t -> int16
    val to_int32 : t -> int32
    val to_int64 : t -> int64
    val to_int : t -> int
    val to_bigint : t -> Z.t
  end

  module Uint8 : sig
    include S with type t = uint8

    external to_char : t -> char = "%identity"
    external to_int : t -> int = "%identity"
  end = struct
    type t = uint8

    external to_char : t -> char = "%identity"

    let to_uint8 = Fun.id
    let to_uint16 = Fun.id
    let to_uint32 = Int32.of_int
    let to_uint64 = Int64.of_int
    let to_int8 = to_uint8
    let to_int16 = to_uint16
    let to_int32 = to_uint32
    let to_int64 = to_uint64

    external to_int : t -> int = "%identity"

    let to_bigint = Z.of_int
  end

  module Uint16 : sig
    include S with type t = uint16

    external to_int : t -> int = "%identity"
  end = struct
    type t = uint16

    let to_uint8 x = x land 0xff
    let to_uint16 = Fun.id
    let to_uint32 = Int32.of_int
    let to_uint64 = Int64.of_int
    let to_int8 = to_uint8
    let to_int16 = to_uint16
    let to_int32 = to_uint32
    let to_int64 = to_uint64

    external to_int : t -> int = "%identity"

    let to_bigint = Z.of_int
  end

  module Uint32 : S with type t = uint32 = struct
    type t = uint32

    let to_uint8 x = Int32.to_int (Int32.logand x 0xffl)
    let to_uint16 x = Int32.to_int (Int32.logand x 0xffffl)
    let to_uint32 = Fun.id
    let to_uint64 x = Int64.logand (Int64.of_int32 x) 0xffffffffL
    let to_int8 = to_uint8
    let to_int16 = to_uint16
    let to_int32 = to_uint32
    let to_int64 = to_uint64
    let to_bigint x = Z.of_int32_unsigned x
    let to_int x = Z.to_int (to_bigint x)
  end

  module Uint64 : S with type t = uint64 = struct
    type t = uint64

    let to_uint8 x = Int64.to_int (Int64.logand x 0xffL)
    let to_uint16 x = Int64.to_int (Int64.logand x 0xffffL)
    let to_uint32 x = Int64.to_int32 (Int64.logand x 0xffffffffL)
    let to_uint64 = Fun.id
    let to_int8 = to_uint8
    let to_int16 = to_uint16
    let to_int32 = to_uint32
    let to_int64 = to_uint64
    let to_bigint x = Z.of_int64_unsigned x
    let to_int x = Z.to_int (to_bigint x)
  end

  module Int8 : S with type t = int8 = struct
    type t = int8

    let to_int x = (x lxor 0x80) - 0x80
    let to_uint8 = Fun.id
    let to_uint16 x = to_int x land 0xffff
    let to_uint32 x = Int32.of_int (to_int x)
    let to_uint64 x = Int64.of_int (to_int x)
    let to_int8 = to_uint8
    let to_int16 = to_uint16
    let to_int32 = to_uint32
    let to_int64 = to_uint64
    let to_bigint x = Z.of_int (to_int x)
  end

  module Int16 : S with type t = int16 = struct
    type t = int16

    let to_int x = (x lxor 0x8000) - 0x8000
    let to_uint8 = Uint16.to_int8
    let to_uint16 = Fun.id
    let to_uint32 x = Int32.of_int (to_int x)
    let to_uint64 x = Int64.of_int (to_int x)
    let to_int8 = to_uint8
    let to_int16 = to_uint16
    let to_int32 = to_uint32
    let to_int64 = to_uint64
    let to_bigint x = Z.of_int (to_int x)
  end

  module Int32 : sig
    include module type of Int32
    include S with type t = int32
  end = struct
    include Int32

    let to_uint8 = Uint32.to_uint8
    let to_uint16 = Uint32.to_uint16
    let to_uint32 = Fun.id
    let to_uint64 = Int64.of_int32
    let to_int8 = to_uint8
    let to_int16 = to_uint16
    let to_int32 = to_uint32
    let to_int64 = to_uint64
    let to_bigint = Z.of_int32
  end

  module Int64 : sig
    include module type of Int64
    include S with type t = int64
  end = struct
    include Int64

    let to_uint8 = Uint64.to_uint8
    let to_uint16 = Uint64.to_uint16
    let to_uint32 = Uint64.to_uint32
    let to_uint64 = Fun.id
    let to_int8 = to_uint8
    let to_int16 = to_uint16
    let to_int32 = to_uint32
    let to_int64 = to_uint64
    let to_bigint = Z.of_int64
  end

  module Int : sig
    include Collection.S with type t = int
    include S with type t := int

    external unsafe_to_uint8 : t -> uint8 = "%identity"
  end = struct
    include Collection_make.Auto (Int)

    let to_uint8 x = x land 0xff
    let to_uint16 x = x land 0xffff
    let to_uint32 = Int32.of_int
    let to_uint64 = Int64.of_int
    let to_int8 = to_uint8
    let to_int16 = to_uint16
    let to_int32 = to_uint32
    let to_int64 = to_uint64
    let to_int = Fun.id
    let to_bigint = Z.of_int

    external unsafe_to_uint8 : t -> uint8 = "%identity"
  end

  module Bigint : sig
    include module type of Z
    include Collection.S with type t := Z.t
    include S with type t := Z.t
  end = struct
    include Z
    include Collection.Hashed (Z)

    let to_uint8 x = Z.to_int (Z.extract x 0 8)
    let to_uint16 x = Z.to_int (Z.extract x 0 16)
    let to_uint32 = to_int32_unsigned
    let to_uint64 = to_int64_unsigned
    let to_int8 = to_uint8
    let to_int16 = to_uint16
    let to_bigint = Fun.id
  end
end

module Constants = struct
  let bytesize = Natural.create 8
end

module Float = Collection_make.Default (Float)
module String = Collection_make.Auto (String)

module Ternary = struct
  type t = Domains.trilean = False | True | Unknown

  let of_bool = function true -> True | false -> False

  let to_bool ?(unknown = false) = function
    | True -> true
    | False -> false
    | Unknown -> unknown

  let lognot = function True -> False | False -> True | Unknown -> Unknown

  let logand t1 t2 =
    match (t1, t2) with
    | True, True -> True
    | _, False | False, _ -> False
    | _, _ -> Unknown

  let logor t1 t2 =
    match (t1, t2) with
    | False, False -> False
    | True, _ | _, True -> True
    | _, _ -> Unknown
end
