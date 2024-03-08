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

type 'a interval = { lo : 'a; hi : 'a }
type u8 = int

module Constants = struct
  let bytesize = Natural.create 8
end

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
    module Hamt = Hashamt.Make (C)

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

module Float = Collection_make.Default (Float)
module String = Collection_make.Auto (String)

module Int64 = struct
  let max n1 n2 = if Int64.compare n1 n2 <= 0 then n2 else n1

  (* We use here the same kind of name used for big int *)
  let is_int_int64 =
    let max_int_int64 = Int64.of_int max_int in
    let min_int_int64 = Int64.of_int min_int in
    fun n -> min_int_int64 <= n && max_int_int64 >= n

  include Collection_make.Auto (Int64)
end

module Addr64 = Int64
module Int = Collection_make.Auto (Int)
module BigInt = Collection_make.Default (Z)

module Ternary = struct
  type t = True | False | Unknown

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
