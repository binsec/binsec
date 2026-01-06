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
  include Sigs.HASHABLE

  module Map : sig
    include Map.S with type key = t

    val pop : 'a t -> (key * 'a) * 'a t
    val keys : 'a t -> key list
    val values : 'a t -> 'a list
  end

  module Set : sig
    include Set.S with type elt = t

    val pop : t -> elt * t
  end

  module Htbl : sig
    include Hashtbl.S with type key = t

    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val bindings : 'a t -> (key * 'a) list
  end
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
