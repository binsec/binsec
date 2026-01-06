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

  (* module Hamt : Hashamt.S with type key = t *)

  module Htbl : sig
    include Hashtbl.S with type key = t

    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val bindings : 'a t -> (key * 'a) list
  end
end

module Default (C : Sigs.COMPARABLE) : S with type t = C.t
module Auto (C : Sigs.COMPARABLE_EXT) : S with type t = C.t
module Hashed (C : Sigs.HASHABLE) : S with type t = C.t
