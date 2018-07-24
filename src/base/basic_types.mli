(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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

(** Definitions of non-DBA types *)

type 'a interval = { lo : 'a; hi : 'a }

type u8 = int

(** {2 Maps & Sets on base types } *)

(* HINT:
   Always use fully-qualified names when using these modules in order not to
   clash with the standard library
*)
module String : Sigs.Collection with type t = string

module Int : Sigs.Collection with type t = int

module BigInt : Sigs.Collection with type t = Bigint.t

module Float : Sigs.Collection with type t = float

module Int64 : sig
  include Sigs.Collection with type t = Int64.t
  val max : t -> t -> t

  val is_int_int64: t -> bool
  (** [is_int_int64 n] returns [true] if the value of [n] is also representable
      as an OCaml [int] on your machine
  *)

end

module Addr64 = Int64

(** {2 Functors } *)

module Collection_make : sig
 module Default(C:Sigs.COMPARABLE) : Sigs.Collection with type t = C.t
 module Hashed(C: Sigs.HASHABLE) : Sigs.Collection with type t = C.t
end

(** {2 Specific modules & types} *)


module Constants : sig
  val bytesize : Natural.t
end

(** {2 Ternary logic} *)
module Ternary : sig
  type t =
    | True
    | False
    | Unknown

  val of_bool : bool -> t

  val to_bool : ?unknown:bool -> t -> bool
  (** [to_bool t] translates a ternary value to its boolean equivalent.
      For [Unknown] the value is given by [unknown] and defaults to [false].
  *)

  (** {3 Operations} *)

  include Sigs.Logical with type t := t
end

module List : sig
  val length : 'a list -> int
  val cons : 'a -> 'a list -> 'a list
  val hd : 'a list -> 'a
  val tl : 'a list -> 'a list
  val nth : 'a list -> int -> 'a
  val rev : 'a list -> 'a list
  val append : 'a list -> 'a list -> 'a list
  val rev_append : 'a list -> 'a list -> 'a list
  val concat : 'a list list -> 'a list
  val flatten : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val iteri : (int -> 'a -> unit) -> 'a list -> unit
  val map : ('a -> 'b) -> 'a list -> 'b list
  val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
  val rev_map : ('a -> 'b) -> 'a list -> 'b list
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
  val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
  val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
  val for_all : ('a -> bool) -> 'a list -> bool
  val exists : ('a -> bool) -> 'a list -> bool
  val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val mem : 'a -> 'a list -> bool
  val memq : 'a -> 'a list -> bool
  val find : ('a -> bool) -> 'a list -> 'a
  val filter : ('a -> bool) -> 'a list -> 'a list
  val find_all : ('a -> bool) -> 'a list -> 'a list
  val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
  val assoc : 'a -> ('a * 'b) list -> 'b
  val assq : 'a -> ('a * 'b) list -> 'b
  val mem_assoc : 'a -> ('a * 'b) list -> bool
  val mem_assq : 'a -> ('a * 'b) list -> bool
  val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
  val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
  val split : ('a * 'b) list -> 'a list * 'b list
  val combine : 'a list -> 'b list -> ('a * 'b) list
  val sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list
  val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
  val pop : 'a list -> 'a * 'a list
  val make : int -> 'a -> 'a list
end

module Array : sig
  val length : 'a array -> int
  val get : 'a array -> int -> 'a
  val set : 'a array -> int -> 'a -> unit
  val make : int -> 'a -> 'a array
  val create_float : int -> float array
  val init : int -> (int -> 'a) -> 'a array
  val make_matrix : int -> int -> 'a -> 'a array array
  val append : 'a array -> 'a array -> 'a array
  val concat : 'a array list -> 'a array
  val sub : 'a array -> int -> int -> 'a array
  val copy : 'a array -> 'a array
  val fill : 'a array -> int -> int -> 'a -> unit
  val blit : 'a array -> int -> 'a array -> int -> int -> unit
  val to_list : 'a array -> 'a list
  val of_list : 'a list -> 'a array
  val iter : ('a -> unit) -> 'a array -> unit
  val iteri : (int -> 'a -> unit) -> 'a array -> unit
  val map : ('a -> 'b) -> 'a array -> 'b array
  val mapi : (int -> 'a -> 'b) -> 'a array -> 'b array
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
  val fold_right : ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a
  val iter2 : ('a -> 'b -> unit) -> 'a array -> 'b array -> unit
  val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
  val for_all : ('a -> bool) -> 'a array -> bool
  val exists : ('a -> bool) -> 'a array -> bool
  val mem : 'a -> 'a array -> bool
  val memq : 'a -> 'a array -> bool
  val sort : ('a -> 'a -> int) -> 'a array -> unit
  val stable_sort : ('a -> 'a -> int) -> 'a array -> unit
  val fast_sort : ('a -> 'a -> int) -> 'a array -> unit
  val find : ('a -> bool) -> 'a array -> 'a
end
