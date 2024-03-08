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

type 'a key = 'a Types.key

external key : int -> 'a key = "%identity"

type data
and proc = data -> data -> data option

external make : int -> int -> 'a array = "caml_obj_block"
external get : data array -> 'a key -> 'a = "%obj_field"
external set : data array -> 'a key -> 'a -> unit = "%obj_set_field"
external handler : ('a -> 'a -> 'a option) -> proc = "%identity"

module type S = sig
  type t

  val id : t -> int
  val get : 'a key -> t -> 'a
  val set : 'a key -> 'a -> t -> unit
  val register_key : ?merge:('a -> 'a -> 'a option) -> 'a -> 'a key
  val register_at_fork : (t -> t -> unit) -> unit
  val register_at_end : (t -> Types.status -> unit) -> unit
end

module Make () : sig
  include S

  val empty : unit -> t
  val fork : t -> t
  val merge : t -> t -> t option
  val terminate : t -> Types.status -> unit
end = struct
  type t = data array

  let id : int key = key 0
  let merger : (data -> data -> data option) array ref = ref (make 0 16)
  let matrice : data array ref = ref (make 0 16)

  let n = ref 0
  and s = ref 1

  let at_fork_callbacks = Queue.create ()
  let register_at_fork f = Queue.add f at_fork_callbacks
  let at_end_callbacks = Queue.create ()
  let register_at_end f = Queue.add f at_end_callbacks
  let default_merge x y = if x == y then Some x else None

  let register_key : ?merge:('a -> 'a -> 'a option) -> 'a -> 'a key =
   fun ?merge default ->
    if !n > 0 then
      Options.Logger.fatal
        "data registration should happen before the start of exploration";
    let l = Array.length !matrice in
    let k = key !s in
    if !s >= l then (
      let merger' = Array.make (2 * l) (Array.get !merger 0) in
      Array.blit !merger 0 merger' 0 l;
      merger := merger';
      let matrice' = Array.make (2 * l) (Array.get !matrice 0) in
      Array.blit !matrice 0 matrice' 0 l;
      matrice := matrice');
    let merge = Option.fold ~none:default_merge ~some:handler merge in
    Array.set !merger !s merge;
    set !matrice k default;
    incr s;
    k

  let empty () =
    let t = Array.sub !matrice 0 !s in
    incr n;
    set t id !n;
    t

  let fork t =
    let t' = Array.copy t in
    incr n;
    set t' id !n;
    Queue.iter (fun f -> f t t') at_fork_callbacks;
    t'

  let rec merge t'' t t' i =
    if i = !s then Some t''
    else
      match (Array.get !merger i) (Array.get t i) (Array.get t' i) with
      | None -> None
      | Some data ->
          Array.set t'' i data;
          merge t'' t t' (i + 1)

  let merge t t' =
    let t'' = Array.copy t in
    merge t'' t t' 1

  let terminate t status = Queue.iter (fun f -> f t status) at_end_callbacks
  let id t = get t id
  let get k t = get t k
  let set k v t = set t k v
end
