(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
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

module Constants = struct
  let bytesize = 8
end

module Natural = struct
  type t = int

  let create n =
    assert (n >= 0);
    n

  let add_int n i = create (n + i)
  let add n1 n2 = add_int n1 n2
  let sub_int n i = create (n - i)
  let sub n1 n2 = sub_int n1 n2
  let eq = (=)
  let gt = (>)
  let ge = (>=)
end


module type Size = sig
  type t = Natural.t
  val create : int -> t
  val to_int : t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val pp_hex : Format.formatter -> t -> unit
  val add : t -> t -> t
  val sub : t -> t -> t
  val div : t -> t -> t
  val mul : t -> t -> t
  val pred : t -> t
  val is_zero : t -> bool
end


module CommonSize = struct
  type t = Natural.t
  let create = Natural.create
  let pp ppf t = Format.fprintf ppf "%d" t
  let pp_hex ppf t = Format.fprintf ppf "%x" t
  let to_int n = n
  let equal = (=)
  let add = (+)
  let sub = (-)
  let mul x y = x * y
  let div = (/)
  let pred n = create (n - 1)
  let is_zero n = equal n 0
end


module BitSize = struct
  include CommonSize

  let bits1 = create 1
  let bits8 = create 8
  let bits16 = create 16
  let bits32 = create 32
  let bits64 = create 64
  let bits128 = create 128
end


module ByteSize = struct
  include CommonSize

  let to_bitsize n = BitSize.create (Constants.bytesize * n)
  let of_bitsize n =
    assert(n mod Constants.bytesize = 0);
    BitSize.create (n / Constants.bytesize)

end

module MapSetMaker(C:Sigs.Comparable) = struct
  module Map = struct
    include Map.Make(C)

    let pop m =
      let (k, _) as elt = choose m in
      elt, remove k m

    let keys m =
      fold (fun k _ acc -> k :: acc) m [] |> List.rev

    let values m =
      fold (fun _ v acc -> v :: acc) m [] |> List.rev
  end

  module Set = struct
    include Set.Make(C)

    let pop set =
      let a = choose set in
      a, remove a set

  end
  include C
end


module Binstream = struct
  type t = int list
  (* The list is stored in reversed order *)

  let length = List.length

  let empty = []

  let of_list l = List.rev l

  let is_byte_value n = n >= 0 && n < 256

  let get_byte h n =
    List.nth (List.rev h) n

  let value_of c =
    let cc = Char.code c in
    if c <= '9' then cc - Char.code '0'
    else 10 + cc - Char.code 'a'

  let add_zeros hs =
    let len = String.length hs in
    if len mod 2 = 0 then hs
    else
      let b = Buffer.create 8 in
      let rec add_zero len =
        if len mod 2 = 0 then Buffer.contents b ^ hs
        else begin
          Buffer.add_char b '0';
          add_zero (len + 1)
        end
      in add_zero len

  let of_bytes s =
    Logger.debug ~level:5 "Binstream.of_bytes %s" s;
    let len = String.length s in
    let rec loop acc idx =
      if idx >= len then acc
      else
        let byte = s.[idx] in
        let ascii = Char.code byte in
        loop (ascii :: acc) (idx + 1)
    in loop [] 0

  let of_nibbles s =
    Logger.debug ~level:5 "Binstream.of_string %s" s;
    let s = String_utils.remove_char ' ' s in
    let s = add_zeros s in
    let len = String.length s in
    assert (len mod 2 = 0);
    let rec loop acc idx =
      if idx >= len then acc
      else
        let byte_str = String.sub s idx 2 in
        let v = 16 * value_of byte_str.[0] + value_of byte_str.[1] in
        loop (v :: acc) (idx + 2)
    in loop [] 0

  let append_int n h =
    assert (is_byte_value n);
    n :: h

  let append_int64 n64 h =
    let n = Int64.to_int n64 in append_int n h

  let prepend_int n h =
    assert (is_byte_value n);
    h @ [n]

  let prepend_int64 n64 h = prepend_int (Int64.to_int n64) h

  let pp ppf t =
    let rec loop ppf = function
      | [] -> Format.fprintf ppf ""
      | [n] -> Format.fprintf ppf "%02x" n
      | n :: l -> Format.fprintf ppf "%a %02x" loop l n
    in loop ppf t

  let to_string t = Format.asprintf "%a" pp t

end

module String = struct
  include MapSetMaker(String)

  module Hashtbl = Hashtbl.Make(
    struct
      include String
      let equal x y = compare x y = 0
      let hash = Hashtbl.hash
    end)
end

module Int64 = struct
  include MapSetMaker(Int64)

  module Hashtbl = Hashtbl.Make(
    struct
      include Int64
      let equal x y = compare x y = 0
      let hash = Hashtbl.hash
    end)

  let max n1 n2 = if Int64.compare n1 n2 <= 0 then n2 else n1

end
module Addr64 = Int64

module Int = MapSetMaker (struct type t = int let compare = compare end)

module OrderedBigInt =
struct
  type t = Bigint.t
  let compare = Bigint.compare_big_int
end

module BigInt = MapSetMaker(OrderedBigInt)

module Ternary = struct
  type t =
    | True
    | False
    | Unknown


  let of_bool = function
    | true -> True
    | false -> False

  let to_bool ?(unknown=false) = function
    | True -> true
    | False -> false
    | Unknown -> unknown

  let lognot = function
    | True -> False
    | False -> True
    | Unknown -> Unknown

  let logand t1 t2 =
    match t1, t2 with
    | True, True -> True
    | _, False
    | False, _ -> False
    | _, _ -> Unknown

  let logor t1 t2 =
    match t1, t2 with
    | False, False -> False
    | True, _
    | _, True -> True
    | _, _ -> Unknown
end
