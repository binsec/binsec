(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

module V_comparable = struct
  type t = int
  let compare = Pervasives.compare
  let equal x y = compare x y = 0
  let hash x = x
end

let equal = V_comparable.equal
include Basic_types.Collection_make.Hashed(V_comparable)

let create n = n
let to_int n = n

let to_int64 = Int64.of_int

let of_int64 n64 =
  assert(Basic_types.Int64.is_int_int64 n64);
  Int64.to_int n64

let of_bigint b =
  assert (Bigint.is_int_big_int b);
  Bigint.int_of_big_int b

let of_string s = create @@ int_of_string s

let to_bigint = Bigint.big_int_of_int

let of_bitvector bv = Bitvector.value_of bv |> of_bigint

let add_int n t = create (t + n)

let succ = add_int 1
let pred t =
  assert (t > 0);
  add_int (-1) t

let pp ppf = Format.fprintf ppf "%08x"

let pp_set ppf vs =
  let open Format in
  pp_open_hovbox ppf 0;
  pp_print_string ppf "{";
  Set.iter (fun v -> fprintf ppf "%a;@ " pp v) vs;
  pp_print_string ppf "}";
  pp_close_box ppf ();
