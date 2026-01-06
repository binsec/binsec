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

open Basic_types.Integers
include Collection.Hashed (Z)

let zero = Z.zero
let create = Z.of_int
let to_int = Z.to_int
let to_int32 = Z.to_int32_unsigned
let to_int64 = Z.to_int64_unsigned
let of_int32 = Z.of_int32_unsigned
let of_uint32 = Uint32.to_bigint
let of_int64 = Z.of_int64_unsigned
let of_uint64 = Uint64.to_bigint
let of_bigint = Fun.id
let of_string s = Z.of_string s
let to_bigint = Fun.id
let of_bitvector = Bitvector.value_of

(* FIXME: we may want to check for overflow? *)
let add = Z.add
let add_int n t = Z.add t (create n)
let add_bigint = Z.add
let succ = Z.succ
let pred = Z.pred

(* FIXME: hope that t and t' are close enough *)
let diff t t' = Z.to_int (Z.sub t t')
let modi t i = Z.to_int Z.(t mod create i)
let to_string v = Z.format "%#08x" v
let pp ppf v = Format.pp_print_string ppf (to_string v)
let pp16 ppf v = Format.pp_print_string ppf (Z.format "%04x" v)
let pp32 = pp
let pp64 ppf v = Format.pp_print_string ppf (Z.format "%016x" v)
let pp_print f = match f with `x16 -> pp16 | `x32 -> pp32 | `x64 -> pp64

let pp_set ppf vs =
  let open Format in
  pp_open_hovbox ppf 0;
  pp_print_string ppf "{";
  Set.iter (fun v -> fprintf ppf "%a;@ " pp v) vs;
  pp_print_string ppf "}";
  pp_close_box ppf ()
