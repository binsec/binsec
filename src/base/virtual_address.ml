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

exception Non_canonical_form

module V_comparable = struct
  type t = int

  let compare = compare
  let equal x y = compare x y = 0
  let hash x = x
end

include Basic_types.Collection_make.Hashed (V_comparable)

let zero = 0
let create n = n
let to_int n = n
let to_int64 x = Int64.(shift_right (shift_left (of_int x) 1) 1)

let of_int64 n64 =
  if Int64.(shift_right (shift_left n64 1) 1) <> n64 then
    raise Non_canonical_form
  else Int64.to_int n64

let of_bigint b =
  if Z.fits_int b then Z.to_int b
  else if Z.numbits b = 64 && Z.testbit b 62 then
    Z.to_int (Z.signed_extract b 0 63)
  else raise Non_canonical_form

let of_string s = of_bigint @@ Z.of_string s
let to_bigint v = Z.extract (Z.signed_extract (Z.of_int v) 0 63) 0 64
let of_bitvector bv = Bitvector.value_of bv |> of_bigint

(* FIXME: we may want to check for overflow? *)
let add_int n t = create (t + n)
let succ = add_int 1
let pred t = add_int (-1) t

(* FIXME: hope that t and t' are close enough *)
let diff t t' = t - t'

let pp ppf v =
  if v < 0 then
    Format.fprintf ppf "0x%x%014x"
      ((v asr 56) land 0xff)
      (v land 0xffffffffffffff)
  else Format.fprintf ppf "0x%08x" v

let to_string v = Format.asprintf "%a" pp v

let pp_set ppf vs =
  let open Format in
  pp_open_hovbox ppf 0;
  pp_print_string ppf "{";
  Set.iter (fun v -> fprintf ppf "%a;@ " pp v) vs;
  pp_print_string ppf "}";
  pp_close_box ppf ()
