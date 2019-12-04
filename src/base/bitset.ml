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

include Bitvector

let create = zeros

let set = set_bit

let get = get_bit

let is_set = get

let flip = flip_bit

let remove = clear_bit

let union = logor

let inter = logand

let size = size_of

let equal = equal

let cardinal b =
  let rec loop c b =
    if is_zeros b then c
    else
      let b' = logand b (sub b (ones (size_of b))) in
      loop (c + 1) b'
  in loop 0 b

let resize b n =
  let m = size b in
  if n >= m then extend b n
  else reduce b n

let pp ppf b = Format.fprintf ppf "%s" (to_hexstring b)

let subset b1 b2 =
  let sz1 = size b1 and sz2 = size b2 in
  sz1 <= sz2
  &&
    let b1 = if sz1 = sz2 then b1 else extend b1 sz2 in
    equal (inter b1 b2) b1

let fold f acc b =
  let sz = size b in
  let rec loop i acc b =
    if i >= sz then acc
    else
      let acc' =
        if is_set b i then f acc i
        else acc
      in loop (i + 1) acc' b
  in loop 0 acc b

let map f b =
  let sz = size b in
  let rec loop i b =
    if i >= sz then b
    else
      let g = if f i then set else remove in
      loop (i + 1) (g b i)
  in loop 0 b
