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

type t = int

let create n =
  assert (n >= 0);
  n

let add_int n i = create (n + i)
let add n1 n2 = add_int n1 n2
let sub_int n i = create (n - i)
let sub n1 n2 = sub_int n1 n2
let mul n1 n2 = n1 * n2
let div = ( / )
let eq = ( = )
let gt = ( > )
let ge = ( >= )
let is_zero = eq 0
let pred n = create (n - 1)
let to_int n = n
let pp ppf = Format.fprintf ppf "%d"
