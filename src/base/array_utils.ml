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

let rec untili p t i =
  if i = Array.length t then raise Not_found
  else if p t.(i) then i
  else untili p t @@ (i + 1)

let findi p t = untili p t 0
let find p t = t.(findi p t)
let find_opt p t = try Some (find p t) with Not_found -> None

let fold_lefti f a t =
  let a = ref a in
  for i = 0 to Array.length t - 1 do
    a := f i !a t.(i)
  done;
  !a

let fold_righti f a t =
  let a = ref a in
  for i = Array.length t - 1 downto 0 do
    a := f i !a t.(i)
  done;
  !a
