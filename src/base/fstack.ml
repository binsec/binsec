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

module type Typed = sig
  type t
end

module Make (X : Typed) = struct

  type elem = X.t
  type t = elem list

  let empty = []


  let is_empty l = l = []

  let push e l = e :: l

  let singleton e = push e empty

  let top = function
    | e :: _ -> e
    | [] -> raise Not_found

  let pop = function
    | e :: es -> e, es
    | [] -> raise Not_found


  let iter f s = List.iter f s

  let fold f acc s = List.fold_left f acc s

  let length = List.length

end
