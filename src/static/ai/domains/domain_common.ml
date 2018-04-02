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

let check_free  = function
  | `Value (`Malloc _ as r, bv) ->
    begin
      match Dba_types.Region.Map.find r !Simulate.mallocs with
      | Dba.Freeable when Bitvector.is_zero bv ->
        Simulate.mallocs := Dba_types.Region.Map.add r Dba.Freed !Simulate.mallocs
      | Dba.Freed -> raise Errors.Freed_variable_access
      | Dba.Freeable -> raise Errors.Invalid_free_address
      | exception Not_found -> failwith "Unbound free region"
    end
  | _ -> raise Errors.Invalid_free_region
