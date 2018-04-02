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

open Common_piqi

let check_consistency_memory_t (addr:action) (value:action) (prefix:string) (default:action): bool =
  let pol_addr = match addr with | `default -> default | _ -> addr in
  let pol_value = match value with | `default -> default | _ -> value in
  match pol_addr, pol_value with
  | `symb,`conc ->
    Logger.warning
      "%s inconsistency, symbolizing addr and concretizing value meaningless"
      prefix;
    false
  | `symb,`symb ->
    Logger.warning
      "%s inconsistency, cannot symbolize content if address symbolic" prefix;
    false
  | _ -> true

let serialize_int64_list (params: int64 list): string =
  let b = Buffer.create 2048 in
  List.iter
    (fun i ->
       Buffer.add_string b
         (Decode_utils.int64_to_littleendian_bin i (Machine.Word_size.get ())))
    params;
  Buffer.contents b
