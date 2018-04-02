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

open Static_types
    
module SubEnv = Basic_types.BigInt.Map

type concreteMap = (Region_bitvector.t SubEnv.t) Env.t

let m_init: (Region_bitvector.t SubEnv.t) Env.t ref = ref Env.empty

let to_hexstring bigint =
  let open Bigint in
  assert (bigint >= zero_big_int);
  let sixteen = big_int_of_int 16 in
  let rec loop v str =
    if lt_big_int v sixteen then
      Format.sprintf "%s%x" str (int_of_big_int v)
    else
      let q, r = quomod_big_int v sixteen in
      loop r (Format.sprintf "%s%x" str (int_of_big_int q))
  in
  if eq_big_int bigint zero_big_int then "0"
  else loop bigint ""

let display ppf memory =
  let pp_elem addr rbv =
    Format.fprintf ppf
      "%s = %a;@ " (to_hexstring addr) Region_bitvector.pp rbv in
  let pp_submem name submem =
    Format.fprintf ppf "%s:@ @[<hov> " name;
    SubEnv.iter pp_elem submem;
    Format.fprintf ppf "@]@ "
  in
  let pp ext_var submem =
    match ext_var with
    | Var (vname, _size) -> pp_submem vname submem
    | Array region ->
      let reg_str =
        match region with
        | `Constant -> "Constant"
        | `Stack -> "Stack"
        | `Malloc ((id, _), _) -> Format.sprintf "Malloc(%d)" id
      in
      pp_submem reg_str submem;
      Format.fprintf ppf "@ "
  in
  Format.fprintf ppf "@[<v 0>";
  Env.iter pp memory;
  Format.fprintf ppf "@]"


