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

open Format
type t =
  | Unknown
  | Unsupported of string option
  | Supported of string

let unknown = Unknown
let unsupported ?mnemonic_hint () = Unsupported mnemonic_hint
let supported v pp = Supported (asprintf "%a" pp v)

let pp ppf = function
  | Supported v -> fprintf ppf "%s" v
  | Unknown -> fprintf ppf "unknown"
  | Unsupported None -> fprintf ppf "unsupported"
  | Unsupported (Some descr) -> fprintf ppf "unsupported %s" descr

let to_string v = asprintf "%a" pp v
