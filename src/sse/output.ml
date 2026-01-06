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

type format = Bin | Dec | Hex | Ascii

type ('var, 'expr) t =
  | Model
  | Formula
  | Slice of ('expr * string) list
  | Value of format * 'expr
  | Stream of 'var
  | String of { array : string option; offset : 'expr; size : 'expr option }

let format_str = function
  | Bin -> "bin"
  | Dec -> "dec"
  | Hex -> "hexa"
  | Ascii -> "ascii"

let pp pp_var pp_expr ppf = function
  | Model -> Format.pp_print_string ppf "model"
  | Formula -> Format.pp_print_string ppf "formula"
  | Slice defs ->
      Format.fprintf ppf "formula for %a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           (fun ppf (expr, name) ->
             Format.fprintf ppf "%a as %s" pp_expr expr name))
        defs
  | Value (fmt, e) -> Format.fprintf ppf "%s %a" (format_str fmt) pp_expr e
  | Stream var -> Format.fprintf ppf "ascii stream %a" pp_var var
  | String { array; offset; size } ->
      Format.fprintf ppf "C string %s[%a, %a]"
        (Option.value ~default:"@" array)
        pp_expr offset
        (fun ppf -> function
          | None -> Format.pp_print_char ppf '*'
          | Some size -> pp_expr ppf size)
        size
