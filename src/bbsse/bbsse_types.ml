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

module Status = struct
  type t = Unknown | Unreachable | Clear | Opaque of bool

  let pp ppf = function
    | Unknown -> Format.pp_print_string ppf "unknown"
    | Unreachable -> Format.pp_print_string ppf "unreachable"
    | Clear -> Format.pp_print_string ppf "clear"
    | Opaque true -> Format.pp_print_string ppf "opaque branch"
    | Opaque false -> Format.pp_print_string ppf "opaque fallthrough"
end

module Directive = struct
  type t =
    | ExpectAt of Virtual_address.t * Status.t
    | SkipJump of Virtual_address.t
    | ProcessCall of Virtual_address.t
end
