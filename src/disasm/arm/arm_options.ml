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

include Cli.Options (struct
  let name = "arm"
  let shortname = name
end)

type supported_modes = Both | Thumb | Arm

module SupportedModes = Builder.Variant_choice (struct
  type t = supported_modes

  let name = "supported-modes"
  let default = Arm

  let doc =
    "Can be used to only decode thumb instructions, arm instructions or both \
     (default: arm)."

  let to_string = function Both -> "both" | Thumb -> "thumb" | Arm -> "arm"

  let of_string = function
    | "both" -> Both
    | "thumb" -> Thumb
    | "arm" -> Arm
    | x ->
        raise
          (Invalid_argument
             (x
            ^ " is not a valid arm decoding mode. Expected one of both, thumb \
               or arm."))

  let choices = [ "both"; "thumb"; "arm" ]
end)
