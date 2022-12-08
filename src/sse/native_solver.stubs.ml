(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2022                                               *)
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

module Solver () : Solver_sig.S = struct
  type result = Sat | Unsat | Unknown

  type memory = unit

  type term = unit

  type value = unit

  type access = Select of term * int | Store of term

  let put _ _ = assert false

  let bind _ _ _ = assert false

  let get _ = assert false

  let set_memory ~addr:_ _ = assert false

  let neq _ _ = assert false

  let get_memory _ = assert false

  let get_at _ _ = assert false

  let get_value _ = assert false

  let assignment _ = assert false

  let succ _ = assert false

  let check_sat _ = assert false

  let close () = ()
end
