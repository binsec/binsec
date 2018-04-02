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

exception Elements_of_top

type t =
  | Top
  | Tainted
  | NotTainted
  | Bot

let universe = Top

let empty = Bot

let of_bounds _ = Top

let elements abs =
  match abs with
  | Bot -> []
  | Tainted | NotTainted | Top -> raise Elements_of_top

let pp ppf = function
  | Top -> Format.fprintf ppf "Top"
  | Tainted -> Format.fprintf ppf "Tainted"
  | NotTainted -> Format.fprintf ppf "Untainted"
  | Bot -> Format.fprintf ppf "Bot"

let to_string abs =
  Format.fprintf Format.str_formatter "%a" pp abs;
  Format.flush_str_formatter ()

let is_empty abs =
  match abs with
  | Bot -> true
  | Tainted | NotTainted | Top -> false

let singleton bv =
  match bv with
  | `Undef _ -> Tainted
  | _ -> NotTainted

let contains abs1 abs2 =
  match abs1, abs2 with
  | Top, _ -> true
  | _, Bot -> true
  | _, _ -> false

let equal abs1 abs2 = (contains abs1 abs2) && (contains abs2 abs1)

let join abs1 abs2 =
  match abs1,abs2 with
  | a, Bot -> a
  | Bot, a -> a
  | Tainted, Tainted -> Tainted
  | NotTainted, NotTainted -> NotTainted
  | _, _ -> Top


let widen abs1 abs2 _thresholds =
  match abs1,abs2 with
  | a, Bot -> a
  | Bot, a -> a
  | Tainted, Tainted -> Tainted
  | NotTainted, NotTainted -> NotTainted
  | _, _ -> Top


let meet abs1 abs2 =
  match abs1, abs2 with
  | Top, a -> a
  | a, Top -> a
  | Tainted, Tainted -> Tainted
  | NotTainted, NotTainted -> NotTainted
  | _, _ -> Bot

let neg abs = abs

let lognot abs = abs

let binop abs1 abs2 =
  match abs1, abs2 with
  | Tainted, _ -> Tainted
  | _, Tainted -> Tainted
  | Top, _ -> Top
  | _, Top -> Top
  | NotTainted, _ -> NotTainted
  | _, NotTainted -> NotTainted
  | Bot, Bot -> Bot

let concat abs1 abs2 = binop abs1 abs2

let add abs1 abs2 = binop abs1 abs2

let sub abs1 abs2 = binop abs1 abs2

let max _abs = failwith "taint.ml: max of taint"

let umul abs1 abs2 = binop abs1 abs2

let smul abs1 abs2 = binop abs1 abs2

let power abs1 abs2 = binop abs1 abs2

let udiv abs1 abs2 = binop abs1 abs2

let sdiv abs1 abs2 = binop abs1 abs2

let restrict abs _of1 _of2 = abs

let umod abs1 abs2 = binop abs1 abs2

let smod abs1 abs2 = binop abs1 abs2

let logor abs1 abs2 = binop abs1 abs2

let logand abs1 abs2 = binop abs1 abs2

let logxor abs1 abs2 = binop abs1 abs2

let lshift abs1 abs2 = binop abs1 abs2

let rshiftU abs1 abs2 = binop abs1 abs2

let rshiftS abs1 abs2 = binop abs1 abs2

let rotate_left abs1 abs2 = binop abs1 abs2

let rotate_right abs1 abs2 = binop abs1 abs2

let extension abs _ = abs

let signed_extension abs _ = abs

let eq abs1 abs2 = binop abs1 abs2

let diff abs1 abs2 = binop abs1 abs2

let leqU abs1 abs2 = binop abs1 abs2

let ltU abs1 abs2 = binop abs1 abs2

let geqU abs1 abs2 = binop abs1 abs2

let gtU abs1 abs2 = binop abs1 abs2

let leqS abs1 abs2 = binop abs1 abs2

let ltS abs1 abs2 = binop abs1 abs2

let geqS abs1 abs2 = binop abs1 abs2

let gtS abs1 abs2 = binop abs1 abs2

let guard _ abs1 abs2 = abs1, abs2


let is_true abs _ _ =
  match abs with
  | Bot -> Basic_types.Ternary.False
  | _ -> Basic_types.Ternary.Unknown


let to_smt _ _ = failwith "to_smt not yet implemented un Kset.ml"


let smt_refine _ _ = failwith "not yet implemented!"
