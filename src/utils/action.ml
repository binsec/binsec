(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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

type side =
  | Consequent
  | Alternative

module Count = struct
  type t =
    | Unlimited
    | Count of int

  let pp ppf = function
    | Count n -> Format.pp_print_int ppf n
    | Unlimited -> Format.pp_print_char ppf '*'

  let count n = assert (n >= 0); Count n

  let unlimited = Unlimited

  let decr = function
    | Count n -> assert (n > 0); count (n - 1)
    | Unlimited -> unlimited

  let is_zero = function
    | Count 0 -> true
    | Count _ | Unlimited -> false
end

type goal =
  | Reach of Count.t
  | Enumerate of Count.t * Dba.Expr.t
  | Cut
  | Restrict of Dba.Expr.t * Dba.Expr.t
  | Choice of side

type t = {
    address : Virtual_address.t;
    goal : goal;
  }

open Format

let pp_goal ppf = function
  | Reach c -> fprintf ppf "reach %a" Count.pp c
  | Enumerate (c, _e) ->
     fprintf ppf "enum %a" Count.pp c
  | Cut -> pp_print_string ppf "cut"
  | Restrict _ -> pp_print_string ppf "restrict"
  | Choice _ -> pp_print_string ppf "choice"
;;

let pp ppf t =
  fprintf ppf "0x%a %a"
  Virtual_address.pp t.address
  pp_goal t.goal
;;

let reach ?(n=1) address =
  assert (n >= 0);
  { address; goal = Reach (Count.count n); }

let reach_all address = { address; goal = Reach Count.unlimited; }

let enumerate ?(n=1) e address =
  assert (n >= 0);
  { address; goal = Enumerate (Count.count n, e); }

let enumerate_all e address =
  { address; goal = Enumerate (Count.unlimited, e)}

let cut address = { address; goal = Cut; }

let restrict e v address =
  { address; goal = Restrict(e, v); }

let goal g = g.goal

let address g = g.address

let choose ~side = Choice side

let choose_alternative address =
  { address; goal = choose ~side:Alternative; }

let choose_consequent address =
  { address; goal = choose ~side:Consequent; }

let check_and_decr g =
  match g.goal with
  | Choice _
  | Restrict _
  | Cut
  | Reach (Count.Unlimited | Count.Count 0)
  | Enumerate ((Count.Unlimited | Count.Count 0), _) -> None
  | Reach (Count.Count n as c) ->
     assert (n >= 1);
     Some { g with goal = Reach (Count.decr c); }
  | Enumerate (Count.Count n as c, e) ->
     assert (n >= 1);
     Some { g with goal = Enumerate (Count.decr c, e); }
