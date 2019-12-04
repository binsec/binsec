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


module Choice = struct
  type side =
    | Consequent
    | Alternative

  type t = {
      alternate : bool; (* Alternate side *)
      mutable side : side;
    }

  let invert = function
    | Consequent -> Alternative
    | Alternative -> Consequent
  ;;

  let create ?(alternate=false) side =
    { alternate; side }

  let do_alternate t =
    if t.alternate then t.side <- invert t.side
  ;;


  let is_alternative t = t.side = Alternative
  let is_consequent t = t.side = Consequent

end

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

type d =
  | Reach of Count.t
  | Enumerate of Count.t * Dba.Expr.t
  | Cut
  | Assume of Dba.Expr.t
  | Choice of Choice.t

type t = {
    loc : Loader_utils.Binary_loc.t;
    goal : d;
  }

open Format

let pp_goal ppf = function
  | Reach c -> fprintf ppf "reach %a" Count.pp c
  | Enumerate (c, _e) ->
     fprintf ppf "enum %a" Count.pp c
  | Cut -> pp_print_string ppf "cut"
  | Assume  _ -> pp_print_string ppf "assume"
  | Choice _ -> pp_print_string ppf "choice"
;;

let pp ppf t =
  fprintf ppf "0x%a %a" Loader_utils.Binary_loc.pp t.loc pp_goal t.goal
;;

let reach ?(n=1) loc =
  assert (n >= 0);
  { loc; goal = Reach (Count.count n); }

let reach_all loc = { loc; goal = Reach Count.unlimited; }

let enumerate ?(n=1) e loc =
  assert (n >= 0);
  { loc; goal = Enumerate (Count.count n, e); }

let enumerate_all e loc =
  { loc; goal = Enumerate (Count.unlimited, e)}

let cut loc = { loc; goal = Cut; }

let assume e loc =
  { loc; goal = Assume e; }

let directive g = g.goal

let loc g = g.loc

let choose ~alternate ~side = Choice (Choice.create ~alternate side)

let choose_alternative ?(alternate=true) loc =
  { loc; goal = choose ~alternate ~side:Choice.Alternative; }

let choose_consequent  ?(alternate=true) loc =
  { loc; goal = choose ~alternate ~side:Choice.Consequent; }

let check_and_decr g =
  match g.goal with
  | Choice _
  | Assume _
  | Cut
  | Reach (Count.Unlimited | Count.Count 0)
  | Enumerate ((Count.Unlimited | Count.Count 0), _) -> None
  | Reach (Count.Count n as c) ->
     assert (n >= 1);
     Some { g with goal = Reach (Count.decr c); }
  | Enumerate (Count.Count n as c, e) ->
     assert (n >= 1);
     Some { g with goal = Enumerate (Count.decr c, e); }


let is_choice = function
  | { goal = Choice _; _} -> true
  | _ -> false
