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

module Action = struct
  type format = Bin | Dec | Hex | Ascii

  type t =
    | Print_formula of (Dba.Expr.t * string) list option
    | Print_model
    | Print_value of format * Dba.Expr.t
    | Print_stream of string
end

module Choice = struct
  type side = Consequent | Alternative
  type t = { alternate : bool; (* Alternate side *) mutable side : side }

  let invert = function Consequent -> Alternative | Alternative -> Consequent
  let create ?(alternate = false) side = { alternate; side }
  let do_alternate t = if t.alternate then t.side <- invert t.side
  let is_alternative t = t.side = Alternative
  let is_consequent t = t.side = Consequent
end

module Count = struct
  type t = Unlimited | Count of int

  let pp ppf = function
    | Count n -> Format.pp_print_int ppf n
    | Unlimited -> Format.pp_print_char ppf '*'

  let count n =
    assert (n >= 0);
    Count n

  let unlimited = Unlimited
  let once = Count 1

  let decr = function
    | Count n ->
        assert (n > 0);
        count (n - 1)
    | Unlimited -> unlimited

  let is_zero = function Count 0 -> true | Count _ | Unlimited -> false
end

type d =
  | Reach of Count.t * Dba.Expr.t * Action.t list
  | Enumerate of int * Dba.Expr.t
  | Cut of Dba.Expr.t
  | Assume of Dba.Expr.t
  | Assert of Dba.Expr.t
  | Choice of Choice.t

type t = { loc : Dba.Expr.t; goal : d }

open Format

let pp_goal ppf = function
  | Reach (c, Dba.Expr.Cst _, _) -> fprintf ppf "reach %a" Count.pp c
  | Reach (c, e, _) ->
      fprintf ppf "reach %a such as %a = true" Count.pp c
        Dba_printer.Ascii.pp_bl_term e
  | Enumerate (c, _e) -> fprintf ppf "enum %d" c
  | Cut (Dba.Expr.Cst _) -> pp_print_string ppf "cut"
  | Cut e -> fprintf ppf "cut if %a = true" Dba_printer.Ascii.pp_bl_term e
  | Assume _ -> pp_print_string ppf "assume"
  | Assert _ -> pp_print_string ppf "assert"
  | Choice _ -> pp_print_string ppf "choice"

let pp ppf t =
  fprintf ppf "0x%a %a" Dba_printer.Ascii.pp_bl_term t.loc pp_goal t.goal

let reach ?(n = 1) ?(guard = Dba.Expr.one) ?(actions = [ Action.Print_model ])
    ~loc () =
  assert (n >= 0);
  assert (not (Dba.Expr.is_equal guard Dba.Expr.zero));
  { loc; goal = Reach (Count.count n, guard, actions) }

let reach_all ?(guard = Dba.Expr.one) ?(actions = [ Action.Print_model ]) ~loc
    () =
  { loc; goal = Reach (Count.unlimited, guard, actions) }

let enumerate ?(n = 1) e ~loc () =
  assert (n >= 0);
  { loc; goal = Enumerate (n, e) }

let enumerate_all e ~loc () = enumerate ~n:(1 lsl Dba.Expr.size_of e) e ~loc ()
let cut ?(guard = Dba.Expr.one) ~loc () = { loc; goal = Cut guard }
let assume e ~loc () = { loc; goal = Assume e }
let dynamic_assert e ~loc () = { loc; goal = Assert e }
let directive g = g.goal
let loc g = g.loc

let addr g =
  let img = Kernel_functions.get_img () in
  Dba_utils.Expr.eval_addr_from_img img g.loc

let choose ~alternate ~side = Choice (Choice.create ~alternate side)

let choose_alternative ?(alternate = true) ~loc () =
  { loc; goal = choose ~alternate ~side:Choice.Alternative }

let choose_consequent ?(alternate = true) ~loc () =
  { loc; goal = choose ~alternate ~side:Choice.Consequent }

let check_and_decr g =
  match g.goal with
  | Choice _ | Assume _ | Assert _ | Cut _
  | Reach ((Count.Unlimited | Count.Count 0), _, _)
  | Enumerate (0, _) ->
      None
  | Reach ((Count.Count n as c), cond, actions) ->
      assert (n >= 1);
      Some { g with goal = Reach (Count.decr c, cond, actions) }
  | Enumerate (n, e) ->
      assert (n >= 1);
      Some { g with goal = Enumerate (n - 1, e) }

let is_choice = function { goal = Choice _; _ } -> true | _ -> false
let reloc loc g = { g with loc }
