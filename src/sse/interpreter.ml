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

open Types

module type DRIVER = sig
  type path
  type outcome

  val step : path -> ([ `Step ], path) fiber -> outcome
  val assign : path -> ([ `Assign ], path) fiber -> outcome
  val clobber : path -> ([ `Clobber ], path) fiber -> outcome
  val load : path -> ([ `Load ], path) fiber -> outcome
  val store : path -> ([ `Store ], path) fiber -> outcome
  val symbolize : path -> ([ `Symbolize ], path) fiber -> outcome
  val apply : path -> ([ `Apply ], path) fiber -> outcome
  val assume : path -> ([ `Assume ], path) fiber -> outcome
  val check : path -> ([ `Assert ], path) fiber -> outcome
  val ite : path -> ([ `Branch ], path) fiber -> outcome
  val goto : path -> ([ `Goto ], path) fiber -> outcome
  val jump : path -> ([ `Jump ], path) fiber -> outcome
  val call : path -> ([ `Call ], path) fiber -> outcome
  val tail_call : path -> ([ `Tail_call ], path) fiber -> outcome
end

type ('path, 'outcome) driver =
  (module DRIVER with type path = 'path and type outcome = 'outcome)

let dispatch :
    type path outcome.
    path -> ([ `All ], path) fiber -> (path, outcome) driver -> outcome =
 fun path fiber driver ->
  let open (val driver) in
  match fiber with
  | Step _ as fiber -> step path fiber
  | Assign _ as fiber -> assign path fiber
  | Clobber _ as fiber -> clobber path fiber
  | Load _ as fiber -> load path fiber
  | Store _ as fiber -> store path fiber
  | Symbolize _ as fiber -> symbolize path fiber
  | Apply _ as fiber -> apply path fiber
  | Assume _ as fiber -> assume path fiber
  | Assert _ as fiber -> check path fiber
  | Branch _ as fiber -> ite path fiber
  | Goto _ as fiber -> goto path fiber
  | Jump _ as fiber -> jump path fiber
  | Call _ as fiber -> call path fiber
  | Tail_call _ as fiber -> tail_call path fiber

module Concrete
    (P : PATH)
    (D : DRIVER with type path = P.t and type outcome = P.t continuation) :
  DRIVER with type path = P.t and type outcome = P.t continuation = struct
  type path = P.t
  type outcome = path continuation

  let step : path -> ([ `Step ], path) fiber -> outcome =
   fun path (Step { addr; succ; _ }) ->
    P.set_pc path addr;
    (dispatch [@tailcall]) path succ (module D)

  let assign : path -> ([ `Assign ], path) fiber -> outcome =
   fun path (Assign { var; rval; succ }) ->
    P.assign path var rval;
    (dispatch [@tailcall]) path succ (module D)

  let clobber : path -> ([ `Clobber ], path) fiber -> outcome =
   fun path (Clobber { var; succ }) ->
    P.clobber path var;
    (dispatch [@tailcall]) path succ (module D)

  let load : path -> ([ `Load ], path) fiber -> outcome =
   fun path (Load { var; base; addr; dir; succ }) ->
    P.load path var base ~addr dir;
    (dispatch [@tailcall]) path succ (module D)

  let store : path -> ([ `Store ], path) fiber -> outcome =
   fun path (Store { base; addr; dir; rval; succ }) ->
    P.store path base ~addr rval dir;
    (dispatch [@tailcall]) path succ (module D)

  let symbolize : path -> ([ `Symbolize ], path) fiber -> outcome =
   fun path (Symbolize { var; succ }) ->
    P.symbolize path var;
    (dispatch [@tailcall]) path succ (module D)

  let apply : path -> ([ `Apply ], path) fiber -> outcome =
   fun path (Apply { f; succ }) ->
    f path;
    (dispatch [@tailcall]) path succ (module D)

  let assume : path -> ([ `Assume ], path) fiber -> outcome =
   fun path (Assume { test; succ }) ->
    if Bitvector.is_zero (P.eval path test) then Signal Unsatisfiable_assumption
    else (dispatch [@tailcall]) path succ (module D)

  let check : path -> ([ `Assert ], path) fiber -> outcome =
   fun path (Assert { test; succ }) ->
    if Bitvector.is_zero (P.eval path test) then Signal Assertion_failure
    else (dispatch [@tailcall]) path succ (module D)

  let ite : path -> ([ `Branch ], path) fiber -> outcome =
   fun path (Branch { test; taken; fallthrough }) ->
    (dispatch [@tailcall]) path
      (if Bitvector.is_zero (P.eval path test) then fallthrough else taken)
      (module D)

  let goto : path -> ([ `Goto ], path) fiber -> outcome =
   fun _ (Goto _ as fiber) -> Continue fiber

  let jump : path -> ([ `Jump ], path) fiber -> outcome =
   fun path (Jump target) ->
    Continue (Goto (Virtual_address.of_bitvector (P.eval path target)))

  let call : path -> ([ `Call ], path) fiber -> outcome =
   fun _ (Call { f; succ }) -> Call (f, Continue succ)

  let tail_call : path -> ([ `Tail_call ], path) fiber -> outcome =
   fun _ (Tail_call f) -> Tail_call f
end
