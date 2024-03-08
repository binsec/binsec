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

open Types

type builtin = ..

type fallthrough =
  | Nop
  | Debug of string
  | Print of Output.t
  | Instruction of Instruction.t
  | Hook of { addr : Virtual_address.t; info : string }
  | Assign of { var : Var.t; rval : Expr.t }
  | Clobber of Var.t
  | Forget of Var.t
  | Load of { var : Var.t; base : A.t; dir : Machine.endianness; addr : Expr.t }
  | Store of {
      base : A.t;
      dir : Machine.endianness;
      addr : Expr.t;
      rval : Expr.t;
    }
  | Symbolize of Var.t
  | Assume of Expr.t
  | Assert of Expr.t
  | Enumerate of { enum : Expr.t; tid : int; format : Output.format; n : int }
  | Reach of { tid : int; n : int; guard : Expr.t; actions : Output.t list }
  | Builtin of builtin

type terminator =
  | Jump of { target : Expr.t; tag : Dba.tag }
  | Halt
  | Cut
  | Die of string

type node =
  | Fallthrough of { kind : fallthrough; succ : int }
  | Branch of { test : Expr.t; target : int; fallthrough : int }
  | Goto of { target : Virtual_address.t; tag : Dba.tag; succ : int option }
  | Terminator of terminator

let pp_probe ppf probe =
  match probe with _ -> Format.pp_print_string ppf "builtin"

let pp_fallthrough, register_builtin_pp =
  let builtin_printers = ref [] in
  let rec pp_builtin ppf x printers =
    match printers with
    | [] -> Format.pp_print_string ppf "unknown builtin"
    | printer :: printers ->
        if not (printer ppf x) then pp_builtin ppf x printers
  in
  ( (fun ppf fallthrough ->
      match fallthrough with
      | Nop -> Format.pp_print_string ppf "nop"
      | Debug msg -> Format.fprintf ppf "debug(%s)" msg
      | Print output -> Format.fprintf ppf "print %a" Output.pp output
      | Instruction inst ->
          Format.fprintf ppf "%a %a" Virtual_address.pp
            (Instruction.address inst) Mnemonic.pp
            (Instruction.mnemonic inst)
      | Hook { addr; info } ->
          Format.fprintf ppf "%a %s" Virtual_address.pp addr info
      | Assign { var = { name; _ }; rval } ->
          Format.fprintf ppf "%s := %a" name Dba_printer.Ascii.pp_bl_term rval
      | Clobber { name; _ } -> Format.fprintf ppf "%s := undef" name
      | Forget { name; _ } -> Format.fprintf ppf "%s := undef" name
      | Load { var = { name; size; _ }; base; dir; addr } ->
          Format.fprintf ppf "%s := %s[%a%s, %d]" name
            (Option.fold ~none:"@" ~some:Fun.id base)
            Dba_printer.Ascii.pp_bl_term addr
            (match dir with LittleEndian -> "" | BigEndian -> ", ->")
            (size / 8)
      | Store { base; dir; addr; rval } ->
          Format.fprintf ppf "%s[%a%s, %d] := %a"
            (Option.fold ~none:"@" ~some:Fun.id base)
            Dba_printer.Ascii.pp_bl_term addr
            (match dir with LittleEndian -> "" | BigEndian -> ", ->")
            (Expr.size_of rval / 8)
            Dba_printer.Ascii.pp_bl_term rval
      | Symbolize { name; _ } -> Format.fprintf ppf "%s := nondet" name
      | Assume test ->
          Format.fprintf ppf "assume %a" Dba_printer.Ascii.pp_bl_term test
      | Assert test ->
          Format.fprintf ppf "assert %a" Dba_printer.Ascii.pp_bl_term test
      | Enumerate _ -> Format.pp_print_string ppf "enumerate"
      | Reach _ -> Format.pp_print_string ppf "reach"
      | Builtin x -> pp_builtin ppf x !builtin_printers),
    fun printer -> builtin_printers := printer :: !builtin_printers )

let pp_terminator ppf term =
  match term with
  | Jump { target; _ } ->
      Format.fprintf ppf "jump at %a" Dba_printer.Ascii.pp_bl_term target
  | Halt -> Format.pp_print_string ppf "halt"
  | Cut -> Format.pp_print_string ppf "cut"
  | Die msg -> Format.fprintf ppf "die(%s)" msg

let pp_node ppf node =
  match node with
  | Fallthrough { kind; _ } -> pp_fallthrough ppf kind
  | Branch { test; target; fallthrough } ->
      Format.fprintf ppf "if %a then goto %d else goto %d"
        Dba_printer.Ascii.pp_bl_term test target fallthrough
  | Goto { target; _ } ->
      Format.fprintf ppf "jump at %a" Virtual_address.pp target
  | Terminator kind -> pp_terminator ppf kind

let shuffle f node =
  match node with
  | Fallthrough { kind; succ } -> Fallthrough { kind; succ = f succ }
  | Branch { test; target; fallthrough } ->
      Branch { test; target = f target; fallthrough = f fallthrough }
  | Goto { target; tag; succ = Some succ } ->
      Goto { target; tag; succ = Some (f succ) }
  | Goto { succ = None; _ } | Terminator _ -> node

module type GRAPH = sig
  include Graph.Sig.G with type V.t = int and type E.t = int * bool * int

  val node : t -> vertex -> node
  val is_new_vertex : t -> vertex -> bool
  val iter_new_vertex : (vertex -> unit) -> t -> unit
  val iter_entries : (vertex -> unit) -> t -> unit
  val iter_exits : (vertex -> unit) -> t -> unit
  val insert_before : t -> vertex -> fallthrough -> int
  val insert_list_before : t -> vertex -> fallthrough list -> int
end
