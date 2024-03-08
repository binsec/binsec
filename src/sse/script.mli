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

type env = {
  wordsize : int;
  endianness : Machine.endianness;
  define : Dba.Var.t -> Lexing.position -> unit;
  origin : string -> Lexing.position option;
  lookup : string -> Dba.LValue.t;
  lookup_symbol : string -> Dba.Var.Tag.attribute -> Dba.Expr.t;
}

type 'a loc = 'a Ast.loc

module Symbol : module type of Ast.Symbol
module Loc : module type of Ast.Loc
module Expr : module type of Ast.Expr
module Instr : module type of Ast.Instr

module Output : sig
  type format = Types.Output.format = Bin | Dec | Hex | Ascii

  type t =
    | Model
    | Formula
    | Slice of (Expr.t loc * string) list
    | Value of format * Expr.t loc
    | Stream of string
    | String of string

  val eval : env -> t -> Types.Output.t
  val pp : Format.formatter -> t -> unit
end

type Ast.Obj.t +=
  | Int of int
  | Int_list of int list
  | Format of Output.format
  | Output of Output.t
  | Output_list of Output.t list
  | String_list of string list
  | Key_val of (string * string)
  | Key_val_list of (string * string) list
  | Symbol_list of Symbol.t loc list
  | Loc_opt of Loc.t loc option
  | Loc_opt_list of Loc.t loc option list
  | Expr_opt of Expr.t loc option
  | Expr_list of Expr.t loc list
  | Named of (Expr.t loc * string)
  | Named_list of (Expr.t loc * string) list

type Ast.Instr.t +=
  | Argument of Loc.t loc * int  (** [lval] := arg([i]) *)
  | Return of Expr.t loc option  (** return [rval] *)
  | Cut of Expr.t loc option
  | Print of Output.t
  | Reach of int * Expr.t loc option * Output.t list
  | Enumerate of int * Expr.t loc

type Ast.t +=
  | Starting_from of Expr.t loc * Instr.t list
  | Starting_from_core of Instr.t list
  | Load_sections of string list
  | Load_data of Loc.t loc
  | Concretize_stack_pointer
  | Import_symbols of Symbol.t loc list * string
  | Hook of Expr.t loc list * Instr.t list * bool
  | Decode of Binstream.t * Instr.t list
  | Init of Instr.t list
  | Explore_all

val pp_options : Format.formatter -> (string * string) list -> unit
val pp_stmts : Format.formatter -> Ast.Instr.t list -> unit
val register_pp : (Format.formatter -> Ast.t -> bool) -> unit
val pp : Format.formatter -> Ast.t -> unit

val read_files :
  (unit, Libparser.obj, unit, unit, Libparser.obj Dyp.dyplexbuf) Dyp.dyp_action
  list
  list ->
  string list ->
  Ast.t list

exception Inference_failure of Expr.t loc
exception Invalid_size of Expr.t loc * int * int
exception Invalid_operation of Expr.t loc

val eval_loc : ?size:int -> Loc.t loc -> env -> Dba.LValue.t
val eval_expr : ?size:int -> Expr.t loc -> env -> Dba.Expr.t
