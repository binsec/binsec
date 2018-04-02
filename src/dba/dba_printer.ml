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

open Format

module type Renderer = sig
  val binary_ops : (Dba.binary_op * string) list
  val unary_ops : (Dba.unary_op * string) list
  val endiannesses : (Dba.endianness * string) list
  val string_of_digit_char : char -> string
  val left_right_parentheses : string * string
end

module AsciiRenderer = struct
  let binary_ops = [
    Dba.Plus         , "+";
    Dba.Minus        , "-";
    Dba.MultU        , "*u";
    Dba.MultS        , "*s";
    Dba.DivU         , "/u";
    Dba.DivS         , "/s";
    Dba.ModU         , "%u";
    Dba.ModS         , "%s";
    Dba.Or           , "|";
    Dba.And          , "&";
    Dba.Xor          , "^";
    Dba.Concat       , "::";
    Dba.LShift       , "<<";
    Dba.RShiftU      , ">>u";
    Dba.RShiftS      , ">>s";
    Dba.LeftRotate   , "lrot";
    Dba.RightRotate  , "rrot";
    Dba.Eq           , "=";
    Dba.Diff         , "!=";
    Dba.LeqU         , "<=u";
    Dba.LtU          , "<u";
    Dba.GeqU         , ">=u";
    Dba.GtU          , ">u";
    Dba.LeqS         , "<=s";
    Dba.LtS          , "<s";
    Dba.GeqS         , ">=s";
    Dba.GtS          , ">s";
  ]

  let unary_ops = [
    Dba.UMinus, "-";
    Dba.Not, "!";
  ]

  let endiannesses = [
    Dba.BigEndian, "B";
    Dba.LittleEndian, "L";
  ]

  let string_of_digit_char c = Format.sprintf "%c" c

  let left_right_parentheses = "(", ")"
end

module UnicodeRenderer : Renderer = struct
  let binary_ops = [
    Dba.Plus       , "+";
    Dba.Minus      , "-";
    Dba.MultU      , "*𝒖";
    Dba.MultS      , "*𝒔";
    Dba.DivU       , "/";
    Dba.DivS       , "/𝒔";
    Dba.ModU       , "mod𝒖";
    Dba.ModS       , "mod𝒔";
    Dba.Or         , "||";
    Dba.And        , "&&";
    Dba.Xor        , "⨁";
    Dba.Concat     , "::";
    Dba.LShift     , "≪";
    Dba.RShiftU    ,  "≫𝒖";
    Dba.RShiftS    ,  "≫𝒔";
    Dba.LeftRotate , "lrot";
    Dba.RightRotate, "rrot";
    Dba.Eq         , "=";
    Dba.Diff       , "≠";
    Dba.LeqU       , "≤𝒖";
    Dba.LtU        , "<𝒖";
    Dba.GeqU       , "≥𝒖";
    Dba.GtU        , ">𝒖";
    Dba.LeqS       , "≤𝒔";
    Dba.LtS        , "<𝒔";
    Dba.GeqS       , "≥𝒔";
    Dba.GtS        , ">𝒔";
  ]

  let unary_ops = [
    Dba.UMinus, "-";
    Dba.Not, "¬";
  ]

  let endiannesses = [
    Dba.LittleEndian, "𝐿";
    Dba.BigEndian, "𝐵";
  ]

  let string_of_digit_char = function
    (* Unicode lowercase digits starts at 0x2080 *)
    | '0' -> "₀"
    | '1' -> "₁"
    | '2' -> "₂"
    | '3' -> "₃"
    | '4' -> "₄"
    | '5' -> "₅"
    | '6' -> "₆"
    | '7' -> "₇"
    | '8' -> "₈"
    | '9' -> "₉"
    | _ -> assert false

  let left_right_parentheses = "₍", "₎"
end

module EIC(R: Renderer) : Renderer = struct
  (* Endian Independent Code: Consider that the code has no endianness *)
  include R

  (* On purpose: this will not print any endianness information *)
  let endiannesses = []
end


module type DbaPrinter = sig
  val pp_code_address : Format.formatter -> Dba.address -> unit
  val pp_tag : Format.formatter -> Dba.tag -> unit
  val pp_binary_op : Format.formatter -> Dba.binary_op -> unit
  val pp_unary_op : Format.formatter -> Dba.unary_op -> unit
  val pp_cond: Format.formatter -> Dba.cond -> unit
  val pp_expr: Format.formatter -> Dba.expr -> unit
  val pp_instruction : Format.formatter -> Dba.instruction -> unit
  val pp_lhs :  Format.formatter -> Dba.lhs -> unit
  val pp_region : Format.formatter -> Dba.region -> unit
end

module Make(R:Renderer) : DbaPrinter = struct
  let mk_tbl assocs =
    let h = Hashtbl.create (List.length assocs) in
    List.iter (fun (key, value) -> Hashtbl.add h key value) assocs;
    h

  let binary_op_tbl = mk_tbl R.binary_ops
  and unary_op_tbl = mk_tbl R.unary_ops
  and endianness_tbl = mk_tbl R.endiannesses

  let find_or_default h key default =
    try Hashtbl.find h key with Not_found -> default

  let pp_binary_op fmt bop =
    fprintf fmt "%s" (find_or_default binary_op_tbl bop "?bop?")

  let pp_unary_op fmt uop =
    fprintf fmt "%s" (find_or_default unary_op_tbl uop "?uop?")

  let pp_endianness fmt endianness =
    fprintf fmt "%s" (find_or_default endianness_tbl endianness "")

  let pp_size fmt size =
    let is_digit c =
      try let ccode = Char.code c in ccode >= 48 && ccode <= 57
      with Invalid_argument _ -> false
    in
    let encode_char c = if is_digit c then R.string_of_digit_char c else "X" in
    let b = Buffer.create 16 in
    (* Wild guess: how many bytes do we need for the size ?*)
    String.iter
      (fun c -> Buffer.add_string b (encode_char c)) (string_of_int size);
    fprintf fmt "%s" (Buffer.contents b)


  let pp_code_address fmt addr =
    fprintf fmt "(%a, %d)" Bitvector.pp_hex addr.Dba.base addr.Dba.id

  let pp_opt pp_value fmt = function
    | None -> ()
    | Some value -> fprintf fmt "%a" pp_value value

  let pp_tag fmt = function
    | Dba.Call caddr ->
      fprintf fmt "#call with return address %@ %a" pp_code_address caddr
    | Dba.Return ->
      fprintf fmt "#return"

  let pp_alternative_tag fmt _ =
    (* FIXME: better printer *)
    fprintf fmt "alternative"

  let rec pp_expr fmt expr =
    let open Bigint in
    match expr with
    | Dba.ExprVar(name, size, _) ->
      let l, r = R.left_right_parentheses in
      fprintf fmt "%s%s%a%s" name l pp_size size r
    | Dba.ExprLoad(size, endian, expr) ->
      fprintf fmt "%@[%a]%a%a"
        pp_expr expr
        pp_endianness endian
        pp_size size
    | Dba.ExprCst(`Constant, bv) ->
      let size = Bitvector.size_of bv in
      let cval = Bitvector.value_of bv in
      let l, r = R.left_right_parentheses in
      fprintf fmt "%s%s%a%s"
        (string_of_big_int cval) l
        pp_size size r
    | Dba.ExprCst(`Stack, bv) ->
      let size = Bitvector.size_of bv in
      let cval = Bitvector.value_of bv in
      fprintf fmt "(stack %s<%a>)"
        (string_of_big_int cval)
        pp_size size
    | Dba.ExprCst(`Malloc ((id,_), _), bv) ->
      let size = Bitvector.size_of bv in
      let cval = Bitvector.value_of bv in
      fprintf fmt "(malloc %d, %s<%a>"
        id (string_of_big_int cval) pp_size size
    | Dba.ExprUnary(unary_op, expr) ->
      fprintf fmt "%a%a"
        pp_unary_op unary_op
        pp_expr expr
    | Dba.ExprBinary(binary_op, lexpr, rexpr) ->
      fprintf fmt "@[<hov 2>(%a@ %a@ %a)@]"
        pp_expr lexpr
        pp_binary_op binary_op
        pp_expr rexpr
    | Dba.ExprRestrict(expr, i, j) ->
      if i = j then fprintf fmt "%a{%d}" pp_expr expr i
      else fprintf fmt "%a{%d,%d}" pp_expr expr i j
    | Dba.ExprExtU(expr, n) ->
      fprintf fmt "@[(extu %a %d)@]"
        pp_expr expr n
    | Dba.ExprExtS(expr,n) ->
      fprintf fmt "@[(exts %a %d)@]" pp_expr expr n
    | Dba.ExprIte (cond, then_expr, else_expr) ->
      fprintf fmt "@[<hov>%a@ ? %a@ :@ %a@]"
        pp_cond cond
        pp_expr then_expr
        pp_expr else_expr
    | Dba.ExprAlternative (exprs, alttag )->
      fprintf fmt "@[%a %a@]"
        pp_alternative_tag alttag
        (Print_utils.pp_list ~pre:"(" ~post:")" ~sep:", " pp_expr)
        exprs

  and pp_cond fmt = function
    | Dba.CondReif expr ->
      pp_expr fmt expr
    | Dba.CondNot cond ->
      fprintf fmt "@[not@ %a@]" pp_cond cond
    | Dba.CondAnd(lcond, rcond) ->
      fprintf fmt "@[<hov 0>%a && %a@]" pp_cond lcond pp_cond rcond
    | Dba.CondOr(lcond, rcond) ->
      fprintf fmt "@[<hov 0>%a@ ||@ %a@]" pp_cond lcond pp_cond rcond
    | Dba.True -> fprintf fmt "true"
    | Dba.False -> fprintf fmt "false"

  let pp_lhs fmt = function
    | Dba.LhsVar(name, _size, _tag) ->
      fprintf fmt "%s" name
    | Dba.LhsVarRestrict(name, _size, i, j) ->
      fprintf fmt "%s{%d, %d}" name i j
    | Dba.LhsStore(size, endian, expr) ->
      fprintf fmt "%@[%a]%a%a"
        pp_expr expr
        pp_endianness endian
        pp_size size

  let pp_lhss fmt lhss =
    fprintf fmt "%a" (Print_utils.pp_list ~sep:", " pp_lhs) lhss

  let pp_address fmt = function
    | Dba.JInner id -> fprintf fmt "%d" id
    | Dba.JOuter caddr -> fprintf fmt "%a" pp_code_address caddr


  let pp_state fmt = function
    | Dba.OK -> fprintf fmt "OK"
    | Dba.KO -> fprintf fmt "KO"
    | Dba.Undefined s -> fprintf fmt "#undefined %s" s
    | Dba.Unsupported s -> fprintf fmt "#unsupported %s" s

  let pp_region fmt = function
    | `Constant -> fprintf fmt "cst"
    | `Stack -> fprintf fmt "stack"
    | `Malloc ((id, _), _) -> fprintf fmt "malloc%d" id

  let pp_instruction n fmt instruction =
    let suffix fmt id =
      match n with
      | None -> fprintf fmt ""
      | Some value ->
        if id = value + 1 then fprintf fmt ""
        else fprintf fmt "; goto %d" id
    in
    match instruction with
    | Dba.IkAssign(lhs, expr, id) ->
      fprintf fmt "%a := %a%a"
        pp_lhs lhs
        pp_expr expr
        suffix id
    | Dba.IkSJump(addr, tagopt) ->
      fprintf fmt "goto %a %a"
        pp_address addr
        (pp_opt pp_tag) tagopt
    | Dba.IkDJump(e_addr, tagopt) ->
      fprintf fmt "goto %a %a"
        pp_expr e_addr
        (pp_opt pp_tag) tagopt
    | Dba.IkIf(cond,  addr, int_addr) ->
      fprintf fmt "@[<hov 2>if %a@ @[<hv 0>goto %a@ else goto %d@]@]"
        pp_cond cond pp_address addr int_addr
    | Dba.IkStop state_opt ->
      fprintf fmt "%a" (pp_opt pp_state) state_opt
    | Dba.IkPrint (_, id) ->
      fprintf fmt
        "print \"message not displayed\"%a"
        suffix id
    | Dba.IkNondetAssume (lhslist ,cond, id) ->
      fprintf fmt "%@nondet_assume ({%a} %a)%a"
        pp_lhss lhslist
        pp_cond cond
        suffix id
    | Dba.IkAssume (cond, id) ->
      fprintf fmt "%@assume (%a)%a"
        pp_cond cond
        suffix id
    | Dba.IkAssert (cond, id) ->
      fprintf fmt "%@assert (%a)%a"
        pp_cond cond
        suffix id
    | Dba.IkMalloc (lhs, expr, id) ->
      fprintf fmt "%a := malloc(%a)%a"
        pp_lhs lhs
        pp_expr expr
        suffix id
    | Dba.IkFree (expr, id) ->
      fprintf fmt "free (%a)%a"
        pp_expr expr
        suffix id
    | Dba.IkUndef (lhs, id) ->
      fprintf fmt
        "%a := \\undef%a" pp_lhs lhs suffix id
    | Dba.IkNondet (lhs, region, id) ->
      fprintf fmt "%a := nondet(%a)%a"
        pp_lhs lhs
        pp_region region
        suffix id


  let pp_instruction fmt instruction = pp_instruction None fmt instruction
end

module Ascii = Make(AsciiRenderer)
module EICAscii = Make(EIC(AsciiRenderer))
module Unicode = Make(UnicodeRenderer)
module EICUnicode = Make(EIC(UnicodeRenderer))
