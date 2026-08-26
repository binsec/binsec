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
module StMap = Basic_types.String.Map

let constant : Smtlib.Lang.constant -> Expr.t = function
  | CstNumeral _ | CstDecimal _ | CstString _ ->
      raise (Invalid_argument "From_smtlib(constant)")
  | CstDecimalSize (value, size) ->
      Expr.constant
        (Bitvector.create (Z.of_string_base 10 value) (int_of_string size))
  | CstHexadecimal value ->
      Expr.constant
        (Bitvector.create (Z.of_string_base 16 value) (4 * String.length value))
  | CstBinary value ->
      Expr.constant
        (Bitvector.create (Z.of_string_base 2 value) (String.length value))
  | CstBool true -> Expr.one
  | CstBool false -> Expr.zero

type env = Expr.t StMap.t * Memory.t StMap.t

let const_variable : env -> Smtlib.Lang.symbol -> Expr.t =
 fun (vars, _) { symbol_desc = SimpleSymbol name | QuotedSymbol name; _ } ->
  StMap.find name vars

let const_array : env -> Smtlib.Lang.symbol -> Memory.t =
 fun (_, arrays) { symbol_desc = SimpleSymbol name | QuotedSymbol name; _ } ->
  StMap.find name arrays

let rec expr : env -> Smtlib.Lang.term -> Expr.t =
 fun env { term_desc; _ } ->
  match term_desc with
  | TermSpecConstant cst -> constant cst
  | TermQualIdentifier
      {
        qual_identifier_desc =
          QualIdentifierIdentifier { id_desc = IdSymbol symbol; _ };
        _;
      } ->
      const_variable env symbol
  | TermQualIdentifierTerms
      ({ qual_identifier_desc = QualIdentifierIdentifier identifier; _ }, terms)
    ->
      apply env identifier terms
  | TermLetTerm (var_bindings, term) ->
      expr (List.fold_left (bind ~parent:env) env var_bindings) term
  | TermAnnotatedTerm (term, _) -> expr env term
  | TermQualIdentifier _ | TermQualIdentifierTerms _ | TermForallTerm _
  | TermExistsTerm _ | TermLambdaTerm _ ->
      raise (Invalid_argument "From_smtlib")

and bind : parent:env -> env -> Smtlib.Lang.var_binding -> env =
 fun ~parent (vars, arrays)
     {
       var_binding_desc =
         VarBinding
           ({ symbol_desc = SimpleSymbol name | QuotedSymbol name; _ }, term);
       _;
     } ->
  try (StMap.add name (expr parent term) vars, arrays)
  with Invalid_argument _ | Not_found ->
    (vars, StMap.add name (array parent term) arrays)

and apply : env -> Smtlib.Lang.identifier -> Smtlib.Lang.term list -> Expr.t =
 fun env symbol arguments ->
  match arguments with
  | [ term ] -> apply1 env symbol term
  | [ term1; term2 ] -> apply2 env symbol term1 term2
  | [ term1; term2; term3 ] -> apply3 env symbol term1 term2 term3
  | _ -> raise (Invalid_argument "From_smtlib(apply)")

and apply1 : env -> Smtlib.Lang.identifier -> Smtlib.Lang.term -> Expr.t =
 fun env { id_desc; _ } arg ->
  match id_desc with
  | IdSymbol { symbol_desc = SimpleSymbol ("not" | "bvnot"); _ } ->
      Expr.lognot (expr env arg)
  | IdSymbol { symbol_desc = SimpleSymbol "bvneg"; _ } ->
      Expr.uminus (expr env arg)
  | IdUnderscore
      ({ symbol_desc = SimpleSymbol "zero_extend"; _ }, [ IdxNum size ]) ->
      Expr.uext (int_of_string size) (expr env arg)
  | IdUnderscore
      ({ symbol_desc = SimpleSymbol "sign_extend"; _ }, [ IdxNum size ]) ->
      Expr.sext (int_of_string size) (expr env arg)
  | IdUnderscore
      ({ symbol_desc = SimpleSymbol "rotate_left"; _ }, [ IdxNum num ]) ->
      let x = expr env arg in
      Expr.rotate_left x
        (Expr.constant
           (Bitvector.create (Z.of_string_base 10 num) (Expr.sizeof x)))
  | IdUnderscore
      ({ symbol_desc = SimpleSymbol "rotate_right"; _ }, [ IdxNum num ]) ->
      let x = expr env arg in
      Expr.rotate_right x
        (Expr.constant
           (Bitvector.create (Z.of_string_base 10 num) (Expr.sizeof x)))
  | IdUnderscore ({ symbol_desc = SimpleSymbol "repeat"; _ }, [ IdxNum num ]) ->
      Expr.repeat (int_of_string num) (expr env arg)
  | IdUnderscore
      ({ symbol_desc = SimpleSymbol "extract"; _ }, [ IdxNum hi; IdxNum lo ]) ->
      Expr.restrict ~hi:(int_of_string hi) ~lo:(int_of_string lo) (expr env arg)
  | _ -> raise (Invalid_argument "From_smtlib(apply1)")

and apply2 :
    env ->
    Smtlib.Lang.identifier ->
    Smtlib.Lang.term ->
    Smtlib.Lang.term ->
    Expr.t =
 fun env { id_desc; _ } arg1 arg2 ->
  match id_desc with
  | IdSymbol { symbol_desc = SimpleSymbol ("and" | "bvand"); _ } ->
      Expr.logand (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol ("or" | "bvor"); _ } ->
      Expr.logor (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol ("xor" | "bvxor"); _ } ->
      Expr.logxor (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "=>"; _ } ->
      Expr.logor (Expr.lognot (expr env arg1)) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol ("=" | "bvcomp"); _ } ->
      Expr.equal (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "distinct"; _ } ->
      Expr.diff (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvnand"; _ } ->
      Expr.lognot (Expr.logand (expr env arg1) (expr env arg2))
  | IdSymbol { symbol_desc = SimpleSymbol "bvadd"; _ } ->
      Expr.add (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvsub"; _ } ->
      Expr.sub (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvmul"; _ } ->
      Expr.mul (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvudiv"; _ } ->
      Expr.udiv (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvsdiv"; _ } ->
      Expr.sdiv (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvurem"; _ } ->
      Expr.urem (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvsrem"; _ } ->
      Expr.srem (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvashr"; _ } ->
      Expr.shift_right_signed (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvlshr"; _ } ->
      Expr.shift_right (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol ("bvlshl" | "bvshl"); _ } ->
      Expr.shift_left (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "concat"; _ } ->
      Expr.append (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvuge"; _ } ->
      Expr.uge (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvugt"; _ } ->
      Expr.ugt (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvule"; _ } ->
      Expr.ule (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvult"; _ } ->
      Expr.ult (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvslt"; _ } ->
      Expr.slt (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvsle"; _ } ->
      Expr.sle (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvsgt"; _ } ->
      Expr.sgt (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "bvsge"; _ } ->
      Expr.sge (expr env arg1) (expr env arg2)
  | IdSymbol { symbol_desc = SimpleSymbol "select"; _ } ->
      Expr.load 1 LittleEndian (expr env arg2) (array env arg1)
  | _ -> raise (Invalid_argument "From_smtlib(apply2)")

and apply3 :
    env ->
    Smtlib.Lang.identifier ->
    Smtlib.Lang.term ->
    Smtlib.Lang.term ->
    Smtlib.Lang.term ->
    Expr.t =
 fun env { id_desc; _ } arg1 arg2 arg3 ->
  match id_desc with
  | IdSymbol { symbol_desc = SimpleSymbol "ite"; _ } ->
      Expr.ite (expr env arg1) (expr env arg2) (expr env arg3)
  | _ -> raise (Invalid_argument "From_smtlib(apply3)")

and array : env -> Smtlib.Lang.term -> Memory.t =
 fun env { term_desc; _ } ->
  match term_desc with
  | TermQualIdentifier
      {
        qual_identifier_desc =
          QualIdentifierIdentifier { id_desc = IdSymbol symbol; _ };
        _;
      } ->
      const_array env symbol
  | TermQualIdentifierTerms
      ( {
          qual_identifier_desc =
            QualIdentifierIdentifier
              {
                id_desc = IdSymbol { symbol_desc = SimpleSymbol "store"; _ };
                _;
              };
          _;
        },
        [ arg1; arg2; arg3 ] ) ->
      let index = expr env arg2 in
      Memory.layer index
        (Store.singleton
           (Bitvector.zeros (Expr.sizeof index))
           (Chunk.of_term (expr env arg3)))
        (array env arg1)
  | TermLetTerm (var_bindings, term) ->
      array (List.fold_left (bind ~parent:env) env var_bindings) term
  | TermAnnotatedTerm (term, _) -> array env term
  | TermSpecConstant _ | TermQualIdentifier _ | TermQualIdentifierTerms _
  | TermForallTerm _ | TermExistsTerm _ | TermLambdaTerm _ ->
      raise (Invalid_argument "From_smtlib(array)")
