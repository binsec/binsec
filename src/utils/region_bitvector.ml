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

open Dba
open Errors
open Smt_bitvectors
open Format
open Smtlib2

let r_res_compare r1 r2 =
  match r1, r2 with
  | `Stack, `Stack -> 0
  | `Malloc ((id1, _), _), `Malloc ((id2, _), _) -> compare id1 id2
  | `Stack, _ -> 1
  | `Malloc _, _ -> -1

type 'a symbol =
  | SymbVal of restricted_region * Bitvector.t
  | Restrict of 'a symbol * int * int
  | Affine of 'a * Bitvector.t

module rec SubSymb : sig
  type t = (int SymbMap.t) symbol
  val compare : t -> t -> int
end
= struct
  type t = (int SymbMap.t) symbol
  let compare symb1 symb2 =
    let rec compare_symbols s1 s2 =
      match s1, s2 with
      | SymbVal (r1, b1), SymbVal (r2, b2) ->
        let b1 = Bitvector.value_of b1 in
        let b2 = Bitvector.value_of b2 in
        let c = r_res_compare r1 r2 in
        if c = 0 then Bigint.compare_big_int b1 b2
        else c
      | Restrict (s1, o11, o12), Restrict (s2, o21, o22) ->
        let c = compare_symbols s1 s2 in
        let c1 = compare o11 o21 in
        let c2 = compare o12 o22 in
        if c = 0 then
          if c1 = 0 then c2
          else c1
        else c
      | Affine (a1, b1), Affine (a2, b2) ->
        let b1 = Bitvector.value_of b1 in
        let b2 = Bitvector.value_of b2 in
        let c = SymbMap.compare compare a1 a2 in
        if c = 0 then Bigint.compare_big_int b1 b2
        else c
      | SymbVal _, _ -> 1
      | _, SymbVal _ -> -1
      | Affine _, _ -> -1
      | _, Affine _ -> 1
    in
    compare_symbols symb1 symb2
end
and SymbMap : Map.S with type key = SubSymb.t = Map.Make (SubSymb)


type t =
  [
    Smt_bitvectors.basic_value
  | `Symb of (int SymbMap.t) symbol
  | `SymbSmt of smtBvExprAlt
  ]


(* FIXME: the following modules are badly named and placed: they should be moved
 * *)
module StringIntSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = string * int
  end )


module RegionSet = Set.Make (
  struct
    let compare (a1, _, _) (a2, _, _) = Pervasives.compare a1 a2
    type t = string * int * Dba.region
  end
  )

module LocalConstraintsSet = Set.Make (
  struct
    let compare = compare
    type t = (string * string)
  end
  )


type stats = {
  mutable call_to_simplify : int;
  mutable max_normalization_regions : int;
  mutable max_normalization_attempts : int;
  mutable max_expr_size: int;
  mutable max_undefs_in_expr: int;
  mutable nb_undef_normalization: int
}

let stats = {
  call_to_simplify = 0;
  max_normalization_regions = 0;
  max_normalization_attempts = 0;
  max_undefs_in_expr = 0;
  max_expr_size = 0;
  nb_undef_normalization = 0;
}

let update_max_stats n esize undefs =
  stats.max_normalization_regions <-  max stats.max_normalization_regions n;
  stats.max_expr_size <- max stats.max_expr_size esize;
  stats.max_undefs_in_expr <- max stats.max_undefs_in_expr undefs

let update_normalization_attempts n =
   stats.max_normalization_attempts <- max stats.max_normalization_attempts n

let incr_call_to_simplify () =
  stats.call_to_simplify <- stats.call_to_simplify + 1

let display_statistics fmt () =
  Format.fprintf fmt
    "%d %d %d %d %d %d d"
    stats.call_to_simplify
    stats.max_expr_size
    stats.max_normalization_attempts
    stats.max_normalization_regions
    stats.max_undefs_in_expr
    stats.nb_undef_normalization

let incr_undef () =
  stats.nb_undef_normalization <- stats.nb_undef_normalization + 1

let create_constant v sz = `Value (`Constant, Bitvector.create v sz)
let zeros sz = `Value (`Constant, Bitvector.zeros sz)

let undefined n = `Undef n

let string_of_bv bv =
  let value = Bitvector.value_of bv in
  let size = Bitvector.size_of bv in
  if size mod 4 = 0 then
    let s = Bitvector.to_hexstring (Bitvector.create value size) in
    let s = String.sub s 1 ((String.length s) - 1) in
    Format.asprintf "#%s" s
  else
    let i = Bigint.string_of_big_int value in
    Format.asprintf "((_ int2bv %d) %s)" size i

let rec get_vars_regions_from_BvExpr v b m expr =
  begin
    match expr with
    | SmtBvCstAlt _ -> StringIntSet.add ("cst", 32) v, b, m
    | SmtBvVarAlt(name, sz, value) ->
      begin
        match value with
        | `Value ((`Malloc((_id, _), size) as r), bv) ->
          let v = StringIntSet.add (name, sz) v in
          let big_size = (Bigint.int_of_big_int size) in
          let b = RegionSet.add (name, big_size, r) b in
          let s_bv = (string_of_bv bv) in
          let m = LocalConstraintsSet.add (name, s_bv) m in
          v, b, m
        | `Value (`Stack, bv)  ->
          let s_bv = (string_of_bv bv) in
          let m = LocalConstraintsSet.add (name, s_bv) m in
          v, b, m
        | _ -> v, b, m
      end
    | SmtBvUnaryAlt(_, e) -> get_vars_regions_from_BvExpr v b m e
    | SmtBvBinaryAlt(_op, e1, e2) ->
      let v1, b1, m1 = get_vars_regions_from_BvExpr v b m  e1 in
      let v2, b2, m2 = get_vars_regions_from_BvExpr v1 b1 m1 e2 in
      v2, b2, m2
    | SmtBvIteAlt (e1, e2, e3) ->
      let v1, b1, m1 = get_vars_regions_from_BvExpr v b m e1 in
      let v2, b2, m2 = get_vars_regions_from_BvExpr v1 b1 m1 e2 in
      let v3, b3, m3 = get_vars_regions_from_BvExpr v2 b2 m2 e3 in
      v3, b3, m3
    | SmtBvUndefAlt (i, size) ->
      StringIntSet.add ("undef_" ^ (string_of_int i), size) v, b, m
  end


let get_BvExpr_size expr =
  let rec aux expr nb snb unb =
    begin
      match expr with
      | SmtBvCstAlt _ -> (nb + 1), snb, unb
      | SmtBvVarAlt (_name, _sz, value) ->
        begin
          match value with
          | `Value (`Stack, _bv) -> (nb + 1), 1, unb
          | _ -> (nb + 1), snb, unb
        end
      | SmtBvUnaryAlt (_op, e) -> aux e (nb + 1) snb unb
      | SmtBvBinaryAlt (_op, e1, e2) ->
        let nb, snb, unb = aux e1 (nb + 1) snb unb in
        let nb, snb, unb = aux e2 nb snb unb in
        nb, snb, unb
      | SmtBvIteAlt (e1, e2, e3) ->
        let nb, snb, unb = aux e1 (nb + 1) snb unb in
        let nb, snb, unb = aux e2 nb snb unb in
        let nb, snb, unb = aux e3 nb snb unb in
        nb, snb, unb
      | SmtBvUndefAlt (_i, _size) -> (nb + 1), snb, (unb + 1)
    end
  in
  aux expr 0 0 0






(* create a smt constraints *)
let print_word_decl size =
  (Format.asprintf "(define-sort Word () (_ BitVec %d))\n") size


let print_var_decl v size =
  let f (y, sz) x =
    if sz = 32
    then Format.asprintf "%s(declare-const %s Word)\n" x y
    else Format.asprintf "%s(declare-const %s (_ BitVec %d))\n" x y sz
  in
  "(declare-const r Word)\n" ^
  "(declare-const offset Word)\n" ^
  "(declare-const stack Word)\n" ^
  (if StringIntSet.mem ("cst", 32) v
   then ""
   else "(declare-const cst Word)\n") ^
  (StringIntSet.fold f v "") ^
  (Format.asprintf "(declare-const c (_ BitVec %d))\n" size)


let print_max_min_decl =
  let args = "((b1 Word) (b2 Word))" in
  "(define-fun maxx " ^ args ^ " Word (ite (bvule b1 b2) b2 b1))\n" ^
  "(define-fun minn " ^ args ^ " Word (ite (bvule b1 b2) b1 b2))\n"


let print_region_size_function b =
  let acc1 = "(ite (= region cst) #xffffffff #x00000000)" in
  let acc2 =
    let c = "(= region stack)" in
    Format.asprintf "(ite %s (bvsub #xffffffff stack) %s)" c acc1
  in
  let f (r, size, _) x =
    let c = Format.asprintf "(= region %s)" r in
    Format.asprintf "(ite %s ((_ int2bv 32) %d) %s)" c size x
  in
  let sz = (RegionSet.fold f b acc2) in
  Format.asprintf "(define-fun size ((region Word)) Word %s)\n" sz


let print_non_overlap_assertion  =
  "(assert (bvult offset (size r)))\n" ^

  "(assert (forall ((b Word) (b1 Word) (o Word) (o1 Word)) \
        (=> (and (not (= b b1)) \
                         (not (= b cst)) \
                         (not (= b1 cst)) \
           (bvult o (size b)) \
           (bvult o1 (size b1))) \
            (distinct (bvadd b o) (bvadd b1 o1)))))\n" ^

  "(assert (forall ((b Word) (o Word)) \
    (=> (and (bvult o (size b)) (not (= b cst))) \
        (distinct (bvadd b o) #x00000000))))\n"


let print_distinct_blocks b =
  let f2 (name, _, r) acc =
    match r with
    | `Malloc ((-1, { base = bv; id = -1}), _) when Bitvector.size_of bv = 32 ->
      let s_bv = string_of_bv bv in
      "(assert (= " ^ name ^ " " ^ s_bv ^ "))\n" ^ acc
    | _ -> acc
  in
  let fr (r, _, _) x = x ^ r ^ " " in
  let regions = (RegionSet.fold fr b "cst stack ") in
  let f (r, _, _) acc = Format.asprintf "(maxx %s %s)" acc r in
  let sub_regions = (RegionSet.fold f b "#x00000000") in
  (Format.asprintf "(assert (distinct %s))\n" regions) ^
  (Format.asprintf "(assert (bvugt stack %s))\n" sub_regions) ^
  (RegionSet.fold f2 b "(assert (= cst #x00000000))\n")


let print_is_a_block_in_expr b =
  let f (r, _, _) x = if r.[0] <> 'g' then x ^ r ^ " " else x in
  let regions = (RegionSet.fold f b "") in
  Format.asprintf "(assert (not (distinct %s cst stack r)))\n" regions


let print_constraints expr size =
  if size = Machine.Word_size.get () then
    Format.asprintf "(assert (= %s (bvadd r offset)))\n"
      (smtBvExpr_to_string expr)
  else
    Format.asprintf "(assert (= %s c))\n" (smtBvExpr_to_string expr)


let print_assumes assumes =
  let f acc elem =
    acc ^ "(assert (= " ^ (smtBvExpr_to_string elem) ^ " #b1))\n" in
  List.fold_left f "" assumes


let print_unicity_constraint opt =
  match opt with
  | None -> ""
  | Some c -> c


let print_get_model b =
  let f (r,_, _) x = x ^ r ^ " " in
  let v = (RegionSet.fold f b "stack ") in
  let assertion = Format.asprintf "(get-value (r offset c %s))\n" v in
  "(check-sat)\n" ^ assertion


let print_local_constaints m =
  let f (r, bv) acc =
    acc ^ "(assert (bvult " ^ bv ^ " (size " ^ r ^ ")))\n" in
  LocalConstraintsSet.fold f m ""

let make_smt_program expr v b0 b1 m opt opt2 size assumes =
    print_word_decl (Machine.Word_size.get ()) ^
    print_var_decl v size ^
    print_max_min_decl ^
    print_region_size_function b1 ^
    print_non_overlap_assertion ^
    print_distinct_blocks b1 ^
    print_is_a_block_in_expr b0 ^
    print_local_constaints m ^
    print_assumes assumes ^
    print_constraints expr size ^
    opt2 ^
    print_unicity_constraint opt ^
    print_get_model b0

let get_model_from_result = function
  | [] ->  Logger.warning "Non-constant model not found!@."; []
  | model :: _ -> model

let getModelPtr expr v b0 b1 m size assumes opt =
  Logger.debug ~level:3 "Model ptr";
  let oc = open_out "smtfile.smt2" in
  let ppf = formatter_of_out_channel oc in
  let opt = "(assert (distinct r cst))\n" ^ opt in
  fprintf
    ppf "@[<v 0> %s @]@."
    (make_smt_program expr v b0 b1 m None opt size assumes);
  close_out oc;
  ignore(Sys.command "cat smtfile.smt2");
  ignore(Sys.command "z3 -smt2 smtfile.smt2 > smtout");
  ignore(Sys.command "cat smtout");
  let smtout = open_in "smtout" in
  let lexbuf = Lexing.from_channel smtout in
  let _, result = SMTParserWp.main SMTLexerWp.token lexbuf in
  let model = get_model_from_result result in
  close_in smtout;
  model


let getModelCst expr v b0 b1 m size assumes opt =
  Logger.debug "Model smt CST";
  let smtfile = open_out "smtfile.smt2" in
  let opt = "(assert (= r cst))\n" ^ opt in
  Printf.fprintf smtfile "%s"
    (make_smt_program expr v b0 b1 m None opt size assumes);
  close_out smtfile;
  ignore (Sys.command "z3 -smt2 smtfile.smt2 > smtout");
  ignore(Sys.command "cat smtout");
  let smtout = open_in "smtout" in
  let lexbuf = Lexing.from_channel smtout in
  let _, result = SMTParserWp.main SMTLexerWp.token lexbuf in
  let model = get_model_from_result result in
  close_in smtout;
  model


let getSat expr v b0 b1 m opt size _model assumes =
  let smtfile2 = open_out "smtfile2.smt2" in
  Printf.fprintf smtfile2 "%s"
    (make_smt_program expr v b0 b1 m opt " " size assumes);
  close_out smtfile2;
  ignore (Unix.system "z3 -smt2 smtfile2.smt2 > smtout2");
  let smtout2 = open_in "smtout2" in
  let sat = input_line smtout2 in
  close_in smtout2;
  sat = "unsat"


let get_block_value_from_ident id l =
  match List.assoc id l with
  | v -> v
  | exception Not_found ->
     let msg = Format.sprintf "Identifier %s not found in model" id in
     failwith msg

let get_region_ident_from_value l v =
  let f (i, j) =
    (v = j) && (i.[0] <> 'r') && (i.[0] <> 'o') && (i <> "c") in
  match List.filter f l with
  | (id, _) :: _ -> id
  | [] -> failwith ("unbound block with value " ^ v)


let make_unicity_constraint model _expr size =
  if size = Machine.Word_size.get () then
    let res_bval = get_block_value_from_ident "r" model in
    let block_id = get_region_ident_from_value model res_bval in
    let bv = get_block_value_from_ident "offset" model in
    let m = sprintf "(bvadd %s %s)" block_id bv in
    let s = sprintf "(assert (distinct (bvadd r offset) %s))\n" m in
    Some s
  else
    let bv = get_block_value_from_ident "c" model in
    Some (Format.asprintf "(assert (distinct c %s))\n" bv)


let make_unicity_constraint2 model _expr =
  let r = get_block_value_from_ident "r" model in
  let bv = get_block_value_from_ident "offset" model in
  let m = sprintf "(bvadd %s %s)" r bv in
  let s = sprintf "(assert (distinct (bvadd r offset) %s))\n" m in
  Some s


let big_int_of_smt_num_string s =
  let n = ("0"^(String.sub s 1 ((String.length s) -1))) in
  let n = Int64.to_string (Int64.of_string n) in
  Bigint.big_int_of_string n


let get_region_from_block_id id b =
  match id with
  | "cst" -> `Constant
  | "stack" -> `Stack
  | _ ->
    try let (_, _, r) = RegionSet.find (id, 0, `Stack) b in r
    with Not_found ->
      let s = sprintf "smtlib2.ml: no region with block_id: %s" id in
      failwith s


let getModel1 expr v b0 b1 c1 size assumes opt =
  let model = getModelPtr expr v b0 b1 c1 size assumes opt in
  if model = [] && size = Machine.Word_size.get ()
  then getModelCst expr v b0 b1 c1 size assumes opt
  else model


let getModel2 size model b0 =
  if size = Machine.Word_size.get () then
    let res_bval = get_block_value_from_ident "r" model in
    let block_id = get_region_ident_from_value model res_bval in
    let r = get_region_from_block_id block_id b0 in
    let bv = get_block_value_from_ident "offset" model in
    let bval = big_int_of_smt_num_string bv in
    (r, (bval, Machine.Word_size.get ())), block_id
  else
    let bv = get_block_value_from_ident "c" model in
    let bv = big_int_of_smt_num_string bv in
    (`Constant, (bv, size)), "cst"


let check_uniqueness size rbv model expr v b0 b1 c1 assumes =
  let unicity_constraint = make_unicity_constraint model expr size in
  if getSat expr v b0 b1 c1 unicity_constraint size model assumes
  then Some rbv
  else
    let unicity_constraint = make_unicity_constraint2 model expr in
    if (size = Machine.Word_size.get ()) &&
       getSat expr v b0 b1 c1 unicity_constraint size model assumes
    then Some rbv
    else None


let get_global_vars_regions global_regions =
  let is_successor a b =
    let succ_a = (Bigint.succ_big_int a) in
    Bigint.eq_big_int succ_a b
  in
  let malloc bv =
    `Malloc (
      (-1, Dba_types.Caddress.create (Bitvector.create bv 32) (-1)),
      Bigint.zero_big_int)
  in
  let f addr (v, b, bv_pred, id, sz, start_bv) =
    let bv = Dba_types.Caddress.base_value addr in
    match bv_pred with
    | None ->
      let name = "global0" in
      let v = StringIntSet.add (name, 32) v in
      let r = malloc bv in
      let b = RegionSet.add (name, 1, r) b in
      v, b, Some bv, 0, 1, bv
    | Some bv_pred ->
      if is_successor bv_pred bv
      then
        let r = malloc start_bv in
        let glb_id = "global" ^ (string_of_int id) in
        let b = RegionSet.add (glb_id, (sz + 1), r) b in
        v, b, Some bv, id, sz + 1, start_bv
      else
        let glb_id = "global" ^ (string_of_int (id + 1)) in
        let v = StringIntSet.add (glb_id, 32) v in
        let r = malloc bv in
        let b = RegionSet.add (glb_id, 1, r) b in
        v, b, Some bv, id + 1, 1, bv
  in
  let args = StringIntSet.empty, RegionSet.empty, None, 0, 0, Bigint.zero_big_int in
  let v, b, _, _, _, _ = Dba_types.Caddress.Set.fold f global_regions args in
  v, b


let simplify expr size assumes global_regions =
  incr_call_to_simplify ();
  let v = StringIntSet.empty in
  let b0 = RegionSet.empty in
  let c0 = LocalConstraintsSet.empty in
  let v, b0, c0 = get_vars_regions_from_BvExpr v b0 c0 expr in
  let f (v, b, m) expr = get_vars_regions_from_BvExpr v b m expr in
  let v', b0' = get_global_vars_regions global_regions in
  let v = StringIntSet.union v v' in
  let b0 = RegionSet.union b0 b0' in
  let v, b1, c1 = List.fold_left f (v, b0, c0) assumes in
  let expr_size, nb_stack, nb_undefs = get_BvExpr_size expr in
  let n = (RegionSet.cardinal b0) + nb_stack in
  update_max_stats n expr_size nb_undefs;
  let rec check_completeness opt nb =
    update_normalization_attempts nb;
    let model = getModel1 expr v b0 b1 c1 size assumes opt in
    if model = []
    then
      begin
        incr_undef ();
        if nb <> 1 then (
          let e = Smt_bitvectors.smtBvExprAlt_to_smtBvExpr expr in
          let e = Smtlib2print.smtbvexpr_to_string e in
          printf "normalize(%s) = undef@." e);
        None
      end
    else
      let rbv, r_name = getModel2 size model b0 in
      let m =
        check_uniqueness size rbv model expr v b0 b1 c1 assumes in
      match m with
      | None ->
        if r_name = "cst" then
          begin
            let e = Smt_bitvectors.smtBvExprAlt_to_smtBvExpr expr in
            let e = Smtlib2print.smtbvexpr_to_string e in
            printf "cstnormalize(%s) = undef@." e;
            incr_undef ();
            None
          end
        else
          let diff = (sprintf "(assert (distinct r %s))\n" r_name) in
          let opt = opt ^ diff in
          check_completeness opt (nb + 1)
      | Some _ -> m
  in check_completeness "" 1


let get_expr expr size assumes global_regions =
  let rbv = simplify expr size assumes global_regions in
  match rbv with
  | None -> raise (Smt_bitvectors.Assume_condition expr)
  | Some (r, bv) -> Dba.ExprCst (r, Bitvector.create_from_tuple bv)

let get_value expr size assumes global_regions =
  let rbv = simplify expr size assumes global_regions in
  match rbv with
  | None -> `Undef size
  | Some (r, bv) -> `Value (r, Bitvector.create_from_tuple bv)


let region_equal r1 r2 =
  match r1, r2 with
  | `Constant, `Constant -> true
  | `Stack, `Stack -> true
  | `Malloc ((id1, _), _), `Malloc ((id2, _), _) -> id1 = id2
  | _, _ -> false


(* FIXME: these functions should not be here *)
let twoPower n = Bigint.power_int_positive_int 2 n

let size_of param =
  match param with
  | `Value (_, bv) -> Bitvector.size_of bv
  | `Symb(SymbVal (_, bv)) -> Bitvector.size_of bv
  | `Symb(Restrict(_,i, j)) -> (j - i + 1)
  | `Symb(Affine (_smb, bv)) -> Bitvector.size_of bv
  | `SymbSmt smb ->
    let smb = smtBvExprAlt_to_smtBvExpr smb in
    Smtlib2.size_bvexpr smb
  | `Undef size -> size

(*******************************************************************)
let modulo b n = Bigint.mod_big_int b (twoPower n)

let _unsigned_to_signed_view param =
  match param with
  | `Value (r, bv) ->
    let size = Bitvector.size_of bv in
    let uvalue = Bitvector.value_of bv in
    let uvalue = modulo uvalue size in
    if Bigint.(lt_big_int uvalue (twoPower (size-1)))
    then `Value (r, Bitvector.create uvalue size)
    else
      let svalue = Bigint.sub_big_int uvalue (twoPower size) in
      `Value (r, Bitvector.create svalue size)
  | _ -> `Undef (size_of param)

let _signed_to_unsigned_view param =
  match param with
  | `Value (r, bv) ->
    let size = Bitvector.size_of bv in
    let svalue = Bitvector.value_of bv in
    let svalue = modulo svalue size in
    if Bigint.sign_big_int svalue = -1 then
      `Value (r, Bitvector.create (Bigint.add_big_int svalue (twoPower size)) size)
    else `Value (r, Bitvector.create svalue size)
  | _ -> `Undef (size_of param)


let is_zero param =
  match param with
  | `Value (`Constant, bv) when
      (Bigint.eq_big_int (Bitvector.value_of bv) Bigint.zero_big_int) -> true
  | _ -> false


let st_of bv = Bitvector.to_hexstring bv

let rec pp ppf = function
  | `Value (`Constant, bv) ->
    fprintf ppf "%s" (Bitvector.print bv)
  | `Value (`Stack, bv) ->
    fprintf ppf "[Stack %s]" (Bitvector.to_hexstring bv)
  | `Value (`Malloc((id, _), _), bv) ->
    fprintf ppf "Malloc(%d,%s)" id (Bitvector.to_hexstring bv)
  | `Symb smb -> fprintf ppf "Symb %a" pp_symb smb
  | `SymbSmt _smb -> fprintf ppf "SymbSmt "
  (*(Smt_bitvectors.smtBvExpr_to_hstring smb) *)
  | `Undef size -> fprintf ppf "Undef(%d)" size

and pp_symb ppf = function
  | SymbVal (`Stack as rr, bv)
  | SymbVal (`Malloc _ as rr, bv) ->
    fprintf ppf "%a" pp (`Value (rr, bv))
  | Restrict (s, o1, o2) ->
    fprintf ppf "(%a{%d.%d})" pp_symb s o1 o2
  | Affine (rs, bv) ->
    let pp_base ppf base =
      SymbMap.iter
        (fun symb coeff -> fprintf ppf "%+d.%a" coeff pp_symb symb)
        base
    in
    fprintf ppf "(%a+%s)" pp_base rs (Bitvector.to_hexstring bv)


let to_string rbv = Format.asprintf "%a" pp rbv


let region_of param : Dba.region =
  match param with
  | `Value (region, _) -> region
  | `Symb (Restrict (SymbVal (region,_), _, _)) -> (
      match region with
      | `Malloc p -> `Malloc p
      | `Stack -> `Stack )
  | `Symb _ ->  raise (Unknown_value "Symb")
  | `SymbSmt smb ->
    let symb = Smt_bitvectors.smtBvExpr_to_string smb in
    let message =
      Format.sprintf "SymbSmt (unknown region): %s" symb in
    raise (Unknown_value message)
  | `Undef _ -> raise (Unknown_value "region of undef")


let value_of param =
  match param with
  | `Value (_, bv) -> Bitvector.value_of bv
  | `Symb _ -> raise (Unknown_value (to_string param))
  | `SymbSmt _smb -> raise (Unknown_value (to_string param))
  | `Undef _size ->  raise (Unknown_value "value_of Undef")

let bitvector_of param =
  match param with
  | `Value (_, bv) -> bv
  | `Symb _
  | `SymbSmt _ -> raise (Unknown_value (to_string param))
  | `Undef _ -> raise (Unknown_value "bitvector_of Undef")

let encode_bitvector region bv =
  let addrsz = Machine.Word_size.get () in
  let size = Bitvector.size_of bv in
  let value = Bitvector.value_of bv in
  let region_name = Print_utils.string_from_pp Dba_printer.Ascii.pp_region region in
  if size > addrsz then
    let bv_resize = Bitvector.create value addrsz in
    let v = `Value (region, bv_resize) in
    let r = SmtBvVarAlt (region_name, addrsz, v) in
    let sz_ext = size - addrsz in
    let offset = SmtBvCstAlt bv_resize in
    let operand = SmtBvBinaryAlt (SmtBvAdd, r, offset) in
    SmtBvUnaryAlt ((SmtBvZeroExtend sz_ext), operand)
  else if size = addrsz then
    let v = `Value (region, bv) in
    let r = SmtBvVarAlt (region_name, size, v) in
    SmtBvBinaryAlt(SmtBvAdd, r, SmtBvCstAlt bv)
  else failwith "region_bitevtor.ml: stack with size < 32!"

let rec smt_symb_of op =
  match op with
  | `Value (`Constant, bv) -> SmtBvCstAlt bv
  | `Value (region, bv) -> encode_bitvector region bv
  | `Symb s -> symb_to_smt_symb s
  | `SymbSmt s -> s
  | `Undef size -> gen_undef size

and symb_to_smt_symb s =
  match s with
  | SymbVal ((`Stack | `Malloc _) as region, bv) -> encode_bitvector region bv
  | Restrict (smb, i, j) ->
    SmtBvUnaryAlt(SmtBvExtract(i, j), symb_to_smt_symb smb)

  | Affine (a, bv) ->
    let size = Bitvector.size_of bv in
    let bv = Bitvector.value_of bv in
    let s1 = affine_to_smt_symb a size in
    let s2 = SmtBvCstAlt (Bitvector.create bv size) in
    SmtBvBinaryAlt (SmtBvAdd, s1, s2)

and affine_to_smt_symb aff size =
  SymbMap.fold (fun s c smt_acc ->
      match s with
      | SymbVal _ ->
        let bv = Bigint.big_int_of_int c in
        let s1 = SmtBvCstAlt (Bitvector.create bv size) in
        let s2 = symb_to_smt_symb s in
        let smt_smb = SmtBvBinaryAlt (SmtBvMult, s1, s2) in
        SmtBvBinaryAlt (SmtBvAdd, smt_smb, smt_acc)

      | Restrict (smb, o1, o2) ->
        let bv = Bigint.big_int_of_int c in
        let smb = symb_to_smt_symb smb in
        let s1 = SmtBvCstAlt (Bitvector.create bv size) in
        let s2 = SmtBvUnaryAlt (SmtBvExtract(o1, o2), smb) in
        let smt_smb = SmtBvBinaryAlt(SmtBvMult, s1, s2) in
        SmtBvBinaryAlt (SmtBvAdd, smt_smb, smt_acc)

      | Affine (a', b) ->
        let sz = Bitvector.size_of b in
        let b = Bitvector.value_of b in
        let bv = Bigint.big_int_of_int c in
        let s1 = SmtBvCstAlt (Bitvector.create bv size) in
        let smb1 = affine_to_smt_symb a' sz in
        let s2 = SmtBvBinaryAlt (SmtBvAdd, smb1, SmtBvCstAlt(Bitvector.create b sz)) in
        let smt_smb = SmtBvBinaryAlt (SmtBvMult, s1, s2) in
        SmtBvBinaryAlt (SmtBvAdd, smt_smb, smt_acc)

    ) aff (SmtBvCstAlt(Bitvector.create (Bigint.zero_big_int) size))


let symb_of_binary_expr op p1 p2 =
  if Simulate_options.SemanticsMode.flat_or_not_basic () then
    begin
      match op with
      | SmtBvComp | SmtBvDiff | SmtBvUle | SmtBvUlt | SmtBvUge
      | SmtBvUgt | SmtBvSge | SmtBvSgt -> `Undef (1)
      | _ -> `Undef (size_of p1)
    end
    (* les opérateurs suivants renvoient un booléen pour z3,
       sont transformés en "ite ((p1 op p2), 1<1>, 0<1>)" *)
  else
    let smb = (SmtBvBinaryAlt(op, smt_symb_of p1, smt_symb_of p2))
    in `SymbSmt smb



let symb_of_unary_expr op p =
  if Simulate_options.SemanticsMode.flat_or_not_basic () then `Undef (size_of p)
  else `SymbSmt (SmtBvUnaryAlt(op, smt_symb_of p))


let normalize_symb s =
  let rec aux s =
    match s with
      SymbVal _ -> s
    | Restrict (Restrict (smb, i1, _j1), i2, j2) ->
      aux (Restrict(smb, i1 + i2, i1 + j2))
    | Restrict (smb, i, j) ->
      let smb = aux smb in
      let size = size_of (`Symb smb) in
      if i = 0 && j = size - 1 then smb
      else Restrict (smb, i, j)
    | Affine (a, bv) ->
      let (a, bv) = (aux2 a (SymbMap.empty, bv)) in
      if SymbMap.cardinal a <= 1 then
        try let (s, c) = SymbMap.choose a in
          match s with
            SymbVal (r, v) ->
            if c = 1
            then SymbVal (r, Bitvector.add bv v)
            else Affine (a, bv)
          | _ -> Affine (a, bv)
        with Not_found -> Affine (a, bv)
      else Affine (a, bv)

  and aux2 aff (acc_aff, acc_cst) =
    SymbMap.fold (fun s c (acc, acc_bv) ->
        match s with
          SymbVal (r, bv) ->
          let sz = Bitvector.size_of bv in
          let smb = (SymbVal (r, Bitvector.zeros sz)) in
          let c =
            try (SymbMap.find smb acc) + c with Not_found -> c
          in
          if c = 0 then (SymbMap.remove smb acc, acc_bv)
          else
            let c_bv = Bitvector.create (Bigint.big_int_of_int c) sz in
            let new_bv = Bitvector.umul bv c_bv in
            (SymbMap.add smb c acc, Bitvector.add new_bv acc_bv)
        | Restrict (_smb, _o1, _o2) ->
          let s = aux s in
          let c = try (SymbMap.find s acc) + c with Not_found -> c in
          if c = 0 then (SymbMap.remove s acc, acc_bv)
          else (SymbMap.add s c acc, acc_bv)
        | Affine (a', bv') ->
          let sz = Bitvector.size_of bv' in
          let new_a = SymbMap.map (fun c' -> c * c') a' in
          let c_bv = Bitvector.create (Bigint.big_int_of_int c) sz in
          let new_bv = Bitvector.umul bv' c_bv in
          aux2 new_a (acc, Bitvector.add acc_bv new_bv)
      ) aff (acc_aff, acc_cst)
  in
  if Simulate_options.SemanticsMode.flat_or_basic_and_full ()
  then `Undef (size_of (`Symb s))
  else if not (Simulate_options.SemanticsMode.basic ()) then `SymbSmt (symb_to_smt_symb s)
  else
    match s with
    | SymbVal (`Stack as rr, bv)
    | SymbVal (`Malloc _ as rr, bv)  -> `Value (rr, bv)
    | Restrict (_smb, _o1, _o2) ->
      let s = (aux s) in begin
        match s with
        | SymbVal (`Stack as rr, bv)
        | SymbVal (`Malloc _ as rr, bv) -> `Value (rr, bv)
        | _ -> `Symb s
      end
    | Affine (a, bv) ->
      if Simulate_options.SemanticsMode.basic_affine ()
      then
        let (a, bv) = (aux2 a (SymbMap.empty, bv)) in
        if SymbMap.cardinal a <= 1 then
          try let (s, c) = SymbMap.choose a in
            match s with
            | SymbVal ((`Stack as r), v)
            | SymbVal ((`Malloc _ as r), v) ->
              if c = 1
              then `Value (r, Bitvector.add bv v)
              else `Symb (Affine (a, bv))
            | _ -> `Symb (Affine (a, bv))
          with Not_found -> `Value (`Constant, bv)
        else `Symb (Affine (a, bv))
      else
        `Undef (size_of (`Symb s))




let rec equal param1 param2 =
  match param1, param2 with
    `Value (r1, b1), `Value (r2, b2) ->
    let size1 = Bitvector.size_of b1 in
    let b1 = Bitvector.value_of b1 in
    let size2 = Bitvector.size_of b2 in
    let b2 = Bitvector.value_of b2 in
    region_equal r1 r2 && Bigint.eq_big_int b1 b2 && size1 = size2
  | `Symb s1, `Symb s2 ->
    let s1, s2 = normalize_symb s1, normalize_symb s2 in begin
      match s1, s2 with
        `Symb s1', `Symb s2' -> SubSymb.compare s1' s2' = 0
      | _, _ -> equal s1 s2
    end
  | `Symb s, `Value v | `Value v, `Symb s ->
    let s' = normalize_symb s in begin
      match s' with
      | `Value v' -> equal (`Value v') (`Value v)
      | _ -> false
    end
  | _, _ -> false


let append param1 param2  =
  match param1, param2  with
  | `Value (`Constant, bv1), `Value(`Constant, bv2) ->
    `Value (`Constant, Bitvector.append bv1 bv2)
  | `Symb (Restrict (s1, i1, j1)), `Symb (Restrict (s2, i2, j2)) ->
    if equal (`Symb s1) (`Symb s2) && i1 = (j2 + 1)
    then
      normalize_symb (Restrict (s1, i2, j1))
    else
      let s = asprintf "%a, %a" pp param1 pp param2 in
      raise (Bad_concat s)
  | _, _ -> symb_of_binary_expr SmtBvConcat param1 param2

let non_deterministic region n =
  Random.self_init ();
  let open Bigint in
  let random_bit () = if Random.bool () then unit_big_int else zero_big_int in
  let rec bit_list acc = function
    | 0 -> acc
    | n -> bit_list (random_bit () :: acc) (n - 1)
  in
  let bvs = List.map (fun bit -> Bitvector.create bit 1) (bit_list [] n) in
  `Value(region, Bitvector.concat bvs)

let restrict param of1 of2 =
  if of2 < of1 then
    raise (Bad_bound ("restrict " ^ (string_of_int of1) ^ ", " ^ (string_of_int of2)))
  else
    match param with
    | `Value (r, b) ->
      let size = Bitvector.size_of b in
      let b = Bitvector.value_of b in
      let n = (of2 - of1 + 1) in
      (match r with
         `Constant ->
         `Value (`Constant, Bitvector.create (Bigint.extract_big_int b of1 n) n)
       | `Malloc p ->
         let elem = SymbVal (`Malloc p, Bitvector.create b size) in
         normalize_symb (Restrict (elem, of1, of2))
       | `Stack ->
         let elem = SymbVal (`Stack, Bitvector.create b size) in
         normalize_symb (Restrict (elem, of1, of2))
      )
    | `Symb s -> begin
        match s with
        | (Restrict (SymbVal (r, b), i, _j)) ->
          let size = Bitvector.size_of b in
          let b = Bitvector.value_of b in
          let o1, o2 = (i + of1, i + of2) in
          normalize_symb (Restrict (SymbVal (r, Bitvector.create b size), o1, o2))
        | _ -> normalize_symb  (Restrict (s, of1, of2))
      end
    | _ -> symb_of_unary_expr (SmtBvExtract (of1, of2)) param

let neg param =
  match param with
  | `Value (`Constant, bv) ->
    `Value (`Constant, Bitvector.neg bv)
  | `Value ((`Stack as r), bv)
  | `Value ((`Malloc _ as r), bv)->
    let sz = Bitvector.size_of bv in
    let key = (SymbVal (r, Bitvector.create (Bigint.zero_big_int) sz)) in
    let res = Bitvector.neg bv in
    normalize_symb (Affine (SymbMap.singleton key (-1), res))
  | `Symb s ->
    let zero = (Bigint.zero_big_int, size_of param) in
    normalize_symb (Affine ((SymbMap.singleton s (-1)),
                            Bitvector.create_from_tuple zero))

  | _ -> symb_of_unary_expr SmtBvNeg param

let lognot param  =
  match param with
    `Value (r, bv) ->
    if r = `Constant then `Value (r, Bitvector.lognot bv)
    else symb_of_unary_expr SmtBvNot param
  | _ -> symb_of_unary_expr SmtBvNot param

let add param1 param2 =
  match param1, param2 with
  | `Value v1, `Value v2 -> begin
      match v1, v2 with
      | (`Constant, bv1), (r, bv2) -> `Value (r, Bitvector.add bv1 bv2)
      | (r, bv1), (`Constant, bv2) -> `Value (r, Bitvector.add bv1 bv2)
      | ((`Stack as r1) ,bv1), ((`Stack as r2), bv2)
      | ((`Malloc _ as r1) ,bv1), ((`Malloc _ as r2), bv2)
      | ((`Stack as r1) ,bv1), ((`Malloc _ as r2), bv2 )
      | ((`Malloc _ as r1) ,bv1), ((`Stack as r2), bv2) ->
        let sz1 = Bitvector.size_of bv1 in
        let bv1 = Bitvector.value_of bv1 in
        let sz2 = Bitvector.size_of bv2 in
        let bv2 = Bitvector.value_of bv2 in
        let s1 = SymbVal (r1, Bitvector.create (Bigint.zero_big_int) sz1) in
        let s2 = SymbVal (r2, Bitvector.create (Bigint.zero_big_int) sz2) in
        let sum = Bitvector.add (Bitvector.create bv1 sz1) (Bitvector.create bv2 sz2) in
        if SubSymb.compare s1 s2 = 0 then
          let smap = (SymbMap.add s1 2 SymbMap.empty) in
          normalize_symb (Affine (smap, sum))
        else
          let smap = (SymbMap.add s1 1 SymbMap.empty) in
          let smap = (SymbMap.add s2 1 smap) in
          normalize_symb (Affine (smap, sum))
    end
  | `Symb s1, `Symb s2 ->
    let zero = (Bigint.zero_big_int, size_of (`Symb s1)) in
    if SubSymb.compare s1 s2 = 0 then
      let smap = (SymbMap.add s1 2 SymbMap.empty) in
      normalize_symb (Affine (smap, Bitvector.create_from_tuple zero))
    else
      let smap = (SymbMap.add s1 1 SymbMap.empty) in
      let smap = (SymbMap.add s2 1 smap) in
      normalize_symb (Affine (smap, Bitvector.create_from_tuple zero))
  | `Value v, `Symb s | `Symb s, `Value v -> begin
      match v with
        (`Constant, bv) ->
        normalize_symb (Affine (SymbMap.add s 1 SymbMap.empty, bv))
      | (`Stack as r, bv)
      | (`Malloc _ as r, bv) ->
        let sz = Bitvector.size_of bv in
        let s' = SymbVal (r, Bitvector.create (Bigint.zero_big_int) sz) in
        if SubSymb.compare s' s = 0 then
          let smap = (SymbMap.add s 2 SymbMap.empty) in
          normalize_symb (Affine(smap, bv))
        else
          let smap = (SymbMap.add s 1 SymbMap.empty) in
          let smap = (SymbMap.add s' 1 smap) in
          normalize_symb (Affine(smap, bv))
    end
  | _, _ -> symb_of_binary_expr SmtBvAdd param1 param2

let succ param  =
  match param with
  | `Value (r, bv) -> `Value (r, Bitvector.succ bv)
  | `Symb s ->
    let one = (Bigint.unit_big_int, size_of param) in
    normalize_symb (Affine (SymbMap.singleton s 1, Bitvector.create_from_tuple one))
  | _ ->
    let smbOne = SmtBvCstAlt (Bitvector.create (Bigint.unit_big_int) (size_of
                                                                        param)) in
    symb_of_binary_expr SmtBvAdd param (`SymbSmt smbOne)

let sub param1 param2  =
  match param1, param2 with
    `Value v1, `Value v2 ->
    begin
      match v1, v2 with
      | (`Constant, bv1), (`Constant, bv2) ->
        `Value (`Constant, Bitvector.sub bv1 bv2)
      | (r1, bv1), (`Constant, bv2) ->
        `Value (r1, Bitvector.sub bv1 bv2)
      | (`Constant, bv1),
        (`Stack as r, bv2)
      | (`Constant, bv1),
        (`Malloc _ as r, bv2) ->
        let sz1 = Bitvector.size_of bv1 in
        let key = (SymbVal (r, Bitvector.create (Bigint.zero_big_int) sz1)) in
        let delta = (Bitvector.sub bv1 bv2) in
        normalize_symb (Affine (SymbMap.singleton key (-1), delta))
      | ((`Stack as r1),b1),((`Stack as r2),b2)
      | ((`Malloc _ as r1),b1),((`Malloc _ as r2),b2)
      | ((`Stack as r1),b1),((`Malloc _ as r2),b2)
      | ((`Malloc _ as r1),b1),((`Stack as r2),b2) ->
        let sz1 = Bitvector.size_of b1 in
        let sz2 = Bitvector.size_of b2 in
        let s1 = SymbVal (r1, Bitvector.create (Bigint.zero_big_int) sz1) in
        let s2 = SymbVal (r2, Bitvector.create (Bigint.zero_big_int) sz2) in
        if SubSymb.compare s1 s2 = 0 then
          `Value (`Constant, Bitvector.sub b1 b2)
        else
          let smap = (SymbMap.add s1 1 SymbMap.empty) in
          let smap = (SymbMap.add s2 (-1) smap) in
          let delta = Bitvector.sub b1 b2 in
          normalize_symb (Affine (smap, delta))
    end
  | `Symb s1, `Symb s2 ->
    if SubSymb.compare s1 s2 = 0 then
      `Value (`Constant, Bitvector.create (Bigint.zero_big_int) (size_of (`Symb s1)))
    else
      let smap = (SymbMap.add s1 1 SymbMap.empty) in
      let smap = (SymbMap.add s2 (-1) smap) in
      let sz = size_of (`Symb s1) in
      normalize_symb (Affine(smap, Bitvector.create (Bigint.zero_big_int) sz))
  | `Value v, `Symb s ->
    begin
      match v with
        (`Constant, bv) ->
        normalize_symb (Affine (SymbMap.add s 1 SymbMap.empty, bv))
      | (`Stack as r, bv)
      | (`Malloc _ as r, bv) ->
        let sz = Bitvector.size_of bv  in
        let s' = SymbVal (r, Bitvector.create (Bigint.zero_big_int) sz) in
        if SubSymb.compare s' s = 0 then
          let smap = (SymbMap.add s 2 SymbMap.empty) in
          normalize_symb (Affine(smap, bv))
        else
          let smap = (SymbMap.add s (-1) SymbMap.empty) in
          let smap = (SymbMap.add s' 1 smap) in
          normalize_symb (Affine(smap, bv))
    end
  | `Symb s, `Value v ->
    begin
      match v with
        (`Constant, bv) ->
        normalize_symb (Affine (SymbMap.add s 1 SymbMap.empty, bv))
      | (`Stack as r, bv)
      | (`Malloc _ as r, bv) ->
        let sz = Bitvector.size_of bv in
        let s' = SymbVal (r, Bitvector.create (Bigint.zero_big_int) sz) in
        if SubSymb.compare s' s = 0 then
          let smap = (SymbMap.add s 2 SymbMap.empty) in
          normalize_symb (Affine(smap, bv))
        else
          let smap = (SymbMap.add s 1 SymbMap.empty) in
          let smap = (SymbMap.add s' (-1) smap) in
          normalize_symb (Affine(smap, Bitvector.neg bv))
    end
  | _, _ -> symb_of_binary_expr SmtBvSub param1 param2


let umul param1 param2  =
  match param1, param2 with
    `Value (`Constant, bv1), `Value (`Constant, bv2) ->
    `Value (`Constant, Bitvector.umul bv1 bv2)
  | `Value (`Constant, bv1), `Value (`Stack as r, bv2)
  | `Value (`Constant, bv1), `Value ((`Malloc _) as r, bv2) ->
    (* TODO: Change coef's type in affine forms to Big_int *)
    let coef = Bigint.int_of_big_int
        (Bitvector.value_of bv1) in
    let smap = (SymbMap.add (SymbVal(r, bv2)) coef SymbMap.empty) in
    let sz = (Bitvector.size_of bv1) in
    normalize_symb (Affine(smap, Bitvector.zeros sz))
  | `Value (`Stack as r, bv2), `Value (`Constant, bv1)
  | `Value ((`Malloc _) as r, bv2), `Value (`Constant, bv1) ->
    (* TODO: Change coef's type in affine forms to Big_int *)
    let coef = Bigint.int_of_big_int
        (Bitvector.value_of bv1) in
    let smap = (SymbMap.add (SymbVal(r, bv2)) coef SymbMap.empty) in
    let sz = (Bitvector.size_of bv1) in
    normalize_symb (Affine(smap, Bitvector.zeros sz))
  | `Value (`Constant, bv), `Symb (_ as s)
  | `Symb (_ as s), `Value (`Constant, bv) ->
    (* TODO: Change coef's type in affine forms to Big_int *)
    let coef = Bigint.int_of_big_int
        (Bitvector.value_of bv) in
    let smap = (SymbMap.add s coef SymbMap.empty) in
    let sz = (Bitvector.size_of bv) in
    normalize_symb (Affine(smap, Bitvector.zeros sz))
  | _, _ -> symb_of_binary_expr SmtBvMult param1 param2

let smul param1 param2  =
  match param1, param2 with
    `Value (`Constant, bv1), `Value (`Constant, bv2) ->
    `Value (`Constant, Bitvector.smul bv1 bv2)
  | `Value (`Constant, bv1), `Value (`Stack as r, bv2)
  | `Value (`Constant, bv1), `Value ((`Malloc _) as r, bv2) ->
    (* TODO: Change coef's type in affine forms to Big_int *)
    let coef = Bigint.int_of_big_int
        (Bitvector.value_of bv1) in
    let smap = (SymbMap.add (SymbVal(r, bv2)) coef SymbMap.empty) in
    let sz = (Bitvector.size_of bv1) in
    normalize_symb (Affine(smap, Bitvector.zeros sz))
  | `Value (`Stack as r, bv2), `Value (`Constant, bv1)
  | `Value ((`Malloc _) as r, bv2), `Value (`Constant, bv1) ->
    (* TODO: Change coef's type in affine forms to Big_int *)
    let coef = Bigint.int_of_big_int
        (Bitvector.value_of bv1) in
    let smap = (SymbMap.add (SymbVal(r, bv2)) coef SymbMap.empty) in
    let sz = (Bitvector.size_of bv1) in
    normalize_symb (Affine(smap, Bitvector.zeros sz))
  | `Value (`Constant, bv), `Symb (_ as s)
  | `Symb (_ as s), `Value (`Constant, bv) ->
    (* TODO: Change coef's type in affine forms to Big_int *)
    let coef = Bigint.int_of_big_int
        (Bitvector.value_of bv) in
    let smap = (SymbMap.add s coef SymbMap.empty) in
    let sz = (Bitvector.size_of bv) in
    normalize_symb (Affine (smap, Bitvector.zeros sz))
  | _, _ -> symb_of_binary_expr SmtBvMult param1 param2

let conflicted_region msg r1 r2 =
  let open Dba_types in
  let msg = Format.asprintf "%s: %a - %a" msg Region.pp r1 Region.pp r2 in
  raise (Regions_conflict msg)

let pow param1 param2 =
  match param1, param2 with
  | `Value(`Constant, bv1), `Value(`Constant, bv2) ->
    `Value(`Constant, Bitvector.pow bv1 bv2)
  | `Undef size, _ | _, `Undef size -> `Undef size
  | `Value (r1, _), `Value (r2, _) ->
    conflicted_region "pow" r1 r2 
  | p1, p2 ->
    raise (Unknown_value (to_string p1 ^ "or" ^ to_string p2))

let udiv param1 param2 =
  match param1, param2 with
    `Value (`Constant, bv1), `Value (`Constant, bv2) ->
    `Value(`Constant, Bitvector.udiv bv1 bv2)
  | _, `Value (`Constant, bv) ->
    let bv = Bitvector.value_of bv in
    if (Bigint.compare_big_int bv Bigint.zero_big_int) = 0
    then raise Division_by_zero
    else symb_of_binary_expr SmtBvUdiv param1 param2
  | _, _ -> symb_of_binary_expr SmtBvUdiv param1 param2


let sdiv param1 param2 =
  match param1, param2 with
    `Value (`Constant, bv1), `Value (`Constant, bv2) ->
    `Value(`Constant, Bitvector.sdiv bv1 bv2)
  | _, `Value (`Constant, bv) ->
    let bv = Bitvector.value_of bv in
    if (Bigint.compare_big_int bv Bigint.zero_big_int) = 0
    then raise Division_by_zero
    else symb_of_binary_expr SmtBvSdiv param1 param2
  | _, _ -> symb_of_binary_expr SmtBvSdiv param1 param2

let umod param1 param2 =
  match param1, param2 with
    `Value(`Constant, bv1), `Value(`Constant, bv2) ->
    `Value(`Constant, Bitvector.umod bv1 bv2)
  | `Undef size, _ | _, `Undef size -> `Undef size
  | _, _ -> symb_of_binary_expr SmtBvUrem param1 param2

let smod param1 param2 =
  match param1, param2 with
    `Value(`Constant, bv1), `Value(`Constant, bv2) ->
    `Value(`Constant, Bitvector.smod bv1 bv2)
  | _, _ -> symb_of_binary_expr SmtBvSmod param1 param2

let urem = umod
let srem = smod


let logor param1 param2 =
  match param1, param2 with
    `Value (`Constant, bv1), `Value (`Constant, bv2) ->
    `Value (`Constant, Bitvector.logor bv1 bv2)
  | `Value (`Constant, bv1), (`Value (`Stack, _) as term)
  | (`Value (`Stack, _) as term), `Value (`Constant, bv1)
  | `Value (`Constant, bv1), (`Value (`Malloc _, _) as term)
  | (`Value (`Malloc _, _) as term), `Value (`Constant, bv1)
  | `Value (`Constant, bv1), (`Symb _ as term)
  | (`Symb _ as term), `Value (`Constant, bv1) ->
    let sz = Bitvector.size_of bv1 in
    if Bitvector.is_zero bv1
    then term
    (* Signed or unsigned here ? *)
    else if Bitvector.is_max_ubv bv1
    then `Value (`Constant, (Bitvector.max_ubv sz))
    else symb_of_binary_expr SmtBvOr param1 param2
  | _, _ -> symb_of_binary_expr SmtBvOr param1 param2

let logand param1 param2 =
  match param1, param2 with
  | `Value (`Constant, bv1), `Value (`Constant, bv2) ->
    `Value (`Constant, Bitvector.logand bv1 bv2)
  | `Value (`Constant, bv1), (`Value (`Stack, _) as term)
  | (`Value (`Stack, _) as term), `Value (`Constant, bv1)
  | `Value (`Constant, bv1), (`Value (`Malloc _, _) as term)
  | (`Value (`Malloc _, _) as term), `Value (`Constant, bv1)
  | `Value (`Constant, bv1), (`Symb _ as term)
  | (`Symb _ as term), `Value (`Constant, bv1) ->
    let sz = Bitvector.size_of bv1 in
    if Bitvector.is_zero bv1
    then `Value (`Constant, Bitvector.create (Bigint.zero_big_int) sz)
    else if Bitvector.is_max_ubv bv1 then term
    else symb_of_binary_expr SmtBvAnd param1 param2

  | `Value (r1, b1), `Value (r2, b2) ->
    let sz = Bitvector.size_of b1 in
    let b1 = Bitvector.value_of b1 in
    let b2 = Bitvector.value_of b2 in
    if (region_equal r1 r2) && Bigint.eq_big_int b1 b2
    then `Value (r1, Bitvector.create b1 sz)
    else symb_of_binary_expr SmtBvAnd param1 param2
  | _, _ -> symb_of_binary_expr SmtBvAnd param1 param2


let logxor param1 param2 =
  match param1, param2 with
    `Value(`Constant, bv1), `Value(`Constant, bv2) ->
    `Value(`Constant, Bitvector.logxor bv1 bv2)
  | `Value _rbv1, `Value _rbv2 ->
    if equal param1 param2 then
      `Value (`Constant, Bitvector.create (Bigint.zero_big_int) (size_of param1))
    else `Undef (size_of param1)
  | `Symb s1, `Symb s2 ->
    let s1, s2 = normalize_symb s1, normalize_symb s2 in begin
      if equal s1 s2 then
        `Value (`Constant, Bitvector.create (Bigint.zero_big_int) (size_of param1))
      else `Undef (size_of param1)
    end
  | `Symb s, `Value _ ->
    let s = normalize_symb s in begin
      if equal s param2 then
        `Value (`Constant, Bitvector.create (Bigint.zero_big_int) (size_of param1))
      else `Undef (size_of param1)
    end
  | `Value _, `Symb s ->
    let s = normalize_symb s in begin
      if equal s param1 then
        `Value (`Constant, Bitvector.create (Bigint.zero_big_int) (size_of param1))
      else `Undef (size_of param1)
    end
  | _, _ -> symb_of_binary_expr SmtBvXor param1 param2

let unsafe_bitvector_to_int bv =
  (* Should check the conversion *)
  Bigint.int_of_big_int (Bitvector.value_of bv)

let lshift param1 param2 =
  match param1, param2 with
  | `Value(`Constant, bv1), `Value(`Constant, bv2) ->
    `Value(`Constant, Bitvector.shift_left bv1 (unsafe_bitvector_to_int bv2))
  | `Undef size, _ | _, `Undef size -> `Undef size
  | _, _ -> symb_of_binary_expr SmtBvShl param1 param2


let rshiftU param1 param2 =
  match param1, param2 with
    `Value(`Constant, bv1), `Value(`Constant, bv2) ->
    `Value(`Constant,
           unsafe_bitvector_to_int bv2 |>
           Bitvector.shift_right bv1)
  | _, _ -> symb_of_binary_expr SmtBvLshr param1 param2

let rshiftS param1 param2 =
  match param1, param2 with
    `Value(`Constant, bv1), `Value(`Constant, bv2) ->
    `Value(`Constant,
           unsafe_bitvector_to_int bv2 |>
           Bitvector.shift_right_signed bv1)
  | _, _ -> symb_of_binary_expr SmtBvAshr param1 param2

let rotate_left (param1:t) (param2:t) : t =
  match param1, param2 with
    `Value (`Constant, bv1), `Value (`Constant, bv2) ->
    `Value (`Constant,
            unsafe_bitvector_to_int bv2 |>
            Bitvector.rotate_left bv1)
  | `Undef size, _ | _, `Undef size -> `Undef size

  | `SymbSmt _s1, _op2 -> failwith "symb rotate left"

  | _op1, `SymbSmt _s2 -> failwith "symb rotate left"

  | `Value _, `Value _ -> raise (Regions_conflict "rotate_left")
  | _,  _ ->
    assert false
(*    raise (Unknown_value ((string_of p1) ^ "or" ^ (string_of p2))) *)

let rotate_right param1 param2 =
  match param1, param2 with
    `Value (`Constant, bv1), `Value (`Constant, bv2) ->
    `Value (`Constant,
            unsafe_bitvector_to_int bv2 |>
            Bitvector.rotate_right bv1)
  | `Undef size, _ | _, `Undef size -> `Undef size
  | `SymbSmt _s1, _op2 -> failwith "symb: rotate right"

  | _op1, `SymbSmt _s2 -> failwith "symb: rotate right"

  | `Value _, `Value _ -> raise (Regions_conflict "rotate_right")
  | _, _ -> assert false
(*    raise (Unknown_value ((string_of p1) ^ "or" ^ (string_of p2)))*)

let rec extension param k =
  match param with
    `Value (r, b) ->
    let b = Bitvector.value_of b in
    if r = `Constant then `Value (r, Bitvector.create b k)
    else `Value (r, Bitvector.create b k)
  | `Symb s ->
    let v = normalize_symb s in
    begin
      match v with
      | `Value _ as p -> extension p k
      | p ->
        let sz = size_of p in
        symb_of_unary_expr (SmtBvZeroExtend (k - sz)) p
    end
  | _ ->
    let sz = size_of param in
    symb_of_unary_expr (SmtBvZeroExtend (k - sz)) param

let signed_extension param k =
  match param with
    `Value (r, b) ->
    let size = Bitvector.size_of b in
    let b = Bitvector.value_of b in
    if r = `Constant then (
      let b = Bitvector.extend_signed (Bitvector.create b size) k in
      `Value (r, b))
    else
      let sz = (k - (size_of param)) in
      symb_of_unary_expr (SmtBvSignExtend sz) param
  | `Symb s ->
    let v = normalize_symb s in
    begin
      match v with
      | `Value _ as p -> extension p k
      | p ->
        let sz = size_of p in
        symb_of_unary_expr (SmtBvSignExtend (k - sz)) p
    end
  | _ ->
    let sz = (k - (size_of param)) in
    symb_of_unary_expr (SmtBvSignExtend sz) param




(* comparisons *)

let rec eq param1 param2 =
  match param1, param2 with
    `Value (r1, b1), `Value (r2, b2) ->
    let b1 = Bitvector.value_of b1 in
    let b2 = Bitvector.value_of b2 in
    if (region_equal r1 r2) && Bigint.eq_big_int b1 b2 then
      `Value (`Constant, Bitvector.one)
    else
      `Value (`Constant, Bitvector.create (Bigint.zero_big_int) 1)
  | `Symb s1, `Symb s2 ->
    let s1, s2 = normalize_symb s1, normalize_symb s2 in begin
      match s1, s2 with
        `Symb s1', `Symb s2' ->
        if SubSymb.compare s1' s2' = 0 then
          `Value (`Constant, Bitvector.one)
        else
          `Undef (1)
      | _, _ -> eq s1 s2
    end
  | `Symb s, `Value v | `Value v, `Symb s ->
    let s' = normalize_symb s in begin
      match s' with
      | `Value v' -> eq (`Value v') (`Value v)
      | _ -> `Value (`Constant, Bitvector.zero)

    end
  | _, _ -> symb_of_binary_expr SmtBvComp param1 param2

let rec diff param1 param2 =
  match param1, param2 with
    `Value (r1, b1), `Value (r2, b2) ->
    let b1 = Bitvector.value_of b1 in
    let b2 = Bitvector.value_of b2 in
    if r1 = r2 && Bigint.eq_big_int b1 b2 then
      `Value (`Constant, Bitvector.zero)
    else
      `Value (`Constant, Bitvector.one)
  | `Symb s1, `Symb s2 ->
    let s1, s2 = normalize_symb s1, normalize_symb s2 in begin
      match s1, s2 with
        `Symb s1', `Symb s2' ->
        if SubSymb.compare s1' s2' = 0 then
          `Value (`Constant, Bitvector.zero)
        else
          `Value(`Constant, Bitvector.zero)
      | _, _ -> diff s1 s2
    end
  | _, _ -> symb_of_binary_expr SmtBvDiff param1 param2

let leqU param1 param2 =
  match param1, param2 with
    `Value (r1, b1), `Value (r2, b2) ->
    let b1 = Bitvector.value_of b1 in
    let b2 = Bitvector.value_of b2 in
    if r1 = r2 then
      if Bigint.le_big_int b1 b2 then
        `Value(`Constant, Bitvector.one)
      else
        `Value(`Constant, Bitvector.zero)
    else (
      match r1, r2 with
      | `Constant, _ -> `Value(`Constant, Bitvector.one)
      | _, `Constant -> `Value(`Constant, Bitvector.zero)
      | _, _ -> symb_of_binary_expr SmtBvUle param1 param2
    )
  | _, _ -> symb_of_binary_expr SmtBvUle param1 param2

let ltU  param1 param2 =
  match param1, param2 with
    `Value (r1, b1), `Value (r2, b2) ->
    let b1 = Bitvector.value_of b1 in
    let b2 = Bitvector.value_of b2 in
    if (region_equal r1 r2) then
      if Bigint.lt_big_int b1 b2 then
        `Value (`Constant, Bitvector.one)
      else
        `Value (`Constant, Bitvector.zero)
    else (
      match r1, r2 with
      | `Constant, _ -> `Value(`Constant, Bitvector.one)
      | _, `Constant -> `Value(`Constant, Bitvector.zero)
      | _, _ -> symb_of_binary_expr SmtBvUlt param1 param2
    )

  | _, _ -> symb_of_binary_expr SmtBvUlt param1 param2

let geqU param1 param2 =
  match param1, param2 with
    `Value (r1, b1), `Value (r2, b2) ->
    let b1 = Bitvector.value_of b1 in
    let b2 = Bitvector.value_of b2 in
    if (region_equal r1 r2) then
      if Bigint.ge_big_int b1 b2 then
        `Value (`Constant, Bitvector.one)
      else
        `Value(`Constant, Bitvector.zero)
    else (
      match r1, r2 with
      | `Constant, _ -> `Value(`Constant, Bitvector.zero)
      | _, `Constant -> `Value(`Constant, Bitvector.one)
      | _, _ -> raise (Regions_conflict "geqU")
    )
  | _, _ -> symb_of_binary_expr SmtBvUge param1 param2

let gtU param1 param2 =
  match param1, param2 with
    `Value (r1, b1), `Value (r2, b2) ->
    let b1 = Bitvector.value_of b1 in
    let b2 = Bitvector.value_of b2 in
    if (region_equal r1 r2) then
      if Bigint.gt_big_int b1 b2 then
        `Value (`Constant, Bitvector.one)
      else
        `Value(`Constant, Bitvector.zero)
    else (
      match r1, r2 with
      | `Constant, _ -> `Value(`Constant, Bitvector.zero)
      | _, `Constant -> `Value(`Constant, Bitvector.one)
      | _, _ -> raise (Regions_conflict "geqU")
    )
  | _, _ -> symb_of_binary_expr SmtBvUgt param1 param2

let leqS param1 param2 =
  match param1, param2 with
    `Value (r1, b1), `Value (r2, b2) ->
    let b1 = Bitvector.value_of b1 in
    let b2 = Bitvector.value_of b2 in
    if region_equal r1 r2 then
      if Bigint.le_big_int b1 b2 then
        `Value (`Constant, Bitvector.one)
      else
        `Value (`Constant, Bitvector.zero)
    else raise (Regions_conflict "leqs")
  | _, _ -> symb_of_binary_expr SmtBvSle param1 param2

let ltS param1 param2 =
  match param1, param2 with
  | `Value (r1, b1), `Value (r2, b2) ->
    if region_equal r1 r2 then
      let b1 = Bitvector.value_of b1 in
      let b2 = Bitvector.value_of b2 in
      if Bigint.lt_big_int b1 b2 then
        `Value (`Constant, Bitvector.one)
      else
        `Value(`Constant, Bitvector.zero)
    else conflicted_region "slt" r1 r2
  | _, _ -> symb_of_binary_expr SmtBvSlt param1 param2

let geqS param1 param2 =
  match param1, param2 with
    `Value (r1, b1), `Value (r2, b2) ->
    let b1 = Bitvector.value_of b1 in
    let b2 = Bitvector.value_of b2 in
    if (region_equal r1 r2) then
      if Bigint.ge_big_int b1 b2 then
        `Value (`Constant, Bitvector.one)
      else
        `Value (`Constant, Bitvector.zero)
    else raise (Regions_conflict "geqS")
  | _, _ -> symb_of_binary_expr SmtBvSge param1 param2

let gtS param1 param2 =
  match param1, param2 with
    `Value (r1, b1), `Value (r2, b2) ->
    let b1 = Bitvector.value_of b1 in
    let b2 = Bitvector.value_of b2 in
    if (region_equal r1 r2) then
      if Bigint.gt_big_int b1 b2 then
        `Value (`Constant, Bitvector.one)
      else
        `Value (`Constant, Bitvector.zero)
    else raise (Regions_conflict "geS")
  | _, _ -> symb_of_binary_expr SmtBvSgt param1 param2


let get_byte_region_at addr =
  let open Bigint in
  try
    let byte = Loader_utils.get_byte_at addr in
    let bitsize = 8 in
    create_constant (big_int_of_int byte) bitsize
  with
  | _ ->
    let msg = Format.sprintf "Could not get byte at %Lx" (int64_of_big_int addr) in
    Errors.invalid_address msg

let default_get_byte_region_at addr =
  try get_byte_region_at addr with Errors.Invalid_address _ -> undefined 8
