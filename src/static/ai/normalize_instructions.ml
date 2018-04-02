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

open Smtlib2
open Smtlib2print

module ThingSet = Set.Make (
  struct
    let compare = Pervasives.compare
    type t = string * int
  end
)

module RegionSet = Set.Make (
  struct
    let compare (a1, _, _) (a2, _, _) = Pervasives.compare a1 a2
    type t = string * int * Dba.region
  end
 )

let inc_varindex varIndexes name =
  try
    let value = Basic_types.String.Map.find name varIndexes in
    Basic_types.String.Map.add name (value+1) varIndexes
  with Not_found ->
    Basic_types.String.Map.add name 1 varIndexes

let get_varindex vars name =
  try Basic_types.String.Map.find name vars
  with Not_found -> 0


(* Converts a dba expression into SMT formula *)
let rec dbaExpr_to_smtExpr expr inputs varIndexes : smt_bv_expr * SmtVarSet.t =
  match expr with
  | Dba.ExprVar (name, size, _) ->
    let new_name = name ^ (string_of_int (get_varindex varIndexes name)) in
    let inputs = SmtVarSet.add (SmtBv (new_name, size)) inputs in
    SmtBvVar (new_name, size), inputs
  | Dba.ExprLoad (size, endian, e) ->
    let smt_load, inputs = load_to_smt e size endian inputs varIndexes in
    smt_load, inputs
  | Dba.ExprRestrict (e, i, j) ->
    let smt_expr, inputs = dbaExpr_to_smtExpr e inputs varIndexes in
    SmtBvUnary (SmtBvExtract (i, j), smt_expr), inputs
  | Dba.ExprCst (_, bv) -> SmtBvCst bv, inputs
  | Dba.ExprUnary (op, e) ->
    let smt_e, inputs = dbaExpr_to_smtExpr e inputs varIndexes in
    SmtBvUnary (Dba_to_smtlib.unary op, smt_e), inputs
  | Dba.ExprBinary (op, e1, e2) -> (
    let expr1, inputs = dbaExpr_to_smtExpr e1 inputs varIndexes in
    let expr2, inputs = dbaExpr_to_smtExpr e2 inputs varIndexes in
    match op with
    | Dba.LShift | Dba.RShiftU | Dba.RShiftS ->
      (* Check size of the two bitvectors because DBA accepts different
         but not smtlib *)
      let size1 = size_bvexpr expr1 in
      let size2 = size_bvexpr expr2 in
      if size1 = size2 then
        SmtBvBinary (Dba_to_smtlib.binary op, expr1, expr2), inputs
      else if size1 > size2 then
        let expr2 = SmtBvUnary (SmtBvZeroExtend (size1 - size2), expr2) in
        SmtBvBinary (Dba_to_smtlib.binary op, expr1, expr2), inputs
      else
        failwith "shift x y avec taille(y) > taille(x)"
    | (Dba.LeqU|Dba.LtU|Dba.GeqU|Dba.GtU|Dba.LeqS|Dba.LtS|Dba.GeqS|Dba.GtS) ->
      (* Create formula to return #b1 or #b0 instead of a boolean *)
      SmtBvIte (SmtBvExpr (SmtBvBinary (Dba_to_smtlib.binary op, expr1, expr2)),
                bvone , bvzero), inputs
    | Dba.LeftRotate ->
      (match expr2 with
      | SmtBvCst bv ->
        let value = Bitvector.value_of bv in
        SmtBvUnary (SmtBvRotateL (Bigint.int_of_big_int value), expr1 ), inputs
      (* smtlib only takes constant as shift index, so fail if not a SmtBvCst *)
      | _ ->
        Format.printf "%a|%s"
          Dba_printer.Ascii.pp_expr e2 (smtbvexpr_to_string expr2);
        failwith "shift index for rotate left cannot be expr (must be const)")
    | Dba.RightRotate -> (
      match expr2 with
      | SmtBvCst bv ->
        let value = Bitvector.value_of bv in
        SmtBvUnary (SmtBvRotateR (Bigint.int_of_big_int value), expr1), inputs
      | _ ->
        Format.printf "%a|%s"
          Dba_printer.Ascii.pp_expr e2 (smtbvexpr_to_string expr2);
        failwith "shift index for rotate right cannot be expr (must be const)"
    )
    | _ ->
      SmtBvBinary(Dba_to_smtlib.binary op, expr1, expr2), inputs
  ) (* Normal case *)
  | Dba.ExprExtU (e, size) ->

    let expr, inputs = dbaExpr_to_smtExpr e inputs varIndexes in
    let s = size_bvexpr expr in
    SmtBvUnary (SmtBvZeroExtend (size - s), expr), inputs
  | Dba.ExprExtS (e, size) ->
    let expr, inputs = dbaExpr_to_smtExpr e inputs varIndexes in
    let s = size_bvexpr expr in
    SmtBvUnary (SmtBvSignExtend (size - s), expr), inputs
  | Dba.ExprIte(c, e1, e2) ->
    let f_c, inputs = cond_to_f c inputs varIndexes in
    let smt_e1, inputs = dbaExpr_to_smtExpr e1 inputs varIndexes in
    let smt_e2, inputs = dbaExpr_to_smtExpr e2 inputs varIndexes in
    SmtBvIte (SmtBvExpr f_c, smt_e1, smt_e2), inputs
  | Dba.ExprAlternative(e_l, _) ->
    let eval e = dbaExpr_to_smtExpr e inputs varIndexes in
    let eq _ _ = true in (* This is probably a false assumption *)
    Dba_utils.eval_alternatives eval eq e_l


and logical_load (addr:smt_bv_expr) size varIndexes : smt_bv_expr = (* size in BYTES *)
  (* Recursive function to concat each byte read into a single bitvector *)
  let rec concat_select_symb sz =
    let to_add = size - sz in
    let new_addr =
      if to_add = 0
      then addr
      else (smtbv_add_int addr to_add)
    in
    let new_name = "memory" ^ (string_of_int (get_varindex varIndexes "memory")) in
    let memory = SmtABvArray (new_name, Machine.Word_size.get (), 8) in
    match sz with
    | 1 -> SmtABvSelect (memory, new_addr)
    | 4 -> SmtABvLoad32 (memory, addr)
    | sz ->
      let op1 = concat_select_symb (sz - 1) in
      let op2 = SmtABvSelect (memory, new_addr) in
      SmtBvBinary (SmtBvConcat, op1, op2)
  in
  concat_select_symb size


(* Converts a load in expression *)
and load_to_smt expr size endianness inputs varIndexes : smt_bv_expr * SmtVarSet.t =
  match endianness with
  | Dba.BigEndian -> failwith "Big endian is not implemented\n"
  | Dba.LittleEndian ->
    let expr_f, inputs = dbaExpr_to_smtExpr expr inputs varIndexes in
    logical_load expr_f size varIndexes, inputs


(* Converts a condition into formula *)
(* TODO:voir pour pour convertir bvcmp X 1 par un =  *)
and cond_to_f c inputs varIndexes : smt_bv_expr * SmtVarSet.t =
  match c with
  | Dba.CondReif e ->
    let expr, inputs = dbaExpr_to_smtExpr e inputs varIndexes in
    expr, inputs
  | Dba.CondNot c ->
    let smt_c, inputs = cond_to_f c inputs varIndexes in
    SmtBvUnary (SmtBvNot, smt_c), inputs
  | Dba.CondAnd (c1, c2) ->
    let f_c1, inputs = cond_to_f c1 inputs varIndexes in
    let f_c2, inputs = cond_to_f c2 inputs varIndexes in
    SmtBvBinary (SmtBvAnd, f_c1, f_c2), inputs
  | Dba.CondOr (c1, c2) ->
    let f_c1, inputs = cond_to_f c1 inputs varIndexes in
    let f_c2, inputs = cond_to_f c2 inputs varIndexes  in
    SmtBvBinary (SmtBvOr, f_c1, f_c2), inputs
  | Dba.True -> bvone, inputs
  | Dba.False -> bvzero, inputs



let change_variable
    _varIndexes _vars (name:string) (low:int) (high:int) (e:Dba.expr) varIndexes vars inputs =
  let new_name = name ^ (string_of_int (get_varindex varIndexes name)) in
  let future_name = name ^ (string_of_int (get_varindex varIndexes name + 1)) in
  let varIndexes = inc_varindex varIndexes name in
  let e, inputs = dbaExpr_to_smtExpr e inputs varIndexes in
  let new_var = SmtBvVar(new_name, (high-low)+1) in
  let size = (high-low)+1 in
  let new_vars =
    if Basic_types.String.Map.mem name vars then                   (* If a symb var of the same name already exist *)
      let l_old, h_old, f_old = Basic_types.String.Map.find name vars in (* get it *)
      if l_old = low && h_old = high then                 (* If the new formula of the same size *)
        Basic_types.String.Map.add name (low, high, new_var) vars      (* If equal simply replace.. *)
      else if l_old <= low && high <= h_old then          (* If the new formula is within the old *)
        let new_e = if high < h_old then SmtBvBinary(SmtBvConcat, SmtBvUnary(SmtBvExtract(high+1,h_old),f_old), e) else e in
        let new_e = if low > l_old then SmtBvBinary(SmtBvConcat, new_e,  SmtBvUnary(SmtBvExtract(l_old,low-1),f_old)) else new_e in
        Basic_types.String.Map.add name (l_old,h_old, new_e) vars        (* Add it to the symbolic var *)
      else
        failwith "disjoint variables..."
    else

      Basic_types.String.Map.add name (low, high, new_var) vars
  in
  let new_e = e in
  let new_let = (SmtBvExpr new_var, SmtBvExpr new_e) in
  let inputs =  SmtVarSet.add (SmtBv (new_name, size)) inputs in
  let inputs =  SmtVarSet.add (SmtBv (future_name, size)) inputs in
  new_let, new_vars, inputs, varIndexes

(* Update variable introducing a new let *)
let maj_variable name _size low high e varIndexes vars inputs =
  change_variable varIndexes vars name low high e varIndexes vars inputs


let store_memory varIndexes (mem_f:smt_abv_expr) =
  let new_mem = mem_f in
  let new_name = "memory" ^ (string_of_int (get_varindex varIndexes "memory" - 1)) in
  let new_memory = SmtABvArray (new_name, 32, 8) in
  let new_let = (SmtABvArrayExpr new_memory, SmtABvArrayExpr new_mem) in
  new_let, varIndexes


let maj_memory varIndexes (fexpr:smt_abv_expr) =
  store_memory varIndexes fexpr


let new_tmpvar varIndexes (size:int) =  (* Create a new variable name *)
  let index = get_varindex varIndexes "tmp" in
  let new_varid = "tmp" ^ (string_of_int index) in
  let new_mem_var = SmtBvVar (new_varid, size) in
  let varIndexes = inc_varindex varIndexes "tmp" in
  varIndexes, new_mem_var


let logical_store (f_addr:smt_bv_expr) (f_expr:smt_bv_expr) (size:Dba.size) varIndexes _vars _inputs =
  let new_name = "memory" ^ (string_of_int (get_varindex varIndexes "memory")) in
  let memory = SmtABvArray(new_name, 32, 8) in
  let rec aux_store_symb sz data l h =
    (* Recursive function to create store on multiple index (by extracting each byte of the bv) *)
    let to_add =  size - sz in
    let new_addr = if to_add = 0 then f_addr else (smtbv_add_int f_addr to_add) in
    match sz with
    | 1 -> SmtABvStore (memory, new_addr, smtbv_extract data l h)
    | sz -> SmtABvStore (aux_store_symb (sz-1) data (l+8) (h+8), new_addr, smtbv_extract data l h)
  in
  match size with
  | 1 -> maj_memory varIndexes (SmtABvStore(memory, f_addr, f_expr))
  | 4 -> maj_memory varIndexes (SmtABvStore32(memory, f_addr, f_expr))
  | size -> let varIndexes, new_var = new_tmpvar varIndexes (size_bvexpr f_expr) in
    let content_f = (aux_store_symb size new_var 0 7) in
    let f = SmtABvLet([(SmtBvExpr new_var, SmtBvExpr f_expr)], content_f) in
    maj_memory varIndexes f


let maj_store e_addr size e endianness varIndexes vars inputs =
  (* Purely symbolic implementation *)
  match endianness with
  | Dba.BigEndian -> failwith "Normalize_dbaInstruction: dbaBig is not yet implemented"
  | Dba.LittleEndian ->
    let varIndexes = inc_varindex varIndexes "memory" in
    let f_expr, inputs = dbaExpr_to_smtExpr e inputs varIndexes in  (* Formula of data to write *)
    let f_addr, inputs = dbaExpr_to_smtExpr e_addr inputs varIndexes in
    let new_let, varIndexes = logical_store f_addr f_expr size varIndexes vars inputs
    in new_let, vars, inputs, varIndexes


let assign_to_f lhs e varIndexes vars inputs =
  match lhs with
  | Dba.LhsVar(name, size, _) ->            maj_variable name size 0 (size-1) e varIndexes vars inputs
  | Dba.LhsVarRestrict(name, size, i, j) -> maj_variable name size i j e varIndexes vars inputs
  | Dba.LhsStore(size, endian, e_addr) ->      maj_store e_addr size e endian varIndexes vars inputs


let rec string_of_lets lets inputs =
  match lets with
  | [] -> "", inputs
  | (var, exp) :: tl ->
    let var_s = smtexpr_to_string var in
    let exp_s = smtexpr_to_string exp in
    match var with
    | SmtBvExpr (SmtBvVar(n,sz)) ->
      let s = Format.asprintf "(define-fun %s () (_ BitVec %d) %s)\n" var_s sz exp_s in
      let inputs = SmtVarSet.remove (SmtBv(n, sz)) inputs in
      let acc, inputs = (string_of_lets tl inputs) in
      s ^ acc, inputs
    | SmtABvArrayExpr (SmtABvArray(_,a,c)) ->
      let s = Format.asprintf "(define-fun %s () (Array (_ BitVec %d) (_ BitVec %d)) %s)\n" var_s a c exp_s in
      (* let inputs = SmtVarSet.remove (SmtABv (n, a, c)) inputs in  *)
      let acc, inputs = (string_of_lets tl inputs) in
      s ^ acc, inputs
    | _ -> failwith "impossible"



let rec string_of_env env =
  match env with
  | [] -> ""
  | smt_cond :: tl ->
    match smt_cond with
      SmtBvBinary (SmtBvUle, _, _)
    | SmtBvBinary (SmtBvSle, _, _) ->
      let assertion = SmtBvExpr smt_cond (* SmtComp (SmtBvExpr smt_cond, SmtBvExpr one) *) in
      let s = Format.asprintf "(assert %s)\n" (smtexpr_to_string assertion) in
      s ^ (string_of_env tl)
    | _ -> let assertion = SmtComp (SmtBvExpr smt_cond, one) in
      let s = Format.asprintf "(assert %s)\n" (smtexpr_to_string assertion) in
      s ^ (string_of_env tl)



let print_word_decl size =
  (Format.asprintf "(define-sort Word () (_ BitVec %d))\n") size


let print_var_decl v =
  let f (y, sz) x =
    if sz = 32
    then Format.asprintf "%s(declare-const %s Word)\n" x y
    else Format.asprintf "%s(declare-const %s (_ BitVec %d))\n" x y sz
  in
  "(declare-const r Word)\n" ^
  "(declare-const offset Word)\n" ^
  "(declare-const stack Word)\n" ^
  (if ThingSet.mem ("cst", 32) v
   then ""
   else "(declare-const cst Word)\n") ^
  (ThingSet.fold f v "") (* ^ *)
(* (Format.asprintf "(declare-const c (_ BitVec %d))\n" size) *)

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

  "(assert (forall ((b Word) (b1 Word) (o Word) (o1 Word))\n\
        (=> (and (not (= b b1))\n\
                         (not (= b cst))\n\
                         (not (= b1 cst))\n\
           (bvult o (size b))\n\
           (bvult o1 (size b1)))\n\
            (distinct (bvadd b o) (bvadd b1 o1)))))\n" ^

  "(assert (forall ((b Word) (o Word))\n\
    (=> (and (bvult o (size b)) (not (= b cst)))\n\
        (distinct (bvadd b o) #x00000000))))\n"


let string_of_bv bv =
  let value = Bitvector.value_of bv in
  let size = Bitvector.size_of bv in
  if (size mod 4) = 0 then
    let s = Bitvector.to_hexstring bv in
    let s = String.sub s 1 ((String.length s) - 1) in
    Format.asprintf "#%s" s
  else
    let i = Bigint.string_of_big_int value in
    Format.asprintf "((_ int2bv %d) %s)" size i

let print_distinct_blocks b =
  let f2 (name, _, r) acc =
    match r with
    | `Malloc ((-1, { Dba.base = bv; Dba.id = -1}), _) when Bitvector.size_of bv = 32 ->
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



let print_get_model var =
  let s, var =
    if String.length var > 50 then
      Format.asprintf "(define-fun t () (_ BitVec %d) %s)\n" 32 var, "t"
    else
      "", var
  in
  let assertion = Format.asprintf "(get-value (%s))\n" var in
  (* s ^ "(check-sat)\n" ^ assertion *)
  s ^ "(check-sat-using (then simplify solve-eqs smt))\n" ^ assertion

let make_smt_program lets env inputs var conds =
  let v = ThingSet.empty in
  let b = RegionSet.empty in
  let regions =
    print_word_decl (Machine.Word_size.get ()) ^
    print_var_decl v ^
    print_max_min_decl ^
    print_region_size_function b ^
    print_non_overlap_assertion ^
    print_distinct_blocks b
  in

  let lets, inputs = string_of_lets lets inputs in
  let env = string_of_env env in
    regions ^
    smtvarset_to_string inputs ^ "\n\n" ^
    Printf.sprintf "%s\n\n" (smt_functions ())
    ^ lets ^ env ^ conds ^
    print_get_model var


let get_block_value_from_ident i l =
  let i = if String.length i > 50 then "t" else i in
  try List.assoc i l
  with
  | Not_found ->
    let i = String.sub i 1 (String.length i - 2) in
    try List.assoc i l
    with
    | Not_found ->
      List.iter (fun (i, _) -> Logger.debug "%s" i) l;
      let msg = Format.sprintf "Ident %s not found in model returned !!" i in
      failwith msg


let big_int_of_smt_num_string s =
  let n = ("0"^(String.sub s 1 ((String.length s) -1))) in
  let n = Int64.to_string (Int64.of_string n) in
  Bigint.big_int_of_string n


let call_smt_solver lets env inputs var conds =
  let smt_program = make_smt_program lets env inputs var conds in
  let smtfile = open_out "smt_dba_backward_in.smt2" in
  Printf.fprintf smtfile "%s" smt_program;
  close_out smtfile;
  ignore (Sys.command "z3 -smt2 smt_dba_backward_in.smt2 > smt_dba_backward_out");
  let smtout = open_in "smt_dba_backward_out" in
  let lexbuf = Lexing.from_channel smtout in
  let result, models = SMTParserWp.main SMTLexerWp.token lexbuf in
  close_in smtout;
  match result with
  | None | Some SAT ->
    begin match models with
      | model :: _ ->
        let bv_string = get_block_value_from_ident var model in
        let bv = big_int_of_smt_num_string bv_string in
        Some (bv_string, bv)
      | [] -> assert false
    end
  | Some UNSAT ->
    None
  | _ -> failwith "SMT result TIMEOUT|UNKNOWN"


let is_sat (lets, env, inputs, var, varIndexes) cond =
  let memory = "memory" ^ (string_of_int (get_varindex varIndexes "memory")) in
  let inputs = SmtVarSet.add (SmtABv(memory, Machine.Word_size.get (), 8)) inputs in
  let conds = cond in
  let res = call_smt_solver lets env inputs var conds in
  not (Utils.is_none res)


let get_lower_bound _ _ bv = bv


let get_upper_bound (lets, env, inputs, _var, varIndexes) var bv =
  let u = Bitvector.value_of bv in
  let size = Bitvector.size_of bv in
  let memory = "memory" ^ (string_of_int (get_varindex varIndexes "memory")) in
  let inputs = SmtVarSet.add (SmtABv(memory, Machine.Word_size.get (), 8)) inputs in

  let rec dichotomous_upper_bound bound index =
    if index < 0
    then (
      Logger.debug ~level:2
        "refined !! bound = %s, u = %s \n%!"
        (Bigint.string_of_big_int bound) (Bigint.string_of_big_int u);
      (Bitvector.create bound size)
    )
    else
      let witness = Bigint.extract_big_int u index 1 in
      if Bigint.compare_big_int witness Bigint.zero_big_int = 0
      then (
        let bound_zero = Bigint.shift_left_big_int bound 1 in
        dichotomous_upper_bound bound_zero (index - 1)
      )
      else
        let cond1 = Format.asprintf "((_ extract %i %i) %s)" (size - 1) index var in
        let bound_zero = Bigint.shift_left_big_int bound 1 in
        let bound_one = Bigint.succ_big_int bound_zero in
        let cond2 = smtbvexpr_to_string (SmtBvCst (Bitvector.create bound_one (size - index))) in
        let conds = Format.asprintf "(assert (= %s %s))\n" cond1 cond2 in
        let res = call_smt_solver lets env inputs var conds in
        match res with
          Some _ -> dichotomous_upper_bound bound_one (index - 1)
        | None -> dichotomous_upper_bound2 bound_zero (index - 1)
  and dichotomous_upper_bound2 bound index =
    if index < 0
    then (
      Logger.debug ~level:2 "refined !! bound = %s, u = %s \n%!"
        (Bigint.string_of_big_int bound) (Bigint.string_of_big_int u) ;
      Bitvector.create bound size
    )
    else
      let bound_zero = Bigint.shift_left_big_int bound 1 in
      let bound_one = Bigint.succ_big_int bound_zero in
      let cond1 = Format.asprintf "((_ extract %i %i) %s)" (size - 1) index var in
      let cond2 = smtbvexpr_to_string (SmtBvCst (Bitvector.create bound_one (size - index))) in
      let conds = Format.asprintf "(assert (= %s %s))\n" cond1 cond2 in
      let res = call_smt_solver lets env inputs var conds in
      match res with
      | Some _ -> dichotomous_upper_bound2 bound_one (index - 1)
      | None -> dichotomous_upper_bound2 bound_zero (index - 1)
  in
  dichotomous_upper_bound Bigint.zero_big_int (size - 1)


let apply_smt_elements_recovery lets env inputs var varIndexes =
  let memory = "memory" ^ (string_of_int (get_varindex varIndexes "memory")) in
  let inputs = SmtVarSet.add (SmtABv (memory, Machine.Word_size.get (), 8)) inputs in
  let rec aux acc conds nb =
    if nb > 10 then acc
    else (
        let res = call_smt_solver lets env inputs var conds in
        match res with
        | Some (bv1, bv2) ->
          let bv = Bitvector.create bv2 32 in
          let acc = `Value (`Constant, bv) :: acc in
          let cond = Format.asprintf "(assert (not (= %s %s)))\n" var bv1 in
          let conds = cond ^ conds in
          aux acc conds (nb + 1)
        | None -> acc
    )
  in aux [] "" 0
