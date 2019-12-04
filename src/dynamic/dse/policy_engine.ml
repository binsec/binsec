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

open Bigint
open Policy_type
open Dse_options

exception Rule_match
exception PlaceHolderNotFound of string
exception InvalidPlaceHolderType
exception InconsistentRule of string

module MetaVarMap = Basic_types.String.Map

type metavarmap = metavar_item MetaVarMap.t

let merge_metavar ?(soft_merge=false) map1 map2: metavarmap =
  MetaVarMap.merge (fun _ a b ->
      match a, b with
      | None, None -> None
      | Some x, None | None, Some x -> Some x
      | Some x, Some _ ->
        if soft_merge then Some x else failwith "Duplicate metavar name !"
    ) map1 map2

let get_metavar_expr = function
  | Dba.(Expr.Var { name = s; size = 0; _}) when (String.get s 0) = '?' -> s
  | _ -> ""

let get_metavar_cond = function
  | Dba.(Expr.Var { name = s; size = 0; _}) when (String.get s 0) = '?' -> s
  | _ -> ""

let get_metavar_lhs (var:Dba.LValue.t): string =
  match var with
  | Dba.(LValue.Var { name = s; size = 0; _}) when (String.get s 0) = '?' -> s
  | _ -> ""
;;

let is_wildcard v =
  let open Dba in v.name = "*" && v.size = 0


let is_wildcard_cond = function
  | Dba.Expr.Var v -> is_wildcard v
  | _ -> false

let is_wildcard_expr = is_wildcard_cond

let is_wildcard_lhs (var:Dba.LValue.t): bool =
  match var with | Dba.LValue.Var v -> is_wildcard v | _ -> false

let is_metavar v = let open Dba in v.name.[0] = '?' && v.size = 0

let is_metavar_expr (var:Dba.Expr.t): bool =
  match var with | Dba.Expr.Var v -> is_metavar v | _ -> false

let is_metavar_cond = function
  | Dba.Expr.Var v -> is_metavar v
  | _ -> false

let is_metavar_lhs (var:Dba.LValue.t): bool =
  match var with
  | Dba.LValue.Var v -> is_metavar v
  | _ -> false

let is_placeholder v = let open Dba in v.name.[0] = '!' && v.size = 0

let is_placeholder_expr (var:Dba.Expr.t): bool =
  match var with | Dba.Expr.Var v -> is_placeholder v | _ -> false

let is_placeholder_cond = function
  | Dba.Expr.Var v -> is_placeholder v
  | _ -> false


let get_placeholder (s:string) (metas:metavarmap): metavar_item =
  try MetaVarMap.find s metas
  with Not_found -> raise (PlaceHolderNotFound(s))

let get_placeholder_expr (var:Dba.Expr.t) (metas:metavarmap): Dba.Expr.t =
  match var with
  | Dba.(Expr.Var { name = s; _}) ->
    begin match get_placeholder s metas with
      | ExprReif e -> e
      | CondReif _ -> raise InvalidPlaceHolderType
      | _ -> raise InvalidPlaceHolderType
    end
  | _ -> failwith "Not a placeholder"

let get_placeholder_cond var metas =
  match var with
  | Dba.(Expr.Var { name = s; _}) ->
    begin match get_placeholder s metas with
      | CondReif c -> c
      | _ -> raise InvalidPlaceHolderType
    end
  | _ -> failwith "Not a placeholder"

let is_current_expr = function
  | Dba.(Expr.Var { name = "!$$"; size = 0; _}) -> true
  |  _ -> false

let to_placeholder (s:string): string =
  "!"^(String.sub s 1 ((String.length s)-1))

let default_rule =
  { loc_p=LocWildcard;
    inst_p=InstWildcard;
    exp_p=ExpWildcard;
    sigma_p=SigmaWildcard;
    action=(KeepOrSymb,"")}

let match_location (pred:loc_pred) (loc:int64) _: bool =
  match pred with
  | LocWildcard -> true
  | LocSet l -> List.fold_left (fun acc addr -> acc || loc=addr) false l
  | LocInterval(low,high) -> loc >= low && loc <= high

let rec check_recursive_expr ?(current=None) (exp_rule:Dba.Expr.t) (exp:Dba.Expr.t) (metas:metavarmap): bool * metavarmap  =
  (* let _ = Logger.debug "%s <==> %s\n" (string_of_expr exp_rule ~ascii:false ~light:true true) (string_of_expr exp ~ascii:false ~light:true true) in *)
  if is_current_expr exp_rule then
    match current with
    | Some a -> check_recursive_expr ~current a exp metas
    | None -> failwith "!$$ used but not provided"
  else if is_wildcard_expr exp_rule then
    true, metas
  else if is_metavar_expr exp_rule then
    true, MetaVarMap.add (to_placeholder (get_metavar_expr exp_rule)) (ExprReif exp) metas
  else if is_placeholder_expr exp_rule then
    (* Do some kind of type checking / size checking ? *)
    check_recursive_expr ~current (get_placeholder_expr exp_rule metas) exp metas
  else
    match exp_rule, exp with
    | Dba.(Expr.Var {name =n1; _}), Dba.(Expr.Var {name = n2; _}) when n1=n2 ->
       true, metas
    | Dba.Expr.Load(_, in1, e1),  Dba.Expr.Load(_, in2, e2) when in1 = in2 ->
      (* Do not compare sizes (if where not infered in parsing) *)
      check_recursive_expr ~current e1 e2 metas
    | Dba.Expr.Cst(`Constant, v1), Dba.Expr.Cst(`Constant, v2)
      when Bitvector.equal v1 v2 -> true, metas
    | Dba.Expr.Unary(op1, e1), Dba.Expr.Unary(op2, e2) when op1 = op2 ->
      check_recursive_expr ~current e1 e2 metas
    | Dba.Expr.Binary(op1, e1_1,e1_2), Dba.Expr.Binary(op2, e2_1,e2_2) when op1 = op2 ->
      let res1, metas1 = check_recursive_expr ~current e1_1 e2_1 metas in
      let res2, metas2 = check_recursive_expr ~current e1_2 e2_2 metas in
      if res1 && res2 then
        true, merge_metavar ~soft_merge:true metas1 metas2
      else
        false, MetaVarMap.empty
    | Dba.Expr.Ite(c1, e1_1, e1_2),  Dba.Expr.Ite(c2, e2_1, e2_2) ->
      let res, metas = check_recursive_cond ~current c1 c2 metas in
      if res then
        let res1, metas1 = check_recursive_expr ~current e1_1 e2_1 metas in
        let res2, metas2 = check_recursive_expr ~current e1_2 e2_2 metas in
        if res1 && res2 then
          true, merge_metavar ~soft_merge:true metas1 metas2
        else
          false, MetaVarMap.empty
      else
        false, MetaVarMap.empty
    | _ -> false, MetaVarMap.empty

and check_recursive_cond ?(current=None) (cond_rule:Dba.Expr.t) cond metas =
  if is_wildcard_cond cond_rule then
    true, metas
  else if is_metavar_cond cond_rule then
    true,
    MetaVarMap.add
      (to_placeholder (get_metavar_cond cond_rule))
      (CondReif cond) metas
  else if is_placeholder_cond cond_rule then
    check_recursive_cond ~current (get_placeholder_cond cond_rule metas) cond metas
  else check_recursive_expr ~current cond_rule cond metas



let check_recursive_lhs (lhs_rule:Dba.LValue.t) (lhs:Dba.LValue.t): bool * metavarmap =
  if is_wildcard_lhs lhs_rule then
    true, MetaVarMap.empty
  else if is_metavar_lhs lhs_rule then
    true, MetaVarMap.add (to_placeholder (get_metavar_lhs lhs_rule)) (LhsReif(lhs)) MetaVarMap.empty
  else
    match lhs_rule, lhs with
    | Dba.(LValue.Var { name = n1; _}), Dba.(LValue.Var {name = n2; _})
         when n1=n2 -> true, MetaVarMap.empty
    | Dba.LValue.Restrict({Dba.name = n1; _}, {Interval.lo=i1; Interval.hi=j1}),
      Dba.LValue.Restrict({Dba.name = n2; _}, {Interval.lo=i2; Interval.hi=j2})
      when n1=n2 && i1=i2 && j1=j2 ->
      true, MetaVarMap.empty
    | Dba.LValue.Store(_, in1, e1), Dba.LValue.Store(_, in2, e2)
      when in1 = in2 ->
      check_recursive_expr e1 e2 MetaVarMap.empty
    | _ -> false, MetaVarMap.empty


let rec check_subterm_expr
    (exp_sub:Dba.Expr.t) (exp_sur:Dba.Expr.t) (current:Dba.Expr.t)
    ?(strict_match=false) (metas:metavarmap): bool * metavarmap =
  if is_wildcard_expr exp_sur || is_current_expr exp_sur || is_metavar_expr exp_sur then
    raise (InconsistentRule("No wildcard/current_expr/metavar symbol allowed in RHS of LHS <: RHS"))
  else if is_placeholder_expr exp_sur then
    check_subterm_expr exp_sub (get_placeholder_expr exp_sur metas) current ~strict_match metas
  else if is_current_expr exp_sub then
    check_subterm_expr current exp_sur current ~strict_match metas
  else if is_placeholder_expr exp_sub then
    check_subterm_expr (get_placeholder_expr exp_sub metas) exp_sur current ~strict_match metas
  else if is_wildcard_expr exp_sub then
    true, metas
  else if is_metavar_expr exp_sub then (* Not so sure about that... *)
    true, MetaVarMap.add (to_placeholder (get_metavar_expr exp_sub)) (ExprReif(exp_sur)) metas
  else
    match exp_sub, exp_sur with
     | Dba.(Expr.Var {name =n1; _}), Dba.(Expr.Var {name = n2; _}) when n1=n2
       -> true, metas
    | Dba.Expr.Load(_, in1, e1),  Dba.Expr.Load(_, in2, e2) when in1 = in2 ->
      check_subterm_expr e1 e2 current ~strict_match:true metas
    | Dba.Expr.Cst(`Constant, v1), Dba.Expr.Cst(`Constant, v2)
      when (
        let v1 = Bitvector.value_of v1 in
        let v2 = Bitvector.value_of v2 in
        eq_big_int v1 v2) -> true, metas
    | Dba.Expr.Unary(op1, e1), Dba.Expr.Unary(op2, e2) when op1 = op2 ->
      check_subterm_expr e1 e2 current ~strict_match:true metas
    | Dba.Expr.Binary(op1, e1_1,e1_2), Dba.Expr.Binary(op2, e2_1,e2_2) when op1 = op2 ->
      let res1, metas1 = check_subterm_expr e1_1 e2_1 current ~strict_match:true metas in
      let res2, metas2 = check_subterm_expr e1_2 e2_2 current ~strict_match:true metas in
      if res1 && res2 then
        true, merge_metavar ~soft_merge:true metas1 metas2
      else
        false, MetaVarMap.empty

    | Dba.Expr.Ite(c1, e1_1, e1_2),  Dba.Expr.Ite(c2, e2_1, e2_2) ->
      let res, metas = check_subterm_expr c1 c2 current metas in
      if res then
        let res1, metas1 = check_subterm_expr e1_1 e2_1 current ~strict_match:true metas in
        let res2, metas2 = check_subterm_expr e1_2 e2_2 current ~strict_match:true metas in
        if res1 && res2 then
          true, merge_metavar ~soft_merge:true metas1 metas2
        else
          false, MetaVarMap.empty
      else
        false, MetaVarMap.empty
    | _ ->
      begin
        if strict_match then
          false, metas
        else
          match exp_sur with
          | Dba.Expr.Load(_, _, e)
          | Dba.Expr.Unary(_, e) ->
            check_subterm_expr exp_sub e current ~strict_match metas
          | Dba.Expr.Binary(_, e1, e2) ->
            let res1, metas1 = check_subterm_expr exp_sub e1 current ~strict_match metas in
            let res2, metas2 = check_subterm_expr exp_sub e2 current ~strict_match metas in
            if res1 || res2 then
              true, merge_metavar ~soft_merge:true metas1 metas2
            else
              false, metas
          | Dba.Expr.Ite(c1, e1_1, e1_2) ->
            let res, metas = List.fold_left (fun (r_acc,m_acc) e ->
                let r, m = check_subterm_expr exp_sub e current ~strict_match metas in
                r_acc||r, merge_metavar ~soft_merge:true m m_acc
              ) (false,metas) [c1] in
            let res1, metas1 = check_subterm_expr exp_sub e1_1 current ~strict_match metas in
            let res2, metas2 = check_subterm_expr exp_sub e1_2 current ~strict_match metas in
            if res || res1 || res2 then
              true, merge_metavar ~soft_merge:true metas (merge_metavar ~soft_merge:true metas1 metas2)
            else
              false, MetaVarMap.empty
          | _ -> false, MetaVarMap.empty
      end



let match_instruction (pred:inst_pred) instr: bool * metavarmap =
  match pred with
  | InstWildcard -> true, MetaVarMap.empty
  | InstPattern instr_rule ->
    begin match instr_rule, instr with
      | Dba.Instr.Assign(rlhs, rexp ,_),Dba.Instr.Assign(lhs, exp, _) ->
        let res, metalhs = check_recursive_lhs rlhs lhs in
        if res then
          let resexp, metaexp =
            check_recursive_expr ~current:None rexp exp MetaVarMap.empty in
          if resexp then
            true, merge_metavar metalhs metaexp
          else
            false, MetaVarMap.empty
        else
          false, MetaVarMap.empty
      | Dba.Instr.DJump(e1,_), Dba.Instr.DJump(e2,_) ->
        check_recursive_expr ~current:None e1 e2 MetaVarMap.empty
      | Dba.Instr.If(c1, _, _), Dba.Instr.If(c2 , _, _) ->
        check_recursive_cond ~current:None c1 c2 MetaVarMap.empty
      | _ -> false, MetaVarMap.empty
    end

let match_expresssion (pred:exp_pred) (metas:metavarmap) (e:Dba.Expr.t) =
  match pred with
  | ExpWildcard -> true, metas
  | ExpDba(exp_rule) -> check_recursive_expr ~current:(Some e) exp_rule e metas
  | ExpSubTerm(subterm_e, englobing_e) -> check_subterm_expr subterm_e englobing_e e metas


let rec get_final_expr (e:Dba.Expr.t) (current:Dba.Expr.t) (metas:metavarmap): Dba.Expr.t =
  if is_current_expr e then
    current
  else if is_wildcard_expr e || is_metavar_expr e then
    failwith "Wildcard and metavar cannot be used in this context"
  else if is_placeholder_expr e then
    get_placeholder_expr e metas
  else
    let open Dba in
    let open Expr in
    match e with
    | Dba.Expr.Load(sz1, in1, e1)->
      get_final_expr e1 current metas
      |> load (Size.Byte.create sz1) in1
    | Dba.Expr.Unary(op, e) ->
      get_final_expr e current metas |> unary op
    | Dba.Expr.Binary(op, e1,e2) ->
      let res1 = get_final_expr e1 current metas in
      let res2 = get_final_expr e2 current metas in
      binary op res1 res2
    | Dba.Expr.Ite(c1, e1, e2) ->
      let fc = get_final_cond c1 current metas in
      let fe1 = get_final_expr e1 current metas in
      let fe2 = get_final_expr e2 current metas in ite fc fe1 fe2
    | _ -> e

and get_final_cond c current (metas:metavarmap) =
  if is_wildcard_cond c || is_metavar_cond c then
    failwith "Wildcard and metavar cannot be used in this context"
  else if is_placeholder_cond c then
    get_placeholder_cond c metas
  else get_final_expr c current metas

let rec match_sigma (pred:sigma_pred) (metas:metavarmap) (e:Dba.Expr.t) analysis =
  match pred with
  | SigmaWildcard -> true
  | SigmaUnary(Not, p) ->
    match_sigma p metas e analysis |> not

  | SigmaBinary(op, p1, p2) ->
    let r1 = match_sigma p1 metas e analysis in
    let r2 = match_sigma p2 metas e analysis in
    begin match op with
      | And -> r1 && r2
      | Or -> r1 || r2
    end
  | TaintCheck(t,e_to_chk) ->
    let is_taint_computed = analysis#is_taint_computed () in
    if not(is_taint_computed) then
      failwith "Policy imply taint checks but taint not computed"
    else
      let taint_engine = analysis#get_taint () in
      let infos = analysis#get_current_concrete_infos () in
      let final_expr = get_final_expr e_to_chk e metas in
      let taint = taint_engine#expr_to_taint final_expr infos in
      Logger.debug ~level:2 "[Taint: %s (expected %s)]"
        (Taint_types.taint_to_string taint) (Taint_types.taint_to_string t) ;
      taint = t
(* taint_engine#is_tainted taint *)


module Printer = Dba_printer.EICUnicode

open Format
let pp_locpred fmt (pred:loc_pred) =
  match pred with
  | LocWildcard -> fprintf fmt "*"
  | LocSet l ->
    (Print_utils.pp_list ~sep:", ")
      (fun fmt i -> fprintf fmt "%Lx" i) fmt l
  | LocInterval(l, h) -> fprintf fmt "[%Lx..%Lx]" l h

let pp_instpred fmt pred =
  match pred with
  | InstWildcard -> fprintf fmt "*"
  | InstPattern inst ->
    Printer.pp_instruction fmt inst

let pp_exppred fmt (pred:exp_pred) =
  match pred with
  | ExpWildcard -> fprintf fmt "*"
  | ExpSubTerm(e1,e2) ->
    fprintf fmt  "%a <: %a"
      Printer.pp_bl_term e1 (* remove parens *)
      Printer.pp_bl_term e2
  | ExpDba e -> Printer.pp_bl_term fmt e

let pp_unary_op fmt = function
  | Not -> fprintf fmt "not"

let pp_binary_op fmt = function
  | And -> fprintf fmt "&&"
  | Or -> fprintf fmt "&&"

let rec pp_sigmapred fmt (pred:sigma_pred) =
  match pred with
  | SigmaWildcard -> fprintf fmt "*"
  | SigmaUnary(unop, p) ->
    fprintf fmt "%a %a"
      pp_unary_op unop pp_sigmapred p
  | SigmaBinary(bop, p1, p2) ->
    fprintf fmt "%a %a %a"
      pp_sigmapred p1
      pp_binary_op bop
      pp_sigmapred p2
  | TaintCheck(taint, expr) ->
    fprintf fmt "T(%s, %a)"
      (Taint_types.taint_to_string taint)
      Printer.pp_bl_term expr (* no parens *)

let pp_action fmt ((act, _):cs_action) =
  match act with
  | Conc -> fprintf fmt "𝑪"
  | Symb -> fprintf fmt "𝑺"
  | KeepOrConc -> fprintf fmt "𝑷𝒄"
  | KeepOrSymb -> fprintf fmt "𝑷𝒔"

let pp_rule fmt rule =
  fprintf fmt "%a :: %a :: %a :: %a ⟹ %a"
    pp_locpred rule.loc_p
    pp_instpred rule.inst_p
    pp_exppred rule.exp_p
    pp_sigmapred rule.sigma_p
    pp_action rule.action


let eval_expr_policy (policy:policy) li (e:Dba.Expr.t) analysis: cs_action =
  let caddr = Dba_types.Statement.location li
  and instr = Dba_types.Statement.instruction li in
  let addr = Dba_types.Caddress.base_value caddr in
  let status = ref (KeepOrSymb,"") in
  Logger.debug ~level:5 "--- Policy exp: %a ---"
    Printer.pp_bl_term e; (* no parens *)
  let policy = match policy with [] -> [default_rule] | _ -> policy in
  try
    List.iter (fun rule ->
        try
          Logger.debug ~level:5 "Rule:%a " pp_rule rule;
          let addr = Virtual_address.to_int64 addr in
          if match_location rule.loc_p addr e then
            let do_match, metas = match_instruction rule.inst_p instr in
            if do_match then
              let do_match, metas = match_expresssion rule.exp_p metas e in
              if do_match then
                if match_sigma rule.sigma_p metas e analysis then
                  (status := rule.action;
                   raise Rule_match)
                else Logger.debug ~level:5 " KO(sigma)"
              else Logger.debug ~level:5 " KO(exp)"
            else Logger.debug ~level:5 " KO(inst)"
          else Logger.debug ~level:5 " KO(loc)"
        with
        | PlaceHolderNotFound _ ->
          Logger.warning ~level:1 "PlaceHolderNotFound caught! Skip rule.."; () (* TODO: Do something ! *)
        | InvalidPlaceHolderType ->
          Logger.warning ~level:1 "InvalidPlaceHolderType caught! Skip rule.."; () (* TODO: Do something ! *)
      ) policy;
    failwith "No matching rules..."
  with Rule_match -> (* Exception to break List iteration if match found *)
    Logger.debug ~level:5 " OK";
    !status

let print_policy (policy:policy): unit =
  (* TODO: Print the policy using logger formatters ! *)
  let padd max size = String.make (max-size) ' ' in
  let is_last r =
    match r.loc_p,r.inst_p,r.exp_p,r.sigma_p with
    | LocWildcard,InstWildcard,ExpWildcard,SigmaWildcard -> true
    | _ -> false in
  let maxloc, maxinst, maxexp, maxsigma =
    List.fold_left (fun (locl,instl,expl,sigl) rule ->
        let sloc = String.length  (asprintf "%a" pp_locpred rule.loc_p) in
        let sinst = String.length (asprintf "%a" pp_instpred rule.inst_p) in
        let sexp = String.length  (asprintf "%a" pp_exppred rule.exp_p) in
        let ssig = String.length  (asprintf "%a" pp_sigmapred rule.sigma_p) in
        max sloc locl, max sinst instl, max sexp expl, max ssig sigl
      ) (0,0,0,0) policy
  in
  let size = (List.length policy) -1 in
  List.iteri (fun i rule ->
      let loc    = asprintf "%a" pp_locpred rule.loc_p in
      let inst   = asprintf "%a" pp_instpred rule.inst_p in
      let exp    = asprintf "%a" pp_exppred rule.exp_p in
      let sigma  = asprintf "%a" pp_sigmapred rule.sigma_p in
      let action = asprintf "%a" pp_action rule.action in
      let _sl, _si, _se, _ss = String.length loc, String.length inst,
                               String.length exp, String.length sigma in
      if i = size && is_last rule then
        let sl, si, se, ss =
          String.length loc, String.length inst,
          String.length exp, String.length sigma in
        if i = size && is_last rule then
          Logger.result "default%s ⟹ %s"
            (padd (maxloc+maxinst+maxexp+maxsigma+12) 7) action
        else
          Logger.result "%s%s :: %s%s :: %s%s :: %s%s ⟹ %s@" loc
            (padd maxloc sl) inst (padd maxinst si) exp (padd maxexp se)
            sigma (padd maxsigma ss) action
    ) policy




let _parse_policy (buf:Lexing.lexbuf): policy =
  try
    Policy_parser.policy Policy_token.token buf
  with
  | Failure s ->
    assert (s = "lexing: empty token");
    let pos = (Lexing.lexeme_end_p buf) in
    let l = pos.Lexing.pos_lnum in
    let c = (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) in
    Logger.error "Lexing error at line %d, character %d." l c;
    exit 1
  | Parsing.Parse_error ->
    let pos = (Lexing.lexeme_end_p buf) in
    let w = (Lexing.lexeme buf) in
    let l = pos.Lexing.pos_lnum in
    let c = (pos.Lexing.pos_cnum - pos.Lexing.pos_bol - 1) in
    Logger.error "Parse error at word \"%s\", line %d, character %d."
      w l c;
    exit 1

let parse_policy_from_string_list (policy:string list): policy =
  let data = List.fold_left (fun acc i -> acc^"\n"^i) "" policy in
  let lexbuf = Lexing.from_string data in
  _parse_policy lexbuf

let parse_policy_from_file (fname:string): policy =
  try
    Logger.debug ~level:1 "Load policy %s..." fname;
    let ic = open_in fname in
    let lexbuf = Lexing.from_channel ic in
    _parse_policy lexbuf
  with Sys_error _ -> failwith "Policy file not found"

let read_raw_policy (name:string): string list =
  let fd = open_in name in
  let pol = ref [] in
  try
    while true do
      let line = input_line fd in
      pol := line::!pol
    done;
    failwith "impossible"
  with End_of_file ->
    begin
      close_in fd;
      List.rev !pol
    end
;;
