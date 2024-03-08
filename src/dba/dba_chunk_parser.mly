/**************************************************************************/
/*  This file is part of BINSEC.                                          */
/*                                                                        */
/*  Copyright (C) 2016-2024                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

%{
    type k =
      | Fallthrough of (int -> Dba.Instr.t)
      | Terminator of Dba.Instr.t
      | Goto of string
      | It of Dba.Expr.t * string

    let set_label stmts label =
      let labels, instr = List.hd stmts and tl = List.tl stmts in
      ((label :: labels), instr) :: tl

    let uid =
      let i = ref 0 in
      fun () -> incr i; Printf.sprintf "%%%d" !i

    let incr e =
      Dba.Expr.add e @@ Dba.Expr.ones @@ Dba.Expr.size_of e

    let dhunk_of_list stmts =
      let block = Array.of_list stmts in
      let module H = Basic_types.String.Htbl in
      let binding = H.create 128 in
      Array.iteri (fun i (labels, _) ->
		    List.iter (fun label -> H.add binding label i) labels)
		  block;
      Dhunk.init (Array.length block)
		 (fun i ->
		   match Array.get block i |> snd with
		   | Fallthrough f -> f @@ i + 1
		   | Terminator i -> i
		   | Goto l -> Dba.Instr.static_inner_jump @@ H.find binding l
		   | It (c, l) ->
		      i + 1 |> Dba.Instr.ite c
			       @@ Dba.Jump_target.inner
			       @@ H.find binding l)
%}

%token WHILE DO
%token FOR IN TO
%token AS
%token SEMICOLON END
%token ABORT

%token <string> LABEL

%%

%public
let chunk :=
  | stmts=append(flatten(list(stmt)), iterminator);
    { dhunk_of_list stmts }

%public
let stmt :=
  | ~=label; SEMICOLON;
    { [ [ label ], Fallthrough (Dba.Instr.static_inner_jump ?tag:None) ] }
  | l=ioption(label); ~=loc; ASSIGN; ~=iexpr; AS; id=IDENT;
    { let size = Dba.LValue.size_of loc in
      let expr = match iexpr with
	| Right e -> e
	| Left i -> Dba.Expr.constant (Bitvector.create i size) in
      let name, size = match id with
	| name, -1 -> name, size
	| name, size' -> assert (size = size'); name, size in
      let var = E.lookup name size in
      let evar =
	if Dba.LValue.size_of var <> size then Dba.Expr.var name size
	else Dba.LValue.to_expr var in
      [ Option.to_list l, Fallthrough (Dba.Instr.assign var expr);
	[], Fallthrough (Dba.Instr.assign loc evar) ] }
  | l=ioption(label); ~=loc; ASSIGN; NONDET; AS; id=IDENT;
    { let size = Dba.LValue.size_of loc in
      let name, size = match id with
	| name, -1 -> name, size
	| name, size' -> assert (size = size'); name, size in
      let var = E.lookup name size in
      let evar =
	if Dba.LValue.size_of var <> size then Dba.Expr.var name size
	else Dba.LValue.to_expr var in
      [ Option.to_list l, Fallthrough (Dba.Instr.non_deterministic var);
	[], Fallthrough (Dba.Instr.assign loc evar) ] }
  | l=ioption(label); ~=single; option(SEMICOLON);
    { [ Option.to_list l, single ] }
  | l=ioption(label); ~=block; END;
    { Option.fold ~none:block ~some:(set_label block) l }

let single :=
  | ~=fallthrough;
    { Fallthrough fallthrough }
  | GOTO; target=LABEL;
    { Goto target }

let id := id=IDENT;
    { let name, size = id in
      E.lookup name size }

let block :=
  | IF; ~=bool; THEN; t=flatten(nonempty_list(stmt));
    { let target = uid () in
      List.concat
	[ [ [], It (Dba.Expr.lognot bool, target) ];
	  t;
	  [ [ target ], Fallthrough Dba.Instr.static_inner_jump ] ] }
  | IF; ~=bool; THEN; t=flatten(nonempty_list(stmt));
    ELSE; e=flatten(nonempty_list(stmt));
    { let target = uid () and target' = uid () in
      List.concat
	[ [ [], It (Dba.Expr.lognot bool, target) ];
	  t;
	  [ [], Goto target' ];
	  set_label e target;
	  [ [ target' ], Fallthrough Dba.Instr.static_inner_jump ] ] }
  | WHILE; ~=bool; DO; d=flatten(nonempty_list(stmt));
    { let target = uid () and target' = uid () in
      (*
      List.concat
	[ [ [ target ], It (Dba.Expr.lognot bool, target') ];
	  d;
	  [ [], Goto target ];
	  [ [ target' ], Fallthrough Dba.Instr.static_inner_jump ] ] *)
      (* TOFIX #WEIRD: as SSE favour the "else" branch, we invert the
      construction to exit the loop faster. *)
      let target'' = uid () in
      List.concat
	[ [ [ target ], It (bool, target') ];
	  [ [], Goto target'' ];
	  set_label d target';
	  [ [], Goto target ];
	  [ [ target'' ], Fallthrough Dba.Instr.static_inner_jump ] ] }
  | FOR; var=id; IN; init=iexpr; TO; upto=iexpr; DO;
    d=flatten(nonempty_list(stmt));
    { let evar = Dba.LValue.to_expr var in
      let size = Dba.LValue.size_of var in
      let init = match init with
	| Right i -> assert (Dba.Expr.size_of i = size); i
	| Left i -> Dba.Expr.constant @@ Bitvector.create i size in
      let upto = match upto with
	| Right u -> assert (Dba.Expr.size_of u = size); u
	| Left u -> Dba.Expr.constant @@ Bitvector.create u size in
      let target = uid () and target' = uid () in
       (* TOFIX #WEIRD: as SSE favour the "else" branch, we invert the
      construction to exit the loop faster. *)
      let target'' = uid () in
      List.concat
	[ [ [], Fallthrough (Dba.Instr.assign var init) ];
	  [ [ target ], It (Dba.Expr.sle evar upto, target') ];
	  [ [], Goto target'' ];
	  set_label d target';
	  [ [], Fallthrough (Dba.Instr.assign var (incr evar)) ];
	  [ [], Goto target ];
	  [ [ target'' ], Fallthrough Dba.Instr.static_inner_jump ] ] }
  | CASE; ~=expr; IS; cases=nonempty_list(case);
    default=option(preceded(pair(ANY, COLON), flatten(nonempty_list(stmt))));
    { let size = Dba.Expr.size_of expr in
      let rec aux cases next stmts =
	match cases with
	| [] -> stmts
	| (value, body) :: cases ->
	   let label = uid () in
	   aux cases label @@
	     ([ label ], It (Dba.Expr.diff value expr, next))
	     :: List.rev_append body stmts in
      let tail = uid () in
      let rev_stmts =
	List.rev_map
	  (fun (value, body) ->
	    let value = Dba.Expr.constant
			@@ match value with
			   | Left i -> Bitvector.create i size
			   | Right b -> assert (Bitvector.size_of b = size);
					b in
	    value, (([], Goto tail) :: List.rev body)) cases in
      let tail_stmts = [ [ tail ], Fallthrough Dba.Instr.static_inner_jump ] in
      let default, default_stmts = match default with
	| None -> tail, tail_stmts
	| Some stmts ->
	   let default = uid () in
	   let default_stmts = List.rev_append stmts tail_stmts in
	   default, set_label default_stmts default in
      aux rev_stmts default default_stmts }

let case :=
  | ~=iconst; COLON; e=flatten(nonempty_list(stmt));
    { iconst, e }

let label ==
  | label=terminated(LABEL, COLON); { label }

let iterminator ==
  | ~=terminator; { [ [], Terminator terminator ] }
  | ABORT;
    { [ [], Fallthrough (Dba.Instr._assert Dba.Expr.zero);
	[], Terminator (Dba.Instr.stop None) ] }
