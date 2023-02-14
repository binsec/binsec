/**************************************************************************/
/*  This file is part of BINSEC.                                          */
/*                                                                        */
/*  Copyright (C) 2016-2022                                               */
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

    let uid =
      let i = ref 0 in
      fun () -> incr i; Printf.sprintf "%%%d" !i

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
  | stmts=flatten(list(stmt));
    { stmts }

%public
let stmt :=
  | ~=label; SEMICOLON;
    { [ Instr.label label ] }
  | label=ioption(label); instr=instr_or_control; option(SEMICOLON);
    { match label with
      | None -> instr
      | Some ident -> Instr.label ident :: instr
    }

let instr_or_control :=
  | ~=instr;
    { instr }
  | ~=control;
    { control }

%public
let instr :=
  | ~=loc; ASSIGN; ~=iexpr; AS; id=IDENT;
    { let size = LValue.size_of loc in
      let expr = match iexpr with
	| Right e -> e
	| Left i -> Expr.constant (Bitvector.create i size) in
      let name, size = match id with
	| name, -1 -> name, size
	| name, size' -> assert (size = size'); name, size in
      let var = Env.lookup name size in
      let evar =
	if LValue.size_of var <> size then Expr.var name size
	else LValue.to_expr var in
      [ Instr.assign var expr; Instr.assign loc evar ]
    }
  | ~=loc; ASSIGN; NONDET; AS; id=IDENT;
    { let size = LValue.size_of loc in
      let name, size = match id with
	| name, -1 -> name, size
	| name, size' -> assert (size = size'); name, size in
      let var = Env.lookup name size in
      let evar =
	if LValue.size_of var <> size then Expr.var name size
	else LValue.to_expr var in
      [ Instr.nondet var; Instr.assign loc evar ] }
  | ~=fallthrough;
    { [ fallthrough ] }

let control :=
  | GOTO; target=LABEL;
    { [ Instr.goto target ] }
  | ~=block; END;
    { block }
  | ~=terminator; { [ terminator ] }
  | ABORT;
    { [ Instr.dynamic_assert Expr.zero; Instr.halt ] }

let id := id=IDENT;
    { let name, size = id in
      Env.lookup name size }

let block :=
  | IF; ~=bool; THEN; t=flatten(nonempty_list(stmt));
    { let target = uid () in
      List.concat
	[ [ Instr.conditional_jump (Expr.lognot bool) target ];
	  t;
	  [ Instr.label target ] ] }
  | IF; ~=bool; THEN; t=flatten(nonempty_list(stmt));
    ELSE; e=flatten(nonempty_list(stmt));
    { let target = uid () and target' = uid () in
      List.concat
	[ [ Instr.conditional_jump (Expr.lognot bool) target ];
	  t;
	  [ Instr.goto target'; Instr.label target ];
	  e;
	  [ Instr.label target' ] ] }
  | WHILE; ~=bool; DO; d=flatten(nonempty_list(stmt));
    { let target = uid () and target' = uid () in
      (*
      List.concat
	[ [ [ target ], It (Expr.lognot bool, target') ];
	  d;
	  [ [], Goto target ];
	  [ [ target' ], Fallthrough Instr.static_inner_jump ] ] *)
      (* TOFIX #WEIRD: as SSE favour the "else" branch, we invert the
      construction to exit the loop faster. *)
      let target'' = uid () in
      List.concat
	[ [ Instr.label target;
	    Instr.conditional_jump bool target';
	    Instr.goto target'';
	    Instr.label target' ];
	  d;
	  [ Instr.goto target; Instr.label target'' ] ] }
  | FOR; var=id; IN; init=iexpr; TO; upto=iexpr; DO;
    d=flatten(nonempty_list(stmt));
    { let evar = LValue.to_expr var in
      let size = LValue.size_of var in
      let init = match init with
	| Right i -> assert (Expr.size_of i = size); i
	| Left i -> Expr.constant @@ Bitvector.create i size in
      let upto = match upto with
	| Right u -> assert (Expr.size_of u = size); u
	| Left u -> Expr.constant @@ Bitvector.create u size in
      let target = uid () and target' = uid () in
       (* TOFIX #WEIRD: as SSE favour the "else" branch, we invert the
      construction to exit the loop faster. *)
      let target'' = uid () in
      List.concat
	[ [ Instr.assign var init;
	    Instr.label target;
	    Instr.conditional_jump (Expr.sle evar upto) target';
	    Instr.goto target'';
	    Instr.label target' ];
	  d;
	  [ Instr.assign var (Expr.succ evar);
	    Instr.goto target;
	    Instr.label target'' ] ] }
  | CASE; ~=expr; IS; cases=nonempty_list(case);
    default=option(preceded(pair(ANY, COLON), flatten(nonempty_list(stmt))));
    { let size = Expr.size_of expr in
      let rec aux cases next stmts =
	match cases with
	| [] -> stmts
	| (value, body) :: cases ->
	   let label = uid () in
	   aux cases label @@
	     Instr.label label
	     :: Instr.conditional_jump (Expr.diff value expr) next
	     :: List.rev_append body stmts in
      let tail = uid () in
      let rev_stmts =
	List.rev_map
	  (fun (value, body) ->
	    let value = Expr.constant
			@@ match value with
			   | Left i -> Bitvector.create i size
			   | Right b -> assert (Bitvector.size_of b = size);
					b in
	    value, (Instr.goto tail :: List.rev body)) cases in
      let tail_stmts = [ Instr.label tail ] in
      let default, default_stmts = match default with
	| None -> tail, tail_stmts
	| Some stmts ->
	   let default = uid () in
	   let default_stmts = List.rev_append stmts tail_stmts in
	   default, Instr.label default :: default_stmts in
      aux rev_stmts default default_stmts }

let case :=
  | ~=iconst; COLON; e=flatten(nonempty_list(stmt));
    { iconst, e }

let label ==
  | label=terminated(LABEL, COLON); { label }
