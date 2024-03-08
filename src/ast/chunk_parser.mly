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

    let uid =
      let i = ref 0 in
      fun () -> incr i; Printf.sprintf "%%%d" !i

%}

%token WHILE DO
%token FOR IN TO
%token AS
%token SEMICOLON END NEWLINE
%token ABORT
%token RETURN

%token <string> LABEL

%%

%public
let chunk :=
  | stmts=flatten(nonempty_list(stmt));
    { stmts }
  | stmts=append(append(flatten(list(stmt)),
                        terminated(control, separator)),
                 loption(append(labelled, chunk)));
    { stmts }

%public
let stmt :=
  | ~=label; separator;
    { [ Instr.label label ] }
  | label=ioption(label); instr=instr_or_block; separator;
    { match label with
      | None -> instr
      | Some ident -> Instr.label ident :: instr
    }

let separator :=
  | SEMICOLON;
    {}
  | NEWLINE;
    {}

let labelled :=
  | ~=label;
    { [ Instr.label label ] }

let instr_or_block :=
  | ~=instr;
    { instr }
  | ~=block; END;
    { block }

%public
let instr :=
  | ~=loc; ASSIGN; ~=expr; AS; id=IDENT;
    { let size = LValue.size_of loc in
      let expr = to_expr size expr in
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
  | ~=terminator; { [ terminator ] }
  | ABORT;
    { [ Instr.dynamic_assert Expr.zero; Instr.halt ] }
  | RETURN; value=option(expr);
    { let value = Option.map (to_expr Env.wordsize) value in
      Instr.of_dhunk (Isa_helper.make_return ?value ()) }

let id := id=IDENT;
    { let name, size = id in
      Env.lookup name size }

let block :=
  | IF; ~=bool; THEN; t=chunk;
    { let target = uid () in
      List.concat
	[ [ Instr.conditional_jump (Expr.lognot bool) target ];
	  t;
	  [ Instr.label target ] ] }
  | IF; ~=bool; THEN; t=chunk; ELSE; e=chunk;
    { let target = uid () and target' = uid () in
      List.concat
	[ [ Instr.conditional_jump (Expr.lognot bool) target ];
	  t;
	  [ Instr.goto target'; Instr.label target ];
	  e;
	  [ Instr.label target' ] ] }
  | WHILE; ~=bool; DO; d=chunk;
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
  | FOR; var=id; IN; init=expr; TO; upto=expr; DO; d=chunk;
    { let evar = LValue.to_expr var in
      let size = LValue.size_of var in
      let init = to_expr size init in
      let upto = to_expr size upto  in
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
    default=option(preceded(pair(ANY, COLON), chunk));
    { let rec aux cases next stmts =
	match cases with
	| [] -> stmts
	| (value, body) :: cases ->
	   let label = uid () in
	   aux cases label @@
	     Instr.label label
	     :: Instr.conditional_jump (to_bool (diff value expr)) next
	     :: List.rev_append body stmts in
      let tail = uid () in
      let rev_stmts =
	List.rev_map
	  (fun (value, body) ->
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
  | ~=const; COLON; e=flatten(nonempty_list(stmt));
    { const, e }

let label ==
  | label=terminated(LABEL, COLON); { label }
