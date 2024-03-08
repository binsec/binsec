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
  open Dba
  open Parse_helpers

  let unknown_successor = -1

  let default_endianness = Utils.get_opt_or_default Machine.LittleEndian

  let mk_declaration (tag:Var.Tag.t) name size =
     Declarations.add name size tag;
     let bitsize = Size.Bit.create size in
     Dba.LValue.var name ~bitsize ~tag


  let _dummy_addr = Virtual_address.create 0

%}

%token PLUS MINUS STAR STAR_U STAR_S SLASH_U SLASH_S
%token MODU MODS UNDEF SOK SKO ASSERT FROMFILE FROM FILE
%token ASSUME NONDET AT
%token ENTRYPOINT /* ENDIANNESS BIG LITTLE */
%token AND OR XOR NOT
%token CONCAT COLON SEMICOLON COMMA DOT DOTDOT
%token LSHIFT RSHIFTU RSHIFTS LROTATE RROTATE
%token EXTU EXTS
%token INFER SUPER
%token MIN MAX BSWAP
%token EQUAL NEQ LEU LES LTU LTS GEU GES GTU GTS
%token LBRACE RBRACE LPAR RPAR LBRACKET RBRACKET RBRACKETS RBRACKETU
%token ARROW ARROWINV STOP
%token ASSIGN TRUE FALSE IF THEN ELSE GOTO
%token ANNOT CALLFLAG AS
%token RETURNFLAG
%token FLAG TEMPORARY REGISTER VAR TEMPTAG FLAGTAG
%token ENUMERATE REACH CUT CONSEQUENT ALTERNATIVE ALTERNATE UNCONTROLLED
%token UNIMPLEMENTED UNDEFINED EOF

%token <string> INT
%token <string> IDENT TMP
%token <string> HEXA
%token <string> BIN
%token <string> STRING

%nonassoc LBRACE
%nonassoc ELSE
%left EQUAL NEQ LEU LES LTU LTS GEU GES GTU GTS
%left CONCAT
%left LSHIFT RSHIFTU RSHIFTS LROTATE RROTATE
%left PLUS MINUS
%left STAR STAR_U STAR_S SLASH_U SLASH_S MODU MODS
%left OR
%left XOR
%left AND

%nonassoc NOT

%start expr_eof
%type <Dba.Expr.t> expr_eof

%start dba
%type <Dba_types.program> dba

%start dhunk_eof
%type <Dhunk.t> dhunk_eof

%start instruction_eof
%type <Dba.Instr.t> instruction_eof

%start dhunk_substitutions_eof
%type <(Loader_utils.Binary_loc.t * Dhunk.t) list> dhunk_substitutions_eof

%start body
%type <(Dba_types.Caddress.Map.key * Dba.Instr.t) list> body

%start decoder_msg
%type <(string * Parse_helpers.Message.Value.t)  list * (Dba_types.Caddress.Map.key * Dba.Instr.t) list> decoder_msg

%start decoder_base
%type <(string * Parse_helpers.Message.Value.t)  list> decoder_base

%start patchmap
%type <Binstream.t Virtual_address.Map.t> patchmap

%start initialization
%type <Parse_helpers.Initialization.t list> initialization

%start directives
%type <Directive.t list> directives

%%

bag_of(LDELIM, RDELIM, SEP, ELT):
| delimited(LDELIM, separated_list(SEP, ELT), RDELIM) { $1 }
;

set_of(X):
| v=bag_of(LBRACE, RBRACE, COMMA, X); { v }
;

dba:
 | config=config;
   decls=list(terminated(declaration, SEMICOLON));
   initialization=list(terminated(assignment, SEMICOLON));
   instructions=body;
   { Mk.program initialization config decls instructions }
;

value:
| HEXA     { Message.Value.vint $1}
| BIN      { Message.Value.vint $1}
| STRING   { Message.Value.vstr $1}
| INT      { Message.Value.vint $1}
;

kv:
| LPAR key=IDENT; DOT v=value; RPAR { key, v }
;

decoder_base:
| opcode=kv; mnemonic=kv; address=kv; size = kv;
    { [opcode; mnemonic; address; size;]  (* Actually the order is not important *) }
| address=kv; LPAR UNDEFINED RPAR EOF
    { [ address ] }
;

decoder_msg:
| base=decoder_base; instructions=body;
    { base, instructions }
| base=decoder_base; LPAR UNIMPLEMENTED RPAR EOF
    { base,
      match base with
      | [ _, Parse_helpers.Message.Value.Int addr; _; _;
	  _, Parse_helpers.Message.Value.Str mnemonic ] ->
	 [ Dba_types.Caddress.of_virtual_address
	     (Virtual_address.of_bigint addr),
	   Dba.Instr.stop (Some (Dba.Unsupported mnemonic)) ]
      | _ -> assert false }

;

body:
| b=list(localized_instruction); EOF { b }
;

config:
| entry=entry; /* addrsize endianness */  { entry }
;

entry:
 | ENTRYPOINT COLON addr=address; { addr }
;

/* addrsize: */
/*  | WORDSIZE COLON value=INT;    { Machine.Word_size.set (int_of_string value) } */
/* ; */

/* endianness: */
/*  | ENDIANNESS COLON BIG    { Dba_types.set_endianness Machine.BigEndian } */
/*  | ENDIANNESS COLON LITTLE { Dba_types.set_endianness Machine.LittleEndian } */
/* ; */



%inline specific_declaration_kwd:
| TEMPORARY { mk_declaration Var.Tag.Temp }
| FLAG      { mk_declaration Var.Tag.Flag }
| REGISTER  { mk_declaration Var.Tag.Empty }
;

declaration:
| VAR id=IDENT; COLON size=INT; tags=option(tags);
  { mk_declaration (match tags with None -> Var.Tag.Empty | Some t -> t)  id (int_of_string size) }
| apply=specific_declaration_kwd; id=IDENT; COLON; size=INT;
  { apply id (int_of_string size) }
;

%inline tags:
 | TEMPTAG { Var.Tag.Temp }
 | FLAGTAG { Var.Tag.Flag }
;

size_annot:
| INFER size=INT; SUPER { int_of_string size }
;

localized_instruction:
| addr=address; instr=instruction;
  { Mk.checked_localized_instruction addr instr }
;

%inline addr_annot:
| addr=preceded(SEMICOLON, addressOption); { addr }
;

jump_annotation:
| ANNOT CALLFLAG addr=address;
  { Dba.Call addr }
| ANNOT RETURNFLAG             { Dba.Return }
;


static_target:
| addr=address; { Dba.Jump_target.outer addr }
| label=INT;    { Dba.Jump_target.inner (int_of_string label) }
;


static_jump:
| t=static_target; tag=ioption(jump_annotation);
  { Dba.Instr.static_jump t ?tag  }
;

jump_target:
| sj=static_jump;
  { sj }
| e=expr; tag=option(jump_annotation);
  { match e with
    | Dba.Expr.Cst bv ->
       let vaddr = Virtual_address.of_bitvector bv in
       let caddr = Dba_types.Caddress.block_start vaddr in
       let target = Dba.Jump_target.outer caddr in
       Dba.Instr.static_jump target ?tag
    | _ ->  Dba.Instr.dynamic_jump e ?tag }
;

%inline stop_annot:
| SKO { Dba.KO }
| SOK { Dba.OK }
;

rvalue:
| e=expr; { fun lv -> Dba.Instr.assign lv e }
| UNDEF   { Dba.Instr.undefined }
| NONDET
  { fun lv -> Dba.Instr.non_deterministic lv }
| UNCONTROLLED
  { fun lv -> Dba.Instr.non_deterministic lv }
;

assignment:
| lvalue=lvalue; ASSIGN f_rv=rvalue;
  { f_rv lvalue unknown_successor}
;

either(X,Y):
| X { $1 }
| Y { $1 }
;

interval_or_set:
| LBRACKET e1=expr; either(DOTDOT,COMMA); e2=expr; RBRACKETS
  { Initialization.Signed_interval(e1,e2) }
| LBRACKET e1=expr; either(DOTDOT,COMMA); e2=expr; RBRACKETU
  { Initialization.Unsigned_interval(e1,e2) }
| LBRACE args=separated_nonempty_list(COMMA,expr) RBRACE
  { Initialization.Set args }
;

%inline fromfile:
| FROMFILE {}
| FROM FILE {}
;

as_annotation:
| AS id=IDENT; { id }
;

initialization_assignment:
| a=assignment;              { Initialization.from_assignment a }
| v=address_lvalue; fromfile { Initialization.from_store v }
| lvalue=lvalue; ASSIGN; is=interval_or_set; idopt=ioption(as_annotation);
  { Initialization.assign ?identifier:idopt lvalue is }
| lv=lvalue;  { Initialization.universal lv }
;

initialization_directive:
| uncontrolled=boption(UNCONTROLLED); init=initialization_assignment;
    { Initialization.set_control (not uncontrolled) init }
| ASSUME e=expr;
    { Initialization.assume e }
;

initialization:
| v=list(terminated(initialization_directive, SEMICOLON)); EOF { v }
;

annotable_instruction:
| assign=assignment; { assign }
| ASSERT condition=delimited(LPAR, cond, RPAR);
  { Dba.Instr._assert condition unknown_successor }
| ASSUME condition=delimited(LPAR, cond, RPAR);
  { Dba.Instr.assume condition unknown_successor }
;

explicit_instruction:
| GOTO jump=jump_target; { jump }
| IF condition=cond;  GOTO st=static_target; ELSE GOTO next=INT;
  { Dba.Instr.ite condition st (int_of_string next) }
| STOP sannot=option(stop_annot);
  { Dba.Instr.stop sannot }
;

instruction:
| instr=annotable_instruction; addr=addr_annot;
  { Dba_types.Instruction.set_successor instr addr }
| instr=explicit_instruction; { instr }
;

instruction_eof:
| l=instruction; EOF {l}

labelled_instruction:
| label=INT; COLON; instr=instruction; { (int_of_string label,instr)}

dhunk:
| l=list(labelled_instruction); {Dhunk.of_labelled_list l}
;

dhunk_eof:
| l=dhunk; EOF { l }
;

dhunk_substitution:
| addr=binary_loc; ARROW; dh=dhunk
  { (addr, dh)}
;

dhunk_substitutions_eof:
| l=list(dhunk_substitution); EOF {l}

%inline addressOption:
| GOTO INT { int_of_string $2 }
|          { cur_address () }
;

%inline store_annotation:
| ARROW    { Machine.BigEndian }
| ARROWINV { Machine.LittleEndian }
;

address_lvalue:
| AT LBRACKET
  e=expr; end_opt=ioption(preceded(COMMA,store_annotation));
  COMMA size=INT;  RBRACKET
  { let sz = int_of_string size |> Size.Byte.create in
    let endianness = default_endianness end_opt in
    Dba.LValue.store sz endianness e
  }
;

lvalue:
| id=IDENT; sz_opt=option(size_annot);
  { let bitsize = Utils.get_opt_or_default 1 sz_opt |> Size.Bit.create in
    Dba.LValue.var id ~bitsize }
| id=IDENT; sz_opt=option(size_annot); offs=offsets;
  { let bitsize = Utils.get_opt_or_default 1 sz_opt |> Size.Bit.create in
    let lo, hi = offs in Dba.LValue._restrict id bitsize lo hi }
| id=TMP; sz_opt=option(size_annot);
  { let bitsize = Utils.get_opt_or_default 1 sz_opt |> Size.Bit.create in
    Dba.LValue.temporary id bitsize }
| id=TMP; sz_opt=option(size_annot); offs=offsets;
    { let bitsize = Utils.get_opt_or_default 1 sz_opt |> Size.Bit.create in
      let var = Dba.Var.temporary id bitsize in
    let lo, hi = offs in Dba.LValue.restrict var lo hi }
| v=address_lvalue { v };
;

constant:
| value=INT; size=size_annot;
  { Bitvector.create (Z.of_string value) size }
| value=HEXA;
| value=BIN;
  { Bitvector.of_string value }
;

%inline offsets:
| LBRACE loff=INT; COMMA roff=INT; RBRACE
  { int_of_string loff, int_of_string roff }
| LBRACE boff=INT; RBRACE
  { let n = int_of_string boff in n, n}
;

expr_eof:
| e=expr; EOF { e }
;

expr:
| LPAR e=expr; RPAR { e }

| id=IDENT; sz_opt=ioption(size_annot);
  { let sz = Utils.get_opt_or_default 1 sz_opt in
    Dba.Expr.var id sz }

| id=TMP; sz_opt=ioption(size_annot);
  { let sz = Utils.get_opt_or_default 1 sz_opt in
    Dba.Expr.temporary id ~size:sz }

| cst=constant;
  { Dba.Expr.constant cst }

| e=expr;  offs=offsets;
  { let lo, hi = offs in Dba.Expr.restrict lo hi e }
| LPAR cst=preceded(COMMA,constant); RPAR
  { Dba.Expr.constant cst }

| AT
  LBRACKET e=expr; end_opt=ioption(preceded(COMMA, store_annotation));
  COMMA size=INT; RBRACKET
  { let size = int_of_string size |> Size.Byte.create in
    let endianness = default_endianness end_opt in
    Dba.Expr.load size endianness e }
 | NOT e=expr;       { Dba.Expr.lognot e }
 | MINUS e=expr;     { Dba.Expr.uminus e }
 | EXTU e=expr; size=INT;
   { Dba.Expr.uext (int_of_string size) e }
 | EXTS e=expr; size=INT;
   { Dba.Expr.sext (int_of_string size) e }
 | IF c=expr; THEN  then_e=expr; ELSE else_e=expr;
   { Dba.Expr.ite c then_e else_e }
 | le=expr; bop=bin_op; re=expr;
    { Dba.Expr.binary bop le re }
 | MIN LPAR e1=expr; COMMA e2=expr; RPAR
    { Dba.Expr.ite (Dba.Expr.ult e1 e2) e1 e2 }
 | MAX LPAR e1=expr; COMMA e2=expr; RPAR
    { Dba.Expr.ite (Dba.Expr.ult e1 e2) e2 e1 }
  | BSWAP e=expr;
    { Dba_utils.Expr.bswap e }
;

%inline bin_op:
 | MODU    { Dba.Binary_op.ModU }
 | MODS    { Dba.Binary_op.ModS }
 | OR      { Dba.Binary_op.Or }
 | AND     { Dba.Binary_op.And }
 | XOR     { Dba.Binary_op.Xor }
 | CONCAT  { Dba.Binary_op.Concat }
 | EQUAL   { Dba.Binary_op.Eq }
 | NEQ     { Dba.Binary_op.Diff }
 | LEU     { Dba.Binary_op.LeqU }
 | LTU     { Dba.Binary_op.LtU  }
 | GEU     { Dba.Binary_op.GeqU }
 | GTU     { Dba.Binary_op.GtU }
 | LES     { Dba.Binary_op.LeqS }
 | LTS     { Dba.Binary_op.LtS }
 | GES     { Dba.Binary_op.GeqS }
 | GTS     { Dba.Binary_op.GtS }
 | PLUS    { Dba.Binary_op.Plus }
 | MINUS   { Dba.Binary_op.Minus }
 | STAR
 | STAR_U  { Dba.Binary_op.Mult}
 | STAR_S  { Dba.Binary_op.Mult}
 | SLASH_U { Dba.Binary_op.DivU}
 | SLASH_S { Dba.Binary_op.DivS}
 | LSHIFT  { Dba.Binary_op.LShift }
 | RSHIFTU { Dba.Binary_op.RShiftU}
 | RSHIFTS { Dba.Binary_op.RShiftS }
 | LROTATE { Dba.Binary_op.LeftRotate }
 | RROTATE { Dba.Binary_op.RightRotate }


%inline cond:
 | TRUE     { Dba.Expr.one }
 | FALSE    { Dba.Expr.zero }
 | e=expr;  { e }
;

address:
   /* | LPAR INT INFER INT SUPER COMMA INT RPAR {
     let size = int_of_string $4 in
     let bigint = Z.of_string $2 in
     let id = int_of_string $7 in
     if size = Dba_types.address_size then ((bigint, Dba_types.address_size), id)
     else
       let message =
         Format.asprintf "addresses must be on %d bits%!" Dba_types.address_size in
       failwith message
   } */
 | LPAR bv=constant; nid=preceded(COMMA, INT); RPAR
  {
    let id = int_of_string nid in
    let vaddr = Virtual_address.of_bitvector bv in
    let addr = Dba_types.Caddress.create vaddr id in
    incr_address addr;
    addr
 }

integer:
 | n=HEXA;
 | n=BIN;
 | n=INT;    { int_of_string n }
 ;

opcode:
 | s=STRING  { Binstream.of_bytes s }
 | integers=delimited(LPAR, nonempty_list(integer), RPAR);
   { Binstream.of_list integers }
;

patch:
 | LPAR address=integer; opc=opcode;  RPAR { address, opc }
;

patchmap:
 | patches=list(patch); EOF { mk_patches patches }
;


%inline integer_argument:
 | times=ioption(delimited(LPAR,integer,RPAR)); { times }
;

%inline consequent:
 | PLUS
 | CONSEQUENT
 | THEN {}
;
%inline alternative:
 | MINUS
 | ALTERNATIVE
 | ELSE {}
;

/* Directives for analyses: SE, interpretation  */
directive:
 | REACH times=integer_argument; guard=option(preceded(IF,expr));
    { let n = Utils.get_opt_or_default 1 times in
      Directive.reach ~n ?guard ~actions:[Directive.Action.Print_model]}
 | REACH STAR
    { Directive.reach_all ~guard:Dba.Expr.one
			  ~actions:[Directive.Action.Print_model] }
 | ENUMERATE   e=expr; times=integer_argument;
   { let n = Utils.get_opt_or_default 1 times in Directive.enumerate ~n e }
 | ASSUME e=expr;
   { Directive.assume e }
 | CUT guard=option(preceded(IF,expr));
   { Directive.cut ?guard }
 | consequent alternate=boption(ALTERNATE);
   { Directive.choose_consequent ~alternate }
 | alternative alternate=boption(ALTERNATE);
   { Directive.choose_alternative ~alternate }
;

bin_loc_with_id:
 | id=IDENT; { Loader_utils.Binary_loc.name id }
 | bloc=bin_loc_with_id; PLUS n=integer;
    { Loader_utils.Binary_loc.offset n bloc }
 | bloc=bin_loc_with_id; MINUS n=integer;
    { Loader_utils.Binary_loc.offset (-n) bloc }
;

binary_loc:
 | address=integer
    { Loader_utils.Binary_loc.address @@ Virtual_address.create address }
 | loc=delimited(INFER, bin_loc_with_id, SUPER); { loc }
;

located_directive:
 | loc=expr; g=directive;
   { g ~loc () }
;

directives:
 | l=separated_nonempty_list(SEMICOLON, located_directive); EOF { l }
;
