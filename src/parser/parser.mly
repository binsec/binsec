/**************************************************************************/
/*  This file is part of Binsec.                                          */
/*                                                                        */
/*  Copyright (C) 2016-2017                                               */
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
  let unknown_bitsize = Basic_types.BitSize.create 1

  let default_endianness = Utils.get_opt_or_default Dba.LittleEndian

  let mk_declaration tags name size =
     Declarations.add name size tags;
     let bitsize = Basic_types.BitSize.create size in
     Dba_types.LValue.var name ~bitsize tags
%}

%token PLUS MINUS MULTU MULTS DIVU DIVS
%token MODU MODS UNDEF SOK SKO PRINT ASSERT
%token ASSUME NONDET NONDETASSUME AT
%token CONSTANT STACK MALLOC FREE NREAD READ NWRITE WRITE
%token NEXEC EXEC ENTRYPOINT ENDIANNESS BIG LITTLE
%token AND OR XOR NOT
%token CONCAT COLON SEMICOLON COMMA DOT
%token LSHIFT RSHIFTU RSHIFTS LROTATE RROTATE
%token EXTU EXTS
%token INFER SUPER
%token EQQ NEQ LEU LES LTU LTS GEU GES GTU GTS
%token LBRACE RBRACE LPAR RPAR LBRACKET RBRACKET
%token ARROW ARROWINV STOP ALTERNATIVE
%token ASSIGN TRUE FALSE IF THEN ELSE GOTO
%token ANNOT CALLFLAG
%token WORDSIZE
%token RETURNFLAG ADDCARRY ADDOVERFLOW BEGIN END PERMISSIONS
%token FLAG TEMPORARY REGISTER VAR TEMPTAG FLAGTAG
%token EOF

%token <string> INT
%token <string> IDENT
%token <string> HEXA
%token <string> STRING

%nonassoc LBRACE
%nonassoc ELSE
%left EQQ NEQ LEU LES LTU LTS GEU GES GTU GTS
%left CONCAT
%left LSHIFT RSHIFTU RSHIFTS LROTATE RROTATE
%left PLUS MINUS
%left MULTU MULTS DIVU DIVS MODU MODS
%left OR
%left XOR
%left AND

%nonassoc NOT
%nonassoc prec_uminus

%start dba
%type <'a Dba_types.program> dba

%start body
%type <(Dba_types.Caddress.Map.key * Dba.instruction) list> body

%start decoder_msg
%type <(string * Parse_helpers.Message.Value.t)  list * (Dba_types.Caddress.Map.key * Dba.instruction) list> decoder_msg

%start decoder_base
%type <(string * Parse_helpers.Message.Value.t)  list> decoder_base

%start patchmap
%type <Basic_types.Binstream.t Dba_types.Virtual_address.Map.t> patchmap

%%

dba:
 | config=config;
   decls=list(terminated(declaration, SEMICOLON));
   permissions=option(permission_block);
   initialization=list(terminated(assignment, SEMICOLON));
   instructions=body;
   { Mk.program permissions initialization config decls instructions }
;

value:
| HEXA     { Message.Value.vhex $1}
| STRING   { Message.Value.vstr $1}
| INT      { Message.Value.vint $1}
;

kv:
| LPAR key=IDENT; DOT v=value; RPAR { key, v }
;

decoder_base:
| opcode=kv; mnemonic=kv; address=kv;
  { [opcode; mnemonic; address]  (* Actually the order is not important *) }
;

decoder_msg:
| base=decoder_base; instructions=body;
  { base, instructions }
;

body:
| b=list(localized_instruction); EOF { b }
;

config :
| entry=entry; addrsize endianness  { entry }
;

entry :
 | ENTRYPOINT COLON addr=address; { addr }
;

addrsize:
 | WORDSIZE COLON value=INT;    { Machine.Word_size.set (int_of_string value) }
;

endianness :
 | ENDIANNESS COLON BIG    { Dba_types.set_endianness BigEndian }
 | ENDIANNESS COLON LITTLE { Dba_types.set_endianness LittleEndian }
;

%inline specific_declaration_kwd:
| TEMPORARY { mk_declaration (Some Dba.Temp) }
| FLAG      { mk_declaration (Some (Flag FlgUnspecified)) }
| REGISTER  { mk_declaration None }
;

declaration:
| VAR id=IDENT; COLON size=INT; tags=option(tags);
  { mk_declaration tags id (int_of_string size) }
| apply=specific_declaration_kwd; id=IDENT; COLON; size=INT;
 { apply id (int_of_string size) }
;

%inline tags :
 | TEMPTAG { Temp }
 | FLAGTAG { Flag Dba.FlgUnspecified }
;

permission_block :
 | BEGIN PERMISSIONS permissions=list(permission); END PERMISSIONS
 { Mk.Permissions.of_list permissions }


permission :
 | region=regionnondet; COLON preds=predicates;
  { region, fst preds, snd preds }
;

predicates :
 | predicates=nonempty_list(predicate); { Mk.Predicates.of_list predicates }
;

read_permission:
 | NREAD { false }
 | READ  { true }
;

write_permission:
 | WRITE  { true }
 | NWRITE { false }
;

exec_permission:
 | EXEC  { true }
 | NEXEC { false }
;

predicate :
 | addr=cond_addr; COLON
   read=read_permission; write=write_permission; exec=exec_permission;
   { Mk.filemode addr read write exec }
;

cond_addr :
 | TRUE        { Dba.True }
 | FALSE       { Dba.False }
 | eaddr=constant_expr; { Mk.checked_cond_expr eaddr }
;

size_annot:
| INFER size=INT; SUPER { int_of_string size }
;

constant_expr :
 | WORDSIZE; { Dba.ExprVar ("\\addr", Machine.Word_size.get (), None) }
 | cst=constant;
  { Dba_types.Expr.constant cst  }
 | e=constant_expr; offs=offsets;
   { Mk.Expr.restricted e offs }
 | MINUS e=constant_expr; %prec prec_uminus
  { Dba.ExprUnary (Dba.UMinus, e) }
 | NOT e=constant_expr;
  { Dba.ExprUnary (Dba.Not, e) }
 | EXTU e=constant_expr; size=INT;
   { Dba.ExprExtU (e, int_of_string size) }
 | EXTS e=constant_expr; size=INT;
   { Dba.ExprExtS (e, int_of_string size) }
 | LPAR e=constant_expr; RPAR { e }
 | le=constant_expr; bop=bin_op; re=constant_expr;
   { Dba_types.Expr.binary bop le re }
;

%inline region:
 | CONSTANT { `Constant }
 | STACK { `Stack }
;

%inline regionnondet :
 | region=region; { region }
 | MALLOC         { Dba_types.Region.malloc (Machine.Word_size.get ())}
;

localized_instruction:
| addr=address; instr=instruction;
  { Mk.checked_localized_instruction addr instr }
;

%inline addr_annot:
| addr=preceded(SEMICOLON, addressOption); { addr }
;

jump_annotation:
| ANNOT CALLFLAG addr=address; { Dba.Call addr }
| ANNOT RETURNFLAG             { Dba.Return }
;


static_target:
| addr=address; { Dba_types.Jump_target.outer addr }
| label=INT;    { Dba_types.Jump_target.inner (int_of_string label) }
;


static_jump:
| t=static_target; tag=option(jump_annotation);
  { Dba_types.Instruction.static_jump t ~tag  }
;


jump_target:
| sj=static_jump;
  { sj }
| e=expr; tag=option(jump_annotation);
  { match e with
    | Dba.ExprCst (`Constant, bv) ->
       let caddr = Dba_types.Caddress.block_start bv in
       let target = Dba_types.Jump_target.outer caddr in
       Dba_types.Instruction.static_jump target ~tag
    | _ ->  Dba_types.Instruction.dynamic_jump e ~tag }
;


%inline stop_annot:
| SKO { Dba.KO }
| SOK { Dba.OK }
;

rvalue:
| e=expr; { fun lv -> Dba_types.Instruction.assign lv e }
| UNDEF   { Dba_types.Instruction.undefined }
| MALLOC e=delimited(LPAR, expr, RPAR);
  { fun lv -> Dba_types.Instruction.malloc lv e  }
| NONDET region=delimited(LPAR,regionnondet, RPAR);
  { fun lv -> Dba_types.Instruction.non_deterministic lv region }
;

set_of(X):
| v=delimited(LBRACE, separated_list(COMMA, X), RBRACE); { v }
;

assignment:
| lvalue=lvalue; ASSIGN frv=rvalue;
  { frv lvalue unknown_successor}
;


annotable_instruction:
| assign=assignment; { assign }
| PRINT args=separated_nonempty_list(COMMA, printarg)
  { Dba_types.Instruction.print args unknown_successor }
| FREE e=delimited(LPAR, expr, RPAR);
  { Dba_types.Instruction.free e unknown_successor }
| ASSERT condition=delimited(LPAR, cond, RPAR);
  { Dba_types.Instruction.iassert condition unknown_successor }
| ASSUME condition=delimited(LPAR, cond, RPAR);
  { Dba_types.Instruction.assume condition unknown_successor }
| NONDETASSUME LPAR lvalues=set_of(lvalue);
  COMMA condition=cond; RPAR
  { Dba_types.Instruction.non_deterministic_assume
      lvalues condition unknown_successor
  }
;

explicit_instruction:
| GOTO jump=jump_target; { jump }
| IF condition=cond;  GOTO st=static_target; ELSE GOTO next=INT;
  { Dba_types.Instruction.ite condition st (int_of_string next) }
| STOP sannot=option(stop_annot);
  { Dba_types.Instruction.stop sannot }
;

instruction:
| instr=annotable_instruction; addr=addr_annot;
   { Dba_types.Instruction.set_successor instr addr }
| instr=explicit_instruction; { instr }
;

%inline addressOption :
| GOTO INT { int_of_string $2 }
|          { !cur_address }
;

printarg:
| e=expr;   { Exp e }
| s=STRING; { Str s }
;

%inline store_annotation:
| ARROW    { BigEndian }
| ARROWINV { LittleEndian }

lvalue :
| id=IDENT; sz_opt=option(size_annot);
{ let bitsize =
    Utils.get_opt_or_default 1 sz_opt |> Basic_types.BitSize.create in
  Dba_types.LValue.var id ~bitsize None }
| id=IDENT; offs=offsets;
  { let lo, hi = offs in Dba_types.LValue.restrict id unknown_bitsize lo hi }
| AT LBRACKET
  e=expr; end_opt=ioption(preceded(COMMA,store_annotation));
  COMMA size=INT;  RBRACKET
  { let sz = int_of_string size |> Basic_types.ByteSize.create in
    let endianness = default_endianness end_opt in
    Dba_types.LValue.store sz endianness e
  }
;

constant:
| value=INT; size=size_annot;
  { Bitvector.create (Bigint.big_int_of_string value) size }
| value=HEXA;
  { Bitvector.of_hexstring value }
;

%inline offsets:
| LBRACE loff=INT; COMMA roff=INT; RBRACE
 { int_of_string loff, int_of_string roff }

expr:
| LPAR e=expr; RPAR { e }
| id=IDENT; sz_opt=ioption(size_annot);
  { let sz =
      Utils.get_opt_or_default 1 sz_opt |> Basic_types.ByteSize.create in
    Dba_types.Expr.var id sz None }

| cst=constant;
  { Dba_types.Expr.constant cst }
| e=expr;  offs=offsets;
  { let lo, hi = offs in Dba_types.Expr.restrict e lo hi }

| LPAR region=region; cst=preceded(COMMA,constant); RPAR
  { Dba_types.Expr.constant cst ~region }

| AT
  LBRACKET e=expr; end_opt=ioption(preceded(COMMA, store_annotation));
  COMMA size=INT; RBRACKET
  { let size = int_of_string size |> Basic_types.ByteSize.create in
    let endianness = default_endianness end_opt in
    Dba_types.Expr.load size endianness e }
 | NOT e=expr;       { Dba.ExprUnary (Dba.Not, e) }
 | MINUS e=expr;     { Dba.ExprUnary (Dba.UMinus, e) }
 | EXTU e=expr; size=INT;
  { Dba.ExprExtU (e, int_of_string size) }
 | EXTS e=expr; size=INT;
  { Dba.ExprExtS (e, int_of_string size) }
 | ALTERNATIVE LPAR
   tag=option(terminated(alternativetag, COLON));
   exprs=separated_nonempty_list(COMMA, expr); RPAR
   { Dba.ExprAlternative (exprs, tag) }
 | IF condition=cond; THEN then_e=expr; ELSE else_e=expr;
   { Dba.ExprIte (condition, then_e, else_e) }
 | le=expr; bop=bin_op; re=expr;
 { Dba_types.Expr.binary bop le re }
;

%inline bin_op :
 | MODU    { Dba.ModU }
 | MODS    { Dba.ModS }
 | OR      { Dba.Or }
 | AND     { Dba.And }
 | XOR     { Dba.Xor }
 | CONCAT  { Dba.Concat }
 | EQQ     { Dba.Eq }
 | NEQ     { Dba.Diff }
 | LEU     { Dba.LeqU }
 | LTU     { Dba.LtU  }
 | GEU     { Dba.GeqU }
 | GTU     { Dba.GtU }
 | LES     { Dba.LeqS }
 | LTS     { Dba.LtS }
 | GES     { Dba.GeqS }
 | GTS     { Dba.GtS }
 | PLUS    { Dba.Plus }
 | MINUS   { Dba.Minus }
 | MULTU   { Dba.MultU}
 | MULTS   { Dba.MultS }
 | DIVU    { Dba.DivU}
 | DIVS    { Dba.DivS}
 | LSHIFT  { Dba.LShift }
 | RSHIFTU { Dba.RShiftU}
 | RSHIFTS { Dba.RShiftS }
 | LROTATE { Dba.LeftRotate }
 | RROTATE { Dba.RightRotate }


alternativetag :
 | ADDCARRY    { AddCarry }
 | ADDOVERFLOW { AddOverflow }

%inline cond :
 | TRUE     { Dba.True }
 | FALSE    { Dba.False }
 | e=expr;  { Dba.CondReif e }
;

address:
   /* | LPAR INT INFER INT SUPER COMMA INT RPAR {
     let size = int_of_string $4 in
     let bigint = Bigint.big_int_of_string $2 in
     let id = int_of_string $7 in
     if size = Dba_types.address_size then ((bigint, Dba_types.address_size), id)
     else
       let message =
         Format.asprintf "addresses must be on %d bits%!" Dba_types.address_size in
       failwith message
   } */
 | LPAR hex=HEXA; nid=preceded(COMMA, INT); RPAR
  {
    let id = int_of_string nid in
    let bv = Bitvector.of_hexstring hex in
    let addr = Dba_types.Caddress.create bv id in
    incr_address addr;
    addr
 }


integer:
 | n=HEXA;
 | n=INT;    { int_of_string n }
 ;

opcode:
 | s=STRING  { Basic_types.Binstream.of_bytes s }
 | integers=delimited(LPAR, nonempty_list(integer), RPAR);
   { Basic_types.Binstream.of_list integers }
;

patch:
 | LPAR address=integer; opc=opcode;  RPAR { address, opc }
;

patchmap:
 | patches=list(patch); EOF { mk_patches patches }
;
