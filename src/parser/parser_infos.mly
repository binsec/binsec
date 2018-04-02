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
  open Infos
  open Dba

  type labeladdress =
  | Label of string
  | Address of jump_target

  type labeloffset =
  | LabelIf of string
  | OffsetIf of id

  type parserinstrkind =
  | Assign of lhs * expr * id
  | SJump of labeladdress * (tag option)
  | DJump of expr * (tag option)
  | If of cond * labeladdress * labeloffset
  | Stop of state option
  | Assert of cond * id
  | Assume of cond * id
  | NondetAssume of (lhs list) * cond * id
  | Nondet of lhs * region * id
  | Undef of lhs * id
  | Malloc of lhs * expr * id
  | Free of expr * id
  | Print of printable list * id


  let incindex addr i =
    Dba_types.Caddress.reid addr (addr.id + i)

  let index = ref 0

  let locallabelMap = ref Basic_types.String.Map.empty

  let reset_label_map = locallabelMap := Basic_types.String.Map.empty

  let rec resolve_labels insns =
    match insns with
    | [] -> []
    | i :: l ->
      let h =
        match i with
          If(cond, thn, els) -> (
            match thn, els with
            | Address a, OffsetIf b ->
              Dba.IkIf(cond, a, b)
            | Address a, LabelIf b -> (
              try
                let b' = Basic_types.String.Map.find b !locallabelMap in
                Dba.IkIf(cond, a, b')
              with Not_found ->
                let message = "parser_infos.mly: jump to undefined label " ^ b in
                failwith message
            )
            | Label b, OffsetIf a -> (
              try
                let b' = Basic_types.String.Map.find b !locallabelMap in
                Dba.IkIf(cond, JInner b', a)
              with Not_found ->
                let message = "parsel_infos.mly: jump to undefined label " ^ b in
                failwith message
            )
            | Label a, LabelIf b -> (
              try
                let a' = Basic_types.String.Map.find a !locallabelMap in
                let b' = Basic_types.String.Map.find b !locallabelMap in
                Dba.IkIf(cond, JInner a', b')
              with Not_found ->
                let message = "parsel_infos.mly: jump to undefined label" in
                failwith message
            )
          )
        | SJump(dst, tag) -> (
          match dst with
          | Address a ->
            Dba.IkSJump(a, tag)
          | Label b -> (
            try
              let b' = Basic_types.String.Map.find b !locallabelMap in
              Dba.IkSJump(JInner b', tag)
            with Not_found ->
              let message = "parser_infos.mly: jump to undefined label " ^ b in
              failwith message
          )
        )
        | Assign(lhs, expr, a) -> Dba.IkAssign(lhs, expr, a)
        | DJump(dst, tag) -> Dba.IkDJump(dst, tag)
        | Stop tag -> (Dba.IkStop tag)
        | Undef (lhs, a) -> Dba.IkUndef(lhs, a)
        | Malloc (a, b, c) -> Dba.IkMalloc (a, b, c)
        | Free (a, b) -> Dba.IkFree (a, b)
        | Assert (a, b) -> Dba.IkAssert (a, b)
        | Assume (a, b) -> Dba.IkAssume (a, b)
        | NondetAssume (a, b, c) -> Dba.IkNondetAssume (a, b, c)
        | Nondet (a, b, c) -> Dba.IkNondet (a, b, c)
        | Print (a, b) -> Dba.IkPrint (a, b)
      in let c = try (Dba_utils.checksize_instruction h)
        with
          Errors.Assignment_size_conflict s ->
            failwith("Assignment_size_conflict " ^ s ^" in info file")
        | Errors.Undeclared_variable s ->
          failwith ("Undeclared_variable " ^ s ^" in info file")
        | Errors.Bad_address_size ->
          failwith ("Bad_address_size in info file")
        | Errors.Bad_exp_size ->
          failwith ("Bad_expression_size in info file")
        | Errors.Operands_size_conflict s ->
          failwith("Operands_size_conflict " ^s^" in info file")
        | Errors.Size_error _ ->
          failwith ("size error in info file")
         in
         if c then h :: (resolve_labels l)
         else	failwith ("size error in info file")


  let chain_insns addr insns =
    let rec aux addr insns l =
      let last_instr = Dba.IkDJump(Dba.ExprVar("ret", 32, None), Some Dba.Return) in
      match insns with
      | [] -> [addr, last_instr]
      | i :: [] -> (
        match i with
        | Dba.IkIf(cond, thn, els) -> (l @ [(addr, Dba.IkIf(cond, thn, els))])
        | Dba.IkAssign(lhs, expr, _) ->
          l @
          [addr, Dba.IkAssign(lhs, expr, addr.id + 1); incindex addr 1, last_instr]
        | Dba.IkUndef (lhs, _) ->
          l @
          [addr, Dba.IkUndef (lhs, addr.id + 1); incindex addr 1, last_instr]
        | Dba.IkSJump(dst, tag) -> (l @ [(addr, Dba.IkSJump(dst, tag))])
        | Dba.IkDJump(dst, tag) -> (l @ [(addr, Dba.IkDJump(dst, tag))])
        | Dba.IkStop st -> (l @ [(addr, Dba.IkStop st)])
        | Dba.IkMalloc (a, b, _) -> (l @ [(addr, Dba.IkMalloc(a, b, addr.id + 1));
                                         ((incindex addr 1), last_instr)])
        | Dba.IkFree (a, _) -> (l @ [(addr, Dba.IkFree(a, addr.id + 1));
                                    ((incindex addr 1), last_instr)])
        | Dba.IkAssert (a, _) -> (l @ [(addr, Dba.IkAssert(a, addr.id + 1));
                                      ((incindex addr 1), last_instr)])
        | Dba.IkAssume (a, _) -> (l @ [(addr, Dba.IkAssume(a, addr.id + 1));
                                      ((incindex addr 1), last_instr)])
        | Dba.IkNondetAssume (a, b, _) -> (l @ [(addr, Dba.IkNondetAssume(a, b, addr.id + 1)); ((incindex addr 1), last_instr)])
        | Dba.IkNondet (a, b, _) -> (l @ [(addr, Dba.IkNondet(a, b, addr.id + 1)); ((incindex addr 1), last_instr)])
        | Dba.IkPrint (a, _) -> (l @ [(addr, Dba.IkPrint(a, addr.id + 1)); ((incindex addr 1), last_instr)])
      )
      | i :: insns ->
        let chained_i =
          match i with
          | Dba.IkIf(cond, thn, els) -> Dba.IkIf(cond, thn, els)
          | Dba.IkAssign(lhs, expr, _) -> Dba.IkAssign(lhs, expr, addr.id + 1)
          | Dba.IkUndef (lhs, _) -> Dba.IkUndef(lhs, addr.id + 1)
          | Dba.IkSJump(dst, tag) -> Dba.IkSJump(dst, tag)
          | Dba.IkDJump(dst, tag) -> Dba.IkDJump(dst, tag)
          | Dba.IkStop tag -> Dba.IkStop tag
          | Dba.IkMalloc (a, b, _) -> Dba.IkMalloc (a, b, addr.id + 1)
          | Dba.IkFree (a, _) -> Dba.IkFree (a, addr.id + 1)
          | Dba.IkAssert (a, _) -> Dba.IkAssert (a, addr.id + 1)
          | Dba.IkAssume (a, _) -> Dba.IkAssume (a, addr.id + 1)
          | Dba.IkNondetAssume (a, b, _) -> Dba.IkNondetAssume (a, b, addr.id + 1)
          | Dba.IkNondet (a, b, _) -> Dba.IkNondet (a, b, addr.id + 1)
          | Dba.IkPrint (a, _) -> Dba.IkPrint (a, addr.id + 1)
        in aux (incindex addr 1) insns (l @ [addr, chained_i])
    in
    aux addr insns []

  let threshold_merge (los1, his1, lou1, hiu1) (los2, his2, lou2, hiu2) =
      (los1 @ los2, his1 @ his2, lou1 @ lou2, hiu1 @ hiu2)

  let mk_threshold (los, his, lou, hiu) =
      let signed = BoundThreshold.mk_from_list los his
      and unsigned = BoundThreshold.mk_from_list lou hiu in
      WideningThreshold.mk signed unsigned

  let empty_threshold = [], [], [], []

  let add_assoc_list_to_map initial_map add assoc =
      List.fold_left (fun m (k, v) -> add k v m) initial_map assoc
%}

%token PLUS MINUS MULTU MULTS DIVU DIVS MODU MODS
%token UNDEF STATE_OK STATE_KO
%token PRINT
%token ASSERT ASSUME NONDET NONDETASSUME STORELOAD CONSTANT STACK MALLOC FREE LINEAR
%token AND OR XOR NOT
%token CONCAT COLON SEMICOLON
%token LSHIFT RSHIFTU RSHIFTS LROTATE RROTATE EXTU EXTS
INFER SUPER
%token EQQ NEQ LEU LES LTU LTS GEU GES GTU GTS
%token LBRACE RBRACE LPAR RPAR LBRACKET RBRACKET COMMA ARROW ARROWINV STOP
ALTERNATIVE
%token ASSIGN TRUE FALSE IFJUMP ELSE ANNOT CALLFLAG
RETURNFLAG ADDCARRY ADDOVERFLOW
%token EOF

%token HIGHSIGNEDTHRESHOLDS LOWSIGNEDTHRESHOLDS
%token HIGHUNSIGNEDTHRESHOLDS LOWUNSIGNEDTHRESHOLDS
%token GLOBALTHRESHOLDS LOCALTHRESHOLDS
%token GLOBALWIDENINGDELAY LOCALWIDENINGDELAY

%token NEXT LNEXT ENTR JUMP ADDINSTRAT REPLACEINSTRAT CLOSEDJUMPS

%token <string> INT
%token <string> IDENT
%token <string> STRING
%token <string * int> HEXA
%token <string * int> HEXADDRESS

%type <Infos.t> configuration
%start configuration

%%


configuration :
 | infos { $1 }

infos:
| eps=addresses_section(ENTR);    infos=infos;
      { Infos.set_entry_points
          (Dba_types.Virtual_address.Set.of_list
           @@ List.map Dba_types.Virtual_address.of_code_address eps)
          infos }
| stops=addresses_section(STOP); infos=infos;
  { Infos.set_stops (Dba_types.Caddress.Set.of_list stops) infos }
| cjumps=addresses_pairs_section(CLOSEDJUMPS); infos=infos;
  { Infos.set_allowed_jumpzones cjumps infos }
| laddrs=addresses_pairs_section(LINEAR);  infos=infos;
  { Infos.set_linear_addresses laddrs infos }
| jmps=jumpslist; infos=infos;
  { Infos.set_jumps jmps infos }
| pstubs=addinstrslist; infos=infos;
  { Infos.set_prepend_stubs pstubs infos }
| sstubs=replaceinstrslist; infos=infos;
  { Infos.set_substitute_stubs sstubs infos }
| gwthr=global_widening_thresholds; infos=infos;
  { Infos.set_global_widening_thresholds gwthr infos }
| gwdel=global_widening_delay; infos=infos;
  { Infos.set_global_widening_delay gwdel infos }
| local_widening_thresholds  infos=infos;   { infos }
| local_widening_delay       infos=infos;   { infos }
| EOF                                       { Infos.default }

addresses_section(SECTION_KWD):
    | SECTION_KWD COLON addrs=addresses;  { addrs }

addresses_pairs_section(SECTION_KWD):
| SECTION_KWD COLON
   addr_pairs=list(delimited(LPAR, separated_pair(address, COMMA, address), RPAR));
   { addr_pairs }

jumpslist :
| JUMP COLON jumps=nonempty_list(jump);
    { add_assoc_list_to_map Dba_types.Caddress.Map.empty Dba_types.Caddress.Map.add jumps  }

jump:
| address NEXT LBRACKET addresses RBRACKET { ($1, $4) }

addinstrslist:
| ADDINSTRAT COLON located_instrs=nonempty_list(located_instruction);
  { let add loc instr addrmap =
      let added_value = resolve_labels instr in
      Dba_types.Caddress.Map.add loc added_value addrmap
    in add_assoc_list_to_map Dba_types.Caddress.Map.empty add located_instrs
  }

replaceinstrslist:
| REPLACEINSTRAT COLON located_instrs=nonempty_list(located_instruction);
  {
    let add loc instr addrmap =
      let added_value = chain_insns loc (resolve_labels instr) in
       Dba_types.Caddress.Map.add loc added_value addrmap
    in add_assoc_list_to_map Dba_types.Caddress.Map.empty add located_instrs  }

located_instruction:
 | loc=address; LBRACE instr=insts; RBRACE  { loc, instr }

addresses :
 | addr=separated_list(SEMICOLON, address); { addr }

global_widening_thresholds :
 | GLOBALTHRESHOLDS threshold_decl { $2 }

threshold_decl:
| LBRACE tspecs=nonempty_list(threshold_spec); RBRACE
  { mk_threshold (List.fold_left threshold_merge empty_threshold tspecs)  }

threshold_spec:
 | HIGHSIGNEDTHRESHOLDS COLON threshold_values
     { ([], $3, [], []) }
 | LOWSIGNEDTHRESHOLDS COLON threshold_values
     { $3, [], [], [] }
 | HIGHUNSIGNEDTHRESHOLDS COLON threshold_values
     { [], [], [], $3 }
 | LOWUNSIGNEDTHRESHOLDS COLON threshold_values
     { [], [], $3, []}

threshold_values :
 | separated_nonempty_list (COMMA, INT)
   { List.map int_of_string $1 }

local_widening_thresholds :
 | LOCALTHRESHOLDS
   LBRACE local_tholds=nonempty_list(located_threshold); RBRACE
  { add_assoc_list_to_map Dba_types.Caddress.Map.empty Dba_types.Caddress.Map.add local_tholds }

located_threshold :
 | address COLON threshold_decl { ($1, $3) }

global_widening_delay :
 | GLOBALWIDENINGDELAY COLON INT { int_of_string $3 }

local_widening_delay :
 | LOCALWIDENINGDELAY LBRACE local_delays RBRACE { $3 }


local_delays:
 | address COLON INT
   { let delay_value = int_of_string $3 in
     Dba_types.Caddress.Map.add $1 delay_value Dba_types.Caddress.Map.empty }
 | address COLON INT SEMICOLON local_delays
   { let delay_value = int_of_string $3 in
     Dba_types.Caddress.Map.add $1 delay_value $5}


address :
 | HEXADDRESS {
   let s, size = $1 in
   let s = String.sub s 1 (String.length s - 1) in
   let bigint = Bigint.big_int_of_string s in
   let bv = Bitvector.create bigint size in
   Dba_types.Caddress.block_start bv
 }

insts:
 | insts inst {
   index := !index + 1;
   $1 @ [$2]
 }
 | insts label inst {
   if Basic_types.String.Map.mem $2 !locallabelMap then
     let message = Format.asprintf "label locally already defined" in
     failwith message
   else (
     locallabelMap := Basic_types.String.Map.add $2 !index !locallabelMap;
     index := !index + 1;
     $1 @ [$3]
   )
 }
 | {  reset_label_map;
      index := 0;
      []
   }

label:
 | IDENT COLON { $1 }

inst:
 | lhs ASSIGN expr SEMICOLON { Assign ($1, $3, 0) }
 | lhs ASSIGN UNDEF SEMICOLON { Undef ($1, 0) }
 | lhs ASSIGN MALLOC LPAR expr RPAR SEMICOLON {
   (* let size = Bigint.big_int_of_string $5 in *)
   Malloc ($1, $5, 0)
 }
 | FREE LPAR expr RPAR SEMICOLON { Free ($3, 0) }
 | NEXT address SEMICOLON { SJump (Address (JOuter $2), None) }
 | NEXT INT SEMICOLON { SJump (Address (JInner (int_of_string $2)), None) }
 | NEXT expr SEMICOLON { DJump ($2, None) }
 | NEXT address SEMICOLON ANNOT CALLFLAG address { SJump (Address (JOuter $2), Some (Dba.Call $6)) }
 | NEXT address SEMICOLON ANNOT RETURNFLAG { SJump (Address (JOuter $2), Some (Dba.Return)) }
 | NEXT expr SEMICOLON ANNOT RETURNFLAG { DJump ($2, Some (Dba.Return)) }
 | NEXT expr SEMICOLON ANNOT CALLFLAG address { DJump ($2, Some (Dba.Call $6)) }
 | LNEXT IDENT SEMICOLON { SJump ((Label $2), None) }
 | IFJUMP cond NEXT address ELSE NEXT INT SEMICOLON {
   If ($2, Address(JOuter $4), OffsetIf(int_of_string $7))
 }
 | IFJUMP cond NEXT INT ELSE NEXT INT SEMICOLON {
   If ($2, Address(JInner (int_of_string $4)), OffsetIf(int_of_string $7))
 }
 | IFJUMP cond  NEXT IDENT ELSE NEXT IDENT SEMICOLON {
   If ($2, (Label $4), (LabelIf $7))
 }
 | STOP STATE_OK SEMICOLON { Stop (Some OK) }
 | STOP STATE_KO SEMICOLON { Stop (Some KO) }
 | STOP SEMICOLON { Stop None }
 | ASSERT LPAR cond RPAR SEMICOLON { Assert ($3, 0) }
 | ASSUME LPAR cond RPAR SEMICOLON { Assume ($3, 0) }
 | NONDETASSUME LPAR LBRACE lhslist RBRACE COMMA cond RPAR SEMICOLON { NondetAssume($4,$7,0) }
 | lhs ASSIGN NONDET LPAR regionnondet RPAR SEMICOLON { Nondet ($1, $5, 0) }
 | PRINT printargs SEMICOLON { Print ($2, 0) }

lhslist :
 | lhs COMMA lhslist { $1 :: $3 }
 | lhs { [$1] }

regionnondet :
 | CONSTANT { `Constant }
 | STACK    { `Stack }
 | MALLOC   { Dba_types.Region.malloc (Machine.Word_size.get ()) }
;

printargs :
 | expr CONCAT printargs   { (Exp $1) :: $3 }
 | STRING CONCAT printargs { (Str $1) :: $3 }
 | expr                    { [Exp $1] }
 | STRING                  { [Str $1] }

lhs :
 | IDENT INFER INT SUPER { let size = int_of_string $3 in
                           Dba.LhsVar ($1, size, None)
                         }
 | IDENT INFER INT SUPER LBRACE INT COMMA INT RBRACE {
   let off1 = int_of_string $6 in
   let off2 = int_of_string $8 in
   let size = int_of_string $3 in
   Dba.LhsVarRestrict ($1, size, off1, off2)
 }
 | STORELOAD LBRACKET expr COMMA ARROW COMMA INT  RBRACKET {
   let size = int_of_string $7 in Dba.LhsStore (size, BigEndian, $3)
 }
 | STORELOAD LBRACKET expr COMMA ARROWINV COMMA INT RBRACKET {
   let size = int_of_string $7 in Dba.LhsStore (size, LittleEndian, $3)
 }
 | STORELOAD LBRACKET expr COMMA INT RBRACKET {
   let size = int_of_string $5 in
   let endia = Dba_types.get_endianness () in Dba.LhsStore (size, endia, $3)
 }


expr:
 | INT INFER INT SUPER {
   let size = int_of_string $3 in
   let bigint = (Bigint.big_int_of_string $1) in
   let bv = Bitvector.create bigint size in
   Dba.ExprCst (`Constant, bv)
 }

 | HEXA {
   let s, size = $1 in
   let bigint = (Bigint.big_int_of_string s) in
   let bv = Bitvector.create bigint size in
   Dba.ExprCst (`Constant, bv)
 }

 | LPAR region COMMA INT INFER INT SUPER RPAR {
   let bigint = (Bigint.big_int_of_string $4) in
   let size = int_of_string $6 in
   let bv = Bitvector.create bigint size in
   Dba.ExprCst ($2, bv)
 }

 | LPAR region COMMA HEXA RPAR {
   let s, size = $4 in
   let bigint = (Bigint.big_int_of_string s) in
   let bv = Bitvector.create bigint size in
   Dba.ExprCst ($2, bv)
 }

 | IDENT {
   Dba.ExprVar ($1, 32, None)
 }

 | STORELOAD LBRACKET expr COMMA ARROW COMMA INT RBRACKET {
   let size = int_of_string $7 in
   Dba.ExprLoad (size, BigEndian, $3 )
 }
 | STORELOAD LBRACKET expr COMMA ARROWINV COMMA INT RBRACKET {
   let size = int_of_string $7 in
   Dba.ExprLoad (size, LittleEndian, $3)
 }
 | STORELOAD LBRACKET expr COMMA INT RBRACKET {
   let size = int_of_string $5 in
   let endia = Dba_types.get_endianness () in Dba.ExprLoad (size, endia, $3)
 }

(*| MINUS expr %prec UMINUS { Dba.ExprUnary (Dba.UMinus, $2) }*)
 | NOT expr expr { Dba.ExprUnary (Dba.Not, $2) }

 | LBRACE expr COMMA INT COMMA INT RBRACE {
   let off1 = int_of_string $4 in
   let off2 = int_of_string $6 in
   Dba.ExprRestrict ($2, off1, off2)
 }
 | EXTU expr INT { let size = int_of_string $3 in Dba.ExprExtU ($2, size) }
 | EXTS expr INT { let size = int_of_string $3 in Dba.ExprExtS ($2, size) }
 | ALTERNATIVE LPAR exprs RPAR { Dba.ExprAlternative ($3, None) }
 | ALTERNATIVE LPAR alternativetag COLON exprs RPAR {
   Dba.ExprAlternative ($5, Some $3)
 }
 | IFJUMP cond expr ELSE expr { Dba.ExprIte ($2, $3, $5) }
 | LPAR expr RPAR { $2 }
 | LPAR expr bin_op expr RPAR { Dba.ExprBinary ($3, $2 , $4) }


bin_op :
 | MODU { Dba.ModU } /*TODO : check operators precedence */
 | MODS { Dba.ModS }
 | OR  { Dba.Or }
 | AND { Dba.And }
 | XOR { Dba.Xor }
 | CONCAT { Dba.Concat }
 | EQQ { Dba.Eq }
 | NEQ { Dba.Diff }
 | LEU { Dba.LeqU }
 | LTU { Dba.LtU  }
 | GEU { Dba.GeqU }
 | GTU { Dba.GtU }
 | LES { Dba.LeqS }
 | LTS { Dba.LtS }
 | GES { Dba.GeqS }
 | GTS { Dba.GtS }
 | PLUS  { Dba.Plus }
 | MINUS { Dba.Minus }
 | MULTU { Dba.MultU}
 | MULTS { Dba.MultS }
 | DIVU   { Dba.DivU}
 | DIVS     { Dba.DivS}
 | LSHIFT  { Dba.LShift }
 | RSHIFTU  { Dba.RShiftU}
 | RSHIFTS   { Dba.RShiftS }
 | LROTATE  { Dba.LeftRotate }
 | RROTATE  { Dba.RightRotate }

region:
 | CONSTANT { `Constant }
 | STACK    { `Stack }

alternativetag :
 | ADDCARRY { AddCarry }
 | ADDOVERFLOW { AddOverflow }

exprs :
 | expr COMMA exprs { $1 :: $3 }
 | expr { [$1] }

cond :
 | TRUE { Dba.True }
 | FALSE { Dba.False }
 | expr { Dba.CondReif $1 }
