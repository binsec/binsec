/**************************************************************************/
/*  This file is part of BINSEC.                                          */
/*                                                                        */
/*  Copyright (C) 2016-2019                                               */
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
  | Address of id jump_target

  type labeloffset =
  | LabelIf of string
  | OffsetIf of id

  type parserinstrkind =
  | Assign of LValue.t * Expr.t * id
  | SJump of labeladdress * Tag.t option
  | DJump of Expr.t * Tag.t option
  | If of Expr.t * labeladdress * labeloffset
  | Stop of state option
  | Assert of Expr.t * id
  | Assume of Expr.t * id
  | NondetAssume of LValue.t list * Expr.t * id
  | Nondet of LValue.t * region * id
  | Undef of LValue.t * id
  | Malloc of LValue.t * Expr.t * id
  | Free of Expr.t * id
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
        | If(cond, thn, els) -> (
            match thn, els with
            | Address a, OffsetIf b ->
              Instr.ite cond a b
            | Address a, LabelIf b -> (
              match Basic_types.String.Map.find b !locallabelMap with
              | b' -> Instr.ite cond a b'
              | exception Not_found ->
                 let message = "parser_infos.mly: jump to undefined label " ^ b in
                 failwith message
            )
            | Label b, OffsetIf a -> (
              match Basic_types.String.Map.find b !locallabelMap with
              | b' -> Instr.ite cond (Jump_target.inner b') a
              | exception Not_found ->
                 let message = "parser_infos.mly: jump to undefined label " ^ b in
                 failwith message
            )
            | Label a, LabelIf b -> (
              try
                let a' = Basic_types.String.Map.find a !locallabelMap in
                let b' = Basic_types.String.Map.find b !locallabelMap in
                Instr.ite cond (Jump_target.inner a') b'
              with Not_found ->
                let message = "parsel_infos.mly: jump to undefined label" in
                failwith message
            )
          )
        | SJump(dst, tag) -> (
          match dst with
          | Address a -> Instr.static_jump a ?tag
          | Label b -> (
            match Basic_types.String.Map.find b !locallabelMap with
            | b' -> Instr.static_jump (Jump_target.inner b') ?tag
            | exception Not_found ->
              let message = "parser_infos.mly: jump to undefined label " ^ b in
              failwith message
          )
        )
        | Assign(lhs, expr, a) -> Instr.assign lhs expr a
        | DJump(dst, tag) -> Instr.dynamic_jump dst ?tag
        | Stop tag -> Instr.stop tag
        | Undef (lhs, a) -> Instr.undefined lhs a
        | Malloc (a, b, c) -> Instr.malloc a b c
        | Free (a, b) -> Instr.free a b
        | Assert (a, b) -> Instr._assert a b
        | Assume (a, b) -> Instr.assume a b
        | NondetAssume (a, b, c) -> Instr.non_deterministic_assume a b c
        | Nondet (a, region, c) -> Instr.non_deterministic a ~region c
        | Print (a, b) -> Instr.print a b
      in let c = try (Dba_utils.checksize_instruction h)
        with
        | Errors.Assignment_size_conflict s ->
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
      let last_instr =
        Instr.dynamic_jump (Expr.var "ret" 32) ~tag:Dba.Return in
      match insns with
      | [] -> [addr, last_instr]
      | i :: [] -> (
        match i with
        | Dba.Instr.If(cond, thn, els) -> l @ [addr, Instr.ite cond thn els]
        | Dba.Instr.Assign(lhs, expr, _) ->
          l @
            [addr, Instr.assign lhs expr (addr.id + 1);
             incindex addr 1, last_instr
            ]
        | Dba.Instr.Undef (lhs, _) ->
          l @
            [addr, Instr.undefined lhs (addr.id + 1);
             incindex addr 1, last_instr]
        | Dba.Instr.SJump _
        | Dba.Instr.DJump _ as i -> l @ [(addr, i)]
        | Dba.Instr.Stop _ -> l @ [addr, i]
        | Dba.Instr.Malloc (a, b, _) ->
           l @ [addr, Instr.malloc a b (addr.id + 1);
                incindex addr 1, last_instr]
        | Dba.Instr.Free (a, _) ->
           l @ [addr, Instr.free a (addr.id + 1);
                incindex addr 1, last_instr]
        | Dba.Instr.Assert (a, _) ->
           l @ [addr, Instr._assert a (addr.id + 1);
                incindex addr 1, last_instr]
        | Dba.Instr.Assume (a, _) ->
           l @ [addr, Instr.assume a (addr.id + 1);
                incindex addr 1, last_instr]
        | Dba.Instr.NondetAssume (a, b, _) ->
           l @ [addr, Instr.non_deterministic_assume a b (addr.id + 1);
                incindex addr 1, last_instr]
        | Dba.Instr.Nondet (a, region, _) ->
           l @ [addr, Instr.non_deterministic a ~region (addr.id + 1);
                incindex addr 1, last_instr]
        | Dba.Instr.Print (a, _) ->
           l @ [addr, Instr.print a (addr.id + 1);
                incindex addr 1, last_instr]
      )
      | i :: insns ->
        let chained_i =
          match i with
          | Dba.Instr.SJump _
          | Dba.Instr.DJump _
          | Dba.Instr.Stop  _
          | Dba.Instr.If    _ -> i
          | Dba.Instr.Assign _
          | Dba.Instr.Undef _
          | Dba.Instr.Malloc _
          | Dba.Instr.Free _
          | Dba.Instr.Assert _
          | Dba.Instr.Assume _
          | Dba.Instr.NondetAssume _
          | Dba.Instr.Nondet _
          | Dba.Instr.Print _ -> Dba_types.Instruction.set_successor i (addr.id + 1)
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
%token ASSIGN TRUE FALSE IFJUMP ELSE ANNOT CALLFLAG RETURNFLAG
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
          (Virtual_address.Set.of_list
           @@ List.map Dba_types.Caddress.to_virtual_address eps)
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
   let vaddr = Virtual_address.of_bitvector bv in
   Dba_types.Caddress.block_start vaddr
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
 | MALLOC   { Dba_types.Region.malloc (Kernel_options.Machine.word_size ()) }
;

printargs :
 | expr CONCAT printargs   { (Exp $1) :: $3 }
 | STRING CONCAT printargs { (Str $1) :: $3 }
 | expr                    { [Exp $1] }
 | STRING                  { [Str $1] }
;

lhs :
 | id=IDENT; INFER sz=INT; SUPER
 { let bitsize = Size.Bit.of_string sz in
   Dba.LValue.var id ~bitsize
 }
 | id=IDENT; INFER sz=INT; SUPER LBRACE lo=INT; COMMA hi=INT; RBRACE {
   let off1 = int_of_string lo in
   let off2 = int_of_string hi in
   let size = Size.Bit.of_string sz in
   Dba.LValue._restrict id size off1 off2
 }
 | STORELOAD LBRACKET e=expr; COMMA
   endianness=ioption(terminated(endianness, COMMA));
   bytes=INT; RBRACKET {
   let size =  Size.Byte.of_string bytes in
   let endia =
     match endianness with
     | Some _endianness -> _endianness
     | None -> Kernel_options.Machine.endianness () in
   Dba.LValue.store size endia e
 }
;

endianness:
 | ARROW     { Machine.BigEndian }
 | ARROWINV  { Machine.LittleEndian }
 ;

expr:
 | INT INFER INT SUPER {
   let size = int_of_string $3 in
   let bigint = (Bigint.big_int_of_string $1) in
   let bv = Bitvector.create bigint size in
   Dba.Expr.constant bv
 }

 | HEXA {
   let s, size = $1 in
   let bigint = (Bigint.big_int_of_string s) in
   let bv = Bitvector.create bigint size in
   Dba.Expr.constant bv
 }

 | LPAR region=region; COMMA INT INFER INT SUPER RPAR {
   let bigint = Bigint.big_int_of_string $4 in
   let size = int_of_string $6 in
   let bv = Bitvector.create bigint size in
   Dba.Expr.constant ~region bv
 }

 | LPAR region=region; COMMA HEXA RPAR {
   let s, size = $4 in
   let bigint = (Bigint.big_int_of_string s) in
   let bv = Bitvector.create bigint size in
   Dba.Expr.constant ~region bv
 }

 | IDENT { Dba.Expr.var $1 32 }

 | STORELOAD LBRACKET expr COMMA ARROW COMMA INT RBRACKET {
   let size = int_of_string $7 |> Size.Byte.create in
   Dba.Expr.load size Machine.BigEndian $3
 }
 | STORELOAD LBRACKET expr COMMA ARROWINV COMMA INT RBRACKET {
   let size = int_of_string $7 |> Size.Byte.create in
   Dba.Expr.load size Machine.LittleEndian $3
 }
 | STORELOAD LBRACKET expr COMMA INT RBRACKET {
   let size = int_of_string $5 |> Size.Byte.create in
   let endia = Kernel_options.Machine.endianness () in
   Dba.Expr.load size endia $3
 }

(*| MINUS expr %prec UMINUS { Dba.ExprUnary (Dba.UMinus, $2) }*)
 | NOT expr expr { Dba.Expr.lognot $2 }

 | LBRACE expr COMMA INT COMMA INT RBRACE {
   let off1 = int_of_string $4 in
   let off2 = int_of_string $6 in
   Dba.Expr.restrict off1 off2 $2
 }
 | EXTU expr INT { let size = int_of_string $3 in Dba.Expr.uext size $2 }
 | EXTS expr INT { let size = int_of_string $3 in Dba.Expr.sext size $2 }
 | IFJUMP expr expr ELSE expr { Dba.Expr.ite $2 $3 $5 }
 | LPAR expr RPAR             { $2 }
 | LPAR expr bin_op expr RPAR { Dba.Expr.binary $3 $2 $4 }


bin_op :
 | MODU { Dba.Binary_op.ModU } /*TODO : check operators precedence */
 | MODS { Dba.Binary_op.ModS }
 | OR  { Dba.Binary_op.Or }
 | AND { Dba.Binary_op.And }
 | XOR { Dba.Binary_op.Xor }
 | CONCAT { Dba.Binary_op.Concat }
 | EQQ { Dba.Binary_op.Eq }
 | NEQ { Dba.Binary_op.Diff }
 | LEU { Dba.Binary_op.LeqU }
 | LTU { Dba.Binary_op.LtU  }
 | GEU { Dba.Binary_op.GeqU }
 | GTU { Dba.Binary_op.GtU }
 | LES { Dba.Binary_op.LeqS }
 | LTS { Dba.Binary_op.LtS }
 | GES { Dba.Binary_op.GeqS }
 | GTS { Dba.Binary_op.GtS }
 | PLUS  { Dba.Binary_op.Plus }
 | MINUS { Dba.Binary_op.Minus }
 | MULTU { Dba.Binary_op.Mult}
 | MULTS { Dba.Binary_op.Mult}
 | DIVU   { Dba.Binary_op.DivU}
 | DIVS     { Dba.Binary_op.DivS}
 | LSHIFT  { Dba.Binary_op.LShift }
 | RSHIFTU  { Dba.Binary_op.RShiftU}
 | RSHIFTS   { Dba.Binary_op.RShiftS }
 | LROTATE  { Dba.Binary_op.LeftRotate }
 | RROTATE  { Dba.Binary_op.RightRotate }

region:
 | CONSTANT { `Constant }
 | STACK    { `Stack }
;

cond :
 | TRUE    { Dba.Expr.one }
 | FALSE   { Dba.Expr.zero }
 | e=expr  { e }
;
