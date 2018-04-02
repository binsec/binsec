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

exception UndeclaredVariable of string
exception WrongInitializationSize of Dba.address * Dba.instruction

let cur_address = ref 0

let incr_address addr = cur_address := addr.Dba.id + 1

let _threshold_merge (los1, his1, lou1, hiu1) (los2, his2, lou2, hiu2) =
  (los1 @ los2, his1 @ his2, lou1 @ lou2, hiu1 @ hiu2)

let _mk_threshold (los, his, lou, hiu) =
  let open Infos in
  let signed = BoundThreshold.mk_from_list los his
  and unsigned = BoundThreshold.mk_from_list lou hiu in
  WideningThreshold.mk signed unsigned

let _empty_threshold = [], [], [], []


module Declarations = struct

  let declarations : (string, Dba.size * Dba.vartag option) Hashtbl.t =
    Hashtbl.create 16

  let _init () = Hashtbl.reset declarations

  let find name = Hashtbl.find declarations name
  let size name = find name |> fst
  let _check name  =
    if not (Hashtbl.mem declarations name)
    then raise (UndeclaredVariable name)

  let add name size opttags =
    Hashtbl.add declarations name (size, opttags)
end


module Mk = struct
  open Dba
  let checked_cond_expr e =
    let cond = Dba.CondReif e in
    ignore(Dba_utils.checksize_dbacond cond);
    cond


  let filemode addr read_perm write_perm exec_perm =
    let access_to_dbabool = function
      | true -> Dba.True
      | false -> Dba.CondNot addr
    in
    let open Dba_types in
    (addr, (Read read_perm, Write write_perm, Exec exec_perm)),
    (access_to_dbabool read_perm,
     access_to_dbabool write_perm,
     access_to_dbabool exec_perm)

  module Predicates = struct
    let mk_and p q = Dba.CondAnd(p, q)
    let rec of_list l =
      match l with
      | [] -> assert false
      | [elmt, p] -> [elmt], p
      | (elmt, (p1, p2, p3)) :: l ->
        let elmts, (q1, q2, q3) = of_list l in
        elmt :: elmts,
        (mk_and p1 q1, mk_and p2 q2, mk_and p3 q3)
  end

  module Permissions = struct
  let empty = Dba_types.Region.Map.empty, Dba_types.Rights.empty

  let add_rights (p1, p2, p3) region rights =
    let open Dba_types.Rights in
    let update_rights v p rights =
      let right_condition =
        match find v rights with
        | p' -> Dba.CondAnd (p, p')
        | exception Not_found -> p
      in add v right_condition rights
    in
    update_rights (R, region) p1 rights
    |> update_rights (W, region) p2
    |> update_rights (X, region) p3

  let add_permissions l region permissions =
    try let p_l = Dba_types.Region.Map.find region permissions in
      (Dba_types.Region.Map.add region (p_l @ l) permissions)
    with Not_found ->
      (Dba_types.Region.Map.add region l permissions)

  let add_permissions l region (perms, rights) =
    add_permissions l region perms, rights

  let add_rights v region (perms, rights) =
    perms, add_rights v region rights

  let of_list l =
    let rec aux p = function
      | [] -> p
      | (region, permission, rights) :: l' ->
        let p'= add_permissions permission region (add_rights rights region p)
        in aux p' l'
    in aux empty l

end

  let default_endianness = Dba_types.get_endianness
   (* match !Dba.endiannness with
    | [] -> raise UnknownEndianness
    | endianness :: _ -> endianness
   *)
  module Lhs = struct
    let lhs name size = Dba.LhsVar (name, size, None)
    let restricted name (loff, roff) size =
      Dba.LhsVarRestrict(name, size, loff, roff)

    let declared name =
      try
        let size = Declarations.size name in
        lhs name size
      with
      | Not_found -> raise (UndeclaredVariable name)

    let declared_restricted name offs =
      try
        let size = Declarations.size name in
        restricted name offs size
      with
      | Not_found -> raise (UndeclaredVariable name)

    let store endianness size expr =
      let size = int_of_string size in
      Dba.LhsStore(size, endianness, expr)

    let big_endian_store = store Dba.BigEndian
    let little_endian_store = store Dba.LittleEndian
    let default_store size expr =
      store (default_endianness ()) size expr

    let store expr size = function
      | Some BigEndian -> big_endian_store size expr
      | Some LittleEndian -> little_endian_store size expr
      | None -> default_store size expr
  end

  module Expr = struct
    let id name size tags = Dba.ExprVar(name, size, tags)

    let declared_id name =
      try
        let size, tags = Declarations.find name in
        id name size tags
      with
      | Not_found -> raise (UndeclaredVariable name)

    let sized_region region (value, size) =
      let value = Bigint.big_int_of_string value in
      Dba_types.Expr.constant ~region (Bitvector.create value size)

    let constant (value, size) =
      let value = Bigint.big_int_of_string value in
      Dba_types.Expr.constant (Bitvector.create value size)

    let load expr size endianness_opt =
      let endianness =
        match endianness_opt with
        | Some e -> e
        | None -> default_endianness ()
      in Dba.ExprLoad(size, endianness, expr)

    let restricted expr (loff, roff) =
      assert (roff >= loff);
      Dba.ExprRestrict(expr, loff, roff)
  end


  module Initializations = struct

    let check addr instruction =
      let is_ok =
        try Dba_utils.checksize_instruction instruction
        with  _ -> false
      in
      if not is_ok then
        begin
          Logger.fatal
            "@[<hov 0>%@ %a:@ bad initialization size for %a@]"
            Dba_printer.Ascii.pp_code_address addr
            Dba_printer.Ascii.pp_instruction instruction ;
          raise (WrongInitializationSize (addr, instruction))
        end

    let checked_of_list instructions =
      (* init_address seems to be set once and for all *)
      List.iter (check !Dba_types.Caddress.default_init) instructions;
      instructions
  end

  module Region = struct
    let malloc addr =
      let minus_one = Bigint.big_int_of_int (-1) in
      let a = Dba_types.Caddress.block_start (Bitvector.create minus_one addr) in
      `Malloc ((-1, a), Bigint.zero_big_int)
  end

  let instruction_size_error instruction =
    Logger.fatal "Bad instruction size: %a"
      Dba_printer.Ascii.pp_instruction instruction;
    Errors.mismatched_instruction_size instruction


  let address_size_error address =
    Logger.fatal "Bad address size: %a"
      Dba_types.Caddress.pp_base address;
    Errors.mismatched_address_size address


  let checked_localized_instruction address instruction =
    if Dba_utils.checksize_address address then
      if Dba_utils.checksize_instruction instruction then address, instruction
      else instruction_size_error instruction
    else address_size_error address


  let extract_declaration_data = function
    | Dba.LhsVar (v, sz, tagopt) -> v, sz, tagopt
    | Dba.LhsVarRestrict _
    | Dba.LhsStore _ -> assert false

  let fill_sizes declarations initializations =
    let open Dba_types in
    let patch_instruction_size = function
      | Dba.IkAssign (lv, rv, id) ->
         let lvname =
           match LValue.name_of lv with
           | Some name -> name
           | None -> assert false
                            (* initializations should not manipulate tables *)
         in
         let declared_size =
           match Basic_types.String.Map.find lvname declarations with
           | size, _ -> Basic_types.BitSize.create size
           | exception Not_found ->
              Logger.fatal "Variable %s was not declared" lvname;
              exit 2
         in Instruction.assign (LValue.resize declared_size lv) rv id
      | _ -> assert false (* initializations should only be assignments *)
    in
    List.map patch_instruction_size initializations

  let set_nexts =
    List.mapi
      (fun i instruction ->
         Dba_types.Instruction.set_successor instruction (i +1))


  let program permissions initializations start_address declarations instructions =
    let permissions =
      Utils.get_opt_or_default Permissions.empty permissions
    and declarations =
      List.map extract_declaration_data declarations
      |> Dba_types.Declarations.of_list in
    let instructions =
      List.fold_left
        (fun map (address, instruction) ->
           Dba_types.Caddress.Map.add address (instruction, None) map)
        Dba_types.Caddress.Map.empty instructions in
    let initializations =
      fill_sizes declarations initializations
      |> Initializations.checked_of_list
      |> set_nexts
    in
    { Dba_types.start_address; declarations;
      permissions; initializations; instructions; }

end


let rec expr_size = function
  | Dba.ExprVar(_ ,sz, _) -> sz
  | Dba.ExprLoad(sz, _endian, _bexpr) -> sz
  | Dba.ExprCst(_r, bv) -> Bitvector.size_of bv
  | Dba.ExprUnary(_uop, bexpr) -> expr_size bexpr
  | Dba.ExprBinary(bop, bexpr1, bexpr2) ->
    let sz1 = expr_size bexpr1 in
    let sz2 = expr_size bexpr2 in
    begin
      match bop with
      | Dba.Plus | Dba.Minus | Dba.MultU | Dba.MultS | Dba.DivU | Dba.DivS
      | Dba.ModU | Dba.ModS | Dba.Or | Dba.And | Dba.Xor ->
        if sz1 = 0 then sz2
        else if sz2 = 0 then sz1
        else (Logger.warning "cannot infer binary expr size"; 0)
      | Dba.LShift | Dba.RShiftU
      | Dba.RShiftS | Dba.LeftRotate | Dba.RightRotate ->
        sz1
      | Dba.Concat ->
        if sz1 = 0 || sz2 = 0 then 0
        else sz1 + sz2
      | Dba.Eq| Dba.Diff | Dba.LeqU | Dba.LtU | Dba.GeqU | Dba.GtU
      | Dba.LeqS | Dba.LtS | Dba.GeqS | Dba.GtS ->
        1
    end
  | Dba.ExprRestrict(_bexpr,i,j) -> j - i +1
  | Dba.ExprExtU(bexpr,n) | Dba.ExprExtS(bexpr,n) ->
    let sz = expr_size bexpr in
    if sz = 0 then 0 else sz + n
  | Dba.ExprIte(_bcond, be1, be2) ->
    let sz1 = expr_size be1 in
    let sz2 = expr_size be2 in
    if sz1 = sz2 then sz1
    else if sz1 = 0 then sz2
    else if sz2 = 0 then sz1
    else (Logger.warning "cannot infer binary expr size"; 0)
  | Dba.ExprAlternative (e :: _, _) ->
    expr_size e
  | Dba.ExprAlternative ([], _) -> 0


let expr_of_name name =
  let first_char = name.[0] in
  if first_char = '_' || first_char =  '?' ||  first_char  = '!' then
    let name = if first_char = '_' then "*" else name in
    Dba.ExprVar(name, 0, None)
  else
    match name with
    | "al" -> Dba.ExprRestrict(Dba.ExprVar("eax", 32,None), 0, 7)
    | "ah" -> Dba.ExprRestrict(Dba.ExprVar("eax", 32,None), 8, 15)
    | "ax" -> Dba.ExprRestrict(Dba.ExprVar("eax", 32,None), 0, 15)
    | "eax" -> Dba.ExprVar ("eax", 32, None)
    | "bl" -> Dba.ExprRestrict(Dba.ExprVar("ebx", 32,None), 0, 7)
    | "bh" -> Dba.ExprRestrict(Dba.ExprVar("ebx", 32,None), 8, 15)
    | "bx" -> Dba.ExprRestrict(Dba.ExprVar("ebx", 32,None), 0, 15)
    | "ebx" -> Dba.ExprVar ("ebx", 32, None)
    | "cl" -> Dba.ExprRestrict(Dba.ExprVar("ecx", 32,None), 0, 7)
    | "ch" -> Dba.ExprRestrict(Dba.ExprVar("ecx", 32,None), 8, 15)
    | "cx" -> Dba.ExprRestrict(Dba.ExprVar("ecx", 32,None), 0, 15)
    | "ecx" -> Dba.ExprVar ("ecx", 32, None)
    | "dl" -> Dba.ExprRestrict(Dba.ExprVar("edx", 32,None), 0, 7)
    | "dh" -> Dba.ExprRestrict(Dba.ExprVar("edx", 32,None), 8, 15)
    | "dx" -> Dba.ExprRestrict(Dba.ExprVar("edx", 32,None), 0, 15)
    | "edx" -> Dba.ExprVar ("edx", 32, None)
    | "di" -> Dba.ExprRestrict(Dba.ExprVar("edi", 32,None), 0, 15)
    | "edi" -> Dba.ExprVar ("edi", 32, None)
    | "si" -> Dba.ExprRestrict(Dba.ExprVar("esi", 32,None), 0, 15)
    | "esi" -> Dba.ExprVar ("esi", 32, None)
    | "bp" -> Dba.ExprRestrict(Dba.ExprVar("ebp", 32,None), 0, 15)
    | "ebp" -> Dba.ExprVar ("ebp", 32, None)
    | "sp" -> Dba.ExprRestrict(Dba.ExprVar("esp", 32,None), 0, 15)
    | "esp" -> Dba.ExprVar ("esp", 32, None)
    | "btemp" -> Dba.ExprVar ("btemp", 8, None)
    | "stemp" -> Dba.ExprVar ("stemp", 16, None)
    | "temp" -> Dba.ExprVar ("temp", 32, None)
    | "dtemp" -> Dba.ExprVar ("dtemp", 64, None)
    | name ->
      Logger.error"Unknown variable name: %s" name;
      raise Parsing.Parse_error

  let is_wildmetapld_expr = function
    | Dba.ExprVar(name, _, _) ->
      let c = name.[0] in c = '*' || c = '?' || c = '!'
    | _ -> false


  let rec patch_expr_size e sz =
    Logger.debug ~level:2 "Will patch: %a with %d" Dba_printer.Ascii.pp_expr e sz;
    match e with
    | Dba.ExprVar(n, _old_sz,l) ->
      if is_wildmetapld_expr e then e
      else Dba.ExprVar(n, sz, l)
    | Dba.ExprCst(r,bv) -> Dba.ExprCst(r,(Bitvector.create (Bitvector.value_of bv) sz))
    | Dba.ExprLoad(_old_sz, en, e) -> Dba.ExprLoad(sz/8, en, e)
    | Dba.ExprUnary(op, e1) -> Dba.ExprUnary(op, patch_expr_size e1 sz)
    | Dba.ExprExtU(e1,size) -> Dba.ExprExtU(patch_expr_size e1 (sz-size), size)
    | Dba.ExprExtS(e1,size) -> Dba.ExprExtS(patch_expr_size e1 (sz-size), size)
    | _ -> e


module Message = struct
  module Value = struct
    type t =
      | Hex of int
      | Int of int
      | Str of string

    let vstr v = Str v
    let vhex v = Hex (int_of_string v)
    let vint v = Int (int_of_string v)
  end

end

let mk_patches l =
  let open Dba_types.Virtual_address in
  List.fold_left
    (fun vmap (vaddr, opcode) -> Map.add (create vaddr) opcode vmap)
    Map.empty l
         
  
