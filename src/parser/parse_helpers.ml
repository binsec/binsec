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


exception WrongInitializationSize of Dba.address * Dba.Instr.t

module Logger = Kernel_options.Logger

let cur_address = ref 0
let incr_address addr = cur_address := addr.Dba.id + 1
let cur_address () = !cur_address


module Declarations = struct

  module SH = Basic_types.String.Htbl

  let declarations : (Dba.size * Dba.VarTag.t) SH.t =
    SH.create 16

  let add name size opttags =
    SH.add declarations name (size, opttags)
end


module Mk = struct
  open Dba
  let checked_cond_expr e =
    ignore(Dba_utils.checksize_dbacond e);
    e

  let filemode addr read_perm write_perm exec_perm =
    let access_to_dbabool = function
      | true -> Expr.one
      | false -> Expr.zero
    in
    let open Dba_types in
    (addr, (Read read_perm, Write write_perm, Exec exec_perm)),
    (access_to_dbabool read_perm,
     access_to_dbabool write_perm,
     access_to_dbabool exec_perm)

  module Predicates = struct
    let rec of_list l =
      match l with
      | [] -> assert false
      | [elmt, p] -> [elmt], p
      | (elmt, (p1, p2, p3)) :: l ->
        let elmts, (q1, q2, q3) = of_list l in
        elmt :: elmts,
        (Expr.logand p1 q1, Expr.logand p2 q2, Expr.logand p3 q3)
  end

  module Permissions = struct
    let empty = Dba_types.Region.Map.empty, Dba_types.Rights.empty

    let add_rights (p1, p2, p3) region rights =
      let open Dba_types.Rights in
      let update_rights v p rights =
        let right_condition =
          match find v rights with
          | p' -> Expr.logand p p'
          | exception Not_found -> p
        in add v right_condition rights
      in
      update_rights (R, region) p1 rights
      |> update_rights (W, region) p2
      |> update_rights (X, region) p3

    let add_permissions l region permissions =
      let ps =
        match Dba_types.Region.Map.find region permissions with
        | p_l -> p_l @ l
        | exception Not_found -> l
      in Dba_types.Region.Map.add region ps permissions

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

  module Initializations = struct
    let check addr instruction =
      let is_ok =
        try Dba_utils.checksize_instruction instruction
        with  _ -> false
      in
      if not is_ok then begin
          Logger.fatal ~e:(WrongInitializationSize (addr, instruction))
            "@[<hov 0>%@ %a:@ bad initialization size for %a@]"
            Dba_printer.Ascii.pp_code_address addr
            Dba_printer.Ascii.pp_instruction instruction
        end

    let checked_of_list instructions =
      (* init_address seems to be set once and for all *)
      List.iter (check !Dba_types.Caddress.default_init) instructions;
      instructions
  end

  let instruction_size_error instruction =
    Logger.fatal ~e:(Errors.Mismatched_instruction_size instruction)
      "Bad instruction size: %a"
      Dba_printer.Ascii.pp_instruction instruction


  let address_size_error address =
    Logger.fatal ~e: (Errors.Mismatched_address_size address)
      "Bad address size: %a"
      Dba_types.Caddress.pp_base address


  let checked_localized_instruction address instruction =
    if Dba_utils.checksize_address address then
      if Dba_utils.checksize_instruction instruction then
        address, instruction
      else instruction_size_error instruction
    else address_size_error address


  let extract_declaration_data = function
    | Dba.LValue.Var {name; size; info} -> name, size, info
    | Dba.LValue.Restrict _
    | Dba.LValue.Store _ -> assert false

  let fill_sizes declarations initializations =
    let patch_instruction_size = function
      | Dba.Instr.Assign (lv, rv, id) ->
        let lvname =
          match Dba_types.LValue.name_of lv with
          | Some name -> name
          | None -> assert false
          (* initializations should not manipulate tables *)
        in
        let declared_size =
          match Basic_types.String.Map.find lvname declarations with
          | size, _ -> Size.Bit.create size
          | exception Not_found ->
            Logger.fatal "Variable %s was not declared" lvname
        in
        Dba.Instr.assign (Dba.LValue.resize declared_size lv) rv id
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


let expr_of_name name =
  let first_char = name.[0] in
  if first_char = '_' || first_char =  '?' ||  first_char  = '!' then
    let name = if first_char = '_' then "*" else name in
    Dba.Expr.var name 0
  else
    let open Dba.Expr in
    let reg name = var name 32 in
    let eax = reg "eax"
    and ebx = reg "ebx"
    and ecx = reg "ecx"
    and edx = reg "edx" in
    match name with
    | "al"    -> restrict 0 7  eax
    | "ah"    -> restrict 8 15 eax
    | "ax"    -> restrict 0 15 eax
    | "eax"   -> eax
    | "bl"    -> restrict 0 7  ebx
    | "bh"    -> restrict 8 15 ebx
    | "bx"    -> restrict 0 15 ebx
    | "ebx"   -> ebx
    | "cl"    -> restrict 0 7  ecx
    | "ch"    -> restrict 8 15 ecx
    | "cx"    -> restrict 0 15 ecx
    | "ecx"   -> ecx
    | "dl"    -> restrict 0 7  edx
    | "dh"    -> restrict 8 15 edx
    | "dx"    -> restrict 0 15 edx
    | "edx"   -> edx
    | "di"    -> restrict 0 15 (reg "edi")
    | "edi"   -> reg "edi"
    | "si"    -> restrict 0 15 (reg "esi")
    | "esi"   -> reg "esi"
    | "bp"    -> restrict 0 15 (reg "ebp")
    | "ebp"   -> reg "ebp"
    | "sp"    -> restrict 0 15 (reg "esp")
    | "esp"   -> reg "esp"
    | "btemp" -> temporary "btemp" ~size:8
    | "stemp" -> temporary "stemp" ~size:16
    | "temp"  -> temporary "temp"  ~size:32
    | "dtemp" -> temporary "dtemp" ~size:64
    | name    ->
      Logger.error"Unknown variable name: %s" name;
      raise Parsing.Parse_error

let is_wildmetapld_expr = function
  | Dba.(Expr.Var {name; _}) ->
    let c = name.[0] in c = '*' || c = '?' || c = '!'
  | _ -> false


let rec patch_expr_size e sz =
  let open Dba.Expr in
  Logger.debug ~level:2
    "Will patch: %a with %d" Dba_printer.Ascii.pp_bl_term e sz;
  match e with
  | Dba.(Expr.Var {name; info = tag; _}) ->
    if is_wildmetapld_expr e then e
    else var name sz ~tag
  | Dba.Expr.Cst(region ,bv) ->
    constant ~region (Bitvector.create (Bitvector.value_of bv) sz)
  | Dba.Expr.Load(_old_sz, en, e) ->
     let bysz = Size.(Bit.create sz |> Byte.of_bitsize) in
     load bysz en e
  | Dba.Expr.Unary(Dba.Unary_op.Uext size, e) ->
    uext size (patch_expr_size e (sz - size))
  | Dba.Expr.Unary(Dba.Unary_op.Sext size, e) ->
    sext size (patch_expr_size e (sz - size))
  | Dba.Expr.Unary(op, e) -> unary op (patch_expr_size e sz)
  | _ -> e


module Message = struct
  module Value = struct
    type t =
      | Hex of int
      | Bin of int
      | Int of int
      | Str of string

    let vstr v = Str v
    let vhex v = Hex (int_of_string v)
    let vbin v = Bin (int_of_string v)
    let vint v = Int (int_of_string v)
  end

end

module Initialization = struct
  open Dba

  type rvalue =
    | Signed_interval   of Dba.Expr.t * Dba.Expr.t
    | Unsigned_interval of Dba.Expr.t * Dba.Expr.t
    | Set of Dba.Expr.t list
    | Singleton of Dba.Expr.t

  type identifier = string
  type operation =
    | Assignment of Dba.LValue.t * rvalue * identifier option
    | Mem_load   of Bitvector.t * int
    | Universal  of Dba.LValue.t

  type t = {
      controlled: bool;
      operation : operation
    }

  let create ~controlled ~operation = { controlled; operation; }

  let assign ?identifier ?(controlled=true) lval rval =
    let operation = Assignment (lval, rval, identifier) in
    create ~controlled ~operation

  let universal lval =
    create ~controlled:false ~operation:(Universal lval)

  let from_assignment ?(controlled=true) = function
    | Dba.Instr.Assign(lval, rval, _) -> assign ~controlled lval (Singleton rval)
    | _ -> failwith "initialization with non assignment"

  let from_store ?(controlled=true) lv =
    Logger.debug "Init from store %a" Dba_printer.Ascii.pp_lhs lv;
    match lv with
    | LValue.Store(size, _, Dba.Expr.Cst(_, bv)) ->
      begin
        assert (Bitvector.size_of bv = Kernel_options.Machine.word_size ());
        let operation = Mem_load (bv, size) in
        create ~controlled ~operation
      end
    | _ -> failwith "initialization from file with non store"

  let set_control controlled t =
    match t.operation with
    | Universal _ -> t
    | _ -> { t with controlled }
end

let mk_patches l =
  List.fold_left
    (fun vmap (vaddr, opcode) ->
      Virtual_address.Map.add (Virtual_address.create vaddr) opcode vmap)
    Virtual_address.Map.empty l
