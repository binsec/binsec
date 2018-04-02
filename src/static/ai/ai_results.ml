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

open Format
open Dba_printer.Ascii

let pp_opcode ppf = function
  | Some s ->
    fprintf ppf "%a" Disasm_types.GenericInstruction.pp_opcode s
  | None ->
    fprintf ppf "No opcode"


let initial_block_address_of_addressStack (addr, _, _) =
  if addr.Dba.id = 0 then Some addr else None


let expression_of_instruction instr =
  match instr with
  | Dba.IkMalloc (_, expr, _)
  | Dba.IkFree (expr, _)
  | Dba.IkAssign (_, expr, _)
  | Dba.IkDJump (expr, _) -> Some expr
  | Dba.IkStop _ -> Some (Dba.ExprVar ("eax", 32, None))
  | Dba.IkSJump _
  | Dba.IkIf _
  | Dba.IkAssert _
  | Dba.IkAssume _
  | Dba.IkNondetAssume _
  | Dba.IkNondet _
  | Dba.IkUndef _
  | Dba.IkPrint _ -> None


let pp_cstatus states (_, stack, loop) ppf  = function
  | Dba.IkIf (_, Dba.JOuter address1, offset) ->
    let p addr loop =
      Dba_types.AddressStack.Map.mem (addr, stack, loop) states in
    let address2 = Dba_types.Caddress.reid address1 offset in
    if p address1 loop && p address2 loop
    || p address1 (succ loop) && p address2 (succ loop)
    || p address1 loop && p address2 (succ loop)
    then fprintf ppf "Maybe"
    else if p address1 loop || p address1 (succ loop)
    then fprintf ppf "True"
    else fprintf ppf "False"
  | Dba.IkAssign _
  | Dba.IkSJump _
  | Dba.IkDJump _
  | Dba.IkIf _
  | Dba.IkStop _
  | Dba.IkAssert _
  | Dba.IkAssume _
  | Dba.IkNondetAssume _
  | Dba.IkNondet _
  | Dba.IkUndef _
  | Dba.IkMalloc _
  | Dba.IkFree _
  | Dba.IkPrint _ -> fprintf ppf ""


let get_fun_addr instr addr djmps =
  match instr with
  | Dba.IkDJump (_, Some (Dba.Call _fun_addr)) ->
      begin
        try let addrSet = Dba_types.AddressStack.Map.find addr djmps in
            Some addrSet
        with Not_found -> None
      end
  | Dba.IkSJump (Dba.JOuter addr, Some (Dba.Call _fun_addr)) ->
    Some (Dba_types.Caddress.Set.singleton addr)
  | _ -> None


let pp_entry_points states instructions ppf djmps =
  let iter = ref 0 in
  let pp_aux addrStack _ visited =
    let addr, _, n = addrStack in
    try
      let instr, _opcode = Dba_types.Caddress.Map.find addr instructions in
      match get_fun_addr instr addrStack djmps with
      | None -> visited
      | Some fun_addrs ->
        Dba_types.Caddress.Set.fold
          (fun fun_addr visited ->
          if not (Dba_types.Caddress.Set.mem fun_addr visited)
          then begin
            fprintf ppf "fun_%d: %a (%d)@ " !iter pp_code_address fun_addr n;
            incr iter;
            Dba_types.Caddress.Set.add fun_addr visited
          end
          else visited
        ) fun_addrs visited
    with Not_found -> assert false
  in
  pp_open_vbox ppf 0;
  let eps =
    Dba_types.AddressStack.Map.fold pp_aux states Dba_types.Caddress.Set.empty
  in
  pp_close_box ppf ();
  if Dba_types.Caddress.Set.is_empty eps then
    fprintf ppf "No function entry point detected"


let pp_dynamic_jumps ppf djmps =
  let pp_addr_set ppf aset =
    Dba_types.Caddress.Set.iter
      (fun e -> fprintf ppf "%a;@ " Dba_printer.Ascii.pp_code_address e)
      aset
  in
  pp_open_vbox ppf 0;
  Dba_types.AddressStack.Map.iter (
    fun (a, _, _) a_set ->
      fprintf ppf "%a : goto @[<hov 0]{%a}@]@ " pp_code_address a pp_addr_set a_set
  ) djmps;
  pp_close_box ppf ();
  if Dba_types.AddressStack.Map.is_empty djmps then
    fprintf ppf "No dynamic jumps "


let pp_conditions ppf rcd_conds =
  let pp_aux addr (_, comp_cond) =
    fprintf ppf "%a: %a@ " pp_code_address addr pp_cond comp_cond
  in
  pp_open_vbox ppf 0;
  Dba_types.Caddress.Map.iter pp_aux rcd_conds;
  pp_close_box ppf ()


let pp_instructions states instructions regs_of_state ppf rcd_conds =
  let pp_aux addrStack state =
    let addr = initial_block_address_of_addressStack addrStack in
    match addr with
    | None -> ()
    | Some addr ->
      try
        let instr, opcode = Dba_types.Caddress.Map.find addr instructions in
        let expr = expression_of_instruction instr in
        let m, flags, equalities, _preds = state in
        let pp_regs ppf = function
          | None -> fprintf ppf ""
          | Some expr ->
            regs_of_state expr ppf (m, flags, equalities)
        in
        let natural_cond ppf () =
          try
            let _, cond = Dba_types.Caddress.Map.find addr rcd_conds in
            Dba_printer.Ascii.pp_cond ppf cond
          with Not_found -> fprintf ppf ""
        in
        Format.fprintf ppf "@[<h>%a: %a @[%a %a %a@]@]@ "
          Dba_types.AddressStack.pp addrStack
          pp_opcode opcode
          pp_regs expr
          (pp_cstatus states addrStack) instr
          natural_cond ()
      with
      | Not_found -> assert false
  in
  Format.fprintf ppf "@[<v 0>";
  if Dba_types.AddressStack.Map.is_empty states then
    Format.fprintf ppf "No analyzed instruction@ "
  else
    Dba_types.AddressStack.Map.iter pp_aux states;
  Format.fprintf ppf "@]"


let pp_section ppf section_name =
  fprintf ppf "#%s" section_name


let pp_results ppf states instructions rcd_conds djmps regs_of_state =
  fprintf ppf
    "@[<v 0>\
    %a@ %a@ @ \
    %a@ %a@ @ \
    %a@ %a@ @ \
    %a@ %a@ @ \
     @]"
    pp_section "Analyzed instructions"
    (pp_instructions states instructions regs_of_state) rcd_conds
    pp_section "Function entry points"
    (pp_entry_points states instructions) djmps
    pp_section "Dynamic jumps"
    pp_dynamic_jumps djmps
    pp_section "Conditions"
    pp_conditions rcd_conds


let pp_file ?(filename="ai_results.txt")
    states instructions rcd_conds djmps regs_of_state =
  let oc = open_out_bin filename in
  Logger.debug ~level:3 "Outputting AI results to %s" filename;
  let ppf = formatter_of_out_channel oc in
  pp_results ppf states instructions rcd_conds djmps regs_of_state;
  close_out oc
