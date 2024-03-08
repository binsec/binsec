(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
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

open Loader_buf
module Map = Basic_types.Int.Map
module Set = Basic_types.Int.Set

type rule = Undef | Same | Value of Dba.Expr.t

type entry = {
  addresses : int Interval.t;
  cfa : Dba.Expr.t;
  registers : rule Map.t;
}

let addresses { addresses; _ } = addresses
let cfa { cfa; _ } = cfa

let rule x { registers; _ } =
  match Map.find x registers with exception Not_found -> Undef | rule -> rule

module Operator = struct
  type t =
    | Advance_loc of int
    | Offset of int * int
    | Restore of int
    | Nop
    | Set_loc of int
    | Undefined of int
    | Same_value of int
    | Register of int * int
    | Remember_state
    | Restore_state
    | Def_cfa of int * int
    | Def_cfa_register of int
    | Def_cfa_offset of int
    | Def_cfa_expression of Dwarf_expr.t
    | Expression of int * Dwarf_expr.t
    | Val_offset of int * int
    | Val_expression of int * Dwarf_expr.t

  let pp ppf = function
    | Advance_loc d -> Format.fprintf ppf "DW_CFA_advance_loc (%d)" d
    | Offset (r, o) ->
        Format.fprintf ppf "DW_CFA_offset (%a, %d)" Dba_printer.Ascii.pp_bl_term
          (Dwarf_expr.map r) o
    | Restore r ->
        Format.fprintf ppf "DW_CFA_restore (%a)" Dba_printer.Ascii.pp_bl_term
          (Dwarf_expr.map r)
    | Nop -> Format.fprintf ppf "DW_CFA_nop"
    | Set_loc a -> Format.fprintf ppf "DW_CFA_set_loc (0x%x)" a
    | Undefined r ->
        Format.fprintf ppf "DW_CFA_undefined (%a)" Dba_printer.Ascii.pp_bl_term
          (Dwarf_expr.map r)
    | Same_value r ->
        Format.fprintf ppf "DW_CFA_same_value (%a)" Dba_printer.Ascii.pp_bl_term
          (Dwarf_expr.map r)
    | Register (r, o) ->
        Format.fprintf ppf "DW_CFA_register (%a, %d)"
          Dba_printer.Ascii.pp_bl_term (Dwarf_expr.map r) o
    | Remember_state -> Format.fprintf ppf "DW_CFA_remember_state"
    | Restore_state -> Format.fprintf ppf "DW_CFA_restore_state"
    | Def_cfa (r, o) ->
        Format.fprintf ppf "DW_CFA_def_cfa (%a, %d)"
          Dba_printer.Ascii.pp_bl_term (Dwarf_expr.map r) o
    | Def_cfa_register r ->
        Format.fprintf ppf "DW_CFA_def_cfa_register (%a)"
          Dba_printer.Ascii.pp_bl_term (Dwarf_expr.map r)
    | Def_cfa_offset o -> Format.fprintf ppf "DW_CFA_def_cfa_offset (%d)" o
    | Def_cfa_expression _ -> Format.fprintf ppf "DW_CFA_def_cfa_expression"
    | Expression _ -> Format.fprintf ppf "DW_CFA_expression"
    | Val_offset (r, y) ->
        Format.fprintf ppf "DW_CFA_val_offset (%a, %d)"
          Dba_printer.Ascii.pp_bl_term (Dwarf_expr.map r) y
    | Val_expression _ -> Format.fprintf ppf "DW_CFA_val_expression"

  let load format cursor code_alignment_factor data_alignment_factor : t =
    match Read.u8 cursor with
    | x when 0xC0 land x = 0x40 ->
        Advance_loc (0x3f land x * code_alignment_factor)
    | x when 0xC0 land x = 0x80 ->
        Offset (0x3f land x, Read.uleb128 cursor * data_alignment_factor)
    | x when 0xC0 land x = 0xC0 -> Restore (0x3f land x)
    | 0x00 -> Nop
    | 0x01 -> Set_loc (read format cursor)
    | 0x02 -> Advance_loc (Read.u8 cursor * code_alignment_factor)
    | 0x03 -> Advance_loc (Read.u16 cursor * code_alignment_factor)
    | 0x04 -> Advance_loc (Read.u32 cursor * code_alignment_factor)
    | 0x05 ->
        let reg = Read.uleb128 cursor in
        Offset (reg, Read.uleb128 cursor * data_alignment_factor)
    | 0x06 -> Restore (Read.uleb128 cursor)
    | 0x07 -> Undefined (Read.uleb128 cursor)
    | 0x08 -> Same_value (Read.uleb128 cursor)
    | 0x09 ->
        let reg = Read.uleb128 cursor in
        Register (reg, Read.uleb128 cursor)
    | 0x0a -> Remember_state
    | 0x0b -> Restore_state
    | 0x0c ->
        let reg = Read.uleb128 cursor in
        Def_cfa (reg, Read.uleb128 cursor)
    | 0x0d -> Def_cfa_register (Read.uleb128 cursor)
    | 0x0e -> Def_cfa_offset (Read.uleb128 cursor)
    | 0x0f -> Def_cfa_expression (Dwarf_expr.load format cursor)
    | 0x10 ->
        let reg = Read.uleb128 cursor in
        Expression (reg, Dwarf_expr.load format cursor)
    | 0x11 ->
        let reg = Read.uleb128 cursor in
        Offset (reg, Read.sleb128 cursor * data_alignment_factor)
    | 0x12 ->
        let reg = Read.uleb128 cursor in
        Def_cfa (reg, Read.sleb128 cursor * data_alignment_factor)
    | 0x13 -> Def_cfa_offset (Read.sleb128 cursor * data_alignment_factor)
    | 0x14 ->
        let r = Read.uleb128 cursor in
        Val_offset (r, Read.uleb128 cursor * data_alignment_factor)
    | 0x15 ->
        let r = Read.uleb128 cursor in
        Val_offset (r, Read.sleb128 cursor * data_alignment_factor)
    | 0x16 ->
        let r = Read.uleb128 cursor in
        Val_expression (r, Dwarf_expr.load format cursor)
    | x ->
        Dwarf_options.Logger.fatal "unable to map %d to a known DW_CFA value" x

  let find_reg = function
    | Dba.Expr.Var _ as v -> v
    | Dba.Expr.Binary
        ( (Dba.Binary_op.Plus | Dba.Binary_op.Minus),
          (Dba.Expr.Var _ as v),
          Dba.Expr.Cst _ )
    | Dba.Expr.Binary (Dba.Binary_op.Plus, Dba.Expr.Cst _, (Dba.Expr.Var _ as v))
      ->
        v
    | loc ->
        raise
        @@ Invalid_argument
             (Format.asprintf "Loc %a has no base register."
                Dba_printer.Ascii.pp_bl_term loc)

  let find_offset = function
    | Dba.Expr.Var _ as k -> Dba.Expr.(zeros (size_of k))
    | Dba.Expr.Binary (Dba.Binary_op.Plus, Dba.Expr.Var _, (Dba.Expr.Cst _ as c))
    | Dba.Expr.Binary (Dba.Binary_op.Plus, (Dba.Expr.Cst _ as c), Dba.Expr.Var _)
      ->
        c
    | Dba.Expr.Binary
        (Dba.Binary_op.Minus, Dba.Expr.Var _, (Dba.Expr.Cst _ as c)) ->
        Dba.Expr.uminus c
    | loc ->
        Dwarf_options.Logger.fatal
          "trying to get offset of the non offsetable location %a"
          Dba_printer.Ascii.pp_bl_term loc

  let finalise cfa registers = Map.map (fun f -> f cfa) registers

  let add_offset e offset =
    Dba.Expr.(add e (constant (Bitvector.of_int ~size:(size_of e) offset)))

  let load_at e =
    Dba.Expr.load
      Size.(Byte.of_bitsize (Bit.create (Dba.Expr.size_of e)))
      (Kernel_options.Machine.endianness ())
      e

  let rec interp entries address frontier cfa columns states = function
    | [] ->
        ( Map.keys columns,
          List.rev
            ({
               addresses = { Interval.lo = address; hi = frontier };
               cfa;
               registers = finalise cfa columns;
             }
            :: entries) )
    | Nop :: tl -> interp entries address frontier cfa columns states tl
    | Advance_loc delta :: tl ->
        let address' = address + delta in
        let entries =
          {
            addresses = { Interval.lo = address; hi = address' - 1 };
            cfa;
            registers = finalise cfa columns;
          }
          :: entries
        in
        if frontier <= address' then (Map.keys columns, List.rev entries)
        else interp entries address' frontier cfa columns states tl
    | Offset (reg, offset) :: tl ->
        interp entries address frontier cfa
          (Map.add reg
             (fun cfa -> Value (load_at (add_offset cfa offset)))
             columns)
          states tl
    | Restore reg :: tl ->
        let state = List.fold_left (fun _ state -> state) Map.empty states in
        interp entries address frontier cfa
          (Map.add reg
             (try Map.find reg state with Not_found -> fun _ -> Undef)
             columns)
          states tl
    | Set_loc address' :: tl ->
        let entries =
          {
            addresses = { Interval.lo = address; hi = address' - 1 };
            cfa;
            registers = finalise cfa columns;
          }
          :: entries
        in
        if frontier <= address' then (Map.keys columns, List.rev entries)
        else interp entries address' frontier cfa columns states tl
    | Undefined reg :: tl ->
        interp entries address frontier cfa
          (Map.add reg (fun _ -> Undef) columns)
          states tl
    | Same_value reg :: tl ->
        interp entries address frontier cfa
          (Map.add reg (fun _ -> Same) columns)
          states tl
    | Register (reg, reg') :: tl ->
        interp entries address frontier cfa
          (Map.add reg (fun _ -> Value (Dwarf_expr.map reg')) columns)
          states tl
    | Remember_state :: tl ->
        interp entries address frontier cfa columns (columns :: states) tl
    | Restore_state :: tl ->
        interp entries address frontier cfa (List.hd states) (List.tl states) tl
    | Def_cfa (reg, offset) :: tl ->
        interp entries address frontier
          (add_offset (Dwarf_expr.map reg) offset)
          columns states tl
    | Def_cfa_register reg :: tl ->
        let offset = find_offset cfa in
        interp entries address frontier
          (Dba.Expr.add (Dwarf_expr.map reg) offset)
          columns states tl
    | Def_cfa_offset offset :: tl ->
        let reg = find_reg cfa in
        interp entries address frontier (add_offset reg offset) columns states
          tl
    | Def_cfa_expression e :: tl ->
        interp entries address frontier (Dwarf_expr.loc e) columns states tl
    | Expression (reg, e) :: tl ->
        interp entries address frontier cfa
          (Map.add reg
             (fun cfa -> Value (load_at (Dwarf_expr.loc ~cfa e)))
             columns)
          states tl
    | Val_offset (reg, offset) :: tl ->
        interp entries address frontier cfa
          (Map.add reg (fun cfa -> Value (add_offset cfa offset)) columns)
          states tl
    | Val_expression (reg, e) :: tl ->
        interp entries address frontier cfa
          (Map.add reg (fun cfa -> Value (Dwarf_expr.loc ~cfa e)) columns)
          states tl
end

module Cie = struct
  type t = {
    offset : int;
    format : [ `x32 | `x64 ];
    version : int;
    augmentation : string;
    code_alignment_factor : int;
    data_alignment_factor : int;
    return_address_register : int;
    augmentation_data : string;
    initial_instructions : Operator.t list;
  }

  let pp =
    let columns = [| Prettytbl.Column.default; Prettytbl.Column.default |] in
    fun ppf cie ->
      Format.fprintf ppf "@[<v 2>%a CIE@ "
        Machine.(Bitwidth.pp_print_hex (cie.format :> bitwidth))
        cie.offset;
      let t = Prettytbl.make columns in
      Prettytbl.append t [| "Version:"; string_of_int cie.version |];
      Prettytbl.append t [| "Augmentation:"; cie.augmentation |];
      Prettytbl.append t
        [| "Code alignement factor:"; string_of_int cie.code_alignment_factor |];
      Prettytbl.append t
        [| "Data alignement factor:"; string_of_int cie.data_alignment_factor |];
      Prettytbl.append t
        [|
          "Return address column:"; string_of_int cie.return_address_register;
        |];
      if String.length cie.augmentation_data > 0 then
        Prettytbl.append t
          [| "Augmentation data:"; String_utils.to_hex cie.augmentation_data |];
      Prettytbl.pp ppf t;
      Format.pp_print_space ppf ();
      List.iter
        (fun op -> Format.fprintf ppf "@ %a" Operator.pp op)
        cie.initial_instructions;
      Format.pp_close_box ppf ()

  let load_eh_frame format cursor offset =
    let version = Read.u8 cursor in
    if version <> 1 then
      Dwarf_options.Logger.fatal
        "trying to read .eh_frame with CIE version <> 1";
    let augmentation = Read.zero_string "" cursor () in
    if augmentation <> "zR" then
      Dwarf_options.Logger.fatal
        "trying to read .eh_frame with CIE augmentation <> \"zR\"";
    let code_alignment_factor = Read.uleb128 cursor in
    let data_alignment_factor = Read.sleb128 cursor in
    let return_address_register = Read.uleb128 cursor in
    (* here we assumes augmentation = "zR" *)
    let augmentation_size = Read.uleb128 cursor in
    let augmentation_data =
      String.init augmentation_size (fun _ -> Char.chr (Read.u8 cursor))
    in
    let initial_instructions =
      let rec loop instrs caf daf =
        if at_end cursor then List.rev instrs
        else loop (Operator.load format cursor caf daf :: instrs) caf daf
      in
      loop [] code_alignment_factor data_alignment_factor
    in
    {
      offset;
      format;
      version;
      augmentation;
      code_alignment_factor;
      data_alignment_factor;
      return_address_register;
      augmentation_data;
      initial_instructions;
    }

  let load_debug_frame format cursor offset =
    let version = Read.u8 cursor in
    if version <> 1 then
      Dwarf_options.Logger.fatal
        "trying to read .debug_frame with CIE version <> 1";
    let augmentation = Read.zero_string "" cursor () in
    if augmentation <> "" then
      Dwarf_options.Logger.fatal
        "trying to read .debug_frame with non empty CIE augmentation";
    let code_alignment_factor = Read.uleb128 cursor in
    let data_alignment_factor = Read.sleb128 cursor in
    let return_address_register = Read.uleb128 cursor in
    let initial_instructions =
      let rec loop instrs caf daf =
        if at_end cursor then List.rev instrs
        else loop (Operator.load format cursor caf daf :: instrs) caf daf
      in
      loop [] code_alignment_factor data_alignment_factor
    in
    {
      offset;
      format;
      version;
      augmentation;
      code_alignment_factor;
      data_alignment_factor;
      return_address_register;
      augmentation_data = "";
      initial_instructions;
    }
end

module Fde = struct
  type t = {
    offset : int;
    cie : Cie.t;
    initial_location : int;
    address_range : int;
    augmentation_data : string;
    instructions : Operator.t list;
  }

  let pp ppf fde =
    let pp_print_hex =
      Machine.(Bitwidth.pp_print_hex (fde.cie.Cie.format :> bitwidth))
    in
    Format.fprintf ppf "@[<v 2>%a FDE cie=%a pc=%a..%a" pp_print_hex fde.offset
      pp_print_hex fde.cie.Cie.offset pp_print_hex fde.initial_location
      pp_print_hex
      (fde.initial_location + fde.address_range);
    List.iter
      (fun op -> Format.fprintf ppf "@ %a" Operator.pp op)
      fde.instructions;
    Format.pp_close_box ppf ()

  let load_eh_frame format cursor (vaddr : Virtual_address.t) offset cie =
    (* TODO here we assumes augmentation = "zR" *)
    let initial_location =
      (vaddr :> int)
      + offset + cursor.position
      + read ~signed:true format cursor
    in
    let address_range = read format cursor in
    let augmentation_size =
      if cie.Cie.augmentation_data <> "" then Read.uleb128 cursor else 0
    in
    let augmentation_data =
      String.init augmentation_size (fun _ -> Char.chr (Read.u8 cursor))
    in
    let instructions =
      let rec loop instrs caf daf =
        if at_end cursor then List.rev instrs
        else loop (Operator.load format cursor caf daf :: instrs) caf daf
      in
      loop [] cie.Cie.code_alignment_factor cie.Cie.data_alignment_factor
    in
    {
      offset;
      cie;
      initial_location;
      augmentation_data;
      address_range;
      instructions;
    }

  let load_debug_frame format cursor offset cie =
    (* TODO this code work only for object files... *)
    let initial_location = Dwarf_utils.read_addr cursor in
    let address_range = Dwarf_utils.read_addr cursor in
    let instructions =
      let rec loop instrs caf daf =
        if at_end cursor then List.rev instrs
        else loop (Operator.load format cursor caf daf :: instrs) caf daf
      in
      loop [] cie.Cie.code_alignment_factor cie.Cie.data_alignment_factor
    in
    {
      offset;
      cie;
      initial_location;
      augmentation_data = "";
      address_range;
      instructions;
    }
end

let pp ppf t =
  Format.pp_open_vbox ppf 0;
  Format.fprintf ppf "Contents of the .eh_frame section:";
  ignore
  @@ List.fold_left
       (fun cies ({ Fde.cie; _ } as fde) ->
         let cies =
           match Set.mem cie.Cie.offset cies with
           | false ->
               Format.pp_print_space ppf ();
               Format.pp_print_space ppf ();
               Cie.pp ppf cie;
               Set.add cie.Cie.offset cies
           | true -> cies
         in
         Format.pp_print_space ppf ();
         Format.pp_print_space ppf ();
         Fde.pp ppf fde;
         cies)
       Set.empty t;
  Format.pp_close_box ppf ()

let load_eh_frame cursor (vaddr : Virtual_address.t) : Fde.t list =
  let rec loop cursor cies fdes =
    if at_end cursor then List.rev fdes
    else
      let offset = cursor.position in
      let format, length =
        match Read.u32 cursor with
        | 0xffffffff -> (`x64, Read.u64 cursor)
        | length -> (`x32, length)
      in
      if length = 0 then List.rev fdes
      else
        let cursor' = sub cursor length in
        match read format cursor' with
        | 0 ->
            loop cursor (Cie.load_eh_frame format cursor' offset :: cies) fdes
        | cie_ptr ->
            let cie =
              List.find
                (function
                  | { Cie.offset = offset'; _ } ->
                      cie_ptr = offset + cursor'.position - offset')
                cies
            in
            loop cursor cies
              (Fde.load_eh_frame format cursor' vaddr offset cie :: fdes)
  in
  loop cursor [] []

let load_debug_frame cursor : Fde.t list =
  let rec loop cursor cies fdes =
    if at_end cursor then List.rev fdes
    else
      let offset = cursor.position in
      let format, length =
        match Read.u32 cursor with
        | 0xffffffff -> (`x64, Read.u64 cursor)
        | length -> (`x32, length)
      in
      let cursor' = sub cursor length in
      match (format, read format cursor') with
      | `x32, 0xffffffff | `x64, 0x7fffffffffffffff ->
          loop cursor (Cie.load_debug_frame format cursor' offset :: cies) fdes
      | _, cie_ptr ->
          let cie =
            List.find
              (function { Cie.offset = offset'; _ } -> cie_ptr = offset')
              cies
          in
          loop cursor cies
            (Fde.load_debug_frame format cursor' offset cie :: fdes)
  in
  loop cursor [] []

type t = (Fde.t * int array * entry list) list

let load img : t =
  let fdes =
    try
      let section = Loader_utils.find_section_by_name ".eh_frame" img in
      let at = (Loader.Section.pos section).Loader_types.raw in
      let length = (Loader.Section.size section).Loader_types.raw in
      let cursor = sub (Loader.Img.cursor ~at img) length in
      let vaddr = (Loader.Section.pos section).Loader_types.virt in
      load_eh_frame cursor (Virtual_address.create vaddr)
    with Not_found ->
      let section = Loader_utils.find_section_by_name ".debug_frame" img in
      let at = (Loader.Section.pos section).Loader_types.raw in
      let length = (Loader.Section.size section).Loader_types.raw in
      let cursor = sub (Loader.Img.cursor ~at img) length in
      load_debug_frame cursor
  in
  Dwarf_options.Logger.debug "%a" pp fdes;
  List.map
    (fun fde ->
      let columns, entries =
        Operator.interp [] fde.Fde.initial_location
          (fde.Fde.initial_location + fde.Fde.address_range)
          Dba.Expr.zero Map.empty []
          (List.concat
             [
               fde.Fde.cie.Cie.initial_instructions;
               [ Operator.Remember_state ];
               fde.Fde.instructions;
             ])
      in
      (fde, Array.of_list columns, entries))
    fdes

let fold f a t =
  List.fold_left
    (fun a ({ Fde.cie; _ }, columns, entries) ->
      List.fold_left
        (fun a entry ->
          f a ~return_address:cie.Cie.return_address_register ~columns entry)
        a entries)
    a t

let iter f t =
  List.iter
    (function
      | { Fde.cie; _ }, columns, entries ->
          List.iter
            (fun entry ->
              f ~return_address:cie.Cie.return_address_register ~columns entry)
            entries)
    t

let pp ppf t =
  List.iter
    (function
      | { Fde.cie; initial_location; address_range; _ }, columns, entries ->
          let pp_print_hex =
            Machine.(Bitwidth.pp_print_hex (cie.Cie.format :> bitwidth))
          in
          Format.fprintf ppf "@[<v>FDE pc=%a..%a@ " pp_print_hex
            initial_location pp_print_hex
            (initial_location + address_range);
          let n = Array.length columns + 2 in
          let t = Prettytbl.(make (Array.make n Column.default)) in
          Prettytbl.append t
            (Array.init n (function
              | 0 -> "LOC"
              | 1 -> "CFA"
              | x ->
                  let x = columns.(x - 2) in
                  if x = cie.Cie.return_address_register then "RA"
                  else
                    Format.asprintf "%a" Dba_printer.Ascii.pp_bl_term
                      (Dwarf_expr.map x)));
          List.iter
            (function
              | { addresses = { Interval.lo; _ }; cfa; registers } ->
                  Prettytbl.append t
                    (Array.init n (function
                      | 0 -> Format.asprintf "%a" pp_print_hex lo
                      | 1 ->
                          Format.asprintf "%a" Dba_printer.Ascii.pp_bl_term cfa
                      | x -> (
                          match Map.find columns.(x - 2) registers with
                          | exception Not_found -> "u"
                          | Undef -> "u"
                          | Same -> "s"
                          | Value e ->
                              Format.asprintf "%a" Dba_printer.Ascii.pp_bl_term
                                e))))
            entries;
          Prettytbl.pp ppf t;
          Format.pp_close_box ppf ();
          Format.pp_print_space ppf ())
    t
