(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

open Basic_types.Integers
open Reader
module Map = Basic_types.Integers.Int.Map
module Set = Basic_types.Integers.Int.Set

type rule = Undef | Same | Value of Dba.Expr.t

type entry = {
  addresses : Virtual_address.t Interval.t;
  cfa : Dba.Expr.t;
  registers : rule Map.t;
}

let addresses { addresses; _ } = addresses
let cfa { cfa; _ } = cfa

let rule x { registers; _ } =
  match Map.find x registers with exception Not_found -> Undef | rule -> rule

module Operator = struct
  type t =
    | Advance_loc of Z.t
    | Offset of int * Z.t
    | Restore of int
    | Nop
    | Set_loc of Virtual_address.t
    | Undefined of int
    | Same_value of int
    | Register of int * int
    | Remember_state
    | Restore_state
    | Def_cfa of int * Z.t
    | Def_cfa_register of int
    | Def_cfa_offset of Z.t
    | Def_cfa_expression of Expr.t
    | Expression of int * Expr.t
    | Val_offset of int * Z.t
    | Val_expression of int * Expr.t

  let pp ppf = function
    | Advance_loc d -> Format.fprintf ppf "DW_CFA_advance_loc (%a)" Z.pp_print d
    | Offset (r, o) ->
        Format.fprintf ppf "DW_CFA_offset (%d, %a)" r Z.pp_print o
    | Restore r -> Format.fprintf ppf "DW_CFA_restore (%d)" r
    | Nop -> Format.fprintf ppf "DW_CFA_nop"
    | Set_loc a -> Format.fprintf ppf "DW_CFA_set_loc (%a)" Virtual_address.pp a
    | Undefined r -> Format.fprintf ppf "DW_CFA_undefined (%d)" r
    | Same_value r -> Format.fprintf ppf "DW_CFA_same_value (%d)" r
    | Register (r, o) -> Format.fprintf ppf "DW_CFA_register (%d, %d)" r o
    | Remember_state -> Format.fprintf ppf "DW_CFA_remember_state"
    | Restore_state -> Format.fprintf ppf "DW_CFA_restore_state"
    | Def_cfa (r, o) ->
        Format.fprintf ppf "DW_CFA_def_cfa (%d, %a)" r Z.pp_print o
    | Def_cfa_register r -> Format.fprintf ppf "DW_CFA_def_cfa_register (%d)" r
    | Def_cfa_offset o ->
        Format.fprintf ppf "DW_CFA_def_cfa_offset (%a)" Z.pp_print o
    | Def_cfa_expression _ -> Format.fprintf ppf "DW_CFA_def_cfa_expression"
    | Expression _ -> Format.fprintf ppf "DW_CFA_expression"
    | Val_offset (r, y) ->
        Format.fprintf ppf "DW_CFA_val_offset (%d, %a)" r Z.pp_print y
    | Val_expression _ -> Format.fprintf ppf "DW_CFA_val_expression"

  let load isa format cursor code_alignment_factor data_alignment_factor : t =
    match Uint8.to_int (Read.u8 cursor) with
    | x when 0xC0 land x = 0x40 ->
        Advance_loc (Z.mul (Z.of_int (0x3f land x)) code_alignment_factor)
    | x when 0xC0 land x = 0x80 ->
        Offset (0x3f land x, Z.mul (Read.uleb128 cursor) data_alignment_factor)
    | x when 0xC0 land x = 0xC0 -> Restore (0x3f land x)
    | 0x00 -> Nop
    | 0x01 -> Set_loc (Utils.read_address format cursor)
    | 0x02 ->
        Advance_loc
          (Z.mul (Uint8.to_bigint (Read.u8 cursor)) code_alignment_factor)
    | 0x03 ->
        Advance_loc
          (Z.mul (Uint16.to_bigint (Read.u16 cursor)) code_alignment_factor)
    | 0x04 ->
        Advance_loc
          (Z.mul (Uint32.to_bigint (Read.u32 cursor)) code_alignment_factor)
    | 0x05 ->
        let reg = Z.to_int (Read.uleb128 cursor) in
        Offset (reg, Z.mul (Read.uleb128 cursor) data_alignment_factor)
    | 0x06 -> Restore (Z.to_int (Read.uleb128 cursor))
    | 0x07 -> Undefined (Z.to_int (Read.uleb128 cursor))
    | 0x08 -> Same_value (Z.to_int (Read.uleb128 cursor))
    | 0x09 ->
        let reg = Z.to_int (Read.uleb128 cursor) in
        Register (reg, Z.to_int (Read.uleb128 cursor))
    | 0x0a -> Remember_state
    | 0x0b -> Restore_state
    | 0x0c ->
        let reg = Z.to_int (Read.uleb128 cursor) in
        Def_cfa (reg, Read.uleb128 cursor)
    | 0x0d -> Def_cfa_register (Z.to_int (Read.uleb128 cursor))
    | 0x0e -> Def_cfa_offset (Read.uleb128 cursor)
    | 0x0f ->
        let size = Z.to_int (Read.uleb128 cursor) in
        Def_cfa_expression (Expr.load isa format size cursor)
    | 0x10 ->
        let reg = Z.to_int (Read.uleb128 cursor) in
        let size = Z.to_int (Read.uleb128 cursor) in
        Expression (reg, Expr.load isa format size cursor)
    | 0x11 ->
        let reg = Z.to_int (Read.uleb128 cursor) in
        Offset (reg, Z.mul (Read.sleb128 cursor) data_alignment_factor)
    | 0x12 ->
        let reg = Z.to_int (Read.uleb128 cursor) in
        Def_cfa (reg, Z.mul (Read.sleb128 cursor) data_alignment_factor)
    | 0x13 -> Def_cfa_offset (Z.mul (Read.sleb128 cursor) data_alignment_factor)
    | 0x14 ->
        let r = Z.to_int (Read.uleb128 cursor) in
        Val_offset (r, Z.mul (Read.uleb128 cursor) data_alignment_factor)
    | 0x15 ->
        let r = Z.to_int (Read.uleb128 cursor) in
        Val_offset (r, Z.mul (Read.sleb128 cursor) data_alignment_factor)
    | 0x16 ->
        let r = Z.to_int (Read.uleb128 cursor) in
        let size = Z.to_int (Read.uleb128 cursor) in
        Val_expression (r, Expr.load isa format size cursor)
    | x -> Logger.fatal "unable to map %d to a known DW_CFA value" x

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
        Logger.fatal "trying to get offset of the non offsetable location %a"
          Dba_printer.Ascii.pp_bl_term loc

  let finalise cfa registers = Map.map (fun f -> f cfa) registers

  let add_offset e offset =
    Dba.Expr.(add e (constant (Bitvector.create offset (size_of e))))

  let load_at isa e =
    Dba.Expr.load
      Size.(Byte.of_bitsize (Bit.create (Dba.Expr.size_of e)))
      (Machine.ISA.endianness isa)
      e

  let rec interp :
      Machine.t ->
      entry list ->
      Virtual_address.t ->
      Virtual_address.t ->
      Dba.Expr.t ->
      (Dba.Expr.t -> rule) Map.t ->
      (Dba.Expr.t * (Dba.Expr.t -> rule) Map.t) list ->
      t list ->
      int list * entry list =
   fun isa entries address frontier cfa columns states instrs ->
    match instrs with
    | [] ->
        ( Map.keys columns,
          List.rev
            ({
               addresses = { Interval.lo = address; hi = frontier };
               cfa;
               registers = finalise cfa columns;
             }
            :: entries) )
    | Nop :: tl -> interp isa entries address frontier cfa columns states tl
    | Advance_loc delta :: tl ->
        let address' = Virtual_address.add_bigint delta address in
        let entries =
          {
            addresses =
              { Interval.lo = address; hi = Virtual_address.pred address' };
            cfa;
            registers = finalise cfa columns;
          }
          :: entries
        in
        if frontier <= address' then (Map.keys columns, List.rev entries)
        else interp isa entries address' frontier cfa columns states tl
    | Offset (reg, offset) :: tl ->
        interp isa entries address frontier cfa
          (Map.add reg
             (fun cfa -> Value (load_at isa (add_offset cfa offset)))
             columns)
          states tl
    | Restore reg :: tl ->
        let state =
          List.fold_left (fun _ (_, state) -> state) Map.empty states
        in
        interp isa entries address frontier cfa
          (Map.add reg
             (try Map.find reg state with Not_found -> fun _ -> Undef)
             columns)
          states tl
    | Set_loc address' :: tl ->
        let entries =
          {
            addresses =
              { Interval.lo = address; hi = Virtual_address.pred address' };
            cfa;
            registers = finalise cfa columns;
          }
          :: entries
        in
        if frontier <= address' then (Map.keys columns, List.rev entries)
        else interp isa entries address' frontier cfa columns states tl
    | Undefined reg :: tl ->
        interp isa entries address frontier cfa
          (Map.add reg (fun _ -> Undef) columns)
          states tl
    | Same_value reg :: tl ->
        interp isa entries address frontier cfa
          (Map.add reg (fun _ -> Same) columns)
          states tl
    | Register (reg, reg') :: tl ->
        interp isa entries address frontier cfa
          (Map.add reg (fun _ -> Value (Expr.map isa reg')) columns)
          states tl
    | Remember_state :: tl ->
        interp isa entries address frontier cfa columns
          ((cfa, columns) :: states) tl
    | Restore_state :: tl ->
        let cfa, collumns = List.hd states in
        interp isa entries address frontier cfa collumns (List.tl states) tl
    | Def_cfa (reg, offset) :: tl ->
        interp isa entries address frontier
          (add_offset (Expr.map isa reg) offset)
          columns states tl
    | Def_cfa_register reg :: tl ->
        let offset = find_offset cfa in
        interp isa entries address frontier
          (Dba.Expr.add (Expr.map isa reg) offset)
          columns states tl
    | Def_cfa_offset offset :: tl ->
        let reg = find_reg cfa in
        interp isa entries address frontier (add_offset reg offset) columns
          states tl
    | Def_cfa_expression e :: tl ->
        interp isa entries address frontier (Expr.cfa isa e) columns states tl
    | Expression (reg, e) :: tl ->
        interp isa entries address frontier cfa
          (Map.add reg
             (fun cfa ->
               Value (Expr.loc isa ~cfa e (Machine.ISA.word_size isa)))
             columns)
          states tl
    | Val_offset (reg, offset) :: tl ->
        interp isa entries address frontier cfa
          (Map.add reg (fun cfa -> Value (add_offset cfa offset)) columns)
          states tl
    | Val_expression (reg, e) :: tl ->
        interp isa entries address frontier cfa
          (Map.add reg
             (fun cfa ->
               Value (Expr.loc isa ~cfa e (Machine.ISA.word_size isa)))
             columns)
          states tl
end

module Cie = struct
  type t = {
    offset : int;
    format : [ `x32 | `x64 ];
    version : int;
    augmentation : string;
    code_alignment_factor : Z.t;
    data_alignment_factor : Z.t;
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
        [| "Code alignement factor:"; Z.to_string cie.code_alignment_factor |];
      Prettytbl.append t
        [| "Data alignement factor:"; Z.to_string cie.data_alignment_factor |];
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

  let load_eh_frame isa format cursor offset =
    let version = Uint8.to_int (Read.u8 cursor) in
    if version <> 1 then
      Logger.fatal "trying to read .eh_frame with CIE version <> 1";
    let augmentation = Read.zero_string "" cursor () in
    if augmentation <> "zR" then
      Logger.fatal "trying to read .eh_frame with CIE augmentation <> \"zR\"";
    let code_alignment_factor = Read.uleb128 cursor in
    let data_alignment_factor = Read.sleb128 cursor in
    let return_address_register = Z.to_int (Read.uleb128 cursor) in
    (* here we assumes augmentation = "zR" *)
    let augmentation_size = Z.to_int (Read.uleb128 cursor) in
    let augmentation_data =
      String.init augmentation_size (fun _ -> Uint8.to_char (Read.u8 cursor))
    in
    let initial_instructions =
      let rec loop instrs caf daf =
        if at_end cursor then List.rev instrs
        else loop (Operator.load isa format cursor caf daf :: instrs) caf daf
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

  let load_debug_frame isa format cursor offset =
    let version = Uint8.to_int (Read.u8 cursor) in
    if version <> 1 then
      Logger.fatal "trying to read .debug_frame with CIE version <> 1";
    let augmentation = Read.zero_string "" cursor () in
    if augmentation <> "" then
      Logger.fatal "trying to read .debug_frame with non empty CIE augmentation";
    let code_alignment_factor = Read.uleb128 cursor in
    let data_alignment_factor = Read.sleb128 cursor in
    let return_address_register = Z.to_int (Read.uleb128 cursor) in
    let initial_instructions =
      let rec loop instrs caf daf =
        if at_end cursor then List.rev instrs
        else loop (Operator.load isa format cursor caf daf :: instrs) caf daf
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
    initial_location : Virtual_address.t;
    address_range : Z.t;
    augmentation_data : string;
    instructions : Operator.t list;
  }

  let pp ppf fde =
    let pp_print_hex =
      Virtual_address.pp_print (fde.cie.Cie.format :> [ `x16 | `x32 | `x64 ])
    and pp_print_hex_offset =
      Machine.Bitwidth.pp_print_hex (fde.cie.Cie.format :> Machine.bitwidth)
    in
    Format.fprintf ppf "@[<v 2>%a FDE cie=%a pc=%a..%a" pp_print_hex_offset
      fde.offset pp_print_hex_offset fde.cie.Cie.offset pp_print_hex
      fde.initial_location pp_print_hex
      (Virtual_address.add_bigint fde.address_range fde.initial_location);
    List.iter
      (fun op -> Format.fprintf ppf "@ %a" Operator.pp op)
      fde.instructions;
    Format.pp_close_box ppf ()

  let load_eh_frame isa format cursor (vaddr : Virtual_address.t) offset cie =
    (* TODO here we assumes augmentation = "zR" *)
    let initial_location =
      Virtual_address.add_bigint
        (Z.of_int64 (Utils.read ~signed:true format cursor))
        (Virtual_address.add_int (get_pos cursor)
           (Virtual_address.add_int offset vaddr))
    in
    let address_range =
      Virtual_address.to_bigint (Utils.read_address format cursor)
    in
    let augmentation_size =
      if cie.Cie.augmentation_data <> "" then Z.to_int (Read.uleb128 cursor)
      else 0
    in
    let augmentation_data =
      String.init augmentation_size (fun _ -> Uint8.to_char (Read.u8 cursor))
    in
    let instructions =
      let rec loop instrs caf daf =
        if at_end cursor then List.rev instrs
        else loop (Operator.load isa format cursor caf daf :: instrs) caf daf
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

  let load_debug_frame isa format cursor offset cie =
    (* TODO this code work only for object files... *)
    let initial_location = Utils.read_addr isa cursor in
    let address_range =
      Virtual_address.to_bigint (Utils.read_addr isa cursor)
    in
    let instructions =
      let rec loop instrs caf daf =
        if at_end cursor then List.rev instrs
        else loop (Operator.load isa format cursor caf daf :: instrs) caf daf
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

let load_eh_frame isa cursor (vaddr : Virtual_address.t) : Fde.t list =
  let rec loop isa cursor cies fdes =
    if at_end cursor then List.rev fdes
    else
      let offset = get_pos cursor in
      let format, length =
        let length = Read.u32 cursor in
        if Uint32.to_int32 length = 0xffffffffl then
          (`x64, Uint64.to_int (Read.u64 cursor))
        else (`x32, Uint32.to_int length)
      in
      if length = 0 then List.rev fdes
      else
        let cursor' = Read.sub cursor length in
        match Utils.read format cursor' with
        | 0L ->
            loop isa cursor
              (Cie.load_eh_frame isa format cursor' offset :: cies)
              fdes
        | cie_ptr ->
            let cie =
              List.find
                (function
                  | { Cie.offset = offset'; _ } ->
                      Int64.to_int cie_ptr = offset + get_pos cursor' - offset')
                cies
            in
            loop isa cursor cies
              (Fde.load_eh_frame isa format cursor' vaddr offset cie :: fdes)
  in
  loop isa cursor [] []

let load_debug_frame isa cursor : Fde.t list =
  let rec loop isa cursor cies fdes =
    if at_end cursor then List.rev fdes
    else
      let offset = get_pos cursor in
      let format, length =
        let length = Read.u32 cursor in
        if Uint32.to_int32 length = 0xffffffffl then
          (`x64, Uint64.to_int (Read.u64 cursor))
        else (`x32, Uint32.to_int length)
      in
      let cursor' = Read.sub cursor length in
      match (format, Utils.read format cursor') with
      | `x32, 0xffffffffL | `x64, 0xffffffffffffffffL ->
          loop isa cursor
            (Cie.load_debug_frame isa format cursor' offset :: cies)
            fdes
      | _, cie_ptr ->
          let cie =
            List.find
              (function
                | { Cie.offset = offset'; _ } -> Int64.to_int cie_ptr = offset')
              cies
          in
          loop isa cursor cies
            (Fde.load_debug_frame isa format cursor' offset cie :: fdes)
  in
  loop isa cursor [] []

type t = Machine.isa * (Fde.t * int array * entry list) list

let load img : t =
  let isa = Loader.Img.arch img in
  let fdes =
    try
      let section = Loader_utils.find_section_by_name ".eh_frame" img in
      let at = (Loader.Section.pos section).Loader_types.raw in
      let length = (Loader.Section.size section).Loader_types.raw in
      let cursor = Read.sub (Loader.Img.cursor ~at img) length in
      let vaddr = (Loader.Section.pos section).Loader_types.virt in
      load_eh_frame isa cursor vaddr
    with Not_found ->
      let section = Loader_utils.find_section_by_name ".debug_frame" img in
      let at = (Loader.Section.pos section).Loader_types.raw in
      let length = (Loader.Section.size section).Loader_types.raw in
      let cursor = Read.sub (Loader.Img.cursor ~at img) length in
      load_debug_frame isa cursor
  in
  Logger.debug "%a" pp fdes;
  ( isa,
    List.map
      (fun fde ->
        let columns, entries =
          Operator.interp isa [] fde.Fde.initial_location
            (Virtual_address.add_bigint
               (Z.pred fde.Fde.address_range)
               fde.Fde.initial_location)
            Dba.Expr.zero Map.empty []
            (List.concat
               [
                 fde.Fde.cie.Cie.initial_instructions;
                 [ Operator.Remember_state ];
                 fde.Fde.instructions;
               ])
        in
        (fde, Array.of_list columns, entries))
      fdes )

let fold f a (_, t) =
  List.fold_left
    (fun a ({ Fde.cie; _ }, columns, entries) ->
      List.fold_left
        (fun a entry ->
          f a ~return_address:cie.Cie.return_address_register ~columns entry)
        a entries)
    a t

let iter f (_, t) =
  List.iter
    (function
      | { Fde.cie; _ }, columns, entries ->
          List.iter
            (fun entry ->
              f ~return_address:cie.Cie.return_address_register ~columns entry)
            entries)
    t

let pp ppf (isa, t) =
  List.iter
    (function
      | { Fde.cie; initial_location; address_range; _ }, columns, entries ->
          let pp_print_hex =
            Virtual_address.pp_print (cie.Cie.format :> [ `x16 | `x32 | `x64 ])
          in
          Format.fprintf ppf "@[<v>FDE pc=%a..%a@ " pp_print_hex
            initial_location pp_print_hex
            (Virtual_address.add_bigint address_range initial_location);
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
                      (Expr.map isa x)));
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
