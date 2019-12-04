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

open Loader_buf

module File = struct
  type t =
    { dir  : int; time : int; size : int; name : string }

  let pp_row idx { dir; time; size; name } =
    [| string_of_int idx; string_of_int dir;
       string_of_int time; string_of_int size;
       name |]

  let load cursor =
    let name = Read.zero_string "" cursor () in
    let dir = Read.uleb128 cursor in
    let time = Read.uleb128 cursor in
    let size = Read.uleb128 cursor in
    {dir; time; size; name}
end

module Opcode = struct
  type t =
    | Standart of int * int array
    | Extended of string
    | Special of int * int
    | Copy
    | Advance_pc of int
    | Advance_line of int
    | Set_file of int
    | Set_column of int
    | Negate_stmt
    | Set_basic_block
    | Const_add_pc of int
    | Fixed_advance_pc of int
    | End_sequence
    | Set_address of int
    | Define_file of File.t
    | Set_discriminator of int

  let pp ppf = function
    | Standart (n, b)     ->
       Format.fprintf ppf "DW_LNS_standart%d %a" n
         (fun ppf b ->
           match Array.length b with
           | 0 -> ()
           | len ->
              Format.fprintf ppf "(%#x" b.(0);
              for i = 1 to len - 1 do
                Format.fprintf ppf ", %#x" b.(i)
              done;
              Format.fprintf ppf ")") b
    | Extended b          ->
       Format.fprintf ppf "DW_LNS_extended %a"
         (fun ppf b ->
           String.iter (fun c -> Format.fprintf ppf "%02x" (Char.code c)) b) b
    | Special (a, l)      -> Format.fprintf ppf "DW_LNS_special (0x%x, %d)" a l
    | Copy                -> Format.fprintf ppf "DW_LNS_copy"
    | Advance_pc p        -> Format.fprintf ppf "DW_LNS_advance_pc (%d)" p
    | Advance_line l      -> Format.fprintf ppf "DW_LNS_advance_line (%d)" l
    | Set_file f          -> Format.fprintf ppf "DW_LNS_set_file (%d)" f
    | Set_column c        -> Format.fprintf ppf "DW_LNS_set_column (%d)" c
    | Negate_stmt         -> Format.fprintf ppf "DW_LNS_negate_stmt"
    | Set_basic_block     -> Format.fprintf ppf "DW_LNS_set_basic_block"
    | Const_add_pc a      -> Format.fprintf ppf "DW_LNS_const_add_pc (0x%x)" a
    | Fixed_advance_pc p  -> Format.fprintf ppf "DW_LNS_fixed_advance_pc (%d)" p
    | End_sequence        -> Format.fprintf ppf "DW_LNS_end_sequence"
    | Set_address a       -> Format.fprintf ppf "DW_LNS_set_address (0x%x)" a
    | Define_file _       -> Format.fprintf ppf "DW_LNS_define_file"
    | Set_discriminator d -> Format.fprintf ppf "DW_LNS_set_discriminator %d" d

  let load_extended cursor size : t =
    if size < 1 then Dwarf_options.Logger.fatal
                       "trying to read Line Extended Opcode with size < 1";
    match Read.u8 cursor with
    | 0x01 -> End_sequence
    | 0x02 ->
       if size = 5 then
         Set_address (Read.u32 cursor)
       else if size = 9 then
         Set_address (Read.u64 cursor)
       else
         Dwarf_options.Logger.fatal
           "trying to read DW_LNS_set_address \
            where address encoding differ from u32 or u64";
    | 0x03 -> Define_file (File.load cursor)
    | 0x04 -> Set_discriminator (Read.uleb128 cursor)
    | _    ->
       advance cursor (-1);
       Extended (String.init size (fun _ -> Char.chr (Read.u8 cursor)))

  let load cursor standard_opcode_lengths line_base line_range opcode_base : t =
    match Read.u8 cursor with
    | 0x00 -> load_extended cursor (Read.uleb128 cursor)
    | 0x01 -> Copy
    | 0x02 -> Advance_pc (Read.uleb128 cursor)
    | 0x03 -> Advance_line (Read.sleb128 cursor)
    | 0x04 -> Set_file (Read.uleb128 cursor)
    | 0x05 -> Set_column (Read.uleb128 cursor)
    | 0x06 -> Negate_stmt
    | 0x07 -> Set_basic_block
    | 0x08 -> Const_add_pc ((255 - opcode_base) / line_range)
    | 0x09 -> Fixed_advance_pc (Read.u16 cursor)
    | x    ->
       if x < Array.length standard_opcode_lengths then
         Standart (x, Array.init standard_opcode_lengths.(x)
                        (fun _ -> Read.uleb128 cursor))
       else
         Special ((x - opcode_base) / line_range,
                  line_base + (x - opcode_base) mod line_range)
end

module Header = struct
  type t =
    {
      version                 : int;
      min_instr_length        : int;
      default_is_stmt         : bool;
      line_base               : int;
      line_range              : int;
      opcode_base             : int;
      standard_opcode_lengths : int array;
      include_directories     : string array;
      file_names              : (int, File.t) Hashtbl.t;
      instructions            : Opcode.t list;
    }

  let pp =
    let columns2 =
            [| Prettytbl.Column.default; Prettytbl.Column.default |] in
    let columns5 =
            [| Prettytbl.Column.default; Prettytbl.Column.default;
               Prettytbl.Column.default; Prettytbl.Column.default;
               Prettytbl.Column.default |] in
    fun ppf h ->
    Format.fprintf ppf
      "@[\
       @[<v 2>Raw dump of debug contents of section .debug_line:\
       @ \
       @ ";
    let t = Prettytbl.make columns2 in
    Prettytbl.append t
      [| "DWARF Version:"; string_of_int h.version |];
    Prettytbl.append t
      [| "Minimum Instruction Length:"; string_of_int h.min_instr_length |];
    Prettytbl.append t
      [| "Initial value of 'is_stmt':"; string_of_bool h.default_is_stmt |];
    Prettytbl.append t
      [| "Line Base:"; string_of_int h.line_base |];
    Prettytbl.append t
      [| "Line Range:"; string_of_int h.line_range |];
    Prettytbl.append t
      [| "Opcode base:"; string_of_int h.opcode_base |];
    Prettytbl.pp ppf t;
    Format.fprintf ppf
      "@]@ \
       @[<v 1>@ \
       @[<v 1>Opcodes:";
    for i = 1 to h.opcode_base - 1 do
      Format.fprintf ppf "@ Opcode %d has %d args" i
        h.standard_opcode_lengths.(i)
    done;
    Format.fprintf ppf
      "@]@]@ \
       @[<v 1>@ \
       @[<v 1>The Directory Table:@ ";
    let t = Prettytbl.make columns2 in
    for i = 1 to Array.length h.include_directories - 1 do
      Prettytbl.append t
        [| string_of_int i; h.include_directories.(i) |]
    done;
    Prettytbl.pp ppf t;
    Format.fprintf ppf
      "@]@]@ \
       @[<v 1>@ \
       @[<v 1>The File Name Table:@ ";
    let t = Prettytbl.make columns5 in
    Prettytbl.append t [| "Entry"; "Dir"; "Time"; "Size"; "Name" |];
    for i = 1 to Hashtbl.length h.file_names do
      Prettytbl.append t (File.pp_row i (Hashtbl.find h.file_names i))
    done;
    Prettytbl.pp ppf t;
    Format.fprintf ppf
      "@]@]@ \
       @[<v 1>@ \
       @[<v 1>Line Number Statements:@ ";
    List.iter
      (fun op -> Format.fprintf ppf "%a@ " Opcode.pp op) h.instructions;
    Format.pp_close_box ppf ();
    Format.pp_close_box ppf ();
    Format.pp_close_box ppf ()

  let load cursor : t =
    let format, length = match Read.u32 cursor with
      | 0xffffffff -> `x64, Read.u64 cursor
      | length -> `x32, length in
    let bound = cursor.position + length in
    let version = Read.u16 cursor in
    if version <> 2 then
      Dwarf_options.Logger.fatal
        "trying to read Line header with version <> 2";
    let prologue_length = read format cursor in
    let prologue_end = cursor.position + prologue_length in
    let min_instr_length = Read.u8 cursor in
    let default_is_stmt = 0 < Read.u8 cursor in
    let line_base = Read.s8 cursor in
    let line_range = Read.u8 cursor in
    let opcode_base = Read.u8 cursor in
    let standard_opcode_lengths =
      Array.init opcode_base (fun i -> if i = 0 then 0 else Read.u8 cursor) in
    let rec loop include_directories = match Peek.u8 cursor with
      | 0x00 -> advance cursor 1; Array.of_list (List.rev include_directories)
      | _ -> loop (Read.zero_string "" cursor ()
                   :: include_directories) in
    let include_directories = loop [ "." ] in
    let file_names = Hashtbl.create 64 in
    while Peek.u8 cursor <> 0 do
      Hashtbl.add file_names
        (Hashtbl.length file_names + 1) (File.load cursor)
    done;
    seek cursor prologue_end;
    let rec loop ins =
      if cursor.position < bound then
        loop (Opcode.load cursor standard_opcode_lengths
                line_base line_range opcode_base :: ins)
      else List.rev ins in
    let instructions = loop [] in
    { version; min_instr_length; default_is_stmt;
      line_base; line_range; opcode_base; standard_opcode_lengths;
      include_directories; file_names; instructions }
end

module Machine = struct
  type state =
    {
      mutable address       : int;
      mutable file          : int;
      mutable line          : int;
      mutable column        : int;
      mutable is_stmt       : bool;
      mutable basic_block   : bool;
      mutable discriminator : int;
    }
  type t =
    {
      previous              : state;
      current               : state;
      mutable end_sequence  : bool;
    }

  let init is_stmt =
    { previous={ address=0; file=1; line=1; column=0;
                 is_stmt; basic_block=false; discriminator=0 };
      current={ address=0; file=1; line=1; column=0;
                is_stmt; basic_block=false; discriminator=0 };
      end_sequence=true }
  let reset vm is_stmt =
    vm.current.address <- 0;
    vm.current.file <- 1;
    vm.current.line <- 1;
    vm.current.column <- 0;
    vm.current.is_stmt <- is_stmt;
    vm.current.basic_block <- false;
    vm.current.discriminator <- 0;
    vm.end_sequence <- true
  let fetch vm =
    vm.previous.address <- vm.current.address;
    vm.previous.file <- vm.current.file;
    vm.previous.line <- vm.current.line;
    vm.previous.column <- vm.current.column;
    vm.previous.is_stmt <- vm.current.is_stmt;
    vm.previous.basic_block <- vm.current.basic_block;
    vm.previous.discriminator <- vm.current.discriminator;
    vm.current.basic_block <- false;
    vm.current.discriminator <- 0;
    vm.end_sequence <- false
end

type entry =
  { addresses: int Interval.t; path: string; line: int; column: int;
    is_stmt: bool; basic_block: bool; discriminator: int }

type t = entry list

let emit vm hd =
  let fd = Hashtbl.find hd.Header.file_names Machine.(vm.previous.file) in
  let path = match String.get fd.File.name 0 with
    | '/' -> fd.File.name
    | _ -> hd.Header.include_directories.(fd.File.dir) ^ "/" ^ fd.File.name in
  let entry =
    { addresses={ Interval.lo=vm.Machine.previous.Machine.address;
                  hi=vm.Machine.current.Machine.address - 1 };
      path;
      line=vm.Machine.previous.Machine.line;
      column=vm.Machine.previous.Machine.column;
      is_stmt=vm.Machine.previous.Machine.is_stmt;
      basic_block=vm.Machine.previous.Machine.basic_block;
      discriminator=vm.Machine.previous.Machine.discriminator } in
  Machine.fetch vm;
  entry


let decode vm hd lines = function
  | Opcode.Special (a, l) ->
     vm.Machine.current.Machine.line <- vm.Machine.current.Machine.line + l;
     vm.Machine.current.Machine.address <- vm.Machine.current.Machine.address +
                                             hd.Header.min_instr_length * a;
     if vm.Machine.end_sequence then
       begin Machine.fetch vm; lines end
     else emit vm hd :: lines
  | Opcode.Copy ->
     if vm.Machine.end_sequence then
       begin Machine.fetch vm; lines end
     else emit vm hd :: lines
  | Opcode.Advance_pc a
  | Opcode.Const_add_pc a ->
     vm.Machine.current.Machine.address <- vm.Machine.current.Machine.address +
                                             hd.Header.min_instr_length * a;
     lines
  | Opcode.Advance_line l ->
     vm.Machine.current.Machine.line <- vm.Machine.current.Machine.line + l;
     lines
  | Opcode.Set_file f ->
     vm.Machine.current.Machine.file <- f;
     lines
  | Opcode.Set_column c ->
     vm.Machine.current.Machine.column <- c;
     lines
  | Opcode.Negate_stmt ->
     vm.Machine.current.Machine.is_stmt <-
       not vm.Machine.current.Machine.is_stmt;
     lines
  | Opcode.Set_basic_block ->
     vm.Machine.current.Machine.basic_block <- true;
     lines
  | Opcode.Fixed_advance_pc a ->
     vm.Machine.current.Machine.address <-
       vm.Machine.current.Machine.address + a;
     lines
  | Opcode.End_sequence ->
     if vm.Machine.end_sequence then
       Dwarf_options.Logger.fatal
         "met a DW_LNS_end_sequence on an empty sequence";
     let lines = emit vm hd :: lines in
     Machine.reset vm hd.Header.default_is_stmt;
     lines
  | Opcode.Set_address address ->
     vm.Machine.current.Machine.address <- address;
     lines
  | Opcode.Define_file f ->
     Hashtbl.(add hd.Header.file_names (length hd.Header.file_names) f);
     lines
  | Opcode.Set_discriminator d ->
     vm.Machine.current.Machine.discriminator <- d;
     lines
  | Opcode.Standart _ | Opcode.Extended _ ->
     lines

let load img : t =
  let section = Loader_utils.find_section_by_name ".debug_line" img in
  let at = (Loader.Section.pos section).Loader_types.raw in
  let length = (Loader.Section.size section).Loader_types.raw in
  let cursor = sub (Loader.Img.cursor ~at img) length in
  let rec loop lines cursor =
    if at_end cursor then List.flatten lines
    else
      let hd = Header.load cursor in
      Dwarf_options.Logger.debug "%a" Header.pp hd;
      let vm = Machine.init hd.Header.default_is_stmt in
      loop
        (List.rev (List.fold_left (decode vm hd) [] hd.Header.instructions)
         :: lines)
        cursor in
  loop [] cursor

let fold = List.fold_left
let iter = List.iter

let pp =
  let columns =
    [|
      Prettytbl.Column.default;
      Prettytbl.(Column.make ~align:R ());
      Prettytbl.(Column.make ~align:R ())
    |] in
  fun ppf l ->
  Format.pp_open_vbox ppf 0;
  let t = Prettytbl.make columns in
  Prettytbl.append t
    [| "File name"; "Line number"; "Starting address" |];
  List.iter
    (fun { path; line; addresses={ Interval.lo; _ }; _ } ->
      Prettytbl.append t
        [| path; string_of_int line; Format.sprintf "%#x" lo |]) l;
  Prettytbl.pp ppf t;
  Format.pp_close_box ppf ()
