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

let section img ?(at = 0) name =
  let sec =
    try Loader_utils.find_section_by_name name img
    with Not_found -> Logger.fatal "missing section %s" name
  in
  let base = (Loader.Section.pos sec).Loader_types.raw in
  let dim = (Loader.Section.size sec).Loader_types.raw in
  let at = base + at in
  Read.sub (Loader.Img.cursor ~at img) dim

module Str = struct
  let load img = section img ".debug_str"

  let get cursor i =
    set_pos cursor i;
    Read.zero_string "" cursor ()

  let pp isa ppf cursor =
    Format.fprintf ppf "@[<v>Contents of the .debug_str section:@ @[<v 2>";
    set_pos cursor 0;
    while not (at_end cursor) do
      if get_pos cursor mod 16 = 0 then
        Format.fprintf ppf "@ 0x%a"
          (Machine.Bitwidth.pp_print_hex (Machine.ISA.bits isa))
          (get_pos cursor);
      if get_pos cursor mod 4 = 0 then Format.pp_print_char ppf ' ';
      Format.fprintf ppf "%02x" (Uint8.to_int (Read.u8 cursor));
      if get_pos cursor mod 16 = 0 then (
        Format.pp_print_char ppf ' ';
        advance cursor (-16);
        for _ = 0 to 15 do
          Format.pp_print_char ppf
            (match Uint8.to_char (Read.u8 cursor) with
            | ' ' .. '~' as c -> c
            | _ -> '.')
        done)
    done;
    let r = get_pos cursor mod 16 in
    if r <> 0 then (
      for _ = 0 to (2 * (16 - r)) + 4 - (r / 4) - 1 do
        Format.pp_print_char ppf ' '
      done;
      advance cursor (-r);
      for _ = 0 to r - 1 do
        Format.pp_print_char ppf
          (match Uint8.to_char (Read.u8 cursor) with
          | ' ' .. '~' as c -> c
          | _ -> '.')
      done);
    Format.pp_close_box ppf ();
    Format.pp_close_box ppf ()
end

module Tag = struct
  type t =
    | Array_type
    | Class_type
    | Entry_point
    | Enumeration_type
    | Formal_parameter
    | Imported_declaration
    | Label
    | Lexical_block
    | Member
    | Pointer_type
    | Reference_type
    | Compile_unit
    | String_type
    | Structure_type
    | Subroutine_type
    | Typedef
    | Union_type
    | Unspecified_parameters
    | Variant
    | Common_block
    | Common_inclusion
    | Inheritance
    | Inlined_subroutine
    | Module
    | Ptr_to_member_type
    | Set_type
    | Subrange_type
    | With_stmt
    | Access_declaration
    | Base_type
    | Catch_block
    | Const_type
    | Constant
    | Enumerator
    | File_type
    | Friend
    | Namelist
    | Namelist_item
    | Packed_type
    | Subprogram
    | Template_type_parameter
    | Template_value_parameter
    | Thrown_type
    | Try_block
    | Variant_part
    | Variable
    | Volatile_type
    | Dwarf_procedure
    | Restrict_type
    | Interface_type
    | Namespace
    | Imported_module
    | Unspecified_type
    | Partial_unit
    | Imported_unit
    | Condition
    | Shared_type
    | Type_unit
    | Rvalue_reference_type
    | Template_alias
    | Coarray_type
    | Generic_subrange
    | Dynamic_type
    | Atomic_type
    | Call_site
    | Call_site_parameter
    | Skeleton_unit
    | Immutable_type
    | Reserved
    | Custom of int

  let of_value = function
    | 0x01 -> Array_type
    | 0x02 -> Class_type
    | 0x03 -> Entry_point
    | 0x04 -> Enumeration_type
    | 0x05 -> Formal_parameter
    | 0x08 -> Imported_declaration
    | 0x0a -> Label
    | 0x0b -> Lexical_block
    | 0x0d -> Member
    | 0x0f -> Pointer_type
    | 0x10 -> Reference_type
    | 0x11 -> Compile_unit
    | 0x12 -> String_type
    | 0x13 -> Structure_type
    | 0x15 -> Subroutine_type
    | 0x16 -> Typedef
    | 0x17 -> Union_type
    | 0x18 -> Unspecified_parameters
    | 0x19 -> Variant
    | 0x1a -> Common_block
    | 0x1b -> Common_inclusion
    | 0x1c -> Inheritance
    | 0x1d -> Inlined_subroutine
    | 0x1e -> Module
    | 0x1f -> Ptr_to_member_type
    | 0x20 -> Set_type
    | 0x21 -> Subrange_type
    | 0x22 -> With_stmt
    | 0x23 -> Access_declaration
    | 0x24 -> Base_type
    | 0x25 -> Catch_block
    | 0x26 -> Const_type
    | 0x27 -> Constant
    | 0x28 -> Enumerator
    | 0x29 -> File_type
    | 0x2a -> Friend
    | 0x2b -> Namelist
    | 0x2c -> Namelist_item
    | 0x2d -> Packed_type
    | 0x2e -> Subprogram
    | 0x2f -> Template_type_parameter
    | 0x30 -> Template_value_parameter
    | 0x31 -> Thrown_type
    | 0x32 -> Try_block
    | 0x33 -> Variant_part
    | 0x34 -> Variable
    | 0x35 -> Volatile_type
    | 0x36 -> Dwarf_procedure
    | 0x37 -> Restrict_type
    | 0x38 -> Interface_type
    | 0x39 -> Namespace
    | 0x3a -> Imported_module
    | 0x3b -> Unspecified_type
    | 0x3c -> Partial_unit
    | 0x3d -> Imported_unit
    | 0x3f -> Condition
    | 0x40 -> Shared_type
    | 0x41 -> Type_unit
    | 0x42 -> Rvalue_reference_type
    | 0x43 -> Template_alias
    | 0x44 -> Coarray_type
    | 0x45 -> Generic_subrange
    | 0x46 -> Dynamic_type
    | 0x47 -> Atomic_type
    | 0x48 -> Call_site
    | 0x49 -> Call_site_parameter
    | 0x4a -> Skeleton_unit
    | 0x4b -> Immutable_type
    | x when x >= 0x4080 || x <= 0xffff -> Custom x
    | _ -> Reserved

  let read cursor : t = of_value (Z.to_int (Read.uleb128 cursor))

  let pp ppf = function
    | Array_type -> Format.fprintf ppf "DW_TAG_array_type"
    | Class_type -> Format.fprintf ppf "DW_TAG_class_type"
    | Entry_point -> Format.fprintf ppf "DW_TAG_entry_point"
    | Enumeration_type -> Format.fprintf ppf "DW_TAG_enumeration_type"
    | Formal_parameter -> Format.fprintf ppf "DW_TAG_formal_parameter"
    | Imported_declaration -> Format.fprintf ppf "DW_TAG_imported_declaration"
    | Label -> Format.fprintf ppf "DW_TAG_label"
    | Lexical_block -> Format.fprintf ppf "DW_TAG_lexical_block"
    | Member -> Format.fprintf ppf "DW_TAG_member"
    | Pointer_type -> Format.fprintf ppf "DW_TAG_pointer_type"
    | Reference_type -> Format.fprintf ppf "DW_TAG_reference_type"
    | Compile_unit -> Format.fprintf ppf "DW_TAG_compile_unit"
    | String_type -> Format.fprintf ppf "DW_TAG_string_type"
    | Structure_type -> Format.fprintf ppf "DW_TAG_structure_type"
    | Subroutine_type -> Format.fprintf ppf "DW_TAG_subroutine_type"
    | Typedef -> Format.fprintf ppf "DW_TAG_typedef"
    | Union_type -> Format.fprintf ppf "DW_TAG_union_type"
    | Unspecified_parameters ->
        Format.fprintf ppf "DW_TAG_unspecified_parameters"
    | Variant -> Format.fprintf ppf "DW_TAG_variant"
    | Common_block -> Format.fprintf ppf "DW_TAG_common_block"
    | Common_inclusion -> Format.fprintf ppf "DW_TAG_common_inclusion"
    | Inheritance -> Format.fprintf ppf "DW_TAG_inheritance"
    | Inlined_subroutine -> Format.fprintf ppf "DW_TAG_inlined_subroutine"
    | Module -> Format.fprintf ppf "DW_TAG_module"
    | Ptr_to_member_type -> Format.fprintf ppf "DW_TAG_ptr_to_member_type"
    | Set_type -> Format.fprintf ppf "DW_TAG_set_type"
    | Subrange_type -> Format.fprintf ppf "DW_TAG_subrange_type"
    | With_stmt -> Format.fprintf ppf "DW_TAG_with_stmt"
    | Access_declaration -> Format.fprintf ppf "DW_TAG_access_declaration"
    | Base_type -> Format.fprintf ppf "DW_TAG_base_type"
    | Catch_block -> Format.fprintf ppf "DW_TAG_catch_block"
    | Const_type -> Format.fprintf ppf "DW_TAG_const_type"
    | Constant -> Format.fprintf ppf "DW_TAG_constant"
    | Enumerator -> Format.fprintf ppf "DW_TAG_enumerator"
    | File_type -> Format.fprintf ppf "DW_TAG_file_type"
    | Friend -> Format.fprintf ppf "DW_TAG_friend"
    | Namelist -> Format.fprintf ppf "DW_TAG_namelist"
    | Namelist_item -> Format.fprintf ppf "DW_TAG_namelist_item"
    | Packed_type -> Format.fprintf ppf "DW_TAG_packed_type"
    | Subprogram -> Format.fprintf ppf "DW_TAG_subprogram"
    | Template_type_parameter ->
        Format.fprintf ppf "DW_TAG_template_type_parameter"
    | Template_value_parameter ->
        Format.fprintf ppf "DW_TAG_template_value_parameter"
    | Thrown_type -> Format.fprintf ppf "DW_TAG_thrown_type"
    | Try_block -> Format.fprintf ppf "DW_TAG_try_block"
    | Variant_part -> Format.fprintf ppf "DW_TAG_variant_part"
    | Variable -> Format.fprintf ppf "DW_TAG_variable"
    | Volatile_type -> Format.fprintf ppf "DW_TAG_volatile_type"
    | Dwarf_procedure -> Format.fprintf ppf "DW_TAG_dwarf_procedure"
    | Restrict_type -> Format.fprintf ppf "DW_TAG_restrict_type"
    | Interface_type -> Format.fprintf ppf "DW_TAG_interface_type"
    | Namespace -> Format.fprintf ppf "DW_TAG_namespace"
    | Imported_module -> Format.fprintf ppf "DW_TAG_imported_module"
    | Unspecified_type -> Format.fprintf ppf "DW_TAG_unspecified_type"
    | Partial_unit -> Format.fprintf ppf "DW_TAG_partial_unit"
    | Imported_unit -> Format.fprintf ppf "DW_TAG_imported_unit"
    | Condition -> Format.fprintf ppf "DW_TAG_condition"
    | Shared_type -> Format.fprintf ppf "DW_TAG_shared_type"
    | Type_unit -> Format.fprintf ppf "DW_TAG_type_unit"
    | Rvalue_reference_type -> Format.fprintf ppf "DW_TAG_rvalue_reference_type"
    | Template_alias -> Format.fprintf ppf "DW_TAG_template_alias"
    | Coarray_type -> Format.fprintf ppf "DW_TAG_coarray_type"
    | Generic_subrange -> Format.fprintf ppf "DW_TAG_generic_subrange"
    | Dynamic_type -> Format.fprintf ppf "DW_TAG_dynamic_type"
    | Atomic_type -> Format.fprintf ppf "DW_TAG_atomic_type"
    | Call_site -> Format.fprintf ppf "DW_TAG_call_site"
    | Call_site_parameter -> Format.fprintf ppf "DW_TAG_call_site_parameter"
    | Skeleton_unit -> Format.fprintf ppf "DW_TAG_skeleton_unit"
    | Immutable_type -> Format.fprintf ppf "DW_TAG_immutable_type"
    | Reserved -> Format.fprintf ppf "reserved"
    | Custom x -> Format.fprintf ppf "custom(%d)" x
end

module Attribute = struct
  type t =
    | Null
    | Sibling
    | Location
    | Name
    | Ordering
    | Byte_size
    | Bit_offset
    | Bit_size
    | Stmt_list
    | Low_pc
    | High_pc
    | Language
    | Discr
    | Discr_value
    | Visibility
    | Import
    | String_length
    | Common_reference
    | Comp_dir
    | Const_value
    | Containing_type
    | Default_value
    | Inline
    | Is_optional
    | Lower_bound
    | Producer
    | Prototyped
    | Return_addr
    | Start_scope
    | Bit_stride
    | Upper_bound
    | Abstract_origin
    | Accessibility
    | Address_class
    | Artificial
    | Base_types
    | Calling_convention
    | Count
    | Data_member_location
    | Decl_column
    | Decl_file
    | Decl_line
    | Declaration
    | Discr_list
    | Encoding
    | External
    | Frame_base
    | Friend
    | Identifier_case
    | Namelist_item
    | Priority
    | Segment
    | Specification
    | Static_link
    | Type
    | Use_location
    | Variable_parameter
    | Virtuality
    | Vtable_elem_location
    | Allocated
    | Associated
    | Data_location
    | Byte_stride
    | Entry_pc
    | Use_UTF8
    | Extension
    | Ranges
    | Trampoline
    | Call_column
    | Call_file
    | Call_line
    | Description
    | Binary_scale
    | Decimal_scale
    | Small
    | Decimal_sign
    | Digit_count
    | Picture_string
    | Mutable
    | Threads_scaled
    | Explicit
    | Object_pointer
    | Endianity
    | Elemental
    | Pure
    | Recursive
    | Signature
    | Main_subprogram
    | Data_bit_offset
    | Const_expr
    | Enum_class
    | Linkage_name
    | String_length_bit_size
    | String_length_byte_size
    | Rank
    | Str_offsets_base
    | Addr_base
    | Rnglists_base
    | Dwo_name
    | Reference
    | Rvalue_reference
    | Macros
    | Call_all_calls
    | Call_all_source_calls
    | Call_all_tail_calls
    | Call_return_pc
    | Call_value
    | Call_origin
    | Call_parameter
    | Call_pc
    | Call_tail_call
    | Call_target
    | Call_target_clobbered
    | Call_data_location
    | Call_data_value
    | Noreturn
    | Alignment
    | Export_symbols
    | Deleted
    | Defaulted
    | Loclists_base
    | Reserved
    | Custom of int

  let of_value = function
    | 0x00 -> Null
    | 0x01 -> Sibling
    | 0x02 -> Location
    | 0x03 -> Name
    | 0x09 -> Ordering
    | 0x0b -> Byte_size
    | 0x0c -> Bit_offset
    | 0x0d -> Bit_size
    | 0x10 -> Stmt_list
    | 0x11 -> Low_pc
    | 0x12 -> High_pc
    | 0x13 -> Language
    | 0x15 -> Discr
    | 0x16 -> Discr_value
    | 0x17 -> Visibility
    | 0x18 -> Import
    | 0x19 -> String_length
    | 0x1a -> Common_reference
    | 0x1b -> Comp_dir
    | 0x1c -> Const_value
    | 0x1d -> Containing_type
    | 0x1e -> Default_value
    | 0x20 -> Inline
    | 0x21 -> Is_optional
    | 0x22 -> Lower_bound
    | 0x25 -> Producer
    | 0x27 -> Prototyped
    | 0x2a -> Return_addr
    | 0x2c -> Start_scope
    | 0x2e -> Bit_stride
    | 0x2f -> Upper_bound
    | 0x31 -> Abstract_origin
    | 0x32 -> Accessibility
    | 0x33 -> Address_class
    | 0x34 -> Artificial
    | 0x35 -> Base_types
    | 0x36 -> Calling_convention
    | 0x37 -> Count
    | 0x38 -> Data_member_location
    | 0x39 -> Decl_column
    | 0x3a -> Decl_file
    | 0x3b -> Decl_line
    | 0x3c -> Declaration
    | 0x3d -> Discr_list
    | 0x3e -> Encoding
    | 0x3f -> External
    | 0x40 -> Frame_base
    | 0x41 -> Friend
    | 0x42 -> Identifier_case
    | 0x44 -> Namelist_item
    | 0x45 -> Priority
    | 0x46 -> Segment
    | 0x47 -> Specification
    | 0x48 -> Static_link
    | 0x49 -> Type
    | 0x4a -> Use_location
    | 0x4b -> Variable_parameter
    | 0x4c -> Virtuality
    | 0x4d -> Vtable_elem_location
    | 0x4e -> Allocated
    | 0x4f -> Associated
    | 0x50 -> Data_location
    | 0x51 -> Byte_stride
    | 0x52 -> Entry_pc
    | 0x53 -> Use_UTF8
    | 0x54 -> Extension
    | 0x55 -> Ranges
    | 0x56 -> Trampoline
    | 0x57 -> Call_column
    | 0x58 -> Call_file
    | 0x59 -> Call_line
    | 0x5a -> Description
    | 0x5b -> Binary_scale
    | 0x5c -> Decimal_scale
    | 0x5d -> Small
    | 0x5e -> Decimal_sign
    | 0x5f -> Digit_count
    | 0x60 -> Picture_string
    | 0x61 -> Mutable
    | 0x62 -> Threads_scaled
    | 0x63 -> Explicit
    | 0x64 -> Object_pointer
    | 0x65 -> Endianity
    | 0x66 -> Elemental
    | 0x67 -> Pure
    | 0x68 -> Recursive
    | 0x69 -> Signature
    | 0x6a -> Main_subprogram
    | 0x6b -> Data_bit_offset
    | 0x6c -> Const_expr
    | 0x6d -> Enum_class
    | 0x6e -> Linkage_name
    | 0x6f -> String_length_bit_size
    | 0x70 -> String_length_byte_size
    | 0x71 -> Rank
    | 0x72 -> Str_offsets_base
    | 0x73 -> Addr_base
    | 0x74 -> Rnglists_base
    | 0x76 -> Dwo_name
    | 0x77 -> Reference
    | 0x78 -> Rvalue_reference
    | 0x79 -> Macros
    | 0x7a -> Call_all_calls
    | 0x7b -> Call_all_source_calls
    | 0x7c -> Call_all_tail_calls
    | 0x7d -> Call_return_pc
    | 0x7e -> Call_value
    | 0x7f -> Call_origin
    | 0x80 -> Call_parameter
    | 0x81 -> Call_pc
    | 0x82 -> Call_tail_call
    | 0x83 -> Call_target
    | 0x84 -> Call_target_clobbered
    | 0x85 -> Call_data_location
    | 0x86 -> Call_data_value
    | 0x87 -> Noreturn
    | 0x88 -> Alignment
    | 0x89 -> Export_symbols
    | 0x8a -> Deleted
    | 0x8b -> Defaulted
    | 0x8c -> Loclists_base
    | x when x >= 0x2000 || x <= 0x3fff -> Custom x
    | _ -> Reserved

  let read cursor : t = of_value (Z.to_int (Read.uleb128 cursor))
  let compare = compare

  module MapSet = Basic_types.Collection_make.Default (struct
    type a = t
    type t = a

    let compare = compare
  end)

  module Map = MapSet.Map

  let pp ppf = function
    | Null -> Format.fprintf ppf "null"
    | Sibling -> Format.fprintf ppf "DW_AT_sibling"
    | Location -> Format.fprintf ppf "DW_AT_location"
    | Name -> Format.fprintf ppf "DW_AT_name"
    | Ordering -> Format.fprintf ppf "DW_AT_ordering"
    | Byte_size -> Format.fprintf ppf "DW_AT_byte_size"
    | Bit_offset -> Format.fprintf ppf "DW_AT_bit_offset"
    | Bit_size -> Format.fprintf ppf "DW_AT_bit_size"
    | Stmt_list -> Format.fprintf ppf "DW_AT_stmt_list"
    | Low_pc -> Format.fprintf ppf "DW_AT_low_pc"
    | High_pc -> Format.fprintf ppf "DW_AT_high_pc"
    | Language -> Format.fprintf ppf "DW_AT_language"
    | Discr -> Format.fprintf ppf "DW_AT_discr"
    | Discr_value -> Format.fprintf ppf "DW_AT_discr_value"
    | Visibility -> Format.fprintf ppf "DW_AT_visibility"
    | Import -> Format.fprintf ppf "DW_AT_import"
    | String_length -> Format.fprintf ppf "DW_AT_string_length"
    | Common_reference -> Format.fprintf ppf "DW_AT_common_reference"
    | Comp_dir -> Format.fprintf ppf "DW_AT_comp_dir"
    | Const_value -> Format.fprintf ppf "DW_AT_const_value"
    | Containing_type -> Format.fprintf ppf "DW_AT_containing_type"
    | Default_value -> Format.fprintf ppf "DW_AT_default_value"
    | Inline -> Format.fprintf ppf "DW_AT_inline"
    | Is_optional -> Format.fprintf ppf "DW_AT_is_optional"
    | Lower_bound -> Format.fprintf ppf "DW_AT_lower_bound"
    | Producer -> Format.fprintf ppf "DW_AT_producer"
    | Prototyped -> Format.fprintf ppf "DW_AT_prototyped"
    | Return_addr -> Format.fprintf ppf "DW_AT_return_addr"
    | Start_scope -> Format.fprintf ppf "DW_AT_start_scope"
    | Bit_stride -> Format.fprintf ppf "DW_AT_bit_stride"
    | Upper_bound -> Format.fprintf ppf "DW_AT_upper_bound"
    | Abstract_origin -> Format.fprintf ppf "DW_AT_abstract_origin"
    | Accessibility -> Format.fprintf ppf "DW_AT_accessibility"
    | Address_class -> Format.fprintf ppf "DW_AT_address_class"
    | Artificial -> Format.fprintf ppf "DW_AT_artificial"
    | Base_types -> Format.fprintf ppf "DW_AT_base_types"
    | Calling_convention -> Format.fprintf ppf "DW_AT_calling_convention"
    | Count -> Format.fprintf ppf "DW_AT_count"
    | Data_member_location -> Format.fprintf ppf "DW_AT_data_member_location"
    | Decl_column -> Format.fprintf ppf "DW_AT_decl_column"
    | Decl_file -> Format.fprintf ppf "DW_AT_decl_file"
    | Decl_line -> Format.fprintf ppf "DW_AT_decl_line"
    | Declaration -> Format.fprintf ppf "DW_AT_declaration"
    | Discr_list -> Format.fprintf ppf "DW_AT_discr_list"
    | Encoding -> Format.fprintf ppf "DW_AT_encoding"
    | External -> Format.fprintf ppf "DW_AT_external"
    | Frame_base -> Format.fprintf ppf "DW_AT_frame_base"
    | Friend -> Format.fprintf ppf "DW_AT_friend"
    | Identifier_case -> Format.fprintf ppf "DW_AT_identifier_case"
    | Namelist_item -> Format.fprintf ppf "DW_AT_namelist_item"
    | Priority -> Format.fprintf ppf "DW_AT_priority"
    | Segment -> Format.fprintf ppf "DW_AT_segment"
    | Specification -> Format.fprintf ppf "DW_AT_specification"
    | Static_link -> Format.fprintf ppf "DW_AT_static_link"
    | Type -> Format.fprintf ppf "DW_AT_type"
    | Use_location -> Format.fprintf ppf "DW_AT_use_location"
    | Variable_parameter -> Format.fprintf ppf "DW_AT_variable_parameter"
    | Virtuality -> Format.fprintf ppf "DW_AT_virtuality"
    | Vtable_elem_location -> Format.fprintf ppf "DW_AT_vtable_elem_location"
    | Allocated -> Format.fprintf ppf "DW_AT_allocated"
    | Associated -> Format.fprintf ppf "DW_AT_associated"
    | Data_location -> Format.fprintf ppf "DW_AT_data_location"
    | Byte_stride -> Format.fprintf ppf "DW_AT_byte_stride"
    | Entry_pc -> Format.fprintf ppf "DW_AT_entry_pc"
    | Use_UTF8 -> Format.fprintf ppf "DW_AT_use_utf8"
    | Extension -> Format.fprintf ppf "DW_AT_extension"
    | Ranges -> Format.fprintf ppf "DW_AT_ranges"
    | Trampoline -> Format.fprintf ppf "DW_AT_trampoline"
    | Call_column -> Format.fprintf ppf "DW_AT_call_column"
    | Call_file -> Format.fprintf ppf "DW_AT_call_file"
    | Call_line -> Format.fprintf ppf "DW_AT_call_line"
    | Description -> Format.fprintf ppf "DW_AT_description"
    | Binary_scale -> Format.fprintf ppf "DW_AT_binary_scale"
    | Decimal_scale -> Format.fprintf ppf "DW_AT_decimal_scale"
    | Small -> Format.fprintf ppf "DW_AT_small"
    | Decimal_sign -> Format.fprintf ppf "DW_AT_decimal_sign"
    | Digit_count -> Format.fprintf ppf "DW_AT_digit_count"
    | Picture_string -> Format.fprintf ppf "DW_AT_picture_string"
    | Mutable -> Format.fprintf ppf "DW_AT_mutable"
    | Threads_scaled -> Format.fprintf ppf "DW_AT_threads_scaled"
    | Explicit -> Format.fprintf ppf "DW_AT_explicit"
    | Object_pointer -> Format.fprintf ppf "DW_AT_object_pointer"
    | Endianity -> Format.fprintf ppf "DW_AT_endianity"
    | Elemental -> Format.fprintf ppf "DW_AT_elemental"
    | Pure -> Format.fprintf ppf "DW_AT_pure"
    | Recursive -> Format.fprintf ppf "DW_AT_recursive"
    | Signature -> Format.fprintf ppf "DW_AT_signature"
    | Main_subprogram -> Format.fprintf ppf "DW_AT_main_subprogram"
    | Data_bit_offset -> Format.fprintf ppf "DW_AT_data_bit_offset"
    | Const_expr -> Format.fprintf ppf "DW_AT_const_expr"
    | Enum_class -> Format.fprintf ppf "DW_AT_enum_class"
    | Linkage_name -> Format.fprintf ppf "DW_AT_linkage_name"
    | String_length_bit_size ->
        Format.fprintf ppf "DW_AT_string_length_bit_size"
    | String_length_byte_size ->
        Format.fprintf ppf "DW_AT_string_length_byte_size"
    | Rank -> Format.fprintf ppf "DW_AT_rank"
    | Str_offsets_base -> Format.fprintf ppf "DW_AT_str_offsets_base"
    | Addr_base -> Format.fprintf ppf "DW_AT_addr_base"
    | Rnglists_base -> Format.fprintf ppf "DW_AT_rnglists_base"
    | Dwo_name -> Format.fprintf ppf "DW_AT_dwo_name"
    | Reference -> Format.fprintf ppf "DW_AT_reference"
    | Rvalue_reference -> Format.fprintf ppf "DW_AT_rvalue_reference"
    | Macros -> Format.fprintf ppf "DW_AT_macros"
    | Call_all_calls -> Format.fprintf ppf "DW_AT_call_all_calls"
    | Call_all_source_calls -> Format.fprintf ppf "DW_AT_call_all_source_calls"
    | Call_all_tail_calls -> Format.fprintf ppf "DW_AT_call_all_tail_calls"
    | Call_return_pc -> Format.fprintf ppf "DW_AT_call_return_pc"
    | Call_value -> Format.fprintf ppf "DW_AT_call_value"
    | Call_origin -> Format.fprintf ppf "DW_AT_call_origin"
    | Call_parameter -> Format.fprintf ppf "DW_AT_call_parameter"
    | Call_pc -> Format.fprintf ppf "DW_AT_call_pc"
    | Call_tail_call -> Format.fprintf ppf "DW_AT_call_tail_call"
    | Call_target -> Format.fprintf ppf "DW_AT_call_target"
    | Call_target_clobbered -> Format.fprintf ppf "DW_AT_call_target_clobbered"
    | Call_data_location -> Format.fprintf ppf "DW_AT_call_data_location"
    | Call_data_value -> Format.fprintf ppf "DW_AT_call_data_value"
    | Noreturn -> Format.fprintf ppf "DW_AT_noreturn"
    | Alignment -> Format.fprintf ppf "DW_AT_alignment"
    | Export_symbols -> Format.fprintf ppf "DW_AT_export_symbols"
    | Deleted -> Format.fprintf ppf "DW_AT_deleted"
    | Defaulted -> Format.fprintf ppf "DW_AT_defaulted"
    | Loclists_base -> Format.fprintf ppf "DW_AT_loclists_base"
    | Reserved -> Format.fprintf ppf "reserved"
    | Custom x -> Format.fprintf ppf "custom(%d)" x
end

module rec Class : sig
  type t =
    | Address of Virtual_address.t
    | Block of string
    | Constant of Z.t
    | Exprloc of Expr.t
    | Flag of bool
    | Offset of int
    | Reference of Die.t lazy_t
    | String of string

  module Constant : sig
    val get : t -> Z.t
  end

  module String : sig
    val get : t -> string
  end

  module Exprloc : sig
    val get : t -> Expr.t
  end

  module Ref : sig
    val get : t -> Die.t
  end

  val pp : Format.formatter -> t -> unit
end = struct
  type t =
    | Address of Virtual_address.t
    | Block of string
    | Constant of Z.t
    | Exprloc of Expr.t
    | Flag of bool
    | Offset of int
    | Reference of Die.t lazy_t
    | String of string

  module Constant = struct
    let get = function
      | Constant c -> c
      | _ -> raise (Invalid_argument "Constant.get")
  end

  module String = struct
    let get = function
      | String s -> s
      | _ -> raise (Invalid_argument "String.get")
  end

  module Exprloc = struct
    let get = function
      | Exprloc e -> e
      | _ -> raise (Invalid_argument "Exprloc.get")
  end

  module Ref = struct
    let get = function
      | Reference (lazy r) -> r
      | _ -> raise (Invalid_argument "Ref.get")
  end

  let pp ppf v =
    match v with
    | Address x -> Virtual_address.pp ppf x
    | Block b -> Format.fprintf ppf "Block%a" String_utils.pp_hex b
    | Constant c -> Z.pp_print ppf c
    | Exprloc ops -> Format.fprintf ppf "%a" Expr.pp ops
    | Flag f -> Format.fprintf ppf "%b" f
    | Offset x -> Format.fprintf ppf "Offset %d" x
    | Reference (lazy { attributes; _ }) ->
        if Attribute.Map.mem Attribute.Name attributes then
          Format.fprintf ppf "Reference to %a" Class.pp
            (Attribute.Map.find Attribute.Name attributes)
        else Format.fprintf ppf "Reference to unamed DIE"
    | String s -> Format.fprintf ppf "%s" s
end

and Die : sig
  type t = {
    offset : int;
    tag : Tag.t;
    attributes : Class.t Attribute.Map.t;
    children : t list;
    parent : t lazy_t;
  }

  val tag : t -> Tag.t
  val get : Attribute.t -> t -> Class.t
  val children : t -> t list
  val parent : t -> t
  val pp : Format.formatter -> t -> unit
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
end = struct
  type t = {
    offset : int;
    tag : Tag.t;
    attributes : Class.t Attribute.Map.t;
    children : t list;
    parent : t lazy_t;
  }

  let tag { tag; _ } = tag
  let get attr { attributes; _ } = Attribute.Map.find attr attributes
  let children { children; _ } = children
  let parent { parent; _ } = Lazy.force parent
  let hash { offset; _ } = offset
  let equal { offset; _ } { offset = offset'; _ } = offset = offset'
  let compare { offset; _ } { offset = offset'; _ } = offset - offset'

  let rec pp ppf { tag; attributes; children; _ } : unit =
    let common tag attributes =
      Format.fprintf ppf "%a@ %a" Tag.pp tag
        (fun ppf attrs ->
          Attribute.Map.iter
            (fun attr klass ->
              Format.fprintf ppf "%a : %a@ " Attribute.pp attr Class.pp klass)
            attrs)
        attributes
    in
    common tag attributes;
    if children <> [] then (
      Format.fprintf ppf "@ @ ";
      let rec loop = function
        | [] -> ()
        | children :: tail ->
            pp ppf children;
            Format.fprintf ppf "@ ";
            loop tail
      in
      loop children)
end

module Header = struct
  type t = {
    offset : int;
    format : [ `x32 | `x64 ];
    length : int;
    version : int;
    abbrev_offset : int;
    pointer_size : [ `x32 | `x64 ];
  }

  let load cursor : t =
    let offset = get_pos cursor in
    let format, length =
      let length = Read.u32 cursor in
      if Uint32.to_int32 length = 0xffffffffl then
        (`x64, Uint64.to_int (Read.u64 cursor))
      else (`x32, Uint32.to_int length)
    in
    let version = Uint16.to_int (Read.u16 cursor) in
    if version <> 4 then
      Logger.fatal "trying to read Compilation Unit header with version <> 4";
    let abbrev_offset = Int64.to_int (Utils.read format cursor) in
    let pointer_size =
      match Uint8.to_int (Read.u8 cursor) with
      | 4 -> `x32
      | 8 -> `x64
      | n -> Logger.fatal "invalid pointer size %d" n
    in
    { offset; format; length; version; abbrev_offset; pointer_size }

  let abbrev_offset h : int = h.abbrev_offset
  let read_addr h cursor = Utils.read_address h.pointer_size cursor
  let read_offset h cursor = Int64.to_int (Utils.read h.format cursor)

  let pp ppf h : unit =
    Format.fprintf ppf "@[<v>Length: 0x%x (%a-bit)@ " h.length
      Machine.Bitwidth.pp
      (h.format :> Machine.bitwidth);
    Format.fprintf ppf "Version: %d@ " h.version;
    Format.fprintf ppf "Abbrev Offset: 0x%x@ " h.abbrev_offset;
    Format.fprintf ppf "Pointer Size: %d"
      (match h.pointer_size with `x32 -> 4 | `x64 -> 8);
    Format.pp_close_box ppf ()
end

module Form = struct
  type t =
    | Null
    | Addr
    | Block2
    | Block4
    | Data2
    | Data4
    | Data8
    | String
    | Block
    | Block1
    | Data1
    | Flag
    | Sdata
    | Strp
    | Udata
    | Ref_addr
    | Ref1
    | Ref2
    | Ref4
    | Ref8
    | Ref_udata
    | Indirect
    | Sec_offset
    | Exprloc
    | Flag_present
    | Strx
    | Addrx
    | Ref_sup4
    | Strp_sup
    | Data16
    | Line_strp
    | Ref_sig8
    | Implicit_const
    | Loclistx
    | Rnglistx
    | Ref_sup8
    | Strx1
    | Strx2
    | Strx3
    | Strx4
    | Addrx1
    | Addrx2
    | Addrx3
    | Addrx4
    | Reserved

  let of_value = function
    | 0x00 -> Null
    | 0x01 -> Addr
    | 0x03 -> Block2
    | 0x04 -> Block4
    | 0x05 -> Data2
    | 0x06 -> Data4
    | 0x07 -> Data8
    | 0x08 -> String
    | 0x09 -> Block
    | 0x0a -> Block1
    | 0x0b -> Data1
    | 0x0c -> Flag
    | 0x0d -> Sdata
    | 0x0e -> Strp
    | 0x0f -> Udata
    | 0x10 -> Ref_addr
    | 0x11 -> Ref1
    | 0x12 -> Ref2
    | 0x13 -> Ref4
    | 0x14 -> Ref8
    | 0x15 -> Ref_udata
    | 0x16 -> Indirect
    | 0x17 -> Sec_offset
    | 0x18 -> Exprloc
    | 0x19 -> Flag_present
    | 0x1a -> Strx
    | 0x1b -> Addrx
    | 0x1c -> Ref_sup4
    | 0x1d -> Strp_sup
    | 0x1e -> Data16
    | 0x1f -> Line_strp
    | 0x20 -> Ref_sig8
    | 0x21 -> Implicit_const
    | 0x22 -> Loclistx
    | 0x23 -> Rnglistx
    | 0x24 -> Ref_sup8
    | 0x25 -> Strx1
    | 0x26 -> Strx2
    | 0x27 -> Strx3
    | 0x28 -> Strx4
    | 0x29 -> Addrx1
    | 0x2a -> Addrx2
    | 0x2b -> Addrx3
    | 0x2c -> Addrx4
    | _ -> Reserved

  let read cursor : t = of_value (Uint8.to_int (Read.u8 cursor))

  let pp ppf = function
    | Null -> Format.fprintf ppf "null"
    | Addr -> Format.fprintf ppf "DW_FORM_addr"
    | Block2 -> Format.fprintf ppf "DW_FORM_block2"
    | Block4 -> Format.fprintf ppf "DW_FORM_block4"
    | Data2 -> Format.fprintf ppf "DW_FORM_data2"
    | Data4 -> Format.fprintf ppf "DW_FORM_data4"
    | Data8 -> Format.fprintf ppf "DW_FORM_data8"
    | String -> Format.fprintf ppf "DW_FORM_string"
    | Block -> Format.fprintf ppf "DW_FORM_block"
    | Block1 -> Format.fprintf ppf "DW_FORM_block1"
    | Data1 -> Format.fprintf ppf "DW_FORM_data1"
    | Flag -> Format.fprintf ppf "DW_FORM_flag"
    | Sdata -> Format.fprintf ppf "DW_FORM_sdata"
    | Strp -> Format.fprintf ppf "DW_FORM_strp"
    | Udata -> Format.fprintf ppf "DW_FORM_udata"
    | Ref_addr -> Format.fprintf ppf "DW_FORM_ref_addr"
    | Ref1 -> Format.fprintf ppf "DW_FORM_ref1"
    | Ref2 -> Format.fprintf ppf "DW_FORM_ref2"
    | Ref4 -> Format.fprintf ppf "DW_FORM_ref4"
    | Ref8 -> Format.fprintf ppf "DW_FORM_ref8"
    | Ref_udata -> Format.fprintf ppf "DW_FORM_ref_udata"
    | Indirect -> Format.fprintf ppf "DW_FORM_indirect"
    | Sec_offset -> Format.fprintf ppf "DW_FORM_sec_offset"
    | Exprloc -> Format.fprintf ppf "DW_FORM_exprloc"
    | Flag_present -> Format.fprintf ppf "DW_FORM_flag_present"
    | Strx -> Format.fprintf ppf "DW_FORM_strx"
    | Addrx -> Format.fprintf ppf "DW_FORM_addrx"
    | Ref_sup4 -> Format.fprintf ppf "DW_FORM_ref_sup4"
    | Strp_sup -> Format.fprintf ppf "DW_FORM_strp_sup"
    | Data16 -> Format.fprintf ppf "DW_FORM_data16"
    | Line_strp -> Format.fprintf ppf "DW_FORM_line_strp"
    | Ref_sig8 -> Format.fprintf ppf "DW_FORM_ref_sig8"
    | Implicit_const -> Format.fprintf ppf "DW_FORM_implicit_const"
    | Loclistx -> Format.fprintf ppf "DW_FORM_loclistx"
    | Rnglistx -> Format.fprintf ppf "DW_FORM_rnglistx"
    | Ref_sup8 -> Format.fprintf ppf "DW_FORM_ref_sup8"
    | Strx1 -> Format.fprintf ppf "DW_FORM_strx1"
    | Strx2 -> Format.fprintf ppf "DW_FORM_strx2"
    | Strx3 -> Format.fprintf ppf "DW_FORM_strx3"
    | Strx4 -> Format.fprintf ppf "DW_FORM_strx4"
    | Addrx1 -> Format.fprintf ppf "DW_FORM_addrx1"
    | Addrx2 -> Format.fprintf ppf "DW_FORM_addrx2"
    | Addrx3 -> Format.fprintf ppf "DW_FORM_addrx3"
    | Addrx4 -> Format.fprintf ppf "DW_FORM_addrx4"
    | Reserved -> Format.fprintf ppf "reserved"

  let load isa cursor header strings dies form : Class.t =
    match form with
    | Null -> assert false
    | Addr -> Address (Header.read_addr header cursor)
    | Block2 ->
        Block
          (String.init
             (Uint16.to_int (Read.u16 cursor))
             (fun _ -> Uint8.to_char (Read.u8 cursor)))
    | Block4 ->
        Block
          (String.init
             (Uint32.to_int (Read.u32 cursor))
             (fun _ -> Uint8.to_char (Read.u8 cursor)))
    | Data2 -> Constant (Uint16.to_bigint (Read.u16 cursor))
    | Data4 -> Constant (Uint32.to_bigint (Read.u32 cursor))
    | Data8 -> Constant (Uint64.to_bigint (Read.u64 cursor))
    | String -> String (Read.zero_string "" cursor ())
    | Block ->
        Block
          (String.init
             (Z.to_int (Read.uleb128 cursor))
             (fun _ -> Uint8.to_char (Read.u8 cursor)))
    | Block1 ->
        Block
          (String.init
             (Uint8.to_int (Read.u8 cursor))
             (fun _ -> Uint8.to_char (Read.u8 cursor)))
    | Data1 -> Constant (Uint8.to_bigint (Read.u8 cursor))
    | Flag -> Flag (Uint8.to_int (Read.u8 cursor) > 0)
    | Sdata -> Constant (Read.sleb128 cursor)
    | Strp ->
        let offset = Uint32.to_int (Read.u32 cursor) in
        String (Str.get strings offset)
    | Udata -> Constant (Read.uleb128 cursor)
    | Ref_addr ->
        ignore @@ Header.read_addr header cursor;
        raise @@ Errors.not_yet_implemented @@ Format.asprintf "%a" pp Ref_addr
    | Ref1 ->
        let offset = header.Header.offset + Uint8.to_int (Read.u8 cursor) in
        Reference (lazy (Hashtbl.find dies offset))
    | Ref2 ->
        let offset = header.Header.offset + Uint16.to_int (Read.u16 cursor) in
        Reference (lazy (Hashtbl.find dies offset))
    | Ref4 ->
        let offset = header.Header.offset + Uint32.to_int (Read.u32 cursor) in
        Reference (lazy (Hashtbl.find dies offset))
    | Ref8 ->
        let offset = header.Header.offset + Uint64.to_int (Read.u64 cursor) in
        Reference (lazy (Hashtbl.find dies offset))
    | Ref_udata ->
        let offset = header.Header.offset + Z.to_int (Read.uleb128 cursor) in
        Reference (lazy (Hashtbl.find dies offset))
    | Sec_offset ->
        let offset = Header.read_offset header cursor in
        Offset offset
    | Exprloc ->
        let size = Z.to_int (Read.uleb128 cursor) in
        Exprloc (Expr.load isa header.Header.format size cursor)
    | Flag_present -> Flag true
    | Ref_sig8 ->
        ignore @@ Read.u64 cursor;
        raise @@ Errors.not_yet_implemented @@ Format.asprintf "%a" pp Ref_sig8
    | x -> raise @@ Errors.not_yet_implemented @@ Format.asprintf "%a" pp x
end

module Encoding = struct
  type t = Attribute.t * Form.t

  let read cursor : t option =
    match Attribute.read cursor with
    | Attribute.Null -> None
    | attribute -> Some (attribute, Form.read cursor)

  let pp ppf (attr, form) : unit =
    Format.fprintf ppf "%a\t%a" Attribute.pp attr Form.pp form
end

module Abbrev = struct
  module A = struct
    type t = Tag.t * bool * Encoding.t list

    let read cursor : (int * t) option =
      match Uint8.to_int (Read.u8 cursor) with
      | 0 -> None
      | no ->
          let tag = Tag.read cursor in
          let has_children = Uint8.to_int (Read.u8 cursor) > 0 in
          let rec loop encodings =
            match Encoding.read cursor with
            | None ->
                advance cursor 1;
                List.rev encodings
            | Some encoding -> loop (encoding :: encodings)
          in
          let encodings = loop [] in
          Some (no, (tag, has_children, encodings))

    let pp ppf (tag, has_children, encodings) : unit =
      Format.fprintf ppf "@[<v 1>%a [%a]@ %a@]" Tag.pp tag
        (fun ppf -> function
          | true -> Format.fprintf ppf "has children"
          | false -> Format.fprintf ppf "no children")
        has_children
        (fun ppf encodings ->
          let rec loop = function
            | [] -> ()
            | [ encoding ] -> Format.fprintf ppf "%a" Encoding.pp encoding
            | encoding :: tail ->
                Format.fprintf ppf "%a@ " Encoding.pp encoding;
                loop tail
          in
          loop encodings)
        encodings
  end

  type t = A.t array

  let load cursor : t =
    let rec loop i abbrevs =
      match A.read cursor with
      | None -> Array.of_list (List.rev abbrevs)
      | Some (no, abbrev) ->
          assert (i = no);
          loop (i + 1) (abbrev :: abbrevs)
    in
    let abbrevs = loop 1 [] in
    abbrevs

  let pp ppf abbrevs : unit =
    Array.iteri
      (fun no abbrev -> Format.fprintf ppf "%4d %a@ " (no + 1) A.pp abbrev)
      abbrevs
end

type t = Die.t

let no_parent () = raise (Invalid_argument "parent")

let load img : t list =
  let isa = Loader.Img.arch img in
  let strings = Str.load img in
  Logger.debug "%a" (Str.pp isa) strings;
  let cursor' = section img ".debug_abbrev" in
  let cursor = section img ".debug_info" in
  let dies = Hashtbl.create 100 in
  let rec outer units =
    if at_end cursor then List.rev units
    else
      let header = Header.load cursor in
      Logger.debug "%a" Header.pp header;
      set_pos cursor' (Header.abbrev_offset header);
      let abbrevs = Abbrev.load cursor' in
      Logger.debug "%a" Abbrev.pp abbrevs;
      let rec inner attrs = function
        | [] -> attrs
        | (attr, form) :: tail -> (
            try
              let klass = Form.load isa cursor header strings dies form in
              inner (Attribute.Map.add attr klass attrs) tail
            with Failure msg ->
              Logger.warning "%s" msg;
              inner attrs tail)
      in
      let rec load ~parent sibling =
        let offset = get_pos cursor in
        match Uint8.to_int (Read.u8 cursor) with
        | 0 -> List.rev sibling
        | x ->
            let tag, has_children, encodings = Array.get abbrevs (x - 1) in
            let attributes = inner Attribute.Map.empty encodings in
            let die : Die.t =
              match has_children with
              | false -> { offset; tag; attributes; children = []; parent }
              | true ->
                  let rec die : Die.t lazy_t =
                    lazy
                      {
                        offset;
                        tag;
                        attributes;
                        children = load ~parent:die [];
                        parent;
                      }
                  in
                  Lazy.force die
            in
            Hashtbl.add dies offset die;
            load ~parent (die :: sibling)
      in
      let offset = get_pos cursor in
      let x = Uint8.to_int (Read.u8 cursor) in
      let tag, has_children, encodings = Array.get abbrevs (x - 1) in
      let attributes = inner Attribute.Map.empty encodings in
      let parent = Lazy.from_fun no_parent in
      let die : Die.t =
        match has_children with
        | false -> { offset; tag; attributes; children = []; parent }
        | true ->
            let rec die : Die.t lazy_t =
              lazy
                {
                  offset;
                  tag;
                  attributes;
                  children = load ~parent:die [];
                  parent;
                }
            in
            Lazy.force die
      in
      Hashtbl.add dies offset die;
      outer (die :: units)
  in
  outer []

let dir : Die.t -> string = function
  | { attributes; _ } ->
      Class.String.get (Attribute.Map.find Attribute.Comp_dir attributes)

let file : Die.t -> string = function
  | { attributes; _ } ->
      let filename =
        Class.String.get (Attribute.Map.find Attribute.Name attributes)
      in
      if Filename.is_relative filename then
        Filename.concat
          (Class.String.get (Attribute.Map.find Attribute.Comp_dir attributes))
          filename
      else filename

module Type = struct
  type t = Die.t

  let name typ = Class.String.get (Die.get Attribute.Name typ)
  let pp ppf typ = Format.fprintf ppf "%s" (name typ)
end

module Var = struct
  type var = Die.t
  type t = var

  let name var = Class.String.get (Die.get Attribute.Name var)
  let line var = Z.to_int (Class.Constant.get (Die.get Attribute.Decl_line var))
  let typ var = Class.Ref.get (Die.get Attribute.Type var)

  let _find unit vname vline =
    let rec walk : Die.t list -> Die.t = function
      | [] -> raise Not_found
      | t :: l -> (
          match t with
          | { tag = Variable | Formal_parameter; children = []; _ } -> (
              match (name t, line t) with
              | name, line -> if name = vname && line = vline then t else walk l
              | exception Not_found -> walk l)
          | { children = []; _ } -> walk l
          | { children; _ } -> List.concat [ children; l ] |> walk)
    in
    walk [ unit ]

  let pp ppf var = Format.fprintf ppf "%a %s" Type.pp (typ var) (name var)
end

module Func = struct
  type func = Die.t
  type t = func

  let name t = Class.String.get (Die.get Attribute.Name t)
  let line t = Z.to_int (Class.Constant.get (Die.get Attribute.Decl_line t))
  let typ t = Class.Ref.get (Die.get Attribute.Type t)
  let cfa t = Class.Exprloc.get (Die.get Attribute.Frame_base t)

  let vars : Die.t -> Die.t list = function
    | { children = []; _ } -> []
    | { children; _ } ->
        let rec walk vars = function
          | [] -> vars
          | d :: l -> (
              match (d : t) with
              | { tag = Variable | Formal_parameter; _ } -> walk (d :: vars) l
              | { tag = Inlined_subroutine; _ } -> vars
              | { children; _ } -> walk (walk vars children) l)
        in
        walk [] children

  let find unit fname =
    let rec walk : Die.t list -> Die.t = function
      | [] -> raise Not_found
      | t :: l -> (
          match t with
          | { tag = Subprogram; children = []; _ } -> (
              match name t with
              | name -> if name = fname then t else walk l
              | exception Not_found -> walk l)
          | { tag = Subprogram; children; _ } -> (
              match name t with
              | name ->
                  if name = fname then t
                  else List.concat [ children; l ] |> walk
              | exception Not_found -> List.concat [ children; l ] |> walk)
          | { children = []; _ } -> walk l
          | { children; _ } -> List.concat [ children; l ] |> walk)
    in
    walk [ unit ]
end

module Global = struct
  let vars : Die.t -> Die.t list = function
    | { children; _ } ->
        let rec walk : Die.t list -> Die.t list -> Die.t list =
         fun vars -> function
          | [] -> vars
          | d :: l -> (
              match d with
              | { tag = Subprogram; _ } -> walk vars l
              | { tag = Variable; _ } -> walk (d :: vars) l
              | { children; _ } -> List.concat [ children; l ] |> walk vars)
        in
        walk [] children
end

let pp = Die.pp
