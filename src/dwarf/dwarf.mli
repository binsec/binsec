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

module Logger : Binsec_base.Logger.S

module Expr : sig
  val map : Machine.isa -> int -> Dba.Expr.t
  (** [map isa n] according to the DWARF Register Number Mapping *)

  type t

  val load : Machine.isa -> [ `x32 | `x64 ] -> int -> int Reader.t -> t
  (** [load isa blocksize cursor] read a DWARF expression at the current cursor position *)

  val loc : Machine.isa -> ?cfa:Dba.Expr.t -> t -> int -> Dba.Expr.t
  (** [loc isa ~cfa expr bitsize] interpret the expression expr
    according to the Canonical Frame Address *)

  val cfa : Machine.isa -> t -> Dba.Expr.t
  (** [cfa isa expr] interpret the expression expr as a Canonical Frame Address *)

  include Sigs.PRINTABLE with type t := t
end

module Cunit : sig
  module Tag : sig
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

    include Sigs.PRINTABLE with type t := t
  end

  module Attribute : sig
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

    include Sigs.PRINTABLE with type t := t
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
  end

  and Die : sig
    type t

    val tag : t -> Tag.t
    val get : Attribute.t -> t -> Class.t
    val children : t -> t list
    val parent : t -> t
    val pp : Format.formatter -> t -> unit
    val hash : t -> int
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end

  type t = Die.t

  val load : Loader.Img.t -> t list

  val dir : t -> string
  (** [dir cunit] return the path from where the compiler have proceed *)

  val file : t -> string
  (** [file cunit] return the path of the proceesed file
      of the compilation unit *)

  module Type : sig
    type t = private Die.t

    val name : t -> string
    (** [name type] return the name of the type declaration *)

    include Sigs.PRINTABLE with type t := t
  end

  module Var : sig
    type t = private Die.t

    val name : t -> string
    (** [name var] return the name of the variable var *)

    val line : t -> int
    (** [line var] return the line of the declaration of the variable var *)

    val typ : t -> Type.t
    (** [typ var] return the type of the declaration of the variable var *)

    include Sigs.PRINTABLE with type t := t
  end

  module Func : sig
    type func = private Die.t

    val find : t -> string -> func
    (** [find cunit func_name] return the function named func_name
        of the compilation unit
        @raise Exception Not_found *)

    type t = func

    val name : t -> string
    (** [name func] return the name of the declaration of the function func *)

    val line : t -> int
    (** [line func] return the line of the declaration of the function func *)

    val typ : t -> Type.t
    (** [typ func] return the type of the declaration of the function func *)

    val cfa : t -> Expr.t
    (** [cfa func] return the Canonical Frame Address of the function func *)

    val vars : t -> Var.t list
    (** [vars func] return the list of local variables
        declared in the function func *)
  end

  module Global : sig
    val vars : t -> Var.t list
    (** [vars cunit] return the list of global variables
        declared in the compilation unit *)
  end

  include Sigs.PRINTABLE with type t := t
end

module Frame : sig
  type rule = Undef | Same | Value of Dba.Expr.t
  type entry

  val addresses : entry -> Virtual_address.t Interval.t
  (** [address entry] return the address range of the entry *)

  val cfa : entry -> Dba.Expr.t
  (** [cfa entry] return the canonical frame address of the entry *)

  val rule : int -> entry -> rule
  (** [rule n entry] return the rule of the n'th column of the entry *)

  type t

  val load : Loader.Img.t -> t
  (** [load img] extract and interpret the content
    of either .debug_frame or .eh_frame section *)

  val fold :
    ('a -> return_address:int -> columns:int array -> entry -> 'a) ->
    'a ->
    t ->
    'a
  (** [fold f frame] iterate through the frame matrix
    columns is the list of valid column indexes of the given entry
    return_address is the column index of the return address of the function *)

  val iter :
    (return_address:int -> columns:int array -> entry -> unit) -> t -> unit
  (** [iter frame] same as fold but without return *)

  include Sigs.PRINTABLE with type t := t
end

module Lines : sig
  type entry = {
    addresses : Virtual_address.t Interval.t;
    path : string;
    line : int;
    column : int;
    is_stmt : bool;
    basic_block : bool;
    discriminator : int;
  }
  (** represent one or more rows of the addresse / line matrix
    [addresses]     the range of virtual addresses of the entry
    [path]          the path of the processed file
    [line]          the line of the source (starting from 1)
    [column]        the column (non reliable, old compilers do not produce it)
    [is_stmt]       if the entry correspond to a statement in the source
    [basic_block]   if the entry is the start of a basic block
    [discriminator] an integer identifying the block to which the entry belong
*)

  type t

  val load : Loader.Img.t -> t
  (** [load img] extract and interpret the content of .debug_line section *)

  val fold : ('a -> entry -> 'a) -> 'a -> t -> 'a
  (** [fold f line] iterate through the line matrix *)

  val iter : (entry -> unit) -> t -> unit
  (** [iter f line] same as fold but without return *)

  include Sigs.PRINTABLE with type t := t
end

module Loclist : sig
  type entry = private
    | Entry of {
        offset : int;
        addresses : Virtual_address.t Interval.t;
        expr : Expr.t;
      }
    | Eol of int

  type t = entry list

  val load : Loader.Img.t -> t Basic_types.Integers.Int.Map.t
  (** [load img] extract and interpret the content of .debug_loc section *)

  include Sigs.PRINTABLE with type t := Machine.isa * t
end

type t = private {
  isa : Machine.isa;
  units : Cunit.t list;
  frame : Frame.t;
  lines : Lines.t;
  loc : Loclist.t Basic_types.Integers.Int.Map.t;
}

val load : Loader.Img.t -> t
(** [load img] extract and interpret the content of debugging sections *)

include Sigs.PRINTABLE with type t := t
