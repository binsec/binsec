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

(** Modules & types related to DBA types *)

open Sigs

type instruction_sequence = (Dba.address *  Dba.instruction) list

module Call_stack : Comparable with type t = (Dba.address * Dba.address) list

module Region : sig
  include Collection with type t = Dba.region

  val malloc : int -> t
  include Printable with type t := Dba.region
end


(** {2 Dba address / Code address } *)

(** A DBA instruction is uniquely located at an address + label/id

    Such a location is named "code address" (or [Caddress])
*)
module Caddress : sig
  type t = Dba.address

  include Collection with type t := t

  module Hashtbl : Hashtbl.S with type key = t

  val default_init : t ref
  val create : Bitvector.t -> int -> t
  val block_start : Bitvector.t -> t
  (** [block_start bv] i [create bv 0] *)

  val block_start_of_int : int -> t

  val rebase : t -> Bitvector.t -> t
  val reid : t -> int -> t

  val equal : t -> t -> bool
  val pp_base : Format.formatter -> t -> unit
  (** [pp_base caddr] only print the base address of the DBA code address as
      hexadecimal, thus ignoring the [id] part
  *)

  val add_int : t -> int -> t
  (** [add_int addr n] Increment the current address from the value of [n] *)

  val add_id : t -> int -> t

  val base_value : t -> Bigint.t

  include Comparable with type t := t
end

(** {2 Virtual addresses} *)

module Virtual_address : sig
  (** A virtual address is a simple location information corresponding to a
      physical (virtual) address of the underlying machine.
  *)

  type t = private int
  val create : int -> t
  val of_int64 : int64 -> t
  val of_bitvector : Bitvector.t -> t

  val of_code_address : Caddress.t -> t
  val to_code_address : t -> Caddress.t
  
  val to_int64 : t -> int64

  val of_bigint : Bigint.t -> t
  val to_bigint : t -> Bigint.t

  val add_int : t -> int -> t
  include Printable with type t := t
  include Collection with type t := t
  module Hashtbl : Hashtbl.S with type key = t
end


module AddressStack : sig
  type t =  Dba.address * Call_stack.t * int
  val pp : Format.formatter -> t -> unit
  include Collection with type t := t
end



module Rights : sig
  type action = R | W | X
  include Map.S with type key = action * Dba.region
  val find_read_right   : Dba.region -> 'a t -> 'a
  val find_write_right  : Dba.region -> 'a t -> 'a
  val find_exec_right   : Dba.region -> 'a t -> 'a
end

(** {2 DBA AST modules} *)

module BinaryOperator : sig
  type t = Dba.binary_op
  val has_inverse : t -> bool
  val invert : t -> t
  (** [invert t] inverts [t] if it has an inverse version.
      Raise [Failure "BinaryOperator.invert "] otherwise
  *)
end

module rec Condition : sig
  type t = Dba.cond
  include Eq with type t := t

  val is_symbolic : t -> bool

  (** {7 Construction functions} *)
  val cnot : t -> t
  val cand : t -> t -> t
  val cor  : t -> t -> t
  val creified : Expr.t -> t
end


and Expr : sig
  type t = Dba.expr

  include Eq with type t := t
  include Printable with type t := t 

  (** {6 Constructors } *)
  val var : string -> Basic_types.BitSize.t -> Dba.vartag option -> t

  val flag : ?bits:Basic_types.BitSize.t -> string -> t
  (** [flag ~bits flagname] constructs a variable named [flagname] tagged as a
      flag.
      [bits] defaults to [Basic_types.BitSize.bits1].
  *)

  val temporary : string -> Basic_types.BitSize.t -> t
  (** [temporary name nbits] constructs a variable named [name] of size [nbits]
      flagged as a temporary *)

  val constant : ?region:Dba.region -> Bitvector.t -> t
  (** [constant ~region bv] creates a constant expression from the bitvector
      [bv] from region [region].
      Default region is [`Constant].
  *)

  val zeros : int -> t
  (** [zeros n] creates a constant expression of value 0 with length [n]*)

  val ones : int -> t
  (** [ones n] creates a constant expression of value 1 with length [n].
      I.e. it has (n - 1) zeros in binary.
  *)

  val one : t
  val zero : t

  val binary : Dba.binary_op -> t -> t -> t
  (** [binary boperator loperand roperand] creates a binary expression from
      [boperator] with left operand [loperand] and right operand [roperand].
  *)

  val add                : t -> t -> t
  val sub                : t -> t -> t
  val umul               : t -> t -> t
  val smul               : t -> t -> t
  val smod               : t -> t -> t
  val umod               : t -> t -> t
  val udiv               : t -> t -> t
  val sdiv               : t -> t -> t
  val append             : t -> t -> t
  val eq                 : t -> t -> t
  val diff               : t -> t -> t
  val ule                : t -> t -> t
  val sle                : t -> t -> t
  val ult                : t -> t -> t
  val slt                : t -> t -> t
  val uge                : t -> t -> t
  val sge                : t -> t -> t
  val ugt                : t -> t -> t
  val sgt                : t -> t -> t

  val unary : Dba.unary_op -> t -> t
  val uminus             : t -> t

  include Sigs.Bitwise with type t := t
  val shift_left  : t -> t -> t
  val shift_right : t -> t -> t
  val shift_right_signed : t -> t -> t
  val rotate_left  : t -> t -> t
  val rotate_right : t -> t -> t

  val sext : t -> Basic_types.BitSize.t -> t
  val uext : t -> Basic_types.BitSize.t -> t

  val ite : Dba.cond -> t -> t -> t
  (** [ite cond then_e else_e] creates [Dba.ExprIte(cond, then_e, else_e)] *)

  val restrict : t -> int -> int -> t
  (** [restrict e off1 off2] creates [Dba.ExprRestrict(e, off1, off2)]  if
      [off2 >= off1 && off1 >=0] .
  *)

  val restrict_to_bit : t -> int -> t
  (** [restrict_to_bit e o] is [restrict e o o] *)

  val bool_false : t
  val bool_true : t
  (** Encoding of booleans as DBA expressions *)

  val load : Basic_types.ByteSize.t -> Dba.endianness -> t -> t

  val temp: Basic_types.BitSize.t -> t
  (** [temp n] creates an expression representing a temporary of size [n] with name
  [Format.sprintf "temp%d" n]. *)

  val of_lvalue : Dba.lhs -> t
  (** {6 Operations } *)


  (** {6 Predicates } *)
  val is_symbolic : t -> bool

  val is_zero : t -> bool
  (** [is_zero e] is [true] if [e]'s value is equal to 0 whatever its length is *)

  val is_one : t -> bool
  (** [is_one e] is [true] if [e]'s value is equal to 1 whatever its length is *)

  val is_max : t -> bool
  (** [is_max e] is [true] if [e] is a constant representing the maximum value for
   ** its size *)

  (** {6 Utils } *)
  val size_of : t -> int
end

module LValue : sig
  type t = Dba.lhs
  include Eq with type t := t
  module Map : Map.S with type key = t

  val var : bitsize:Basic_types.BitSize.t -> string -> Dba.vartag option -> t
  (** [var name size tagopt] creates a DBA lvalue for a variable *)
  
  val flag : ?bitsize:Basic_types.BitSize.t -> ?subflag:Dba.compare -> string -> t
  (** [flag ~size ~subflag fname] creates a variable whose flag is of the
      subtype [subflag].
      - [size] defaults to 1
      - [subflage] defaults to [Dba.FlgUnspecified]
  *)

  val temporary : string -> Basic_types.BitSize.t -> t
  val restrict : string -> Basic_types.BitSize.t -> int -> int -> t
  val restrict_to_bit : string -> Basic_types.BitSize.t -> int -> t
  val store : Basic_types.ByteSize.t -> Dba.endianness -> Dba.expr -> t

  val temp: Basic_types.BitSize.t -> t
  (** [temp n] creates a lvalue representing a temporary of size [n] with name
      [Format.sprintf "temp%d" n]. *)

  val name_of : t -> string option
  (** [name_of lval] returns [Some name] if the lvalue is named, [None]
    otherwise *)

  val bitsize : t -> Basic_types.BitSize.t
  val unsafe_bitsize : t -> int

  val is_expr_translatable : Dba.expr -> bool
  (** [is_expr_translatable e] returns true is the expression can have a valid
      lvalue translation *)
  
  val of_expr : Dba.expr -> t
  (** [of_expr e] translates an expression to its lvalue equivalent if possible.

      Use [is_expr_translatable e] to check the feasability of this translation.

      @raise Failure "LValue.of_expr ..." if it is not possible.
  *)

  val is_temporary : t -> bool
  val is_flag : t -> bool

  val resize : Basic_types.BitSize.t -> t -> t
  (** [resize bitsize lv] patches the lvalue [lv] and gives it a size of
      [bitsize].
   *)
end

module Tag : sig
  type t = Dba.tag
  include Eq with type t := t
end


module Jump_target : sig
  type t = Dba.jump_target

  val outer : Dba.address -> t
  val inner : int -> t

  val outer_jumps : t -> Virtual_address.Set.t
end

type ('a, 'b) defuse = {
  defs: 'a;
  uses: 'b;
}


module Instruction : sig
  type t = Dba.instruction

  (** {7 Constructors} *)
  val assign : LValue.t -> Expr.t -> int -> t
  val ite : Condition.t -> Jump_target.t -> int -> t
  val undefined : LValue.t -> int -> t
  val non_deterministic : LValue.t -> Dba.region -> int -> t
  val static_jump : ?tag:Tag.t option -> Jump_target.t -> t
  val static_inner_jump : ?tag:Tag.t option -> int -> t
  val call : return_address:Caddress.t -> Jump_target.t -> t
  val dynamic_jump : ?tag:Tag.t option -> Expr.t -> t

  val malloc : LValue.t -> Expr.t -> int -> t
  val free : Expr.t -> int -> t

  val iassert : Condition.t -> int -> t
  val assume : Condition.t -> int -> t
  val non_deterministic_assume : LValue.t list -> Condition.t -> int -> t
  val stop : Dba.state option -> t

  val print : Dba.printable list -> int -> t

  (** {7 Modificators} *)
  val set_successor : t -> int -> t
  val reset_successor : src_id:int -> dst_id:int -> t -> t

  (** {7 Properties and computations} *)
  val successors : t -> Dba.jump_target list
  
  val variables :
    t -> (Basic_types.String.Set.t, Basic_types.String.Set.t) defuse
  (** [variables t] returns a couple [defined * used] representing sets of
      variable names defined and used at instruction [t] *)
  
  val temporaries :
    t -> (Basic_types.String.Set.t, Basic_types.String.Set.t) defuse
  (** [temporaries t] returns a couple [defined * used] representing sets of
      temporaries being defined and used *)

  val outer_jumps : t -> Virtual_address.Set.t
  (** [outer_jumps t] returns the set of virtual addresses this instruction may
      jump to.

      This is a conservative, syntactic computation. 
      Whenever an instruction can jump to a virtual address, this corresponds to
      a jump outside a DBA block. This function is used in [Block.outer_jumps].
  *)
                           
  val is_call : t -> bool
  (** [is_call t] returns [true] if the instruction is a function call.
      
      A DBA function call is encoded a jump (static or dynamic) with a [Call]
      tag which stores the return address.
  *)
end


module Declarations : sig
  type t = (Dba.size * Dba.vartag option) Basic_types.String.Map.t
  (** A DBA declaration has a name, a size in bits and some optional tags *)

  val of_list : (string * Dba.size * Dba.vartag option) list -> t
end


module Statement : sig
  type t = private {
    location : Caddress.t;
    instruction : Instruction.t
  }


  include Printable with type t := t

  val create : Caddress.t -> Instruction.t -> t
  val location : t -> Caddress.t
  val instruction : t -> Instruction.t

  val set_instruction : t -> Instruction.t -> t
  val set_location : t -> Caddress.t -> t
end

(** {4 DBA blocks} *)

module Block : sig

  type t = private Dba.instruction array
  (** A DBA block represents a set of DBA instructions with explicit links to
      the next one. The first instruction of the block always has [id] 0.
      Tipically, DBA a block is the translation of one binary/assembly
      instruction.
  *)

  val empty : t
  val is_empty : t -> bool

  val init : int -> (int -> Dba.instruction) -> t
  val singleton : Dba.instruction -> t
  val length : t -> int
  val get : t -> int -> Dba.instruction
  val start : t -> int
  (** [start b] is the first index of block [b] *)

  val copy : t -> t
  val iter  : (Dba.instruction -> unit) -> t -> unit
  val iteri : (int -> Dba.instruction -> unit) -> t -> unit
  val map   : (Dba.instruction -> Dba.instruction) -> t -> t
  val mapi  : (int -> Dba.instruction -> Dba.instruction) -> t -> t
  val of_list : Dba.instruction list -> t
    (** [of_list l] assumes the list is sorted in increasing order inside the
        block *)

  val to_list : t -> Dba.instruction list
  val fold_left : ('a -> Dba.instruction -> 'a) -> 'a -> t -> 'a
  val for_all : (Dba.instruction -> bool) -> t -> bool

  val remove : t -> int -> t


  include Printable with type t:=t

  (** {7 Block properties}*)

  module Check : sig
    val has_inbound_inner_jumps : t -> bool
    val no_undeclared_variables : Declarations.t -> t -> bool
    val no_temporary_leak : t -> bool
    (** [no_temporary_leak b] checks the invariant that a block must always
        (re)define a temporary before using it. This guarantees that no
        assumption is made on the block sequences and that no "leaked"
        information from another block is used inside a block.

        @returns [true] if that is the case.
    *)

  end

  val to_dbainstrs : t -> Virtual_address.t -> Statement.t list

  val outer_jumps : t -> Virtual_address.Set.t
  (** [outer_jumps b] computes the set of jumps to external addresses in block
      [b].
      Due to dynamic jumps, this represents a syntactic under-approximation of
      the possible jumps from this block.
  *)

  val callees : t -> Virtual_address.Set.t
  (** [callees b] computes the set of addresses this block may call *)
end



val malloc_id : int ref

val get_endianness : unit -> Dba.endianness
val set_endianness : Dba.endianness -> unit

type 'a dbainstrmap = (Dba.instruction * 'a option) Caddress.Map.t

type read_perm = Read of bool
type write_perm = Write of bool
type exec_perm =  Exec of bool
type permissions = Dba.cond * (read_perm * write_perm * exec_perm)
                    
type 'a program = {
  start_address : Dba.address;
  declarations : Declarations.t;
  permissions: permissions list Region.Map.t * Dba.cond Rights.t;
  initializations : Dba.instruction list;
  instructions : 'a dbainstrmap;
}
