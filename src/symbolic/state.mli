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

exception Unknown
(** Raised by reasoning functions ({!module-type-S.val-check_sat} and
    {!module-type-ENUMERATION.val-next}) when no solution has been found
    within the resource budget (e.g. {!constructor-SetSMTSolverTimeout}). *)

exception Undefined of Dba.Var.t
(** Raised by the {!module-type-DATA.val-lookup} function when the variable
    is not defined. *)

exception Undeclared of string option
(** Raised by the {!module-type-DATA.val-read}, {!module-type-DATA.val-select},
    {!module-type-S.val-write} and {!module-type-S.val-store} functions when
    the memory array is not defined. *)

exception Non_mergeable
(** Raised by the {!module-type-S.val-merge} function the two states can not
    be merged. *)

type trilean = Basic_types.Ternary.t
(** Extends the booleans with {!constructor-Unknown}. *)

type size = Term.size
(** A size in bit. *)

and 'a interval = 'a Term.interval
and unary = Term.unary
and binary = Term.binary

and 'a operator = 'a Term.operator =
  | Not : unary operator
  | Sext : size -> unary operator
  | Uext : size -> unary operator
  | Restrict : int interval -> unary operator
  | Plus : binary operator
  | Minus : _ operator
  | Mul : binary operator
  | Udiv : binary operator (* Corresponds to *)
  | Urem : binary operator (* the truncated division *)
  | Sdiv : binary operator (* of C99 and most *)
  | Srem : binary operator (* processors *)
  | Or : binary operator
  | And : binary operator
  | Xor : binary operator
  | Concat : binary operator
  | Lsl : binary operator
  | Lsr : binary operator
  | Asr : binary operator
  | Rol : binary operator
  | Ror : binary operator
  | Eq : binary operator
  | Diff : binary operator
  | Ule : binary operator
  | Ult : binary operator
  | Uge : binary operator
  | Ugt : binary operator
  | Sle : binary operator
  | Slt : binary operator
  | Sge : binary operator
  | Sgt : binary operator

module type UID = sig
  type t
  (** A unique identifier for variable naming (SSA form). *)

  val zero : t
  (** [zero] returns the first unique identifier. *)

  val succ : t -> t
  (** [succ i] returns the unique identifier following [i]. *)

  val compare : t -> t -> int
  (** [compare i1 i2] same behavior as {!module-Stdlib.val-compare}. *)
end

module type VALUE = sig
  type t
  (** A symbolic value. *)

  type id
  (** The unique identifier for variable naming (SSA form). *)

  (** {3 Creation} *)

  val constant : Bitvector.t -> t
  (** [constant v] creates a value that evaluates to the
      {!module-Binsec_kernel.module-Bitvector} [v]. *)

  val zero : t
  (** [zero] shorthand for {!val-constant} {!module-Binsec_kernel.module-Bitvector.val-zero}. *)

  val var : id -> string -> int -> t
  (** [var i n s] creates a (first-order) constant value of [s] bits,
      named [n] and uniquely identified by [i]. *)

  val unary : unary operator -> t -> t
  (** [unary o v] returns the application of the unary operator [o] applied
      to the value [v]. *)

  val binary : binary operator -> t -> t -> t
  (** [binary o v1 v2] returns the application of the binary operator [o]
      applied to the values [v1] and [v2]. *)

  val ite : t -> t -> t -> t
  (** [ite cond if_term else_term] returns the value that evaluates to
      [if_term] if [cond] is [true], or [else_term] otherwise. *)

  (** {3 Inspection} *)

  val is_symbolic : t -> bool
  (** [is_symbolic v] returns [true] if the [v] does not syntactically evaluate
      to constant {!module-Binsec_kernel.module-Bitvector.type-t}. *)

  val is_zero : t -> trilean
  (** [is_zero v] returns {!constructor-True} if [v] equals zero, 
      {!constructor-False} if [v] equals one and {!constructor-Unknown} otherwise. *)

  val sizeof : t -> int
  (** [sizeof v] returns the size in bits of the value [v]. *)
end

module type MODEL = sig
  type t
  (** A model is a mapping from symbolic values ({!type-value}) to their concrete 
      values assignment ({!module-Binsec_kernel.module-Bitvector.type-t}). *)

  type value
  (** A symbolic value. *)

  val empty : unit -> t
  (** [empty] returns an empty model. *)

  val eval : value -> t -> Bitvector.t
  (** [eval v m] evaluates the expression [v] in the model [m]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp f m] prints the model [m] in the formatter [f]. *)

  val pp_with_sections :
    (Virtual_address.t -> string option) -> Format.formatter -> t -> unit
  (** [pp_with_sections s f m] same as {!val-pp}, but may also display the section names from which memory accesses belong. *)
end

module type COOKIE = sig
  type t
  (** A cookie contains the configuration elements to be used to reason on the symbolic formula ({!module-type-S.val-predicate}).
      
      It is passed to the functions {!module-type-S.val-check_sat} and {!module-type-S.val-enumerate}.
      It can be configured via the extension implemented by the state (see {!type-feature} and {!module-type-S.val-more}).
      For instance, SMT solver based states can use {!constructor-SetSMTSolver} to select a different solver backend. *)

  val default : unit -> t
  (** [default] creates a cookie with default parameters. *)
end

module type ENUMERATION = sig
  type t
  (** The current state of the value enumeration.  *)

  type value
  (** The value returned by the enumeration. *)

  val next : t -> value option
  (** [next e] gives the following value possible for the enumeration [e]. If there is no remaining value, it returns [None].

       @raise Unknown when no solution is found within the current resource budget (see {!constructor-SetSMTSolverTimeout}).
  *)

  val suspend : t -> unit
  (** [suspend e] pauses the enumeration [e] and releases the external ressources (e.g. solver session).

      It can be used to release ressources as soon as the enumeration is no longer used or will not be used for a long time.
      Calling {!val-next} resumes a suspended enumeration where it was paused.
  *)
end

module type DATA = sig
  type t
  (** A mapping for variables ({!module-Binsec_kernel.module-Dba.module-Var.type-t} [->] {!type-value})
      and memory accesses ({!type-value} [->] {!type-value}). *)

  type value
  (** A symbolic value. *)

  val lookup : Dba.Var.t -> t -> value
  (** [lookup var s] returns the value assigned to [var] in [s].
  
      @raise Undefined if [var] is not in [s].
  *)

  val read : addr:value -> int -> Machine.endianness -> t -> value * t
  (** [read ~addr len d s] returns [len] bytes of the value stored at address [addr] in the main memory array {b \@},
      together with the updated state [s]. The byte order is determined by the endianness [d].
      
      @raise Undeclared [None] if there is no main memory in [s] (see {!module-type-S.val-declare}).
  *)

  val select :
    string -> addr:value -> int -> Machine.endianness -> t -> value * t
  (** [select m ~addr len d s] returns [len] bytes of the value stored at address [addr] in the memory array [m], together with
      the updated state [s]. The byte order is determined by the endianness [d].
  
      @raise Undeclared [Some m] if [m] is not in [s] (see {!module-type-S.val-declare}).
  *)
end

type ('value, 'state, 'cookie, 'a) feature = ..
(** A [feature] is a state functionality that is not part of the common interface.

    A value of type ['a] can be queried with the function {!module-type-S.val-more}.
    It can be anything, including a function, and may depend on the type parameters
    ['value], ['state] and ['cookie].
    
    For instance, the function {!module-type-S.val-lookup} can be expressed as a
    [!type-feature] as follows.
    [type ('value, 'state, 'cookie, 'a) feature +=
      | Lookup : ('value, 'state, 'cookie, Dba.Var.t -> 'state -> 'value) feature]
*)

module type S = sig
  type t
  (** A symbolic state. *)

  module Uid : UID
  module Value : VALUE with type id = Uid.t
  include DATA with type t := t and type value := Value.t

  val empty : unit -> t
  (** [empty] creates an empty state. *)

  val assign : Dba.Var.t -> Value.t -> t -> t
  (** [assign var v s] returns a copy of [s] with the value [v] assigned to the variable [var]. *)

  val declare : array:string option -> int -> t -> t
  (** [declare ~array idx s] returns a copy of [s] where [array] is a fresh mapping between addresses
      of [idx] bits to (first-order) constant.
      
      The [None] represents the main memory {b \@} (e.g. the RAM).
      The functions {!val-read} and {!val-write} operate on {b \@}.
      
      The [Some name] represents a named array.
      The functions {!val-select} and {!val-store} operate on [name].
  *)

  val write : addr:Value.t -> Value.t -> Machine.endianness -> t -> t
  (** [write ~addr v d s] returns a copy of [s] where the value [v] is written at the address [addr]
      in the main memory. The byte order is determined by the endianness [d].
  
      @raise Undeclared [None] if there is no main memory {b \@} in [s] (see {!val-declare}). 
  *)

  val store : string -> addr:Value.t -> Value.t -> Machine.endianness -> t -> t
  (** [store m ~addr v s] returns a copy of [s] where the value [v] is stored at the address [addr]
      in the memory array [m]. The byte order is determined by the endianness [d].

      @raise Undeclared [Some m] if [m] is not in [s] (see {!val-declare}). 
  *)

  val memcpy :
    string option -> addr:Value.t -> int -> Loader_types.buffer -> t -> t
  (** [memcpy m ~addr len content s] returns a copy of [s] where [len] bytes from the zero extended
      buffer [content] are copied to the memory array [m] at the address [addr].

      @raise Undeclared [m] is not in [s] (see {!val-declare}). 
  *)

  val merge : t -> t -> t
  (** [merge t1 t2] returns a new state with the values of both [t1] and [t2]. *)

  val assume : Value.t -> t -> t option
  (** [assume v s] returns the a copy of the state [s] for which the boolean condition [v] has been
      added to the path predicate.
    
      Returns [None] if the state [s] can infer that [v] always evaluates to [false].
  *)

  val predicate : t -> Value.t list
  (** [predicate s] returns the state predicate as a list of {!type-value}. *)

  val is_symbolic : Value.t -> t -> bool
  (** [is_symbolic v s] checks if the value [v] may depend on symbolic values. *)

  val is_zero : Value.t -> t -> trilean
  (** [is_zero v s] checks if [v] may depend on symbolic values.

      It returns {!constructor-True} when the state implies [v] is [false]
      and {!constructor-False} when the state implies [v] is [true].

      Otherwise, it returns [!constructor-Unknown], that means that [v] syntactically
      depends on a symbolic value. *)

  module Model : MODEL with type value := Value.t
  module Cookie : COOKIE

  val check_sat : Cookie.t -> t -> Model.t option
  (** [check_sat c s] returns a model that satisfies the predicate of [s], using the
      configuration stored in the cookie [c].
  
      @raise Unknown when no solution is found within the current resource budget
      (see {!constructor-SetSMTSolverTimeout}).
  *)

  module Enumeration : ENUMERATION with type value := Bitvector.t * Model.t

  val enumerate :
    Cookie.t -> Value.t -> ?except:Bitvector.t list -> t -> Enumeration.t
  (** [enumerate c v ~except s] returns a new enumeration for the value [v], using the
      configuration stored in the cookie [c].

      The enumeration will not contain any {!module-Binsec_kernel.module-Bitvector} present in [except].
  *)

  val print_smtlib :
    ?slice:(Value.t * string) list -> Format.formatter -> t -> unit
  (** [print_smtlib ~slice f s] outputs the predicate of [s] in the SMTlib format
      in the formatter [f].
      
      If [slice] is given, it outputs the current mapping between values and name.
      Otherwise, it outputs the full mapping (variables and arrays) of [s]. 
  *)

  val pp : Format.formatter -> t -> unit
  (** [pp f s] outputs the state [s] in the formatter [f]. *)

  val more : (Value.t, t, Cookie.t, 'a) feature -> 'a option
  (** [more feature]
      returns [Some] a value of type ['a]; or [None] if the current implementation
      does not support the queried feature.
  *)
end

type _ value_kind = ..
(** A witness of the value type used by the state ({!module-type-S.val-more} {!constructor-ValueKind}). *)

type ('value, 'state, 'cookie, 'a) feature +=
  | ValueKind : ('value, 'state, 'cookie, 'value value_kind) feature
  | SetSMTSolver :
      ( 'value,
        'state,
        'cookie,
        'cookie -> Smtlib.Solver.backend -> unit )
      feature
  | SetSMTSolverTimeout :
      ('value, 'state, 'cookie, 'cookie -> float -> unit) feature
  | SetSMTDumpDir : ('value, 'state, 'cookie, 'cookie -> string -> unit) feature
  | SetSMTSolverMultiChecks :
      ('value, 'state, 'cookie, 'cookie -> bool -> unit) feature
  | ToFormula :
      ('value, 'state, 'cookie, 'state -> Smtlib.Formula.formula) feature
