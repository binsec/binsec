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

type trilean = Basic_types.Ternary.t

(** Return type of the function {!module-type-S.val-partition}. *)
type ('state, 'model) partition =
  | False  (** The condition is [false]. *)
  | True  (** The condition is [true]. *)
  | Falsish of 'state
      (** The condition evaluates to [false] in the path models,
          but the given state may lead to [true]. *)
  | Trueish of 'state
      (** The condition evaluates to [true] in the path models,
          but the given state may lead to [false]. *)
  | Split of 'state * 'model list
      (** The condition evaluates to [true] in the path models,
          and evaluates to [false] in the given state and
          models. *)

module type S = sig
  type t
  (** A path. *)

  type state
  (** A symbolic state (see {!module-State.module-type-S}). *)

  type value
  (** A symbolic state (see {!module-State.module-type-VALUE}). *)

  type model
  (** A concrete assignment (see {!module-State.module-type-MODEL})*)

  val id : t -> int
  (** [id p] returns the unique identifier of the path [p]. *)

  val pc : t -> Virtual_address.t
  (** [addr p] returns the current program counter of the path [p]. *)

  (** {2 State update} *)

  val symbolize : t -> Dba.Var.t -> unit
  (** [symbolize p v] assigns the variable [v] with a new symbolic value.

      Each call to [symbolize] records the new value on top of the
      {!val-symbols} history. *)

  val assign : t -> Dba.Var.t -> Dba.Expr.t -> unit
  (** [assign p v e] assigns the variable [v] with the expression [e]. *)

  val clobber : t -> Dba.Var.t -> unit
  (** [assign p v e] assigns the variable [v] with a new symbolic value.
      
      Clobbered variables are expected to not be used by the program and are not recorded
      in the {!val-symbols} history. *)

  val load :
    t ->
    Dba.Var.t ->
    string option ->
    addr:Dba.Expr.t ->
    Machine.endianness ->
    unit
  (** [load p v m ~addr d] assigns the variable [v] with the bytes read at the
      address [addr] from the memory array [m].

      When [m] is [None], the load is performed from the main memory {b \@}.

      The bit size of [v] should be a multiple of the size of a byte (8).
      The byte order is determined by the endianness [d]. *)

  val store :
    t ->
    string option ->
    addr:Dba.Expr.t ->
    Dba.Expr.t ->
    Machine.endianness ->
    unit
  (** [store p m ~addr e d] writes the expression [e] at the address [addr]
      in the memory array [m].

      When [m] is [None], the write is performed in the main memory {b \@}.

      The bit size of [e] should be a multiple of the size of a byte (8).
      The byte order is determined by the endianness [d]. *)

  val memcpy :
    t -> string option -> addr:Dba.Expr.t -> int -> Loader_types.buffer -> unit
  (** [memcpy p m ~addr len content] initializes the memory array [m] at the
      address [addr] with [len] bytes from the zero extended buffer [content].

      When [m] is [None], the write is performed in the main memory {b \@}. *)

  (** {2 Predicate query} *)

  val predicate : t -> value list
  (** [predicate p] returns the path predicate as a list of boolean {!type-value}. *)

  val is_symbolic : t -> Dba.Expr.t -> bool
  (** [is_symbolic p e] checks if the expression [e] may depend on symbolic values.

      When it returns [false], the path predicate of [p] implies that [e] has a
      single value. This value can be obtained via {!eval}.

      Otherwise, it means that [e] syntactically depends on a symbolic value.
      Use {!enumerate} with [n = 2] to prove or disprove that
      it can take several values. *)

  val is_zero : t -> Dba.Expr.t -> trilean
  (** [is_zero p e] checks if the boolean expression [e] may depend on symbolic values.

      It returns {!constructor-True} when the path predicate implies [e] is [false]
      and {!constructor-False} when the path predicate implies [e] is [true].

      Otherwise, it returns {!constructor-Unknown}, that means that [e] syntactically
      depends on a symbolic value.
      Use {!enumerate} with [n = 2] to prove or disprove that
      it can take several values. *)

  val assume : t -> Dba.Expr.t -> model option
  (** [assume p e] tests if the condition [e] can be [true].
      If so, it returns a witness model ([Some]) and updates the symbolic predicate.
      Otherwise, nothing is updated and it returns [None].

      @raise State.Unknown when the symboloc engine fails to give an answer. *)

  val check_sat_assuming : t -> ?retain:bool -> Dba.Expr.t -> model option
  (** [check_sat_assuming p ~retain e] tests if the condition [e] can be [true].
      If so, it returns a witness model ([Some]), otherwise, it returns [None].

      If [retain] is [true] ({b default}), the model is saved into the path [p].

      @raise State.Unknown when the symbolic engine fails to give an answer. *)

  val partition : t -> Dba.Expr.t -> (state, model) partition
  (** [partition p e] tests if the condition [e] can be [true] and update its
      internal symbolic state accordingly.

      It returns {!constructor-True} when the path predicate implies [e] is [true]
      and {!constructor-False} when the path predicate implies [e] is [false].

      It returns {!constructor-Trueish} when the path predicate allows [e] to be
      [true] but may allow [false] too, and {!constructor-Falsish} when the path
      predicate allows [e] to be [false] but may allow [true] too.
      The returned {!type-state} can be used to check the other assumption.
      
      It returns {!constructor-Split} when the path predicate allows [e] to be
      both [true] and [false]. The current path keeps models for which [e]
      evaluates to [true] (same as {!val-assume}) while the returned {!type-state}
      comes with a list of models for which [e] evaluates to [false]. *)

  val enumerate :
    t ->
    ?retain:bool ->
    ?n:int ->
    ?accumulator:model Bitvector.Map.t ->
    ?assuming:Dba.Expr.t ->
    Dba.Expr.t ->
    model Bitvector.Map.t
  (** [enumerate p ~retain ~n ~accumulator ~assuming e]
      lists the possible values of the expression [e].

      It returns a map of up to [n] new possible values that are not already present
      in [accumulator], together with their respective model witnesses.
      When [n] is omitted, the enumeration is unbounded.

      The new models satisfy the predicate [assuming] when present.

      If [retain] is [true] ({b default}), new models are saved into the path [p].

      @raise State.Unknown when the symbolic engine fails to give an answer. *)

  val check_model : t -> ?retain:bool -> model -> bool
  (** [check_model p ~retain m] returns [true] if the model [m] is a witness
       for the path predicate of [p].

      If [retain] is [true] ({b default}), the model is saved into the path [p]
      when the function returns [true]. *)

  (** {2 Evaluation} *)

  val eval : t -> Dba.Expr.t -> Bitvector.t
  (** [eval p e] evaluates the expression [e] in the last witness model.

      The returned values are consistent with each other as long as the witness
      model is not updated via {!val-assume}, {!val-check_sat_assuming},
      {!val-enumerate}, {!val-check_model} or {!val-partition}. *)

  val get_value : t -> Dba.Expr.t -> value
  (** [get_value p e] evaluates the expression [e] to its symbolic value. *)

  val lookup : t -> Dba.Var.t -> value
  (** [lookup p v] evaluates the content of the variable [v] to its symbolic value. *)

  val read :
    t -> string option -> addr:Dba.Expr.t -> int -> Machine.endianness -> value
  (** [read p m ~addr n d]
      reads the content of the memory array [m] at the address [addr].

      When [m] is [None], the load is performed from the main memory.

      Returns a value of [n] bytes.
      The byte order is determined by the endianness [d]. *)

  val symbols : t -> value list Dba_types.Var.Map.t
  (** [symbols p] returns the (reverse) history of symbols created by
      {!val-symbolize}. *)

  (** {2 Custom storage} *)

  type 'a key
  (** A key entry associated to a value of type ['a]. *)

  val get : t -> 'a key -> 'a
  (** [get p k] returns the associated value of the key [k]. *)

  val set : t -> 'a key -> 'a -> unit
  (** [set p k v] replaces the associated value of the key [k] by [v]. *)

  (** {2 Second API (value)} *)

  module Value : State.VALUE with type t = value

  val assign_v : t -> Dba.Var.t -> value -> unit
  (** Same as {!val-assign} but with {!type-value} input. *)

  val load_v :
    t -> Dba.Var.t -> string option -> addr:value -> Machine.endianness -> unit
  (** Same as {!val-load} but with {!type-value} input. *)

  val store_v :
    t -> string option -> addr:value -> value -> Machine.endianness -> unit
  (** Same as {!val-store} but with {!type-value} input. *)

  val memcpy_v :
    t -> string option -> addr:value -> int -> Loader_types.buffer -> unit
  (** Same as {!val-memcpy} but with {!type-value} input. *)

  val is_symbolic_v : t -> value -> bool
  (** Same as {!val-is_symbolic} but with {!type-value} input. *)

  val is_zero_v : t -> value -> trilean
  (** Same as {!val-is_zero} but with {!type-value} input. *)

  val assume_v : t -> value -> model option
  (** Same as {!val-assume} but with {!type-value} input. *)

  val check_sat_assuming_v : t -> ?retain:bool -> value -> model option
  (** Same as {!val-check_sat_assuming_v} but with {!type-value} input. *)

  val partition_v : t -> value -> (state, model) partition
  (** Same as {!val-partition} but with {!type-value} input. *)

  val enumerate_v :
    t ->
    ?retain:bool ->
    ?n:int ->
    ?accumulator:model Bitvector.Map.t ->
    ?assuming:value ->
    value ->
    model Bitvector.Map.t
  (** Same as {!val-enumerate} but with {!type-value} input. *)

  val eval_v : t -> value -> Bitvector.t
  (** Same as {!val-eval} but with {!type-value} input. *)

  val read_v :
    t -> string option -> addr:value -> int -> Machine.endianness -> value
  (** Same as {!val-read} but with {!type-value} input. *)

  (** {2 Direct manipulation (unsafe)} *)

  module Model : State.MODEL with type t = model and type value := value

  module State :
    State.S
      with type t = state
       and type Value.t = value
       and type Model.t = model

  val set_pc : t -> Virtual_address.t -> unit
  (** [set_pc p v] replaces the current program counter of [p] by [v]. *)

  val models : t -> model list
  (** [models p] returns the list of the witness models of [p]. *)

  val set_models : t -> model list -> unit
  (** [set_models p l] replaces the witness models of [p] by the ones in [l].
  
      {b Warning.} It is the caller responsibility to ensure that [l] is
      non-empty and that each model in [l] is consistent with the current
      symbolic state {!val-state}. *)

  val state : t -> state
  (** [state p] returns the symbolic state of [p]. *)

  val set_state : t -> state -> unit
  (** [set_state p s] replaces the symbolic state of [p] by [s].
  
      {b Warning.} It is the caller responsibility to ensure that each
      model in {!models} is consistent with the new symbolic state [s].
  *)

  val transform_state : t -> (state -> state) -> unit
  (** [transform_state p f] is equivalent to [set_state p (f (state p))]. *)
end

module Make (_ : Metrics.S) (State : State.S) : sig
  (** Main implementation of the signature {!module-type-S}. *)
  include
    S
      with type state = State.t
       and type value = State.Value.t
       and type model = State.Model.t
       and module State = State

  val cookie : t -> State.Cookie.t
  (** [cookie p] returns the cookie of [p] (see {!module-State.module-type-COOKIE} for more details). *)

  (** {2 Path extension}
  
      A path is a mutable table which maps typed {!type-key}s to their values.
      Each instance of [Make] has its own set of keys. The current implementation
      requires that all keys be known (i.e. {!val-declare_field}) before a path
      is created ({!val-create}).
      A value can then be read with {!val-get} and written with {!val-set}.
  *)

  val declare_field :
    ?copy:('a -> 'a) -> ?merge:('a -> 'a -> 'a option) -> 'a -> 'a key
  (** [declare_field ~copy ~merge default] registers a new field with the
      initial value [default], then returns the typed {!type-key} to access it.

      When given, the [copy] function will be called to initialize this field in
      the new path during {!val-fork}.

      When given, the [merge] function will be called to initialize this field in
      the new path during {!val-merge}. Returning [None] prevents the paths to be
      merged.

      @raise Invalid_argument when this function is called after a call to {!val-create}.
  *)

  (** {2 Builder functions} *)

  val create : unit -> t
  (** [create ()] makes a path with all fields initialized with their default values.
  
      {b Warning.} It is an error to try adding more fields after calling this function.
  *)

  val fork : t -> t
  (** [fork p] returns a copy of [p] with a new identifier.

      Fields are copied using the [copy] functions given via {!val-declare_field}
      (default to {!module-Fun.val-id}).
  *)

  val merge : t -> t -> t option
  (** [merge p1 p2] tries to merge all the fields of [p1] and [p2] using the [merge]
      functions given via {!val-declare_field} (default works only when values are
      physically equal). Returns [None] if any field fails to merge. *)
end
