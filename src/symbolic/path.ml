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

exception Undefined = State.Undefined
exception Undeclared = State.Undeclared
exception Unknown = State.Unknown
exception Non_mergeable = State.Non_mergeable

type trilean = Basic_types.Ternary.t

type ('state, 'model) partition =
  | False
  | True
  | Falsish of 'state
  | Trueish of 'state
  | Split of 'state * 'model list

module type S = sig
  type t
  type state
  type value
  type model

  val id : t -> int
  val pc : t -> Virtual_address.t
  val symbolize : t -> Dba.Var.t -> unit
  val assign : t -> Dba.Var.t -> Dba.Expr.t -> unit
  val clobber : t -> Dba.Var.t -> unit

  val load :
    t ->
    Dba.Var.t ->
    string option ->
    addr:Dba.Expr.t ->
    Machine.endianness ->
    unit

  val store :
    t ->
    string option ->
    addr:Dba.Expr.t ->
    Dba.Expr.t ->
    Machine.endianness ->
    unit

  val memcpy :
    t -> string option -> addr:Dba.Expr.t -> int -> Loader_types.buffer -> unit

  val predicate : t -> value list
  val is_symbolic : t -> Dba.Expr.t -> bool
  val is_zero : t -> Dba.Expr.t -> trilean
  val assume : t -> Dba.Expr.t -> model option
  val check_sat_assuming : t -> ?retain:bool -> Dba.Expr.t -> model option
  val partition : t -> Dba.Expr.t -> (state, model) partition

  val enumerate :
    t ->
    ?retain:bool ->
    ?n:int ->
    ?accumulator:model Bitvector.Map.t ->
    ?assuming:Dba.Expr.t ->
    Dba.Expr.t ->
    model Bitvector.Map.t

  val check_model : t -> ?retain:bool -> model -> bool
  val eval : t -> Dba.Expr.t -> Bitvector.t
  val get_value : t -> Dba.Expr.t -> value
  val lookup : t -> Dba.Var.t -> value

  val read :
    t -> string option -> addr:Dba.Expr.t -> int -> Machine.endianness -> value

  val symbols : t -> value list Dba_types.Var.Map.t

  type 'a key

  val get : t -> 'a key -> 'a
  val set : t -> 'a key -> 'a -> unit

  module Value : State.VALUE with type t = value

  val assign_v : t -> Dba.Var.t -> value -> unit

  val load_v :
    t -> Dba.Var.t -> string option -> addr:value -> Machine.endianness -> unit

  val store_v :
    t -> string option -> addr:value -> value -> Machine.endianness -> unit

  val memcpy_v :
    t -> string option -> addr:value -> int -> Loader_types.buffer -> unit

  val is_symbolic_v : t -> value -> bool
  val is_zero_v : t -> value -> trilean
  val assume_v : t -> value -> model option
  val check_sat_assuming_v : t -> ?retain:bool -> value -> model option
  val partition_v : t -> value -> (state, model) partition

  val enumerate_v :
    t ->
    ?retain:bool ->
    ?n:int ->
    ?accumulator:model Bitvector.Map.t ->
    ?assuming:value ->
    value ->
    model Bitvector.Map.t

  val eval_v : t -> value -> Bitvector.t

  val read_v :
    t -> string option -> addr:value -> int -> Machine.endianness -> value

  module Model : State.MODEL with type t = model and type value := value

  module State :
    State.S
      with type t = state
       and type Value.t = value
       and type Model.t = model

  val set_pc : t -> Virtual_address.t -> unit
  val models : t -> model list
  val set_models : t -> model list -> unit
  val state : t -> state
  val set_state : t -> state -> unit
  val transform_state : t -> (state -> state) -> unit
end

module Make (Metrics : Metrics.S) (State : State.S) : sig
  include
    S
      with type state = State.t
       and type value = State.Value.t
       and type model = State.Model.t
       and module State = State

  val create : unit -> t
  val cookie : t -> State.Cookie.t
  val fork : t -> t
  val merge : t -> t -> t option

  val declare_field :
    ?copy:('a -> 'a) -> ?merge:('a -> 'a -> 'a option) -> 'a -> 'a key
end = struct
  type data
  type t = data array
  type 'a field = int
  type merge_handler = M : 'a field * ('a -> 'a -> 'a option) -> merge_handler
  type copy_handler = C : 'a field * ('a -> 'a) -> copy_handler

  let default_merge : type a. a -> a -> a option =
   fun x y -> if x == y then Some x else None

  module State = struct
    include State

    let check_sat : Cookie.t -> t -> Model.t option =
     fun cookie state ->
      Metrics.Solver.Timer.start ();
      match check_sat cookie state with
      | Some _ as result ->
          Metrics.Solver.Timer.stop ();
          Metrics.Solver.incr Sat;
          result
      | None ->
          Metrics.Solver.Timer.stop ();
          Metrics.Solver.incr Unsat;
          None
      | exception (Unknown as e) ->
          Metrics.Solver.Timer.stop ();
          Metrics.Solver.incr Unknown;
          raise e

    let enumerate :
        Cookie.t -> Value.t -> ?except:Bitvector.t list -> t -> Enumeration.t =
     fun cookie target ?except state ->
      Metrics.Solver.Timer.start ();
      let result = enumerate cookie target ?except state in
      Metrics.Solver.Timer.stop ();
      result

    module Enumeration = struct
      include Enumeration

      let next : t -> (Bitvector.t * Model.t) option =
       fun enum ->
        Metrics.Solver.Timer.start ();
        match next enum with
        | Some _ as result ->
            Metrics.Solver.Timer.stop ();
            Metrics.Solver.incr Sat;
            result
        | None ->
            Metrics.Solver.Timer.stop ();
            Metrics.Solver.incr Unsat;
            None
        | exception (Unknown as e) ->
            Metrics.Solver.Timer.stop ();
            Metrics.Solver.incr Unknown;
            raise e
    end
  end

  module Value = State.Value
  module Model = State.Model

  type state = State.t
  and value = Value.t
  and model = Model.t

  module Field = struct
    type 'a t =
      | Id : int t
      | Pc : Virtual_address.t t
      | Nid : State.Uid.t t
      | Symbols : Value.t list Dba_types.Var.Map.t t
      | State : State.t t
      | Cookie : State.Cookie.t t
      | Models : Model.t list t
      | Last : unit t

    external unsafe_of_int : int -> 'a t = "%identity"
    external to_int : 'a t -> int = "%identity"

    let default : type a. a t -> a = function
      | Id -> 0
      | Pc -> Virtual_address.zero
      | Nid -> State.Uid.zero
      | Symbols -> Dba_types.Var.Map.empty
      | State -> State.empty ()
      | Cookie -> State.Cookie.default ()
      | Models -> [ Model.empty () ]
      | Last -> assert false

    let merge : type a. a t -> a -> a -> a option = function
      | Id -> fun x y -> Some (min x y)
      | Pc -> default_merge
      | Nid -> fun x y -> Some (max x y)
      | Symbols -> default_merge
      | State -> (
          fun x y -> try Some (State.merge x y) with Non_mergeable -> None)
      | Cookie -> default_merge
      | Models -> fun x y -> Some (List.append x y)
      | Last -> assert false

    let merge t = M (to_int t, merge t)

    let iter f =
      for i = 0 to to_int Last - 1 do
        f (unsafe_of_int i)
      done

    let last = to_int Last
  end

  external get : t -> 'a Field.t -> 'a = "%obj_field"
  external set : t -> 'a Field.t -> 'a -> unit = "%obj_set_field"

  let id path = get path Id
  let pc path = get path Pc
  let set_pc path addr = set path Pc addr
  let symbols path = get path Symbols
  let state path = get path State
  let cookie path = get path Cookie
  let models path = get path Models
  let set_models path models = set path Models models
  let set_state path state = set path State state
  let transform_state path f = set_state path (f (state path))

  let symbolize : t -> Dba.Var.t -> unit =
   fun path var ->
    let nid = get path Nid in
    set path Nid (State.Uid.succ nid);
    let value = Value.var nid var.name var.size in
    let symbols = get path Symbols in
    let stream =
      try Dba_types.Var.Map.find var symbols with Not_found -> []
    in
    set path Symbols (Dba_types.Var.Map.add var (value :: stream) symbols);
    set_state path (State.assign var value (state path))

  let clobber : t -> Dba.Var.t -> unit =
   fun path var ->
    let nid = get path Nid in
    set path Nid (State.Uid.succ nid);
    let value = Value.var nid var.name var.size in
    set_state path (State.assign var value (state path))

  let rec lookup : t -> Dba.Var.t -> Value.t =
   fun path var ->
    try State.lookup var (state path)
    with Undefined _ ->
      symbolize path var;
      lookup path var

  let rec get_value : t -> Dba.Expr.t -> Value.t =
    let uop e (o : Dba.Unary_op.t) : Term.unary Term.operator =
      match o with
      | Not -> Not
      | UMinus -> Minus
      | Sext n -> Sext (n - Dba.Expr.size_of e)
      | Uext n -> Uext (n - Dba.Expr.size_of e)
      | Restrict interval -> Restrict interval
    in
    let bop (op : Dba.Binary_op.t) : Term.binary Term.operator =
      match op with
      | Plus -> Plus
      | Minus -> Minus
      | Mult -> Mul
      | DivU -> Udiv
      | DivS -> Sdiv
      | RemU -> Urem
      | RemS -> Srem
      | Eq -> Eq
      | Diff -> Diff
      | LeqU -> Ule
      | LtU -> Ult
      | GeqU -> Uge
      | GtU -> Ugt
      | LeqS -> Sle
      | LtS -> Slt
      | GeqS -> Sge
      | GtS -> Sgt
      | Xor -> Xor
      | And -> And
      | Or -> Or
      | Concat -> Concat
      | LShift -> Lsl
      | RShiftU -> Lsr
      | RShiftS -> Asr
      | LeftRotate -> Rol
      | RightRotate -> Ror
    in
    fun path e ->
      match e with
      | Cst bv | Var { info = Symbol (_, (lazy bv)); _ } -> Value.constant bv
      | Var var -> lookup path var
      | Load (len, dir, addr, array) ->
          load path len array (get_value path addr) dir
      | Unary (f, x) -> Value.unary (uop x f) (get_value path x)
      | Binary (f, x, y) ->
          Value.binary (bop f) (get_value path x) (get_value path y)
      | Ite (c, r, e) -> (
          let c = get_value path c in
          match Value.is_zero c with
          | True -> get_value path e
          | False -> get_value path r
          | Unknown -> Value.ite c (get_value path r) (get_value path e))

  and load :
      t -> int -> string option -> Value.t -> Machine.endianness -> Value.t =
   fun path len array addr dir ->
    try
      let value, state =
        match array with
        | None -> State.read ~addr len dir (state path)
        | Some array -> State.select array ~addr len dir (state path)
      in
      set_state path state;
      value
    with Undeclared array ->
      let state = State.declare ~array (Value.sizeof addr) (state path) in
      set_state path state;
      load path len array addr dir

  let read_v :
      t -> string option -> addr:value -> int -> Machine.endianness -> Value.t =
   fun path array ~addr len dir -> load path len array addr dir

  let read :
      t ->
      string option ->
      addr:Dba.Expr.t ->
      int ->
      Machine.endianness ->
      Value.t =
   fun path array ~addr len dir ->
    read_v path array ~addr:(get_value path addr) len dir

  let assign_v : t -> Dba.Var.t -> value -> unit =
   fun path var value -> set_state path (State.assign var value (state path))

  let assign : t -> Dba.Var.t -> Dba.Expr.t -> unit =
   fun path var expr -> assign_v path var (get_value path expr)

  let load_v :
      t ->
      Dba.Var.t ->
      string option ->
      addr:value ->
      Machine.endianness ->
      unit =
   fun path var array ~addr dir ->
    let value = load path (var.size lsr 3) array addr dir in
    set_state path (State.assign var value (state path))

  let load :
      t ->
      Dba.Var.t ->
      string option ->
      addr:Dba.Expr.t ->
      Machine.endianness ->
      unit =
   fun path var array ~addr dir ->
    load_v path var array ~addr:(get_value path addr) dir

  let store_v :
      t -> string option -> addr:value -> value -> Machine.endianness -> unit =
   fun path array ~addr value dir ->
    let state = state path in
    match array with
    | None ->
        set_state path
          (try State.write ~addr value dir state
           with Undeclared array ->
             State.write ~addr value dir
               (State.declare ~array (Value.sizeof addr) state))
    | Some name ->
        set_state path
          (try State.store name ~addr value dir state
           with Undeclared array ->
             State.store name ~addr value dir
               (State.declare ~array (Value.sizeof addr) state))

  let store :
      t ->
      string option ->
      addr:Dba.Expr.t ->
      Dba.Expr.t ->
      Machine.endianness ->
      unit =
   fun path array ~addr value dir ->
    store_v path array ~addr:(get_value path addr) (get_value path value) dir

  let memcpy_v :
      t -> string option -> addr:value -> int -> Loader_types.buffer -> unit =
   fun path array ~addr len content ->
    let state = state path in
    set_state path
      (try State.memcpy array ~addr len content state
       with Undeclared array ->
         State.memcpy array ~addr len content
           (State.declare ~array (Value.sizeof addr) state))

  let memcpy :
      t ->
      string option ->
      addr:Dba.Expr.t ->
      int ->
      Loader_types.buffer ->
      unit =
   fun path array ~addr len content ->
    memcpy_v path array ~addr:(get_value path addr) len content

  let predicate : t -> value list = fun path -> State.predicate (state path)

  let is_symbolic_v : t -> value -> bool =
   fun path value -> State.is_symbolic value (state path)

  let is_symbolic : t -> Dba.Expr.t -> bool =
   fun path e -> is_symbolic_v path (get_value path e)

  let is_zero_v : t -> value -> trilean =
   fun path value -> State.is_zero value (state path)

  let is_zero : t -> Dba.Expr.t -> trilean =
   fun path e -> is_zero_v path (get_value path e)

  let assume_v : t -> value -> Model.t option =
   fun path value ->
    match Value.is_zero value with
    | True -> None
    | False -> Some (List.hd (models path))
    | Unknown -> (
        Metrics.Preprocess.Timer.start ();
        match State.assume value (state path) with
        | None ->
            Metrics.Preprocess.Timer.stop ();
            Metrics.Preprocess.incr Unsat;
            None
        | Some state -> (
            let models =
              List.fold_left
                (fun models model ->
                  if Bitvector.is_zero (Model.eval value model) then models
                  else model :: models)
                [] (models path)
            in
            Metrics.Preprocess.Timer.stop ();
            match models with
            | model :: _ ->
                Metrics.Preprocess.incr Sat;
                set_state path state;
                set_models path models;
                Some model
            | [] -> (
                Metrics.Preprocess.incr Unknown;
                match State.check_sat (cookie path) state with
                | None -> None
                | Some model ->
                    set_state path state;
                    set_models path [ model ];
                    Some model)))

  let assume : t -> Dba.Expr.t -> Model.t option =
   fun path e -> assume_v path (get_value path e)

  let check_sat_assuming_v : t -> ?retain:bool -> value -> Model.t option =
   fun path ?(retain = true) value ->
    let models = models path in
    match Value.is_zero value with
    | True -> None
    | False -> Some (List.hd models)
    | Unknown -> (
        Metrics.Preprocess.Timer.start ();
        match
          List.find
            (fun model -> Bitvector.is_one (Model.eval value model))
            models
        with
        | model ->
            Metrics.Preprocess.Timer.stop ();
            Metrics.Preprocess.incr Sat;
            Some model
        | exception Not_found -> (
            match State.assume value (state path) with
            | None ->
                Metrics.Preprocess.Timer.stop ();
                Metrics.Preprocess.incr Unsat;
                None
            | Some state -> (
                Metrics.Preprocess.Timer.stop ();
                Metrics.Preprocess.incr Unknown;
                match State.check_sat (cookie path) state with
                | None -> None
                | Some model ->
                    if retain then set_models path (model :: models);
                    Some model)))

  let check_sat_assuming : t -> ?retain:bool -> Dba.Expr.t -> Model.t option =
   fun path ?retain e -> check_sat_assuming_v path ?retain (get_value path e)

  let partition_v : t -> value -> (state, model) partition =
   fun path value ->
    match Value.is_zero value with
    | True -> False
    | False -> True
    | Unknown -> (
        let state = state path and models = models path in
        Metrics.Preprocess.Timer.start ();
        match State.is_zero value state with
        | True ->
            Metrics.Preprocess.Timer.stop ();
            False
        | False ->
            Metrics.Preprocess.Timer.stop ();
            True
        | Unknown -> (
            match
              List.partition
                (fun model -> Bitvector.is_one (Model.eval value model))
                models
            with
            | [], _ -> (
                set_state path
                  (Option.get (State.assume (Value.unary Not value) state));
                Metrics.Preprocess.incr Sat;
                let true_state = State.assume value state in
                Metrics.Preprocess.Timer.stop ();
                match true_state with
                | None ->
                    Metrics.Preprocess.incr Unsat;
                    False
                | Some true_state -> Falsish true_state)
            | _, [] -> (
                set_state path (Option.get (State.assume value state));
                Metrics.Preprocess.incr Sat;
                let false_state = State.assume (Value.unary Not value) state in
                Metrics.Preprocess.Timer.stop ();
                match false_state with
                | None ->
                    Metrics.Preprocess.incr Unsat;
                    True
                | Some false_state -> Trueish false_state)
            | true_models, false_models ->
                set_state path (Option.get (State.assume value state));
                set_models path true_models;
                let false_state =
                  Option.get (State.assume (Value.unary Not value) state)
                in
                Metrics.Preprocess.Timer.stop ();
                Metrics.Preprocess.incr Sat;
                Metrics.Preprocess.incr Unsat;
                Split (false_state, false_models)))

  let partition : t -> Dba.Expr.t -> (state, model) partition =
   fun path test -> partition_v path (get_value path test)

  let check_model : t -> ?retain:bool -> Model.t -> bool =
   fun path ?(retain = true) model ->
    let r =
      List.for_all
        (fun e -> Bitvector.is_one (Model.eval e model))
        (State.predicate (state path))
    in
    if retain && r then set_models path (model :: models path);
    r

  let enumerate_v :
      t ->
      ?retain:bool ->
      ?n:int ->
      ?accumulator:Model.t Bitvector.Map.t ->
      ?assuming:value ->
      value ->
      Model.t Bitvector.Map.t =
    let decr : int option -> int option = function
      | None -> None
      | Some n -> Some (n - 1)
    in
    let rec fold_enumeration :
        t ->
        bool ->
        int option ->
        State.Enumeration.t ->
        Model.t Bitvector.Map.t ->
        Model.t Bitvector.Map.t =
     fun path retain n enum result ->
      match n with
      | Some 0 ->
          State.Enumeration.suspend enum;
          result
      | None | Some _ -> (
          match State.Enumeration.next enum with
          | None -> result
          | Some (bv, model) ->
              if retain then set_models path (model :: models path);
              fold_enumeration path retain (decr n) enum
                (Bitvector.Map.add bv model result))
    in
    let check_valid : Value.t option -> Model.t -> bool =
     fun assumtpion model ->
      match assumtpion with
      | None -> true
      | Some cond -> Bitvector.to_bool (Model.eval cond model)
    in
    let ensure_valid : Value.t option -> State.t -> State.t option =
     fun assumption state ->
      match assumption with
      | None -> Some state
      | Some cond -> State.assume cond state
    in
    let rec fold_models :
        t ->
        bool ->
        int option ->
        Value.t option ->
        Value.t ->
        Bitvector.t list ->
        Model.t Bitvector.Map.t ->
        Model.t list ->
        Model.t Bitvector.Map.t =
     fun path retain n assumption value except result models ->
      match n with
      | Some 0 ->
          Metrics.Preprocess.Timer.stop ();
          result
      | Some _ | None -> (
          match models with
          | [] -> (
              match ensure_valid assumption (state path) with
              | None ->
                  Metrics.Preprocess.Timer.stop ();
                  Metrics.Preprocess.incr Unsat;
                  result
              | Some state ->
                  Metrics.Preprocess.Timer.stop ();
                  Metrics.Preprocess.incr Unknown;
                  fold_enumeration path retain n
                    (State.enumerate (cookie path) value ~except state)
                    result)
          | model :: models ->
              if check_valid assumption model then
                let bv = Model.eval value model in
                if Bitvector.Map.mem bv result then
                  fold_models path retain n assumption value except result
                    models
                else (
                  Metrics.Preprocess.incr Sat;
                  fold_models path retain (decr n) assumption value
                    (bv :: except)
                    (Bitvector.Map.add bv model result)
                    models)
              else
                fold_models path retain n assumption value except result models)
    in
    fun path ?(retain = true) ?n ?(accumulator = Bitvector.Map.empty) ?assuming
        value ->
      if Value.is_symbolic value then (
        Metrics.Preprocess.Timer.start ();
        fold_models path retain n assuming value [] accumulator (models path))
      else (
        Metrics.Preprocess.incr Sat;
        let model = List.hd (models path) in
        if check_valid assuming model then
          let bv = Model.eval value model in
          Bitvector.Map.singleton bv model
        else Bitvector.Map.empty)

  let enumerate :
      t ->
      ?retain:bool ->
      ?n:int ->
      ?accumulator:Model.t Bitvector.Map.t ->
      ?assuming:Dba.Expr.t ->
      Dba.Expr.t ->
      Model.t Bitvector.Map.t =
   fun path ?retain ?n ?accumulator ?assuming e ->
    enumerate_v path ?retain ?n ?accumulator
      ?assuming:(Option.map (get_value path) assuming)
      (get_value path e)

  let eval_v : t -> value -> Bitvector.t =
   fun path value -> Model.eval value (List.hd (models path))

  let eval : t -> Dba.Expr.t -> Bitvector.t =
   fun path e -> eval_v path (get_value path e)

  external make : int -> int -> data array = "caml_obj_block"

  let sealed = ref false
  let size = ref Field.last
  let template : t ref = ref (make 0 (2 lsl Z.numbits (Z.of_int Field.last)))
  let merge_handlers = Queue.create ()
  let copy_handlers = Queue.create ()
  let n = ref 0

  let () =
    Queue.add
      (C
         ( 0,
           fun _ ->
             incr n;
             !n ))
      copy_handlers;
    Field.iter (fun f ->
        set !template f (Field.default f);
        Queue.push (Field.merge f) merge_handlers)

  external get : t -> 'a field -> 'a = "%obj_field"
  external set : t -> 'a field -> 'a -> unit = "%obj_set_field"

  let add_field default =
    if !sealed then raise (Invalid_argument "sealed");
    let capacity = Array.length !template and fid = !size in
    if fid >= capacity then (
      let template' = make 0 (2 * capacity) in
      Array.blit !template 0 template' 0 capacity;
      template := template');
    set !template fid default;
    incr size;
    fid

  type 'a key = 'a field

  let declare_field :
      ?copy:('a -> 'a) -> ?merge:('a -> 'a -> 'a option) -> 'a -> 'a key =
   fun ?copy ?(merge = default_merge) data ->
    let fid = add_field data in
    Queue.add (M (fid, merge)) merge_handlers;
    Option.iter (fun copy -> Queue.add (C (fid, copy)) copy_handlers) copy;
    fid

  let create : unit -> t =
   fun () ->
    sealed := true;
    Array.sub !template 0 !size

  let fork : t -> t =
   fun path ->
    let path' = Array.copy path in
    Queue.iter
      (fun (C (fid, copy)) -> set path' fid (copy (get path' fid)))
      copy_handlers;
    path'

  let merge : t -> t -> t option =
   fun path path' ->
    let path = Array.copy path in
    try
      Queue.iter
        (fun (M (fid, merge)) ->
          match merge (get path fid) (get path' fid) with
          | None -> raise_notrace Exit
          | Some data -> set path fid data)
        merge_handlers;
      Some path
    with Exit -> None
end
