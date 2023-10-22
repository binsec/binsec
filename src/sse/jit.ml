open Cse

module Env (S : Types.STATE) = struct
  type _ key =
    | Int : int -> int key
    | String : string -> string key
    | Endian : Machine.endianness -> Machine.endianness key
    | Unop : Term.unary Term.operator -> Term.unary Term.operator key
    | Binop : Term.binary Term.operator -> Term.binary Term.operator key
    | Var : Dba.Var.t -> Dba.Var.t key
    | Expr : Expr.t -> S.Value.t key
    | Layer : Layer.t -> S.t key
    | Lookup : (Dba.Var.t -> S.t -> S.Value.t) key
    | Read
        : (S.Value.t -> int -> Machine.endianness -> S.t -> S.Value.t * S.t) key
    | Select
        : (string ->
          S.Value.t ->
          int ->
          Machine.endianness ->
          S.t ->
          S.Value.t * S.t)
          key
    | Unary : (Term.unary Term.operator -> S.Value.t -> S.Value.t) key
    | Binary
        : (Term.binary Term.operator -> S.Value.t -> S.Value.t -> S.Value.t) key
    | Ite : (S.Value.t -> S.Value.t -> S.Value.t -> S.Value.t) key
    | Assign : (Dba.Var.t -> S.Value.t -> S.t -> S.t) key
    | Write : (S.Value.t -> S.Value.t -> Machine.endianness -> S.t -> S.t) key
    | Store
        : (string -> S.Value.t -> S.Value.t -> Machine.endianness -> S.t -> S.t)
          key

  module Staged : sig
    type t

    val init : (S.t, Jitpsi.value) Jitpsi.t -> t

    val get : t -> 'a key -> ('a, Jitpsi.value) Jitpsi.t

    val assign : t -> Dba.Var.t -> Expr.t -> unit

    val write : t -> string -> Layer.t * bool -> unit

    val finalize : t -> (S.t, Jitpsi.value) Jitpsi.t

    val pair : t -> Expr.t -> (S.t * S.Value.t, Jitpsi.value) Jitpsi.t
  end = struct
    type value

    external to_value : ('a, Jitpsi.value) Jitpsi.t -> value = "%identity"

    external of_value : value -> ('a, Jitpsi.value) Jitpsi.t = "%identity"

    module Key = struct
      type t = Any : 'a key -> t [@@unboxed]

      let equal (Any t) (Any t') =
        match (t, t') with
        | Int i, Int i' -> i = i'
        | String s, String s' -> String.equal s s'
        | Endian e, Endian e' -> e = e'
        | Unop o, Unop o' -> Term.Op.equal o o'
        | Binop o, Binop o' -> Term.Op.equal o o'
        | Var v, Var v' -> Dba.Var.equal v v'
        | Expr e, Expr e' -> Expr.is_equal e e'
        | Layer l, Layer l' -> Layer.equal l l'
        | Lookup, Lookup
        | Read, Read
        | Select, Select
        | Unary, Unary
        | Binary, Binary
        | Ite, Ite
        | Assign, Assign
        | Write, Write
        | Store, Store ->
            true
        | ( ( Int _ | String _ | Endian _ | Unop _ | Binop _ | Var _ | Expr _
            | Layer _ | Lookup | Read | Select | Unary | Binary | Ite | Assign
            | Write | Store ),
            ( Int _ | String _ | Endian _ | Unop _ | Binop _ | Var _ | Expr _
            | Layer _ | Lookup | Read | Select | Unary | Binary | Ite | Assign
            | Write | Store ) ) ->
            false

      let hash (Any t) =
        match t with
        | Int i -> 11 + i
        | String s -> Hashtbl.hash s
        | Endian LittleEndian -> 9
        | Endian BigEndian -> 10
        | Unop o -> Term.Op.hash o
        | Binop o -> Term.Op.hash o
        | Var v -> Dba.Var.hash v
        | Expr e -> Expr.hash e
        | Layer l -> Layer.hash l
        | Lookup -> 0
        | Read -> 1
        | Select -> 2
        | Unary -> 3
        | Binary -> 4
        | Ite -> 5
        | Assign -> 6
        | Write -> 7
        | Store -> 8
    end

    module T = Hashtbl.Make (Key)

    type t = {
      mutable input : (S.t, Jitpsi.value) Jitpsi.t;
      mutable state : (S.t, Jitpsi.value) Jitpsi.t;
      mutable last : value;
      locals : value T.t;
    }

    let init state =
      { input = state; state; last = to_value state; locals = T.create 32 }

    let set :
        type a. t -> ?const:bool -> a key -> (a, Jitpsi.value) Jitpsi.t -> unit
        =
     fun t ?(const = true) k x ->
      T.add t.locals (Any k) (to_value x);
      if const then
        let last = of_value t.last in
        t.last <- to_value (Jitpsi.unsafe_sequence last x)
      else t.last <- to_value x

    let rec get : type a. t -> a key -> (a, Jitpsi.value) Jitpsi.t =
     fun t k ->
      match T.find t.locals (Any k) with
      | v -> of_value v
      | exception Not_found -> (
          match k with
          | Int i ->
              let i' = Jitpsi.const i in
              set t k i';
              i'
          | String s ->
              let s' = Jitpsi.const s in
              set t k s';
              s'
          | Endian e ->
              let e' = Jitpsi.const e in
              set t k e';
              e'
          | Unop o ->
              let o' = Jitpsi.const o in
              set t k o';
              o'
          | Binop o ->
              let o' = Jitpsi.const o in
              set t k o';
              o'
          | Var v ->
              let v' = Jitpsi.const v in
              set t k v';
              v'
          | Expr (Cst bv) ->
              let c' = Jitpsi.const (S.Value.constant bv) in
              set t k c';
              c'
          | Expr (Var { label = var; _ }) ->
              let lookup' = get t Lookup in
              let var' = get t (Var var) in
              let state' = t.input in
              let last' = of_value t.last in
              let e' = Jitpsi.unsafe_apply2 last' lookup' var' state' in
              set ~const:false t k e';
              e'
          | Expr (Load { len; dir; addr; label; _ }) ->
              let addr' = get t (Expr addr) in
              let len' = get t (Int len) in
              let dir' = get t (Endian dir) in
              ignore (get t (Layer label));
              let state' = t.state in
              let tuple =
                match Layer.base label with
                | None ->
                    let read' = get t Read in
                    let last' = of_value t.last in
                    Jitpsi.unsafe_apply4 last' read' addr' len' dir' state'
                | Some array ->
                    let select' = get t Select in
                    let array' = get t (String array) in
                    let last' = of_value t.last in
                    Jitpsi.unsafe_apply5 last' select' array' addr' len' dir'
                      state'
              in
              let state' = Jitpsi.snd tuple in
              let e' = Jitpsi.unsafe_fst state' tuple in
              t.state <- state';
              set ~const:false t k e';
              e'
          | Expr (Unary { f; x; _ }) ->
              let unary' = get t Unary in
              let f' = get t (Unop f) in
              let x' = get t (Expr x) in
              let last' = of_value t.last in
              let e' = Jitpsi.unsafe_apply2 last' unary' f' x' in
              set ~const:false t k e';
              e'
          | Expr (Binary { f; x; y; _ }) ->
              let binary' = get t Binary in
              let f' = get t (Binop f) in
              let x' = get t (Expr x) in
              let y' = get t (Expr y) in
              let last' = of_value t.last in
              let e' = Jitpsi.unsafe_apply3 last' binary' f' x' y' in
              set ~const:false t k e';
              e'
          | Expr (Ite { c; t = r; e = l; _ }) ->
              let ite' = get t Ite in
              let c' = get t (Expr c) in
              let r' = get t (Expr r) in
              let l' = get t (Expr l) in
              let last' = of_value t.last in
              let e' = Jitpsi.unsafe_apply3 last' ite' c' r' l' in
              set ~const:false t k e';
              e'
          | Layer (Base _) ->
              T.add t.locals (Any k) (to_value t.input);
              t.input
          | Layer (Layer { over; base; addr; store; _ }) ->
              (ignore (get t (Layer over));
               match base with
               | None ->
                   let write' = get t Write in
                   Store.iter
                     (fun offset value ->
                       let addr = Expr.addz addr offset in
                       let addr' = get t (Expr addr) in
                       let dir' = get t (Endian LittleEndian) in
                       let value' = get t (Expr value) in
                       let state' = t.state in
                       let last' = of_value t.last in
                       let state' =
                         Jitpsi.unsafe_apply4 last' write' addr' value' dir'
                           state'
                       in
                       t.state <- state';
                       t.last <- to_value state')
                     store
               | Some array ->
                   let store' = get t Store in
                   let array' = get t (String array) in
                   Store.iter
                     (fun offset value ->
                       let addr = Expr.addz addr offset in
                       let addr' = get t (Expr addr) in
                       let dir' = get t (Endian LittleEndian) in
                       let value' = get t (Expr value) in
                       let state' = t.state in
                       let last' = of_value t.last in
                       let state' =
                         Jitpsi.unsafe_apply5 last' store' array' addr' value'
                           dir' state'
                       in
                       t.state <- state';
                       t.last <- to_value state')
                     store);
              T.add t.locals (Any k) (to_value t.state);
              t.state
          | Lookup ->
              let f' = Jitpsi.const S.lookup in
              set t k f';
              f'
          | Read ->
              let f' =
                Jitpsi.const (fun addr len dir state ->
                    S.read ~addr len dir state)
              in
              set t k f';
              f'
          | Select ->
              let f' =
                Jitpsi.const (fun array addr len dir state ->
                    S.select array ~addr len dir state)
              in
              set t k f';
              f'
          | Unary ->
              let f' = Jitpsi.const S.Value.unary in
              set t k f';
              f'
          | Binary ->
              let f' = Jitpsi.const S.Value.binary in
              set t k f';
              f'
          | Ite ->
              let f' = Jitpsi.const S.Value.ite in
              set t k f';
              f'
          | Assign ->
              let f' = Jitpsi.const S.assign in
              set t k f';
              f'
          | Write ->
              let f' =
                Jitpsi.const (fun addr value dir state ->
                    S.write ~addr value dir state)
              in
              set t k f';
              f'
          | Store ->
              let f' =
                Jitpsi.const (fun array addr value dir state ->
                    S.store array ~addr value dir state)
              in
              set t k f';
              f')

    let assign t (var : Dba.Var.t) value =
      let assign' = get t Assign in
      let var' = get t (Var var) in
      let value' = get t (Expr value) in
      let state' = t.state in
      let last' = of_value t.last in
      let state' = Jitpsi.unsafe_apply3 last' assign' var' value' state' in
      t.last <- to_value state';
      t.state <- state'

    let write t _ (layer, _) = ignore (get t (Layer layer))

    let finalize { state; _ } =
      (* Jitpsi.sequence *)
      (*   (Jitpsi.apply *)
      (*      (Jitpsi.const (fun () -> Options.Logger.info "builtin")) *)
      (*      (Jitpsi.const ())) *)
      state

    let pair t e =
      let e = get t (Expr e) in
      Jitpsi.unsafe_pair (of_value t.last) t.state e
  end

  include Env

  let commit t =
    Jitpsi.return
      (Jitpsi.lambda (fun state ->
           let s = Staged.init state in
           List.iter
             (fun r -> ignore (Staged.get s (Expr r)))
             (List.rev t.rev_reads);
           VarMap.iter
             (fun var value ->
               match (value : Expr.t) with
               | Var { label = var'; _ } when Dba.Var.equal var var' -> ()
               | _ ->
                   (try
                      let input = VarMap.find var t.input_vars in
                      ignore (Staged.get s (Expr input))
                    with Not_found -> ());
                   Staged.assign s var value)
             t.vars;
           StrMap.iter (Staged.write s) t.layers;
           Staged.finalize s))

  let compute e t =
    Jitpsi.return
      (Jitpsi.lambda (fun state ->
           let s = Staged.init state in
           List.iter
             (fun r -> ignore (Staged.get s (Expr r)))
             (List.rev t.rev_reads);
           VarMap.iter
             (fun var value ->
               (try
                  let input = VarMap.find var t.input_vars in
                  ignore (Staged.get s (Expr input))
                with Not_found -> ());
               Staged.assign s var value)
             t.vars;
           StrMap.iter (Staged.write s) t.layers;
           Staged.pair s e))
end
