module rec Expr : (Term.S with type a := Dba.Var.t and type b := Layer.t) =
  Term.Make (Dba.Var) (Layer)

and Store : sig
  type t

  val empty : t

  val store : Bitvector.t -> Expr.t -> t -> t

  val select : (Z.t -> int -> Expr.t) -> Bitvector.t -> int -> t -> Expr.t

  val iter : (Z.t -> Expr.t -> unit) -> t -> unit

  val rev_iter : (Z.t -> Expr.t -> unit) -> t -> unit
end = struct
  module Chunk = struct
    type t = Expr.t

    let equal = Expr.is_equal

    let len t = Expr.sizeof t lsr 3

    let crop ~lo ~hi t = Expr.restrict ~lo:(lo lsl 3) ~hi:((hi lsl 3) + 7) t

    let concat = Expr.append
  end

  include Lmap.Make (Chunk)

  let store addr value t =
    let z = Bitvector.value_of addr and s = Chunk.len value in
    let u = Z.add z (Z.of_int s) in
    let n = Bitvector.size_of addr in
    if Z.numbits u > n && Z.popcount u > 1 then
      let o = Z.to_int (Z.extract u 0 n) in
      store z
        (Chunk.crop ~hi:(s - o - 1) ~lo:0 value)
        (store Z.zero (Chunk.crop ~hi:(s - 1) ~lo:(s - o) value) t)
    else store z value t

  let select f addr s t =
    let z = Bitvector.value_of addr in
    let u = Z.add z (Z.of_int s) in
    let n = Bitvector.size_of addr in
    if Z.numbits u > n && Z.popcount u > 1 then
      let o = Z.to_int (Z.extract u 0 n) in
      Chunk.concat (select f Z.zero o t) (select f z (s - o) t)
    else select f z s t
end

and Layer : sig
  type t =
    | Base of string option
    | Layer of {
        id : int;
        over : t;
        base : string option;
        addr : Expr.t;
        store : Store.t;
      }

  val base : t -> string option

  val write : read:bool -> addr:Expr.t -> Expr.t -> Expr.endianness -> t -> t

  val read : addr:Expr.t -> int -> Expr.endianness -> t -> Expr.t * bool

  include Sigs.HASHABLE with type t := t
end = struct
  type t =
    | Base of string option
    | Layer of {
        id : int;
        over : t;
        base : string option;
        addr : Expr.t;
        store : Store.t;
      }

  let id = ref 0

  let hash = function Base b -> Hashtbl.hash b | Layer { id; _ } -> id

  let compare t t' =
    match (t, t') with
    | Base b, Base b' -> compare b b'
    | Layer { id; _ }, Layer { id = id'; _ } -> id - id'
    | Base _, Layer _ -> -1
    | Layer _, Base _ -> 1

  let equal t t' = compare t t' = 0

  let bswap =
    let rec iter e i r =
      if i = 0 then r
      else
        iter e (i - 8) (Expr.append (Expr.restrict ~hi:(i - 1) ~lo:(i - 8) e) r)
    in
    fun e ->
      let size = Expr.sizeof e in
      assert (size land 0x7 = 0);
      iter e (size - 8) (Expr.restrict ~hi:(size - 1) ~lo:(size - 8) e)

  let rebase (addr : Expr.t) =
    match addr with
    | Cst bv -> (Expr.zeros (Bitvector.size_of bv), bv)
    | Binary { f = Plus; x; y = Cst bv; _ } -> (x, bv)
    | Binary { f = Minus; x; y = Cst bv; _ } -> (x, Bitvector.neg bv)
    | _ -> (addr, Bitvector.zeros (Expr.sizeof addr))

  let base = function Base base | Layer { base; _ } -> base

  let write addr value over =
    let addr, offset = rebase addr in
    incr id;
    Layer
      {
        id = !id;
        over;
        base = base over;
        addr;
        store = Store.store offset value Store.empty;
      }

  let write ~read ~addr value (dir : Expr.endianness) over =
    let value =
      match dir with LittleEndian -> value | BigEndian -> bswap value
    in
    match over with
    | Base _ -> write addr value over
    | Layer _ when read -> write addr value over
    | Layer { base = base'; addr = addr'; store = store'; over = over'; _ } -> (
        match Expr.sub addr addr' with
        | Expr.Cst bv ->
            let store = Store.store bv value store' in
            incr id;
            Layer { id = !id; over = over'; base = base'; addr = addr'; store }
        | _ -> write addr value over)

  let rec read ~addr bytes (dir : Expr.endianness) t =
    match t with
    | Base _ -> (Expr.load bytes dir addr t, true)
    | Layer { addr = addr'; store; over; _ } -> (
        match Expr.sub addr addr' with
        | Expr.Cst bv ->
            let miss i s =
              fst (read ~addr:(Expr.addz addr' i) s Expr.LittleEndian over)
            in
            let bytes = Store.select miss bv bytes store in
            let bytes =
              match dir with LittleEndian -> bytes | BigEndian -> bswap bytes
            in
            (bytes, false)
        | _ -> (Expr.load bytes dir addr t, true))
end

module StrMap = Basic_types.String.Map

module VarMap = Map.Make (struct
  type t = Dba.Var.t

  let compare (x : t) (y : t) = x.id - y.id
end)

let uop (e : Dba.Expr.t) (op : Dba.Unary_op.t) : Term.unary Term.operator =
  match op with
  | Not -> Not
  | UMinus -> Minus
  | Sext n -> Sext (n - Dba.Expr.size_of e)
  | Uext n -> Uext (n - Dba.Expr.size_of e)
  | Restrict interval -> Restrict interval

let bop (op : Dba.Binary_op.t) : Term.binary Term.operator =
  match op with
  | Plus -> Plus
  | Minus -> Minus
  | Mult -> Mul
  | DivU -> Udiv
  | DivS -> Sdiv
  | ModU -> Umod
  | ModS -> Smod
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

module Env = struct
  type t = {
    vars : Expr.t VarMap.t;
    layers : (Layer.t * bool) StrMap.t;
    rev_reads : Expr.t list;
    input_vars : Expr.t VarMap.t;
  }

  let is_empty { vars; layers; rev_reads; _ } =
    VarMap.is_empty vars && StrMap.is_empty layers && rev_reads = []

  let rec eval (e : Types.Expr.t) t =
    match e with
    | Cst bv | Var { info = Symbol (_, (lazy bv)); _ } -> (Expr.constant bv, t)
    | Var var -> lookup var t
    | Load (len, dir, addr, base) -> load len dir addr base t
    | Unary (f, x) ->
        let x', t' = eval x t in
        (Expr.unary (uop x f) x', t')
    | Binary (f, x, y) ->
        let x', t' = eval x t in
        let y', t' = eval y t' in
        (Expr.binary (bop f) x' y', t')
    | Ite (c, r, e) ->
        let c', t' = eval c t in
        let r', t' = eval r t' in
        let e', t' = eval e t' in
        (Expr.ite c' r' e', t')

  and lookup var t =
    try (VarMap.find var t.vars, t)
    with Not_found ->
      let input = Expr.var "" var.size var in
      ( input,
        {
          t with
          vars = VarMap.add var input t.vars;
          input_vars = VarMap.add var input t.input_vars;
        } )

  and load len dir addr base t =
    let name = Option.value ~default:"" base in
    let layer, read =
      try StrMap.find name t.layers with Not_found -> (Layer.Base base, true)
    in
    let addr, t' = eval addr t in
    let bytes, read' = Layer.read ~addr len dir layer in
    ( bytes,
      {
        t' with
        layers = StrMap.add name (layer, read || read') t'.layers;
        rev_reads = (if read' then bytes :: t'.rev_reads else t'.rev_reads);
      } )

  let empty =
    {
      vars = VarMap.empty;
      layers = StrMap.empty;
      rev_reads = [];
      input_vars = VarMap.empty;
    }

  let assign var value t =
    let value', t' = eval value t in
    { t' with vars = VarMap.add var value' t'.vars }

  let clobber var t =
    assign var (Dba.Expr.constant (Bitvector.zeros var.size)) t

  let forget var t = { t with vars = VarMap.remove var t.vars }

  let load (var : Dba.Var.t) base dir addr t =
    let bytes, t' =
      eval
        (Dba.Expr.load ?array:base (Size.Byte.create (var.size lsr 3)) dir addr)
        t
    in
    { t' with vars = VarMap.add var bytes t'.vars }

  let store base dir ~addr value t =
    let name = Option.value ~default:"" base in
    let layer, read =
      try StrMap.find name t.layers with Not_found -> (Layer.Base base, false)
    in
    let addr', t' = eval addr t in
    let value', t' = eval value t' in
    let layer' = Layer.write ~read ~addr:addr' value' dir layer in
    { t' with layers = StrMap.add name (layer', false) t'.layers }
end

type value = Bv of Expr.t | Ax of string option * Expr.t * Expr.t

module VarTbl = Dba_types.Var.Htbl
module VarSet = Dba_types.Var.Set

module BvTbl = Hashtbl.Make (struct
  type t = Expr.t

  let hash = Expr.hash

  let equal = Expr.is_equal
end)

module AxTbl = Hashtbl.Make (Layer)

type t = {
  mutable id : int;
  mutable rev : value list;
  locals : Dba.Var.t BvTbl.t;
  layers : unit AxTbl.t;
}

let init () =
  { id = 0; rev = []; locals = BvTbl.create 32; layers = AxTbl.create 4 }

let bv_once = Dba.Var.create ~tag:Dba.Var.Tag.Empty "" ~bitsize:Size.Bit.bits1

let rec visit_bv t bv =
  try
    if BvTbl.find t.locals bv == bv_once then (
      let name = Format.sprintf "$$$%d" t.id in
      t.id <- t.id + 1;
      BvTbl.replace t.locals bv
        (Dba.Var.create ~tag:Dba.Var.Tag.Temp name
           ~bitsize:(Size.Bit.create (Expr.sizeof bv))))
  with Not_found -> (
    match bv with
    | Var _ -> ()
    | Load { addr; label; _ } ->
        let name = Format.sprintf "$$$%d" t.id in
        t.id <- t.id + 1;
        BvTbl.add t.locals bv
          (Dba.Var.create ~tag:Dba.Var.Tag.Temp name
             ~bitsize:(Size.Bit.create (Expr.sizeof bv)));
        visit_ax t label;
        visit_bv t addr;
        t.rev <- Bv bv :: t.rev
    | Cst _ -> ()
    | Unary { x; _ } ->
        BvTbl.add t.locals bv bv_once;
        visit_bv t x;
        t.rev <- Bv bv :: t.rev
    | Binary { x; y; _ } ->
        BvTbl.add t.locals bv bv_once;
        visit_bv t x;
        visit_bv t y;
        t.rev <- Bv bv :: t.rev
    | Ite { c; t = r; e; _ } ->
        BvTbl.add t.locals bv bv_once;
        visit_bv t c;
        visit_bv t r;
        visit_bv t e;
        t.rev <- Bv bv :: t.rev)

and visit_ax t ax =
  if not (AxTbl.mem t.layers ax) then (
    AxTbl.add t.layers ax ();
    match ax with
    | Base _ -> ()
    | Layer { base; addr; store; over; _ } ->
        visit_ax t over;
        Store.rev_iter
          (fun offset value ->
            let addr = Expr.addz addr offset in
            visit_bv t addr;
            visit_bv t value;
            t.rev <- Ax (base, addr, value) :: t.rev)
          store)

let mk_unop (op : Term.unary Term.operator) x : Dba.Unary_op.t =
  match op with
  | Not -> Not
  | Minus -> UMinus
  | Uext n -> Uext (Expr.sizeof x + n)
  | Sext n -> Sext (Expr.sizeof x + n)
  | Restrict i -> Restrict i

let mk_binop (op : Term.binary Term.operator) : Dba.Binary_op.t =
  match op with
  | Plus -> Plus
  | Minus -> Minus
  | Mul -> Mult
  | Udiv -> DivU
  | Sdiv -> DivS
  | Umod -> ModU
  | Smod -> ModS
  | Eq -> Eq
  | Diff -> Diff
  | Ule -> LeqU
  | Ult -> LtU
  | Uge -> GeqU
  | Ugt -> GtU
  | Sle -> LeqS
  | Slt -> LtS
  | Sge -> GeqS
  | Sgt -> GtS
  | Xor -> Xor
  | And -> And
  | Or -> Or
  | Concat -> Concat
  | Lsl -> LShift
  | Lsr -> RShiftU
  | Asr -> RShiftS
  | Rol -> LeftRotate
  | Ror -> RightRotate

let rec mk_bv t bv =
  match BvTbl.find t.locals bv with
  | var -> if var == bv_once then mk_bv_no_cons t bv else Dba.Expr.v var
  | exception Not_found -> mk_bv_no_cons t bv

and mk_bv_no_cons t bv =
  match bv with
  | Var { label = var; _ } -> Dba.Expr.v var
  | Load _ -> assert false
  | Cst bv -> Dba.Expr.constant bv
  | Unary { f; x; _ } -> Dba.Expr.unary (mk_unop f x) (mk_bv t x)
  | Binary { f; x; y; _ } ->
      Dba.Expr.binary (mk_binop f) (mk_bv t x) (mk_bv t y)
  | Ite { c; t = r; e; _ } -> Dba.Expr.ite (mk_bv t c) (mk_bv t r) (mk_bv t e)

exception Skip

let rec visit_bv' bindings rev_bindings t (bv : Expr.t) =
  match bv with
  | Var { label = var; _ } -> (
      try
        let bv = VarTbl.find bindings var in
        BvTbl.remove rev_bindings bv;
        if BvTbl.length rev_bindings = 0 then raise_notrace Skip
      with Not_found -> ())
  | Unary { x; _ } when BvTbl.find t.locals bv == bv_once ->
      visit_bv' bindings rev_bindings t x
  | Binary { x; y; _ } when BvTbl.find t.locals bv == bv_once ->
      visit_bv' bindings rev_bindings t x;
      visit_bv' bindings rev_bindings t y
  | Ite { c; t = r; e; _ } when BvTbl.find t.locals bv == bv_once ->
      visit_bv' bindings rev_bindings t c;
      visit_bv' bindings rev_bindings t r;
      visit_bv' bindings rev_bindings t e
  | Cst _ | Load _ | Unary _ | Binary _ | Ite _ -> ()

let commit (body : Env.t) =
  if Env.is_empty body then []
  else
    let t = init () in
    List.iter (visit_bv t) (List.rev body.rev_reads);
    StrMap.iter (fun _ (ax, _) -> visit_ax t ax) body.layers;
    let bindings = VarTbl.create 32
    and inputs = VarTbl.create 32
    and rev_bindings = BvTbl.create 32 in
    VarMap.iter
      (fun var bv ->
        match (bv : Expr.t) with
        | Var { label = var'; _ } when Dba.Var.equal var var' -> ()
        | _ ->
            VarTbl.add bindings var bv;
            if VarMap.mem var body.input_vars then (
              let bv = Expr.var "" var.size var in
              let name = Format.sprintf "$$$%d" t.id in
              t.id <- t.id + 1;
              let tmp =
                Dba.Var.create ~tag:Dba.Var.Tag.Temp name
                  ~bitsize:(Size.Bit.create var.size)
              in
              BvTbl.add t.locals bv tmp;
              VarTbl.add inputs var tmp);
            if not (BvTbl.mem rev_bindings bv) then
              BvTbl.add rev_bindings bv var;
            visit_bv t bv)
      body.vars;
    (try
       List.iter
         (function
           | Bv bv -> (
               try
                 let var = BvTbl.find rev_bindings bv in
                 BvTbl.replace t.locals bv var;
                 VarTbl.remove bindings var;
                 BvTbl.remove rev_bindings bv;
                 if BvTbl.length rev_bindings = 0 then raise_notrace Skip
               with Not_found -> visit_bv' bindings rev_bindings t bv)
           | Ax (_, addr, value) ->
               visit_bv' bindings rev_bindings t addr;
               visit_bv' bindings rev_bindings t value)
         t.rev
     with Skip -> ());
    VarTbl.fold
      (fun var tmp assigns ->
        Ir.Assign { var = tmp; rval = Dba.Expr.v var } :: assigns)
      inputs
      (List.fold_left
         (fun assigns -> function
           | Bv (Load { dir; addr; label; _ } as bv) ->
               Ir.Load
                 {
                   var = BvTbl.find t.locals bv;
                   base = Layer.base label;
                   dir;
                   addr = mk_bv t addr;
                 }
               :: assigns
           | Bv bv ->
               let var = BvTbl.find t.locals bv in
               if var == bv_once then assigns
               else Ir.Assign { var; rval = mk_bv_no_cons t bv } :: assigns
           | Ax (base, addr, value) ->
               Ir.Store
                 {
                   base;
                   dir = LittleEndian;
                   addr = mk_bv t addr;
                   rval = mk_bv t value;
                 }
               :: assigns)
         (VarTbl.fold
            (fun var bv assigns ->
              Ir.Assign { var; rval = mk_bv t bv } :: assigns)
            bindings [])
         t.rev)
