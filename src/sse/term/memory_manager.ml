(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
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

exception Non_mergeable = Types.Non_mergeable

open Sexpr

module type CONTEXT = sig
  include Ai.S

  val anchor : t -> Memory.t -> unit
  val anchored : t -> Memory.t -> bool
end

module Make (D : Domains.S) (C : CONTEXT with type v := D.t) : sig
  include module type of Memory

  val source : C.t -> addr:Expr.t -> len:int -> Loader_buf.t -> t -> t
  val write : C.t -> addr:Expr.t -> Expr.t -> Expr.endianness -> t -> t
  val read : C.t -> addr:Expr.t -> int -> Expr.endianness -> t -> Expr.t
  val merge : C.t -> Expr.t -> t -> t -> t
end = struct
  include Memory

  let rebase (addr : Expr.t) =
    match addr with
    | Cst bv -> (Expr.zeros (Bv.size_of bv), bv)
    | Binary { f = Plus; x; y = Cst bv; _ } -> (x, bv)
    | Binary { f = Minus; x; y = Cst bv; _ } -> (x, Bv.neg bv)
    | _ -> (addr, Bv.zeros (Expr.sizeof addr))

  let blit offset buf len over =
    let s = Bigarray.Array1.dim buf in
    if len <= s then
      let buf = Bigarray.Array1.sub buf 0 len in
      Store.store offset (Chunk.of_hunk buf) over
    else
      let buf' =
        Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.C_layout (len - s)
      in
      Bigarray.Array1.fill buf' 0;
      if s = 0 then Store.store offset (Chunk.of_hunk buf') over
      else
        Store.store (Bv.add_int offset s) (Chunk.of_hunk buf')
          (Store.store offset (Chunk.of_hunk buf) over)

  let fill addr len orig over =
    let addr, offset = rebase addr in
    layer addr (blit offset orig len Store.empty) over

  let source ctx ~addr ~len orig over =
    match over with
    | Root | Symbol _ -> fill addr len orig over
    | Layer _ when C.anchored ctx over -> fill addr len orig over
    | Layer { addr = addr'; store = store'; over = over'; _ } -> (
        match Expr.sub addr addr' with
        | Expr.Cst bv -> layer addr' (blit bv orig len store') over'
        | _ ->
            C.anchor ctx over;
            fill addr len orig over)

  let singleton addr value over =
    let addr, offset = rebase addr in
    layer addr (Store.singleton offset (Chunk.of_term value)) over

  let write env ~addr value (dir : Expr.endianness) over =
    let value =
      match dir with LittleEndian -> value | BigEndian -> bswap value
    in
    match over with
    | Root | Symbol _ -> singleton addr value over
    | Layer _ when C.anchored env over -> singleton addr value over
    | Layer { addr = addr'; store = store'; over = over'; _ } -> (
        match Expr.sub addr addr' with
        | Expr.Cst bv ->
            layer addr' (Store.store bv (Chunk.of_term value) store') over'
        | _ ->
            C.anchor env over;
            singleton addr value over)

  let rec read env ~addr bytes (dir : Expr.endianness) memory =
    match memory with
    | Root | Symbol _ -> Expr.load bytes dir addr memory
    | Layer { addr = addr'; store; over; _ } -> (
        match Expr.sub addr addr' with
        | Expr.Cst bv -> (
            let miss i s =
              Chunk.of_term
                (read env ~addr:(Expr.addz addr' i) s Expr.LittleEndian over)
            in
            let bytes = Chunk.to_term (Store.select miss bv bytes store) in
            match dir with LittleEndian -> bytes | BigEndian -> bswap bytes)
        | e -> (
            let d = C.eval env e and size = Expr.sizeof e in
            match D.project ~size d with
            | Point z -> (
                let miss i s =
                  Chunk.of_term
                    (read env ~addr:(Expr.addz addr' i) s Expr.LittleEndian over)
                in
                let bv = Bv.create z size in
                let bytes = Chunk.to_term (Store.select miss bv bytes store) in
                match dir with
                | LittleEndian -> bytes
                | BigEndian -> bswap bytes)
            (* | Seq { start; n } when dir = LittleEndian && Z.lt n (Z.of_int 1023)
             *   ->
             *     let size = Expr.sizeof e in
             *     let n = Z.to_int n + 1 in
             *     let miss i s =
             *       Chunk.of_term
             *         (Expr.load s LittleEndian (Expr.addz addr' i) over)
             *     in
             *     let base = Bitvector.create start size in
             *     let chunk = Chunk.to_term (Store.select miss base n store) in
             *     Expr.restrict
             *       ~hi:((8 * bytes) - 1)
             *       ~lo:0
             *       (Expr.shift_right chunk
             *          (Expr.uext (8 * n)
             *             (Expr.shift_left
             *                (Expr.sub e (Expr.constant base))
             *                (Expr.constant (Bitvector.of_int ~size 3))))) *)
            | Seq { start; n } ->
                let last = Z.add (Z.add start n) (Z.of_int (bytes - 2))
                and size = Expr.sizeof e in
                if
                  if Z.numbits last > size then
                    Store.is_empty_between start
                      (Z.extract Z.minus_one 0 size)
                      store
                    && Store.is_empty_between Z.zero (Z.extract last 0 size)
                         store
                  else Store.is_empty_between start last store
                then read env ~addr bytes dir over
                else (
                  if not (C.anchored env memory) then C.anchor env memory;
                  Expr.load bytes dir addr memory)
            | Top ->
                if not (C.anchored env memory) then C.anchor env memory;
                Expr.load bytes dir addr memory))

  let rec merge env c t t' =
    if t == t' then t
    else
      match (t, t') with
      | Layer { over; addr; store; _ }, t'
        when over == t' && not (C.anchored env t) ->
          let store =
            Store.map
              (fun offset chunk ->
                if not (Chunk.is_hunk chunk) then
                  let value = Chunk.to_term chunk in
                  let size = Expr.sizeof value in
                  Chunk.of_term
                    (Expr.ite c value
                       (read env ~addr:(Expr.addz addr offset) (size / 8)
                          LittleEndian over))
                else raise_notrace Non_mergeable)
              store
          in
          layer addr store over
      | t, Layer { over; _ } when t == over -> merge env (Expr.lognot c) t' t
      | ( Layer { over; addr; store; _ },
          Layer { over = over'; addr = addr'; store = store'; _ } )
        when Expr.is_equal addr addr' && over == over'
             && (not (C.anchored env t))
             && not (C.anchored env t') ->
          let store =
            Store.merge
              (fun offset o0 o1 ->
                match (o0, o1) with
                | Some c0, Some c1 ->
                    if Chunk.equal c0 c1 then o0
                    else
                      Some
                        (Chunk.of_term
                           (Expr.ite c (Chunk.to_term c0) (Chunk.to_term c1)))
                | Some c0, None ->
                    let value = Chunk.to_term c0 in
                    let size = Expr.sizeof value in
                    Some
                      (Chunk.of_term
                         (Expr.ite c value
                            (read env ~addr:(Expr.addz addr offset) (size / 8)
                               LittleEndian over)))
                | None, Some c1 ->
                    let value = Chunk.to_term c1 in
                    let size = Expr.sizeof value in
                    Some
                      (Chunk.of_term
                         (Expr.ite c
                            (read env ~addr:(Expr.addz addr offset) (size / 8)
                               LittleEndian over)
                            value))
                | None, None -> None)
              store store'
          in
          layer addr store over
      | (Root | Symbol _ | Layer _), (Root | Symbol _ | Layer _) ->
          raise_notrace Non_mergeable
end
