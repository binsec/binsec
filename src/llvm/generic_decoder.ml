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

open Generic_decoder_sig;;

module Decode_Expr(I:Expr_Input) = struct

  open Dba

  open I

  let (>>=) = M.bind
  
  let rec expr:Dba.expr -> I.binary M.m = function
    | ExprCst(`Constant,bv) ->
      let size = Bitvector.size_of bv in
      I.Binary.biconst ~size (Bitvector.value_of bv)
    | ExprCst(_,_) -> assert false
    | ExprBinary(bop,e1,e2) ->
      let size = Dba_types.Expr.size_of e1 in
      expr e1 >>= fun v1 ->
      expr e2 >>= fun v2 ->
      (match bop with
       (* Binary operations. *)
       | Plus -> I.Binary.biadd ~size v1 v2
       | Minus -> I.Binary.bisub ~size v1 v2
       | MultU -> I.Binary.bimul ~size v1 v2
       | MultS -> I.Binary.bimul ~size v1 v2
       | DivU -> I.Binary.biudiv ~size v1 v2
       | DivS -> I.Binary.bisdiv ~size v1 v2
       | ModU -> I.Binary.biumod ~size v1 v2
       | ModS -> I.Binary.bismod ~size v1 v2
       | Or -> I.Binary.bor ~size v1 v2
       | And -> I.Binary.band ~size v1 v2
       | Xor -> I.Binary.bxor ~size v1 v2
       | Concat -> I.Binary.bconcat
                     ~size1:(Dba_types.Expr.size_of e1) v1
                     ~size2:(Dba_types.Expr.size_of e2) v2
       | LShift -> I.Binary.bshl ~size v1 v2
       | RShiftU -> I.Binary.blshr ~size v1 v2
       | RShiftS -> I.Binary.bashr ~size v1 v2
       | LeftRotate -> assert false (* extract + concat?  *)
       | RightRotate -> assert false (* extract + concat?  *)

       (* Predicates. *)
       | Eq -> I.Binary.beq ~size v1 v2 >>= fun bool -> I.bin_of_bool bool
       | Diff ->
         I.Binary.beq ~size v1 v2 >>= fun bool ->
         I.Boolean.not bool >>= fun nbool ->
         I.bin_of_bool nbool
       | LeqU -> I.Binary.biule ~size v1 v2 >>= fun bool -> I.bin_of_bool bool
       | GeqU -> I.Binary.biule ~size v2 v1 >>= fun bool -> I.bin_of_bool bool           
       | LeqS -> I.Binary.bisle ~size v1 v2 >>= fun bool -> I.bin_of_bool bool
       | GeqS -> I.Binary.bisle ~size v2 v1 >>= fun bool -> I.bin_of_bool bool           
       | LtU -> I.Binary.biult  ~size v1 v2 >>= fun bool -> I.bin_of_bool bool
       | GtU -> I.Binary.biult  ~size v2 v1 >>= fun bool -> I.bin_of_bool bool           
       | LtS -> I.Binary.bislt  ~size v1 v2 >>= fun bool -> I.bin_of_bool bool
       | GtS -> I.Binary.bislt  ~size v2 v1 >>= fun bool -> I.bin_of_bool bool)
    | ExprUnary(op,e1) as e ->
      let size = Dba_types.Expr.size_of e in
      expr e1 >>= fun v1 ->
      (match op with
       | UMinus ->
         I.Binary.biconst ~size Bigint.zero_big_int >>= fun vz ->
         I.Binary.bisub ~size vz v1
       | Not ->
         I.Binary.biconst ~size Bigint.(minus_big_int unit_big_int) >>= fun ffff ->
         I.Binary.bxor ~size ffff v1
      )
    | ExprRestrict(e,lo,hi) ->
      let oldsize = Dba_types.Expr.size_of e in
      expr e >>= fun v ->
      I.Binary.bextract ~size:(1 + hi - lo) ~oldsize ~index:lo v
    | ExprExtU(e,newsize) ->
      expr e >>= fun v -> I.Binary.buext ~size:newsize v
    | ExprExtS(e,newsize) ->
      expr e >>= fun v -> I.Binary.bsext ~size:newsize v      
    | ExprVar(var,size,_) -> I.get_var ~size var
    | ExprLoad(size,endianness,e) ->
      expr e >>= fun address ->
      I.load ~size:(size * 8) endianness address
    | ExprIte(c,e1,e2) ->
      cond c >>= fun vc ->
      expr e1 >>= fun v1 ->
      expr e2 >>= fun v2 ->
      I.select vc v1 v2
    | ExprAlternative _ -> (* TODO: Convert the value of boolean registers into booleans. *)
      assert false


  and cond:Dba.cond -> I.boolean M.m = function
    | CondReif(e) -> expr e >>= fun v -> I.bool_of_bin v
    | CondNot c -> cond c >>= fun v -> I.Boolean.not v
    | CondAnd(c1,c2) ->
      cond c1 >>= fun v1 ->
      cond c2 >>= fun v2 ->
      I.Boolean.(&&) v1 v2
    | CondOr(c1,c2) ->
      cond c1 >>= fun v1 ->
      cond c2 >>= fun v2 ->
      I.Boolean.(||) v1 v2
    | True -> I.Boolean.true_
    | False -> I.Boolean.false_                
  

end

module Decode_Instr(S:Instr_Input):sig
  val instruction: S.State.t -> Dba.instruction -> (S.boolean,S.binary) Generic_decoder_sig.jump_kind * S.State.t
end

= struct

  module EDecode = Decode_Expr(struct
      include S
      module M = State_Monad(S.State)
    end)

  open Dba

  let write_lhs state value = function
    | LhsVar(name,size,_) -> S.set_var ~size name value state
    | LhsVarRestrict(name,size,lo,hi) ->
      let value_size = size in
      let (old,state) = S.get_var ~size name state in
      let written_size = 1 + hi - lo in
      let (v,state) = 
        if lo == 0 then (value,state)
        else let _pold, state = S.Binary.bextract ~oldsize:value_size ~index:0 ~size:lo old state in
          S.Binary.bconcat ~size1:written_size ~size2:lo value old state in
      let (v,state) =
        if hi == (size - 1) then (v,state)
        else let (pold,state) =
               S.Binary.bextract ~oldsize:value_size ~index:hi ~size:(1 + size - hi) old state in
          S.Binary.bconcat ~size1:(size -  hi) ~size2:hi pold v state in
      S.set_var ~size name v state
    | LhsStore(size,endianness,address) ->
      let (vaddress,state) = EDecode.expr address state in
      S.store ~size:(size * 8) endianness vaddress value state


  
  let instruction state = let open Generic_decoder_sig in function
    | IkAssign(lhs,expr,id) ->
      let (v,state) = EDecode.expr expr state in
      let state = write_lhs state v lhs in
      (JKJump (Static (JInner id)),state)
    | IkSJump (target,_) ->
      (JKJump (Static target), state)
    | IkDJump(target,_) ->
      let (v,state) = EDecode.expr target state in
      (JKJump (Dynamic v), state)
    | IkIf(cond,target,id) ->
      let (v,state) = EDecode.cond cond state in
      (JKIf(v,Static target,Static (JInner id))),state
    | IkStop _ -> JKStop, state
    | IkAssume(cond,id) | IkAssert(cond,id) ->
      let (v,state) = EDecode.cond cond state in
      JKAssume(v,Static (JInner id)),state
    | IkNondet(lhs,_,id) ->
      let size = assert false in
      let (v,state) = S.unknown ~size state in
      let state = write_lhs state v lhs in
      (JKJump (Static (JInner id))),state
    | IkNondetAssume(l,cond,id) ->
      let size = assert false in
      let (acc,state) = List.fold_left (fun (acc,state) _lhs ->
          let (v,state) = S.unknown ~size state in
          v::acc,state) ([],state) l in
      let values = List.rev acc in
      let state = List.fold_left2 write_lhs state values l in
      let (cond,state) = EDecode.cond cond state in
      JKAssume (cond, Static (JInner id)),state
    | IkUndef(lhs,id) ->
      let size = assert false in
      let (v,state) = S.undef ~size state in
      let state = write_lhs state v lhs in
      (JKJump (Static (JInner id)),state)
    | IkMalloc _ -> assert false
    | IkFree _ -> assert false
    | IkPrint _ -> assert false

end
