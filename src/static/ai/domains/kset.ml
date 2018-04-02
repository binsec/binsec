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

open Errors

exception Elements_of_top

module K_set = Set.Make (
  struct
    type t = Region_bitvector.t
    let compare param1 param2 =
      match param1, param2 with
      |`Value (r1, b1), `Value (r2, b2) ->
        let size1 = Bitvector.size_of b1 in
        let b1 = Bitvector.value_of b1 in
        let size2 = Bitvector.size_of b2 in
        let b2 = Bitvector.value_of b2 in
        let modulo b n =
          let max = Bigint.power_int_positive_int 2 n in
          Bigint.mod_big_int b max
        in
        let bb1 = modulo b1 size1 in
        let bb2 = modulo b2 size2 in
        let c = Bigint.compare_big_int bb1 bb2 in
        if c = 0 && (Region_bitvector.region_equal r1 r2) then 0
        else if c < 0 then -1
        else 1
      | `Symb s1, `Symb s2 -> Region_bitvector.SubSymb.compare s1 s2
      | `SymbSmt s1, `SymbSmt s2 ->
        (* FIXME: Why not use Pervasives here ? *)
        if Smt_bitvectors.is_equal_smtBvExpr s1 s2 then 0 else 1
      | `Undef sz1, `Undef sz2 -> compare sz1 sz2
      | `Value _, _ -> 1
      | _, `Value _ -> -1
      | `Symb _, `SymbSmt _ -> 1
      | `SymbSmt _, `Symb _ -> -1
      | `Undef _, _ -> -1
      | _, `Undef _ -> 1
  end
  )

type proper_kset = (int * K_set.t)

type tag = NoTag  (* Lbound of Region_bitvector.t | Hbound of Region_bitvector.t *)

type  t =
  | Proper_kset of proper_kset
  | Top of tag

let universe = Top NoTag

let of_bounds _ = Top NoTag

let elements kset =
  match kset with
  | Proper_kset (_k, s) -> K_set.elements s
  | Top _ -> raise Elements_of_top


let pp ppf kset =
  let open Format in
  match kset with
  | Top _ -> fprintf ppf "T"
  | Proper_kset (_, set) ->
    fprintf ppf "{@[<hov 0>%a@]}"
      (fun ppf rbvs ->
         K_set.iter (fun rbv -> fprintf ppf "%a;@ " Region_bitvector.pp rbv) rbvs
      ) set


let to_string kset =
  Format.(fprintf str_formatter "%a" pp kset; flush_str_formatter ())


let empty = Proper_kset (Ai_options.KSetSize.get (), K_set.empty)


let _is_empty kset =
  match kset with
    Proper_kset (_, s) -> K_set.is_empty s
  | _ -> false


let _insert elem kset =
  match kset with
  | Proper_kset (k, set) -> Proper_kset (k, K_set.add elem set)
  | Top tag -> Top tag

let is_empty = function
  | Proper_kset (_, s) -> K_set.is_empty s
  | Top _ -> false

let _is_homogeneous ks =
  let s =
    match ks with
    | Proper_kset (_, s) -> s
    | Top _ -> failwith "Kset.ml : is_homogeneous of Top?"
  in
  let param1 = K_set.choose s in
  let is_same_region param2 =
    match param1, param2 with
    | `Value (r1, _), `Value (r2, _) ->
      Region_bitvector.region_equal r1 r2
    | _, _ -> false
  in
  let cond = K_set.for_all is_same_region s in
  if cond = true then Some (Region_bitvector.region_of param1)
  else None


let create =
  let limit = Ai_options.KSetSize.get () in
  fun s ->
    if K_set.cardinal s > limit then Top NoTag
    else Proper_kset (limit, s)

let singleton i =
  Proper_kset (Ai_options.KSetSize.get (), K_set.singleton i)

let _nondet = Top NoTag

let _mem l ks =
  match ks with
  | Proper_kset (_, s) -> K_set.mem l s
  | _ -> false

let contains ks1 ks2 =
  match ks1,ks2 with
  | Proper_kset (_, s1) , Proper_kset (_, s2) -> K_set.subset s2 s1
  | Proper_kset _, Top _ -> false
  | Top _, Proper_kset _ -> true
  | Top _, Top _ ->  true

let equal ks1 ks2 =
  (contains ks1 ks2) && (contains ks2 ks1)

let concat ks1 ks2 =
  match ks1, ks2 with
  | Proper_kset (_k1, s1), Proper_kset (_k2, s2) ->
    let s =
      K_set.fold (fun elem1 acc1 ->
          K_set.fold (fun elem2 acc2 ->
              try
                K_set.add (Region_bitvector.append elem1 elem2) acc2
              with
              | Errors.Bad_concat _ ->
                Logger.warning "KSet: Bad concat";
                acc2
            ) s2 acc1
        ) s1 K_set.empty
    in
    Proper_kset (Ai_options.KSetSize.get (), s)
  | _, _ -> Top NoTag



let  max ks =
  match ks with
    Proper_kset (_, s) -> K_set.max_elt s
  | _ -> failwith "kset.ml: max of non proper kset"

let _min ks =
  match ks with
    Proper_kset (_, s) -> K_set.min_elt s
  | _ -> failwith "kset.ml: max of non proper kset"



let join ks1 ks2 =
  match ks1,ks2 with
  | Proper_kset (_,s1) , Proper_kset (_,s2) ->
    if K_set.cardinal s1 > Ai_options.KSetSize.get () ||
       K_set.cardinal s2 > Ai_options.KSetSize.get () then
      Top NoTag
    else
      let s = K_set.union s1 s2 in
      if K_set.cardinal s > Ai_options.KSetSize.get () then Top NoTag
      else Proper_kset (Ai_options.KSetSize.get (), s)
  | _, _ -> Top NoTag


let widen ks1 ks2 _thresholds =
  match ks1, ks2 with
  | Proper_kset (_,s1) , Proper_kset (_,s2) ->
    if K_set.cardinal s1 > Ai_options.KSetSize.get () ||
       K_set.cardinal s2 > Ai_options.KSetSize.get ()
    then Top NoTag
    else
      let s = K_set.union s1 s2 in
      if (K_set.cardinal s) > Ai_options.KSetSize.get () then Top NoTag
      else if ((K_set.cardinal s) > (K_set.cardinal s1))
      then Top NoTag
      else Proper_kset (Ai_options.KSetSize.get (), s)
  | _, _ -> Top NoTag


let meet ks1 ks2 =
  match ks1,ks2 with
  | Proper_kset (_,s1) , Proper_kset (_,s2) ->
    let s = K_set.inter s1 s2 in
    if K_set.cardinal s > Ai_options.KSetSize.get () then Top NoTag
    else Proper_kset (Ai_options.KSetSize.get (), s)
  | Proper_kset (_, s), Top _
  | Top _, Proper_kset (_, s) ->
    if K_set.cardinal s > Ai_options.KSetSize.get () then Top NoTag
    else Proper_kset (Ai_options.KSetSize.get (), s)
  | Top _, Top _ -> Top NoTag

let neg ks =
  match ks with
  | Proper_kset (k, s) ->
    let s' = K_set.fold
        (fun elem acc ->
           K_set.add (Region_bitvector.neg elem) acc)
        s K_set.empty in
    Proper_kset (k, s')
  | Top t -> Top t

let lognot ks =
  match ks with
  | Proper_kset (k, s) ->
    let s' = K_set.fold
        (fun elem acc ->
           K_set.add (Region_bitvector.lognot elem) acc)
        s K_set.empty in
    Proper_kset (k, s')
  | Top t -> Top t


let addc ks c =
  match ks with
  | Proper_kset (_ , s) ->
    let f elem acc = K_set.add (Region_bitvector.add elem c) acc in
    let s' = K_set.fold f s K_set.empty in
    create s'
  | Top t -> Top t


let apply f ks1 ks2 =
  match ks1, ks2 with
  | Proper_kset (_, s1), Proper_kset (_, s2) ->
    let s' =
      K_set.fold (fun elem1 acc1 ->
          K_set.fold (fun elem2 acc2 ->
              K_set.add (f elem1 elem2) acc2)
            s2 acc1)
        s1 K_set.empty in
    create s'
  | _ -> Top  NoTag (* TODO: check region *)


let add = apply Region_bitvector.add

let _subc ks c = addc ks (Region_bitvector.neg c)

let _csub c ks = addc (neg ks) c

let sub ks1 ks2 = add ks1 (neg ks2)

let umul = apply Region_bitvector.umul

let smul = apply Region_bitvector.smul

let power = apply Region_bitvector.pow

let udiv = apply Region_bitvector.udiv

let sdiv ks1 ks2 =
  match ks1, ks2 with
  | Proper_kset (_, s1), Proper_kset (_, s2) ->
    let s' =
      K_set.fold (fun elem1 acc1 ->
          K_set.fold (fun elem2 acc2 ->
              K_set.add (Region_bitvector.sdiv elem1 elem2) acc2)
            s2 acc1)
        s1 K_set.empty in
    create s'
  | _, Proper_kset (_, s2) ->
    let s = K_set.filter Region_bitvector.is_zero s2 in
    if K_set.cardinal s > 0 then raise Errors.Div_by_zero
    else Top NoTag
  | _ -> Top NoTag (* TODO: check region *)

let restrict ks of1 of2 =
  match ks with
  | Proper_kset (_, s) ->
    let s' =
      K_set.fold (fun elem1 acc1 ->
          K_set.add (Region_bitvector.restrict elem1 of1 of2) acc1)
        s K_set.empty in
    create s'
  | _ -> Top  NoTag (* TODO: check region *)

let umod = apply Region_bitvector.umod

let smod = apply Region_bitvector.smod

let logor = apply Region_bitvector.logor

let logxor = apply Region_bitvector.logxor

let logand = apply Region_bitvector.logand

let lshift = apply Region_bitvector.lshift

let rshiftU = apply Region_bitvector.rshiftU

let rshiftS = apply Region_bitvector.rshiftS

let rotate_left = apply Region_bitvector.rotate_left

let rotate_right = apply Region_bitvector.rotate_right

let extension ks1 l =
  match ks1 with
  | Proper_kset (_, s1) ->
    let s' =
      K_set.fold (fun elem1 acc1 ->
          K_set.add (Region_bitvector.extension elem1 l) acc1)
        s1 K_set.empty in
    create s'
  | _ -> Top  NoTag (* TODO: check region *)

let signed_extension ks1 l =
  match ks1 with
  | Proper_kset (_, s1) ->
    let s' =
      K_set.fold (fun elem1 acc1 ->
          K_set.add (Region_bitvector.signed_extension elem1 l) acc1)
        s1 K_set.empty in
    create s'
  | _ -> Top  NoTag (* TODO: check region *)


let eq = apply Region_bitvector.eq

let diff = apply Region_bitvector.diff

let leqU = apply Region_bitvector.leqU
let leqS = apply Region_bitvector.leqS

let ltU = apply Region_bitvector.ltU
let ltS = apply Region_bitvector.ltS

let geqU = apply Region_bitvector.geqU
let geqS = apply Region_bitvector.geqS

let gtU = apply Region_bitvector.gtU
let gtS = apply Region_bitvector.gtS


let filter (f : K_set.elt -> bool) ks =
  match ks with
  | Proper_kset (k, s) -> Proper_kset (k, K_set.filter f s)
  | Top tag -> Top tag

let is_true rbv =
  match rbv with
  | `Value (`Constant, bv) when Bitvector.is_one bv -> true
  | _ -> false

let exists f ks =
  match ks with
  | Proper_kset (_k, s) -> K_set.exists f s
  | Top _ -> true

let filter_exists f ks1 ks2 =
  filter (fun elt1 -> exists (fun elt2 -> is_true (f elt1 elt2)) ks2) ks1,
  filter (fun elt1 -> exists (fun elt2 -> is_true (f elt1 elt2)) ks1) ks2

  
let guard op ks1 ks2 =
  let ks_1, ks_2 =
    (match op with
     | Dba.Eq ->
       let ks = meet ks1 ks2 in ks, ks
     | Dba.Diff -> filter_exists Region_bitvector.diff ks1 ks2
     | Dba.LeqU -> 
       (filter (fun elt1 ->
            exists (fun elt2 ->
                let c = (Region_bitvector.leqU elt1 elt2) in
                is_true c) ks2) ks1),
       (filter (fun elt1 ->
            exists (fun elt2 ->
                let c = (Region_bitvector.geqU elt1 elt2) in
                is_true c) ks1) ks2)
     | Dba.LtU ->
       (filter (fun elt1 ->
            exists (fun elt2 ->
                let c = (Region_bitvector.ltU elt1 elt2) in
                is_true c) ks2) ks1),
       (filter (fun elt1 ->
            exists (fun elt2 ->
                let c = (Region_bitvector.gtU elt1 elt2) in
                is_true c) ks1) ks2)
     | Dba.GeqU ->
       (filter (fun elt1 ->
            exists (fun elt2 ->
                let c = (Region_bitvector.geqU elt1 elt2) in
                is_true c) ks2) ks1),
       (filter (fun elt1 ->
            exists (fun elt2 ->
                let c = (Region_bitvector.leqU elt1 elt2) in
                is_true c) ks1) ks2)
     | Dba.GtU ->
       (filter (fun elt1 ->
            exists (fun elt2 ->
                let c = (Region_bitvector.gtU elt1 elt2) in
                is_true c) ks2) ks1),
       (filter (fun elt1 ->
            exists (fun elt2 ->
                let c = (Region_bitvector.ltU elt1 elt2) in
                is_true c) ks1) ks2)
     | Dba.LeqS ->
       (filter (fun elt1 ->
            exists (fun elt2 ->
                let c = (Region_bitvector.leqS elt1 elt2) in
                is_true c) ks2) ks1),
       (filter (fun elt1 ->
            exists (fun elt2 ->
                let c = (Region_bitvector.geqS elt1 elt2) in
                is_true c) ks1) ks2)
     | Dba.LtS ->
       (filter (fun elt1 ->
            exists (fun elt2 ->
                let c = (Region_bitvector.ltS elt1 elt2) in
                is_true c) ks2) ks1),
       (filter (fun elt1 ->
            exists (fun elt2 ->
                let c = (Region_bitvector.gtS elt1 elt2) in
                is_true c) ks1) ks2)
     | Dba.GeqS ->
       (filter (fun elt1 ->
            exists (fun elt2 ->
                let c = (Region_bitvector.geqS elt1 elt2) in
                is_true c) ks2) ks1),
       (filter (fun elt1 ->
            exists (fun elt2 ->
                let c = (Region_bitvector.leqS elt1 elt2) in
                is_true c) ks1) ks2)
     | Dba.GtS ->
       (filter (fun elt1 ->
            exists (fun elt2 ->
                let c = (Region_bitvector.gtS elt1 elt2) in
                is_true c) ks2) ks1),
       (filter (fun elt1 ->
            exists (fun elt2 ->
                let c = (Region_bitvector.ltS elt1 elt2) in
                is_true c) ks1) ks2)
     |  _ -> ks1, ks2)
  in
  ks_1, ks_2


let _fold f a b =
  match a with
  | Proper_kset (_k, s) -> K_set.fold f s b
  | Top _ -> failwith "kset.ml : fold of top kset"


let _iter f a =
  match a with
    Proper_kset (_, s) -> K_set.iter f s
  |	Top _ -> failwith "Kset.ml : iter of top"


let is_true kset assumes glbs =
  begin match kset with
    | Proper_kset (_k, set) ->
      let b' =
        let elem = K_set.choose set in
        begin match elem with
          | `Value (`Constant, b) when Bitvector.size_of b = 1 ->
            Bitvector.value_of b
          | `SymbSmt smb ->
            let e = Region_bitvector.get_expr smb 1 assumes glbs in
            begin match e with
              | Dba.ExprCst (`Constant, b)
                when Bitvector.size_of b = 1 -> Bitvector.value_of b
              | _ -> raise (Bad_region "Evaluating non cst condition1")
            end
          | _ -> raise (Bad_region "Evaluating non cst condition2")
        end
      in
      let cond_homogeneous =
        let f rbv =
          begin match rbv with
            | `Value (`Constant, b) when Bitvector.size_of b = 1 ->
              Bigint.eq_big_int (Bitvector.value_of b) b'
            | `SymbSmt smb ->
              let e = Region_bitvector.get_expr smb 1 assumes glbs in
              begin match e with
                | Dba.ExprCst (`Constant, b) when Bitvector.size_of b = 1 ->
                  Bigint.eq_big_int (Bitvector.value_of b) b'
                | _ -> raise (Bad_region "Evaluating non cst condition3")
              end
            | _ -> raise (Bad_region "Evaluating non cst condition4")
          end
        in
        K_set.for_all f set
      in
      let open Basic_types.Ternary in
      if cond_homogeneous
      then of_bool (Bigint.eq_big_int b' Bigint.unit_big_int)
      else Unknown
    | Top _ -> Basic_types.Ternary.Unknown
  end


let to_smt (kset: t) (var: Smtlib2.smt_bv_expr) : Smtlib2.smt_bv_expr list =
  match kset with
    Top _p -> []
  | Proper_kset (_k, set) ->
    let expr =
      K_set.fold (fun rbv acc ->
          match rbv with
          | `Value (_r, bv) ->
            let bv = Smtlib2.SmtBvCst bv in
            Smtlib2.SmtBvBinary (
              Smtlib2.SmtBvOr,
              (Smtlib2.SmtBvBinary (Smtlib2.SmtBvComp, bv, var)),
              acc)
          | _ -> acc
        ) set (Smtlib2.SmtBvCst (Bitvector.zero))
    in
    [expr]


let smt_refine kset env_smt var =
  match kset with
    Top p -> Top p
  | Proper_kset (k, set) ->
    let set = K_set.filter (fun rbv ->
        match rbv with
        | `Value (_r, bv) ->
          let cond = Smtlib2print.smtbvexpr_to_string (Smtlib2.SmtBvCst bv) in
          let conds = Format.asprintf "(assert (= %s %s))@\n" var cond in
          Normalize_instructions.is_sat env_smt conds
        | _ -> true
      ) set
    in
    Proper_kset (k, set)
