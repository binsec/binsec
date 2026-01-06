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

module BiTbl = Basic_types.Integers.Bigint.Htbl

module type DEBUG = sig
  val comment : (Format.formatter -> 'a -> unit) -> 'a -> unit
  val get_value : Types.Expr.t -> Z.t -> int -> unit
end

type inout = { mutable term : Types.Expr.t; mutable repr : string }

let make :
    type a.
    (module Smtlib.Solver.Session.S with type arg = a) ->
    a ->
    (module DEBUG) * (module Session.S) =
 fun session arg ->
  let module Session = (val session) in
  let session =
    let session = Session.open_session arg in
    Session.put session (Smtlib.Formula.pp_header ~theory:"QF_ABV") ();
    session
  and ctx = ref [ Printer.create ~next_id:Suid.zero () ] in
  let top () = List.hd !ctx in
  let inout = { term = Types.Expr.zero; repr = "" } in
  ( (module struct
      let comment ppf a = Session.comment session ppf a

      let get_value e v sz =
        inout.term <- e;
        Session.put session
          (fun ppf r ->
            let ctx = top () in
            r.repr <- Printer.pp_flush_bv ctx ppf r.term)
          inout;
        inout.term <- e;
        ignore (Session.get_value session Format.pp_print_string inout.repr);
        Session.comment session
          (fun ppf k ->
            Format.pp_print_string ppf "((";
            Format.pp_print_string ppf k;
            Format.pp_print_char ppf ' ';
            Printer.pp_bv ppf v sz;
            Format.pp_print_string ppf "))")
          inout.repr
    end),
    (module struct
      open Types

      let visit_formula formula =
        let ctx = top () in
        List.iter
          (fun bl ->
            Printer.visit_bl ctx bl;
            Printer.visit_bl ctx bl)
          formula

      let iter_free_variables f = Printer.iter_free_variables f (top ())
      let iter_free_arrays f = Printer.iter_free_arrays f (top ())

      let push () =
        ctx := Printer.copy (List.hd !ctx) :: !ctx;
        Session.put session Format.pp_print_string "(push 1)"

      let pop () =
        ctx := List.tl !ctx;
        Session.put session Format.pp_print_string "(pop 1)"

      let assert_formula bl =
        Session.put session
          (fun ppf bl ->
            let ctx = top () in
            (* print declarations *)
            let bl = Printer.pp_flush_bl ctx ppf bl in
            (* print assertion *)
            Format.pp_print_string ppf "(assert ";
            Format.pp_print_string ppf bl;
            Format.pp_print_char ppf ')';
            Format.pp_print_space ppf ())
          bl

      let assert_distinct x y =
        Session.put session
          (fun ppf (x, y) ->
            let ctx = top () in
            (* print declarations *)
            let x = Printer.pp_flush_bv ctx ppf x in
            let y = Printer.pp_flush_bv ctx ppf y in
            (* print assertion *)
            Format.pp_print_string ppf "(assert (not (= ";
            Format.pp_print_string ppf x;
            Format.pp_print_space ppf ();
            Format.pp_print_string ppf y;
            Format.pp_print_string ppf ")))";
            Format.pp_print_space ppf ())
          (x, y)

      let value_of_constant (cst : Smtlib.Lang.constant) =
        match cst with
        | CstBool false -> Z.zero
        | CstBool true -> Z.one
        | CstBinary b -> Z.of_string_base 2 b
        | CstDecimal d | CstDecimalSize (d, _) -> Z.of_string_base 10 d
        | CstHexadecimal x -> Z.of_string_base 16 x
        | CstNumeral _ | CstString _ -> assert false

      let value_of_term (t : (Smtlib.Lang.term * Smtlib.Lang.term) list) =
        match t with
        | [ (_, { term_desc = TermSpecConstant cst; _ }) ] ->
            value_of_constant cst
        | _ -> assert false

      let get_value x =
        inout.term <- x;
        Session.put session
          (fun ppf r ->
            let ctx = top () in
            r.repr <- Printer.pp_flush_bv ctx ppf r.term)
          inout;
        value_of_term
          (Session.get_value session Format.pp_print_string inout.repr)

      let get_at name x s =
        value_of_term
          (Session.get_value session
             (fun ppf x ->
               Format.pp_print_string ppf "(select ";
               Format.pp_print_string ppf name;
               Format.pp_print_char ppf ' ';
               Printer.pp_bv ppf x s;
               Format.pp_print_char ppf ')')
             x)

      let fold_array_values :
          (Z.t -> Z.t -> 'a -> 'a) -> Memory.symbol -> 'a -> 'a =
       fun f (Symbol { index = idx_size; _ } as ar) x ->
        let ctx = top () in
        let n = Printer.array_accesses_count ctx ar in
        if n = 0 then x
        else
          let dirty = BiTbl.create n in
          let name = Format.asprintf "%a" (Printer.pp_print_ax ctx) ar in
          Printer.fold_array_accesses
            (fun x (access : Printer.access) ->
              match access with
              | Select (index, len) ->
                  let index =
                    value_of_term
                      (Session.get_value session Format.pp_print_string index)
                  in
                  let rec fold index len x =
                    if len = 0 then x
                    else if BiTbl.mem dirty index then
                      fold (Z.succ index) (len - 1) x
                    else
                      let k = get_at name index idx_size in
                      fold (Z.succ index) (len - 1) (f index k x)
                  in
                  fold index len x
              | Store (index, len) ->
                  let index =
                    value_of_term
                      (Session.get_value session Format.pp_print_string index)
                  in
                  let rec loop index len =
                    if len <> 0 then (
                      BiTbl.replace dirty index ();
                      loop (Z.succ index) (len - 1))
                  in
                  loop index len;
                  x)
            ctx ar x

      let check_sat ?(timeout = Float.infinity) () =
        Session.check_sat session ~timeout

      let check_sat_assuming ?(timeout = Float.infinity) bl =
        inout.term <- bl;
        Session.put session
          (fun ppf r ->
            let ctx = top () in
            r.repr <- Printer.pp_flush_bl ctx ppf bl)
          inout;
        Session.check_sat_assuming session ~timeout Format.pp_print_string
          inout.repr

      let close () = Session.close_session session
    end) )

let make_carbon_copy : (module Session.S) -> string -> (module Session.S) =
 fun session filename ->
  (module struct
    module Main = (val session)

    let debug, copy = make (module Smtlib.Solver.Session.Dump) filename

    module Debug = (val debug)
    module Copy = (val copy)

    let visit_formula formula =
      Copy.visit_formula formula;
      Main.visit_formula formula

    let iter_free_variables = Main.iter_free_variables
    let iter_free_arrays = Main.iter_free_arrays

    let assert_formula formula =
      Copy.assert_formula formula;
      Main.assert_formula formula

    let assert_distinct x y =
      Copy.assert_distinct x y;
      Main.assert_distinct x y

    let check_sat ?timeout () =
      ignore (Copy.check_sat ?timeout ());
      let r = Main.check_sat ?timeout () in
      Debug.comment Smtlib.Solver.Session.pp_status r;
      r

    let check_sat_assuming ?timeout e =
      ignore (Copy.check_sat_assuming ?timeout e);
      let r = Main.check_sat_assuming ?timeout e in
      Debug.comment Smtlib.Solver.Session.pp_status r;
      r

    let get_value e =
      let r = Main.get_value e in
      Debug.get_value e r (Types.Expr.sizeof e);
      r

    let fold_array_values :
        (Z.t -> Z.t -> 'a -> 'a) -> Types.Memory.symbol -> 'a -> 'a =
     fun f (Symbol { index; _ } as s) a ->
      Main.fold_array_values
        (fun k v a ->
          Debug.get_value
            (Types.Expr.load 1 LittleEndian
               (Types.Expr.constant (Bitvector.create k index))
               s)
            v 8;
          f k v a)
        s a

    let push () =
      Copy.push ();
      Main.push ()

    let pop () =
      Copy.pop ();
      Main.pop ()

    let close () =
      Copy.close ();
      Main.close ()
  end : Session.S)

let make :
    type a.
    (module Smtlib.Solver.Session.S with type arg = a) ->
    a ->
    (module Session.S) =
 fun session arg -> snd (make session arg)
