(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2022                                               *)
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

open Options
open Types

exception Halt

module OMap = Map.Make (struct
  type t = string

  let compare =
    let rec iter s s' i =
      let d =
        Char.code (String.unsafe_get s i) - Char.code (String.unsafe_get s' i)
      in
      if d = 0 && i > 0 then iter s s' (i - 1) else d
    in

    fun s s' -> iter s s' (min (String.length s) (String.length s') - 1)
end)

let pp_value_as (format : Output.format) ppf bv =
  match format with
  | Bin -> Format.pp_print_string ppf @@ Bitvector.to_bitstring bv
  | Dec -> Z.pp_print ppf @@ Bitvector.signed_of bv
  | Hex -> Format.pp_print_string ppf @@ Bitvector.to_hexstring bv
  | Ascii -> Format.fprintf ppf "%S" @@ Bitvector.to_asciistring bv

module Start (SF : STATE_FACTORY) (W : WORKLIST) = struct
  type time = { mutable sec : float }

  module Exploration_stats = Stats.Exploration ()

  module Query_stats = Stats.Query ()

  module Screen = Screen.Make (Exploration_stats) (Query_stats)

  module State = struct
    include SF (Query_stats)

    let get_value' = get_value

    let rec get_value e t =
      try get_value' (Value.eval e t) t with
      | Undef var -> get_value e (fresh var t)
      | Uninterp array -> get_value e (alloc ~array t)

    let assume' = assume

    let rec assume e t =
      try assume' (Value.eval e t) t with
      | Undef var -> assume e (fresh var t)
      | Uninterp array -> assume e (alloc ~array t)

    let test' = test

    let rec test e t =
      try test' (Value.eval e t) t with
      | Undef var -> test e (fresh var t)
      | Uninterp array -> test e (alloc ~array t)

    let rec split_on e ?n ?except t =
      try enumerate (Value.eval e t) ?n ?except t with
      | Undef var -> split_on e ?n ?except (fresh var t)
      | Uninterp array -> split_on e ?n ?except (alloc ~array t)

    let assign' = assign

    let rec assign name e t =
      try assign' name (Value.eval e t) t with
      | Undef var -> assign name e (fresh var t)
      | Uninterp array -> assign name e (alloc ~array t)

    let rec read ~addr bytes dir t =
      try Value.read ~addr:(Value.eval addr t) bytes dir t with
      | Undef var -> read ~addr bytes dir (fresh var t)
      | Uninterp array -> read ~addr bytes dir (alloc ~array t)

    let get_value' = get_value

    let write' = write

    let rec write ~addr value dir t =
      try write' ~addr:(Value.eval addr t) (Value.eval value t) dir t with
      | Undef var -> write ~addr value dir (fresh var t)
      | Uninterp array -> write ~addr value dir (alloc ~array t)

    let rec select name ~addr bytes dir t =
      try Value.select name ~addr:(Value.eval addr t) bytes dir t with
      | Undef var -> select name ~addr bytes dir (fresh var t)
      | Uninterp array -> select name ~addr bytes dir (alloc ~array t)

    let store' = store

    let rec store name ~addr value dir t =
      try store' name ~addr:(Value.eval addr t) (Value.eval value t) dir t with
      | Undef var -> store name ~addr value dir (fresh var t)
      | Uninterp array -> store name ~addr value dir (alloc ~array t)
  end

  type label = [ `Label ] Fiber.t

  and fiber = [ `All ] Fiber.t

  and thunk = {
    id : int;
    depth : int;
    scope : label;
    fiber : fiber;
    state : State.t;
  }

  and 'a mode =
    | Default : unit mode
    | Linear : State.t mode
    | Merge : thunk mode

  type section =
    | RX of { base : Virtual_address.t; content : Loader_buf.t }
    | RWX

  type t = {
    mutable code : section Imap.t;  (** set of executable sections *)
    rocache : label Virtual_address.Htbl.t;
        (** instruction cache for RX sections *)
    cache : label OMap.t Virtual_address.Htbl.t;
        (** instruction cache for RWX sections *)
    mutable worklist : thunk W.t;  (** worklist of pending path *)
    mutable tid : int;  (** the next unique task identifier *)
    tasks : unit I.Htbl.t;  (** set of tasks to perform *)
  }

  let env : t =
    let img = Kernel_functions.get_img () in
    let transient = TransientEnum.get () > 0 in
    let rocache_size, _cache_size, code =
      Array.fold_left
        (fun (rocache, cache, code) s ->
          if Loader.Section.has_flag Loader_types.Exec s then
            let { Loader_types.virt = pos; _ } = Loader.Section.pos s in
            let { Loader_types.virt = size; _ } = Loader.Section.size s in
            let rodelta, delta, section =
              if Loader.Section.has_flag Loader_types.Write s then
                if transient then (
                  Logger.debug ~level:4
                    "Section %S [%a, 0x%x] has both Write and Execute flags."
                    (Loader.Section.name s) Virtual_address.pp
                    (Virtual_address.create pos)
                    size;
                  (0, size, RWX))
                else (
                  Logger.warning
                    "Section %S [%a, 0x%x] has both Write and Execute flags.@ \
                     Self-modifying code is disabled and writes will be \
                     ignored.@ Use '-sse-self-written-enum N' to enable \
                     symbolic reasoning up to 'N - 1' forks."
                    (Loader.Section.name s) Virtual_address.pp
                    (Virtual_address.create pos)
                    size;
                  ( size,
                    0,
                    RX
                      {
                        base = Virtual_address.create pos;
                        content = Loader.Img.content img s;
                      } ))
              else
                ( size,
                  0,
                  RX
                    {
                      base = Virtual_address.create pos;
                      content = Loader.Img.content img s;
                    } )
            in
            ( rocache + rodelta,
              cache + delta,
              Imap.add ~base:(Z.of_int pos) size section code )
          else (rocache, cache, code))
        (0, 0, Imap.empty) (Loader.Img.sections img)
    in
    let arrays = S.Htbl.create 7 in
    S.Htbl.add arrays "@" A.default;
    {
      code;
      rocache = Virtual_address.Htbl.create rocache_size;
      cache = Virtual_address.Htbl.create 0 (* cache_size *);
      worklist = W.empty;
      tid = 0;
      tasks = I.Htbl.create 7;
    }

  let choose () =
    try
      let path, worklist = W.pop env.worklist in
      Logger.debug "Selecting path #%d (among %d)" path.id
        (Exploration_stats.get_pending_paths ());
      env.worklist <- worklist;
      path
    with Not_found ->
      Logger.info "Empty path worklist: halting ...";
      raise_notrace Halt

  let add path = env.worklist <- W.push path env.worklist

  let halt () =
    Screen.release ();
    Logger.info "@[<v 0>@[<v 2>SMT queries@,%a@]@,@[<v 2>Exploration@,%a@]@,@]"
      Query_stats.pp () Exploration_stats.pp ()

  let print addr state (output : Output.t) =
    match output with
    | Model ->
        Logger.result "@[<v 0>Model %@ %a@ %a@]" Virtual_address.pp addr
          State.pp state
    | Formula ->
        Logger.result "Formula %@ %a@\n%a" Virtual_address.pp addr
          (State.pp_smt None) state
    | Slice slice ->
        Logger.result "Formula for %a %@ %a@\n%a" Virtual_address.pp addr
          (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf (_, n) ->
               Format.pp_print_string ppf n))
          slice
          (State.pp_smt (Some slice))
          state
    | Value (format, e) ->
        Logger.result "@[<v 0>Value %a : %a@]" Dba_printer.Ascii.pp_bl_term e
          (pp_value_as format)
          (fst (List.hd (State.split_on ~n:1 e state)))
    | Stream name ->
        Logger.result "@[<v 0>Ascii stream %s : %S@]" name
          (State.as_ascii ~name state)
    | String name ->
        Logger.result "@[<v 0>C string %s : %S@]" name
          (State.as_c_string ~name state)

  let relink cur succ (pred : fiber) =
    match pred with
    | Hook t -> t.succ <- succ
    | Exec t -> t.succ <- succ
    | Assign t -> t.succ <- succ
    | Clobber t -> t.succ <- succ
    | Load t -> t.succ <- succ
    | Store t -> t.succ <- succ
    | Symbolize t -> t.succ <- succ
    | Assume t -> t.succ <- succ
    | Assert t -> t.succ <- succ
    | Branch ({ taken; _ } as t) when taken == cur -> t.taken <- succ
    | Branch t -> t.fallthrough <- succ
    | Probe t -> t.succ <- succ
    | Goto _ | Jump _ | Cut | Halt | Die _ -> ()

  let disasm =
    let return infos hunks usize =
      let infos = List.rev infos in
      let addr, info = List.hd infos
      and others = List.tl infos
      and n = Queue.length hunks in
      let cur = ref (Queue.pop hunks) and pos = ref 0 in
      let hunk =
        Dhunk.init usize (fun i ->
            if i - !pos = Dhunk.length !cur then (
              cur := Queue.pop hunks;
              pos := i);
            let inst = Dhunk.inst_exn !cur (i - !pos) in
            match inst with
            | Assign (lval, rval, i) -> Dba.Instr.assign lval rval (!pos + i)
            | SJump (JInner i, tag) ->
                Dba.Instr.static_inner_jump ~tag (!pos + i)
            | SJump (_, tag) ->
                if Queue.is_empty hunks then inst
                else Dba.Instr.static_inner_jump ~tag (!pos + Dhunk.length !cur)
            | DJump _ -> inst
            | If (exp, JInner i, i') ->
                Dba.Instr.ite exp (JInner (!pos + i)) (!pos + i')
            | If (exp, jo, i) ->
                let jo =
                  if Queue.is_empty hunks then jo
                  else JInner (!pos + Dhunk.length !cur)
                in
                Dba.Instr.ite exp jo (!pos + i)
            | Stop _ -> inst
            | Assert (exp, i) -> Dba.Instr._assert exp (!pos + i)
            | Assume (exp, i) -> Dba.Instr.assume exp (!pos + i)
            | Nondet (lval, i) -> Dba.Instr.non_deterministic lval (!pos + i)
            | Undef (lval, i) -> Dba.Instr.undefined lval (!pos + i))
      in
      let scope =
        Fiber.Exec { addr; info; n; others; succ = Fiber.of_dhunk hunk }
      in
      Virtual_address.Htbl.add env.rocache addr scope;
      scope
    in
    let rec disasm addr reader infos hunks usize =
      Exploration_stats.register_address addr;
      let inst, _ = Disasm_core.decode_from reader addr in
      let hunk = Instruction.hunk inst in
      Queue.add hunk hunks;
      let infos =
        (addr, Mnemonic.to_string (Instruction.mnemonic inst)) :: infos
      and usize = usize + Dhunk.length hunk in
      match Dhunk.exits hunk with
      | [ i ] -> (
          match Dhunk.inst_exn hunk i with
          | SJump (JOuter { base; _ }, _) ->
              if
                Virtual_address.diff base addr
                = Size.Byte.to_int (Instruction.size inst)
                && not (Virtual_address.Htbl.mem env.rocache base)
              then disasm base reader infos hunks usize
              else return infos hunks usize
          | _ -> return infos hunks usize)
      | _ -> return infos hunks usize
    in
    fun addr reader -> disasm addr reader [] (Queue.create ()) 0

  exception Continue of label

  let rec exec :
      type a.
      a mode -> int -> int -> max_depth:int -> State.t -> label -> fiber -> a =
   fun mode id depth ~max_depth state scope fiber ->
    match fiber with
    | Hook { succ; info; _ } as scope ->
        Logger.debug ~level:2 "hook %s" info;
        exec mode id depth ~max_depth state scope succ
    | Exec { addr; info; n; others; succ } as scope -> (
        let depth = depth + n in
        Logger.debug ~level:2 "@[<v>%@%a %s%a@]" Virtual_address.pp addr info
          (fun ppf others ->
            List.iter
              (fun (addr, info) ->
                Format.fprintf ppf "@ %@%a %s" Virtual_address.pp addr info)
              others)
          others;
        Exploration_stats.add_instructions n;
        Exploration_stats.update_depth depth;
        if depth <= max_depth then
          exec mode id depth ~max_depth state scope succ
        else
          match mode with
          | Default ->
              Logger.warning "@[<hov>Cut path %d (max depth) %@ %a@]" id
                Virtual_address.pp addr;
              yield Default ~max_depth
          | Linear -> state
          | Merge -> { id; depth; scope; state; fiber })
    | Symbolize { var; succ } ->
        exec mode id depth ~max_depth (State.fresh var state) scope succ
    | Assign { var; rval; succ } ->
        exec mode id depth ~max_depth (State.assign var rval state) scope succ
    | Clobber { var = { name; size; _ } as var; succ } ->
        Logger.warning ~level:3
          "path %d undefined variable %S arbitrarily set to zeros %@ %a" id name
          Virtual_address.pp (Fiber.addr scope);
        exec mode id depth ~max_depth
          (State.assign var (Dba.Expr.zeros size) state)
          scope succ
    | Load { var; base = None; dir; addr; succ } ->
        let rval, state = State.read ~addr (var.size / 8) dir state in
        exec mode id depth ~max_depth (State.assign' var rval state) scope succ
    | Store { base = None; dir; addr; rval; succ } ->
        exec mode id depth ~max_depth
          (State.write ~addr rval dir state)
          scope succ
    | Load { var; base = Some name; dir; addr; succ } ->
        let rval, state = State.select name ~addr (var.size / 8) dir state in
        exec mode id depth ~max_depth (State.assign' var rval state) scope succ
    | Store { base = Some name; dir; addr; rval; succ } ->
        exec mode id depth ~max_depth
          (State.store name ~addr rval dir state)
          scope succ
    | Assume _ as fiber -> assume mode id depth ~max_depth state scope fiber
    | Assert _ as fiber -> check mode id depth ~max_depth state scope fiber
    | Branch _ as fiber -> ite mode id depth ~max_depth state scope fiber
    | Goto { addr; preds } ->
        goto mode id depth ~max_depth state scope addr preds fiber
    | Jump _ as fiber -> dynamic_jump mode id depth ~max_depth state scope fiber
    | Probe _ as fiber -> probe mode id depth ~max_depth state scope fiber
    | Cut ->
        Exploration_stats.terminate_path ();
        Logger.debug ~level:0 "@[<hov>Cut path %d (directive) %@ %a@]" id
          Virtual_address.pp (Fiber.addr scope);
        yield mode ~max_depth
    | Halt -> (
        match mode with
        | Default ->
            Exploration_stats.terminate_path ();
            Logger.debug ~level:0 "@[<hov>End of path %d %@ %a@]" id
              Virtual_address.pp (Fiber.addr scope);
            yield mode ~max_depth
        | Linear -> state
        | Merge -> { id; depth; scope; state; fiber })
    | Die msg ->
        Exploration_stats.interrupt_path ();
        Logger.error "@[<hov>Cut path %d (uninterpreted %S) %@ %a@]" id msg
          Virtual_address.pp (Fiber.addr scope);
        yield mode ~max_depth

  and assume :
      type a.
      a mode ->
      int ->
      int ->
      max_depth:int ->
      State.t ->
      label ->
      [ `Assume ] Fiber.t ->
      a =
   fun mode id depth ~max_depth state scope (Assume { test; succ } as fiber) ->
    match mode with
    | Merge -> (
        match State.get_value test state with
        | exception Non_unique -> { id; depth; scope; state; fiber }
        | x ->
            if Bitvector.is_one x then
              exec mode id depth ~max_depth state scope succ
            else { id; depth; scope; state; fiber = Cut })
    | Linear | Default -> (
        match State.assume test state with
        | exception Unknown ->
            Exploration_stats.interrupt_path ();
            yield mode ~max_depth
        | None ->
            Logger.warning
              "@[<hov>Cut path %d (unsatifiable assumption) %@ %a@]" id
              Virtual_address.pp (Fiber.addr scope);
            Exploration_stats.interrupt_path ();
            yield mode ~max_depth
        | Some state -> exec mode id depth ~max_depth state scope succ)

  and check :
      type a.
      a mode ->
      int ->
      int ->
      max_depth:int ->
      State.t ->
      label ->
      [ `Assert ] Fiber.t ->
      a =
   fun mode id depth ~max_depth state scope (Assert { test; succ } as fiber) ->
    match mode with
    | Linear -> assert false
    | Merge -> (
        match State.get_value test state with
        | exception Non_unique -> { id; depth; scope; state; fiber }
        | x ->
            Exploration_stats.add_assert ();
            if Bitvector.is_one x then
              exec mode id depth ~max_depth state scope succ
            else (
              Logger.error "@[<v 2> Assertion failed %@ %a@ %a@]"
                Virtual_address.pp (Fiber.addr scope) State.pp state;
              Exploration_stats.add_failed_assert ();
              { id; depth; scope; state; fiber = Cut }))
    | Default -> (
        Exploration_stats.add_assert ();
        match State.test test state with
        | exception Unknown ->
            Exploration_stats.interrupt_path ();
            yield mode ~max_depth
        | True state -> exec mode id depth ~max_depth state scope succ
        | False state ->
            Logger.error "@[<v 2> Assertion failed %@ %a@ %a@]"
              Virtual_address.pp (Fiber.addr scope) State.pp state;
            Exploration_stats.add_failed_assert ();
            yield mode ~max_depth
        | Both { t = state; f } ->
            Logger.error "@[<v 2> Assertion failed %@ %a@ %a@]"
              Virtual_address.pp (Fiber.addr scope) State.pp f;
            Exploration_stats.add_failed_assert ();
            exec mode id depth ~max_depth state scope succ)

  and ite :
      type a.
      a mode ->
      int ->
      int ->
      max_depth:int ->
      State.t ->
      label ->
      [ `Branch ] Fiber.t ->
      a =
   fun mode id depth ~max_depth state scope
       (Branch { test; taken; fallthrough; _ } as fiber) ->
    Exploration_stats.add_branch ();
    match mode with
    | Linear -> assert false
    | Merge -> (
        match State.get_value test state with
        | exception Non_unique -> { id; depth; scope; state; fiber }
        | x ->
            Exploration_stats.add_branch ();
            exec Merge id depth ~max_depth state scope
              (if Bitvector.is_one x then taken else fallthrough))
    | Default -> (
        match State.test test state with
        | exception Unknown ->
            Exploration_stats.interrupt_path ();
            yield mode ~max_depth
        | True state ->
            add { id; depth; scope; state; fiber = taken };
            yield mode ~max_depth
        | False state ->
            add { id; depth; scope; state; fiber = fallthrough };
            yield mode ~max_depth
        | Both { t = state; f = state' } ->
            let k = QMerge.get () in
            if k > 0 then
              let id' = Exploration_stats.get_paths () in
              let taken =
                exec Merge id depth ~max_depth:(depth + k) state scope taken
              and fallthrough =
                exec Merge id' depth ~max_depth:(depth + k) state' scope
                  fallthrough
              in
              if
                taken.scope == fallthrough.scope
                && taken.fiber == fallthrough.fiber
              then
                match State.merge taken.state fallthrough.state with
                | exception Non_mergeable ->
                    add taken;
                    Exploration_stats.add_path ();
                    add fallthrough;
                    yield mode ~max_depth
                | state ->
                    exec mode id
                      (max taken.depth fallthrough.depth)
                      ~max_depth state taken.scope taken.fiber
              else (
                add taken;
                Exploration_stats.add_path ();
                add fallthrough;
                yield mode ~max_depth)
            else (
              add { id; depth; scope; state; fiber = taken };
              let id' = Exploration_stats.get_paths () in
              Exploration_stats.add_path ();
              add
                { id = id'; depth; scope; state = state'; fiber = fallthrough };
              yield mode ~max_depth))

  and dynamic_jump :
      type a.
      a mode ->
      int ->
      int ->
      max_depth:int ->
      State.t ->
      label ->
      [ `Jump ] Fiber.t ->
      a =
   fun mode id depth ~max_depth state scope (Jump target as fiber) ->
    let n = JumpEnumDepth.get () in

    let handle id depth scope addr (bv, state) =
      try
        Logger.debug ~level:4 "@[<hov>Dynamic jump@ %@%a@ could lead to@ %a@]"
          Virtual_address.pp addr Bitvector.pp_hex_or_bin bv;
        add
          {
            id;
            depth;
            scope;
            fiber = Goto { addr = Virtual_address.of_bitvector bv; preds = [] };
            state;
          }
      with Virtual_address.Non_canonical_form ->
        Logger.warning
          "@[<hov>Dynamic jump@ %a@ could have led to invalid address %a;@ \
           skipping@]"
          Virtual_address.pp addr Bitvector.pp_hex_or_bin bv;
        Exploration_stats.interrupt_path ()
    in
    match mode with
    | Linear -> assert false
    | Merge -> (
        match State.get_value target state with
        | exception Non_unique -> { id; depth; scope; state; fiber }
        | x ->
            Exploration_stats.add_branch ();
            goto mode id depth ~max_depth state scope
              (Virtual_address.of_bitvector x)
              [] fiber)
    | Default -> (
        Exploration_stats.add_branch ();
        match State.split_on target ~n state with
        | [] | (exception Unknown) ->
            Exploration_stats.interrupt_path ();
            yield mode ~max_depth
        | [ (x, state) ] ->
            goto mode id depth ~max_depth state scope
              (Virtual_address.of_bitvector x)
              [] fiber
        | x :: bx ->
            let addr = Fiber.addr scope in
            handle id depth scope addr x;
            List.iter
              (fun x ->
                handle
                  Exploration_stats.(
                    let id = get_paths () in
                    add_path ();
                    id)
                  depth scope addr x)
              bx;
            yield mode ~max_depth)

  and goto :
      type a.
      a mode ->
      int ->
      int ->
      max_depth:int ->
      State.t ->
      label ->
      Virtual_address.t ->
      fiber list ->
      fiber ->
      a =
   fun mode id depth ~max_depth state scope addr preds rollback ->
    try
      (match scope with
      | Hook { addr = addr'; _ } when addr' = addr -> raise_notrace Not_found
      | Hook _ | Exec _ -> ());
      let scope = Virtual_address.Htbl.find env.rocache addr in
      raise_notrace (Continue scope)
    with
    | Continue scope ->
        let ((Hook _ | Exec _) as succ) = scope in
        if preds <> [] then List.iter (relink rollback succ) preds;
        exec mode id depth ~max_depth state scope succ
    | Not_found -> (
        match Imap.find (Virtual_address.to_bigint addr) env.code with
        | exception Not_found ->
            Logger.warning "@[<hov>Cut path %d (non executable) %@ %a@]" id
              Virtual_address.pp addr;
            Exploration_stats.interrupt_path ();
            yield mode ~max_depth
        | RX { base; content } ->
            let reader =
              Lreader.of_zero_extend_buffer
                ~at:(Virtual_address.diff addr base)
                content
            in
            let scope : label = disasm addr reader in
            let ((Hook _ | Exec _) as fiber) = scope in
            if preds <> [] then List.iter (relink rollback fiber) preds;
            exec mode id depth ~max_depth state scope fiber
        | RWX -> (
            match mode with
            | Linear -> assert false
            | Merge -> { id; depth; scope; state; fiber = rollback }
            | Default ->
                transient_instruction mode id depth ~max_depth state addr))

  and transient_instruction :
      type a.
      a mode -> int -> int -> max_depth:int -> State.t -> Virtual_address.t -> a
      =
   fun mode id depth ~max_depth state addr ->
    let n = TransientEnum.get () in
    let handle id depth (omap : label OMap.t) addr (bv, state) =
      let opcode = Bitvector.to_asciistring bv in
      let omap, scope =
        try (omap, OMap.find opcode omap)
        with Not_found ->
          let reader = Lreader.of_bytes opcode in
          let inst = fst (Disasm_core.decode_from reader addr) in
          let succ = Fiber.of_dhunk (Instruction.hunk inst) in
          let scope =
            Fiber.Exec
              {
                addr;
                info = Mnemonic.to_string (Instruction.mnemonic inst);
                n = 1;
                others = [];
                succ;
              }
          in
          Exploration_stats.register_address addr;
          let s = (Instruction.size inst :> int) in
          let omap =
            if s = 0 then omap
            else
              OMap.add
                (String.sub opcode 0 (Instruction.size inst :> int))
                scope omap
          in
          (omap, scope)
      in
      Logger.debug ~level:4
        "@[<hov>Self-written instruction @@ %a could be %s [ %a ]@]"
        Virtual_address.pp addr
        (match scope with Hook { info; _ } | Exec { info; _ } -> info)
        String_utils.pp_hex opcode;
      let ((Hook _ | Exec _) as fiber) = scope in
      add { id; depth; scope; state; fiber };
      omap
    in
    let omap =
      try Virtual_address.Htbl.find env.cache addr
      with Not_found -> OMap.empty
    in
    let opcode =
      Expr.load
        (Isa_helper.max_instruction_len ())
        Machine.LittleEndian
        (Expr.constant
           (Bitvector.of_int
              ~size:(Kernel_options.Machine.word_size ())
              (addr :> int)))
    in

    match State.split_on opcode ~n state with
    | [] | (exception Unknown) ->
        Exploration_stats.interrupt_path ();
        yield mode ~max_depth
    | x :: bx ->
        let omap =
          List.fold_left
            (fun omap x ->
              let id = Exploration_stats.get_paths () in
              Exploration_stats.add_path ();
              let omap = handle id depth omap addr x in

              omap)
            omap bx
        in
        let omap = handle id depth omap addr x in
        Virtual_address.Htbl.replace env.cache addr omap;
        yield mode ~max_depth

  and yield : type a. a mode -> max_depth:int -> a =
   fun mode ~max_depth ->
    let { id; depth; scope; fiber; state } = choose () in
    exec mode id depth ~max_depth state scope fiber

  and probe :
      type a.
      a mode ->
      int ->
      int ->
      max_depth:int ->
      State.t ->
      label ->
      [ `Probe ] Fiber.t ->
      a =
   fun mode id depth ~max_depth state scope (Probe { kind; succ } as fiber) ->
    match kind with
    | Print output ->
        print (Fiber.addr scope) state output;
        exec mode id depth ~max_depth state scope succ
    | Enumerate ({ enum; id = tid; format; n; k; values } as e) ->
        if n > k then (
          let values' = State.split_on enum ~n ~except:values state in
          let values =
            List.fold_left (fun values (bv, _) -> bv :: values) values values'
          in
          let k = List.length values in
          Logger.result
            "@[<hov 0>Directive :: enumerate@ possible values (%d) for %a %@ \
             %a:@ @[<hov 0>%a@]@]"
            k Dba_printer.Ascii.pp_bl_term enum Virtual_address.pp
            (Fiber.addr scope)
            (Print_utils.pp_list ~sep:",@ " (pp_value_as format))
            values;
          e.k <- k;
          e.values <- values;
          if n = k then (
            I.Htbl.remove env.tasks tid;
            if I.Htbl.length env.tasks = 0 then raise_notrace Halt));
        exec mode id depth ~max_depth state scope succ
    | Reach { n = 0; _ } -> exec mode id depth ~max_depth state scope succ
    | Reach ({ id = tid; n; guard; actions } as r) -> (
        match mode with
        | Linear -> assert false
        | Merge -> { id; depth; scope; state; fiber }
        | Default -> (
            match State.assume guard state with
            | None -> exec mode id depth ~max_depth state scope succ
            | Some state' ->
                let addr = Fiber.addr scope in
                Logger.result "@[<h>Path %d reached address %a (%a to go)@]" id
                  Virtual_address.pp addr
                  (fun ppf n ->
                    if n = -1 then Format.pp_print_char ppf '*'
                    else Format.pp_print_int ppf (n - 1))
                  n;
                List.iter (fun output -> print addr state' output) actions;
                if n > 0 then (
                  r.n <- n - 1;
                  if n = 1 then (
                    I.Htbl.remove env.tasks tid;
                    if I.Htbl.length env.tasks = 0 then raise_notrace Halt));
                exec Default id depth ~max_depth state scope succ))

  let initialize_state () =
    let state = State.empty () in
    let addr_size = Kernel_options.Machine.word_size ()
    and img = Kernel_functions.get_img () in
    let entry =
      match Kernel_functions.get_ep () with
      | Some addr -> addr
      | None -> Virtual_address.create (Loader.Img.entry img)
    in
    let start = ref (Fiber.Goto { addr = entry; preds = [] }) in
    let directives = ref [] in
    let symbols : (Dba.Var.Tag.attribute * Bitvector.t) S.Htbl.t =
      S.Htbl.create 100
    and core_symbols : (Dba.Var.Tag.attribute * Bitvector.t) S.Htbl.t S.Htbl.t =
      S.Htbl.create 1
    in
    (match img with
    | ELF img ->
        let open Loader_elf in
        Array.iter
          (fun sym ->
            match Symbol.header sym with
            | { kind = SECTION; sh = SEC { name; addr; size; _ }; _ }
            | { sh = SEC _; name; value = addr; size; _ } ->
                S.Htbl.add symbols name
                  (Value, Bitvector.of_int ~size:addr_size addr);
                S.Htbl.add symbols name
                  (Size, Bitvector.of_int ~size:addr_size size);
                S.Htbl.add symbols name
                  (Last, Bitvector.of_int ~size:addr_size (addr + size - 1))
            | _ -> ())
          (Img.symbols img)
    | _ ->
        let open Loader in
        Array.iter
          (fun sym ->
            S.Htbl.add symbols (Symbol.name sym)
              (Value, Bitvector.of_int ~size:addr_size (Symbol.value sym)))
          (Img.symbols img);
        Array.iter
          (fun sec ->
            let name = Section.name sec in
            let { Loader_types.virt = addr; _ } = Section.pos sec in
            S.Htbl.add symbols name
              (Value, Bitvector.of_int ~size:addr_size addr);
            let { Loader_types.virt = size; _ } = Section.size sec in
            S.Htbl.add symbols name (Size, Bitvector.of_int ~size:addr_size size);
            S.Htbl.add symbols name
              (Last, Bitvector.of_int ~size:addr_size (addr + size - 1)))
          (Img.sections img));
    let rec copy_from addr size state =
      let section =
        match
          Loader_utils.find_section_by_address ~address:(Bitvector.to_int addr)
            img
        with
        | None ->
            Logger.fatal "address %a does not belong to file" Virtual_address.pp
              (Virtual_address.of_bitvector addr)
        | Some section -> section
      in
      let { Loader_types.virt; _ } = Loader.Section.size section in
      if virt >= size then
        State.memcpy ~addr size (Loader.Img.content img section) state
      else
        copy_from
          (Bitvector.add_int addr virt)
          (size - virt)
          (State.memcpy ~addr virt (Loader.Img.content img section) state)
    in
    let context =
      Fiber.Hook { addr = Virtual_address.zero; info = "%init%"; succ = Halt }
    in
    let set state init =
      let init = Fiber.of_script ~continue:Halt init in
      exec Linear 0 0 ~max_depth:max_int state context init
    in
    let from_core prehook state =
      match Kernel_functions.get_img () with
      | Loader.Raw _ | Loader.PE _ | Loader.TI83 _ ->
          Logger.fatal "Binary is not an ELF file."
      | Loader.ELF img' ->
          let hdr = Loader_elf.Img.header img' in
          if hdr.Loader_elf.Ehdr.kind <> Loader_elf.Ehdr.ET.CORE then
            Logger.fatal "Binary is not a core file";
          let vmap, state =
            Array.fold_left
              (fun (vmap, state) section ->
                let open Loader_elf in
                let hdr = Section.header section in
                if Section.has_flag Loader_types.Read section then
                  let addr =
                    Bitvector.of_int
                      ~size:(Kernel_options.Machine.word_size ())
                      hdr.Shdr.addr
                  in
                  ( Imap.add ~base:(Bitvector.value_of addr) hdr.Shdr.size true
                      vmap,
                    State.memcpy ~addr hdr.Shdr.size (Img.content img' section)
                      state )
                else (vmap, state))
              (Imap.empty, state)
              (Loader_elf.Img.sections img')
          in
          let transient = TransientEnum.get () > 0 in
          let _, _, xcode, state =
            Array.fold_left
              (fun (vmap, fmap, xcode, state)
                   { Loader_elf.addresses = { lo; hi }; offset; name } ->
                if S.Set.mem name fmap then (vmap, fmap, xcode, state)
                else
                  let img = Loader_elf.load_file name in
                  let size = Virtual_address.diff hi lo + 1 in
                  let section =
                    Option.get
                      (Loader_utils.find_section
                         ~p:(fun s ->
                           let { Loader_types.raw; _ } = Loader.Section.pos s in
                           Loader.Section.has_flag Loader_types.Read s
                           && offset <= raw
                           && raw < offset + size)
                         (Loader.ELF img))
                  in
                  let { Loader_types.raw; virt } = Loader.Section.pos section in
                  let base =
                    Virtual_address.diff
                      (Virtual_address.add_int (raw - offset) lo)
                      (Virtual_address.create virt)
                  in
                  Logger.debug "%08x :: %a-%a %08x %s" base Virtual_address.pp
                    lo Virtual_address.pp hi offset name;
                  let private_symbols :
                      (Dba.Var.Tag.attribute * Bitvector.t) S.Htbl.t =
                    S.Htbl.create 100
                  in
                  Array.iter
                    (fun sym ->
                      match Loader_elf.Symbol.header sym with
                      | { kind = SECTION; sh = SEC { name; addr; size; _ }; _ }
                      | { sh = SEC _; name; value = addr; size; _ } ->
                          S.Htbl.add private_symbols name
                            ( Value,
                              Bitvector.of_int ~size:addr_size (base + addr) );
                          S.Htbl.add private_symbols name
                            (Size, Bitvector.of_int ~size:addr_size size);
                          S.Htbl.add private_symbols name
                            ( Last,
                              Bitvector.of_int ~size:addr_size
                                (base + addr + size - 1) )
                      | _ -> ())
                    (Loader_elf.Img.symbols img);
                  Array.iter
                    (fun section ->
                      let name = Loader_elf.Section.name section
                      and { Loader_types.virt = addr; _ } =
                        Loader_elf.Section.pos section
                      and { Loader_types.virt = size; _ } =
                        Loader_elf.Section.size section
                      in
                      S.Htbl.add private_symbols name
                        (Value, Bitvector.of_int ~size:addr_size (base + addr));
                      S.Htbl.add private_symbols name
                        (Size, Bitvector.of_int ~size:addr_size size);
                      S.Htbl.add private_symbols name
                        ( Last,
                          Bitvector.of_int ~size:addr_size
                            (base + addr + size - 1) ))
                    (Loader_elf.Img.sections img);
                  S.Htbl.add core_symbols (Filename.basename name)
                    private_symbols;
                  let sections = Loader_elf.Img.sections img in
                  let vmap, xcode, state =
                    Array.fold_left
                      (fun (vmap, xcode, state) s ->
                        let open Loader_elf in
                        let hdr = Section.header s in
                        let pos = hdr.Shdr.addr and size = hdr.Shdr.size in
                        let addr =
                          Bitvector.of_int ~size:addr_size (pos + base)
                        in
                        let base = Bitvector.value_of addr in
                        if
                          (hdr.kind = RELA || hdr.kind = REL)
                          &&
                          let name =
                            Section.name (Array.get sections hdr.Shdr.info)
                          in
                          String_utils.start_with ~prefix:".got" name
                          || String_utils.start_with ~prefix:".plt" name
                        then (
                          Array.iter
                            (fun Rel.
                                   {
                                     offset;
                                     kind = _;
                                     symbol = { name; _ };
                                     addend;
                                   } ->
                              let addend = Option.value ~default:0 addend in
                              let reader =
                                Lreader.create
                                  ~at:(Z.to_int base + offset - pos)
                                  Loader_elf.read_address img'
                              in
                              let value =
                                Bitvector.add_int
                                  (Lreader.Read.read reader (addr_size lsr 3))
                                  (-addend)
                              in
                              Logger.debug ~level:4 "symbol %S resolved at %a"
                                name Virtual_address.pp
                                (Virtual_address.of_bitvector value);
                              S.Htbl.add symbols name (Value, value))
                            (Rel.read img hdr);
                          (vmap, xcode, state))
                        else if
                          (not (Section.has_flag Loader_types.Read s))
                          || Imap.mem base vmap
                        then (vmap, xcode, state)
                        else
                          let vmap = Imap.add ~base size true vmap in
                          let xcode =
                            if Section.has_flag Loader_types.Exec s then
                              let p =
                                if Section.has_flag Loader_types.Write s then
                                  if transient then RWX
                                  else (
                                    Logger.warning
                                      "Section %S [%a, 0x%x] has both Write \
                                       and Execute flags.@ Self-modifying code \
                                       is disabled and writes will be \
                                       ignored.@ Use '-sse-self-written-enum \
                                       N' to enable symbolic reasoning up to \
                                       'N - 1' forks."
                                      (Section.name s) Virtual_address.pp
                                      (Virtual_address.create pos)
                                      size;
                                    RX
                                      {
                                        base = Virtual_address.create pos;
                                        content = Img.content img s;
                                      })
                                else
                                  RX
                                    {
                                      base = Virtual_address.of_bitvector addr;
                                      content = Img.content img s;
                                    }
                              in
                              Imap.add ~base size p xcode
                            else xcode
                          in
                          let state =
                            State.memcpy ~addr size (Img.content img s) state
                          in
                          (vmap, xcode, state))
                      (vmap, xcode, state) sections
                  in
                  (vmap, S.Set.add name fmap, xcode, state))
              (vmap, S.Set.empty, env.code, state)
              (Loader_elf.files img')
          in
          let entrypoint, initializations = Isa_helper.core img' in
          start :=
            Fiber.of_script
              ~continue:(Goto { addr = entrypoint; preds = [] })
              prehook;
          env.code <- xcode;
          let state =
            List.fold_left
              (fun state (var, value) -> State.assign var value state)
              state initializations
          in
          state
    in
    let add_directive addr saddr (d : Script.Directive.t) =
      let fiber : fiber =
        match Virtual_address.Htbl.find env.rocache addr with
        | (Exec _ | Hook _) as fiber -> fiber
        | exception Not_found -> Fiber.Goto { addr; preds = [] }
      in
      let fiber =
        match d with
        | Cut guard -> Fiber.mk_cut addr saddr guard fiber
        | Assume test -> Fiber.mk_assume addr saddr test fiber
        | Assert test -> Fiber.mk_assert addr saddr test fiber
        | Reach (n, guard, actions) ->
            let tid = env.tid in
            I.Htbl.add env.tasks tid ();
            env.tid <- env.tid + 1;
            Fiber.mk_reach addr saddr tid guard n (List.rev actions) fiber
        | Enumerate (n, enum) ->
            let tid = env.tid in
            I.Htbl.add env.tasks tid ();
            env.tid <- env.tid + 1;
            Fiber.mk_enumerate addr saddr tid Hex enum n fiber
      in
      Virtual_address.Htbl.replace env.rocache addr fiber
    in
    let state =
      match ScriptFiles.get () with
      | [] -> state
      | files ->
          let module M :
            Ast_builder.ENV
              with type lval := Script.LValue.t
               and type expr := Script.Expr.t = struct
            let wordsize = Kernel_options.Machine.word_size ()

            let endianness = Kernel_options.Machine.endianness ()

            let tbl = S.Htbl.create 128;;

            List.iter
              (fun (name, var) ->
                S.Htbl.add tbl (String.lowercase_ascii name) var)
              (Isa_helper.get_defs ())

            let lookup name size =
              let ci_name = String.lowercase_ascii name in
              try Basic_types.String.Htbl.find tbl ci_name
              with Not_found ->
                if size = -1 then
                  Logger.fatal "size is missing for variable %s" name;
                let var =
                  Script.LValue.var name ~bitsize:(Size.Bit.create size)
                in
                S.Htbl.add tbl ci_name var;
                var

            let tbl = S.Htbl.create 128

            let lookup_symbol name (attr : Dba.Var.Tag.attribute) =
              try List.assoc attr (S.Htbl.find_all tbl name)
              with Not_found ->
                let value =
                  lazy
                    (try List.assoc attr (S.Htbl.find_all symbols name)
                     with Not_found ->
                       Logger.fatal "Can not resolve symbol <%s%a>" name
                         Dba.Var.Tag.pp_attribute attr)
                in
                let tag = Dba.Var.Tag.Symbol (attr, value) in
                let sym = Script.Expr.var ~tag name wordsize in
                S.Htbl.add tbl name (attr, sym);
                sym
          end in
          let module P = Sse_parser.Make (M) in
          List.fold_left
            (fun state filename ->
              if not (Sys.file_exists filename) then
                Logger.fatal "Cannot find sse configuration file %s" filename;
              let script =
                Logger.debug "Reading script from %s" filename;
                let parser = P.script and lexer = Script_lexer.token in
                Parse_utils.read_file ~parser ~lexer ~filename
              in
              List.fold_left
                (fun state (stmt : Script.t) ->
                  match stmt with
                  | Start_from (a, prehook) -> (
                      try
                        let bv = State.get_value a state in
                        Logger.debug ~level:40
                          "the entrypoint address %a resolves to %a"
                          Dba_printer.Ascii.pp_bl_term a Bitvector.pp_hex_or_bin
                          bv;
                        start :=
                          Fiber.of_script
                            ~continue:
                              (Goto
                                 {
                                   addr = Virtual_address.of_bitvector bv;
                                   preds = [];
                                 })
                            prehook;
                        state
                      with Non_unique ->
                        Logger.fatal
                          "the entrypoint address %a does not resolve to a \
                           unique value"
                          Dba_printer.Ascii.pp_bl_term a)
                  | Start_from_core prehook -> from_core prehook state
                  | Load_sections names ->
                      List.fold_left
                        (fun ss name ->
                          let section =
                            Loader_utils.find_section_by_name name img
                          in
                          let addr =
                            Bitvector.of_int ~size:addr_size
                              (Loader.Section.pos section).virt
                          and size = (Loader.Section.size section).virt in
                          Logger.info "Load section %s (%a, %#x)" name
                            Bitvector.pp_hex_or_bin addr size;
                          State.memcpy ~addr size
                            (Loader.Img.content img section)
                            ss)
                        state names
                  | Load_data load -> (
                      match load with
                      | Load (len, LittleEndian, addr, None) -> (
                          try
                            let bv = State.get_value addr state in
                            Logger.debug ~level:40
                              "the memory initializer address %a resolves to %a"
                              Dba_printer.Ascii.pp_bl_term addr Bitvector.pp bv;
                            copy_from bv len state
                          with Non_unique ->
                            Logger.fatal
                              "the memory initializer address %a does not \
                               resolve to a unique value"
                              Dba_printer.Ascii.pp_bl_term addr)
                      | _ -> assert false)
                  | Import_symbols (names, file) ->
                      List.iter
                        (fun (name, attr) ->
                          try
                            S.Htbl.add symbols name
                              (List.find
                                 (fun (attr', _) -> attr = attr')
                                 (S.Htbl.find_all
                                    (S.Htbl.find core_symbols file)
                                    name))
                          with Not_found ->
                            Logger.fatal "unable to import <%s%a> from %s" name
                              Dba.Var.Tag.pp_attribute attr file)
                        names;
                      state
                  | Script.Stub (a, b) ->
                      let hook = Fiber.of_script b in
                      List.iter
                        (fun a ->
                          try
                            Logger.debug ~level:10
                              "@[<v 2> replace address %a by%a@]"
                              Dba_printer.Ascii.pp_bl_term a
                              Script.Instr.pp_list b;
                            let bv = State.get_value a state in
                            Logger.debug ~level:40
                              "the stub address %a resolves to %a"
                              Dba_printer.Ascii.pp_bl_term a
                              Bitvector.pp_hex_or_bin bv;
                            let addr = Virtual_address.of_bitvector bv in
                            let info =
                              Format.asprintf "stub for %a"
                                Dba_printer.Ascii.pp_bl_term a
                            in
                            Virtual_address.Htbl.add env.rocache addr
                              (Hook { addr; info; succ = hook })
                          with Not_found ->
                            Logger.fatal
                              "the stub address %a does not resolve to a \
                               unique value"
                              Dba_printer.Ascii.pp_bl_term a)
                        a;
                      state
                  | Init i -> set state i
                  | Directive (loc, action) -> (
                      try
                        let bv = State.get_value loc state in
                        Logger.debug ~level:40
                          "the directive address %a resolves to %a"
                          Dba_printer.Ascii.pp_bl_term loc
                          Bitvector.pp_hex_or_bin bv;
                        let addr = Virtual_address.of_bitvector bv in
                        directives :=
                          ( addr,
                            Format.asprintf "%a" Dba_printer.Ascii.pp_bl_term
                              loc,
                            action )
                          :: !directives;
                        state
                      with Non_unique ->
                        Logger.fatal
                          "the directive address %a does not resolve to a \
                           unique value"
                          Dba_printer.Ascii.pp_bl_term loc))
                state script)
            state files
    in
    List.iter
      (fun (addr, label, directive) -> add_directive addr label directive)
      !directives;
    (!start, state)

  let () =
    let filename = Kernel_options.ExecFile.get () in
    Logger.debug "Running SSE on %s" filename;
    let entry, state = initialize_state () in
    let start =
      Fiber.Hook { addr = Virtual_address.zero; info = "start"; succ = entry }
    in
    if I.Htbl.length env.tasks = 0 then
      Logger.warning "Nothing to reach: halting..."
    else
      try
        Sys.catch_break true;
        Option.iter
          (fun timeout ->
            Sys.set_signal Sys.sigalrm
              (Sys.Signal_handle
                 (fun s ->
                   assert (s = Sys.sigalrm);
                   raise_notrace Halt));
            ignore (Unix.alarm timeout))
          (Timeout.get_opt ());
        Screen.init ();
        exec Default 0 0 ~max_depth:(MaxDepth.get ()) state start entry
      with
      | Halt | Sys.Break -> halt ()
      | err ->
          Screen.release ();
          raise err
end
