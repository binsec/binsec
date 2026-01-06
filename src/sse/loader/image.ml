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

open Basic_types.Integers
module StrMap = Basic_types.String.Map
module StrTbl = Basic_types.String.Htbl
module AttrMap = Dba.Var.Tag.Attribute.Map

type buffer =
  | Zero
  | Data of { offset : int; len : int; value : Loader_types.buffer }

let crop_buffer : lo:Z.t -> hi:Z.t -> buffer -> buffer =
 fun ~lo ~hi buffer ->
  match buffer with
  | Zero -> Zero
  | Data { offset; value; _ } ->
      let lo = Z.to_int lo and hi = Z.to_int hi in
      Data { offset = offset + lo; len = hi - lo + 1; value }

let map_content : buffer Zmap.t -> buffer Zmap.t -> buffer Zmap.t =
  let stich : buffer Zmap.item -> buffer Zmap.item -> buffer option =
   fun (Item { elt = elt0; _ }) (Item { elt = elt1; _ }) ->
    match (elt0, elt1) with
    | Zero, Zero -> Some Zero
    | ( Data { offset = offset0; len = len0; value = value0 },
        Data { offset = offset1; len = len1; value = value1 } )
      when value0 == value1 && offset0 + len0 = offset1 ->
        Some (Data { offset = offset0; len = len0 + len1; value = value0 })
    | Zero, Data _ | Data _, Zero | Data _, Data _ -> None
  in
  fun mapping content ->
    Zmap.union_left ~stich ~crop:crop_buffer mapping content

let content_reader :
    Virtual_address.t ->
    Z.t ->
    ?endianness:Machine.endianness ->
    buffer Zmap.t ->
    Virtual_address.t Reader.t =
  let offset : Virtual_address.t -> int -> Virtual_address.t =
   fun addr n -> Virtual_address.add_int n addr
  and get :
      buffer Zmap.item_opt ref -> buffer Zmap.t -> Virtual_address.t -> char =
   fun cache map addr ->
    let idx = Virtual_address.to_bigint addr in
    match !cache with
    | Item { lo; hi; elt } when Z.leq lo idx && Z.leq idx hi -> (
        match elt with
        | Zero -> '\x00'
        | Data { offset; value; _ } ->
            Char.unsafe_chr
              (Bigarray.Array1.get value (Z.to_int (Z.sub idx lo) + offset)))
    | Item _ | Empty -> (
        let last_buffer = Zmap.find_opt idx map in
        match last_buffer with
        | Empty -> raise Not_found
        | Item { elt = Zero; _ } -> '\x00'
        | Item { lo; elt = Data { offset; value; _ }; _ } ->
            cache := last_buffer;
            Char.unsafe_chr
              (Bigarray.Array1.get value (Z.to_int (Z.sub idx lo) + offset)))
  in
  fun start dim ?endianness content ->
    Reader.create ~offset
      ~get:(get (ref Zmap.none))
      ?endianness ~start
      ~stop:(Virtual_address.add_bigint dim start)
      content

type protection = R | RW | RX | RWX

let map_protection : protection Zmap.t -> protection Zmap.t -> protection Zmap.t
    =
  let stich : protection Zmap.item -> protection Zmap.item -> protection option
      =
   fun (Item { elt = elt0; _ }) (Item { elt = elt1; _ }) ->
    if elt0 = elt1 then Some elt0 else None
  in
  fun mapping protection -> Zmap.union_left ~stich mapping protection

let update_append : 'a -> 'a list option -> 'a list option =
 fun x list_opt -> Some (x :: Option.value ~default:[] list_opt)

type symbol = { base : Virtual_address.t; name : string; origin : string }

type section = {
  base : Virtual_address.t;
  name : string;
  origin : string;
  symbols : symbol Zmap.t;
}

type t = {
  content : buffer Zmap.t; (* set of initialized data *)
  protection : protection Zmap.t; (* set of mappings *)
  symbols : (Z.t * string) list AttrMap.t StrMap.t;
      (* set of symbol attributes*)
  layout : section Zmap.t; (* reverse memory paving *)
}

type layout_cache = {
  mutable last_section : section Zmap.item_opt;
  mutable last_symbol : symbol Zmap.item_opt;
}

let layout_with_cache :
    t -> (Virtual_address.t -> section) * (Virtual_address.t -> symbol) =
  let rev_section :
      section Zmap.t -> layout_cache -> Virtual_address.t -> section =
   fun layout cache addr ->
    let idx = Virtual_address.to_bigint addr in
    match cache.last_section with
    | Item { lo; hi; elt } when Z.leq lo idx && Z.leq idx hi -> elt
    | Item _ | Empty -> (
        let last_section = Zmap.find_opt idx layout in
        match last_section with
        | Empty -> raise Not_found
        | Item { elt; _ } ->
            cache.last_section <- last_section;
            elt)
  in
  let rev_symbol : section Zmap.t -> layout_cache -> Virtual_address.t -> symbol
      =
   fun layout cache addr ->
    let idx = Virtual_address.to_bigint addr in
    match cache.last_symbol with
    | Item { lo; hi; elt } when Z.leq lo idx && Z.leq idx hi -> elt
    | Item _ | Empty -> (
        match rev_section layout cache addr with
        | { symbols; _ } -> (
            let last_symbol = Zmap.find_opt idx symbols in
            match last_symbol with
            | Empty -> raise Not_found
            | Item { elt; _ } ->
                cache.last_symbol <- last_symbol;
                elt))
  in
  fun { layout; _ } ->
    let cache = { last_section = Zmap.none; last_symbol = Zmap.none } in
    (rev_section layout cache, rev_symbol layout cache)

let empty =
  {
    content = Zmap.empty;
    protection = Zmap.empty;
    symbols = StrMap.empty;
    layout = Zmap.empty;
  }

let generic_load : string -> Loader.Img.t -> t =
 fun path source ->
  let value = Loader.Img.buffer source in
  Array.fold_left
    (fun ({ symbols; _ } as image) symbol ->
      let name = Loader.Symbol.name symbol in
      if String.equal name String.empty then image
      else
        {
          image with
          symbols =
            StrMap.add name
              (AttrMap.singleton Value
                 [
                   (Virtual_address.to_bigint (Loader.Symbol.value symbol), path);
                 ])
              symbols;
        })
    (Array.fold_left
       (fun ({ content; protection; symbols; layout } as image) section ->
         if Loader.Section.has_flag Read section then
           match (Loader.Section.pos section, Loader.Section.size section) with
           | { virt = base; raw = offset }, { virt = size; raw = len } ->
               let lo = Virtual_address.to_bigint base in
               let hi = Z.add lo (Z.of_int (len - 1)) in
               let mapping =
                 if 0 < len then
                   Zmap.singleton ~lo ~hi (Data { offset; len; value })
                 else Zmap.empty
               in
               let mapping, hi =
                 if Z.lt (Z.of_int len) size then
                   let lo = Z.add lo (Z.of_int len)
                   and hi = Z.add lo (Z.pred size) in
                   (Zmap.union_left mapping (Zmap.singleton ~lo ~hi Zero), hi)
                 else (mapping, hi)
               in
               let content = map_content mapping content in
               let mapping =
                 Zmap.singleton ~lo ~hi
                   (match
                      ( Loader.Section.has_flag Write section,
                        Loader.Section.has_flag Exec section )
                    with
                   | false, false -> R
                   | true, false -> RW
                   | false, true -> RX
                   | true, true -> RWX)
               in
               let protection = map_protection mapping protection in
               let symbols, layout =
                 let name = Loader.Section.name section in
                 if String.equal name String.empty then (symbols, layout)
                 else
                   let attributes =
                     AttrMap.add Value
                       [ (lo, path) ]
                       (AttrMap.add Size
                          [ (size, path) ]
                          (AttrMap.singleton Last [ (hi, path) ]))
                   in
                   let symbols = StrMap.add name attributes symbols in
                   let mapping =
                     Zmap.singleton ~lo ~hi
                       { base; name; origin = path; symbols = Zmap.empty }
                   in
                   let layout = Zmap.union_left mapping layout in
                   (symbols, layout)
               in
               { content; protection; symbols; layout }
         else image)
       empty
       (Loader.Img.sections source))
    (Loader.Img.symbols source)

let elf_map_synthetic_symtab :
    string -> Loader_elf.Img.t -> Virtual_address.t -> t -> t =
 fun path source at image ->
  List.fold_left
    (fun ({ symbols; layout; _ } as image) (value, name) ->
      let value = Virtual_address.to_bigint (Virtual_address.add at value) in
      let symbols =
        StrMap.update name
          (fun attributes ->
            Some
              (AttrMap.update Plt
                 (update_append (value, path))
                 (Option.value ~default:AttrMap.empty attributes)))
          symbols
      in
      let layout =
        match Zmap.find value layout with
        | Item { lo; hi; elt = { symbols; _ } as section } ->
            Zmap.union_left
              (Zmap.singleton ~lo ~hi
                 {
                   section with
                   symbols =
                     Zmap.union_left
                       (Zmap.singleton ~lo:value ~hi:value
                          {
                            base = Virtual_address.of_bigint value;
                            name = name ^ "@plt";
                            origin = path;
                          })
                       symbols;
                 })
              layout
        | exception Not_found -> layout
      in
      { image with symbols; layout })
    image
    (Loader_elf.Utils.synthetic_symtab source)

let elf_map_symtab : string -> Loader_elf.Img.t -> Virtual_address.t -> t -> t =
 fun path source at image ->
  let at = Virtual_address.to_bigint at in
  Array.fold_left
    (fun ({ symbols; layout; _ } as image) symbol ->
      match Loader_elf.Symbol.header symbol with
      | {
       sh = SEC { flags; _ };
       kind = NOTYPE | FUNC | OBJECT;
       name;
       value = addr;
       size;
       _;
      }
        when (not (String.equal name String.empty))
             && Loader_elf.Shdr.SHF.is flags ALLOC ->
          let value = Z.add at (Virtual_address.to_bigint addr) in
          let last = Z.add value (Z.of_int (size - 1)) in
          let symbols =
            StrMap.update name
              (fun attributes ->
                let attributes =
                  Option.value ~default:AttrMap.empty attributes
                in
                let attributes =
                  AttrMap.update Value (update_append (value, path)) attributes
                in
                let attributes =
                  if 0 < size then
                    AttrMap.update Size
                      (update_append (Z.of_int size, path))
                      (AttrMap.update Last
                         (update_append (last, path))
                         attributes)
                  else attributes
                in
                Some attributes)
              symbols
          in
          let layout =
            if 0 < size then
              match Zmap.find value layout with
              | Item { lo; hi; elt = { symbols; _ } as section } ->
                  Zmap.union_left
                    (Zmap.singleton ~lo ~hi
                       {
                         section with
                         symbols =
                           Zmap.union_left
                             (Zmap.singleton ~lo:value ~hi:last
                                {
                                  base = Virtual_address.of_bigint value;
                                  name;
                                  origin = path;
                                })
                             symbols;
                       })
                    layout
              | exception Not_found ->
                  Logger.fatal "can not find section for symbol %s (%s)" name
                    (Z.format "%#x" value)
            else layout
          in
          { image with symbols; layout }
      | _ -> image)
    (Array.fold_left
       (fun ({ symbols; layout; _ } as image) section ->
         match Loader_elf.Section.header section with
         | { name; addr; size; flags; _ } ->
             if String.equal name String.empty || size = 0 then image
             else
               let lo = Z.add at (Virtual_address.to_bigint addr) in
               let size = Z.of_int size
               and hi = Z.add lo (Z.of_int (size - 1)) in
               let symbols =
                 StrMap.update name
                   (fun attributes ->
                     Some
                       (AttrMap.update Value
                          (update_append (lo, path))
                          (AttrMap.update Size
                             (update_append (size, path))
                             (AttrMap.update Last
                                (update_append (hi, path))
                                (Option.value ~default:AttrMap.empty attributes)))))
                   symbols
               in
               if Loader_elf.Shdr.SHF.is flags ALLOC then
                 let mapping =
                   Zmap.singleton ~lo ~hi
                     {
                       base = Virtual_address.of_bigint lo;
                       name;
                       origin = path;
                       symbols = Zmap.empty;
                     }
                 in
                 let layout = Zmap.union_left mapping layout in
                 { image with symbols; layout }
               else { image with symbols })
       image
       (Loader_elf.Img.sections source))
    (Loader_elf.Img.symbols source)

let elf_buildid_path : ?dir:string -> Loader_elf.Img.t -> string option =
 fun ?(dir = "/usr/lib/debug/") source ->
  let endianness =
    match Loader_elf.Img.header source with { ident = { data; _ }; _ } -> data
  in
  Array.find_map
    (fun section ->
      match Loader_elf.Section.header section with
      | { kind = NOTE; _ } -> (
          let cursor =
            Reader.of_bigarray ~endianness
              (Loader_elf.Img.content source section)
          in
          match Loader_elf.Note.read cursor with
          | { name = "GNU"; kind = 3; offset; size } ->
              Reader.set_pos cursor offset;
              let build_id = Reader.Read.bytes cursor size in
              let idx = String_utils.to_hex (String.sub build_id 0 1)
              and basename =
                String_utils.to_hex
                  (String.sub build_id 1 (String.length build_id - 1))
                ^ ".debug"
              in
              Some
                (Filename.concat dir
                   (Filename.concat ".build-id" (Filename.concat idx basename)))
          | _ -> None)
      | _ -> None)
    (Loader_elf.Img.sections source)

let elf_debug_locations :
    ?dir:string -> string -> Loader_elf.Img.t -> string list =
 fun ?(dir = "/usr/lib/debug/") path source ->
  let other_locations =
    let debug = path ^ ".debug"
    and basename = Filename.basename path
    and dirname = Filename.dirname path in
    [
      debug;
      Filename.concat dirname (Filename.concat ".debug" (basename ^ ".debug"));
      Filename.concat dir debug;
    ]
  in
  match elf_buildid_path ~dir source with
  | None -> other_locations
  | Some loc -> loc :: other_locations

let elf_map_symtab_with_debug :
    fs:(string -> Loader_types.buffer) ->
    string ->
    Loader_elf.Img.t ->
    Virtual_address.t ->
    t ->
    t =
 fun ~fs path source at image ->
  elf_map_synthetic_symtab path source at
    (elf_map_symtab path
       (Option.value ~default:source
          (List.find_map
             (fun path ->
               Logger.debug "trying to load debug symbols at %s" path;
               match fs path with
               | exception Not_found -> None
               | buf -> Some (Loader_elf.load buf))
             (elf_debug_locations path source)))
       at image)

let elf_load_segments : Loader_types.buffer -> Loader_elf.Phdr.t array -> t =
 fun value segments ->
  Array.fold_left
    (fun ({ content; protection; _ } as image)
         ({ flags; offset; vaddr; filesz; memsz; _ } : Loader_elf.Phdr.t) ->
      let memsz = Uint64.to_bigint memsz in
      if Loader_elf.Phdr.PHF.is flags R && Z.lt Z.zero memsz then
        let lo = Virtual_address.to_bigint vaddr in
        let hi = Z.add lo (Z.of_int (filesz - 1)) in
        let mapping =
          if 0 < filesz then
            Zmap.singleton ~lo ~hi (Data { offset; len = filesz; value })
          else Zmap.empty
        in
        let mapping, hi =
          if Z.lt (Z.of_int filesz) memsz then
            let lo = Z.add lo (Z.of_int filesz)
            and hi = Z.add lo (Z.pred memsz) in
            (Zmap.union_left mapping (Zmap.singleton ~lo ~hi Zero), hi)
          else (mapping, hi)
        in
        let content = map_content mapping content in
        let mapping =
          Zmap.singleton ~lo ~hi
            (match
               (Loader_elf.Phdr.PHF.is flags W, Loader_elf.Phdr.PHF.is flags X)
             with
            | false, false -> R
            | true, false -> RW
            | false, true -> RX
            | true, true -> RWX)
        in
        let protection = map_protection mapping protection in
        { image with content; protection }
      else image)
    empty segments

let elf_load_sections : Loader_types.buffer -> Loader_elf.Section.t array -> t =
 fun value sections ->
  Array.fold_left
    (fun ({ content; protection; _ } as image) section ->
      match Loader_elf.Section.header section with
      | { addr; offset; size; flags; kind; _ } ->
          if Loader_elf.Shdr.SHF.is flags ALLOC then
            let lo = Virtual_address.to_bigint addr in
            let hi = Z.add lo (Z.of_int (size - 1)) in
            let mapping =
              Zmap.singleton ~lo ~hi
                (match kind with
                | NOBITS -> Zero
                | _ -> Data { offset; len = size; value })
            in
            let content = map_content mapping content in
            let mapping =
              Zmap.singleton ~lo ~hi
                (match
                   ( Loader_elf.Shdr.SHF.is flags WRITE,
                     Loader_elf.Shdr.SHF.is flags EXECINSTR )
                 with
                | false, false -> R
                | true, false -> RW
                | false, true -> RX
                | true, true -> RWX)
            in
            let protection = map_protection mapping protection in
            { image with content; protection }
          else image)
    empty sections

let elf_load :
    fs:(string -> Loader_types.buffer) -> string -> Loader_elf.Img.t -> t =
 fun ~fs path source ->
  let value = Loader_elf.Img.buffer source in
  elf_map_symtab_with_debug ~fs path source Virtual_address.zero
    (match Loader_elf.Img.header source with
    | { kind = EXEC | DYN; _ } ->
        elf_load_segments value (Loader_elf.program_headers source)
    | _ -> elf_load_sections value (Loader_elf.Img.sections source))

let elf_revmap_jmprel :
    string -> Loader_elf.Img.t -> Virtual_address.t -> t -> t =
 fun path source base image ->
  let sizeof_symbol, endianness =
    match Loader_elf.Img.header source with
    | { ident = { kind = `x32; data; _ }; _ } -> (4, data)
    | { ident = { kind = `x64; data; _ }; _ } -> (8, data)
  in
  let isa = Loader_elf.Img.arch source in
  List.fold_left
    (fun ({ symbols; _ } as image)
         { Loader_elf.Rel.offset; kind; symbol = { name; _ }; addend } ->
      if
        (not (Loader_elf.Utils.is_jump_slot isa kind))
        || String.equal name String.empty
      then image
      else
        let addend = Option.value ~default:Z.zero addend in
        let reader =
          content_reader
            (Virtual_address.add base offset)
            (Z.of_int sizeof_symbol) ~endianness image.content
        in
        let value =
          Z.sub
            (Bitvector.value_of (Reader.Read.read reader sizeof_symbol))
            addend
        in
        let symbols =
          StrMap.update name
            (fun attributes ->
              Some
                (AttrMap.update Value
                   (update_append (value, path))
                   (Option.value ~default:AttrMap.empty attributes)))
            symbols
        in
        Logger.debug ~level:3 "%a %s" Bitvector.pp_hex_or_bin
          (Bitvector.create value (8 * sizeof_symbol))
          name;
        { image with symbols })
    image
    (Loader_elf.Utils.jmprel source)

let elf_core_fix_segment_permissions :
    Loader_elf.Img.t -> Virtual_address.t -> t -> t =
 fun source at image ->
  Array.fold_left
    (fun ({ protection; _ } as image)
         ({ flags; vaddr; memsz; _ } : Loader_elf.Phdr.t) ->
      let memsz = Uint64.to_bigint memsz in
      if Loader_elf.Phdr.PHF.is flags R && Z.lt Z.zero memsz then
        let lo = Virtual_address.to_bigint (Virtual_address.add at vaddr) in
        let hi = Z.add lo (Z.pred memsz) in
        let mapping =
          Zmap.singleton ~lo ~hi
            (match
               (Loader_elf.Phdr.PHF.is flags W, Loader_elf.Phdr.PHF.is flags X)
             with
            | false, false -> R
            | true, false -> RW
            | false, true -> RX
            | true, true -> RWX)
        in
        let protection =
          map_protection protection
            mapping (* keep the core segment if overlapping *)
        in
        { image with protection }
      else image)
    image
    (Loader_elf.program_headers source)

let elf_load_core : fs:(string -> Loader_types.buffer) -> Loader_elf.Img.t -> t
    =
 fun ~fs source ->
  let value = Loader_elf.Img.buffer source in
  let image = elf_load_segments value (Loader_elf.program_headers source) in
  let files = Loader_elf.files source in
  let contents = StrTbl.create (Array.length files) and pending = ref [] in
  let image =
    Array.fold_left
      (fun ({ content; _ } as image)
           { Loader_elf.addresses = { lo; hi }; offset; name = fname } ->
        let value =
          try StrTbl.find contents fname
          with Not_found -> (
            let value =
              try fs fname
              with Not_found -> Logger.fatal "Unable to open file %s" fname
            in
            match Loader.load value with
            | ELF source -> (
                match Loader_elf.Img.header source with
                | { kind = EXEC | DYN; _ } -> (
                    match
                      Array.find_opt
                        (fun ({ offset = fileoff; filesz; _ } :
                               Loader_elf.Phdr.t) ->
                          fileoff <= offset && offset < fileoff + filesz)
                        (Loader_elf.program_headers source)
                    with
                    | None -> value
                    | Some { offset = fileoff; vaddr; _ } ->
                        let base =
                          Virtual_address.create
                            (Virtual_address.diff
                               (Virtual_address.add_int (fileoff - offset) lo)
                               vaddr)
                        in
                        Logger.debug "%a :: %a-%a %08x %s" Virtual_address.pp
                          base Virtual_address.pp lo Virtual_address.pp hi
                          offset fname;
                        StrTbl.add contents fname value;
                        pending := (fname, source, base) :: !pending;
                        value)
                | _ -> value)
            | Raw _ | PE _ | TI83 _ -> value)
        in
        let mapping =
          Zmap.singleton
            ~lo:(Virtual_address.to_bigint lo)
            ~hi:(Z.pred (Virtual_address.to_bigint hi))
            (Data
               {
                 offset;
                 len =
                   min
                     (Virtual_address.diff hi lo)
                     (Bigarray.Array1.dim value - offset);
                 value;
               })
        in
        {
          image with
          content =
            map_content content
              mapping (* keep the core segment if overlapping *);
        })
      image files
  in
  List.fold_left
    (fun image (fname, source, base) ->
      elf_revmap_jmprel fname source base
        (elf_map_symtab_with_debug ~fs fname source base
           (elf_core_fix_segment_permissions source base image)))
    image !pending

let load : fs:(string -> Loader_types.buffer) -> string -> Loader.Img.t -> t =
 fun ~fs path source ->
  match source with
  | ELF img -> (
      match Loader_elf.Img.header img with
      | { kind = CORE; _ } -> elf_load_core ~fs img
      | _ -> elf_load ~fs path img)
  | _ -> generic_load path source
