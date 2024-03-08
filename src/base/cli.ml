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

module L = Logger.Make (struct
  let name = "cli"
end)

(* Possible actions for a command line argument.contents.
   Either you parse one argument or you do not.
   One could imagine a generalization of that to *n* arguments
*)
type action = Scan of (string -> unit) | Unit of (unit -> unit)

(* Generic helper functions for modules below *)
let prefix s = if s = "" then s else "-" ^ s

let cmp_plugin =
  let rec cmp switch plugin ls lp i =
    if i = lp then i = ls || String.get switch (i + 1) = '-'
    else
      i <> ls
      &&
      match (String.get switch (i + 1), String.get plugin i) with
      | '-', '_' -> cmp switch plugin ls lp (i + 1)
      | s, p -> s = p && cmp switch plugin ls lp (i + 1)
  in
  fun switch plugin ->
    cmp switch plugin (String.length switch - 1) (String.length plugin) 0

let mk_opt path = List.fold_left (fun acc name -> prefix name ^ acc) "" path

(* Whenever there are values, the line will be automatically broken by
   [pp_options] (see below).
   Therefore, they are excluded from the computation of the max [key] length.
*)
let max l =
  let rec loop max = function
    | [] -> max
    | (name, values, _) :: tl ->
        if String.length values <> 0 then loop max tl
        else
          let v = String.length name in
          if v > max then loop v tl else loop max tl
  in
  loop 0 l

let pp_options ppf options =
  let open Format in
  let extra_indent = 2 in
  let indent_newline = 8 in
  let key_indent = extra_indent + max options in
  let key_fmt =
    Scanf.format_from_string ("%-" ^ string_of_int key_indent ^ "s") "%s"
  in
  let fmt = "@[<h>" ^^ key_fmt ^^ "@]@ @[<h>%s@]" in
  List.iter
    (fun (option_name, values, docstring) ->
      let box_open =
        if String.length values = 0 then pp_open_hovbox else pp_open_vbox
      in
      box_open ppf indent_newline;
      fprintf ppf fmt (option_name ^ values) docstring;
      pp_close_box ppf ();
      pp_print_cut ppf ())
    options

module Values = struct
  open Format

  type t = One_of of string list | Unconstrained

  let one_of l = One_of l

  let pp ppf = function
    | One_of strs ->
        fprintf ppf " {";
        let rec pp_aux = function
          | [] -> ()
          | [ s ] -> pp_print_string ppf s
          | s :: ss ->
              pp_print_string ppf s;
              pp_print_string ppf "|";
              pp_aux ss
        in
        pp_aux strs;
        fprintf ppf "}"
    | Unconstrained -> ()
end

module S = String
module H = Basic_types.String.Htbl

module Argv = struct
  type t = { key : Arg.key; doc : Arg.doc; spec : action; values : Values.t }
  type _args = t list

  let _compare a1 a2 = String.compare a1.key a2.key

  let create ?(values = Values.Unconstrained) ~key ~doc ~spec () =
    { key; doc; spec; values }

  let list_of_args a =
    H.fold
      (fun k v l ->
        let values =
          Values.pp Format.str_formatter v.values;
          Format.flush_str_formatter ()
        in
        (k, values, v.doc) :: l)
      a []
    |> List.sort compare
end

exception Scan_error of string * Values.t

let scan_error got expected = raise (Scan_error (got, expected))

module Cli_spec = struct
  type t = Argv.t

  let align doc =
    if String.length doc = 0 then "[undocumented option]" else String.trim doc

  let create ~values ~key ~doc ~spec =
    let doc = align @@ String.capitalize_ascii doc in
    Argv.create ~values ~key ~doc ~spec ()

  let simple ~key ~doc ~spec =
    let values = Values.Unconstrained in
    create ~values ~key ~doc ~spec
end

module rec Cli_node : sig
  type t = private { title : string; key : Arg.key; cmds : Cli.t }

  val create : title:string -> key:Arg.key -> t
  val insert : t -> Cli_spec.t -> unit
  val args : t -> Argv.t H.t
  val pp : Format.formatter -> t -> unit
end = struct
  type t = { title : string; key : Arg.key; cmds : Cli.t }

  let create ~title ~key = { title; key; cmds = Cli.create () }

  (* We do not allow more nesting *)
  let insert t csp = Cli.insert_leaf t.cmds csp

  let args t =
    let hargs = H.create (Cli.size t.cmds) in
    let rec loop pfx h =
      H.iter
        (fun _ v ->
          let open Cli in
          match v with
          | Leaf ({ Argv.key; _ } as action) ->
              let opt_name = mk_opt (key :: pfx) in
              H.add hargs opt_name { action with Argv.key = opt_name }
          | Node { Cli_node.title = _; Cli_node.key; Cli_node.cmds } ->
              loop (key :: pfx) cmds)
        h
    in
    loop [ t.key ] t.cmds;
    hargs

  let pp ppf t =
    let pp_args ppf () =
      let args = args t |> Argv.list_of_args in
      pp_options ppf args
    in
    Format.fprintf ppf "@[<v 4>* %s options@ @ %a@]"
      (String.capitalize_ascii t.title)
      pp_args ()
end

and Cli : sig
  type elt = private Leaf of Cli_spec.t | Node of Cli_node.t
  type t = elt H.t

  val args : unit -> Argv.t H.t
  val cli : t (* THE central cli *)
  val create : unit -> t
  val insert_leaf : t -> Cli_spec.t -> unit
  val insert_node : t -> Cli_node.t -> unit
  val pp : Format.formatter -> unit -> unit
  val size : t -> int
end = struct
  type elt = Leaf of Cli_spec.t | Node of Cli_node.t
  and t = elt H.t

  let create () = H.create 11
  let cli = create ()
  let size t = H.length t

  let keys () =
    H.fold (fun k _ acc -> k :: acc) cli [] |> List.sort String.compare

  let _items () = H.fold (fun _ v acc -> v :: acc) cli []
  let insert_leaf t csp = H.add t csp.Argv.key (Leaf csp)
  let insert_node t csp = H.add t csp.Cli_node.key (Node csp)

  let args () =
    let hargs = H.create 17 in
    let rec loop pfx h =
      H.iter
        (fun _ v ->
          let open Cli in
          match v with
          | Leaf ({ Argv.key; _ } as action) ->
              let opt_name = mk_opt (key :: pfx) in
              H.add hargs opt_name action
          | Node { Cli_node.title = _; Cli_node.key; Cli_node.cmds } ->
              loop (key :: pfx) cmds)
        h
    in
    loop [] cli;
    hargs

  let is_leaf h k =
    match H.find h k with
    | Leaf _ -> true
    | Node _ -> false
    | exception Not_found -> assert false

  let pp ppf () =
    let _leaves, nodes = List.partition (is_leaf cli) (keys ()) in
    let pure_nodes =
      List.map
        (fun k ->
          match H.find cli k with Leaf _ -> assert false | Node n -> n)
        nodes
    in
    let open Format in
    pp_open_vbox ppf 0;
    List.iter
      (fun no ->
        Cli_node.pp ppf no;
        pp_print_cut ppf ())
      pure_nodes;
    pp_print_cut ppf ();
    pp_print_cut ppf ();
    pp_close_box ppf ()
end

module type S = sig
  val is_enabled : unit -> bool

  module Logger : Logger.S
end

(** Module types for command-line parameters *)
module type GENERIC = sig
  type t

  val set : t -> unit
  val get : unit -> t
  val is_set : unit -> bool
  val is_default : unit -> bool
end

module type GENERIC_OPT = sig
  include GENERIC

  val get_opt : unit -> t option
end

module type BOOLEAN = GENERIC with type t = bool

(** {5 Integer parameters} *)

module type INTEGER = GENERIC with type t = int
module type INTEGER_SET = GENERIC with type t = Basic_types.Int.Set.t
module type INTEGER_LIST = GENERIC with type t = int list
module type INTEGER_OPT = GENERIC_OPT with type t = int

(** {5 Floating point parameters} *)

module type FLOAT = GENERIC with type t = float
module type FLOAT_SET = GENERIC with type t = Basic_types.Float.Set.t
module type FLOAT_LIST = GENERIC with type t = float list
module type FLOAT_OPT = GENERIC_OPT with type t = float

(** {5 String parameters} *)

module type STRING = GENERIC with type t = string
module type STRING_OPT = GENERIC_OPT with type t = string
module type STRING_SET = GENERIC with type t = Basic_types.String.Set.t
module type STRING_LIST = GENERIC with type t = string list

module type CLI_DECL = sig
  val name : string
  val doc : string
end

(* Helper module *)
module Listify (S : Sigs.STR_INJECTIBLE) = struct
  type t = S.t list

  let default = []
  let of_string s = String_utils.cli_split s |> List.map S.of_string
  let default_str = None
end

module type DECL = sig
  val name : string
  val shortname : string
end

module type DEFAULTED_CLI_DECL = sig
  include CLI_DECL

  type t

  val default : t
end

module type DETAILED_CLI_DECL = sig
  include DEFAULTED_CLI_DECL
  include Sigs.STR_INJECTIBLE with type t := t

  val default_str : string option
end

module type Cli_sig = sig
  module Logger : Logger.S
  module Debug_level : INTEGER
  module Loglevel : STRING
  module Quiet : BOOLEAN

  module Builder : sig
    (** A very generic functor that lets you handle cases that are not provided
        otherwise. Use it only as last resort.
     *)
    module Any (P : DETAILED_CLI_DECL) : GENERIC with type t = P.t

    module Any_opt (P : sig
      include CLI_DECL
      include Sigs.STR_INJECTIBLE
    end) : GENERIC_OPT with type t = P.t

    (** {4 Boolean functors}*)
    module Boolean (P : sig
      include CLI_DECL

      val default : bool
    end) : BOOLEAN
    (** Generic boolean option to which you give a [default] value *)

    (** An option that defaults to [false]. *)
    module False (P : CLI_DECL) : BOOLEAN

    (** An option that defaults to [true].
        The provided command-line switch
        automatically add a [no-] prefix to your option name.
     *)
    module No (P : CLI_DECL) : BOOLEAN

    (** {4 Integer functors}*)
    module Integer (P : sig
      include CLI_DECL

      val default : int
    end) : INTEGER

    module Zero (P : CLI_DECL) : INTEGER
    module Integer_set (P : CLI_DECL) : INTEGER_SET
    module Integer_list (P : CLI_DECL) : INTEGER_LIST
    module Integer_option (P : CLI_DECL) : INTEGER_OPT

    (** {4 Floating point functors}*)
    module Float (P : sig
      include CLI_DECL

      val default : float
    end) : FLOAT

    module Float_set (P : CLI_DECL) : FLOAT_SET
    module Float_list (P : CLI_DECL) : FLOAT_LIST
    module Float_option (P : CLI_DECL) : FLOAT_OPT

    (** {4 String functors}*)
    module String (P : sig
      include CLI_DECL

      val default : string
    end) : STRING

    module String_choice (P : sig
      include CLI_DECL

      val default : string
      val choices : string list
    end) : STRING

    module String_option (P : CLI_DECL) : STRING_OPT
    module String_set (P : CLI_DECL) : STRING_SET
    module String_list (P : CLI_DECL) : STRING_LIST

    (** {4 Variant functors} *)
    module Variant_choice (P : sig
      include CLI_DECL

      type t

      val to_string : t -> string
      val of_string : string -> t
      val choices : string list
      val default : t
    end) : GENERIC with type t = P.t
    (** Functor to map a string choice --- i.e., just one out of a set of
        possible value --- into a variant type.
     *)

    (** Like [Variant_choice] but with automatically generated [to_string] and
        [of_string] function from [assoc_map].
     *)
    module Variant_choice_assoc (P : sig
      include CLI_DECL

      type t

      val assoc_map : (string * t) list
      val default : t
    end) : GENERIC with type t = P.t

    module Variant_list (P : sig
      include CLI_DECL
      include Sigs.STR_INJECTIBLE
    end) : GENERIC with type t = P.t list
  end
end

module Generic_make (D : DECL) = struct
  let sep = ","
  let cli_space = Cli_node.create ~key:D.shortname ~title:D.name
  let unsafe_extend = Cli_node.insert cli_space

  let extend cspec =
    let open Argv in
    assert (cspec.key <> "");
    unsafe_extend cspec

  (* Hard-coded convention: kernel options do not have a prefix.
     Hence the kernel is a "regular" node with the empty string prefix.
  *)
  let is_kernel = D.shortname = ""
  let name = if is_kernel then "kernel" else D.shortname

  module Args = struct
    let get () = Cli_node.args cli_space
    let get_list () = Argv.list_of_args @@ get ()
  end

  (* Add the logging channels for this command line node
     Use shortname as prefix to logging messages.
  *)
  module Logger = Logger.Make (struct
    include D

    let name = if is_kernel then "kernel" else shortname
  end)

  module Debug_level = struct
    type t = int

    let default = Logger.get_debug_level ()
    let is_set = ref false

    let set v =
      is_set := true;
      Logger.set_debug_level v

    let get () = Logger.get_debug_level ()
    let is_set () = !is_set
    let is_default () = get () = default

    let doc = Printf.sprintf "Set %s debug level [%d]" name 0
    and key = "debug-level"
    and spec = Scan (fun s -> int_of_string s |> set)

    let cspec = Cli_spec.simple ~key ~doc ~spec
    let _ = extend cspec
  end

  module Loglevel = struct
    type t = string

    let default = "info"
    let is_set = ref false
    let value = ref default

    let set v =
      is_set := true;
      value := v;
      Logger.set_log_level v

    let get () = !value
    let is_set () = !is_set
    let is_default () = !value = default

    let doc =
      Printf.sprintf
        "Display %s log messages only above or equal this level [%s]" name
        default

    let values = [ "info"; "debug"; "warning"; "error"; "fatal"; "result" ]
    let key = "loglevel"
    let spec = Scan set
    let cspec = Cli_spec.create ~values:(Values.one_of values) ~key ~doc ~spec
    let _ = extend cspec
  end

  module Quiet = struct
    type t = bool

    (* FIXME : for now, there is no way to come back to previous state *)
    let value = ref false

    let set v =
      value := v;
      if v then Logger.quiet ()

    let get () = !value
    let is_set () = !value
    let is_default () = not !value

    let key = "quiet"
    and spec = Unit (fun _ -> set true)
    and doc = Printf.sprintf "Quiet all channels for %s" name

    let cspec = Cli_spec.simple ~key ~doc ~spec
    let _ = extend cspec
  end

  let usage () =
    Format.eprintf "@[<v 2>* Help for %s (%s)@ @ %a@]@." D.name D.shortname
      pp_options (Args.get_list ())

  let add_help () =
    if is_kernel then (
      (* Add hook for -help *)
      let key = "help" in
      let spec =
        Unit
          (fun () ->
            Plugins.Plugins.Plugins.load_all ();
            Cli.pp Format.std_formatter ();
            exit 0)
      in
      let doc = "Display full option list" in
      let cspec = Cli_spec.simple ~key ~doc ~spec in
      extend cspec;
      (* Add hook for --help *)
      let cspec = Cli_spec.simple ~key:"-help" ~doc ~spec in
      extend cspec;
      (* Add specific hook for -kernel-help *)
      let key = "kernel-help" in
      let spec =
        Unit
          (fun () ->
            usage ();
            exit 0)
      in
      let doc = Printf.sprintf "Display options list for %s" name in
      let cspec = Cli_spec.simple ~key ~doc ~spec in
      extend cspec)
    else
      let key = "help" in
      let spec =
        Unit
          (fun () ->
            usage ();
            exit 0)
      in
      let doc = Printf.sprintf "Display options list for %s" name in
      let cspec = Cli_spec.simple ~key ~doc ~spec in
      extend cspec

  let _ =
    add_help ();
    Logger.debug ~level:5 "Help added";
    Cli.insert_node Cli.cli cli_space

  module Builder = struct
    module Gen (P : sig
      include DETAILED_CLI_DECL

      val values : Values.t
    end) =
    struct
      type t = P.t

      let default = P.default
      let is_set = ref false
      let value = ref P.default

      let set v =
        is_set := true;
        value := v

      let get () = !value
      let is_set () = !is_set

      let doc =
        Format.asprintf "%s%a" P.doc
          (Format.pp_print_option (fun ppf str ->
               Format.fprintf ppf " [%s]" str))
          P.default_str

      let spec = Scan (fun s -> set @@ P.of_string s)
      let key = P.name
      let cspec = Cli_spec.create ~values:P.values ~key ~doc ~spec
      let is_default () = !value = default
      let _ = extend cspec
    end

    module Any (P : DETAILED_CLI_DECL) = Gen (struct
      include P

      let values = Values.Unconstrained
    end)

    module Any_opt (P : sig
      include CLI_DECL
      include Sigs.STR_INJECTIBLE
    end) =
    struct
      type t = P.t

      let is_set = ref false
      let value = ref None

      let set v =
        is_set := true;
        value := Some v

      let get () = Option.get !value
      let get_opt () = !value
      let is_set () = !is_set
      let doc = Format.asprintf "%s" P.doc
      let spec = Scan (fun s -> set @@ P.of_string s)
      let key = P.name
      let cspec = Cli_spec.simple ~key ~doc ~spec
      let is_default () = !value = None
      let _ = extend cspec
    end

    module Choice (P : sig
      include DETAILED_CLI_DECL

      val choices : string list
    end) =
    Gen (struct
      include P

      let values = Values.one_of P.choices
    end)

    module Boolean (P : sig
      include CLI_DECL

      val default : bool
    end) =
    struct
      type t = bool

      let value = ref P.default
      let is_set = ref false

      let set v =
        is_set := true;
        value := v

      let get () = !value
      let is_set () = !is_set
      let doc = Printf.sprintf "%s [%b]" P.doc P.default
      let spec = Unit (fun () -> set (not !value))
      let key = P.name
      let cspec = Cli_spec.simple ~key ~doc ~spec
      let is_default () = !value = P.default
      let _ = extend cspec
    end

    module False (P : CLI_DECL) = struct
      include Boolean (struct
        include P

        let default = false
      end)
    end

    module No (P : CLI_DECL) = struct
      include False (struct
        include P

        let name = "no-" ^ name
      end)

      let get () = not (get ())
      let set v = set (not v)
    end

    module Integer (P : sig
      include CLI_DECL

      val default : int
    end) =
    Any (struct
      type t = int

      include P

      let of_string = int_of_string
      let default_str = Some (string_of_int default)
    end)

    module Integer_option (P : sig
      include CLI_DECL
    end) =
    Any_opt (struct
      type t = int

      include P

      let of_string = int_of_string
    end)

    module Integer_set (P : CLI_DECL) = Any (struct
      type t = Basic_types.Int.Set.t

      include P

      let default = Basic_types.Int.Set.empty

      let of_string s =
        let rexp = Str.regexp sep in
        Str.split rexp s |> List.map int_of_string
        |> Basic_types.Int.Set.of_list

      let default_str = None
    end)

    module Integer_list (P : CLI_DECL) = Any (struct
      include Listify (struct
        type t = int

        let of_string = int_of_string
      end)

      include P
    end)

    module Zero (P : CLI_DECL) = struct
      include Integer (struct
        include P

        let default = 0
      end)
    end

    module Float (P : sig
      include CLI_DECL

      val default : float
    end) =
    Any (struct
      type t = float

      include P

      let of_string = float_of_string
      let default_str = Some (string_of_float default)
    end)

    module Float_option (P : sig
      include CLI_DECL
    end) =
    Any_opt (struct
      type t = float

      include P

      let of_string = float_of_string
    end)

    module Float_set (P : CLI_DECL) = Any (struct
      type t = Basic_types.Float.Set.t

      include P

      let default = Basic_types.Float.Set.empty

      let of_string s =
        let rexp = Str.regexp sep in
        Str.split rexp s |> List.map float_of_string
        |> Basic_types.Float.Set.of_list

      let default_str = None
    end)

    module Float_list (P : CLI_DECL) = Any (struct
      include Listify (struct
        type t = float

        let of_string = float_of_string
      end)

      include P
    end)

    module String (P : sig
      include CLI_DECL

      val default : string
    end) =
    Any (struct
      type t = string

      include P

      let of_string = Fun.id
      let default_str = Some default
    end)

    module String_option (P : CLI_DECL) = Any_opt (struct
      type t = string

      include P

      let of_string = Fun.id
    end)

    module String_list (P : CLI_DECL) = Any (struct
      include Listify (struct
        type t = string

        let of_string = Fun.id
      end)

      include P
    end)

    module String_set (P : CLI_DECL) = Any (struct
      type t = Basic_types.String.Set.t

      include P

      let default = Basic_types.String.Set.empty

      let of_string s =
        let rexp = Str.regexp sep in
        Str.split rexp s |> Basic_types.String.Set.of_list

      let default_str = None
    end)

    module String_choice (P : sig
      include CLI_DECL

      val default : string
      val choices : string list
    end) =
    Choice (struct
      type t = string

      include P

      let of_string s =
        let name = S.lowercase_ascii s in
        if List.mem name choices then name else scan_error s (One_of choices)

      let default_str = Some default
    end)

    (* The fact that we use Arg.Symbol might be too restrictive.
       Using regexp to specify the choices might be more relevant in some
       cases.
       E.g if I want to be able to specify : isa(:wordsize)?(:endianness)?
    *)
    module Variant_choice (P : sig
      include CLI_DECL
      include Sigs.STRINGIFIABLE

      val choices : string list
      val default : t
    end) =
    Choice (struct
      include P

      let of_string s =
        let name = S.lowercase_ascii s in
        if List.mem name choices then of_string name
        else scan_error s (One_of choices)

      let default_str = Some (to_string default)
    end)

    module Variant_choice_assoc (P : sig
      include CLI_DECL

      type t

      val assoc_map : (string * t) list
      val default : t
    end) =
    Variant_choice (struct
      include P

      let choices = List.map fst assoc_map

      let prj1, prj2 =
        let len = List.length assoc_map in
        let h1 = Hashtbl.create len in
        let h2 = Hashtbl.create len in
        List.iter
          (fun (name, value) ->
            Hashtbl.add h1 name value;
            Hashtbl.add h2 value name)
          assoc_map;
        (h1, h2)

      let of_string s =
        Logger.debug ~level:5 "Calling %s of_string" P.name;
        Hashtbl.find prj1 (S.lowercase_ascii s)

      let to_string =
        Logger.debug ~level:5 "Calling %s to_string" P.name;
        Hashtbl.find prj2
    end)

    module Variant_list (P : sig
      include CLI_DECL
      include Sigs.STR_INJECTIBLE
    end) =
    Any (struct
      include P
      include Listify (P)
    end)
  end
end

module Make (D : DECL) = struct
  include Generic_make (D)

  module Enabled = struct
    (* Add enabled switch *)
    let is_enabled = ref false
    let doc = Printf.sprintf "Enable %s" name
    let key = ""
    let spec = Unit (fun () -> is_enabled := true)
    let cspec = Cli_spec.simple ~key ~doc ~spec
    let is_enabled () = is_kernel || !is_enabled
    let _ = if not is_kernel then unsafe_extend cspec
  end

  include Enabled
end

module Options = Generic_make

module Boot = struct
  let h = H.create 7

  let enlist ~name ~f =
    if H.mem h name then
      let msg =
        Printf.sprintf "Startup hook %s has already been declared\n" name
      in
      failwith msg
    else (
      L.debug ~level:5 "Enlisting %s ..." name;
      H.add h name f)

  let launch _name f = f ()

  let run () =
    L.debug ~level:5 "Boot has %d enlisted closures" (H.length h);
    H.iter launch h

  let warn name =
    L.warning "Could not find command line argument %s. Ignoring ..." name

  let find_switch name =
    let open Cli in
    match H.find cli name with
    | Leaf _ -> ()
    | Node n -> (
        (* L.info "Found node %s" name; *)
        match H.find n.Cli_node.cmds "" with
        | Leaf { Argv.spec = Unit f; _ } -> f ()
        (* | Leaf { spec = Arg.Set v;  _ } -> v := true *)
        | Leaf _ | Node _ -> assert false
        | exception Not_found -> warn name)
    | exception Not_found -> warn name

  let maybe_enable = find_switch
end

module Parse = struct
  (* The argument parser *)

  module L = Logger.Make (struct
    let name = "cli_parser"
  end)

  (*  let _ = L.set_debug_level 3;; *)

  let level = 3

  let fail_on switch =
    Plugins.Plugins.Plugins.load_all ();
    Format.eprintf
      "@[<v 0>%s: unknown option %s@ Valid command-line switches are:@ %a@]@."
      Sys.argv.(0) switch Cli.pp ();
    exit 2

  type origin = Command_line | Config_file

  exception Cli_parse_error of string

  let run_on ?(origin = Command_line) cli_options =
    L.debug ~level "Parsing command line: @[<h>%a@]"
      (fun ppf a ->
        Array.iter
          (fun s ->
            Format.pp_print_space ppf ();
            Format.pp_print_string ppf s)
          a)
      cli_options;
    let len = Array.length cli_options in
    L.debug ~level "Got %d args -- parsing now ..." len;
    let stack = Stack.create () in
    let plugins = Basic_types.String.Htbl.create 4 in
    let rec arg_loop args ~last_action i =
      if i < len then (
        let open Argv in
        let switch = cli_options.(i) in
        L.debug ~level "Options %s" switch;
        try
          match H.find args switch with
          | { spec = Unit f; _ } as action ->
              L.debug ~level "Setting %s" switch;
              f ();
              arg_loop args ~last_action:(Some action) (i + 1)
          | { spec = Scan f; doc; _ } as action ->
              let j = i + 1 in
              let arg = cli_options.(j) in
              (try
                 L.debug ~level "Setting %s with %s" switch arg;
                 f arg
               with e ->
                 L.fatal
                   ~e:(Cli_parse_error ("Error parsing option " ^ switch))
                   "@[<v 5>Error parsing %s %s@.%s@.%s@]" switch arg
                   (Printexc.to_string e) doc);
              arg_loop args ~last_action:(Some action) (j + 1)
          | exception Not_found ->
              if switch.[0] = '-' then (
                L.debug
                  "@[<h>unknow switch %S; looking in available plugins [ %a ]@]"
                  switch
                  (Format.pp_print_list ~pp_sep:Format.pp_print_space
                     Format.pp_print_string)
                  (Plugins.Plugins.Plugins.list ());
                match
                  List.find (cmp_plugin switch)
                    (Plugins.Plugins.Plugins.list ())
                with
                | plugin when not (Basic_types.String.Htbl.mem plugins plugin)
                  ->
                    Basic_types.String.Htbl.add plugins plugin ();
                    Plugins.Plugins.Plugins.load plugin;
                    arg_loop (Cli.args ()) ~last_action i
                | (exception Not_found) | _ -> fail_on switch)
              else (
                (match origin with
                | Command_line ->
                    L.debug ~level "Queuing potential file %s" switch;
                    Stack.push switch stack
                | Config_file -> (
                    assert (i <> 0);
                    (* Assumption: there's no way to specify a
                       file without a cli-flag in the configuration file. *)
                    match last_action with
                    | Some { spec = Unit _; key; _ } ->
                        let msg =
                          Printf.sprintf
                            "Error parsing switch %s. Unexpected value %s \
                             after '=' (expected true or false)"
                            key switch
                        in
                        L.fatal ~e:(Cli_parse_error msg) "%s" msg
                    | Some { key; _ } ->
                        let msg =
                          Printf.sprintf
                            "Error parsing switch %s. Unexpected %s after '='"
                            key switch
                        in
                        L.fatal ~e:(Cli_parse_error msg) "%s" msg
                    | None ->
                        L.fatal
                          ~e:
                            (Cli_parse_error ("Unkown error at value " ^ switch))
                          "Got value %s. Don't know what to do." switch));
                arg_loop args ~last_action:None (i + 1))
        with
        | Cli_parse_error _ as e -> raise e
        | Invalid_argument _ ->
            let msg =
              Printf.sprintf
                "Error for option %s (maybe a missing option value ?)" switch
            in
            L.fatal ~e:(Cli_parse_error msg) "%s" msg
        | Not_found ->
            L.fatal
              ~e:(Cli_parse_error ("Unknown option " ^ switch))
              "Unknown option %s" switch)
    in
    let start_index =
      match origin with
      | Command_line -> 1 (* 0 is the executable's name in this case *)
      | Config_file -> 0
      (* we need start right away *)
    in
    arg_loop (Cli.args ()) ~last_action:None start_index;
    (* Get back the rest of possible file names *)
    Stack.fold (fun acc s -> s :: acc) [] stack

  let arg_sep = '='

  let section_read line =
    assert (line.[0] = '[');
    let pos = String.rindex line ']' in
    let section_name = String.sub line 1 (pos - 1) in
    L.debug ~level "Reading section name %s" section_name;
    match section_name with "" | "kernel" -> "" | s -> s

  (* Split the line on arg_sep (aka '=') as name, value pair *)
  let split_on_sep line =
    let open String in
    match index line arg_sep with
    | i ->
        let len = length line in
        (trim @@ sub line 0 i, Some (trim @@ sub line (i + 1) (len - 1 - i)))
    (* line for a boolean switch *)
    | exception Not_found -> (trim line, None)

  let arg_read section line =
    match split_on_sep line with
    | name, None -> (prefix section ^ prefix name, None)
    | "enabled", Some arg_value ->
        L.debug ~level "enabled : <%s>" arg_value;
        assert (bool_of_string arg_value);
        (prefix section, None)
    | name, Some arg_value -> (
        match arg_value with
        | "true" -> (prefix section ^ prefix name, None)
        | "false" -> (prefix section ^ "-no" ^ prefix name, None)
        | _ -> (prefix section ^ prefix name, Some arg_value))

  let unstack stack =
    let len = Stack.length stack in
    let argv = Array.make len "" in
    let rec unwind p =
      if p > 0 then (
        let p' = p - 1 in
        argv.(p') <- Stack.pop stack;
        unwind p')
    in
    unwind len;
    argv

  let of_filename ~filename =
    L.debug ~level "Reading configuration file %s" filename;
    let ic = open_in_bin filename in
    let stack = Stack.create () in
    (* We can write things on several lines using \ at the end of lines. *)
    let rec read_loop line_num current_section =
      let rec multi_line_read acc =
        let line = input_line ic |> String.trim in
        let length = String.length line in
        match String.get line (length - 1) with
        | exception _ ->
            assert (length = 0);
            acc
        (* Substitute the '\' with a space and continue  reading. *)
        | '\\' ->
            multi_line_read (acc ^ (String.sub line 0 @@ (length - 1)) ^ " ")
        | _ -> acc ^ line
      in
      match multi_line_read "" with
      | exception End_of_file -> ()
      | "" -> read_loop (line_num + 1) current_section
      | line -> (
          match line.[0] with
          | '#' ->
              (* This is a comment line *)
              read_loop (line_num + 1) current_section
          | '[' -> (
              (* This is a section title *)
              match section_read line with
              | section_name -> read_loop (line_num + 1) section_name
              | exception Not_found ->
                  L.error "Error reading section line %d in %s" line_num
                    filename;
                  exit 1)
          | _ -> (
              match arg_read current_section line with
              | arg_name, None ->
                  L.debug ~level "Activating %s" arg_name;
                  Stack.push arg_name stack;
                  read_loop (line_num + 1) current_section
              | arg_name, Some arg_value ->
                  L.debug ~level "Reading %s <- %s" arg_name arg_value;
                  Stack.push arg_name stack;
                  Stack.push arg_value stack;
                  read_loop (line_num + 1) current_section
              | exception Not_found ->
                  L.error "Error reading arg line %d in %s" line_num filename;
                  exit 1))
    in
    read_loop 0 "";
    close_in ic;
    run_on ~origin:Config_file (unstack stack)

  let run () = run_on ~origin:Command_line Sys.argv
end

let parse = Parse.run
let parse_configuration_file ~filename = Parse.of_filename ~filename
