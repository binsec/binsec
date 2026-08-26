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

open Format

module Color = struct
  type t =
    | Default
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Purple
    | Cyan
    | White

  let terminal_encoding = function
    | Default -> ""
    | Black -> ";30"
    | Red -> ";31"
    | Green -> ";32"
    | Yellow -> ";33"
    | Blue -> ";34"
    | Purple -> ";35"
    | Cyan -> ";36"
    | White -> ";37"

  let to_string = function
    | Default -> "default"
    | Black -> "black"
    | Red -> "red"
    | Green -> "green"
    | Yellow -> "yellow"
    | Blue -> "blue"
    | Purple -> "purple"
    | Cyan -> "cyan"
    | White -> "white"

  let of_string = function
    | "black" -> Black
    | "red" -> Red
    | "green" -> Green
    | "yellow" -> Yellow
    | "blue" -> Blue
    | "purple" -> Purple
    | "cyan" -> Cyan
    | "white" -> White
    | _ -> Default

  let string_to_terminal_color_codes color_name =
    terminal_encoding (of_string color_name)
end

module Emphasis = struct
  type t = Default | Bold | Italic

  let terminal_encoding = function
    | Default -> "0"
    | Bold -> "1"
    | Italic -> "3"

  let to_string = function
    | Default -> "default"
    | Bold -> "bold"
    | Italic -> "italic"

  let of_string = function "bold" -> Bold | "italic" -> Italic | _ -> Default
end

type font = Color.t * Emphasis.t

module Profile = struct
  type t = {
    name : string;
    debug : font;
    step : font;
    info : font;
    result : font;
    warning : font;
    error : font;
    fatal : font;
  }

  let default =
    {
      name = "default";
      debug = (Cyan, Default);
      step = (Default, Default);
      info = (Green, Default);
      result = (Default, Bold);
      warning = (Yellow, Default);
      error = (Red, Bold);
      fatal = (Red, Bold);
    }

  let default_light =
    {
      name = "default_light";
      debug = (Cyan, Default);
      step = (Default, Default);
      info = (Purple, Default);
      result = (Default, Bold);
      warning = (Yellow, Default);
      error = (Red, Bold);
      fatal = (Red, Bold);
    }

  let dark =
    {
      name = "dark";
      debug = (Blue, Default);
      step = (White, Default);
      info = (Default, Italic);
      result = (Default, Bold);
      warning = (Yellow, Italic);
      error = (Red, Bold);
      fatal = (Red, Bold);
    }

  let light =
    {
      name = "light";
      debug = (Cyan, Default);
      step = (Default, Default);
      info = (Black, Italic);
      result = (Black, Bold);
      warning = (Yellow, Italic);
      error = (Red, Bold);
      fatal = (Red, Bold);
    }

  let of_string s =
    match String.lowercase_ascii s with
    | "default" -> default
    | "default_light" -> default_light
    | "dark" -> dark
    | "light" -> light
    | _ -> raise Not_found

  let all_profiles = [ default; default_light; dark; light ]
end

let with_tags_on ppf fmt =
  let mark_tags = pp_get_mark_tags ppf ()
  and print_tags = pp_get_print_tags ppf () in
  pp_set_mark_tags ppf true;
  pp_set_print_tags ppf true;
  kfprintf
    (fun ppf ->
      pp_set_mark_tags ppf mark_tags;
      pp_set_print_tags ppf print_tags)
    ppf fmt

type stag += Font of (Color.t * Emphasis.t)

(** Reference color profile - should be one listed in Profile. *)
let profile = ref Profile.default

let set_profile profile_input = profile := profile_input
let get_profile () = !profile

module ChannelKind = struct
  type t =
    | Debug (* Debug message *)
    | Step (* General operations *)
    | Info (* Normal output to feedback positive results *)
    | Result (* Normal output kind *)
    | Warning (* Warning message *)
    | Error (* For events not meant to occur but that we can handle *)
    | Fatal (* Fatal failures *)

  let to_string = function
    | Debug -> "debug"
    | Step -> "step"
    | Info -> "info"
    | Result -> "result"
    | Warning -> "warning"
    | Error -> "error"
    | Fatal -> "fatal"

  let values = [ Debug; Step; Info; Result; Warning; Error; Fatal ]

  let loglevel = function
    | Debug -> 0
    | Step -> 5
    | Info -> 10
    | Warning -> 20
    | Error -> 100
    | Fatal -> max_int (* Fatal & Result channels cannot be turned off *)
    | Result -> max_int

  let of_string s =
    match String.lowercase_ascii s with
    | "debug" -> Debug
    | "step" -> Step
    | "info" -> Info
    | "result" -> Result
    | "warning" -> Warning
    | "error" -> Error
    | "fatal" -> Fatal
    | s -> failwith (sprintf "%s not a channel string identifier" s)

  let is_string_identifier s =
    match of_string s with _ -> true | exception Failure _ -> false
end

module type S = sig
  type channel

  val fatal_channel : channel
  val error_channel : channel
  val result_channel : channel
  val warning_channel : channel
  val info_channel : channel
  val step_channel : channel
  val debug_channel : channel
  val fatal : ?e:exn -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  val error : ('a, Format.formatter, unit) format -> 'a
  val result : ('a, Format.formatter, unit) format -> 'a
  val warning : ?level:int -> ('a, Format.formatter, unit) format -> 'a
  val set_warning_level : int -> unit
  val get_warning_level : unit -> int
  val info : ?level:int -> ('a, Format.formatter, unit) format -> 'a
  val set_info_level : int -> unit
  val get_info_level : unit -> int
  val step : ('a, Format.formatter, unit) format -> 'a
  val debug : ?level:int -> ('a, Format.formatter, unit) format -> 'a

  val fdebug :
    ?level:int -> (unit -> (unit, Format.formatter, unit) format) -> unit

  val set_debug_level : int -> unit
  val get_debug_level : unit -> int
  val is_debug_enabled : unit -> bool
  val set_tagged_entry : bool -> unit
  val set_log_level : string -> unit
  val quiet : unit -> unit
  val channel_set_color : bool -> channel -> unit
  val channel_get_color : channel -> bool
  val set_color : bool -> unit
  val get_color : unit -> bool
  val set_logging : (string -> unit) option -> channel -> unit
end

module type ChannelGroup = sig
  val name : string
end

type channel = {
  kind : ChannelKind.t;
  mutable ppfs : Format.formatter list; (* These are similar to listeners *)
}

module Make (G : ChannelGroup) = struct
  let set_log_level, log_level_of_chkind, get_log_level, quiet =
    let loglevel = ref (ChannelKind.loglevel Info) in
    ( (fun (s : string) ->
        loglevel := ChannelKind.of_string s |> ChannelKind.loglevel),
      (fun ck ->
        let ck_loglevel = ChannelKind.loglevel ck in
        (* Only auto-update loglevel if it is lower than it already is *)
        if ck_loglevel < !loglevel then loglevel := ck_loglevel),
      (fun () -> !loglevel),
      fun () -> loglevel := max_int )

  type nonrec channel = channel

  let default_out kind = { kind; ppfs = [ Format.std_formatter ] }
  let err_out kind = { kind; ppfs = [ Format.err_formatter ] }

  let debug_channel = default_out Debug
  and step_channel = default_out Step
  and info_channel = default_out Info
  and result_channel = default_out Result
  and warning_channel = err_out Warning
  and error_channel = err_out Error
  and fatal_channel = err_out Fatal

  let channels =
    [
      debug_channel;
      step_channel;
      info_channel;
      warning_channel;
      result_channel;
      error_channel;
      fatal_channel;
    ]

  let set_formatters ppfs channel = channel.ppfs <- ppfs

  let reset_channel : channel -> unit =
   fun channel ->
    match channel.kind with
    | Info | Result | Step | Debug ->
        set_formatters [ Format.std_formatter ] channel
    | Warning | Error | Fatal -> set_formatters [ Format.err_formatter ] channel

  let set_tagged_entry, get_tagged_entry =
    let tag = ref true in
    ((fun ta -> tag := ta), fun () -> !tag)

  let channel_group_delimiter = ':'

  let channel_name chan_kind =
    let chan_kind_name = ChannelKind.to_string chan_kind in
    if G.name = "" then chan_kind_name
    else sprintf "%s%c%s" G.name channel_group_delimiter chan_kind_name

  (* @assumes a tag string for channels has a form <[group_name/]channel_name>
     It should have been produced by a call to [channel_name] to ensure the
     pre-condition.
  *)
  let channel_kind_of_tagstring tag_string =
    match String.index tag_string channel_group_delimiter with
    | n ->
        assert (n <> 0);
        String.sub tag_string (n + 1) (String.length tag_string - n - 1)
    | exception Not_found -> tag_string

  let is_channel_tagstring tag_string =
    channel_kind_of_tagstring tag_string |> ChannelKind.is_string_identifier

  (* Tag functions that react to color codes and channel tagging *)
  let tag_functions ppf =
    let mark_open_stag = function
      | String_tag tag_string -> (
          match Color.string_to_terminal_color_codes tag_string with
          | "" -> ""
          | code -> sprintf "\027[0;%sm" code)
      | Font (color, emphasis) ->
          sprintf "\027[%s%sm"
            (Emphasis.terminal_encoding emphasis)
            (Color.terminal_encoding color)
      | _ -> ""
    and print_open_stag = function
      | String_tag tag_string ->
          if get_tagged_entry () && is_channel_tagstring tag_string then
            fprintf ppf "[%s] " tag_string
      | _ -> ()
    (* otherwise it's assumed to be a color tag string, handled by
       [mark_open_tag] *)
    and print_close_stag _tag_string = ()
    and mark_close_stag _ = "\027[0m" in
    { mark_open_stag; mark_close_stag; print_open_stag; print_close_stag }

  let local_profile (channel : ChannelKind.t) (profile : Profile.t) : stag =
    match channel with
    | Debug -> Font profile.debug
    | Step -> Font profile.step
    | Info -> Font profile.info
    | Result -> Font profile.result
    | Warning -> Font profile.warning
    | Error -> Font profile.error
    | Fatal -> Font profile.fatal

  let log finally channel txt =
    let ppfs = channel.ppfs in
    let pp fmt txt =
      if ChannelKind.loglevel channel.kind >= get_log_level () then (
        Format.pp_open_stag fmt (local_profile channel.kind !profile);
        Format.pp_open_stag fmt (String_tag (channel_name channel.kind));
        Format.kfprintf
          (fun fmt ->
            Format.kfprintf
              (fun fmt -> Format.kfprintf finally fmt "@]@}@}@.")
              fmt txt)
          fmt "@[<hov 0>")
      else Format.ikfprintf finally fmt txt
    in
    let rec aux = function
      | [] -> assert false
      (* One should not be able to "dry" a channel,
         i.e. have no pretty-printing formatter associated to it *)
      | [ ppf ] -> pp ppf txt
      | ppf :: ppfs ->
          ignore @@ pp ppf txt;
          aux ppfs
    in
    aux ppfs

  (*  module type Leveled_chan = sig
   *     val set : int -> unit
   *     val get : unit -> int
   *     val pass : int -> bool
   *  end
   *
   *
   * let mk_level_mod chan =
   *   let module M = struct
   *       let level = ref 0
   *       let set n =
   *         assert (n >= 0);
   *         level := n;
   *         log_level_of_chkind chan.kind
   *
   *       let get () = !level
   *
   *       let pass n = n <= !level
   *     end
   *   in (module M:Leveled_chan) *)

  let mk_level_functions chan =
    let level = ref 0 in
    ( (fun () -> !level),
      (fun n ->
        assert (n >= 0);
        level := n;
        log_level_of_chkind chan.kind),
      fun lvl -> lvl <= !level )

  (* let d = mk_level_mod debug_channel
   * module Debug_level = (val d : Leveled_chan)
   *
   * let i = mk_level_mod info_channel
   * module Info_level = (val i : Leveled_chan)
   *
   * let w = mk_level_mod warning_channel
   * module Warning_level = (val w : Leveled_chan) *)

  let get_debug_level, set_debug_level, debug_pass =
    mk_level_functions debug_channel

  let is_debug_enabled =
    let threshold = ChannelKind.loglevel Debug in
    fun () -> get_log_level () <= threshold

  let get_info_level, set_info_level, info_pass =
    mk_level_functions info_channel

  let get_warning_level, set_warning_level, warning_pass =
    mk_level_functions warning_channel

  let leveled_channel finally channel level_pass ?(level = 0) txt =
    if level_pass level then log finally channel txt
    else Format.ifprintf Format.std_formatter txt

  let finally_unit _ = ()
  let step txt = log finally_unit step_channel txt

  let debug ?(level = 0) txt =
    leveled_channel finally_unit debug_channel debug_pass ~level txt

  let fdebug ?(level = 0) f =
    if debug_pass level then log finally_unit debug_channel (f ())
    else Format.ifprintf Format.std_formatter ""

  let info ?(level = 0) txt =
    leveled_channel finally_unit info_channel info_pass ~level txt

  let warning ?(level = 0) txt =
    leveled_channel finally_unit warning_channel warning_pass ~level txt

  let fatal ?(e = Failure "abort") txt =
    log (fun _ -> raise e) fatal_channel txt

  let error txt = log finally_unit error_channel txt
  let result txt = log finally_unit result_channel txt

  let _ =
    List.iter
      (fun channel ->
        let ppfs = channel.ppfs in
        List.iter
          (fun ppf ->
            pp_set_formatter_stag_functions ppf (tag_functions ppf);
            pp_set_print_tags ppf true)
          ppfs)
      channels

  let channel_set_color, channel_get_color =
    let color_tbl = Hashtbl.create (List.length ChannelKind.values) in
    ( (fun b channel ->
        Hashtbl.replace color_tbl channel b;
        let ppfs = channel.ppfs in
        List.iter (fun ppf -> pp_set_mark_tags ppf b) ppfs),
      fun channel ->
        match Hashtbl.find color_tbl channel with
        | color_bool -> color_bool
        | exception Not_found -> false )

  let set_color, get_color =
    let v = ref false in
    ( (fun b ->
        v := b;
        List.iter (channel_set_color b) channels),
      fun () -> !v )

  let set_logging send channel =
    match send with
    | None -> reset_channel channel
    | Some f ->
        let buffer = Buffer.create 2048 in
        let out_string str start len =
          Buffer.add_substring buffer str start len
        in
        let flush () =
          let msg = Buffer.contents buffer in
          Buffer.reset buffer;
          f msg
        in
        set_formatters [ Format.make_formatter out_string flush ] channel
end

module type GROUP = sig
  include S
  module Sub (_ : ChannelGroup) : S with type channel = channel
end

module Group (G : ChannelGroup) : GROUP = struct
  let loggers : (module S with type channel = channel) Queue.t = Queue.create ()

  let iter : ((module S with type channel = channel) -> unit) -> unit =
   fun f -> Queue.iter f loggers

  include Make (G)

  let set_warning_level n =
    set_warning_level n;
    iter (fun logger ->
        let module L = (val logger) in
        L.set_warning_level n)

  let set_info_level n =
    set_info_level n;
    iter (fun logger ->
        let module L = (val logger) in
        L.set_info_level n)

  let set_debug_level n =
    set_debug_level n;
    iter (fun logger ->
        let module L = (val logger) in
        L.set_debug_level n)

  let set_tagged_entry v =
    set_tagged_entry v;
    iter (fun logger ->
        let module L = (val logger) in
        L.set_tagged_entry v)

  let set_log_level t =
    set_log_level t;
    iter (fun logger ->
        let module L = (val logger) in
        L.set_log_level t)

  let quiet () =
    quiet ();
    iter (fun logger ->
        let module L = (val logger) in
        L.quiet ())

  let channel_set_color v c =
    channel_set_color v c;
    iter (fun logger ->
        let module L = (val logger) in
        L.channel_set_color v c)

  let set_color v =
    set_color v;
    iter (fun logger ->
        let module L = (val logger) in
        L.set_color v)

  let set_logging f c =
    set_logging f c;
    iter (fun logger ->
        let module L = (val logger) in
        L.set_logging f c)

  module Sub (G : ChannelGroup) : S with type channel = channel = struct
    module L = Make (G)
    include L

    let () = Queue.add (module L : S with type channel = channel) loggers
  end
end

(* default printers *)
