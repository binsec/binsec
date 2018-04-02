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

open Format

let with_tags_on ppf fmt =
  let mark_tags = pp_get_mark_tags ppf ()
  and print_tags = pp_get_print_tags ppf () in
  pp_set_mark_tags ppf true;
  pp_set_print_tags ppf true;
  kfprintf (fun ppf ->
      pp_set_mark_tags ppf mark_tags;
      pp_set_print_tags ppf print_tags)
    ppf fmt 

module Color = struct
  type t =
    | Black
    | DarkGray
    | Blue
    | LightBlue
    | Green
    | LightGreen
    | Cyan
    | LightCyan
    | Red
    | LightRed
    | Purple
    | LightPurple
    | Brown
    | Yellow
    | LightGray
    | White

  let _terminal_encoding = function
    | Black -> "0;30"
    | DarkGray -> "1;30"
    | Blue -> "0;34"
    | LightBlue -> "1;34"
    | Green -> "0;32"
    | LightGreen -> "1;32"
    | Cyan -> "0;36"
    | LightCyan -> "1;36"
    | Red -> "0;31"
    | LightRed -> "1;31"
    | Purple -> "0;35"
    | LightPurple -> "1;35"
    | Brown -> "0;33"
    | Yellow -> "1;33"
    | LightGray -> "0;37"
    | White -> "1;37"


  let to_string = function
    | Black -> "black"
    | DarkGray -> "darkgray"
    | Blue -> "blue"
    | LightBlue -> "lightblue"
    | Green -> "green"
    | LightGreen -> "lightgreen"
    | Cyan -> "cyan"
    | LightCyan -> "lightcyan"
    | Red -> "red"
    | LightRed -> "lightred"
    | Purple -> "purple"
    | LightPurple -> "lightpurple"
    | Brown -> "brown"
    | Yellow -> "yellow"
    | LightGray -> "lightgray"
    | White -> "white"


  let string_to_terminal_color_codes color_name =
    match String.lowercase color_name with
    | "black" -> Some "0;30"
    | "darkgray" -> Some "1;30"
    | "blue" -> Some "0;34"
    | "lightblue" -> Some "1;34"
    | "green" -> Some "0;32"
    | "lightgreen" -> Some "1;32"
    | "cyan" -> Some "0;36"
    | "lightcyan" -> Some "1;36"
    | "red" -> Some "0;31"
    | "lightred" -> Some "1;31"
    | "purple" -> Some "0;35"
    | "lightpurple" -> Some "1;35"
    | "brown" -> Some "0;33"
    | "yellow" -> Some "1;33"
    | "lightgray" -> Some "0;37"
    | "white" -> Some "1;37"
    | _ -> (* warning "Unsupported color name %s@." s;*) None

  (* ¯\_(ツ)_/¯ *)
  let _black = Black
  let _darkgray = DarkGray
  let _blue = Blue
  let _lightblue = LightBlue
  let _green = Green
  let _lightgreen = LightGreen
  let _cyan = Cyan
  let _lightcyan = LightCyan
  let _red = Red
  let _lightred = LightRed
  let _purple = Purple
  let _lightpurple = LightPurple
  let _brown = Brown
  let _yellow = Yellow
  let _lightgray = LightGray
  let _white = White

end

module ChannelKind = struct
  type t =
    | ChInfo    (* Normal output to feedback positive results *)
    | ChResult  (* Normal output kind *)
    | ChDebug   (* Debug message *)
    | ChWarning (* Warning message *)
    | ChError   (* For events not meant to occur but that we can handle *)
    | ChFatal   (* Fatal failures *)


  let to_string = function
    | ChInfo -> "info"
    | ChResult -> "result"
    | ChDebug -> "debug"
    | ChWarning -> "warning"
    | ChError -> "error"
    | ChFatal -> "fatal"


  let values =[ "debug"; "info"; "result"; "warning"; "error"; "fatal"; ]


  let loglevel  = function
    | ChDebug -> 0
    | ChInfo -> 10
    | ChWarning -> 20
    | ChError -> 100
    | ChFatal -> max_int  (* Fatal & Result channels cannot be turned off *)
    | ChResult -> max_int


  let of_string s =
    match String.lowercase s with
    | "info"    -> ChInfo
    | "result"  -> ChResult
    | "warning" -> ChWarning
    | "error"   -> ChError
    | "fatal"   -> ChFatal
    | "debug"   -> ChDebug
    | s ->
      failwith (sprintf "%s not a channel string identifier" s)


  let is_string_identifier s =
    match of_string s with
    | _ -> true
    | exception (Failure _) -> false

  let color kind =
    let open Color in
    match kind with
    | ChInfo    -> LightGray
    | ChResult  -> LightGray
    | ChDebug   -> Cyan
    | ChWarning -> Yellow
    | ChError   -> LightRed
    | ChFatal   -> Red

end

module type S = sig
  type channel

  val fatal_channel   : channel
  val error_channel   : channel
  val result_channel  : channel
  val warning_channel : channel
  val info_channel    : channel
  val debug_channel   : channel

  val fatal: ('a, Format.formatter, unit) format -> 'a
  val error: ('a, Format.formatter, unit) format -> 'a
  val result: ('a, Format.formatter, unit) format -> 'a
  val warning: ?level:int -> ('a, Format.formatter, unit) format -> 'a
  val set_warning_level : int -> unit
  val get_warning_level : unit -> int

  val info: ?level:int -> ('a, Format.formatter,unit) format -> 'a
  val set_info_level : int -> unit
  val get_info_level : unit -> int

  val debug: ?level:int -> ('a, Format.formatter, unit) format -> 'a
  val set_debug_level : int -> unit
  val get_debug_level : unit -> int

  val set_tagged_entry : bool -> unit
  val set_log_level : string -> unit

  val cli_handler : Arg.spec
  val quiet : unit -> unit

  val channel_set_color : bool -> channel -> unit
  val channel_get_color : channel -> bool

  val set_color : bool -> unit
  val get_color : unit -> bool
end

module type ChannelGroup = sig
  val name : string
end


module Make(G : ChannelGroup) = struct

  let set_log_level, log_level_of_chkind, get_log_level, quiet =
    let loglevel = ref (ChannelKind.loglevel ChannelKind.ChInfo) in
    (fun (s:string) ->
      loglevel := ChannelKind.of_string s |> ChannelKind.loglevel),
    (fun ck ->
       let ck_loglevel = ChannelKind.loglevel ck in
       (* Only auto-update loglevel if it is lower than it already is *)
       if ck_loglevel < !loglevel then loglevel := ck_loglevel ),
    (fun () -> !loglevel),
    (fun () -> loglevel := max_int)


  let cli_handler =
    Arg.Symbol(ChannelKind.values, set_log_level)

  type channel = {
    kind : ChannelKind.t;
    mutable ppfs : Format.formatter list; (* These are similar to listeners *)
  }

  let stdout_ppf () = formatter_of_out_channel stdout
  let stderr_ppf () = formatter_of_out_channel stderr

  
  let default_out kind = { kind; ppfs = [ stdout_ppf () ]; }
  let err_out kind = { kind; ppfs = [ stderr_ppf () ]; }

  let debug_channel   = default_out ChannelKind.ChDebug
  and info_channel    = default_out ChannelKind.ChInfo
  and result_channel  = default_out ChannelKind.ChResult
  and warning_channel = err_out ChannelKind.ChWarning
  and error_channel   = err_out ChannelKind.ChError
  and fatal_channel   = err_out ChannelKind.ChFatal


  let set_formatters ppfs channel =
    channel.ppfs <- ppfs

  let reset_channels () =
    let stdfmt () = [ stdout_ppf () ]
    and errfmt () = [ stderr_ppf () ] in
    set_formatters (stdfmt ()) debug_channel;
    set_formatters (stdfmt ()) info_channel;
    set_formatters (stdfmt ()) result_channel;
    set_formatters (errfmt ()) warning_channel;
    set_formatters (errfmt ()) error_channel;
    set_formatters (errfmt ()) fatal_channel


  let channels =
    [ debug_channel; info_channel; warning_channel;
      result_channel; error_channel; fatal_channel;
    ]


  let set_tagged_entry, get_tagged_entry =
    let tag = ref true in
    (fun ta -> tag := ta),
    (fun () -> !tag)

  let channel_group_delimiter = '/'

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
    let mark_open_tag tag_string =
      match Color.string_to_terminal_color_codes tag_string with
      | None -> ""
      | Some tcolor_code ->
        sprintf "\027[%sm" tcolor_code

    and print_open_tag tag_string =
      if get_tagged_entry () && is_channel_tagstring tag_string
      then fprintf ppf "[%s] " tag_string
    (* otherwise it's assumed to be a color tag string, handled by
       [mark_open_tag] *)
          
    and print_close_tag _tag_string = ()

    and mark_close_tag _ = "\027[0m"
    in { mark_open_tag; mark_close_tag; print_open_tag; print_close_tag; }


  
  let log (channel: channel) txt  =
    let ppfs = channel.ppfs in
    let pp fmt txt =
      if (ChannelKind.loglevel channel.kind) >= (get_log_level ())
      then 
        Format.kfprintf
          (fun fmt ->
             Format.kfprintf (fun fmt -> Format.fprintf fmt "@]@}@}@.") fmt txt)
          fmt "@{<%s>@{<%s>@[<hov 0>"
          (ChannelKind.color channel.kind |> Color.to_string)
          (channel_name channel.kind)
      else Format.ifprintf fmt txt
    in
    let rec aux = function
      | [] -> assert false (* One should not be able to "dry" a channel,
                              i.e. have no pretty-printing formatter associated to it *)
      | [ppf] -> pp ppf txt
      | ppf :: ppfs -> pp ppf txt; aux ppfs
    in aux ppfs


  let mk_level_functions chan =
    let level = ref 0 in
    (fun () -> !level),
    (fun n ->
      assert (n >= 0);
      level := n;
      log_level_of_chkind chan.kind
    ),
    (fun lvl -> lvl <= !level)


  let get_debug_level, set_debug_level, debug_pass = mk_level_functions debug_channel
  let get_info_level, set_info_level, info_pass = mk_level_functions info_channel
  let get_warning_level, set_warning_level, warning_pass = mk_level_functions warning_channel

  let leveled_channel channel level_pass =
    (fun ?(level=0) txt ->
       if level_pass level
       then log channel txt
       else Format.ifprintf Format.std_formatter txt)

  let debug ?(level=0) txt =
    leveled_channel debug_channel debug_pass ~level txt

  let info ?(level=0) txt =
    leveled_channel info_channel info_pass ~level txt

  let warning ?(level=0) txt =
    leveled_channel warning_channel warning_pass ~level txt

  let fatal  txt = log fatal_channel txt
  let error  txt = log error_channel txt
  let result txt = log result_channel txt

  let _ =
    List.iter
      (fun channel ->
         let ppfs = channel.ppfs in
         List.iter
           (fun ppf ->
              pp_set_formatter_tag_functions ppf (tag_functions ppf);
              pp_set_print_tags ppf true;
           ) ppfs
      ) channels

  
  let channel_set_color, channel_get_color =
    let color_tbl = Hashtbl.create (List.length ChannelKind.values) in
    (fun b channel ->
       Hashtbl.replace color_tbl channel b;
       let ppfs = channel.ppfs in
       match b with
       | true ->
         List.iter (fun ppf -> pp_set_mark_tags ppf true) ppfs
       | false ->
         List.iter (fun ppf -> pp_set_mark_tags ppf false) ppfs
    ),
    (fun channel ->
       match Hashtbl.find color_tbl channel with
       | color_bool -> color_bool
       | exception Not_found -> false
    )

  let set_color, get_color =
    let v = ref false in
    (fun b ->
       v := b;
       List.iter (channel_set_color b) channels),
    (fun () -> !v)

end


(* default printers *)
include Make(struct let name = "" end)


let set_zmq_logging_only ~send = function
  | false -> reset_channels ()
  | true ->
    let zmq_buffer = Buffer.create 2048 in
    let out_string str start len =
      let s = String.sub str start len in
      Buffer.add_string zmq_buffer s;
    in
    let flush () =
      let msg = Buffer.contents zmq_buffer in
      send msg;
      Buffer.reset zmq_buffer
    in
    let zeromq_fmt = Format.make_formatter out_string flush in
    List.iter (set_formatters [zeromq_fmt]) channels
