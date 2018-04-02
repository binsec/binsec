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

(** Logging/output facilities *)


module type S = sig

  (** {2 Channels} *)

  type channel

  val fatal_channel   : channel
  val error_channel   : channel
  val result_channel  : channel
  val warning_channel : channel
  val info_channel    : channel
  val debug_channel   : channel
  (** These predefined channels are flushed after each call and a newline character is
      inserted. *)


  val fatal: ('a, Format.formatter, unit) format -> 'a
  (** For messages that show a fatal failure, In this case, you should not be
      able to continue and exit [code] should follow the emission.
      Use [error] otherwise. *)

  val error: ('a, Format.formatter, unit) format -> 'a
  (** For error messages only. *)

  val result: ('a, Format.formatter, unit) format -> 'a
  (** For important results that will be displayed *)

  val warning: ?level:int -> ('a, Format.formatter, unit) format -> 'a
  (** For warning messages. *)

  val set_warning_level : int -> unit
  val get_warning_level : unit -> int

  val info: ?level:int -> ('a, Format.formatter,unit) format -> 'a
  (** Any info that should be displayed *)

  val set_info_level : int -> unit
  val get_info_level : unit -> int

  val debug: ?level:int -> ('a, Format.formatter, unit) format -> 'a
  (** [debug ~level:n msg] will be displayed if at least level n + 1 debug is
      activated *)

  val set_debug_level : int -> unit
  val get_debug_level : unit -> int

  val set_tagged_entry : bool -> unit
  (** [set_tagged_entry]
      Print channel identifiers, like [warning] for the warning channel, in front
      of messages to explicit their origins.

      If might not be necessary if you use colors for example.
  *)


  val set_log_level : string -> unit
  (** Set logger to display only messages from that channel and those with
      higher loglevels.

      Valid arguments in increasing order of loglevels are :
      "debug", "info", "warning", "error", "fatal"/"result".

      You cannot turn off [fatal_channel] or [result_channel].
  *)


  val cli_handler : Arg.spec
  val quiet : unit -> unit

  val channel_set_color : bool -> channel -> unit
  (** [set_channel_color b chan] activates (if [b] is [true]) or deactivates (if
      [b] is [false]) the emission of ANSI color tags for terminal.

      You might want to deactivate the feature if you plan on analyzing the log in
      a file for example.
  *)

  val channel_get_color : channel -> bool

  val set_color : bool -> unit
  (** Activate color tags rendering on all outputs.

      In your format strings, tags of the form ["\@\{<color>format\@\}"] will thus
      be interpreted.

      [color] can be any of the following:

          - black
          - darkgray
          - blue
          - lightblue
          - green
          - lightgreen,
          - cyan
          - lightcyan
          - red
          - lightred
          - purple
          - lightpurple
          - brown
          - yellow
          - lightgray
          - white

      [format] is any format string recognized by [Format].
      For example, after activating color tags
      [Format.printf "\@\{<purple>Hello!\@\}"] will write "Hello!" in
      purple if your terminal has such features.
  *)

  val get_color : unit -> bool

end

(* {2 Functors} *)

module type ChannelGroup = sig
  val name : string
end

module Make(G: ChannelGroup): S

include S


(* {2 Generic utilites} *)

val set_zmq_logging_only : send:(string -> unit) -> bool -> unit
(** [set_zmq_logging_only ~send b identity] diverts all formatting to an identity if
    [b] is [true]. Be careful in this case: all other formatters are erased.
    If [b] is false, formatters are reset to default initial values.
*)


val with_tags_on : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
(** [with_tags_on ppf fmt] pretty-prints [fmt] on the pretty-printing formatter
    [ppf] with tag marking and printing functions activated.
    Before it quits, those functions are deactivated.

    This allows delimits an environment where tags simply need to be interpreted
    or might have specific semantics.
*)
