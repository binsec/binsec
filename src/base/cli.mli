(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

(** Functors for command-line parameters declarations *)


(** {3 Module type declarations }*)
module type GENERIC = sig
  type t
  val set : t -> unit
  val get : unit -> t
  val is_default : unit -> bool
end

module type CHECKABLE = sig
  include GENERIC
  val is_set : unit -> bool
end

module type GENERIC_OPT = sig
  include CHECKABLE
  val get_opt : unit -> t option
end


(** {4 Module types for option builders}*)

module type BOOLEAN = GENERIC with type t = bool

(** {5 Integer parameters} *)
module type INTEGER = GENERIC with type t = int
module type INTEGER_SET = CHECKABLE with type t = Basic_types.Int.Set.t
module type INTEGER_LIST = CHECKABLE with type t = int list
module type INTEGER_OPT = GENERIC_OPT with type t = int

(** {5 Floating-point parameters} *)
module type FLOAT = GENERIC with type t = float
module type FLOAT_SET = CHECKABLE with type t = Basic_types.Float.Set.t
module type FLOAT_LIST = CHECKABLE with type t = float list
module type FLOAT_OPT = GENERIC_OPT with type t = float


(** {5 String parameters} *)
module type STRING = GENERIC with type t = string
module type STRING_OPT = GENERIC_OPT with type t = string
module type STRING_SET = CHECKABLE with type t = Basic_types.String.Set.t
module type STRING_LIST = CHECKABLE with type t = string list

(** {3 Builder functors for command-line parameters} *)

module type DECL = sig
  val name : string
  (** Name to be displayed in documentation *)

  val shortname : string
  (** Name to use as prefix for command-line options *)
end

(** {4 Module types for command-line declarations}*)
module type CLI_DECL = sig
  val name : string
  val doc : string
end

module type DEFAULTED_CLI_DECL = sig
  include CLI_DECL
  type t
  val default : t
end

module type DETAILED_CLI_DECL = sig
  include DEFAULTED_CLI_DECL
  include Sigs.STRINGIFIABLE with type t := t
end

module type S = sig
  val is_enabled : unit -> bool
  module Logger : Logger.S
end

(** {3 Functor signature }*)

module type Cli_sig = sig
  module Logger : Logger.S

  module Debug_level : INTEGER
  module Loglevel : STRING
  module Quiet : BOOLEAN

  module Builder : sig

    module Any(P: DETAILED_CLI_DECL):
      GENERIC with type t = P.t
    (** A very generic functor that lets you handle cases that are not provided
        otherwise. Use it only as last resort.
     *)


    (** {4 Boolean functors}*)
    module Boolean(P: sig include CLI_DECL val default : bool end) :
    BOOLEAN
    (** Generic boolean option to which you give a [default] value *)


    module False(P:CLI_DECL) : BOOLEAN
    (** An option that defaults to [false]. *)

    module True (P:CLI_DECL) : BOOLEAN
    (** An options that defaults to [true].
       The provided command-line switch
       automatically add a [no-] prefix to your option name.
     *)


    (** {4 Integer functors}*)
    module Integer(P: sig include CLI_DECL val default : int end) :
    INTEGER
    module Zero(P:CLI_DECL) : INTEGER
    module Integer_set(P: CLI_DECL) : INTEGER_SET
    module Integer_list(P: CLI_DECL) : INTEGER_LIST
    module Integer_option(P:CLI_DECL) : INTEGER_OPT


    (** {4 Floating point functors}*)
    module Float(P: sig include CLI_DECL val default : float end) :
    FLOAT
    module Float_set(P: CLI_DECL) : FLOAT_SET
    module Float_list(P: CLI_DECL) : FLOAT_LIST
    module Float_option(P:CLI_DECL) : FLOAT_OPT


    (** {4 String functors}*)
    module String(P: sig include CLI_DECL val default : string end) :
    STRING

    module String_choice(P: sig include CLI_DECL
                               val default : string
                               val choices : string list
                          end) : STRING

    module String_option(P: CLI_DECL) : STRING_OPT
    module String_set(P: CLI_DECL)  : STRING_SET
    module String_list(P: CLI_DECL) : STRING_LIST

    (** {4 Variant functors} *)
    module Variant_choice (P: sig include CLI_DECL
                                 type t
                                 val to_string : t -> string
                                 val of_string : string -> t
                                 val choices : string list
                                 val default : t
                            end): GENERIC with type t = P.t
    (** Functor to map a string choice --- i.e., just one out of a set of
        possible value --- into a variant type.
     *)


    module Variant_choice_assoc(P: sig
                                    include CLI_DECL
                                    type t
                                    val assoc_map: (string * t) list
                                    val default : t
                                end) : GENERIC with type t = P.t
    (** Like [Variant_choice] but with automatically generated [to_string] and
        [of_string] function from [assoc_map].
     *)


    module Variant_list(
               P:sig include CLI_DECL
                     include Sigs.STR_INJECTIBLE end):
    CHECKABLE with type t = P.t list
  end
end

(** {3 Functor}*)

(** Call [Cli.Make] to create a kind of command line namespace *)
module Make(D:DECL) : sig
  (** [is_enabled] is a switch that is automatically set. *)
  val is_enabled : unit -> bool

  include Cli_sig
end

(** Call [Cli.Options] instead if you just want to have dedicated options to a
 ** set of functionalities.
 **
 ** The difference with [Cli.Make] is the absence of a dedicated command line
 ** switch for this set of options.
 **
 ** For example [Cli.Make(struct let name = "foo" ... end)] will add a '-foo'
 ** global switch to the command line whereas [Cli.Options(struct let name =
 ** "foo" ... end)] will not.
 **
 ** Also, using [Cli.Options] entails that there should not be any associated
 ** functions to be automatically run at startup (i.e., no [Cli.Boot.enlist]
 ** call from inside the functionality kernel associated with this set of
 ** command-line switches).
 *)
module Options(D:DECL) : Cli_sig ;;

(** {2 Boot}
    This module collects the functions to be executed at startup time.
*)

module Boot : sig

  val enlist : name:string -> f:(unit -> unit) -> unit
  (** [enlist ~name ~f] enlists the named unit [name] to be run at startup with
      function [f].

      If [name] has already been reserved, it raises a [Failure].
   *)

  val run : unit -> unit
  (** [run ()] executes all enlisted startup points.

     The execution order is unspecified.
  *)


  val maybe_enable : string -> unit
 (** [maybe_enable switch] tries to find a switch to enable from the command
     line.
  *)
end


val parse: unit -> string list
(** [parse ()] parses options from the command line. *)

val parse_configuration_file : filename:string -> string list
(** [parse ()] parses options from configuration file [filename]. *)
