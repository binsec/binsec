type executable = string ;;
type arguments  = string array ;;

module Command : sig
  type t = private {
     executable : executable;
     arguments  : arguments;
  } ;;

  val to_string : t -> string ;;
end

type t = Formula_options.solver ;;

val is_boolector : t -> bool ;;
val is_yices : t -> bool ;;

(** {2 Accessors} *)
val name_of : t -> string ;;

val command : ?incremental:bool -> int -> t -> Command.t ;;

val command_string : ?incremental:bool -> int -> t -> string ;;

val timeout_s : int -> t -> int ;;
