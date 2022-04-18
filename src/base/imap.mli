(* TODO: poor man interval map -- sorted list *)

type 'a t

val empty : 'a t

val add : base:Z.t -> int -> 'a -> 'a t -> 'a t
(** [add ~base size value t]
    add a new pair (\[[base] .. [base] + [size]\[, [value]) in [t]
*)

val mem : Z.t -> 'a t -> bool
(** [mem index t]
    check if [index] is in [t].
*)

val find : Z.t -> 'a t -> 'a
(** [find index t]
    lookup [index] in [t].

    @raise Not_found if [index] is not in [t].
*)

val iter : (Z.t * Z.t -> 'a -> unit) -> 'a t -> unit
