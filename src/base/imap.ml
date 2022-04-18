(* TODO: poor man interval map -- sorted list *)

include Map.Make (struct
  type t = Z.t * Z.t

  let compare (lo, hi) (lo', hi') =
    if Z.lt hi lo' then -1 else if Z.gt lo hi' then 1 else 0
end)

let add ~base size x t = add (base, Z.pred (Z.add base (Z.of_int size))) x t

let mem a t = mem (a, a) t

let find a t = find (a, a) t
