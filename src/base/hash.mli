type t = private int

external seed : int -> t = "%identity" [@@noalloc]

external fold_int : t -> int -> t
  = "cstubs_hash_fold_int" "cstubs_hash_fold_int_untagged"
  [@@noalloc] [@@untagged]

external fold_string : (t[@untagged]) -> string -> (t[@untagged])
  = "cstubs_hash_fold_string" "cstubs_hash_fold_string_untagged"
  [@@noalloc]

val return : t -> int [@@inline]
