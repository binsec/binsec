type t = int

external seed : int -> t = "%identity" [@@noalloc]

external fold_int : t -> int -> t
  = "cstubs_hash_fold_int" "cstubs_hash_fold_int_untagged"
  [@@noalloc] [@@untagged]

external fold_string : (t[@untagged]) -> string -> (t[@untagged])
  = "cstubs_hash_fold_string" "cstubs_hash_fold_string_untagged"
  [@@noalloc]

let return h =
  let h = h lxor (h lsr 16) in
  let h = h * 0x85ebca6b in
  let h = h lxor (h lsr 13) in
  let h = h * 0xc2b2ae35 in
  let h = h lxor (h lsr 16) in
  h land 0x3fffffff
  [@@inline]
