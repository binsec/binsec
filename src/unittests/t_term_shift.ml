(* Regression for distributing bitwise operations over left shifts. *)
module Label = struct
  type t = unit
  let compare () () = 0
  let equal () () = true
  let hash () = 0
end

module E = Term.Make (Label) (Label)

let bv n = E.constant (Bitvector.of_int ~size:8 n)

let check op concrete_op =
  let x = E.var "x" 8 () in
  for shift = 1 to 7 do
    for mask = 0 to 255 do
      let expr = op (E.shift_left x (bv shift)) (bv mask) in
      for value = 0 to 255 do
        let actual = E.map (fun _ _ () -> bv value)
            (fun _ _ _ () -> assert false) expr in
        let expected = bv (concrete_op ((value lsl shift) land 255) mask) in
        if not (E.is_equal actual expected) then
          failwith (Printf.sprintf "shift=%d mask=%d value=%d" shift mask value)
      done
    done
  done

let () =
  check E.logxor (lxor);
  check E.logor (lor)
