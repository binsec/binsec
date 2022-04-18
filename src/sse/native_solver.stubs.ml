module Solver () : Solver_sig.S = struct
  type result = Sat | Unsat | Unknown

  type memory = unit

  type term = unit

  type access = Select of term * int | Store of term

  let put _ _ = assert false

  let bind _ _ _ = assert false

  let get _ = assert false

  let add _ = assert false

  let neq _ _ = assert false

  let get_memory _ = assert false

  let get_at _ _ = assert false

  let get_value _ = assert false

  let succ _ = assert false

  let check_sat _ = assert false

  let close () = ()
end
