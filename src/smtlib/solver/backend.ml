module Bindings = Binsec_smtlib_bindings

type t =
  | None : t
  | Text : { session : (module Session.S with type arg = 'a); arg : 'a } -> t
  | Binding : {
      factory : (module Bindings.OPEN);
      complete_fold_ax_values : bool;
          (** [complete_fold_ax_values] is [true] if there is an explicit value
              for each accessed address, [false] when there can exist a missed
              default value. *)
    }
      -> t

let default () =
  match Bindings.bitwuzla_cxx with
  | Some factory -> Binding { factory; complete_fold_ax_values = false }
  | None -> (
      match Bindings.bitwuzla_c with
      | Some factory -> Binding { factory; complete_fold_ax_values = true }
      | None -> (
          match Bindings.z3 with
          | Some factory -> Binding { factory; complete_fold_ax_values = false }
          | None -> (
              match
                List.find Solver.ping [ Bitwuzla; Boolector; Yices; Z3; CVC4 ]
              with
              | solver ->
                  Text { session = (module Session.Spawn); arg = solver }
              | exception Not_found -> None)))
