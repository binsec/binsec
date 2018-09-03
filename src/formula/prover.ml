open Formula_options ;;

type executable = string ;;
type arguments  = string array ;;

module Command = struct
  type t = {
      executable : executable;
      arguments  : arguments;
    }
  ;;

  let to_string t =
    let b = Buffer.create 128 in
    Buffer.add_string b t.executable;
    Array.iter (fun sw -> Buffer.(add_char b ' '; add_string b sw;)) t.arguments;
    Buffer.contents b
  ;;
end

type t = Formula_options.solver;;

let is_boolector = function
  | Boolector -> true
  | Z3 | CVC4 | Yices -> false
;;

let is_yices = function
  | Yices -> true
  | Z3 | CVC4 | Boolector -> false
;;
             

let executable = function
  | Boolector -> "boolector"
  | CVC4 -> "cvc4"
  | Z3 -> "z3"
  | Yices -> "yices-smt2"
;;

let name_of = executable

let default_arguments = function
  | Boolector -> [ "-m"; "-x"; "--smt2-model"; ]
  | CVC4 -> [ "--lang=smt2"; "--produce-models"; ]
  | Z3 -> [ "--smt2"; ]
  | Yices -> []
;;

let incremental_a = function
  | Boolector -> "-i"
  | CVC4 -> "--incremental"
  | Z3   -> "-in"
  | Yices -> "--incremental"
;;

let arguments prover =
  match Formula_options.Solver.Options.get_opt () with
  | Some opts -> [ opts ]
  | None -> default_arguments prover
;;

let timeout_s time = function
  | Boolector | Yices -> time
  | Z3 | CVC4 -> 1000 * time
;;

let time_switch time prover =
  assert (time > 0);
  let t = timeout_s time prover in
  let t_switch =
    match prover with
    | Boolector -> "-t "
    | CVC4 -> "--tlimit-per="
    | Z3 -> "-t:"
    | Yices -> "--timeout="
  in t_switch ^ string_of_int t
;;

let add_switch_if p switch switches =
  if p then switch :: switches else switches
;;

let command ?(incremental=false) timeout prover =
  let open Command in
  let arguments =
    arguments prover
    |> add_switch_if incremental (incremental_a prover) 
    |> add_switch_if (timeout > 0) (time_switch timeout prover) 
    |> Array.of_list 
  in { executable = executable prover; arguments; }
;;

let command_string ?(incremental=false) timeout prover =
  command ~incremental timeout prover |> Command.to_string
;;

