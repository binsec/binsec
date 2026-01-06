module Namespace =
  Binsec_cli.Cli.Make_from_logger
    (Shadow_stack.Log)
    (struct
      let name = "Shadow stack tutorial"
      let shortname = "tuto"
    end)

type mode =
  | Inline  (** Use standard DBA assertions *)
  | Builtin  (** Use new push and pop builtins *)

module Mode = Namespace.Builder.Variant_choice_assoc (struct
  type t = mode

  let name = "mode"

  let doc =
    "Use standard DBA assertions [inline] or new push and pop builtins \
     [builtin]"

  let default = Inline
  let assoc_map = [ ("inline", Inline); ("builtin", Builtin) ]
end)

let () =
  Binsec_cli_sse.Plugins.register ~is_enabled:Namespace.is_enabled (fun () ->
      match Mode.get () with
      | Inline -> (module Shadow_stack.Plugin_v1)
      | Builtin -> (module Shadow_stack.Plugin_v2))
