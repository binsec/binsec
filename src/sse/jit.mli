module Env (S : Types.STATE) : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val assign : Dba.Var.t -> Dba.Expr.t -> t -> t

  val clobber : Dba.Var.t -> t -> t

  val forget : Dba.Var.t -> t -> t

  val load :
    Dba.Var.t -> string option -> Machine.endianness -> Dba.Expr.t -> t -> t

  val store :
    string option ->
    Machine.endianness ->
    addr:Dba.Expr.t ->
    Dba.Expr.t ->
    t ->
    t

  val eval : Dba.Expr.t -> t -> Cse.Expr.t * t

  val commit : t -> S.t -> S.t

  val compute : Cse.Expr.t -> t -> S.t -> S.t * S.Value.t
end
