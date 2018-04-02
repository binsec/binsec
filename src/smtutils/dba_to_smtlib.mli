(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** Convert some DBA structure to Smtlib *)

(** Raised if trying to convert DBA operators
    that don't have equivalent in smtlib2. The
    two operators that don't have equivalent are:
    {!const:Dba.LeftRotate} and {!const:Dba.RightRotate} that
    can take a variable shift value while smtlib2
    only support constant *)
exception NoSmtEquivalent

(** convert a DBA unary operator to a Smtlib
    unary operator *)
val unary: Dba.unary_op -> Smtlib2.smt_bv_unary

(** convert a DBA binary operator to a Smtlib
    binary operator *)
val binary: Dba.binary_op -> Smtlib2.smt_bv_binary