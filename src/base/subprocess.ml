(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
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

external set_pdeathsig : int -> int
  = "caml_subprocess_set_pdeathsig" "native_subprocess_set_pdeathsig"
[@@untagged] [@@noalloc]

let handle_pdeathsig = function
  | None -> ()
  | Some signal ->
      if set_pdeathsig signal = -1 then Unix.kill (Unix.getpid ()) Sys.sigkill

type t = {
  mutable valid : bool;
  pid : int;
  stdin : out_channel;
  stdout : in_channel;
  stderr : in_channel;
}

open Unix

let spawn ?pdeathsig args =
  let inr, inw = pipe ~cloexec:true () in
  let outr, outw =
    try pipe ~cloexec:true ()
    with e ->
      close inr;
      close inw;
      raise e
  in
  let errr, errw =
    try pipe ~cloexec:true ()
    with e ->
      close inr;
      close inw;
      close outr;
      close outw;
      raise e
  in
  match fork () with
  | 0 ->
      (* in child *)
      dup2 ~cloexec:false inr Unix.stdin;
      dup2 ~cloexec:false outw Unix.stdout;
      dup2 ~cloexec:false errw Unix.stderr;
      handle_pdeathsig pdeathsig;
      execvp args.(0) args
  | pid ->
      close inr;
      close outw;
      close errw;
      {
        valid = true;
        pid;
        stdin = out_channel_of_descr inw;
        stdout = in_channel_of_descr outr;
        stderr = in_channel_of_descr errr;
      }
  | exception e ->
      close inr;
      close inw;
      close outr;
      close outw;
      close errr;
      close errw;
      raise e

let rec waitpid_non_intr pid =
  try waitpid [] pid with Unix_error (EINTR, _, _) -> waitpid_non_intr pid

let please f x = try f x with Sys_error _ | Unix_error _ -> ()

let close t =
  if t.valid = false then
    raise (Invalid_argument "process has already been closed");
  t.valid <- false;
  please close_out t.stdin;
  please close_in t.stdout;
  please close_in t.stderr;
  snd (waitpid_non_intr t.pid)

let pid { pid; _ } = pid
let stdin { stdin; _ } = stdin
let stdout { stdout; _ } = stdout
let stderr { stderr; _ } = stderr
