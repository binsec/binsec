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

open Server_callback

let get_time_string () =
  let open Unix in
  let date = gmtime (time ()) in
  let _month =
    match date.tm_mon with
    | 0 -> "Jan" | 1 -> "Feb" | 2 -> "Mar"
    | 3 -> "Apr" | 4 -> "May" | 5 -> "Jun"
    | 6 -> "Jul" | 7 -> "Aug" | 8 -> "Sep"
    | 9 -> "Oct" | 10 -> "Nov" | 11 -> "Dec"
    | _ -> assert false in
  Format.asprintf "%d:%02d:%02d" (date.tm_hour+1) date.tm_min date.tm_sec

let single_thread_server_loop (): unit =
  Network_io.bind !Options.port;
  let items = ZMQ.Poll.mask_of [| (!Network_io.frontend_socket, ZMQ.Poll.In) |] in
  try
    while true do
      let pollResults = ZMQ.Poll.poll ~timeout:100 items in
      let frontResults = pollResults.(0) in
      match frontResults with
      | Some _ ->
        (* Retrieve client command *)
        let content = Network_io.receive_with_identity true in
        begin
          match content with
          | ident :: cmd :: msg :: _ ->
            Logger.debug "%s [server]: received %s from %s!" (get_time_string()) cmd ident;
            Server_callback.request_dispatcher ident cmd msg
          | _ -> assert false
        end
      | None -> ();
    done

  with Stop -> ();
    Network_io.close_and_terminate_socket ()
