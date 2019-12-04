(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

type zmq_socket = [`Router] Zmq.Socket.t

let zmq_context = ref (Zmq.Context.create ())
let frontend_socket = ref (Zmq.Socket.create !zmq_context Zmq.Socket.router)

let send_client_message ident cmd ?(block=true) data =
  Zmq.Socket.send_all ~block:block !frontend_socket [ident; cmd; data]

let connect_to_pin ip port =
  Zmq.Socket.connect !frontend_socket ("tcp://"^ip^":"^(string_of_int port))

let close_and_terminate_socket () =
  Zmq.Socket.close !frontend_socket;
  Zmq.Context.terminate !zmq_context

let bind port =
  let port_frontend = string_of_int port in
  Zmq.Socket.bind !frontend_socket ("tcp://*:"^port_frontend)

let receive blocking =
  let data = Zmq.Socket.recv_all ~block:blocking !frontend_socket in
  match data with
  | _ :: tail -> tail
  | _ -> Printf.printf "Malformed message received !"; data

let receive_with_identity blocking =
  Zmq.Socket.recv_all ~block:blocking !frontend_socket

let log_to_zmq (module L: Logger.S) flag identity =
  let send = send_client_message identity ~block:false "LOG" in
  L.set_zmq_logging_only ~send flag
