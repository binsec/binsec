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

include Cli.Make(
struct
  let name = "server"
  let shortname = "server"
end)


module Server_port = Builder.Integer(
struct
  let name = "port"
  let doc = "Set the port on which to listen"
  let default = 5570
end
)

module Backend_port = Builder.Integer(
struct
  let name = "backend-port"
  let doc = "Set the port for transfers"
  let default = 5580
end
)

module Host_ip = Builder.String_option(
struct
  let name = "host"
  let doc = "Set host ip address of PINSEC server"
end
)

module Workers = Builder.Integer(
struct
  let name = "n-workers"
  let default = 2
  let doc = "Set the number of workers"
end
)
