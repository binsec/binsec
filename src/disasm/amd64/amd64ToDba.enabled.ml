(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2025                                               *)
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

include Cli.Options (struct
  let name = "amd64"
  let shortname = name
end)

module Parser = Unisim_helper.Make (Logger)

let decode addr bytes =
  Amd64dba.decode ~m64:true ~addr bytes |> Parser.parse_message

let read_sample_size = 15
(* This value is chosen to be large enough to get a sure opcode hit *)

let fill_buffer =
  let rec aux i bytes reader =
    if i = Bytes.length bytes then i
    else
      try
        Bytes.set bytes i (Char.unsafe_chr (Lreader.Read.u8 reader));
        aux (i + 1) bytes reader
      with _ -> i
  in
  aux 0

let decode_from_reader addr reader =
  let bytes = Bytes.create read_sample_size in
  let n = fill_buffer bytes reader in
  let bytes = String_utils.to_hex (Bytes.unsafe_to_string bytes) in
  let ins, dhunk, status = decode addr bytes in
  (match status with
  | None ->
      Lreader.rewind reader (n - Size.Byte.to_int ins.size);
      Parser.incr_success ins.opcode
  | Some Undefined ->
      Lreader.rewind reader n;
      Parser.incr_error Undefined bytes
  | Some err ->
      Lreader.rewind reader (n - Size.Byte.to_int ins.size);
      Parser.incr_error err ins.opcode);
  (ins, dhunk)

let decode reader (addr : Virtual_address.t) =
  let res = decode_from_reader (Virtual_address.to_int64 addr) reader in
  Logger.debug ~level:5 "@[%a@]" Dhunk.pp (snd res);
  Logger.debug ~level:3 "@[%a@]" Parser.pp_statistics ();
  res

let cached_decode reader =
  let h = Virtual_address.Htbl.create 7 in
  fun (addr : Virtual_address.t) ->
    match Virtual_address.Htbl.find h addr with
    | res -> res
    | exception Not_found ->
        let res = decode reader addr in
        Virtual_address.Htbl.add h addr res;
        res

let () = Disasm_core.register_decoder Machine.amd64 decode Fun.id
