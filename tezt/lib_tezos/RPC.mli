(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

include module type of RPC_core

include module type of RPC_legacy

(** RPC: [GET /network/connections]

    Result is a list of [(address, port)] pairs. *)
val get_connections : (string * int) list t

(** RPC: [GET /network/connections/<peer_id>]

    Result is the address and port of the given peer ID if connected.
    This RPC returns 404 Not Found if the peer ID is not connected. *)
val get_connection : string -> (string * int) t

val private_injection_operations :
  ?force:bool ->
  ?async:bool ->
  ops:Hex.t list ->
  unit ->
  [`OpHash of string] list t

(** RPC: [GET /chain/[chain]/blocks/[block]]

    [chain] defaults to ["main"].
    [block] defaults to ["head"].
*)
val get_block : ?chain:string -> ?block:string -> unit -> JSON.t t

(** RPC: [GET /chain/[chain]/blocks/[block]/metadata]

    [chain] defaults to ["main"].
    [block] defaults to ["head"].
*)
val get_block_metadata : ?chain:string -> ?block:string -> unit -> JSON.t t

(** A level and its hash *)
type block_descriptor = {block_hash : string; level : int}

(** RPC: [GET /chain/[chain]/levels/checkpoint]

    [chain] defaults to ["main"]. *)
val get_checkpoint : ?chain:string -> unit -> block_descriptor t

(** RPC: [GET /chain/[chain]/levels/savepoint]

    [chain] defaults to ["main"]. *)
val get_savepoint : ?chain:string -> unit -> block_descriptor t

(** RPC: [GET /chain/[chain]/levels/caboose]

    [chain] defaults to ["main"]. *)
val get_caboose : ?chain:string -> unit -> block_descriptor t
