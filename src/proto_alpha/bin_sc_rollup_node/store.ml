(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Conf = struct
  let entries = 32

  let stable_hash = 256

  let inode_child_order = `Seeded_hash
end

module IStore =
  Irmin_pack.KV (Irmin_pack.Version.V2) (Conf) (Irmin.Contents.String)

type path = string list

let master = ref None

let set_master m = master := Some m

let get_master () =
  match !master with Some master -> master | None -> assert false

let loaded_status = ref false

let loaded () = !loaded_status

let load configuration =
  let open Configuration in
  IStore.Repo.v (Irmin_pack.config configuration.data_dir) >>= fun repo ->
  IStore.master repo >>= fun master ->
  loaded_status := true ;
  return @@ set_master master

let close configuration =
  let open Configuration in
  IStore.Repo.v (Irmin_pack.config configuration.data_dir) >>= fun repo ->
  IStore.Repo.close repo

let info message =
  let date = Unix.gettimeofday () |> int_of_float |> Int64.of_int in
  Irmin.Info.v ~author:"Tezos smart-contract rollup node" ~date message

module Make_append_only_map (P : sig
  val path : path

  val keep_last_n_entries_in_memory : int

  type key

  val string_of_key : key -> string

  type value

  val value_encoding : value Data_encoding.t
end) =
struct
  (* Ignored for now. *)
  let _ = P.keep_last_n_entries_in_memory

  let path_key = IStore.Key.v P.path

  let make_key key = IStore.Key.rcons path_key (P.string_of_key key)

  let mem key = IStore.mem (get_master ()) (make_key key)

  let decode_value encoded_value =
    Lwt.return
    @@ Data_encoding.Binary.of_string_exn P.value_encoding encoded_value

  let get key = IStore.get (get_master ()) (make_key key) >>= decode_value

  let add key value =
    mem key >>= fun already_exists ->
    assert (not already_exists) ;
    let encoded_value =
      Data_encoding.Binary.to_string_exn P.value_encoding value
    in
    let info () = info (String.concat "/" P.path ^ P.string_of_key key) in
    IStore.set_exn ~info (get_master ()) (make_key key) encoded_value
end

module Make_mutable_value (P : sig
  val path : path

  type value

  val value_encoding : value Data_encoding.t
end) =
struct
  let path_key = IStore.Key.v P.path

  let decode_value encoded_value =
    Lwt.return
    @@ Data_encoding.Binary.of_string_exn P.value_encoding encoded_value

  let set value =
    let encoded_value =
      Data_encoding.Binary.to_string_exn P.value_encoding value
    in
    let info () = info (String.concat "/" P.path) in
    IStore.set_exn ~info (get_master ()) path_key encoded_value

  let get () = IStore.get (get_master ()) path_key >>= decode_value
end
