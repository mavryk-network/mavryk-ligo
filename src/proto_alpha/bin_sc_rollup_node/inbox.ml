(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Lwt_result_syntax
open Protocol
open Alpha_context
module Block_services = Block_services.Make (Protocol) (Protocol)

let head_processing_failure e =
  Format.eprintf
    "Error during head processing: @[%a@]"
    Error_monad.(TzTrace.pp_print_top pp)
    e ;
  Lwt_exit.exit_and_raise 1

let unstarted_failure () =
  Format.eprintf "Sc rollup node inbox is not started.\n" ;
  Lwt_exit.exit_and_raise 1

module State = struct
  module Store = struct
    module Messages = Store.Make_append_only_map (struct
      let path = ["messages"]

      let keep_last_n_entries_in_memory = 10

      type key = Block_hash.t

      let string_of_key = Block_hash.to_b58check

      type value = string list

      let value_encoding = Data_encoding.(list string)
    end)

    module Inboxes = Store.Make_append_only_map (struct
      let path = ["inboxes"]

      let keep_last_n_entries_in_memory = 10

      type key = Block_hash.t

      let string_of_key = Block_hash.to_b58check

      type value = Sc_rollup.Inbox.t

      let value_encoding = Sc_rollup.Inbox.encoding
    end)
  end

  let add_messages = Store.Messages.add

  let add_inbox = Store.Inboxes.add

  let inbox_exists = Store.Inboxes.mem

  let inbox_of_hash = Store.Inboxes.get

  let (set_sc_rollup_address, get_sc_rollup_address) =
    let sc_rollup_address = ref None in
    ( (fun x -> sc_rollup_address := Some x),
      fun () ->
        match !sc_rollup_address with
        | None -> unstarted_failure ()
        | Some a -> a )
end

let get_messages cctxt head rollup =
  let open Block_services in
  Block_services.Operations.operations
    cctxt
    ~chain:`Main
    ~block:(`Level (snd head))
    ()
  >>=? fun operations ->
  let is_add_message = function
    | Contents
        (Manager_operation
          {operation = Sc_rollup_add_messages {rollup = rollup'; messages}; _})
      when Sc_rollup.Address.(rollup' = rollup) ->
        messages
    | _ -> []
  in
  let process_contents {protocol_data = Operation_data {contents; _}; _} =
    let operations = Operation.to_list (Contents_list contents) in
    List.(map is_add_message operations |> concat)
  in
  let process_operation l = List.map process_contents l |> List.concat in
  let operations = List.map process_operation operations in
  return List.(concat operations)

let process_head cctxt Layer1.(Head {level; hash = head_hash} as head) =
  let rollup = State.get_sc_rollup_address () in
  let open Lwt_syntax in
  get_messages cctxt (head_hash, level) rollup >>= function
  | Error e -> head_processing_failure e
  | Ok messages ->
      Inbox_event.get_messages head_hash level (List.length messages)
      >>= fun () ->
      State.add_messages head_hash messages >>= fun () ->
      (*

          We compute the inbox of this block using the inbox of its
          predecessor. That way, the computation of inboxes is robust
          to chain reorganization.

      *)
      let* predecessor = Layer1.predecessor head in
      let* inbox = State.inbox_of_hash predecessor in
      let level = Raw_level_repr.of_int32_exn level in
      let inbox =
        Sc_rollup.Inbox.add_messages_uncarbonated messages level inbox
      in
      State.add_inbox head_hash inbox

let update cctxt chain_event =
  let open Layer1 in
  match chain_event with
  | SameBranch {new_head; intermediate_heads} ->
      List.iter_s (process_head cctxt) intermediate_heads >>= fun () ->
      process_head cctxt new_head
  | Rollback {new_head = _} ->
      (*

          Since [process_head] is robust to chain reorganizations, we do
          need a specific treatment of [Rollback] events.

      *)
      Lwt.return ()

let inbox_of_hash = State.inbox_of_hash

let start sc_rollup_address =
  State.set_sc_rollup_address sc_rollup_address ;
  Inbox_event.starting () >>= fun () ->
  State.inbox_exists Layer1.genesis_hash >>= function
  | false -> State.add_inbox Layer1.genesis_hash Sc_rollup.Inbox.empty
  | true -> Lwt.return ()
