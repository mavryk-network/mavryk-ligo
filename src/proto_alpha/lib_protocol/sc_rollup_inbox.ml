(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(**

   A Merkelized inbox represents a list of available messages. This
   list is decomposed into sublist of messages, one for each Tezos
   level greater than the level of the Last Finalized Commitment
   (LFC).

   This module is designed to:

   1. give a constant-time access to the number of available messages
   ;

   2. provide a space-efficient representation for proofs of inbox
   inclusions (only for inboxes obtained at the end of block
   validation) ;

   3. offer a constant-time update to add a new batch of messages in
   the inbox at the current level.

   To solve (1), we simply maintain the number of available messages
   in a field.

   To solve (2), we use a proof tree H which is inspired by skip
   lists: each node of this list is the hash of an inbox at level L
   and points to its predecessor L-1 but also to deeper ancestors
   (L - 100, L - 1000, etc). These extra pointers allow for more
   compact inclusion proof.

   To solve (3), we maintain a separate proof tree C for the sublist
   of the current level.

   The protocol maintains the number of available messages, the root
   hash of H, and the root hash of C.

   The rollup node needs to maintain a full representation for C and a
   partial representation for H back to the level of the LFC.

*)
open Raw_context

type error += Invalid_level_add_messages of Raw_level_repr.t

type error += Invalid_number_of_messages_to_consume of int64

let () =
  let open Data_encoding in
  register_error_kind
    `Temporary
    ~id:"sc_rollup_inbox.invalid_level_add_messages"
    ~title:"Internal error: Trying to add an message to a previous level inbox"
    ~description:
      "An inbox can only accept messages for its current level or for the next \
       levels."
    (obj1 (req "level" Raw_level_repr.encoding))
    (function Invalid_level_add_messages level -> Some level | _ -> None)
    (fun level -> Invalid_level_add_messages level) ;

  register_error_kind
    `Temporary
    ~id:"sc_rollup_inbox.consume_n_messages"
    ~title:"Internal error: Trying to consume a negative number of messages"
    ~description:
      "Sc_rollup_inbox.consume_n_messages must be called with a non negative \
       integer."
    (obj1 (req "n" int64))
    (function Invalid_number_of_messages_to_consume n -> Some n | _ -> None)
    (fun n -> Invalid_number_of_messages_to_consume n)

module type Context = sig
  type t

  val empty_current_level_messages : t -> Sc_rollup_repr.t -> t Lwt.t

  val get_current_level_messages :
    t -> Sc_rollup_repr.t -> Raw_context.tree Lwt.t

  val set_current_level_messages :
    t -> Sc_rollup_repr.t -> Raw_context.tree -> t Lwt.t

  val get_previous_levels_messages :
    t -> Sc_rollup_repr.t -> Raw_level_repr.t -> Raw_context.tree Lwt.t

  val set_previous_levels_messages :
    t -> Sc_rollup_repr.t -> Raw_level_repr.t -> Raw_context.tree -> t Lwt.t
end

module type S = sig
  type t

  type inbox_context

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  val empty : Sc_rollup_repr.t -> Raw_level_repr.t -> t

  val number_of_available_messages : t -> Z.t

  val consume_n_messages : int -> t -> t option tzresult

  val get_message :
    inbox_context ->
    t ->
    Raw_level_repr.t ->
    Z.t ->
    Raw_context.tree option Lwt.t

  val add_messages :
    inbox_context ->
    t ->
    Raw_level_repr.t ->
    string list ->
    (t * inbox_context) tzresult Lwt.t
end

module Make (Inbox_context : Context) = struct
  type t = {
    rollup : Sc_rollup_repr.t;
    message_counter : Z.t;
    nb_available_messages : int64;
    level : Raw_level_repr.t;
    hash : Proof.hash;
  }

  type inbox_context = Inbox_context.t

  let pp fmt inbox =
    Format.fprintf
      fmt
      {|
         rollup = %a
         level = %a
         hash  = %a
         nb_available_messages = %s
         message_counter = %a
    |}
      Sc_rollup_repr.Address.pp
      inbox.rollup
      Raw_level_repr.pp
      inbox.level
      Context_hash.pp
      inbox.hash
      (Int64.to_string inbox.nb_available_messages)
      Z.pp_print
      inbox.message_counter

  let encoding =
    Data_encoding.(
      conv
        (fun {rollup; message_counter; nb_available_messages; level; hash} ->
          (rollup, message_counter, nb_available_messages, level, hash))
        (fun (rollup, message_counter, nb_available_messages, level, hash) ->
          {rollup; message_counter; nb_available_messages; level; hash})
        (obj5
           (req "rollup" Sc_rollup_repr.encoding)
           (req "message_counter" n)
           (req "nb_available_messages" int64)
           (req "level" Raw_level_repr.encoding)
           (req "hash" Context_hash.encoding)))

  let number_of_available_messages inbox =
    Z.of_int64 inbox.nb_available_messages

  let empty rollup level =
    {
      rollup;
      level;
      message_counter = Z.zero;
      nb_available_messages = 0L;
      hash = Context_hash.hash_bytes [Bytes.empty];
    }

  let key_of_message message_index =
    Data_encoding.Binary.to_string_exn Data_encoding.z message_index

  let add_message (current_level_messages, inbox) payload =
    let message_index = inbox.message_counter in
    let key = key_of_message message_index in
    let message_counter = Z.succ inbox.message_counter in
    Tree.(add current_level_messages [key] (Bytes.of_string payload))
    >>= fun current_level_messages ->
    Lwt.return (current_level_messages, {inbox with message_counter})

  let get_message ctxt inbox level message_index =
    Inbox_context.get_previous_levels_messages ctxt inbox.rollup level
    >>= fun messages -> Tree.find_tree messages [key_of_message message_index]

  let inbox_hashing_scheme hash1 hash2 =
    Context_hash.(hash_bytes [to_bytes hash1; to_bytes hash2])

  let next_level ctxt inbox =
    let open Inbox_context in
    get_current_level_messages ctxt inbox.rollup
    >>= fun current_level_messages ->
    let hash =
      inbox_hashing_scheme inbox.hash (Tree.hash current_level_messages)
    in
    set_previous_levels_messages
      ctxt
      inbox.rollup
      inbox.level
      current_level_messages
    >>= fun ctxt ->
    empty_current_level_messages ctxt inbox.rollup >>= fun ctxt ->
    let inbox =
      {
        rollup = inbox.rollup;
        level = Raw_level_repr.succ inbox.level;
        hash;
        message_counter = inbox.message_counter;
        nb_available_messages = inbox.nb_available_messages;
      }
    in
    Lwt.return (inbox, ctxt)

  let rec add_messages ctxt inbox level messages :
      (t * Inbox_context.t) tzresult Lwt.t =
    let open Inbox_context in
    if Raw_level_repr.(level = inbox.level) then
      get_current_level_messages ctxt inbox.rollup
      >>= fun current_level_messages ->
      List.fold_left_s add_message (current_level_messages, inbox) messages
      >>= fun (current_level_messages, inbox) ->
      set_current_level_messages ctxt inbox.rollup current_level_messages
      >>= fun ctxt -> return (inbox, ctxt)
    else if Raw_level_repr.(level > inbox.level) then
      next_level ctxt inbox >>= fun (inbox, ctxt) ->
      add_messages ctxt inbox level messages
    else fail (Invalid_level_add_messages level)

  let consume_n_messages n ({nb_available_messages; _} as inbox) :
      t option tzresult =
    if Compare.Int.(n < 0) then
      error (Invalid_number_of_messages_to_consume (Int64.of_int n))
    else if Compare.Int64.(Int64.of_int n > nb_available_messages) then ok None
    else
      let nb_available_messages =
        Int64.(sub nb_available_messages (of_int n))
      in
      ok (Some {inbox with nb_available_messages})
end

module Raw_context_inbox : Context = struct
  type t = Raw_context.t

  let key s rollup =
    [Sc_rollup_repr.Address.to_bytes rollup |> Bytes.to_string; s]

  let current_level_messages_key = key "current_level_messages"

  let empty_current_level_messages ctxt rollup =
    Raw_context.remove ctxt (current_level_messages_key rollup)

  let get_tree_or_empty ctxt key =
    Raw_context.find_tree ctxt key >>= function
    | None -> Lwt.return (Raw_context.Tree.empty ctxt)
    | Some tree -> Lwt.return tree

  let get_current_level_messages ctxt rollup =
    get_tree_or_empty ctxt (current_level_messages_key rollup)

  let set_current_level_messages ctxt rollup msgs =
    Raw_context.add_tree ctxt (current_level_messages_key rollup) msgs

  let previous_level_messages_key level =
    Data_encoding.Binary.to_string_exn Raw_level_repr.encoding level |> key

  let get_previous_levels_messages ctxt rollup level =
    get_tree_or_empty ctxt (previous_level_messages_key level rollup)

  let set_previous_levels_messages ctxt rollup level msgs =
    Raw_context.add_tree ctxt (previous_level_messages_key level rollup) msgs
end

include Make (Raw_context_inbox)
