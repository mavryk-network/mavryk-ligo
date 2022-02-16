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

(** Merkelizing inbox for smart-contract rollups.

   {1 Overview}

   The inbox of a smart-contract rollup denotes the incoming messages
   of the rollup. This inbox is the source of truth about what
   operations are being published and have an effect on the rollup
   state. As such, the inbox completely determines the state of the
   rollup. Hence, if two claims disagree about the state of the
   rollup, there are only two possibilities: either these two claims
   correspond to two distinct interpretations of the same inbox ; or,
   these two claims differ on their views about the contents of the
   inbox itself. {!Sc_rollup_PVM_sem} is meant to arbitrate the first
   kind of conflicts while {!Sc_rollup_inbox} focuses on the second
   kind of conflicts.

   {1 Inbox messages}

   A message is a chunk of bytes. Messages are indexed using natural
   numbers.

   A message is said to be *consumed* when its processing has been
   finalized, that is, when no refutation about its insertion can
   happen anymore because the commitment that describes the effect of
   this message on the state is final. A message is said to be
   *available* (for dispute) if it is not consumed.

   A message processed by the rollup can be consumed or available. A
   message unprocessed by the rollup is always available.

   The number of available messages is bounded by
   {!Constants_repr.sc_rollup_max_available_messages}. When an inbox
   reaches the maximum number of available messages, the inbox is said
   to be full and cannot accept more messages. This limitation is
   meant to ensure that Merkle proofs about the inbox contents have a
   bounded size. (See next section.)

   {1 Merkelization of the inbox}

   As for the state of the {!Sc_rollup_PVM_sem}, the layer 1 does not
   have to store the entire inbox but only a compressed form
   (typically a low number of hashes) that witnesses its contents, so
   that one can check the validity of a proof about its contents. This
   saves space in the context of the layer 1 and is sufficient for the
   level 1 to provide a source of truth about the contents of the
   inbox at the current level.

   {1 A level-indexed chain of inboxes}

   By design, inboxes are logically indexed by Tezos levels. This is
   required to have a simple way to decide if two commitments are in
   conflict. (See {!Sc_rollup_commitment}.)

   A commitment included in the block at level L describes the effect
   of the messages of the inboxes with an index between a starting
   level L_0 and a stopping level L_1, both strictly inferior to
   L. The level L_0 must be the stopping level of its parent
   commitment.

   When a commitment is published, its state contains the inbox in a
   compressed form, which witnesses the assumption made about the
   messages available in the inbox of level L_1.

   To be valid, a commitment needs to embed an inbox at level L_1
   which is consistent with the inbox at level L stored in the layer 1
   context. So, it should be possible at any time to build a proof
   that a given inbox is a previous version at level L_1 of the inbox
   found at level L: these are called inclusion proofs.

   {1 Implications of a valid commitment inbox}

   Assuming that a commitment inbox is valid, every access to the
   contents of a message through the inbox embedded in the rollup
   state can be considered valid in the sense that it matches the
   official contents of the inbox.

   {1 Clients}

   This module is meant to be used both by the protocol and by the
   rollup node to maintain consistent inboxes on both side.

   To reduce the space consumption of rollups on the chain storage,
   the protocol only stores metadata about the inbox. The messages of
   the current level are kept in memory during block validation.  The
   messages of the previous levels are not kept in the context.  They
   can be retrieved from the chain history though. However, they are
   not accessible to the protocol.

   The rollup node must keep a more precise inbox to be able to
   produce Merkle proofs about the content of specific messages, at
   least during the refutation period.

   To cope with the discrepancy of requirements in terms of inbox
   storage while preserving a consistent Merkelization of message
   collections, this module is parameterized by {!Context} to
   abstract the messages of a given level. This way, we ensure that
   the hashing scheme between the node and the protocol is the same
   while letting the node and the protocol choose different storage
   strategies.

*)

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
  (** The type of the inbox for a smart-contract rollup. *)
  type t

  type inbox_context

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  (** [empty level] is an inbox started at some given [level] with no
      message at all. *)
  val empty : Sc_rollup_repr.t -> Raw_level_repr.t -> t

  (** [number_of_available_messages inbox] returns the number of
      messages that can be consumed in [inbox]. *)
  val number_of_available_messages : t -> Z.t

  (** [consume_n_messages n inbox] returns an inbox where [n] messages have
      been consumed, or [None] if there are strictly less than [n] messages
      available in [inbox].

      @raise Invalid_argument if [n <= 0]. *)
  val consume_n_messages : int -> t -> t option tzresult

  (** [get_message inbox n] returns [Answer (Some message)], the [n]-th
      message of the [inbox], if it is still available for
      dispute. Returns [Answer None] otherwise.

      This function returns [DontKnow] if the inbox does not store
      this piece of information (typically in the protocol).

      @raise Invalid_argument if [n <= 0].  *)

  val get_message :
    inbox_context ->
    t ->
    Raw_level_repr.t ->
    Z.t ->
    Raw_context.tree option Lwt.t

  (** [add_messages msg_list level inbox] adds [msg_list] to [inbox] at
      level [level] (preserving their order). *)
  val add_messages :
    inbox_context ->
    t ->
    Raw_level_repr.t ->
    string list ->
    (t * inbox_context) tzresult Lwt.t
end

module Make (Inbox_context : Context) :
  S with type inbox_context = Inbox_context.t

include S with type inbox_context = Raw_context.t
