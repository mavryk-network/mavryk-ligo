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

type error +=
  | (* `Temporary *)
      Sc_rollup_does_not_exist of Sc_rollup_repr.t
  | (* `Temporary *)
      Sc_rollup_already_staked
  | (* `Temporary *)
      Sc_rollup_not_staked_on_final
  | (* `Temporary *)
      Sc_rollup_staker_backtracked
  | (* `Temporary *)
      Sc_rollup_unknown_commitment of
      Sc_rollup_repr.Commitment_hash.t
  | (* `Temporary *)
      Sc_rollup_parent_not_final
  | (* `Temporary *)
      Sc_rollup_too_recent
  | (* `Temporary *)
      Sc_rollup_no_stakers
  | (* `Temporary *)
      Sc_rollup_disputed
  | (* `Temporary *)
      Sc_rollup_no_conflict
  | (* `Temporary *)
      Sc_rollup_not_staked
  | (* `Temporary *)
      Sc_rollup_remove_final

(** [originate context ~kind ~boot_sector] produces an address [a] for
   a smart contract rollup using the origination nonce found in
   [context]. This function also initializes the storage with a new
   entry indexed by [a] to remember the [kind] of the rollup at
   address [a] and also to remember its [boot_sector].

   Also returns the number of allocated bytes.  *)
val originate :
  Raw_context.t ->
  kind:Sc_rollup_repr.Kind.t ->
  boot_sector:Sc_rollup_repr.PVM.boot_sector ->
  (Sc_rollup_repr.Address.t * Z.t * Raw_context.t) tzresult Lwt.t

(** [kind context address] returns [Some kind] iff [address] is an
    existing rollup of some [kind]. Returns [None] if [address] is
    not the address of an existing rollup. *)
val kind :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Kind.t option tzresult Lwt.t

(** [add_message context rollup msg] adds [msg] to [rollup]'s inbox.

    This function returns the updated context as well as the size diff. *)
val add_messages :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  string list ->
  (Sc_rollup_inbox.t * Z.t * Raw_context.t) tzresult Lwt.t

(** [inbox context rollup] returns the current state of the inbox. *)
val inbox :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  (Sc_rollup_inbox.t * Raw_context.t) tzresult Lwt.t

(** [deposit_stake context rollup staker] stakes [staker] at the last
    final commitment, freezing [sc_rollup_deposit] from [staker]'s account
    balance.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_already_staked] if [staker] is already staked}
      {li [Sc_rollup_staker_funds_too_low] if [staker] does not have enough funds to cover the deposit}
    }

    This should usually be followed by [refine_stake] to stake on a
    specific commitment.

    This function does not authenticate the staker. *)
val deposit_stake :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  (unit * Raw_context.t) tzresult Lwt.t

(** [withdraw_stake context rollup staker] removes [staker] and returns
    any deposit previously frozen by [deposit_stake].

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_not_staked_on_final] if [staker] is not staked on the last final commitment}
    }

    By design, the operation wrapping this should {i not} be authenticated,
    as it may be necessary for nodes on the honest branch to refund stakers on
    the LFC. They must do so by using [withdraw_stake] as they are implicitly
    staked on the LFC and can not dispute it. *)
val withdraw_stake :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  (unit * Raw_context.t) tzresult Lwt.t

(** [refine_stake context rollup level staker commitment] moves the stake of
    [staker] to [commitment]. Because we do not assume any form of coordination
    between validators, we do not distinguish between {i adding new}
    commitments and {i staking on existing commitments}.  The storage of
    commitments is content-addressable to minimize storage duplication.

    The first time a commitment hash is staked on, it is assigned a deadline,
    which is counted in Tezos blocks (levels). Further stakes on the block does
    not affect the deadline. The commitment can not be finalized before the
    deadline has expired. Note that if a commitment is removed due to disputes
    and then re-entered, a new deadline may be assigned. Assuming one honest
    staker is always available, this only affects invalid commitments.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_not_staked]}
      {li [Sc_rollup_staker_backtracked] if [staker] is not staked on an ancestor of [commitment]}
      {li [Sc_rollup_unknown_commitment] if the parent of the given commitment does not exist}
    }

    This function does not authenticate the staker. *)
val refine_stake :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Raw_level_repr.t ->
  Sc_rollup_repr.Staker.t ->
  Sc_rollup_repr.Commitment.t ->
  (Sc_rollup_repr.Commitment_hash.t * Raw_context.t) tzresult Lwt.t

(** [last_final_commitment context rollup] returns the current last final
    commitment of the rollup.

    If no commitments have been finalized, the rollup is said to be in a
    pre-boot state, and [last_final_commitment = Commitment_hash.zero].

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist} *)
val last_final_commitment :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  (Sc_rollup_repr.Commitment_hash.t * Raw_context.t) tzresult Lwt.t

(** [finalize_commitment context rollup commitment] finalizes the given
    commitment.

    If successful, [last_final_commitment] is set to the given [commitment] and
    the appropriate amount of inbox messages is consumed. Stakers directly
    staked on the last final commitment become staked on [commitment].

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_unknown_commitment] if [commitment] does not exist}
      {li [Sc_rollup_parent_not_final] if [commitment] is not the child of the last final commitment}
      {li [Sc_rollup_too_recent] if [commitment] has not passed its deadline}
      {li [Sc_rollup_no_stakers] if there are zero stakers}
      {li [Sc_rollup_disputed] if at least one staker is not staked on [commitment]}
    } *)
val finalize_commitment :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Raw_level_repr.t ->
  Sc_rollup_repr.Commitment_hash.t ->
  (unit * Raw_context.t) tzresult Lwt.t

type conflict_point =
  Sc_rollup_repr.Commitment_hash.t * Sc_rollup_repr.Commitment_hash.t

(** [get_conflict_point context rollup staker1 staker2] returns the first point
    of disagreement between the given stakers. The returned commitments are
    distinct, and have the same [parent] commitment.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_no_conflict] if [staker1] is staked on an ancestor of the commitment staked on by [staker2], or vice versa}
    } *)
val get_conflict_point :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  Sc_rollup_repr.Staker.t ->
  (conflict_point * Raw_context.t) tzresult Lwt.t

(** [get_commitment context rollup commitment_hash] returns the commitment with the given hash.

    May fail with:
    {ul
      {li [Sc_rollup_unknown_commitment] if [commitment] does not exist}
    } *)
val get_commitment :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Commitment_hash.t ->
  (Sc_rollup_repr.Commitment.t * Raw_context.t) tzresult Lwt.t

(** [remove_staker context rollup staker] forcibly removes the given [staker]
    and confiscates their frozen deposits. Returns the confiscated amount.

    Any commitments no longer staked on are removed and storage reclaimed by
    [remove_staker]. Because of this there is no need to explicitly reject
    commitments.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_not_staked] if [staker] is not staked}
      {li [Sc_rollup_remove_final] if [staker] is staked on a finalized commitment}
    } *)
val remove_staker :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  (unit * Raw_context.t) tzresult Lwt.t
