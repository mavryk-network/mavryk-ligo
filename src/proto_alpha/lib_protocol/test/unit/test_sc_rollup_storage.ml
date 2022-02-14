(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** Testing
    -------
    Component:  Protocol Sc_rollup_storage
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
      -- test "^\[Unit\] Sc_rollup_storage.ml$"
    Subject:    Tests for the gas monad module
*)

open Protocol
open Lwt_result_syntax

(** Lift a computation using using environment errors to use shell errors. *)
let lift k = Lwt.map Environment.wrap_tzresult k

(** Trivial assertion.

    By convention, context is passed linearly as [ctxt].  This takes a context
    argument to allow this.
    *)
let assert_true _ctxt = return ()

(** Assert that the computation fails with the given message. *)
let assert_fails_with ~loc k msg =
  let expected_error_msg = "Error:\n  " ^ msg ^ "\n" in
  k >>= function
  | Ok _ -> Stdlib.failwith "Expected failure"
  | Error err ->
      let actual_error_msg : string =
        Format.asprintf "%a" Environment.Error_monad.pp_trace err
      in
      Assert.equal_string ~loc expected_error_msg actual_error_msg

(** Assert commitment hash equality.

    By convention, context is passed linearly as [ctxt].  This takes a context
    argument to allow this.
    *)
let assert_commitment_hash_equal ~loc _ctxt x y =
  Assert.equal
    ~loc
    Sc_rollup_repr.Commitment_hash.equal
    "Compare commitment hash"
    Sc_rollup_repr.Commitment_hash.pp
    x
    y

let new_context =
  let* (b, _contracts) = Context.init 1 in
  Incremental.begin_construction b >|=? fun inc ->
  let state = Incremental.validation_state inc in
  let ctxt = state.ctxt in
  (* Necessary to originate rollups. *)
  let ctxt = Alpha_context.Origination_nonce.init ctxt Operation_hash.zero in
  Alpha_context.Internal_for_tests.to_raw ctxt

let new_sc_rollup ctxt =
  let* (rollup, _size, ctxt) =
    Sc_rollup_storage.originate
      ctxt
      ~kind:Example_arith
      ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
  in
  return (rollup, ctxt)

let test_deposit_to_missing_rollup () =
  let* ctxt = new_context in
  let rollup = Sc_rollup_repr.Address.hash_bytes [] in
  let staker = Sc_rollup_repr.Staker.zero in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.deposit_stake ctxt rollup staker)
    "Rollup scr1Ew52VCdi6nF1JuokRGMqfmSeiAEXymW2m does not exist"

let test_initial_state_is_pre_boot () =
  let* ctxt = new_context in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let* (lfc, ctxt) =
    lift @@ Sc_rollup_storage.last_final_commitment ctxt rollup
  in
  assert_commitment_hash_equal
    ~loc:__LOC__
    ctxt
    lfc
    Sc_rollup_repr.Commitment_hash.zero

let test_deposit_to_existing_rollup () =
  let* ctxt = new_context in
  lift
  @@ let* (rollup, ctxt) = new_sc_rollup ctxt in
     let staker =
       Signature.Public_key_hash.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker in
     assert_true ctxt

let test_removing_staker_from_lfc_fails () =
  let* ctxt = new_context in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Signature.Public_key_hash.of_b58check_exn
      "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.remove_staker ctxt rollup staker)
    "Can not remove a final commitment."

let test_deposit_then_withdraw () =
  let* ctxt = new_context in
  lift
  @@ let* (rollup, ctxt) = new_sc_rollup ctxt in
     let staker =
       Signature.Public_key_hash.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker in
     let* ctxt = Sc_rollup_storage.withdraw_stake ctxt rollup staker in
     assert_true ctxt

let test_withdraw_when_not_staked () =
  let* ctxt = new_context in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Signature.Public_key_hash.of_b58check_exn
      "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.withdraw_stake ctxt rollup staker)
    "Unknown staker."

let test_withdrawing_twice () =
  let* ctxt = new_context in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Signature.Public_key_hash.of_b58check_exn
      "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  let* ctxt = lift @@ Sc_rollup_storage.withdraw_stake ctxt rollup staker in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.withdraw_stake ctxt rollup staker)
    "Unknown staker."

let number_of_messages_exn n =
  match Sc_rollup_repr.Number_of_messages.of_int32 n with
  | Some x -> x
  | None -> Stdlib.failwith "Bad Number_of_messages"

let number_of_ticks_exn n =
  match Sc_rollup_repr.Number_of_ticks.of_int32 n with
  | Some x -> x
  | None -> Stdlib.failwith "Bad Number_of_ticks"

let test_deposit_then_refine () =
  let* ctxt = new_context in
  let level = (Raw_context.current_level ctxt).level in
  lift
  @@ let* (rollup, ctxt) = new_sc_rollup ctxt in
     let staker =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker in
     let commitment =
       Sc_rollup_repr.Commitment.
         {
           predecessor = Sc_rollup_repr.Commitment_hash.zero;
           inbox_level = Raw_level_repr.of_int32_exn 125l;
           number_of_messages = number_of_messages_exn 3l;
           number_of_ticks = number_of_ticks_exn 1232909l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (_node, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup level staker commitment
     in
     assert_true ctxt

let test_finalize () =
  let* ctxt = new_context in
  let level_0 = (Raw_context.current_level ctxt).level in
  let level_after = Raw_level_repr.add level_0 20_160 in
  lift
  @@ let* (rollup, ctxt) = new_sc_rollup ctxt in
     let staker =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker in
     let commitment =
       Sc_rollup_repr.Commitment.
         {
           predecessor = Sc_rollup_repr.Commitment_hash.zero;
           inbox_level = Raw_level_repr.of_int32_exn 125l;
           number_of_messages = number_of_messages_exn 3l;
           number_of_ticks = number_of_ticks_exn 1232909l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (c1, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup level_0 staker commitment
     in
     let* ctxt =
       Sc_rollup_storage.finalize_commitment ctxt rollup level_after c1
     in
     assert_true ctxt

let test_finalize_fail_too_recent () =
  let* ctxt = new_context in
  let level = (Raw_context.current_level ctxt).level in
  let level_1999 = Raw_level_repr.add level 20_159 in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 125l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup level staker commitment
  in
  let* () =
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_storage.finalize_commitment ctxt rollup level c1)
      "Attempted to finalize a commitment before its refutation deadline."
  in
  let* () =
    assert_fails_with
      ~loc:__LOC__
      (Sc_rollup_storage.finalize_commitment ctxt rollup level_1999 c1)
      "Attempted to finalize a commitment before its refutation deadline."
  in
  assert_true ctxt

let test_withdrawal_fails_when_not_staked_on_lfc () =
  let* ctxt = new_context in
  let level = (Raw_context.current_level ctxt).level in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker in
  let commitment =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 125l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (_node, ctxt) =
    lift @@ Sc_rollup_storage.refine_stake ctxt rollup level staker commitment
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.withdraw_stake ctxt rollup staker)
    "Attempted to withdraw while not staked on a final node."

let test_stake_on_existing_node () =
  let* ctxt = new_context in
  let level = (Raw_context.current_level ctxt).level in
  lift
  @@ let* (rollup, ctxt) = new_sc_rollup ctxt in
     let staker1 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let staker2 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo"
     in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker1 in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker2 in
     let commitment =
       Sc_rollup_repr.Commitment.
         {
           predecessor = Sc_rollup_repr.Commitment_hash.zero;
           inbox_level = Raw_level_repr.of_int32_exn 125l;
           number_of_messages = number_of_messages_exn 3l;
           number_of_ticks = number_of_ticks_exn 1232909l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (_node, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup level staker1 commitment
     in
     let* (_node, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup level staker2 commitment
     in
     assert_true ctxt

let test_finalize_with_two_stakers () =
  let* ctxt = new_context in
  let level_0 = (Raw_context.current_level ctxt).level in
  let level_after = Raw_level_repr.add level_0 20_160 in
  lift
  @@ let* (rollup, ctxt) = new_sc_rollup ctxt in
     let staker1 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let staker2 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo"
     in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker1 in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker2 in
     let commitment1 =
       Sc_rollup_repr.Commitment.
         {
           predecessor = Sc_rollup_repr.Commitment_hash.zero;
           inbox_level = Raw_level_repr.of_int32_exn 125l;
           number_of_messages = number_of_messages_exn 3l;
           number_of_ticks = number_of_ticks_exn 1232909l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (c1, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup level_0 staker1 commitment1
     in
     let commitment2 =
       Sc_rollup_repr.Commitment.
         {
           predecessor = c1;
           inbox_level = Raw_level_repr.of_int32_exn 1l;
           number_of_messages = number_of_messages_exn 3l;
           number_of_ticks = number_of_ticks_exn 1232909l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (_node, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup level_0 staker2 commitment2
     in
     let* ctxt =
       Sc_rollup_storage.finalize_commitment ctxt rollup level_after c1
     in
     assert_true ctxt

let test_can_remove_staker () =
  let* ctxt = new_context in
  let level_0 = (Raw_context.current_level ctxt).level in
  let level_after = Raw_level_repr.add level_0 20_160 in
  lift
  @@ let* (rollup, ctxt) = new_sc_rollup ctxt in
     let staker1 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let staker2 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo"
     in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker1 in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker2 in
     let commitment1 =
       Sc_rollup_repr.Commitment.
         {
           predecessor = Sc_rollup_repr.Commitment_hash.zero;
           inbox_level = Raw_level_repr.of_int32_exn 125l;
           number_of_messages = number_of_messages_exn 3l;
           number_of_ticks = number_of_ticks_exn 1232909l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (c1, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup level_0 staker1 commitment1
     in
     let commitment2 =
       Sc_rollup_repr.Commitment.
         {
           predecessor = c1;
           inbox_level = Raw_level_repr.of_int32_exn 1l;
           number_of_messages = number_of_messages_exn 3l;
           number_of_ticks = number_of_ticks_exn 1232909l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (_node, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup level_0 staker2 commitment2
     in
     let* ctxt = Sc_rollup_storage.remove_staker ctxt rollup staker1 in
     let* ctxt =
       Sc_rollup_storage.finalize_commitment ctxt rollup level_after c1
     in
     assert_true ctxt

let test_can_remove_staker2 () =
  let* ctxt = new_context in
  let level_0 = (Raw_context.current_level ctxt).level in
  let level_after = Raw_level_repr.add level_0 20_160 in
  lift
  @@ let* (rollup, ctxt) = new_sc_rollup ctxt in
     let staker1 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let staker2 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo"
     in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker1 in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker2 in
     let commitment1 =
       Sc_rollup_repr.Commitment.
         {
           predecessor = Sc_rollup_repr.Commitment_hash.zero;
           inbox_level = Raw_level_repr.of_int32_exn 125l;
           number_of_messages = number_of_messages_exn 3l;
           number_of_ticks = number_of_ticks_exn 1232909l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (c1, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup level_0 staker1 commitment1
     in
     let commitment2 =
       Sc_rollup_repr.Commitment.
         {
           predecessor = c1;
           inbox_level = Raw_level_repr.of_int32_exn 1l;
           number_of_messages = number_of_messages_exn 3l;
           number_of_ticks = number_of_ticks_exn 1232909l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (_node, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup level_0 staker2 commitment2
     in
     let* ctxt = Sc_rollup_storage.remove_staker ctxt rollup staker2 in
     let* ctxt =
       Sc_rollup_storage.finalize_commitment ctxt rollup level_after c1
     in
     assert_true ctxt

let test_removed_staker_can_not_withdraw () =
  let* ctxt = new_context in
  let level_0 = (Raw_context.current_level ctxt).level in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker1 =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let staker2 =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker1 in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker2 in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 125l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift
    @@ Sc_rollup_storage.refine_stake ctxt rollup level_0 staker1 commitment1
  in
  let commitment2 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = c1;
        inbox_level = Raw_level_repr.of_int32_exn 1l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (_node, ctxt) =
    lift
    @@ Sc_rollup_storage.refine_stake ctxt rollup level_0 staker2 commitment2
  in
  let* ctxt = lift @@ Sc_rollup_storage.remove_staker ctxt rollup staker2 in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.withdraw_stake ctxt rollup staker2)
    "Unknown staker."

let test_no_finalization_on_conflict () =
  let* ctxt = new_context in
  let level_0 = (Raw_context.current_level ctxt).level in
  let level_after = Raw_level_repr.add level_0 2000 in
  let* (rollup, ctxt) = lift @@ new_sc_rollup ctxt in
  let staker1 =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
  in
  let staker2 =
    Sc_rollup_repr.Staker.of_b58check_exn "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo"
  in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker1 in
  let* ctxt = lift @@ Sc_rollup_storage.deposit_stake ctxt rollup staker2 in
  let commitment1 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 125l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (c1, ctxt) =
    lift
    @@ Sc_rollup_storage.refine_stake ctxt rollup level_0 staker1 commitment1
  in
  let commitment2 =
    Sc_rollup_repr.Commitment.
      {
        predecessor = Sc_rollup_repr.Commitment_hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 1l;
        number_of_messages = number_of_messages_exn 3l;
        number_of_ticks = number_of_ticks_exn 1232909l;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let* (_node, ctxt) =
    lift
    @@ Sc_rollup_storage.refine_stake ctxt rollup level_0 staker2 commitment2
  in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.finalize_commitment ctxt rollup level_after c1)
    "Attempted to finalize a disputed commitment."

let test_finds_conflict_point_at_lfc () =
  let* ctxt = new_context in
  let level_0 = (Raw_context.current_level ctxt).level in
  lift
  @@ let* (rollup, ctxt) = new_sc_rollup ctxt in
     let staker1 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let staker2 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo"
     in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker1 in
     let* ctxt = Sc_rollup_storage.deposit_stake ctxt rollup staker2 in
     let commitment1 =
       Sc_rollup_repr.Commitment.
         {
           predecessor = Sc_rollup_repr.Commitment_hash.zero;
           inbox_level = Raw_level_repr.of_int32_exn 125l;
           number_of_messages = number_of_messages_exn 3l;
           number_of_ticks = number_of_ticks_exn 1232909l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (c1, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup level_0 staker1 commitment1
     in
     let commitment2 =
       Sc_rollup_repr.Commitment.
         {
           predecessor = Sc_rollup_repr.Commitment_hash.zero;
           inbox_level = Raw_level_repr.of_int32_exn 1l;
           number_of_messages = number_of_messages_exn 3l;
           number_of_ticks = number_of_ticks_exn 1232909l;
           compressed_state = Sc_rollup_repr.State_hash.zero;
         }
     in
     let* (c2, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup level_0 staker2 commitment2
     in
     let* ((left, right), ctxt) =
       Sc_rollup_storage.get_conflict_point ctxt rollup staker1 staker2
     in
     if left = c1 && right = c2 then assert_true ctxt else assert false

let tests =
  [
    Tztest.tztest
      "deposit to missing rollup fails"
      `Quick
      test_deposit_to_missing_rollup;
    Tztest.tztest
      "deposit to existing rollup"
      `Quick
      test_deposit_to_existing_rollup;
    Tztest.tztest "deposit, then withdraw" `Quick test_deposit_then_withdraw;
    Tztest.tztest
      "withdrawing when not staked fails"
      `Quick
      test_withdraw_when_not_staked;
    Tztest.tztest "withdrawing twice fails" `Quick test_withdrawing_twice;
    Tztest.tztest "stake on new node" `Quick test_deposit_then_refine;
    Tztest.tztest "stake on existing node" `Quick test_stake_on_existing_node;
    Tztest.tztest
      "withdrawal fails when not staked on LFC"
      `Quick
      test_withdrawal_fails_when_not_staked_on_lfc;
    Tztest.tztest
      "rollup starts in pre-boot state"
      `Quick
      test_initial_state_is_pre_boot;
    Tztest.tztest "finalize" `Quick test_finalize;
    Tztest.tztest
      "finalize fails when too recent"
      `Quick
      test_finalize_fail_too_recent;
    Tztest.tztest
      "finalize with two stakers"
      `Quick
      test_finalize_with_two_stakers;
    Tztest.tztest
      "no finalization on conflict"
      `Quick
      test_no_finalization_on_conflict;
    Tztest.tztest
      "finds conflict point at LFC"
      `Quick
      test_finds_conflict_point_at_lfc;
    Tztest.tztest "can remove staker 1" `Quick test_can_remove_staker;
    Tztest.tztest "can remove staker 2" `Quick test_can_remove_staker2;
    Tztest.tztest
      "removed staker can not withdraw"
      `Quick
      test_removed_staker_can_not_withdraw;
    Tztest.tztest
      "removing staker from the LFC fails"
      `Quick
      test_removing_staker_from_lfc_fails;
  ]

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/2460
   Further tests to be added.
   *)
