(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
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

let assert_fails_with :
    type a b.
    loc:string ->
    b Environment.Error_monad.tzresult Lwt.t ->
    string ->
    (unit, a) result Lwt.t =
 fun ~loc k msg ->
  let _ = loc in
  k >>= function
  | Ok _ -> Stdlib.failwith "Expected failure"
  | Error err ->
      (* TODO use derived equality on errors, or at least comparison of registered id, rather than description equality *)
      let str : string =
        Format.asprintf "%a" Environment.Error_monad.pp_trace err
      in
      if Stdlib.(str = "Error:\n  " ^ msg ^ "\n") then Lwt.return (Ok ())
        (* TODO say what was expected *)
      else Stdlib.failwith @@ "Failed with wrong error message, got: " ^ str

(* TODO use this *)
let assert_fails_with2 ~loc (k : 'b Lwt.t)
    (error : Environment.Error_monad.error) : (unit, 'a) result Lwt.t =
  k >>= function
  | Ok _ -> Stdlib.failwith "Expected failure"
  | Error err ->
      let actual_str : string =
        Format.asprintf "%a" Environment.Error_monad.pp_trace err
      in
      let expected_str : string =
        Format.asprintf
          "%a"
          Environment.Error_monad.pp_trace
          (Environment.Error_monad.trace_of_error error)
      in
      Assert.equal_string ~loc actual_str expected_str

(* TODO use default_raw_context instead of new_context
   See
 https://gitlab.com/tezos/tezos/-/merge_requests/4276/diffs#bfd07d868be6a3674a52987cc801d6ae576f542e
  *)
let new_context ~limit =
  let* (b, _contracts) = Context.init 1 in
  Incremental.begin_construction b >|=? fun inc ->
  let state = Incremental.validation_state inc in
  let _ = limit in
  let ctxt = state.ctxt in
  (*
  let ctxt =
    Alpha_context.Gas.set_limit ctxt (Saturation_repr.safe_int limit)
  in
  *)
  let ctxt = Alpha_context.Origination_nonce.init ctxt Operation_hash.zero in
  Alpha_context.Internal_for_tests.to_raw ctxt

let consume _ctxt = return ()

let test_deposit_to_missing_rollup () =
  let* ctxt = new_context ~limit:1_000_000 in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@
  let rollup = Sc_rollup_repr.Address.hash_bytes [] in
  let staker = Sc_rollup_repr.Staker.zero in
  assert_fails_with
    ~loc:__LOC__
    (Sc_rollup_storage.deposit_stake ctxt rollup staker)
    "Rollup scr1Ew52VCdi6nF1JuokRGMqfmSeiAEXymW2m does not exist"

let test_initial_state_is_pre_boot () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let* (lfc, ctxt) = Sc_rollup_storage.last_final_commitment ctxt rollup in
     if lfc = Sc_rollup_repr.Commitment_hash.zero then consume ctxt
     else assert false

let test_deposit_to_existing_rollup () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let staker =
       Signature.Public_key_hash.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* (_, ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker in
     consume ctxt

let test_removing_staker_from_lfc_fails () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let staker =
       Signature.Public_key_hash.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* (_, ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker in
     assert_fails_with
       ~loc:__LOC__
       (Sc_rollup_storage.remove_staker ctxt rollup staker)
       "Can not remove a final commitment."

let test_deposit_then_withdraw () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let staker =
       Signature.Public_key_hash.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker in
     let* ((), ctxt) = Sc_rollup_storage.withdraw_stake ctxt rollup staker in
     consume ctxt

let test_withdraw_when_not_staked () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let staker =
       Signature.Public_key_hash.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     assert_fails_with
       ~loc:__LOC__
       (Sc_rollup_storage.withdraw_stake ctxt rollup staker)
       "Unknown staker."

let test_withdrawing_twice () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let staker =
       Signature.Public_key_hash.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker in
     let* ((), ctxt) = Sc_rollup_storage.withdraw_stake ctxt rollup staker in
     assert_fails_with
       ~loc:__LOC__
       (Sc_rollup_storage.withdraw_stake ctxt rollup staker)
       "Unknown staker."

(* TODO move to lib_base, not exposed in Proto *)
let number_of_messages_exn n =
  match Sc_rollup_repr.Number_of_messages.of_int32 n with
  | Some x -> x
  | None -> Stdlib.failwith "Bad Number_of_messages"

let number_of_ticks_exn n =
  match Sc_rollup_repr.Number_of_ticks.of_int32 n with
  | Some x -> x
  | None -> Stdlib.failwith "Bad Number_of_ticks"

let test_deposit_then_refine () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  let level = (Raw_context.current_level ctxt).level in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let staker =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker in
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
     consume ctxt

let test_finalize () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  let level_0 = (Raw_context.current_level ctxt).level in
  let level_2000 = Raw_level_repr.add level_0 2000 in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let staker =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker in
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
     let* ((), ctxt) =
       Sc_rollup_storage.finalize_commitment ctxt rollup level_2000 c1
     in
     consume ctxt

let test_finalize_fail_to_recent () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  let level = (Raw_context.current_level ctxt).level in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let staker =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker in
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
       Sc_rollup_storage.refine_stake ctxt rollup level staker commitment
     in
     assert_fails_with
       ~loc:__LOC__
       (Sc_rollup_storage.finalize_commitment ctxt rollup level c1)
       "Attempted to finalize a commitment before its refutation deadline."

let test_withdrawal_fails_when_not_staked_on_lfc () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  let level = (Raw_context.current_level ctxt).level in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let staker =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker in
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
     assert_fails_with
       ~loc:__LOC__
       (Sc_rollup_storage.withdraw_stake ctxt rollup staker)
       "Attempted to withdraw while not staked on a final node."

let test_stake_on_existing_node () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  let level = (Raw_context.current_level ctxt).level in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let staker1 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let staker2 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo"
     in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker1 in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker2 in
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
     consume ctxt

let test_finalize_with_two_stakers () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  let level_0 = (Raw_context.current_level ctxt).level in
  let level_2000 = Raw_level_repr.add level_0 2000 in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let staker1 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let staker2 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo"
     in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker1 in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker2 in
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
     let* ((), ctxt) =
       Sc_rollup_storage.finalize_commitment ctxt rollup level_2000 c1
     in
     consume ctxt

let test_can_remove_staker () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  let level_0 = (Raw_context.current_level ctxt).level in
  let level_2000 = Raw_level_repr.add level_0 2000 in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let staker1 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let staker2 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo"
     in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker1 in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker2 in
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
     let* ((), ctxt) = Sc_rollup_storage.remove_staker ctxt rollup staker1 in
     let* ((), ctxt) =
       Sc_rollup_storage.finalize_commitment ctxt rollup level_2000 c1
     in
     consume ctxt

let test_can_remove_staker2 () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  let level_0 = (Raw_context.current_level ctxt).level in
  let level_2000 = Raw_level_repr.add level_0 2000 in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let staker1 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let staker2 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo"
     in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker1 in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker2 in
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
     let* ((), ctxt) = Sc_rollup_storage.remove_staker ctxt rollup staker2 in
     let* ((), ctxt) =
       Sc_rollup_storage.finalize_commitment ctxt rollup level_2000 c1
     in
     consume ctxt

let test_removed_staker_can_not_withdraw () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  let level_0 = (Raw_context.current_level ctxt).level in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let staker1 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let staker2 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo"
     in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker1 in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker2 in
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
     let* ((), ctxt) = Sc_rollup_storage.remove_staker ctxt rollup staker2 in
     assert_fails_with
       ~loc:__LOC__
       (Sc_rollup_storage.withdraw_stake ctxt rollup staker2)
       "Unknown staker."

let test_no_finalization_on_conflict () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  let level_0 = (Raw_context.current_level ctxt).level in
  let level_2000 = Raw_level_repr.add level_0 2000 in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let staker1 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let staker2 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo"
     in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker1 in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker2 in
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
     let* (_node, ctxt) =
       Sc_rollup_storage.refine_stake ctxt rollup level_0 staker2 commitment2
     in
     assert_fails_with
       ~loc:__LOC__
       (Sc_rollup_storage.finalize_commitment ctxt rollup level_2000 c1)
       "Attempted to finalize a disputed commitment."

let test_finds_conflict_point_at_lfc () =
  let* ctxt = new_context ~limit:1_000_000_000 in
  let level_0 = (Raw_context.current_level ctxt).level in
  Lwt.map (fun x -> Environment.wrap_tzresult x)
  @@ let* (rollup, _size, ctxt) =
       Sc_rollup_storage.originate
         ctxt
         ~kind:Example_arith
         ~boot_sector:(Sc_rollup_repr.PVM.boot_sector_of_string "")
     in
     let staker1 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1SdKt9kjPp1HRQFkBmXtBhgMfvdgFhSjmG"
     in
     let staker2 =
       Sc_rollup_repr.Staker.of_b58check_exn
         "tz1RikjCkrEde1QQmuesp796jCxeiyE6t3Vo"
     in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker1 in
     let* ((), ctxt) = Sc_rollup_storage.deposit_stake ctxt rollup staker2 in
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
     if left = c1 && right = c2 then consume ctxt else assert false

(* TODO test we can extract conflict point *beneath* LFC *)
(* TODO test get_conflict_point with either or both stakers being on LFC *)
(* TODO test get_conflict_point with either or both stakers being on LFC when SCORU is in pre-boot state *)
(* TODO test finalization consumes inbox *)
(* TODO mutation test *)

let tests =
  [
    Tztest.tztest
      "deposit to missing rollup fails"
      `Quick
      test_deposit_to_missing_rollup;
    (* TODO check ALL storage methods fail on non-existing rollup *)
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
    Tztest.tztest "deposit and refine" `Quick test_deposit_then_refine;
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
      test_finalize_fail_to_recent;
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
    (* TODO test for memory leaks *)
    (* TODO add test showing that "concurrent" insertion with refine_stake is OK *)
    (* TODO test that we fail gracefully if remove_staker is called
       for a staker staked on LFC. It should not be possible to
       force unstaking from the LFC, instead other nodes should
       refund stakers who refuse to refine from the LFC.
    *)
    (* TODO other tests from spec.ts, line 530 *)
  ]
