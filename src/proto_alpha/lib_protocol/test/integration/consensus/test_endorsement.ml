(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
    Component:  Protocol (endorsement)
    Invocation: dune exec \
                src/proto_alpha/lib_protocol/test/integration/consensus/main.exe \
                -- test "^endorsement$"
    Subject:    Endorsing a block adds an extra layer of confidence
                to the Tezos' PoS algorithm. The block endorsing
                operation must be included in the following block.
*)

open Protocol
open Alpha_context

let init_genesis ?policy () =
  Context.init_n ~consensus_threshold:0 5 () >>=? fun (genesis, _contracts) ->
  Block.bake ?policy genesis >>=? fun b -> return (genesis, b)

(** {1 Positive tests} *)

(** Correct endorsement from the slot 0 endorser. *)
let test_simple_endorsement () =
  let open Lwt_result_syntax in
  let* _genesis, endorsed_block = init_genesis () in
  Consensus_helpers.test_consensus_operation_all_modes
    ~loc:__LOC__
    ~endorsed_block
    Endorsement

(** Test that the endorsement's branch does not affect its
    validity. *)
let test_arbitrary_branch () =
  let open Lwt_result_syntax in
  let* _genesis, endorsed_block = init_genesis () in
  Consensus_helpers.test_consensus_operation_all_modes
    ~loc:__LOC__
    ~endorsed_block
    ~branch:Block_hash.zero
    Endorsement

(** Correct endorsement with a level and a round that are both
    different from {!test_simple_endorsement}. *)
let test_non_zero_round () =
  let open Lwt_result_syntax in
  let* _genesis, b = init_genesis () in
  let* endorsed_block = Block.bake ~policy:(By_round 10) b in
  Consensus_helpers.test_consensus_operation_all_modes
    ~loc:__LOC__
    ~endorsed_block
    Endorsement

(** Fitness gap: this is a straightforward update from Emmy to Tenderbake,
    that is, check that the level is incremented in a child block. *)
let test_fitness_gap () =
  let open Lwt_result_syntax in
  let* _genesis, pred_b = init_genesis () in
  let* operation = Op.endorsement pred_b in
  let* b = Block.bake ~operation pred_b in
  let fitness =
    match Fitness.from_raw b.header.shell.fitness with
    | Ok fitness -> fitness
    | _ -> assert false
  in
  let pred_fitness =
    match Fitness.from_raw pred_b.header.shell.fitness with
    | Ok fitness -> fitness
    | _ -> assert false
  in
  let level = Fitness.level fitness in
  let pred_level = Fitness.level pred_fitness in
  let level_diff =
    Int32.sub (Raw_level.to_int32 level) (Raw_level.to_int32 pred_level)
  in
  Assert.equal_int32 ~loc:__LOC__ level_diff 1l

(** {1 Negative tests}

    The following test scenarios are supposed to raise errors. *)

(** {2 Wrong slot} *)

(** Apply an endorsement with a negative slot. *)
let test_negative_slot () =
  Context.init_n 5 () >>=? fun (genesis, _contracts) ->
  Block.bake genesis >>=? fun b ->
  Context.get_endorser (B b) >>=? fun (delegate, _slots) ->
  Lwt.catch
    (fun () ->
      Op.endorsement
        ~delegate
        ~slot:(Slot.of_int_do_not_use_except_for_parameters (-1))
        b
      >>=? fun (_ : packed_operation) ->
      failwith "negative slot should not be accepted by the binary format")
    (function
      | Data_encoding.Binary.Write_error _ -> return_unit | e -> Lwt.fail e)

(** Endorsement with a non-normalized slot (that is, a slot that
    belongs to the delegate but is not the delegate's smallest slot). *)
let test_not_smallest_slot () =
  let open Lwt_result_syntax in
  let* _genesis, b = init_genesis () in
  let* endorsers = Context.get_endorsers (B b) in
  let delegate, slots =
    (* Find an endorser with more than 1 slot. *)
    WithExceptions.Option.get
      ~loc:__LOC__
      (List.find_map
         (fun {RPC.Validators.delegate; slots; _} ->
           if Compare.List_length_with.(slots > 1) then Some (delegate, slots)
           else None)
         endorsers)
  in
  (* Check that the slots are sorted and have no duplicates. *)
  let rec check_sorted = function
    | [] | [_] -> true
    | x :: (y :: _ as t) -> Slot.compare x y < 0 && check_sorted t
  in
  assert (check_sorted slots) ;
  let slot =
    match slots with [] | [_] -> assert false | _ :: slot :: _ -> slot
  in
  Consensus_helpers.test_consensus_operation_all_modes
    ~loc:__LOC__
    ~endorsed_block:b
    ~delegate
    ~slot
    ~error:(function
      | Validate_errors.Consensus.Wrong_slot_used_for_consensus_operation
          {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    Endorsement

(** Endorsement with a slot that does not belong to the delegate. *)
let test_other_delegate_slot () =
  let open Lwt_result_syntax in
  let* _genesis, b = init_genesis () in
  let* endorsers = Context.get_endorsers (B b) in
  let delegate, other_delegate_slot =
    match endorsers with
    | [] | [_] -> assert false (* at least two delegates with rights *)
    | {delegate; _} :: {slots; _} :: _ ->
        (delegate, WithExceptions.Option.get ~loc:__LOC__ (List.hd slots))
  in
  Consensus_helpers.test_consensus_operation_all_modes
    ~loc:__LOC__
    ~endorsed_block:b
    ~delegate
    ~slot:other_delegate_slot
    ~error:(function
      | Alpha_context.Operation.Invalid_signature -> true | _ -> false)
    Endorsement

(** Invalid_endorsement_level: apply an endorsement with an incorrect
    level (i.e. the predecessor level). *)
let test_invalid_endorsement_level () =
  init_genesis () >>=? fun (genesis, b) ->
  Context.get_level (B genesis) >>?= fun genesis_level ->
  Op.endorsement ~level:genesis_level b >>=? fun operation ->
  Block.bake ~operation b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Consensus.Consensus_operation_for_old_level {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)

(** Duplicate endorsement : apply an endorsement that has already been applied. *)
let test_duplicate_endorsement () =
  init_genesis () >>=? fun (_genesis, b) ->
  Incremental.begin_construction b >>=? fun inc ->
  Op.endorsement b >>=? fun operation ->
  Incremental.add_operation inc operation >>=? fun inc ->
  Op.endorsement b >>=? fun operation ->
  Incremental.add_operation inc operation >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Consensus.Conflicting_consensus_operation {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)

(** Consensus operation for future level : apply an endorsement with a level in the future *)
let test_consensus_operation_endorsement_for_future_level () =
  init_genesis () >>=? fun (_genesis, pred) ->
  let raw_level = Raw_level.of_int32 (Int32.of_int 10) in
  let level = match raw_level with Ok l -> l | Error _ -> assert false in
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~endorsed_block:pred
    ~level
    ~error:(function
      | Validate_errors.Consensus.Consensus_operation_for_future_level {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    Endorsement
    Mempool

(** Consensus operation for old level : apply an endorsement one level in the past *)
let test_consensus_operation_endorsement_for_predecessor_level () =
  init_genesis () >>=? fun (_genesis, pred) ->
  let raw_level = Raw_level.of_int32 (Int32.of_int 0) in
  let level = match raw_level with Ok l -> l | Error _ -> assert false in
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~endorsed_block:pred
    ~level
    ~error:(function
      | Validate_errors.Consensus.Consensus_operation_for_old_level {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    Endorsement
    Mempool

(** Consensus operation for old level : apply an endorsement with more than one level in the past *)
let test_consensus_operation_endorsement_for_old_level () =
  init_genesis () >>=? fun (genesis, pred) ->
  Block.bake genesis >>=? fun _next_block ->
  let raw_level = Raw_level.of_int32 (Int32.of_int 0) in
  let level = match raw_level with Ok l -> l | Error _ -> assert false in
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~endorsed_block:pred
    ~level
    ~error:(function
      | Validate_errors.Consensus.Consensus_operation_for_old_level {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    Endorsement
    Mempool

(** Consensus operation for future round : apply an endorsement with a round in the future *)
let test_consensus_operation_endorsement_for_future_round () =
  init_genesis () >>=? fun (_genesis, pred) ->
  Environment.wrap_tzresult (Round.of_int 21) >>?= fun round ->
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~endorsed_block:pred
    ~round
    ~error:(function
      | Validate_errors.Consensus.Consensus_operation_for_future_round {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    Endorsement
    Mempool

(** Consensus operation for old round : apply an endorsement with a round in the past *)
let test_consensus_operation_endorsement_for_old_round () =
  init_genesis ~policy:(By_round 10) () >>=? fun (_genesis, pred) ->
  Environment.wrap_tzresult (Round.of_int 0) >>?= fun round ->
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~endorsed_block:pred
    ~round
    ~error:(function
      | Validate_errors.Consensus.Consensus_operation_for_old_round {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    Endorsement
    Mempool

(** Consensus operation on competing proposal : apply an endorsement on a competing proposal *)
let test_consensus_operation_endorsement_on_competing_proposal () =
  init_genesis () >>=? fun (_genesis, pred) ->
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~endorsed_block:pred
    ~block_payload_hash:Block_payload_hash.zero
    ~error:(function
      | Validate_errors.Consensus.Wrong_payload_hash_for_consensus_operation
          {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    Endorsement
    Mempool

(** Wrong round : apply an endorsement with an incorrect round *)
let test_wrong_round () =
  init_genesis () >>=? fun (_genesis, b) ->
  Environment.wrap_tzresult (Round.of_int 2) >>?= fun round ->
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~endorsed_block:b
    ~round
    ~error:(function
      | Validate_errors.Consensus.Consensus_operation_for_future_round {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    Endorsement
    Application

(** Wrong level : apply an endorsement with an incorrect level *)
let test_wrong_level () =
  init_genesis () >>=? fun (_genesis, b) ->
  (* let context = Context.B genesis in*)
  let raw_level = Raw_level.of_int32 (Int32.of_int 0) in
  let level = match raw_level with Ok l -> l | Error _ -> assert false in
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~endorsed_block:b
    ~level
    ~error:(function
      | Validate_errors.Consensus.Consensus_operation_for_old_level {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    Endorsement
    Application

(** Wrong payload hash : apply an endorsement with an incorrect payload hash *)
let test_wrong_payload_hash () =
  init_genesis () >>=? fun (_genesis, b) ->
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~endorsed_block:b
    ~block_payload_hash:Block_payload_hash.zero
    ~error:(function
      | Validate_errors.Consensus.Wrong_payload_hash_for_consensus_operation
          {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)
    Endorsement
    Application

(** Check that:
    - a block with not enough endorsement cannot be baked;
    - a block with enough endorsement is baked. *)
let test_endorsement_threshold ~sufficient_threshold () =
  (* We choose a relative large number of accounts so that the probability that
     any delegate has [consensus_threshold] slots is low and most delegates have
     about 1 slot so we can get closer to the limit of [consensus_threshold]: we
     check that a block with endorsing power [consensus_threshold - 1] won't be
     baked. *)
  Context.init_n 10 () >>=? fun (genesis, _contracts) ->
  Block.bake genesis >>=? fun b ->
  Context.get_constants (B b)
  >>=? fun {parametric = {consensus_threshold; _}; _} ->
  Context.get_endorsers (B b) >>=? fun endorsers_list ->
  Block.get_round b >>?= fun round ->
  List.fold_left_es
    (fun (counter, endos) {Plugin.RPC.Validators.delegate; slots; _} ->
      let new_counter = counter + List.length slots in
      if
        (sufficient_threshold && counter < consensus_threshold)
        || ((not sufficient_threshold) && new_counter < consensus_threshold)
      then
        Op.endorsement ~round ~delegate b >>=? fun endo ->
        return (new_counter, endo :: endos)
      else return (counter, endos))
    (0, [])
    endorsers_list
  >>=? fun (_, endos) ->
  Block.bake ~operations:endos b >>= fun b ->
  if sufficient_threshold then return_unit
  else Assert.proto_error_with_info ~loc:__LOC__ b "Not enough endorsements"

let test_preendorsement_endorsement_same_level () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (genesis, _contract) ->
  Block.bake genesis >>=? fun b1 ->
  Incremental.begin_construction ~mempool_mode:true ~policy:(By_round 2) b1
  >>=? fun i ->
  Op.endorsement b1 >>=? fun op_endo ->
  Incremental.add_operation i op_endo >>=? fun (_i : Incremental.t) ->
  Op.preendorsement b1 >>=? fun op_preendo ->
  Incremental.add_operation i op_preendo >>=? fun (_i : Incremental.t) ->
  return_unit

(** Endorsement for next level *)
let test_endorsement_for_next_level () =
  init_genesis () >>=? fun (genesis, _) ->
  Consensus_helpers.test_consensus_op_for_next
    ~genesis
    ~kind:`Endorsement
    ~next:`Level

(** Endorsement for next round *)
let test_endorsement_for_next_round () =
  init_genesis () >>=? fun (genesis, _) ->
  Consensus_helpers.test_consensus_op_for_next
    ~genesis
    ~kind:`Endorsement
    ~next:`Round

(** Endorsement of grandparent  *)
let test_endorsement_grandparent () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (genesis, _contract) ->
  Block.bake genesis >>=? fun b_gp ->
  Block.bake b_gp >>=? fun b ->
  Incremental.begin_construction ~mempool_mode:true b >>=? fun i ->
  (* Endorsement on grandparent *)
  Op.endorsement b_gp >>=? fun op1 ->
  (* Endorsement on parent *)
  Op.endorsement b >>=? fun op2 ->
  (* Both should be accepted by the mempool *)
  Incremental.add_operation i op1 >>=? fun i ->
  Incremental.add_operation i op2 >>=? fun (_i : Incremental.t) -> return_unit

(** Double inclusion of grandparent endorsement *)
let test_double_endorsement_grandparent () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (genesis, _contract) ->
  Block.bake genesis >>=? fun b_gp ->
  Block.bake b_gp >>=? fun b ->
  Incremental.begin_construction ~mempool_mode:true b >>=? fun i ->
  (* Endorsement on grandparent *)
  Op.endorsement b_gp >>=? fun op1 ->
  (* Endorsement on parent *)
  Op.endorsement b >>=? fun op2 ->
  (* The first grand parent endorsement should be accepted by the
     mempool but the second rejected. *)
  Incremental.add_operation i op1 >>=? fun i ->
  Incremental.add_operation i op1 >>= fun res ->
  Assert.proto_error_with_info
    ~loc:__LOC__
    res
    "Double inclusion of consensus operation"
  >>=? fun () ->
  Incremental.add_operation i op2 >>=? fun (_i : Incremental.t) -> return_unit

(** Endorsement of grandparent on same slot as parent *)
let test_endorsement_grandparent_same_slot () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (genesis, _contract) ->
  Block.bake genesis >>=? fun b_gp ->
  Block.bake b_gp >>=? fun b ->
  Incremental.begin_construction ~mempool_mode:true b >>=? fun i ->
  (* Endorsement on parent *)
  Consensus_helpers.delegate_of_first_slot (B b) >>=? fun (delegate, slot) ->
  Op.endorsement ~delegate b >>=? fun op2 ->
  (* Endorsement on grandparent *)
  Consensus_helpers.delegate_of_slot slot (B b_gp) >>=? fun delegate ->
  Op.endorsement ~delegate b_gp >>=? fun op1 ->
  (* Both should be accepted by the mempool *)
  Incremental.add_operation i op1 >>=? fun i ->
  Incremental.add_operation i op2 >>=? fun (_i : Incremental.t) -> return_unit

(** Endorsement of grandparent in application mode should be rejected *)
let test_endorsement_grandparent_application () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (genesis, _contract) ->
  Block.bake genesis >>=? fun b_gp ->
  Block.bake b_gp >>=? fun b ->
  Op.endorsement b_gp >>=? fun operation ->
  Block.bake ~operation b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Consensus.Consensus_operation_for_old_level {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)

(** Endorsement of grandparent in full construction mode should be rejected *)
let test_endorsement_grandparent_full_construction () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (genesis, _contract) ->
  Block.bake genesis >>=? fun b_gp ->
  Block.bake b_gp >>=? fun b ->
  Incremental.begin_construction b >>=? fun i ->
  (* Endorsement on grandparent *)
  Op.endorsement b_gp >>=? fun op1 ->
  Incremental.add_operation i op1 >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Consensus.Consensus_operation_for_old_level {kind; _}
        when kind = Validate_errors.Consensus.Endorsement ->
          true
      | _ -> false)

let tests =
  [
    (* Positive tests *)
    Tztest.tztest "Simple endorsement" `Quick test_simple_endorsement;
    Tztest.tztest "Arbitrary branch" `Quick test_arbitrary_branch;
    Tztest.tztest "Non-zero round" `Quick test_non_zero_round;
    Tztest.tztest "Fitness gap" `Quick test_fitness_gap;
    (* Negative tests *)
    (* Wrong slot *)
    Tztest.tztest "Endorsement with slot -1" `Quick test_negative_slot;
    Tztest.tztest "Non-normalized slot" `Quick test_not_smallest_slot;
    Tztest.tztest "Slot of another delegate" `Quick test_other_delegate_slot;
    Tztest.tztest
      "Invalid endorsement level"
      `Quick
      test_invalid_endorsement_level;
    Tztest.tztest "Duplicate endorsement" `Quick test_duplicate_endorsement;
    Tztest.tztest
      "Endorsement for future level"
      `Quick
      test_consensus_operation_endorsement_for_future_level;
    Tztest.tztest
      "Endorsement for predecessor level"
      `Quick
      test_consensus_operation_endorsement_for_old_level;
    Tztest.tztest
      "Endorsement for old level"
      `Quick
      test_consensus_operation_endorsement_for_old_level;
    Tztest.tztest
      "Endorsement for future round"
      `Quick
      test_consensus_operation_endorsement_for_future_round;
    Tztest.tztest
      "Endorsement for old round"
      `Quick
      test_consensus_operation_endorsement_for_old_round;
    Tztest.tztest
      "Endorsement on competing proposal"
      `Quick
      test_consensus_operation_endorsement_on_competing_proposal;
    Tztest.tztest "Wrong level for consensus operation" `Quick test_wrong_level;
    Tztest.tztest "Wrong round for consensus operation" `Quick test_wrong_round;
    Tztest.tztest
      "Wrong payload hash for consensus operation"
      `Quick
      test_wrong_payload_hash;
    Tztest.tztest
      "sufficient endorsement threshold"
      `Quick
      (test_endorsement_threshold ~sufficient_threshold:true);
    Tztest.tztest
      "insufficient endorsement threshold"
      `Quick
      (test_endorsement_threshold ~sufficient_threshold:false);
    Tztest.tztest
      "Endorsement/Preendorsement at same level"
      `Quick
      test_preendorsement_endorsement_same_level;
    Tztest.tztest
      "Endorsement for next level"
      `Quick
      test_endorsement_for_next_level;
    Tztest.tztest
      "Endorsement for next round"
      `Quick
      test_endorsement_for_next_round;
    Tztest.tztest
      "Endorsement for grandparent"
      `Quick
      test_endorsement_grandparent;
    Tztest.tztest
      "Double endorsement of grandparent"
      `Quick
      test_double_endorsement_grandparent;
    Tztest.tztest
      "Endorsement for grandparent on same slot as parent"
      `Quick
      test_endorsement_grandparent_same_slot;
    Tztest.tztest
      "Endorsement for grandparent in application mode"
      `Quick
      test_endorsement_grandparent_application;
    Tztest.tztest
      "Endorsement for grandparent in full construction mode"
      `Quick
      test_endorsement_grandparent_full_construction;
  ]
