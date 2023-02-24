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

open Protocol.Alpha_context
open Injector_sigs

module Parameters :
  PARAMETERS
    with type state = Node_context.ro
     and type Tag.t = Configuration.purpose
     and type Operation.t = L1_operation.t = struct
  type state = Node_context.ro

  let events_section = ["sc_rollup.injector"]

  module Tag : TAG with type t = Configuration.purpose = struct
    type t = Configuration.purpose

    let compare = Stdlib.compare

    let equal = Stdlib.( = )

    let hash = Hashtbl.hash

    let string_of_tag = Configuration.string_of_purpose

    let pp ppf t = Format.pp_print_string ppf (string_of_tag t)

    let encoding : t Data_encoding.t =
      let open Data_encoding in
      string_enum
        (List.map (fun t -> (string_of_tag t, t)) Configuration.purposes)
  end

  module Operation = L1_operation

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3459
     Very coarse approximation for the number of operation we
     expect for each block *)
  let table_estimated_size : Tag.t -> int = function
    | Publish -> 1
    | Add_messages -> 100
    | Cement -> 1
    | Timeout -> 1
    | Refute -> 1

  let operation_tag : Operation.t -> Tag.t = function
    | Add_messages _ -> Add_messages
    | Cement _ -> Cement
    | Publish _ -> Publish
    | Timeout _ -> Timeout
    | Refute _ -> Refute

  let fee_parameter node_ctxt operation =
    Node_context.get_fee_parameter node_ctxt (operation_tag operation)

  (* Below are dummy values that are only used to approximate the
     size. It is thus important that they remain above the real
     values if we want the computed size to be an over_approximation
     (without having to do a simulation first).

     TODO: https://gitlab.com/tezos/tezos/-/issues/2812
     check the size, or compute them wrt operation kind *)
  let approximate_fee_bound _ _ =
    {
      fee = Tez.of_mutez_exn 3_000_000L;
      counter = Manager_counter.Internal_for_tests.of_int 500_000;
      gas_limit = Gas.Arith.integral_of_int_exn 500_000;
      storage_limit = Z.of_int 500_000;
    }

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3459
     Decide if some batches must have all the operations succeed. See
     {!Injector_sigs.Parameter.batch_must_succeed}. *)
  let batch_must_succeed _ = `At_least_one

  let retry_unsuccessful_operation _node_ctxt (op : Operation.t) status =
    let open Lwt_syntax in
    match status with
    | Backtracked | Skipped | Other_branch ->
        (* Always retry backtracked or skipped operations, or operations that
           are on another branch because of a reorg:

           - Commitments are always produced on finalized blocks. They don't
             need to be recomputed, and as such are valid in another branch.

           - The cementation operations should be re-injected because the node
             only keeps track of the last cemented level and the last published
             commitment, without rollbacks.

           - Messages posted to an inbox should be re-emitted (i.e. re-queued)
             in case of a fork.

           - Timeout should be re-submitted as the timeout may be reached as well
             on the other branch.

           - Refutation should be re-submitted in case of fork.
             TODO: https://gitlab.com/tezos/tezos/-/issues/3459
             maybe check if game exists on other branch as well.
        *)
        return Retry
    | Failed error -> (
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/4071
           Think about which operations should be retried and when. *)
        let is_gas_error =
          TzTrace.fold
            (fun found -> function
              | Environment.Ecoproto_error Gas.Operation_quota_exceeded -> true
              | _ -> found)
            false
            error
        in
        if is_gas_error then
          (* Always retry operations which have gas errors *)
          return Retry
        else
          match op with
          | Timeout _ | Refute _ | Cement _ | Add_messages _ ->
              (* Failing timeout and refutation operations can be ignored. *)
              return Forget
          | Publish _ -> return (Abort error))
end

include Injector_functor.Make (Parameters)