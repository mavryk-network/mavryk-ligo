(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component:    Accuser
   Invocation:   dune exec tezt/tests/main.exe -- double baking accuser
   Subject:      Detect double baking through the accuser.
*)

(* This test is duplicated in [remote_tests/double_baking.ml]. Any modification
   to this test should be reported there too. *)

let is_operation_in_operations ops oph =
  let open JSON in
  let ops_list = ops |=> 2 |> as_list in
  List.exists (fun e -> e |-> "hash" |> as_string = oph) ops_list

let is_operation_in_applied_mempool mempool oph =
  let open JSON in
  let applied_list = as_list (mempool |-> "applied") in
  List.exists (fun e -> e |-> "hash" |> as_string = oph) applied_list

(* Matches events where the message is of the form:
   "double baking evidence injected <operation_hash>".
   For example:

    "event": {
      "double_baking_denounced.v0": {
        "hash": "onkfjSun49iRrGtuN9FwtiCqDAEgzPKzg1BSa7BSHnaAkButUxx",
        "bytes": "..."
      }
    }
 *)
let wait_for_denunciation accuser =
  let filter json = JSON.(json |-> "hash" |> as_string_opt) in
  Accuser.wait_for accuser "double_baking_denounced.v0" filter

(* Matches events which contain an injection request.
   For example:

   "event": {
     "node_prevalidator.v0": [
       "2020-09-11T12:32:05.353-00:00",
       {
         "event": {
           "request": {
             "request": "inject",
             "operation": {
               "branch": "BM3J62AvjnjJKfinoq1op2uw5Hdn3YGMQmusnLdrfCd1yrpftG2",
               "data": "030000...00000"
             }
           },
           "status": {
             "pushed": "2020-09-11T12:32:05.343-00:00",
             "treated": 4.5947e-05,
             "completed": 0.009614550999999999
           }
         },
         "level": "notice"
       }
     ]
   }
 *)
let wait_for_denunciation_injection node client oph_promise =
  let filter json =
    match JSON.(json |-> "view" |-> "request" |> as_string_opt) with
    | Some s when s = "inject" -> Some s
    | Some _ | None -> None
  in
  let* _ = Node.wait_for node "request_completed_notice.v0" filter in
  let* oph = oph_promise in
  let* mempool = RPC.get_mempool_pending_operations client in
  if is_operation_in_applied_mempool mempool oph then some oph else none

(* This tests aims to detect a double baking evidence with an
   accuser. The scenario is the following:

   1. Run Node 1 and Node 2, connect Node 1 to Node 2 and bake the
   activation block. Terminate Node 2.

   2. Node 1 bakes three blocks with bootstrap1 key. Node 1 is
   terminated.

   3. Then, Node 2 is restarted and bakes two blocks from level 1; the
   first one with the bootstrap2 key and the second one with the
   bootstrap1 key. Thus, the block at level 3 is double baked (and we
   ensure that the double baked blocks are different as they emanate
   from two distinct branches),

   4. Node 3 is run with its accuser. we bake one block and wait for
   the accuser to be ready.

   5. We connect Node 3 with Node 2. We wait Node 3 to catch up. The
   accuser have seen one block at level 3 baked by bootstrap1.

   6. Node 1 is run and connected to Node 3. We wait for Node 3 to
   catch up. Because Node 1 branch is longer, Node 3 needs to switch
   from its current branch to the Node 1 branch. This way, we ensure
   that Node 3 will revalidate a block at level 3 from Node 1's
   branch. Consequently, the accuser will see a second block at level
   3 baked by bootstrap1.

   7. A block is baked with the accusation operation.

   8. Check the denunciation is in the last block. *)
let double_bake =
  Protocol.register_test
    ~__FILE__
    ~title:
      (sf
         "double baking with accuser%s"
         (Option.fold ~none:"" ~some:(fun d -> ", delay: " ^ d)
         @@ Sys.getenv_opt "TEZOS_ACCUSER_DELAY"))
    ~tags:["double"; "baking"; "accuser"; "node"]
  @@ fun protocol ->
  let log_step counter msg =
    let color = Log.Color.(bold ++ FG.blue) in
    let prefix = "step" ^ string_of_int counter in
    Log.info ~color ~prefix msg
  in
  (* common_ancestor is only the activation block. *)
  let common_ancestor = 1 in
  let node_2_branch_size = 2 in
  let node_1_branch_size = node_2_branch_size + 1 in
  let node_3_first_catch_up_level = common_ancestor + node_2_branch_size in
  let node_3_second_catch_up_level = common_ancestor + node_1_branch_size in
  let node_3_final_level = node_3_second_catch_up_level + 1 in

  log_step 1 "Activate protocol for Node 1 and Node 2. Terminate Node 2." ;
  (* Note: we start all nodes with [--private] to prevent the [connect address]
     command from [node_2] to [node_3] from failing due to an "already connected"
     error that could otherwise non-deterministically occur due to P2P propagation.
     This means that we need to use [trust address] too. *)
  let* node_1 = Node.init [Synchronisation_threshold 0; Private_mode]
  and* node_2 = Node.init [Synchronisation_threshold 0; Private_mode] in
  let* client_1 = Client.init ~endpoint:(Node node_1) ()
  and* client_2 = Client.init ~endpoint:(Node node_2) () in
  let* () = Client.Admin.trust_address client_1 ~peer:node_2
  and* () = Client.Admin.trust_address client_2 ~peer:node_1 in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2 in
  let* () = Client.activate_protocol ~protocol client_1 in
  let bootstrap1_key = Constant.bootstrap1.alias in
  let bootstrap2_key = Constant.bootstrap2.alias in
  let* _ = Node.wait_for_level node_1 common_ancestor
  and* _ = Node.wait_for_level node_2 common_ancestor in
  let* () = Node.terminate node_2 in

  log_step 2 "Bake %d blocks on Node 1 and terminate Node 1." node_1_branch_size ;
  (* Craft a branch of size 2, baked by bootstrap1 *)
  let* () = Client.bake_for ~keys:[bootstrap1_key] client_1 in
  let* () = Client.bake_for ~keys:[bootstrap1_key] client_1 in
  let* () = Client.bake_for ~keys:[bootstrap1_key] client_1 in
  let* _ = Node.wait_for_level node_1 (common_ancestor + node_1_branch_size) in
  let* () = Node.terminate node_1 in

  log_step 3 "Run Node 2 and bake %d blocks" node_2_branch_size ;
  let* () = Node.run node_2 [Synchronisation_threshold 0] in
  let* () = Node.wait_for_ready node_2 in
  (* Craft a branch of size 2, the first block is baked by bootstrap2 *)
  let* () = Client.bake_for ~keys:[bootstrap2_key] client_2 in
  (* The second block is double baked by bootstrap1 to simulate a
     double bake *)
  let* () = Client.bake_for ~keys:[bootstrap1_key] client_2 in
  let* _ = Node.wait_for_level node_2 (common_ancestor + node_2_branch_size) in

  log_step 4 "Run Node 3, bake one block and wait for the accuser to be ready." ;
  let* node_3 = Node.init [Synchronisation_threshold 0; Private_mode] in
  let* client_3 = Client.init ~endpoint:(Node node_3) () in
  let* accuser_3 = Accuser.init ~protocol node_3 in
  let* () = Client.activate_protocol ~protocol client_3 in
  let* _ = Node.wait_for_level node_3 1 in
  (* We bake one block to ensure the accuser daemon receives a
     block. Indeed, there is a strange behavior in the accuser which makes *)
  let* () = Accuser.wait_for_ready accuser_3 in
  let denunciation = wait_for_denunciation accuser_3 in
  let denunciation_injection =
    wait_for_denunciation_injection node_3 client_3 denunciation
  in

  log_step 5 "Connect Node 3 with Node 2 and wait for Node 3 to catch up." ;
  let* () = Client.Admin.trust_address client_2 ~peer:node_3
  and* () = Client.Admin.trust_address client_3 ~peer:node_2 in
  let* () = Client.Admin.connect_address client_2 ~peer:node_3 in
  let* _ = Node.wait_for_level node_3 node_3_first_catch_up_level in

  log_step 6 "Run and connect Node 1 to Node 3. Wait for Node 3 to catch up." ;
  let* () = Node.run node_1 [Synchronisation_threshold 0] in
  let* () = Node.wait_for_ready node_1 in
  let* () = Client.Admin.trust_address client_1 ~peer:node_3
  and* () = Client.Admin.trust_address client_3 ~peer:node_1 in
  let* () = Client.Admin.connect_address client_1 ~peer:node_3 in
  let* _ = Node.wait_for_level node_3 node_3_second_catch_up_level in
  (* Ensure that the denunciation was emitted by the accuser *)
  let* denunciation_oph = denunciation in
  (* Ensure that the denunciation is in node_3's mempool *)
  let* _ = denunciation_injection in

  log_step 7 "Bake a block on Node 3 and wait for everybody to catch up." ;
  let* () = Client.bake_for ~keys:[bootstrap1_key] client_3 in
  let* _ = Node.wait_for_level node_1 node_3_final_level
  and* _ = Node.wait_for_level node_2 node_3_final_level
  and* _ = Node.wait_for_level node_3 node_3_final_level in
  log_step 8 "Check denunciation is in the last block." ;
  (* Getting the operations of the current head *)
  let* ops = RPC.get_operations client_1 in
  let* () = Accuser.terminate accuser_3 in
  if is_operation_in_operations ops denunciation_oph then unit
  else Test.fail "Double baking evidence was not found"

let register ~protocols = double_bake ~protocols
