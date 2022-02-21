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
    Component:    Protocol Environment
    Invocation:   dune build @src/lib_protocol_environment/runtest
    Dependencies: src/lib_protocol_environment/test/assert.ml
    Subject:      Low-level operations on proxy contexts.
*)

(** Context creation *)

(*
  Genesis -- block2 -- block3a
                  \
                   \-- block3b
*)

let create_block ctxt =
  let open Lwt_syntax in
  let* ctxt = Context.add ctxt ["a"; "b"] (Bytes.of_string "Novembre") in
  let* ctxt = Context.add ctxt ["a"; "c"] (Bytes.of_string "Juin") in
  let* ctxt = Context.add ctxt ["version"] (Bytes.of_string "0.0") in
  Lwt.return ctxt

type t = Context.t

let init_contexts (f : t -> unit Lwt.t) _ () : 'a Lwt.t =
  let open Lwt_syntax in
  let proxy_genesis : Context.t = Proxy_context.empty None in
  let* proxy = create_block proxy_genesis in
  f proxy

let test_cmp msg testfunc val_assert proxy_ctx =
  let open Lwt_syntax in
  (* Assert the value is the one expected *)
  let* proxy_got = testfunc proxy_ctx in
  Format.printf "%s: Value assertion ..." msg;
  val_assert proxy_got ;
  Format.printf " OK\n";
  Lwt.return_unit

(* Test MEM *)
let test_mem_fct proxy =
  let open Lwt_syntax in
  let testmemfct msg path exp =
    test_cmp
      msg
      (fun ctx -> Context.mem ctx path)
      (Assert.equal_bool ~msg exp)
      proxy
  in

  let* () = testmemfct "1st_layer_leaf" ["version"] true in
  let* () = testmemfct "2nd_layer_leaf_0" ["a"; "b"] true in
  let* () = testmemfct "2nd_layer_leaf_1" ["a"; "c"] true in
  let* () = testmemfct "not_exist" ["a"; "d"] false in

  Lwt.return_unit

(* Test MEM TREE *)
let test_mem_tree_fct proxy =
  let open Lwt_syntax in
  let testmemtreefct msg path exp =
    test_cmp
      msg
      (fun ctx -> Context.mem_tree ctx path)
      (Assert.equal_bool ~msg exp)
      proxy
  in

  let* () = testmemtreefct "exist_tree" ["a"] true in
  let* () = testmemtreefct "doesnt_exist_tree" ["b"] false in

  (* let* () = testmemtreefct "is_leaf_not_tree" ["a"; "b"] false in *)
  Lwt.return_unit

(* Test FIND *)
let test_find_fct proxy =
  let open Lwt_syntax in
  let testfindfct msg path exp =
    test_cmp
      msg
      (fun ctx -> Context.find ctx path)
      (Assert.equal_bytes_option ~msg exp)
      proxy
  in

  let* () =
    testfindfct "exist_1stlayer_leaf" ["version"] (Some (Bytes.of_string "0.0"))
  in
  let* () =
    testfindfct "exist_leaf" ["a"; "b"] (Some (Bytes.of_string "Novembre"))
  in
  let* () = testfindfct "doesnt_exist_leaf" ["a"; "x"] None in

  Lwt.return_unit

(* Test FIND TREE *)
let test_find_tree_fct proxy =
  let open Lwt_syntax in
  let testfindtreefct msg path exp =
    test_cmp
      msg
      (fun ctx -> Context.find ctx path)
      (Assert.equal_bytes_option ~msg exp)
      proxy
  in

  let* () =
    testfindtreefct
      "exist_1stlayer_leaf"
      ["version"]
      (Some (Bytes.of_string "0.0"))
  in
  let* () =
    testfindtreefct "exist_leaf" ["a"; "b"] (Some (Bytes.of_string "Novembre"))
  in
  let* () = testfindtreefct "doesnt_exist_leaf" ["a"; "x"] None in

  Lwt.return_unit

(* Test LIST *)
let test_list_fct proxy = 
  let open Lwt_syntax in
  let testlistfct msg path assert_fct =
    test_cmp
      msg
      (fun ctx -> Context.list ctx path)
      (fun ret -> Assert.equal_bool ~msg true (assert_fct ret))
      proxy
  in

  let* () = testlistfct "exist_1stlayer_leaf" ["version"] (( = ) []) in
  let* () = testlistfct "exist_leaf" ["a"; "b"] (( = ) []) in
  let* () = testlistfct "doesnt_exist_leaf" ["a"; "x"] (( = ) []) in
  let* () = testlistfct "exist_tree" ["a"] (fun res -> (List.map (fun (i, _) -> i) res) = ["b"; "c"]) in

  Lwt.return_unit

(******************************************************************************)

let tests =
  [
    ("mem", test_mem_fct);
    ("memtree", test_mem_tree_fct);
    ("find", test_find_fct);
    ("find_tree", test_find_tree_fct);
    ("list", test_list_fct);
    (* Should break with delegation set *)
  ]

let tests : unit Alcotest_lwt.test_case list =
  List.map
    (fun (n, f) -> Alcotest_lwt.test_case n `Quick (init_contexts f))
    tests

let () =
  Alcotest_lwt.run "tezos-shell-proxy-context" [("proxy_context", tests)]
  |> Lwt_main.run
