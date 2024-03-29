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

(* Testing
   -------
   Component:    Context
   Invocation:   dune build @src/lib_context/runtest
   Subject:      On context features.
*)

open Context

let ( let* ) = Lwt.bind

let ( let+ ) p f = Lwt.map f p

(* Same as [let* but handle errors using [Assert.fail_msg]. *)
let ( let*!! ) x f =
  let* result = x in
  match result with
  | Error trace ->
      let message = Format.asprintf "%a" Error_monad.pp_print_trace trace in
      Assert.fail_msg "%s" message
  | Ok x -> f x

open Filename.Infix

(** Basic blocks *)

let genesis_block =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

let genesis_protocol =
  Protocol_hash.of_b58check_exn
    "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp"

let genesis_time = Time.Protocol.of_seconds 0L

let chain_id = Chain_id.of_block_hash genesis_block

(** Context creation *)

let commit = commit ~time:Time.Protocol.epoch ~message:""

let create_block2 idx genesis_commit =
  let* o = checkout idx genesis_commit in
  match o with
  | None -> Assert.fail_msg "checkout genesis_block"
  | Some ctxt ->
      let* ctxt = add ctxt ["a"; "b"] (Bytes.of_string "Novembre") in
      let* ctxt = add ctxt ["a"; "c"] (Bytes.of_string "Juin") in
      let* ctxt = add ctxt ["version"] (Bytes.of_string "0.0") in
      commit ctxt

let create_block3a idx block2_commit =
  let* o = checkout idx block2_commit in
  match o with
  | None -> Assert.fail_msg "checkout block2"
  | Some ctxt ->
      let* ctxt = remove ctxt ["a"; "b"] in
      let* ctxt = add ctxt ["a"; "d"] (Bytes.of_string "Mars") in
      commit ctxt

let create_block3b idx block2_commit =
  let* o = checkout idx block2_commit in
  match o with
  | None -> Assert.fail_msg "checkout block3b"
  | Some ctxt ->
      let* ctxt = remove ctxt ["a"; "c"] in
      let* ctxt = add ctxt ["a"; "d"] (Bytes.of_string "Février") in
      commit ctxt

type t = {
  idx : Context.index;
  genesis : Context_hash.t;
  block2 : Context_hash.t;
  block3a : Context_hash.t;
  block3b : Context_hash.t;
}

let wrap_context_init f _ () =
  Lwt_utils_unix.with_tempdir "tezos_test_" (fun base_dir ->
      let root = base_dir // "context" in
      let* idx = Context.init root in
      let*!! genesis =
        Context.commit_genesis
          idx
          ~chain_id
          ~time:genesis_time
          ~protocol:genesis_protocol
      in
      let* block2 = create_block2 idx genesis in
      let* block3a = create_block3a idx block2 in
      let* block3b = create_block3b idx block2 in
      f {idx; genesis; block2; block3a; block3b})

(** Simple test *)

let c = function None -> None | Some s -> Some (Bytes.to_string s)

(** Checkout the context applied until [block2]. It is asserted that
    the following key-values are present:
    - (["version"], ["0.0"])
    - (["a"; "b"], ["Novembre"])
    - (["a; "c""], ["Juin"]) *)
let test_simple {idx; block2; _} =
  let* o = checkout idx block2 in
  match o with
  | None -> Assert.fail_msg "checkout block2"
  | Some ctxt ->
      let* version = find ctxt ["version"] in
      Assert.equal_string_option ~msg:__LOC__ (c version) (Some "0.0") ;
      let* novembre = find ctxt ["a"; "b"] in
      Assert.equal_string_option (Some "Novembre") (c novembre) ;
      let* juin = find ctxt ["a"; "c"] in
      Assert.equal_string_option ~msg:__LOC__ (Some "Juin") (c juin) ;
      Lwt.return_unit

let test_list {idx; block2; _} =
  let* o = checkout idx block2 in
  match o with
  | None -> Assert.fail_msg "checkout block2"
  | Some ctxt ->
      let* ls = list ctxt ["a"] in
      let ls = List.sort compare (List.map fst ls) in
      Assert.equal_string_list ~msg:__LOC__ ["b"; "c"] ls ;
      Lwt.return_unit

(** Checkout the context applied until [block3a]. It is asserted that
    the following key-values are present:
    - (["version"], ["0.0"])
    - (["a"; "c"], ["Juin"])
    - (["a"; "d"], ["Mars"])
    Additionally, the key ["a"; "b"] is associated with nothing as it
    has been removed by block [block3a]. *)
let test_continuation {idx; block3a; _} =
  let* o = checkout idx block3a in
  match o with
  | None -> Assert.fail_msg "checkout block3a"
  | Some ctxt ->
      let* version = find ctxt ["version"] in
      Assert.equal_string_option ~msg:__LOC__ (Some "0.0") (c version) ;
      let* novembre = find ctxt ["a"; "b"] in
      Assert.is_none ~msg:__LOC__ (c novembre) ;
      let* juin = find ctxt ["a"; "c"] in
      Assert.equal_string_option ~msg:__LOC__ (Some "Juin") (c juin) ;
      let* mars = find ctxt ["a"; "d"] in
      Assert.equal_string_option ~msg:__LOC__ (Some "Mars") (c mars) ;
      Lwt.return_unit

(** Checkout the context applied until [block3b]. It is asserted that
    the following key-values are present:
    - (["version"], ["0.0"])
    - (["a"; "b"], ["Novembre"])
    - (["a"; "d"], ["Février"])
    Additionally, the key ["a"; "c"] is associated with nothing as it
    has been removed by block [block3b]. *)
let test_fork {idx; block3b; _} =
  let* o = checkout idx block3b in
  match o with
  | None -> Assert.fail_msg "checkout block3b"
  | Some ctxt ->
      let* version = find ctxt ["version"] in
      Assert.equal_string_option ~msg:__LOC__ (Some "0.0") (c version) ;
      let* novembre = find ctxt ["a"; "b"] in
      Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
      let* juin = find ctxt ["a"; "c"] in
      Assert.is_none ~msg:__LOC__ (c juin) ;
      let* mars = find ctxt ["a"; "d"] in
      Assert.equal_string_option ~msg:__LOC__ (Some "Février") (c mars) ;
      Lwt.return_unit

(** Checkout the context at [genesis] and explicitly replay
    setting/getting key-values. *)
let test_replay {idx; genesis; _} =
  let* o = checkout idx genesis in
  match o with
  | None -> Assert.fail_msg "checkout genesis_block"
  | Some ctxt0 ->
      let* ctxt1 = add ctxt0 ["version"] (Bytes.of_string "0.0") in
      let* ctxt2 = add ctxt1 ["a"; "b"] (Bytes.of_string "Novembre") in
      let* ctxt3 = add ctxt2 ["a"; "c"] (Bytes.of_string "Juin") in
      let* ctxt4a = add ctxt3 ["a"; "d"] (Bytes.of_string "July") in
      let* ctxt4b = add ctxt3 ["a"; "d"] (Bytes.of_string "Juillet") in
      let* ctxt5a = add ctxt4a ["a"; "b"] (Bytes.of_string "November") in
      let* novembre = find ctxt4a ["a"; "b"] in
      Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
      let* november = find ctxt5a ["a"; "b"] in
      Assert.equal_string_option ~msg:__LOC__ (Some "November") (c november) ;
      let* july = find ctxt5a ["a"; "d"] in
      Assert.equal_string_option ~msg:__LOC__ (Some "July") (c july) ;
      let* novembre = find ctxt4b ["a"; "b"] in
      Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
      let* juillet = find ctxt4b ["a"; "d"] in
      Assert.equal_string_option ~msg:__LOC__ (Some "Juillet") (c juillet) ;
      Lwt.return_unit

let fold_keys s root ~order ~init ~f =
  fold s root ~order ~init ~f:(fun k v acc ->
      match Tree.kind v with
      | `Value -> f (root @ k) acc
      | `Tree -> Lwt.return acc)

let steps =
  ["00"; "01"; "02"; "03"; "05"; "06"; "07"; "09"; "0a"; "0b"; "0c";
   "0e"; "0f"; "10"; "11"; "12"; "13"; "14"; "15"; "16"; "17"; "19";
   "1a"; "1b"; "1c"; "1d"; "1e"; "1f"; "20"; "22"; "23"; "25"; "26";
   "27"; "28"; "2a"; "2b"; "2f"; "30"; "31"; "32"; "33"; "35"; "36";
   "37"; "3a"; "3b"; "3c"; "3d"; "3e"; "3f"; "40"; "42"; "43"; "45";
   "46"; "47"; "48"; "4a"; "4b"; "4c"; "4e"; "4f"; "50"; "52"; "53";
   "54"; "55"; "56"; "57"; "59"; "5b"; "5c"; "5f"; "60"; "61"; "62";
   "63"; "64"; "65"; "66"; "67"; "69"; "6b"; "6c"; "6d"; "6e"; "6f";
   "71"; "72"; "73"; "74"; "75"; "78"; "79"; "7a"; "7b"; "7c"; "7d";
   "7e"; "80"; "82"; "83"; "84"; "85"; "86"; "88"; "8b"; "8c"; "8d";
   "8f"; "92"; "93"; "94"; "96"; "97"; "99"; "9a"; "9b"; "9d"; "9e";
   "9f"; "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"; "a8"; "aa";
   "ab"; "ac"; "ad"; "ae"; "af"; "b0"; "b1"; "b2"; "b3"; "b4"; "b6";
   "b8"; "b9"; "bb"; "bc"; "bf"; "c0"; "c1"; "c2"; "c3"; "c4"; "c5";
   "c8"; "c9"; "cb"; "cc"; "cd"; "ce"; "d0"; "d1"; "d2"; "d4"; "d5";
   "d7"; "d8"; "d9"; "da"; "e0"; "e3"; "e6"; "e8"; "e9"; "ea"; "ec";
   "ee"; "ef"; "f0"; "f1"; "f5"; "f7"; "f8"; "f9"; "fb"; "fc"; "fd";
   "fe"; "ff"]
[@@ocamlformat "disable"]

let bindings =
  let zero = Bytes.make 10 '0' in
  List.map (fun x -> (["root"; x], zero)) steps

let test_fold_keys ~order {idx; genesis; _} =
  let keys t =
    fold_keys t ~order ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))
  in
  let sort_keys l =
    match order with `Sorted -> List.rev l | `Undefined -> List.sort compare l
  in
  let* o = checkout idx genesis in
  match o with
  | None -> Assert.fail_msg "checkout genesis_block"
  | Some ctxt ->
      let* ctxt = add ctxt ["a"; "b"] (Bytes.of_string "Novembre") in
      let* ctxt = add ctxt ["a"; "c"] (Bytes.of_string "Juin") in
      let* ctxt = add ctxt ["a"; "d"; "e"] (Bytes.of_string "Septembre") in
      let* ctxt = add ctxt ["f"] (Bytes.of_string "Avril") in
      let* ctxt = add ctxt ["g"; "h"] (Bytes.of_string "Avril") in
      let* l = keys ctxt [] in
      let l = sort_keys l in
      Assert.equal_string_list_list
        ~msg:__LOC__
        [["a"; "b"]; ["a"; "c"]; ["a"; "d"; "e"]; ["f"]; ["g"; "h"]]
        l ;
      let* l = keys ctxt ["a"] in
      let l = sort_keys l in
      Assert.equal_string_list_list
        ~msg:__LOC__
        [["a"; "b"]; ["a"; "c"]; ["a"; "d"; "e"]]
        l ;
      let* l = keys ctxt ["f"] in
      Assert.equal_string_list_list ~msg:__LOC__ [] l ;
      let* l = keys ctxt ["g"] in
      Assert.equal_string_list_list ~msg:__LOC__ [["g"; "h"]] l ;
      let* l = keys ctxt ["i"] in
      Assert.equal_string_list_list ~msg:__LOC__ [] l ;
      let* ctxt =
        Lwt_list.fold_left_s (fun ctxt (k, v) -> add ctxt k v) ctxt bindings
      in
      let* h = commit ctxt in
      let* ctxt = checkout_exn idx h in
      let* bs =
        fold_keys ctxt ["root"] ~order ~init:[] ~f:(fun k acc ->
            Lwt.return (k :: acc))
      in
      let bs = sort_keys bs in
      Assert.equal_string_list_list ~msg:__LOC__ (List.map fst bindings) bs ;
      Lwt.return_unit

let test_fold_keys_sorted = test_fold_keys ~order:`Sorted

let test_fold_keys_undefined = test_fold_keys ~order:`Undefined

(** Checkout the context at [genesis] and fold upon a context a series
    of key settings. *)
let test_fold {idx; genesis; _} =
  let* o = checkout idx genesis in
  match o with
  | None -> Assert.fail_msg "checkout genesis_block"
  | Some ctxt ->
      let foo1 = Bytes.of_string "foo1" in
      let foo2 = Bytes.of_string "foo2" in
      let* ctxt = add ctxt ["foo"; "toto"] foo1 in
      let* ctxt = add ctxt ["foo"; "bar"; "toto"] foo2 in
      let fold depth ecs ens =
        let* (cs, ns) =
          fold
            ?depth
            ctxt
            []
            ~order:`Sorted
            ~init:([], [])
            ~f:(fun path t (cs, ns) ->
              match Tree.kind t with
              | `Tree -> Lwt.return (cs, path :: ns)
              | `Value -> Lwt.return (path :: cs, ns))
        in
        Assert.equal_string_list_list ~msg:__LOC__ ecs cs ;
        Assert.equal_string_list_list ~msg:__LOC__ ens ns ;
        Lwt.return ()
      in
      let* () =
        fold
          None
          [["foo"; "toto"]; ["foo"; "bar"; "toto"]]
          [["foo"; "bar"]; ["foo"]; []]
      in
      let* () = fold (Some (`Eq 0)) [] [[]] in
      let* () = fold (Some (`Eq 1)) [] [["foo"]] in
      let* () = fold (Some (`Eq 2)) [["foo"; "toto"]] [["foo"; "bar"]] in
      let* () = fold (Some (`Lt 2)) [] [["foo"]; []] in
      let* () =
        fold (Some (`Le 2)) [["foo"; "toto"]] [["foo"; "bar"]; ["foo"]; []]
      in
      let* () =
        fold
          (Some (`Ge 2))
          [["foo"; "toto"]; ["foo"; "bar"; "toto"]]
          [["foo"; "bar"]]
      in
      fold (Some (`Gt 2)) [["foo"; "bar"; "toto"]] []

let test_trees {idx; genesis; _} =
  let* o = checkout idx genesis in
  match o with
  | None -> Assert.fail_msg "checkout genesis_block"
  | Some ctxt ->
      let* () =
        Tree.fold
          ~depth:(`Eq 1)
          ~order:`Sorted
          ~init:()
          (Tree.empty ctxt)
          []
          ~f:(fun k _ () ->
            assert (Compare.List_length_with.(k = 1)) ;
            Assert.fail_msg "empty")
      in
      let foo1 = Bytes.of_string "foo1" in
      let foo2 = Bytes.of_string "foo2" in
      Tree.empty ctxt |> fun v1 ->
      let* v1 = Tree.add v1 ["foo"; "toto"] foo1 in
      let* v1 = Tree.add v1 ["foo"; "bar"; "toto"] foo2 in
      let fold depth ecs ens =
        let* (cs, ns) =
          Tree.fold
            v1
            ?depth
            []
            ~order:`Sorted
            ~init:([], [])
            ~f:(fun path t (cs, ns) ->
              match Tree.kind t with
              | `Tree -> Lwt.return (cs, path :: ns)
              | `Value -> Lwt.return (path :: cs, ns))
        in
        Assert.equal_string_list_list ~msg:__LOC__ ecs cs ;
        Assert.equal_string_list_list ~msg:__LOC__ ens ns ;
        Lwt.return ()
      in
      let* () =
        fold
          None
          [["foo"; "toto"]; ["foo"; "bar"; "toto"]]
          [["foo"; "bar"]; ["foo"]; []]
      in
      let* () = fold (Some (`Eq 0)) [] [[]] in
      let* () = fold (Some (`Eq 1)) [] [["foo"]] in
      let* () = fold (Some (`Eq 2)) [["foo"; "toto"]] [["foo"; "bar"]] in
      let* () = fold (Some (`Lt 2)) [] [["foo"]; []] in
      let* () =
        fold (Some (`Le 2)) [["foo"; "toto"]] [["foo"; "bar"]; ["foo"]; []]
      in
      let* () =
        fold
          (Some (`Ge 2))
          [["foo"; "toto"]; ["foo"; "bar"; "toto"]]
          [["foo"; "bar"]]
      in
      let* () = fold (Some (`Gt 2)) [["foo"; "bar"; "toto"]] [] in
      let* v1 = Tree.remove v1 ["foo"; "bar"; "toto"] in
      let* v = Tree.find v1 ["foo"; "bar"; "toto"] in
      Assert.equal_bytes_option ~msg:__LOC__ None v ;
      let* v = Tree.find v1 ["foo"; "toto"] in
      Assert.equal_bytes_option ~msg:__LOC__ (Some foo1) v ;
      Tree.empty ctxt |> fun v1 ->
      let* v1 = Tree.add v1 ["foo"; "1"] foo1 in
      let* v1 = Tree.add v1 ["foo"; "2"] foo2 in
      let* v1 = Tree.remove v1 ["foo"; "1"] in
      let* v1 = Tree.remove v1 ["foo"; "2"] in
      let* v = Tree.find v1 ["foo"; "1"] in
      Assert.equal_bytes_option ~msg:__LOC__ None v ;
      let* v1 = Tree.remove v1 [] in
      Assert.equal_bool ~msg:__LOC__ true (Tree.is_empty v1) ;
      Lwt.return ()

let test_raw {idx; genesis; _} =
  let* o = checkout idx genesis in
  match o with
  | None -> Assert.fail_msg "checkout genesis_block"
  | Some ctxt ->
      let foo1 = Bytes.of_string "foo1" in
      let foo2 = Bytes.of_string "foo2" in
      let* ctxt = add ctxt ["foo"; "toto"] foo1 in
      let* ctxt = add ctxt ["foo"; "bar"; "toto"] foo2 in
      let* tree = find_tree ctxt [] in
      let tree = WithExceptions.Option.get ~loc:__LOC__ tree in
      let* raw = Tree.to_raw tree in
      let a = TzString.Map.singleton "toto" (`Value foo1) in
      let b = TzString.Map.singleton "toto" (`Value foo2) in
      let c = TzString.Map.add "bar" (`Tree b) a in
      let d = TzString.Map.singleton "foo" (`Tree c) in
      let e = `Tree d in
      Assert.equal_raw_tree ~msg:__LOC__ e raw ;
      Lwt.return ()

let string n = String.make n 'a'

let test_encoding {idx; genesis; _} =
  let* o = checkout idx genesis in
  match o with
  | None -> Assert.fail_msg "checkout genesis_block"
  | Some ctxt ->
      let foo1 = Bytes.of_string "foo1" in
      let foo2 = Bytes.of_string "foo2" in
      let* ctxt = add ctxt ["a"; string 7] foo1 in
      let* ctxt = add ctxt ["a"; string 8] foo2 in
      let* ctxt = add ctxt [string 16] foo2 in
      let* ctxt = add ctxt [string 32] foo2 in
      let* ctxt = add ctxt [string 64] foo2 in
      let* ctxt = add ctxt [string 127] foo2 in
      let* h = commit ctxt in
      Assert.equal_context_hash
        ~msg:__LOC__
        (Context_hash.of_b58check_exn
           "CoWJsL2ehZ39seTr8inBCJb5tVjW8KGNweJ5cvuVq51mAASrRmim")
        h ;
      let* ctxt = add ctxt [string 255] foo2 in
      let* h = commit ctxt in
      Assert.equal_context_hash
        ~msg:__LOC__
        (Context_hash.of_b58check_exn
           "CoVexcEHMXmSA2k42aNc5MCDtVJFRs3CC6vcQWYwFoj7EFsBPw1c")
        h ;
      Lwt.return ()

(** Exports to a dump file the context reached at block [block2b].
    After importing it, it is asserted that the context hash is
    preserved. *)
let test_dump {idx; block3b; _} =
  let*!! () =
    Lwt_utils_unix.with_tempdir "tezos_test_" (fun base_dir2 ->
        let open Lwt_result_syntax in
        let dumpfile = base_dir2 // "dump" in
        let ctxt_hash = block3b in
        let mk_empty_block_header context =
          Block_header.
            {
              protocol_data = Bytes.empty;
              shell =
                {
                  level = 0l;
                  proto_level = 0;
                  predecessor = Block_hash.zero;
                  timestamp = Time.Protocol.epoch;
                  validation_passes = 0;
                  operations_hash = Operation_list_list_hash.zero;
                  fitness = [];
                  context;
                };
            }
        in
        let empty_block_header = mk_empty_block_header ctxt_hash in
        let nb_context_elements = 0 in
        let target_context_hash = empty_block_header.shell.context in
        let* _ =
          let*! context_fd =
            Lwt_unix.openfile
              dumpfile
              Lwt_unix.[O_WRONLY; O_CREAT; O_TRUNC]
              0o644
          in
          Lwt.finalize
            (fun () ->
              Context.dump_context idx target_context_hash ~fd:context_fd)
            (fun () -> Lwt_unix.close context_fd)
        in
        let root = base_dir2 // "context" in
        let*! idx2 = Context.init ?patch_context:None root in
        let*! context_fd =
          Lwt_unix.openfile dumpfile Lwt_unix.[O_RDONLY] 0o444
        in
        Lwt.finalize
          (fun () ->
            Context.restore_context
              idx2
              ~expected_context_hash:target_context_hash
              ~nb_context_elements
              ~fd:context_fd)
          (fun () -> Lwt_unix.close context_fd))
  in
  Lwt.return_unit

let test_is_empty {idx; block2; _} =
  let* o = checkout idx block2 in
  match o with
  | None -> Assert.fail_msg "checkout block2"
  | Some ctxt -> (
      (* By [create_block2] above, [ctxt] maps "a/b", "a/c", and "version" *)
      let etree = Context.Tree.empty ctxt in
      Assert.equal_bool true (Tree.is_empty etree) ;
      let* o = Context.find_tree ctxt ["a"] in
      match o with
      | None -> Assert.fail_msg "dir 'a/' not found"
      | Some dir_a ->
          let* dir_a = Tree.remove dir_a ["b"] in
          let* dir_a = Tree.remove dir_a ["c"] in
          let* ls = Tree.list dir_a [] in
          let assert_equal_ls = Assert.make_equal_list ( = ) fst in
          assert_equal_ls
            ~msg:"length of directory /a/ is unexpectedly not 0"
            []
            ls ;
          Assert.equal_context_hash
            ~msg:
              "A fresh empty tree has the same hash as a tree containing data \
               after removing all its data"
            (Tree.hash etree)
            (Tree.hash dir_a) ;
          Assert.equal_bool
            ~msg:"directory /a/ is unexpectedly not empty"
            true
            (Context.Tree.is_empty dir_a) ;
          Lwt.return_unit)

(** Test that [get_hash_version succeeds] *)
let test_get_version_hash {idx; block2; _} =
  let+ ctxt = Context.checkout_exn idx block2 in
  let _ = get_hash_version ctxt in
  ()

(** Test [set_hash_version] on values on which it goes into the error monad *)
let test_set_version_hash_tzresult {idx; block2; _} =
  List.iter_s
    (fun wrong_version ->
      let* ctxt = Context.checkout_exn idx block2 in
      let+ r =
        set_hash_version ctxt @@ Context_hash.Version.of_int wrong_version
      in
      match r with
      | Ok _ -> Assert.fail_msg "set_hash_version should have returned Error _"
      | Error _ -> ())
    (* Only version 0 is supported atm *)
    [1; 2; 256]

(******************************************************************************)

let tests : (string * (t -> unit Lwt.t)) list =
  [
    ("is_empty", test_is_empty);
    ("simple", test_simple);
    ("list", test_list);
    ("continuation", test_continuation);
    ("fork", test_fork);
    ("replay", test_replay);
    ("fold_keys_sorted", test_fold_keys_sorted);
    ("fold_keys_undefined", test_fold_keys_undefined);
    ("fold", test_fold);
    ("trees", test_trees);
    ("raw", test_raw);
    ("dump", test_dump);
    ("encoding", test_encoding);
    ("get_hash_version", test_get_version_hash);
    ("set_hash_version_tzresult", test_set_version_hash_tzresult);
  ]

let tests =
  List.map
    (fun (s, f) -> Alcotest_lwt.test_case s `Quick (wrap_context_init f))
    tests
