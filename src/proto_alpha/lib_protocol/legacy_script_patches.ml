(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type t = {
  addresses : string list;
  hash : Script_expr_hash.t;
  patched_code : Michelson_v1_primitives.prim Micheline.canonical;
}

let script_hash {hash; _} = hash

let code {patched_code; _} = patched_code

let bin_expr_exn hex =
  match
    Option.bind
      (Hex.to_bytes @@ `Hex hex)
      (fun bytes ->
        Data_encoding.Binary.of_bytes_opt Script_repr.expr_encoding bytes)
  with
  | Some expr -> expr
  | None -> raise (Failure "Decoding script failed.")

let patches =
  [
    {
      addresses = ["KT1SL6CGhjPUyLypDbFv9bXsNF2sHG7Fy3j9"];
      hash =
        Script_expr_hash.of_b58check_exn
          "exprteUgjaxjf3jTheaj2fQkQsPFynyj3pCJ4mbzP26D4giEjQBwFb";
      patched_code =
        (* This patched code was obtained by manually editing the Michelson code
           of the smart contract and then converting the modified code to binary
           representation using octez-client convert script command. *)
        bin_expr_exn
          "02000019c70500046c00000005256d61696e050108650861036e036a0000000b3a6465706f7369746f72730765046e00000006256f776e65720765046a0000000c256d696e5f6465706f736974076504620000000d2577697468647261775f666565076504620000000d25636f6c6c61745f636f6566660765046a0000000a256465706f73697465640765046a0000000925626f72726f7765640765046200000010256465706f7369746f72735f73697a65045d000000092564656c6567617465000000083a73746f72616765050202000018f80321051f02000000160417000000104073746f726167655f736c6173685f3104160000000a405f5f736c6173685f32084303620080897a0000000a406f6e655f70726563360931000000cf07650765036a03620362036a02000000be045800000021405f616d6f756e745f636f6566665f5f6f6e655f70726563365f736c6173685f380321041700000012406f6e655f70726563365f736c6173685f33051f02000000020321034c031604170000000640636f656666071f00020200000002032105700002031604160000000740616d6f756e74033a0322072f020000002b0743036801000000204469766973696f6e206572726f7220696e20606170706c795f636f656666602e032702000000020316051f020000000203200000000004420000000c406170706c795f636f6566660448000000074073656e64657204130000000740616d6f756e7404150000001140636f6e74726163745f62616c616e6365071f0005020000000e0421000000084073746f72616765057000050421000000024073020000000e0317041600000006256f776e6572071f0004020000000d0421000000074073656e646572057000040319033c0743036a0080897a071f0004020000000d04210000000740616d6f756e74057000040319032a0314072c02000004c90421000000024073020000002303170317041600000019406d696e5f6465706f73697420256d696e5f6465706f736974071f0003020000000d04210000000740616d6f756e740570000303190337072c020000002907430368010000001e4465706f736974656420616d6f756e7420697320746f6f20736d616c6c2e03270200000002034f0320042100000002407304160000000b256465706f7369746f7273071f0004020000000d0421000000074073656e646572057000040329072f02000001b90421000000024073032104160000000b256465706f7369746f7273034c03170321041600000006256f776e6572034c0317032104160000000c256d696e5f6465706f736974034c0317032104160000000d2577697468647261775f666565034c0317032104160000000d25636f6c6c61745f636f656666034c0317032104160000000a256465706f7369746564034c0317032104160000000925626f72726f776564034c03170417000000092564656c6567617465074303620001071f0009020000000804210000000240730570000902000000240317031703170317031703170317041600000010256465706f7369746f72735f73697a65031204420000001a256465706f7369746f72735f73697a65202564656c6567617465034c04420000000925626f72726f776564034c04420000000a256465706f7369746564034c04420000000d25636f6c6c61745f636f656666034c04420000000d2577697468647261775f666565034c04420000000c256d696e5f6465706f736974034c044200000006256f776e6572034c04420000000b256465706f7369746f7273071f0003020000000d04210000000740616d6f756e74057000030342020000004f051f02000000080421000000024073034c071f0004020000000d04210000000740616d6f756e7405700004071f00020200000008042100000002406205700002071f00030200000002032003120342045800000010405f757365725f62616c616e63655f730321041700000002407304210000000240730317051f02000000080421000000024073034c04160000000b256465706f7369746f7273071f0003020000000203210570000304160000000d40757365725f62616c616e6365071f0008020000000d0421000000074073656e64657205700008051f02000000020346035004420000000e407320256465706f7369746f72730421000000024073032104160000000b256465706f7369746f7273034c03170321041600000006256f776e6572034c0317032104160000000c256d696e5f6465706f736974034c0317032104160000000d2577697468647261775f666565034c0317032104160000000d25636f6c6c61745f636f656666034c03170317071f000b020000000d04210000000740616d6f756e740570000b071f00070200000008042100000002407305700007071f0008020000000405200003020000001a0317031703170317031704160000000a256465706f7369746564031204420000000a256465706f7369746564034c04420000000d25636f6c6c61745f636f656666034c04420000000d2577697468647261775f666565034c04420000000c256d696e5f6465706f736974034c044200000006256f776e6572034c04420000000e407320256465706f7369746f7273053d036d0342020000123b0421000000024073020000000e0317041600000006256f776e6572071f0004020000000d0421000000074073656e646572057000040319033c0743036a0080897a071f0004020000000d04210000000740616d6f756e7405700004031903320314072c02000006f7042100000002407304160000000b256465706f7369746f7273071f0004020000000d0421000000074073656e646572057000040329072f020000002807430368010000001d4f6e6c79206465706f7369746f72732063616e2077697468647261772e0327020000000004580000000d40757365725f62616c616e6365071f0002020000001704210000001140636f6e74726163745f62616c616e636505700002051f020000001304210000000d40757365725f62616c616e6365034c0319032a072c0200000041074303680100000036576974686472617720616d6f756e742067726561746572207468616e2063757272656e7420636f6e74726163742062616c616e63652e03270200000002034f0320051f02000000080421000000024073034c032104160000000b256465706f7369746f7273034c03170321041600000006256f776e6572034c0317032104160000000c256d696e5f6465706f736974034c0317032104160000000d2577697468647261775f666565034c0317032104160000000d25636f6c6c61745f636f656666034c03170317071f0006020000001304210000000d40757365725f62616c616e636505700006071f00080200000008042100000002407305700008020000001a0317031703170317031704160000000a256465706f73697465640393072f0200000004034f0327020000000004420000000a256465706f7369746564034c04420000000d25636f6c6c61745f636f656666034c04420000000d2577697468647261775f666565034c04420000000c256d696e5f6465706f736974034c044200000006256f776e6572034c04420000000e407320256465706f7369746f727304210000000240730317051f02000000080421000000024073034c04160000000b256465706f7369746f7273053e036a071f0008020000000d0421000000074073656e64657205700008035004420000000e407320256465706f7369746f72730421000000024073032104160000000b256465706f7369746f7273034c03170321041600000006256f776e6572034c0317032104160000000c256d696e5f6465706f736974034c0317032104160000000d2577697468647261775f666565034c0317032104160000000d25636f6c6c61745f636f656666034c0317032104160000000a256465706f7369746564034c0317032104160000000925626f72726f776564034c03170417000000092564656c6567617465074303620001071f0009020000000804210000000240730570000902000000240317031703170317031703170317041600000010256465706f7369746f72735f73697a65034b03210311034c0328072c0200000000020000002507430368010000001a4465706f7369746f727320636f756e74696e67206572726f722e032704420000001a256465706f7369746f72735f73697a65202564656c6567617465034c04420000000925626f72726f776564034c04420000000a256465706f7369746564034c04420000000d25636f6c6c61745f636f656666034c04420000000d2577697468647261775f666565034c04420000000c256d696e5f6465706f736974034c044200000006256f776e6572034c04420000000e407320256465706f7369746f7273071f0008020000001204210000000c406170706c795f636f65666605700008051f02000000080421000000024073034c020000001903170317031704160000000d2577697468647261775f666565071f0005020000001304210000000d40757365725f62616c616e6365057000050342051f020000000803210316034c0317034204260000000b406665655f616d6f756e74051f02000000080421000000024073034c053d036d071f000a020000000d0421000000074073656e6465720570000a0555036c072f020000003907430368010000002e4e6f20656e747279706f696e742064656661756c74207769746820706172616d65746572207479706520756e697403270200000000071f000a020000000d04210000000740616d6f756e740570000a071f0004020000001104210000000b406665655f616d6f756e7405700004071f0009020000001304210000000d40757365725f62616c616e6365057000090393072f0200000004034f032702000000000412000000104077697468647261775f616d6f756e74034f044d0000000c406f705f7769746864726177031b071f00030200000008042100000002407305700003020000000e0317041600000006256f776e65720555036c072f020000003907430368010000002e4e6f20656e747279706f696e742064656661756c74207769746820706172616d65746572207479706520756e697403270200000000071f0003020000001104210000000b406665655f616d6f756e7405700003071f0004020000000405200005034f044d00000007406f705f666565031b03420200000ad70421000000024073020000000e0317041600000006256f776e6572071f0004020000000d0421000000074073656e64657205700004031903250743036a0000071f0004020000000d04210000000740616d6f756e7405700004031903250314072c020000067d084303620080897a0000000a406f6e655f70726563360931000000c0076503620362036202000000b304580000001a40636f6566665f5f6f6e655f70726563365f736c6173685f3134032104160000000f40636f6566665f736c6173685f3135051f02000000020321034c041700000012406f6e655f70726563365f736c6173685f33034b03210311034c0328072c0200000000020000003a07430368010000002f496e76616c696420636f656666696369656e742076616c756520696e20606765745f636f6566665f636f6d706c602e0327051f0200000002032000000000044200000010406765745f636f6566665f636f6d706c071f0005020000001204210000000c406170706c795f636f656666057000050342051f02000000080421000000024073034c020000001b031703170317031704160000000d25636f6c6c61745f636f656666071f00020200000008042100000002407305700002020000001a0317031703170317031704160000000a256465706f7369746564034203420321020000001d0317041600000015406170706c795f636f6566665f736c6173685f3133051f02000000020321034c02000000210317041700000019406765745f636f6566665f636f6d706c5f736c6173685f3138071f00020200000002032105700002031604170000000d40636f6c6c61745f636f656666051f020000000803210316034c031703420326071f00020200000002032105700002031604160000000a406465706f73697465640342051f020000000803210316034c0317071f00030200000002032003420326051f02000000080421000000024073034c020000001b03170317031703170317031704160000000925626f72726f776564051f020000001404210000000e406d61785f626f72726f77696e67034c03190337072c020000004a07430368010000003f4e6f20617661696c61626c652066756e647320746f20626f72726f773a20636f6e747261637420697320756e6465722d636f6c6c61746572616c697a65642e03270200000002034f0320071f0002020000001704210000001140636f6e74726163745f62616c616e636505700002071f00020200000008042100000002407305700002020000001b03170317031703170317031704160000000925626f72726f776564071f0002020000001404210000000e406d61785f626f72726f77696e67057000020393072f0200000004034f03270200000000034203210416000000024061051f02000000020321034c04170000000240620421000000024062071f000202000000080421000000024061057000020319032a072c020000000804210000000240620200000011051f02000000080421000000024061034c051f0200000004052000030743036a0000051f020000001004210000000a40626f72726f77696e67034c03190325072c020000002807430368010000001d4e6f20617661696c61626c652066756e647320746f20626f72726f772e03270200000002034f0320071f00020200000008042100000002407305700002032104160000000b256465706f7369746f7273034c03170321041600000006256f776e6572034c0317032104160000000c256d696e5f6465706f736974034c0317032104160000000d2577697468647261775f666565034c0317032104160000000d25636f6c6c61745f636f656666034c0317032104160000000a256465706f7369746564034c03170317071f0007020000001004210000000a40626f72726f77696e6705700007071f000a020000000804210000000240730570000a020000001b03170317031703170317031704160000000925626f72726f776564031204420000000925626f72726f776564034c04420000000a256465706f7369746564034c04420000000d25636f6c6c61745f636f656666034c04420000000d2577697468647261775f666565034c04420000000c256d696e5f6465706f736974034c044200000006256f776e6572034c04420000000e407320256465706f7369746f72730421000000024073053d036d071f00020200000008042100000002407305700002020000000e0317041600000006256f776e65720555036c072f020000003907430368010000002e4e6f20656e747279706f696e742064656661756c74207769746820706172616d65746572207479706520756e697403270200000000071f0004020000001004210000000a40626f72726f77696e6705700004071f0004020000000405200003034f044d00000003406f70031b034202000003ef0421000000024073020000000e0317041600000006256f776e6572071f0004020000000d0421000000074073656e64657205700004031903250743036a00a0a233071f0004020000000d04210000000740616d6f756e7405700004031903250314072c02000000ec0421000000024073053d036d071f00020200000008042100000002407305700002020000001d03170317031703170317031703170417000000092564656c65676174650346044e00000010406f705f7365745f64656c6567617465031b071f00020200000008042100000002407305700002020000000e0317041600000006256f776e65720555036c072f020000003907430368010000002e4e6f20656e747279706f696e742064656661756c74207769746820706172616d65746572207479706520756e6974032702000000000743036a00a0a233034f044d0000000a406f705f726566756e64031b034202000002960421000000024073020000000e0317041600000006256f776e6572071f0004020000000d0421000000074073656e64657205700004031903250743036a0000071f0004020000000d04210000000740616d6f756e74057000040319032a0314072c020000020a0421000000024073020000002503170317031703170317031704160000001340626f72726f7765642025626f72726f776564071f0003020000000d04210000000740616d6f756e74057000030319032a072c020000002d07430368010000002243616e2774206f7665722d636f6c6c61746572616c697a6520636f6e74726163742e03270200000002034f03200421000000024073032104160000000b256465706f7369746f7273034c03170321041600000006256f776e6572034c0317032104160000000c256d696e5f6465706f736974034c0317032104160000000d2577697468647261775f666565034c0317032104160000000d25636f6c6c61745f636f656666034c0317032104160000000a256465706f7369746564034c03170317071f0009020000000d04210000000740616d6f756e7405700009071f00080200000008042100000002407305700008020000001b03170317031703170317031704160000000925626f72726f7765640393072f0200000004034f0327020000000004420000000925626f72726f776564034c04420000000a256465706f7369746564034c04420000000d25636f6c6c61745f636f656666034c04420000000d2577697468647261775f666565034c04420000000c256d696e5f6465706f736974034c044200000006256f776e6572034c04420000000e407320256465706f7369746f7273053d036d03420200000021074303680100000016596f752073686f756c646e277420626520686572652e0327051f020000000405200007";
    };
  ]

let addresses_to_patch =
  List.concat_map
    (fun {hash; patched_code; addresses} ->
      List.map (fun addr -> (addr, hash, patched_code)) addresses)
    patches
