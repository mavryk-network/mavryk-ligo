(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

type error += (* `Permanent *) Wrong_rejection

type error += (* `Permanent *) Rejection_without_prerejection

type error += (* `Permanent *) Duplicate_prerejection

type t = {
  rollup : Tx_rollup_repr.t;
  level : Raw_level_repr.t;
  hash : Tx_rollup_commitments_repr.Commitment_hash.t;
  batch_index : int;
}

val encoding : t Data_encoding.t

module Rejection_hash : sig
  val rejection_hash : string

  include S.HASH

  module Index : Storage_description.INDEX with type t = t
end

val generate_prerejection :
  nonce:int64 ->
  source:Contract_repr.t ->
  rollup:Tx_rollup_repr.t ->
  level:Raw_level_repr.t ->
  commitment_hash:Tx_rollup_commitments_repr.Commitment_hash.t ->
  batch_index:int ->
  Rejection_hash.t
