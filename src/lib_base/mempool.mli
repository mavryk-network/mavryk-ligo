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

(** Tezos Shell Module - Mempool, a.k.a. the operations safe to be broadcast. *)

type t = {
  known_valid : Tezos_crypto.Operation_hash.t list;
      (** A valid sequence of operations on top of the current head. *)
  pending : Tezos_crypto.Operation_hash.Set.t;
      (** Set of known not-invalid operation. *)
}

type mempool = t

val encoding : mempool Data_encoding.t

val bounded_encoding : ?max_operations:int -> unit -> mempool Data_encoding.t

(** Empty mempool. *)
val empty : mempool

(** [is_empty mempool] returns true if and only if [mempool] is empty. *)
val is_empty : mempool -> bool

(** [cons_valid oph t] prepends [oph] to the [known_valid] field of [t]. *)
val cons_valid : Tezos_crypto.Operation_hash.t -> mempool -> mempool

(** Remove an operation from all the fields of a mempool. *)
val remove : Tezos_crypto.Operation_hash.t -> mempool -> mempool
