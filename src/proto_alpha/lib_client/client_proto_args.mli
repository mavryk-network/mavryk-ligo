(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Protocol
open Alpha_context
open Protocol_client_context

val tez_sym: string

val init_arg: (string, full) Clic.arg
val fee_arg: (Tez.t option, full) Clic.arg
val counter_arg: (Z.t option, full) Clic.arg
val gas_limit_arg: (Z.t option, full) Clic.arg
val storage_limit_arg: (Z.t option, full) Clic.arg
val arg_arg: (string option, full) Clic.arg
val source_arg: (string option, full) Clic.arg
val entrypoint_arg: (string option, full) Clic.arg

val delegate_arg: (Signature.Public_key_hash.t option, full) Clic.arg
val delegatable_switch: (bool, full) Clic.arg
val spendable_switch: (bool, full) Clic.arg
val max_priority_arg: (int option, full) Clic.arg
val minimal_fees_arg: (Tez.tez, full) Clic.arg
val minimal_nanotez_per_gas_unit_arg: (Z.t, full) Clic.arg
val minimal_nanotez_per_byte_arg: (Z.t, full) Clic.arg
val force_low_fee_arg: (bool, full) Clic.arg
val fee_cap_arg: (Tez.t, full) Clic.arg
val burn_cap_arg: (Tez.t, full) Clic.arg
val no_waiting_for_endorsements_arg: (bool, full) Clic.arg
val await_endorsements_arg: (bool, full) Clic.arg
val force_switch: (bool, full) Clic.arg
val minimal_timestamp_switch: (bool, full) Clic.arg
val endorsement_delay_arg: (int, full) Clic.arg
val preserved_levels_arg: (int, full) Clic.arg

val no_print_source_flag: (bool, full) Clic.arg
val no_confirmation: (bool, full) Clic.arg

val tez_arg :
  default:string ->
  parameter:string ->
  doc:string ->
  (Tez.t, full) Clic.arg
val tez_param :
  name:string ->
  desc:string ->
  ('a, full) Clic.params ->
  (Tez.t -> 'a, full) Clic.params

val signature_parameter :
  (Signature.t, full) Clic.parameter

module Daemon : sig
  val baking_switch: (bool, full) Clic.arg
  val endorsement_switch: (bool, full) Clic.arg
  val denunciation_switch: (bool, full) Clic.arg
end

val int_parameter : (int, full) Clic.parameter
val string_parameter : (string, full) Clic.parameter
val bytes_parameter : (MBytes.t, full) Clic.parameter
