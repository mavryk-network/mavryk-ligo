(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** This module encapsulates the following (carbonated) storage maps:
    - [Storage.Contract.Frozen_bonds]
    - [Storage.Contract.Total_frozen_bonds]

   This module enforces the following invariants:
    - [ (Frozen_bonds.mem x) <-> (Total_frozen_bonds.mem x) ]
    - [ Total_frozen_bonds.find (ctxt, contract) ] = sum of all bond values in
      [Frozen_bonds] that are associated to [contract]. *)

open Storage.Contract

type error +=
  | Frozen_bonds_must_be_spent_at_once of Contract_repr.t * Bond_id_repr.t

let () =
  register_error_kind
    `Permanent
    ~id:"frozen_bonds.must_be_spent_at_once"
    ~title:"Partial spending of frozen bonds"
    ~description:"Frozen bonds must be spent at once."
    ~pp:(fun ppf (contract, bond_id) ->
      Format.fprintf
        ppf
        "The frozen funds for contract (%a) and bond (%a) are not allowed to \
         be partially withdrawn. The amount withdrawn must be equal to the \
         entire deposit for the said bond."
        Contract_repr.pp
        contract
        Bond_id_repr.pp
        bond_id)
    Data_encoding.(
      obj2
        (req "contract" Contract_repr.encoding)
        (req "bond_id" Bond_id_repr.encoding))
    (function
      | Frozen_bonds_must_be_spent_at_once (c, b) -> Some (c, b) | _ -> None)
    (fun (c, b) -> Frozen_bonds_must_be_spent_at_once (c, b))

let has_frozen_bonds ctxt contract = Total_frozen_bonds.mem ctxt contract >|= ok

let allocated ctxt contract bond_id = Frozen_bonds.mem (ctxt, contract) bond_id

let find ctxt contract bond_id = Frozen_bonds.find (ctxt, contract) bond_id

(** PRE : amount > 0, fullfilled by unique caller [Token.transfer]. *)
let spend_only_call_from_token ctxt contract bond_id amount =
  Contract_delegate_storage.remove_contract_stake ctxt contract amount
  >>=? fun ctxt ->
  Frozen_bonds.get (ctxt, contract) bond_id >>=? fun (ctxt, frozen_bonds) ->
  error_when
    Tez_repr.(frozen_bonds <> amount)
    (Frozen_bonds_must_be_spent_at_once (contract, bond_id))
  >>?= fun () ->
  Frozen_bonds.remove_existing (ctxt, contract) bond_id >>=? fun (ctxt, _) ->
  Total_frozen_bonds.get ctxt contract >>=? fun total ->
  Tez_repr.(total -? amount) >>?= fun new_total ->
  if Tez_repr.(new_total = zero) then
    Total_frozen_bonds.remove_existing ctxt contract
  else Total_frozen_bonds.update ctxt contract new_total

(** PRE : [amount > 0], fullfilled by unique caller [Token.transfer].*)
let credit_only_call_from_token ctxt contract bond_id amount =
  Contract_delegate_storage.add_contract_stake ctxt contract amount
  >>=? fun ctxt ->
  ( Frozen_bonds.find (ctxt, contract) bond_id
  >>=? fun (ctxt, frozen_bonds_opt) ->
    match frozen_bonds_opt with
    | None -> Frozen_bonds.init (ctxt, contract) bond_id amount
    | Some frozen_bonds ->
        Tez_repr.(frozen_bonds +? amount) >>?= fun new_amount ->
        Frozen_bonds.update (ctxt, contract) bond_id new_amount )
  >>=? fun (ctxt, _consumed1) ->
  Total_frozen_bonds.find ctxt contract >>=? fun total_opt ->
  match total_opt with
  | None -> Total_frozen_bonds.init ctxt contract amount
  | Some total ->
      Tez_repr.(total +? amount) >>?= fun new_total ->
      Total_frozen_bonds.update ctxt contract new_total

let total ctxt contract =
  Total_frozen_bonds.find ctxt contract >|=? Option.value ~default:Tez_repr.zero
