(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

(* ------------------------------------------------------------------------- *)
(* Mockup protocol parameters *)

type mockup_protocol_parameters = {
  initial_timestamp : Time.Protocol.t;
  bootstrap_accounts : Parameters.bootstrap_account list;
  bootstrap_contracts : Parameters.bootstrap_contract list;
  constants : Constants.parametric;
}

(** Equivalent of [Constants.parametric] with additionally [chain_id] and [timestamp] but each field is wrapped in an [option].
[Some] is an override, [None] means "Use the default value".
*)
type protocol_constants_overrides = {
  preserved_cycles : int option;
  blocks_per_cycle : int32 option;
  blocks_per_commitment : int32 option;
  blocks_per_roll_snapshot : int32 option;
  blocks_per_voting_period : int32 option;
  time_between_blocks : Period.t list option;
  endorsers_per_block : int option;
  hard_gas_limit_per_operation : Gas.Arith.integral option;
  hard_gas_limit_per_block : Gas.Arith.integral option;
  proof_of_work_threshold : int64 option;
  tokens_per_roll : Tez.t option;
  michelson_maximum_type_size : int option;
  seed_nonce_revelation_tip : Tez.t option;
  origination_size : int option;
  block_security_deposit : Tez.t option;
  endorsement_security_deposit : Tez.t option;
  baking_reward_per_endorsement : Tez.t list option;
  endorsement_reward : Tez.t list option;
  cost_per_byte : Tez.t option;
  hard_storage_limit_per_operation : Z.t option;
  quorum_min : int32 option;
  quorum_max : int32 option;
  min_proposal_quorum : int32 option;
  initial_endorsers : int option;
  delay_per_missing_endorsement : Period.t option;
  (* Additional, "bastard" parameters (they are not protocol constants but partially treated the same way). *)
  chain_id : Chain_id.t option;
  timestamp : Time.Protocol.t option;
}

type parsed_account_repr = {
  name : string;
  sk_uri : Client_keys.sk_uri;
  amount : Tez.t;
}

let parsed_account_repr_pp ppf account =
  let open Format in
  let format_amount ppf value = fprintf ppf "amount:%a" Tez.pp value in
  fprintf
    ppf
    "@[<v>name:%s@,sk_uri:%s@,%a@]"
    account.name
    (Uri.to_string (account.sk_uri :> Uri.t))
    format_amount
    account.amount

let bootstrap_account_encoding : Parameters.bootstrap_account Data_encoding.t =
  let open Data_encoding in
  let open Parameters in
  conv
    (fun {public_key_hash; public_key; amount} ->
      (public_key_hash, public_key, amount))
    (fun (public_key_hash, public_key, amount) ->
      {public_key_hash; public_key; amount})
    (obj3
       (req "public_key_hash" Signature.Public_key_hash.encoding)
       (opt "public_key" Signature.Public_key.encoding)
       (req "amount" Tez.encoding))

let bootstrap_contract_encoding : Parameters.bootstrap_contract Data_encoding.t
    =
  let open Data_encoding in
  let open Parameters in
  conv
    (fun {delegate; amount; script} -> (delegate, amount, script))
    (fun (delegate, amount, script) -> {delegate; amount; script})
    (obj3
       (req "delegate" Baker_hash.encoding)
       (req "amount" Tez.encoding)
       (req "script" Script.encoding))

let mockup_protocol_parameters_encoding :
    mockup_protocol_parameters Data_encoding.t =
  let open Data_encoding in
  conv
    (fun p ->
      ( p.initial_timestamp,
        p.bootstrap_accounts,
        p.bootstrap_contracts,
        p.constants ))
    (fun (initial_timestamp, bootstrap_accounts, bootstrap_contracts, constants)
         ->
      {initial_timestamp; bootstrap_accounts; bootstrap_contracts; constants})
    (obj4
       (req "initial_timestamp" Time.Protocol.encoding)
       (req "bootstrap_accounts" (list bootstrap_account_encoding))
       (req "bootstrap_contracts" (list bootstrap_contract_encoding))
       (req "constants" Constants.parametric_encoding))

(*TODO Reorganize code. Maybe use submodules? *)

(** Shamefully copied from [Constants_repr.parametric_encoding] and adapted ([opt] instead of [req]). *)
let protocol_constants_overrides_encoding :
    protocol_constants_overrides Data_encoding.t =
  let open Data_encoding in
  conv
    (fun c ->
      ( ( c.preserved_cycles,
          c.blocks_per_cycle,
          c.blocks_per_commitment,
          c.blocks_per_roll_snapshot,
          c.blocks_per_voting_period,
          c.time_between_blocks,
          c.endorsers_per_block,
          c.hard_gas_limit_per_operation,
          c.hard_gas_limit_per_block ),
        ( ( c.proof_of_work_threshold,
            c.tokens_per_roll,
            c.michelson_maximum_type_size,
            c.seed_nonce_revelation_tip,
            c.origination_size,
            c.block_security_deposit,
            c.endorsement_security_deposit,
            c.baking_reward_per_endorsement ),
          ( c.endorsement_reward,
            c.cost_per_byte,
            c.hard_storage_limit_per_operation,
            c.quorum_min,
            c.quorum_max,
            c.min_proposal_quorum,
            c.initial_endorsers,
            c.delay_per_missing_endorsement,
            c.chain_id,
            c.timestamp ) ) ))
    (fun ( ( preserved_cycles,
             blocks_per_cycle,
             blocks_per_commitment,
             blocks_per_roll_snapshot,
             blocks_per_voting_period,
             time_between_blocks,
             endorsers_per_block,
             hard_gas_limit_per_operation,
             hard_gas_limit_per_block ),
           ( ( proof_of_work_threshold,
               tokens_per_roll,
               michelson_maximum_type_size,
               seed_nonce_revelation_tip,
               origination_size,
               block_security_deposit,
               endorsement_security_deposit,
               baking_reward_per_endorsement ),
             ( endorsement_reward,
               cost_per_byte,
               hard_storage_limit_per_operation,
               quorum_min,
               quorum_max,
               min_proposal_quorum,
               initial_endorsers,
               delay_per_missing_endorsement,
               chain_id,
               timestamp ) ) ) ->
      {
        preserved_cycles;
        blocks_per_cycle;
        blocks_per_commitment;
        blocks_per_roll_snapshot;
        blocks_per_voting_period;
        time_between_blocks;
        endorsers_per_block;
        hard_gas_limit_per_operation;
        hard_gas_limit_per_block;
        proof_of_work_threshold;
        tokens_per_roll;
        michelson_maximum_type_size;
        seed_nonce_revelation_tip;
        origination_size;
        block_security_deposit;
        endorsement_security_deposit;
        baking_reward_per_endorsement;
        endorsement_reward;
        cost_per_byte;
        hard_storage_limit_per_operation;
        quorum_min;
        quorum_max;
        min_proposal_quorum;
        initial_endorsers;
        delay_per_missing_endorsement;
        chain_id;
        timestamp;
      })
    (merge_objs
       (obj9
          (opt "preserved_cycles" uint8)
          (opt "blocks_per_cycle" int32)
          (opt "blocks_per_commitment" int32)
          (opt "blocks_per_roll_snapshot" int32)
          (opt "blocks_per_voting_period" int32)
          (opt "time_between_blocks" (list Period.encoding))
          (opt "endorsers_per_block" uint16)
          (opt "hard_gas_limit_per_operation" Gas.Arith.z_integral_encoding)
          (opt "hard_gas_limit_per_block" Gas.Arith.z_integral_encoding))
       (merge_objs
          (obj8
             (opt "proof_of_work_threshold" int64)
             (opt "tokens_per_roll" Tez.encoding)
             (opt "michelson_maximum_type_size" uint16)
             (opt "seed_nonce_revelation_tip" Tez.encoding)
             (opt "origination_size" int31)
             (opt "block_security_deposit" Tez.encoding)
             (opt "endorsement_security_deposit" Tez.encoding)
             (opt "baking_reward_per_endorsement" (list Tez.encoding)))
          (obj10
             (opt "endorsement_reward" (list Tez.encoding))
             (opt "cost_per_byte" Tez.encoding)
             (opt "hard_storage_limit_per_operation" z)
             (opt "quorum_min" int32)
             (opt "quorum_max" int32)
             (opt "min_proposal_quorum" int32)
             (opt "initial_endorsers" uint16)
             (opt "delay_per_missing_endorsement" Period.encoding)
             (opt "chain_id" Chain_id.encoding)
             (opt "initial_timestamp" Time.Protocol.encoding))))

let default_mockup_parameters : mockup_protocol_parameters =
  let parameters =
    Default_parameters.parameters_of_constants
      Default_parameters.constants_sandbox
  in
  {
    initial_timestamp = Time.Protocol.epoch;
    bootstrap_accounts = parameters.bootstrap_accounts;
    bootstrap_contracts = parameters.bootstrap_contracts;
    constants = parameters.constants;
  }

let default_mockup_protocol_constants
    (cctxt : Tezos_client_base.Client_context.full) :
    protocol_constants_overrides tzresult Lwt.t =
  let cpctxt = new Protocol_client_context.wrap_full cctxt in
  Protocol.Constants_services.all cpctxt (cpctxt#chain, cpctxt#block)
  >>=? fun {parametric; _} ->
  let to_chain_id_opt = function `Hash c -> Some c | _ -> None in
  Shell_services.Blocks.Header.shell_header
    cpctxt
    ~chain:cpctxt#chain
    ~block:cpctxt#block
    ()
  >>=? fun header ->
  return
    {
      preserved_cycles = Some parametric.preserved_cycles;
      blocks_per_cycle = Some parametric.blocks_per_cycle;
      blocks_per_commitment = Some parametric.blocks_per_commitment;
      blocks_per_roll_snapshot = Some parametric.blocks_per_roll_snapshot;
      blocks_per_voting_period = Some parametric.blocks_per_voting_period;
      time_between_blocks = Some parametric.time_between_blocks;
      endorsers_per_block = Some parametric.endorsers_per_block;
      hard_gas_limit_per_operation =
        Some parametric.hard_gas_limit_per_operation;
      hard_gas_limit_per_block = Some parametric.hard_gas_limit_per_block;
      proof_of_work_threshold = Some parametric.proof_of_work_threshold;
      tokens_per_roll = Some parametric.tokens_per_roll;
      michelson_maximum_type_size = Some parametric.michelson_maximum_type_size;
      seed_nonce_revelation_tip = Some parametric.seed_nonce_revelation_tip;
      origination_size = Some parametric.origination_size;
      block_security_deposit = Some parametric.block_security_deposit;
      endorsement_security_deposit =
        Some parametric.endorsement_security_deposit;
      baking_reward_per_endorsement =
        Some parametric.baking_reward_per_endorsement;
      endorsement_reward = Some parametric.endorsement_reward;
      cost_per_byte = Some parametric.cost_per_byte;
      hard_storage_limit_per_operation =
        Some parametric.hard_storage_limit_per_operation;
      quorum_min = Some parametric.quorum_min;
      quorum_max = Some parametric.quorum_max;
      min_proposal_quorum = Some parametric.min_proposal_quorum;
      initial_endorsers = Some parametric.initial_endorsers;
      delay_per_missing_endorsement =
        Some parametric.delay_per_missing_endorsement;
      (* Bastard, additional parameters. *)
      chain_id = to_chain_id_opt cpctxt#chain;
      timestamp = Some header.timestamp;
    }

(* Use the wallet to convert a bootstrap account's public key
  into a parsed_account_repr secret key Uri *)
let bootstrap_account_to_parsed_account_repr cctxt
    (bootstrap_account : Parameters.bootstrap_account) =
  Client_keys.get_key cctxt bootstrap_account.public_key_hash
  >>=? fun (name, _, sk_uri) ->
  return {name; sk_uri; amount = bootstrap_account.amount}

let parsed_account_repr_encoding =
  let open Data_encoding in
  conv
    (fun p -> (p.name, p.sk_uri, p.amount))
    (fun (name, sk_uri, amount) -> {name; sk_uri; amount})
    (obj3
       (req "name" string)
       (req "sk_uri" Client_keys.Secret_key.encoding)
       (req "amount" Tez.encoding))

let mockup_default_bootstrap_accounts
    (cctxt : Tezos_client_base.Client_context.full) : string tzresult Lwt.t =
  let rpc_context = new Protocol_client_context.wrap_full cctxt in
  let wallet = (cctxt :> Client_context.wallet) in
  let parsed_account_reprs = ref [] in
  let errors = ref [] in
  Client_keys.list_keys wallet
  >>=? fun all_keys ->
  List.iter_s
    (function
      | (name, pkh, _pk_opt, Some sk_uri) -> (
          let contract =
            Protocol.Alpha_context.Contract.implicit_contract pkh
          in
          Client_proto_context.get_balance
            rpc_context
            ~chain:cctxt#chain
            ~block:cctxt#block
            contract
          >>= fun tz_balance ->
          match tz_balance with
          | Ok balance -> (
              let tez_repr =
                Tez.of_mutez @@ Protocol.Alpha_context.Tez.to_mutez balance
              in
              match tez_repr with
              | None ->
                  (* we're reading the wallet, it's content MUST be valid *)
                  assert false
              | Some amount ->
                  parsed_account_reprs :=
                    {name; sk_uri; amount} :: !parsed_account_reprs ;
                  Lwt.return_unit )
          | Error err ->
              errors := err :: !errors ;
              Lwt.return_unit )
      | _ ->
          Lwt.return_unit)
    all_keys
  >>= fun () ->
  match !errors with
  | [] ->
      let json =
        Data_encoding.Json.construct
          (Data_encoding.list parsed_account_repr_encoding)
          !parsed_account_reprs
      in
      return @@ Data_encoding.Json.to_string json
  | errs ->
      Lwt.return_error @@ List.concat errs

let protocol_constants_no_overrides =
  {
    preserved_cycles = None;
    blocks_per_cycle = None;
    blocks_per_commitment = None;
    blocks_per_roll_snapshot = None;
    blocks_per_voting_period = None;
    time_between_blocks = None;
    endorsers_per_block = None;
    hard_gas_limit_per_operation = None;
    hard_gas_limit_per_block = None;
    proof_of_work_threshold = None;
    tokens_per_roll = None;
    michelson_maximum_type_size = None;
    seed_nonce_revelation_tip = None;
    origination_size = None;
    block_security_deposit = None;
    endorsement_security_deposit = None;
    baking_reward_per_endorsement = None;
    endorsement_reward = None;
    cost_per_byte = None;
    hard_storage_limit_per_operation = None;
    quorum_min = None;
    quorum_max = None;
    min_proposal_quorum = None;
    initial_endorsers = None;
    delay_per_missing_endorsement = None;
    chain_id = None;
    timestamp = None;
  }

(** Existential wrapper to support heterogeneous lists/maps. *)
type protocol_override_field =
  | O : {
      name : string;
      override_value : 'a option;
      pp : Format.formatter -> 'a -> unit;
    }
      -> protocol_override_field

let protocol_override_field_pp ppf (O {name; override_value; pp; _}) =
  match override_value with
  | None ->
      ()
  | Some value ->
      Format.fprintf ppf "@[<h>%s: %a@]" name pp value

let apply_protocol_overrides (cctxt : Tezos_client_base.Client_context.full)
    (o : protocol_constants_overrides) (c : Constants.parametric) :
    Constants.parametric tzresult Lwt.t =
  let open Format in
  let pp_print_int32 ppf i = fprintf ppf "%li" i in
  let pp_print_int64 ppf i = fprintf ppf "%Li" i in
  let fields : protocol_override_field list =
    [ O
        {
          name = "preserved_cycles";
          override_value = o.preserved_cycles;
          pp = pp_print_int;
        };
      O
        {
          name = "blocks_per_cycle";
          override_value = o.blocks_per_cycle;
          pp = pp_print_int32;
        };
      O
        {
          name = "blocks_per_commitment";
          override_value = o.blocks_per_commitment;
          pp = pp_print_int32;
        };
      O
        {
          name = "blocks_per_roll_snapshot";
          override_value = o.blocks_per_roll_snapshot;
          pp = pp_print_int32;
        };
      O
        {
          name = "blocks_per_voting_period";
          override_value = o.blocks_per_voting_period;
          pp = pp_print_int32;
        };
      O
        {
          name = "time_between_blocks";
          override_value = o.time_between_blocks;
          pp = pp_print_list Period.pp;
        };
      O
        {
          name = "endorsers_per_block";
          override_value = o.endorsers_per_block;
          pp = pp_print_int;
        };
      O
        {
          name = "hard_gas_limit_per_operation";
          override_value = o.hard_gas_limit_per_operation;
          pp = Gas.Arith.pp_integral;
        };
      O
        {
          name = "hard_gas_limit_per_block";
          override_value = o.hard_gas_limit_per_block;
          pp = Gas.Arith.pp_integral;
        };
      O
        {
          name = "proof_of_work_threshold";
          override_value = o.proof_of_work_threshold;
          pp = pp_print_int64;
        };
      O
        {
          name = "tokens_per_roll";
          override_value = o.tokens_per_roll;
          pp = Tez.pp;
        };
      O
        {
          name = "michelson_maximum_type_size";
          override_value = o.michelson_maximum_type_size;
          pp = pp_print_int;
        };
      O
        {
          name = "seed_nonce_revelation_tip";
          override_value = o.seed_nonce_revelation_tip;
          pp = Tez.pp;
        };
      O
        {
          name = "origination_size";
          override_value = o.origination_size;
          pp = pp_print_int;
        };
      O
        {
          name = "block_security_deposit";
          override_value = o.block_security_deposit;
          pp = Tez.pp;
        };
      O
        {
          name = "endorsement_security_deposit";
          override_value = o.endorsement_security_deposit;
          pp = Tez.pp;
        };
      O
        {
          name = "baking_reward_per_endorsement";
          override_value = o.baking_reward_per_endorsement;
          pp = pp_print_list Tez.pp;
        };
      O
        {
          name = "endorsement_reward";
          override_value = o.endorsement_reward;
          pp = pp_print_list Tez.pp;
        };
      O {name = "cost_per_byte"; override_value = o.cost_per_byte; pp = Tez.pp};
      O
        {
          name = "hard_storage_limit_per_operation";
          override_value = o.hard_storage_limit_per_operation;
          pp = Z.pp_print;
        };
      O
        {
          name = "quorum_min";
          override_value = o.quorum_min;
          pp = pp_print_int32;
        };
      O
        {
          name = "quorum_max";
          override_value = o.quorum_max;
          pp = pp_print_int32;
        };
      O
        {
          name = "min_proposal_quorum";
          override_value = o.min_proposal_quorum;
          pp = pp_print_int32;
        };
      O
        {
          name = "initial_endorsers";
          override_value = o.initial_endorsers;
          pp = pp_print_int;
        };
      O
        {
          name = "delay_per_missing_endorsement";
          override_value = o.delay_per_missing_endorsement;
          pp = Period.pp;
        };
      O {name = "chain_id"; override_value = o.chain_id; pp = Chain_id.pp};
      O
        {
          name = "timestamp";
          override_value = o.timestamp;
          pp = Time.Protocol.pp_hum;
        } ]
  in
  let fields_with_override =
    fields
    |> List.filter (fun (O {override_value; _}) ->
           Option.is_some override_value)
  in
  ( if fields_with_override <> [] then
    cctxt#message
      "@[<v>mockup client uses protocol overrides:@,%a@]@?"
      (pp_print_list protocol_override_field_pp)
      fields_with_override
  else Lwt.return_unit )
  >>= fun () ->
  return
    ( {
        preserved_cycles =
          Option.value ~default:c.preserved_cycles o.preserved_cycles;
        blocks_per_cycle =
          Option.value ~default:c.blocks_per_cycle o.blocks_per_cycle;
        blocks_per_commitment =
          Option.value ~default:c.blocks_per_commitment o.blocks_per_commitment;
        blocks_per_roll_snapshot =
          Option.value
            ~default:c.blocks_per_roll_snapshot
            o.blocks_per_roll_snapshot;
        blocks_per_voting_period =
          Option.value
            ~default:c.blocks_per_voting_period
            o.blocks_per_voting_period;
        time_between_blocks =
          Option.value ~default:c.time_between_blocks o.time_between_blocks;
        endorsers_per_block =
          Option.value ~default:c.endorsers_per_block o.endorsers_per_block;
        hard_gas_limit_per_operation =
          Option.value
            ~default:c.hard_gas_limit_per_operation
            o.hard_gas_limit_per_operation;
        hard_gas_limit_per_block =
          Option.value
            ~default:c.hard_gas_limit_per_block
            o.hard_gas_limit_per_block;
        proof_of_work_threshold =
          Option.value
            ~default:c.proof_of_work_threshold
            o.proof_of_work_threshold;
        tokens_per_roll =
          Option.value ~default:c.tokens_per_roll o.tokens_per_roll;
        michelson_maximum_type_size =
          Option.value
            ~default:c.michelson_maximum_type_size
            o.michelson_maximum_type_size;
        seed_nonce_revelation_tip =
          Option.value
            ~default:c.seed_nonce_revelation_tip
            o.seed_nonce_revelation_tip;
        origination_size =
          Option.value ~default:c.origination_size o.origination_size;
        block_security_deposit =
          Option.value
            ~default:c.block_security_deposit
            o.block_security_deposit;
        endorsement_security_deposit =
          Option.value
            ~default:c.endorsement_security_deposit
            o.endorsement_security_deposit;
        baking_reward_per_endorsement =
          Option.value
            ~default:c.baking_reward_per_endorsement
            o.baking_reward_per_endorsement;
        endorsement_reward =
          Option.value ~default:c.endorsement_reward o.endorsement_reward;
        cost_per_byte = Option.value ~default:c.cost_per_byte o.cost_per_byte;
        hard_storage_limit_per_operation =
          Option.value
            ~default:c.hard_storage_limit_per_operation
            o.hard_storage_limit_per_operation;
        quorum_min = Option.value ~default:c.quorum_min o.quorum_min;
        quorum_max = Option.value ~default:c.quorum_max o.quorum_max;
        min_proposal_quorum =
          Option.value ~default:c.min_proposal_quorum o.min_proposal_quorum;
        initial_endorsers =
          Option.value ~default:c.initial_endorsers o.initial_endorsers;
        delay_per_missing_endorsement =
          Option.value
            ~default:c.delay_per_missing_endorsement
            o.delay_per_missing_endorsement
          (* Notice that the chain_id and the timestamp are not used here as they are not protocol constants... *);
      }
      : Constants.parametric )

let to_bootstrap_account repr =
  Tezos_client_base.Client_keys.neuterize repr.sk_uri
  >>=? fun pk_uri ->
  Tezos_client_base.Client_keys.public_key pk_uri
  >>=? fun public_key ->
  let public_key_hash = Signature.Public_key.hash public_key in
  return
    Parameters.
      {public_key_hash; public_key = Some public_key; amount = repr.amount}

(* ------------------------------------------------------------------------- *)
(* Blocks *)

type block = {
  hash : Block_hash.t;
  header : Protocol.Alpha_context.Block_header.t;
  operations : Protocol.Alpha_context.Operation.packed list;
  context : Protocol.Environment.Context.t;
}

module Forge = struct
  let default_proof_of_work_nonce =
    Bytes.create Protocol.Alpha_context.Constants.proof_of_work_nonce_size

  let make_shell ~level ~predecessor ~timestamp ~fitness ~operations_hash =
    Tezos_base.Block_header.
      {
        level;
        predecessor;
        timestamp;
        fitness;
        operations_hash;
        proto_level = 0;
        validation_passes = 0;
        context = Context_hash.zero;
      }
end

(* ------------------------------------------------------------------------- *)
(* RPC context *)

let initial_context (header : Block_header.shell_header)
    (params : mockup_protocol_parameters) =
  let bootstrap_accounts =
    (* Required because of 0-balance baker accounts introduced since
           Baking Accounts *)
    List.filter
      (fun (account : Protocol.Alpha_context.Parameters.bootstrap_account) ->
        Protocol.Alpha_context.Tez.(account.amount <> zero))
      params.bootstrap_accounts
  in
  let parameters =
    Default_parameters.parameters_of_constants
      ~bootstrap_accounts
      ~bootstrap_contracts:params.bootstrap_contracts
      ~with_commitments:false
      params.constants
  in
  let json = Default_parameters.json_of_parameters parameters in
  let proto_params =
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  Tezos_protocol_environment.Context.(
    let empty = Memory_context.empty in
    add empty ["version"] (Bytes.of_string "genesis")
    >>= fun ctxt -> add ctxt ["protocol_parameters"] proto_params)
  >>= fun ctxt ->
  Protocol.Main.init ctxt header
  >|= Protocol.Environment.wrap_tzresult
  >>=? fun {context; _} -> return context

let mem_init :
    cctxt:Tezos_client_base.Client_context.full ->
    parameters:mockup_protocol_parameters ->
    constants_overrides_json:Data_encoding.json option ->
    bootstrap_accounts_json:Data_encoding.json option ->
    (Chain_id.t * Tezos_protocol_environment.rpc_context) tzresult Lwt.t =
 fun ~cctxt ~parameters ~constants_overrides_json ~bootstrap_accounts_json ->
  let hash =
    Block_hash.of_b58check_exn
      "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU"
  in
  (* Need to read this Json file before since timestamp modification may be in
     there *)
  ( match constants_overrides_json with
  | None ->
      return protocol_constants_no_overrides
  | Some json -> (
    match
      Data_encoding.Json.destruct protocol_constants_overrides_encoding json
    with
    | x ->
        return x
    | exception error ->
        failwith
          "cannot read protocol constants overrides: %a"
          (Data_encoding.Json.print_error ?print_unknown:None)
          error ) )
  >>=? fun protocol_overrides ->
  let default = parameters.initial_timestamp in
  let timestamp = Option.value ~default protocol_overrides.timestamp in
  ( if not @@ Time.Protocol.equal default timestamp then
    cctxt#message "@[<h>initial_timestamp: %a@]" Time.Protocol.pp_hum timestamp
  else Lwt.return_unit )
  >>= fun () ->
  let shell =
    Forge.make_shell
      ~level:0l
      ~predecessor:hash
      ~timestamp
      ~fitness:(Fitness.from_int64 0L)
      ~operations_hash:Operation_list_list_hash.zero
  in
  apply_protocol_overrides cctxt protocol_overrides parameters.constants
  >>=? fun protocol_custom ->
  ( match bootstrap_accounts_json with
  | None ->
      return None
  | Some json -> (
    match
      Data_encoding.Json.destruct
        (Data_encoding.list parsed_account_repr_encoding)
        json
    with
    | accounts ->
        cctxt#message "@[<h>mockup client uses custom bootstrap accounts:@]"
        >>= fun () ->
        let open Format in
        cctxt#message
          "@[%a@]"
          (pp_print_list
             ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
             parsed_account_repr_pp)
          accounts
        >>= fun () ->
        List.map_es to_bootstrap_account accounts
        >>=? fun bootstrap_accounts -> return (Some bootstrap_accounts)
    | exception error ->
        failwith
          "cannot read definitions of bootstrap accounts: %a"
          (Data_encoding.Json.print_error ?print_unknown:None)
          error ) )
  >>=? fun bootstrap_accounts_custom ->
  initial_context
    shell
    {
      parameters with
      bootstrap_accounts =
        Option.value
          ~default:parameters.bootstrap_accounts
          bootstrap_accounts_custom;
      constants = protocol_custom;
    }
  >>=? fun context ->
  let chain_id =
    Tezos_mockup_registration.Mockup_args.Chain_id.choose
      ~from_config_file:protocol_overrides.chain_id
  in
  return
    ( chain_id,
      {
        Tezos_protocol_environment.block_hash = hash;
        block_header = shell;
        context;
      } )

let migrate :
    Chain_id.t * Tezos_protocol_environment.rpc_context ->
    (Chain_id.t * Tezos_protocol_environment.rpc_context) tzresult Lwt.t =
 fun (chain_id, rpc_context) ->
  let {Tezos_protocol_environment.block_hash; context; block_header} =
    rpc_context
  in
  Protocol.Main.init context block_header
  >|= Protocol.Environment.wrap_tzresult
  >>=? fun {context; _} ->
  let rpc_context =
    {Tezos_protocol_environment.block_hash; block_header; context}
  in
  return (chain_id, rpc_context)

(* ------------------------------------------------------------------------- *)
(* Register mockup *)

let () =
  let open Tezos_mockup_registration.Registration in
  let module M : MOCKUP = struct
    type parameters = mockup_protocol_parameters

    type protocol_constants = protocol_constants_overrides

    let parameters_encoding = mockup_protocol_parameters_encoding

    let protocol_constants_encoding = protocol_constants_overrides_encoding

    let default_bootstrap_accounts = mockup_default_bootstrap_accounts

    let default_parameters = default_mockup_parameters

    let default_protocol_constants = default_mockup_protocol_constants

    let protocol_hash = Protocol.hash

    module Protocol = Protocol_client_context.Lifted_protocol
    module Block_services = Protocol_client_context.Alpha_block_services

    let directory = Tezos_protocol_plugin_alpha.Plugin.RPC.rpc_services

    let init = mem_init

    let migrate = migrate
  end in
  register_mockup_environment (module M)
