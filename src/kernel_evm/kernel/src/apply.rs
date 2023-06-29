// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use evm_execution::account_storage::EthereumAccountStorage;
use evm_execution::handler::ExecutionOutcome;
use evm_execution::precompiles::PrecompileBTreeMap;
use evm_execution::run_transaction;
use primitive_types::{H160, H256, U256};
use tezos_ethereum::block::BlockConstants;
use tezos_ethereum::signatures::EthereumTransactionCommon;
use tezos_ethereum::transaction::TransactionHash;
use tezos_smart_rollup_debug::{debug_msg, Runtime};

use crate::error::{Error, TransferError};
use crate::indexable_storage::IndexableStorage;
use crate::storage::index_account;

/// This defines the needed function to apply a transaction.
pub trait ApplicableTransaction {
    /// Returns the transaction's caller.
    fn caller(&self) -> Result<H160, Error>;

    /// Checks if the transaction's nonce is the expected caller's nonce.
    fn check_nonce<Host: Runtime>(
        &self,
        caller: H160,
        host: &mut Host,
        evm_account_storage: &mut EthereumAccountStorage,
    ) -> bool;

    fn chain_id(&self) -> U256;

    fn to(&self) -> Option<H160>;

    fn data(&self) -> Vec<u8>;

    fn gas_limit(&self) -> u64;

    fn gas_price(&self) -> U256;

    fn value(&self) -> U256;

    fn nonce(&self) -> U256;

    fn v(&self) -> U256;

    fn r(&self) -> H256;

    fn s(&self) -> H256;
}

impl ApplicableTransaction for EthereumTransactionCommon {
    fn caller(&self) -> Result<H160, Error> {
        self.caller()
            .map_err(|_| Error::Transfer(TransferError::InvalidCallerAddress))
    }

    fn check_nonce<Host: Runtime>(
        &self,
        caller: H160,
        host: &mut Host,
        evm_account_storage: &mut EthereumAccountStorage,
    ) -> bool {
        let nonce = |caller| -> Option<U256> {
            let caller_account_path =
                evm_execution::account_storage::account_path(&caller).ok()?;
            let caller_account =
                evm_account_storage.get(host, &caller_account_path).ok()?;
            match caller_account {
                Some(account) => account.nonce(host).ok(),
                None => Some(U256::zero()),
            }
        };
        match nonce(caller) {
            None => false,
            Some(expected_nonce) => self.nonce == expected_nonce,
        }
    }

    fn chain_id(&self) -> U256 {
        self.chain_id
    }

    fn to(&self) -> Option<H160> {
        self.to
    }

    fn data(&self) -> Vec<u8> {
        self.data.clone()
    }

    fn gas_limit(&self) -> u64 {
        self.gas_limit
    }

    fn gas_price(&self) -> U256 {
        self.gas_price
    }

    fn value(&self) -> U256 {
        self.value
    }

    fn nonce(&self) -> U256 {
        self.nonce
    }

    fn v(&self) -> U256 {
        self.v
    }

    fn r(&self) -> H256 {
        self.r
    }

    fn s(&self) -> H256 {
        self.s
    }
}

pub struct TransactionReceiptInfo {
    pub tx_hash: TransactionHash,
    pub index: u32,
    pub execution_outcome: Option<ExecutionOutcome>,
    pub caller: H160,
    pub to: Option<H160>,
}

pub struct TransactionObjectInfo {
    pub from: H160,
    pub gas_used: U256,
    pub gas_price: U256,
    pub hash: TransactionHash,
    pub input: Vec<u8>,
    pub nonce: U256,
    pub to: Option<H160>,
    pub index: u32,
    pub value: U256,
    pub v: U256,
    pub r: H256,
    pub s: H256,
}

#[inline(always)]
fn make_receipt_info(
    tx_hash: TransactionHash,
    index: u32,
    execution_outcome: Option<ExecutionOutcome>,
    caller: H160,
    to: Option<H160>,
) -> TransactionReceiptInfo {
    TransactionReceiptInfo {
        tx_hash,
        index,
        execution_outcome,
        caller,
        to,
    }
}

#[inline(always)]
fn make_object_info(
    transaction: impl ApplicableTransaction,
    transaction_hash: TransactionHash,
    from: H160,
    index: u32,
    gas_used: U256,
) -> TransactionObjectInfo {
    TransactionObjectInfo {
        from,
        gas_used,
        gas_price: transaction.gas_price(),
        hash: transaction_hash,
        input: transaction.data(),
        nonce: transaction.nonce(),
        to: transaction.to(),
        index,
        value: transaction.value(),
        v: transaction.v(),
        r: transaction.r(),
        s: transaction.s(),
    }
}

// From a receipt, indexes the caller, recipient and the new address if needs
// be.
fn index_new_accounts<Host: Runtime>(
    host: &mut Host,
    accounts_index: &mut IndexableStorage,
    receipt: &TransactionReceiptInfo,
) -> Result<(), Error> {
    index_account(host, &receipt.caller, accounts_index)?;
    if let Some(to) = receipt.to {
        index_account(host, &to, accounts_index)?
    };
    match receipt
        .execution_outcome
        .as_ref()
        .and_then(|o| o.new_address)
    {
        Some(to) => index_account(host, &to, accounts_index),
        None => Ok(()),
    }
}

#[allow(clippy::too_many_arguments)]
pub fn apply_transaction<Host: Runtime>(
    host: &mut Host,
    block_constants: &BlockConstants,
    precompiles: &PrecompileBTreeMap<Host>,
    transaction: impl ApplicableTransaction,
    transaction_hash: TransactionHash,
    index: u32,
    evm_account_storage: &mut EthereumAccountStorage,
    accounts_index: &mut IndexableStorage,
) -> Result<Option<(TransactionReceiptInfo, TransactionObjectInfo)>, Error> {
    let caller = match transaction.caller() {
        Ok(caller) => caller,
        Err(err) => {
            debug_msg!(
                host,
                "{} ignored because of {:?}\n",
                hex::encode(transaction_hash),
                err
            );
            // Transaction with undefined caller are ignored, i.e. the caller
            // could not be derived from the signature.
            return Ok(None);
        }
    };
    if !transaction.check_nonce(caller, host, evm_account_storage) {
        // Transactions with invalid nonces are ignored.
        return Ok(None);
    }
    let to = transaction.to();
    let call_data = transaction.data();
    let gas_limit = transaction.gas_limit();
    let value = transaction.value();
    let execution_outcome = match run_transaction(
        host,
        block_constants,
        evm_account_storage,
        precompiles,
        to,
        caller,
        call_data,
        Some(gas_limit),
        Some(value),
    ) {
        Ok(outcome) => Some(outcome),
        Err(err) => {
            // TODO: https://gitlab.com/tezos/tezos/-/issues/5665
            // Because the proposal's state is unclear, and we do not have a sequencer
            // if an error that leads to a durable storage corruption is caught, we
            // invalidate the entire proposal.
            return Err(Error::InvalidRunTransaction(err));
        }
    };

    let gas_used = match &execution_outcome {
        Some(execution_outcome) => execution_outcome.gas_used.into(),
        None => U256::zero(),
    };

    let receipt_info =
        make_receipt_info(transaction_hash, index, execution_outcome, caller, to);
    let object_info =
        make_object_info(transaction, transaction_hash, caller, index, gas_used);
    index_new_accounts(host, accounts_index, &receipt_info)?;

    Ok(Some((receipt_info, object_info)))
}
