// SPDX-FileCopyrightText: 2022 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

//! Basic Ethereum types for computation
//!
//! Many of the functions in this module (all the `one` and `zero`) can be made
//! constant, but the underlying library and functions we use are not constant.
//! TODO: <https://gitlab.com/tezos/tezos/-/milestones/114>
use primitive_types::{H256 as PTH256, U256};
use rlp::{Decodable, DecoderError, Encodable, Rlp};
use sha3::{Digest, Keccak256};
use tezos_crypto_rs::hash::BlockHash;

/// The size of one 256 bit word. Size in bytes
pub const WORD_SIZE: usize = 32_usize;

/// Gas price newtype to wrap U256
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct GasPrice {
    /// tezos_encoding doesn't support deriving reader and writer from newtypes so therefore this
    /// public field instead.
    pub value: U256,
}

impl GasPrice {
    /// Create a new gas price from serilizable u256
    pub fn new(value: U256) -> Self {
        Self { value }
    }

    /// Create a new gas price from primitive type
    pub fn from_u256(value: U256) -> Self {
        Self { value }
    }

    /// Zero
    pub fn zero() -> Self {
        Self {
            value: U256::zero(),
        }
    }

    /// One
    pub fn one() -> Self {
        Self { value: U256::one() }
    }
}

impl Decodable for GasPrice {
    fn decode(decoder: &Rlp<'_>) -> Result<GasPrice, DecoderError> {
        Ok(Self {
            value: U256::decode(decoder)?,
        })
    }
}
impl Encodable for GasPrice {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.append(&self.value);
    }
}

/// Gas limit newtype to wrap U256
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct GasLimit {
    /// tezos_encoding doesn't support deriving reader and writer from newtypes so therefore this
    /// public field instead.
    pub value: U256,
}

impl GasLimit {
    /// Translate to unsigned 64 bit (should be adequate for all calls at the moment)
    /// Error can only be overflow.
    pub fn to_u64(&self) -> Option<u64> {
        // Unfortunately, the `primitive_types` library doesn't implement u64 -> U256
        // conversion in `const`.
        let max_u64: U256 = U256::from(core::u64::MAX);

        if self.value <= max_u64 {
            Some(self.value.low_u64())
        } else {
            None
        }
    }

    /// Create a new gas limit from serilizable u256
    pub fn new(value: U256) -> Self {
        Self { value }
    }

    /// Create a new gas limit from primitive type
    pub fn from_u256(value: U256) -> Self {
        Self { value }
    }

    /// Zero
    pub fn zero() -> Self {
        Self {
            value: U256::zero(),
        }
    }

    /// One
    pub fn one() -> Self {
        Self { value: U256::one() }
    }
}

impl Decodable for GasLimit {
    fn decode(decoder: &Rlp<'_>) -> Result<GasLimit, DecoderError> {
        Ok(Self {
            value: U256::decode(decoder)?,
        })
    }
}
impl Encodable for GasLimit {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.append(&self.value);
    }
}

/// Amount or value in Wei. Newtype wrapper for U256
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Wei {
    /// tezos_encoding doesn't support deriving reader and writer from newtypes so therefore this
    /// public field instead.
    pub value: U256,
}

impl Wei {
    /// Create a new value in Wei from serlizable type
    pub fn new(value: U256) -> Self {
        Self { value }
    }

    /// Create a new value in Wei from primitive type
    pub fn from_u256(value: U256) -> Self {
        Self { value }
    }

    /// Zero
    pub fn zero() -> Self {
        Self {
            value: U256::zero(),
        }
    }

    /// One
    pub fn one() -> Self {
        Self { value: U256::one() }
    }
}

impl Decodable for Wei {
    fn decode(decoder: &Rlp<'_>) -> Result<Wei, DecoderError> {
        Ok(Self {
            value: U256::decode(decoder)?,
        })
    }
}
impl Encodable for Wei {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.append(&self.value);
    }
}

/// 256 bit hash (Keccak)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd)]
pub struct H256(PTH256);

impl H256 {
    /// Value zero
    pub fn zero() -> H256 {
        Self(PTH256::zero())
    }

    /// Decode a H256 value from an hex string, unsafe
    pub fn from_string_unsafe(s: &str) -> Self {
        let mut v: [u8; 32] = [0; 32];
        hex::decode_to_slice(s, &mut v).expect("Could not parse to 256 hex value ");
        H256::from(v)
    }
}

impl From<&[u8]> for H256 {
    fn from(v: &[u8]) -> Self {
        H256(PTH256::from_slice(v))
    }
}

impl From<[u8; WORD_SIZE]> for H256 {
    fn from(v: [u8; WORD_SIZE]) -> Self {
        H256(PTH256::from(v))
    }
}

impl From<PTH256> for H256 {
    fn from(v: PTH256) -> Self {
        Self(v)
    }
}

#[allow(clippy::from_over_into)]
impl Into<PTH256> for H256 {
    fn into(self) -> PTH256 {
        self.0
    }
}

/// Rehashes Tezos block hash to Keccak256 (which is Ehtereum one)
impl From<BlockHash> for H256 {
    fn from(v: BlockHash) -> Self {
        Self(PTH256::from_slice(Keccak256::digest(v.0).as_slice()))
    }
}
impl From<H256> for String {
    fn from(e: H256) -> Self {
        format!("{:x}", e.0)
    }
}

impl Decodable for H256 {
    fn decode(decoder: &Rlp<'_>) -> Result<H256, DecoderError> {
        let length = decoder.data()?.len();
        if length == 32 {
            Ok(H256::from(decoder.data()?))
        } else if length < 32 && length > 0 {
            // there were missing 0 that encoding deleted
            let missing = 32 - length;
            let mut full = [0u8; 32];
            full[missing..].copy_from_slice(decoder.data()?);
            Ok(H256::from(full))
        } else if decoder.data()?.is_empty() {
            // considering the case empty allows to decode unsigned transactions
            Ok(H256::zero())
        } else {
            Err(DecoderError::RlpInvalidLength)
        }
    }
}

impl Encodable for H256 {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        if &H256::zero() != self {
            s.append(&self.0);
        } else {
            // we could make the distinction between 0 and null
            // but we don't, null is encoded as 0
            // which is not such a big deal as H256 is used for hashed values
            s.append_empty_data();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    /// used in test to decode a string and get the size of the decoded input,
    /// before determining the H256 value
    fn decode(str: &str) -> (Result<H256, DecoderError>, usize) {
        let hash = hex::decode(str).unwrap();
        let decoder = Rlp::new(&hash);
        let decoded = H256::decode(&decoder);
        assert!(decoded.is_ok(), "hash should be decoded ok");
        let length = decoder.data().unwrap().len();
        (decoded, length)
    }

    #[test]
    fn test_decode_h256_l0() {
        // rlp encoding of empty is the byte 80
        let (decoded, length) = decode("80");
        assert_eq!(0, length);
        assert_eq!(
            H256::zero(),
            decoded.unwrap(),
            "empty hash should be decoded as 0x0...0"
        );
    }

    #[test]
    fn test_decode_h256_l32() {
        // rlp encoding of hex string of 32 bytes
        let (decoded, length) =
            decode("a03232323232323232323232323232323232323232323232323232323232323232");
        assert_eq!(32, length);
        assert_eq!(
            "3232323232323232323232323232323232323232323232323232323232323232",
            String::from(decoded.unwrap()),
            "32 hash should be decoded as 0x32...32"
        );
    }

    #[test]
    fn test_decode_h256_l31() {
        // rlp encoding of hex string of 31 bytes
        let (decoded, length) =
            decode("9f31313131313131313131313131313131313131313131313131313131313131");
        assert_eq!(31, length);
        assert_eq!(
            "0031313131313131313131313131313131313131313131313131313131313131",
            String::from(decoded.unwrap()),
            "31 hash should be decoded as 0x0031..31"
        );
    }
}
