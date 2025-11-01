// Copyright 2025 Contributors to the Veraison project.
// SPDX-License-Identifier: Apache-2.0

//! Root CMW type: either Monad or Collection, with sniffing and unified Deserialize/Serialize.

use minicbor::{encode::Write, Decode, Decoder, Encode};

use crate::{
    collection::Collection,
    monad::Monad,
    utils::{
        start_cbor_collection, start_cbor_record, start_cbor_tag, start_json_collection,
        start_json_record,
    },
};
use std::fmt;

use thiserror::Error as ThisError;

#[derive(Debug, ThisError)]
pub enum Error {
    #[error("Invalid data: {0}")]
    InvalidData(String),
    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),
    #[error("CBOR decoding error: {0}")]
    CborDecode(String),
    // Note: this is a String to avoid the type param bubbling up.
    // Unfortunately this also means losing the content of the error from minicbor,
    // because it doesn't enforce `Display` or even `Debug` on the error type.
    // I hate type generics sometimes.
    #[error("CBOR encoding error: {0}")]
    CborEncode(String),
    #[error("TN/CF conversion error: {0}")]
    TnCf(#[from] crate::tn::TnCfError),
    #[error("X.509 error: {0}")]
    X509(#[from] crate::x509_ext::X509Error),
    #[error("Unexpected error: {0}")]
    Unexpected(String),
}

/// Kind enum: Unknown, Monad, Collection.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Monad,
    Collection,
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Kind::Collection => "collection",
            Kind::Monad => "monad",
        };
        write!(f, "{s}")
    }
}

/// CMW: either a Monad variant or a Collection variant.
#[derive(Clone, Debug, PartialEq)]
pub enum CMW {
    Monad(Monad),
    Collection(Collection),
}

impl CMW {
    pub fn kind(&self) -> Kind {
        match self {
            CMW::Monad(_) => Kind::Monad,
            CMW::Collection(_) => Kind::Collection,
        }
    }

    pub fn validate(&self) -> Result<(), Error> {
        match self {
            CMW::Collection(c) => c.validate(),
            CMW::Monad(_) => Ok(()),
        }
    }

    /// Marshal to JSON.
    pub fn marshal_json(&self) -> Result<Vec<u8>, Error> {
        match self {
            CMW::Monad(m) => serde_json::to_vec(&m.to_json_value()?).map_err(Error::Json),
            CMW::Collection(c) => c.marshal_json(),
        }
    }

    /// Unmarshal from JSON.
    pub fn unmarshal_json(b: &[u8]) -> Result<Self, Error> {
        if b.is_empty() {
            return Err(Error::InvalidData("empty buffer".into()));
        }
        let start = b[0];
        if start_json_record(start) {
            Ok(Monad::from_json_bytes(b)?.into())
        } else if start_json_collection(start) {
            Ok(Collection::unmarshal_json(b)?.into())
        } else {
            Err(Error::InvalidData(format!(
                "want JSON object or JSON array start symbols, got: 0x{:02x}",
                start
            )))
        }
    }

    /// Marshal to CBOR.
    pub fn marshal_cbor(&self) -> Result<Vec<u8>, Error> {
        match self {
            CMW::Monad(m) => m.marshal_cbor(),
            CMW::Collection(c) => c.marshal_cbor(),
        }
    }

    /// Unmarshal from CBOR.
    pub fn unmarshal_cbor(b: &[u8]) -> Result<Self, Error> {
        if b.is_empty() {
            return Err(Error::InvalidData("empty buffer".into()));
        }
        let start = b[0];
        if start_cbor_record(start) || start_cbor_tag(start) {
            Ok(Monad::unmarshal_cbor(b)?.into())
        } else if start_cbor_collection(start) {
            Ok(Collection::unmarshal_cbor(b)?.into())
        } else {
            Err(Error::InvalidData(format!(
                "want CBOR map, CBOR array or CBOR Tag start symbols, got: 0x{:02x}",
                start
            )))
        }
    }

    pub(crate) fn unmarshal_from_cbor_decoder(decoder: &mut Decoder<'_>) -> Result<Self, Error> {
        // Decode the first byte to determine the type
        let start = decoder.input()[decoder.position()];
        if start_cbor_record(start) || start_cbor_tag(start) {
            Monad::unmarshal_from_cbor_decoder(decoder).map(|m| m.into())
        } else if start_cbor_collection(start) {
            Collection::unmarshal_from_cbor_decoder(decoder).map(|c| c.into())
        } else {
            Err(Error::InvalidData(format!(
                "want CBOR map, CBOR array or CBOR Tag start symbols, got: 0x{:02x}",
                start
            )))
        }
    }

    /// Deserialize from either JSON or CBOR by sniffing first byte.
    pub fn deserialize(b: &[u8]) -> Result<Self, Error> {
        if b.is_empty() {
            return Err(Error::InvalidData("empty buffer".into()));
        }
        let s = b[0];
        if start_cbor_collection(s) || start_cbor_record(s) || start_cbor_tag(s) {
            CMW::unmarshal_cbor(b)
        } else if start_json_record(s) || start_json_collection(s) {
            CMW::unmarshal_json(b)
        } else {
            Err(Error::InvalidData(format!(
                "unknown start symbol for CMW: 0x{:02x}",
                s
            )))
        }
    }
}

impl<'b> Decode<'b, ()> for CMW {
    fn decode(d: &mut Decoder<'b>, _ctx: &mut ()) -> Result<Self, minicbor::decode::Error> {
        CMW::unmarshal_from_cbor_decoder(d).map_err(|e| {
            minicbor::decode::Error::custom(Error::InvalidData(format!(
                "Failed to decode CMW from bytes: {e}"
            )))
        })
    }
}

impl Encode<()> for CMW {
    fn encode<W: Write>(
        &self,
        e: &mut minicbor::Encoder<W>,
        _ctx: &mut (),
    ) -> Result<(), minicbor::encode::Error<W::Error>> {
        match self {
            CMW::Monad(m) => m.marshal_cbor_to_encoder(e).map_err(|e| {
                minicbor::encode::Error::custom(Error::InvalidData(format!(
                    "Failed to encode Monad to CBOR: {e}"
                )))
            }),
            CMW::Collection(c) => c.marshal_cbor_to_encoder(e).map_err(|e| {
                minicbor::encode::Error::custom(Error::InvalidData(format!(
                    "Failed to encode Collection to CBOR: {e}"
                )))
            }),
        }
    }
}

impl From<Monad> for CMW {
    fn from(value: Monad) -> Self {
        CMW::Monad(value)
    }
}

impl From<Collection> for CMW {
    fn from(value: Collection) -> Self {
        CMW::Collection(value)
    }
}
