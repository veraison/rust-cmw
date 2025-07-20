// Copyright 2025 Contributors to the Veraison project.
// SPDX-License-Identifier: Apache-2.0

//! X.509 Extension encoding/decoding of wrapped CMW (OID 1.3.6.1.5.5.7.1.35).

use crate::cmw::{Error, CMW};
use lazy_static::lazy_static;
use simple_asn1::{from_der, oid, to_der, ASN1Block, OID};
use thiserror::Error as ThisError;

lazy_static! {
    pub static ref OID_EXT_CMW: OID = oid!(1, 3, 6, 1, 5, 5, 7, 1, 35);
}

#[derive(Debug, ThisError)]
pub enum X509Error {
    #[error("ASN.1 encoding failed: {0}")]
    Asn1Encode(String),

    #[error("ASN.1 decoding failed: {0}")]
    Asn1Decode(String),

    #[error("Invalid extension OID: expected id-pe-cmw, got {0}")]
    InvalidOid(String),
}

/// Representation of a minimal X.509 Extension.
#[derive(Clone, Debug, PartialEq)]
pub struct X509Extension {
    pub oid: OID,
    pub critical: bool,
    pub value: Vec<u8>,
}

impl X509Extension {
    pub fn new(oid: OID, critical: bool, value: Vec<u8>) -> Self {
        X509Extension {
            oid,
            critical,
            value,
        }
    }

    pub fn marshal_cbor(cmw: &CMW, critical: bool) -> Result<Self, Error> {
        let serialized_cmw = cmw.marshal_cbor()?;

        let wrapped = ASN1Block::OctetString(0, serialized_cmw.clone());

        // Build the sequence: SEQUENCE { OID, wrapped }
        let seq = ASN1Block::Sequence(
            0,
            vec![
                ASN1Block::ObjectIdentifier(0, OID_EXT_CMW.clone()),
                ASN1Block::Boolean(0, critical),
                wrapped,
            ],
        );
        let der = to_der(&seq)
            .map_err(|e| X509Error::Asn1Encode(format!("Failed to generate DER: {}", e)))?;
        Ok(X509Extension {
            oid: OID_EXT_CMW.clone(),
            critical,
            value: der,
        })
    }

    pub fn marshal_json(cmw: &CMW, critical: bool) -> Result<Self, Error> {
        let serialized_cmw = cmw.marshal_json()?;

        let s = String::from_utf8(serialized_cmw.clone())
            .map_err(|e| Error::Unexpected(format!("Failed to parse JSON CMW as UTF-8: {e}")))?;
        let wrapped = ASN1Block::UTF8String(0, s);

        // Build the sequence: SEQUENCE { OID, wrapped }
        let seq = ASN1Block::Sequence(
            0,
            vec![
                ASN1Block::ObjectIdentifier(0, OID_EXT_CMW.clone()),
                ASN1Block::Boolean(0, critical),
                wrapped,
            ],
        );
        let der = to_der(&seq)
            .map_err(|e| X509Error::Asn1Encode(format!("Failed to generate DER: {}", e)))?;
        Ok(X509Extension {
            oid: OID_EXT_CMW.clone(),
            critical,
            value: der,
        })
    }

    /// Decode CMW from a provided X.509 Extension.
    pub fn decode_cmw(ext: &X509Extension) -> Result<CMW, Error> {
        if ext.oid != *OID_EXT_CMW {
            return Err(X509Error::InvalidOid(format!("{:?}", ext.oid)).into());
        }
        let blocks = from_der(&ext.value)
            .map_err(|e| X509Error::Asn1Decode(format!("Failed to generate DER: {}", e)))?;
        if blocks.is_empty() {
            return Err(X509Error::Asn1Decode("Extension is empty".to_string()).into());
        }
        // Expect SEQUENCE of length 3
        if let ASN1Block::Sequence(_, ref items) = blocks[0] {
            if items.len() != 3 {
                return Err(X509Error::Asn1Decode(format!(
                    "Sequence has wrong length: {}",
                    items.len()
                ))
                .into());
            }
            // items[2] is either OCTET STRING or UTF8String
            match &items[2] {
                ASN1Block::OctetString(_, bytes) => Ok(CMW::unmarshal_cbor(bytes)?),
                ASN1Block::UTF8String(_, s) => Ok(CMW::unmarshal_json(s.as_bytes())?),
                _ => Err(X509Error::Asn1Decode(format!("Wrong item type: {:?}", items[2])).into()),
            }
        } else {
            Err(X509Error::Asn1Decode(format!("Wrong element type: {:?}", blocks[0])).into())
        }
    }
}
