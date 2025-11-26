// Copyright 2025 Contributors to the Veraison project.
// SPDX-License-Identifier: Apache-2.0

//! Value wrapper around a byte vector, with base64‐URL‐safe JSON <-> byte[] mapping.

use crate::cmw::Error;
use base64::Engine;
use minicbor_serde::{from_slice as cbor_from_slice, to_vec as cbor_to_vec};
use serde_json::Value as JsonValue;

#[derive(Clone, Debug, PartialEq)]
pub struct Value(pub(crate) Vec<u8>);

impl Value {
    pub fn new(bytes: Vec<u8>) -> Result<Self, Error> {
        if bytes.is_empty() {
            Err(Error::InvalidData("empty value".into()))
        } else {
            Ok(Value(bytes))
        }
    }

    pub fn is_set(&self) -> bool {
        !self.0.is_empty()
    }
}

impl Value {
    /// Helper to produce CBOR‐serialized value (raw bytes).
    pub fn to_cbor_bytes(&self) -> Result<Vec<u8>, Error> {
        cbor_to_vec(self.0.clone())
            .map_err(|e| Error::CborEncode(format!("Failed to encode Value to CBOR: {e}")))
    }

    /// Helper to decode CBOR raw bytes into Value.
    pub fn from_cbor_bytes(b: &[u8]) -> Result<Self, Error> {
        match cbor_from_slice::<Vec<u8>>(b) {
            Ok(v) if !v.is_empty() => Ok(Value(v)),
            Err(e) => Err(Error::CborDecode(format!(
                "Failed to decode Value from CBOR: {e}"
            ))),
            _ => Err(Error::CborDecode("got wrong CBOR type".into())),
        }
    }

    /// Helper to produce JSON‐serialized value (base64 URL‐safe string).
    pub fn to_json_value(&self) -> Result<JsonValue, Error> {
        let b64 = base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(&self.0);
        Ok(JsonValue::String(b64))
    }

    /// Helper to decode JSON base64 URL‐safe string into Value.
    pub fn from_json_value(v: &JsonValue) -> Result<Self, Error> {
        if let JsonValue::String(s) = v {
            match base64::engine::general_purpose::URL_SAFE_NO_PAD.decode(s) {
                Ok(bytes) if !bytes.is_empty() => Ok(Value(bytes)),
                Err(e) => Err(Error::InvalidData(format!(
                    "Failed to decode Value from JSON: {e}"
                ))),
                _ => Err(Error::InvalidData("got empty byte array".into())),
            }
        } else {
            Err(Error::InvalidData("got wrong JSON type".into()))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Value;

    #[test]
    fn test_value_json_roundtrip() {
        let raw = vec![0xde, 0xad, 0xbe, 0xef];
        let v = Value::new(raw.clone()).unwrap();
        let json_value = v.to_json_value().unwrap();
        assert_eq!(json_value, serde_json::Value::String("3q2-7w".into()));
        let d = Value::from_json_value(&json_value).unwrap();
        assert_eq!(d, Value(raw));
    }

    #[test]
    fn test_value_cbor_roundtrip() {
        let raw = vec![0xde, 0xad, 0xbe, 0xef];
        let v = Value::new(raw.clone()).unwrap();
        let cbor_bytes = v.to_cbor_bytes().unwrap();
        let decoded = Value::from_cbor_bytes(&cbor_bytes).unwrap();
        assert_eq!(decoded, Value(raw));
    }
}
