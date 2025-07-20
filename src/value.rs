// Copyright 2025 Contributors to the Veraison project.
// SPDX-License-Identifier: Apache-2.0

//! Value wrapper around a byte vector, with base64‐URL‐safe JSON <-> byte[] mapping.

use base64::Engine;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_cbor::{de::from_slice as cbor_from_slice, ser::to_vec as cbor_to_vec};

use crate::cmw::Error;

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

// JSON: base64 URL‐safe encoding of the raw bytes.
impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let b64 = base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(&self.0);
        serializer.serialize_str(&b64)
    }
}

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D) -> Result<Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let decoded = base64::engine::general_purpose::URL_SAFE_NO_PAD
            .decode(&s)
            .map_err(serde::de::Error::custom)?;
        if decoded.is_empty() {
            Err(serde::de::Error::custom("empty value"))
        } else {
            Ok(Value(decoded))
        }
    }
}

impl Value {
    /// Helper to produce CBOR‐serialized value (raw bytes).
    pub fn to_cbor_bytes(&self) -> Result<Vec<u8>, Error> {
        cbor_to_vec(&serde_cbor::Value::Bytes(self.0.clone()))
            .map_err(|e| Error::CborEncode(format!("Failed to encode Value to CBOR: {e}")))
    }

    /// Helper to decode CBOR raw bytes into Value.
    pub fn from_cbor_bytes(b: &[u8]) -> Result<Self, Error> {
        match cbor_from_slice::<serde_cbor::Value>(b) {
            Ok(serde_cbor::Value::Bytes(v)) if !v.is_empty() => Ok(Value(v)),
            Err(e) => Err(Error::CborDecode(format!(
                "Failed to decode Value from CBOR: {e}"
            ))),
            _ => Err(Error::CborDecode("got wrong CBOR type".into())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Value;
    use serde_json;

    #[test]
    fn test_value_json_roundtrip() {
        let raw = vec![0xde, 0xad, 0xbe, 0xef];
        let v = Value::new(raw.clone()).unwrap();
        let s = serde_json::to_string(&v).unwrap();
        // Base64 of deadbeef is '3q2-7w'
        assert!(s.contains("3q2-7w"));
        let d: Value = serde_json::from_str(&s).unwrap();
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
