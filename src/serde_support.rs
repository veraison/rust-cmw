// Copyright 2025 Contributors to the Veraison project.
// SPDX-License-Identifier: Apache-2.0

//! Serde integration for [CMW](crate::CMW).
//!
//! CMW implements serde for both human-readable (JSON) and binary (CBOR)
//! formats, reusing the crate's own codec so callers can embed a [CMW] in any
//! `serde`-serialized structure without handling the two encodings themselves.
//!
//! * Human-readable serializers (e.g. JSON) go through
//!   [CMW::to_json_value] / [CMW::from_json_value], matching the `json-cmw`
//!   wire shapes in draft-ietf-rats-msg-wrap.
//! * Non-human-readable serializers (CBOR) go through
//!   [CMW::marshal_cbor] / [CMW::unmarshal_cbor]. Because the CBOR CMW profile
//!   relies on semantic tags (CBOR major type 6), which serde's data model
//!   cannot represent directly, the native CBOR bytes are bridged into the
//!   serde stream via [`ciborium::value::Value`], which faithfully carries
//!   tags. This requires the surrounding CBOR (de)serializer to understand
//!   ciborium's tag convention (as ciborium itself does).

use crate::cmw::CMW;
use serde::{de::Error as _, ser::Error as _, Deserialize, Deserializer, Serialize, Serializer};
use serde_json::Value as JsonValue;

impl Serialize for CMW {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if serializer.is_human_readable() {
            self.to_json_value()
                .map_err(S::Error::custom)?
                .serialize(serializer)
        } else {
            let bytes = self.marshal_cbor().map_err(S::Error::custom)?;
            let value: ciborium::value::Value =
                ciborium::de::from_reader(bytes.as_slice()).map_err(S::Error::custom)?;
            value.serialize(serializer)
        }
    }
}

impl<'de> Deserialize<'de> for CMW {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if deserializer.is_human_readable() {
            let v = JsonValue::deserialize(deserializer)?;
            Self::from_json_value(&v).map_err(D::Error::custom)
        } else {
            let value = ciborium::value::Value::deserialize(deserializer)?;
            let mut bytes: Vec<u8> = Vec::new();
            ciborium::ser::into_writer(&value, &mut bytes).map_err(D::Error::custom)?;
            Self::unmarshal_cbor(&bytes).map_err(D::Error::custom)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{indicator::Indicator, monad::Monad, Mime, CMW};
    use std::{fs, str::FromStr};

    #[test]
    fn serde_json_collection_roundtrip() {
        let data = fs::read_to_string("testdata/collection-ok.json")
            .expect("read testdata/collection-ok.json");
        let parsed: CMW = serde_json::from_str(&data).expect("deserialize JSON");
        let encoded = serde_json::to_string(&parsed).expect("serialize JSON");
        let reparsed: CMW = serde_json::from_str(&encoded).expect("re-deserialize JSON");
        assert_eq!(parsed, reparsed);
    }

    #[test]
    fn serde_json_monad_roundtrip() {
        let monad = Monad::new_media_type(
            Mime::from_str("application/vnd.a").expect("mime"),
            vec![0x61],
            Some(Indicator::from_bits_truncate(4)),
        )
        .expect("monad");
        let cmw: CMW = monad.into();
        let json = serde_json::to_string(&cmw).expect("serialize");
        let decoded: CMW = serde_json::from_str(&json).expect("deserialize");
        assert_eq!(cmw, decoded);
    }

    #[test]
    fn serde_cbor_monad_roundtrip() {
        // CBOR now round-trips through serde, bridged via ciborium.
        let monad = Monad::new_cf(30001, vec![0x01, 0x02, 0x03], None).expect("monad");
        let cmw: CMW = monad.into();

        let mut buf: Vec<u8> = Vec::new();
        ciborium::ser::into_writer(&cmw, &mut buf).expect("serialize CBOR");
        let decoded: CMW = ciborium::de::from_reader(buf.as_slice()).expect("deserialize CBOR");
        assert_eq!(cmw, decoded);

        // The serde output must match the crate's native CBOR encoding.
        assert_eq!(buf, cmw.marshal_cbor().expect("marshal_cbor"));
    }

    #[test]
    fn serde_cbor_matches_json_source() {
        // A CMW parsed from JSON (internal format = JSON) must still be
        // CBOR-encodable through serde.
        let cmw = CMW::from_json_value(&serde_json::json!(["application/vnd.example", "3q2-7w"]))
            .expect("from_json_value");

        let mut buf: Vec<u8> = Vec::new();
        ciborium::ser::into_writer(&cmw, &mut buf).expect("serialize CBOR");
        let decoded: CMW = ciborium::de::from_reader(buf.as_slice()).expect("deserialize CBOR");
        assert_eq!(cmw, decoded);
    }
}
