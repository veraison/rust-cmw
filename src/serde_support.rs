// Copyright 2025 Contributors to the Veraison project.
// SPDX-License-Identifier: Apache-2.0

//! Serde integration for [CMW](crate::CMW).
//!
//! CMW implements serde for human-readable (JSON) formats only, reusing
//! [CMW::to_json_value] / [CMW::from_json_value]. This matches the `json-cmw`
//! wire shapes in draft-ietf-rats-msg-wrap.
//!
//! CBOR is intentionally **not** supported through serde. The CBOR CMW profile
//! relies on semantic tags (the Tag CMW form, CBOR major type 6), which have no
//! representation in serde's data model — `minicbor_serde` cannot emit a tag and
//! its `deserialize_any` rejects `Type::Tag`. Use [CMW::marshal_cbor] /
//! [CMW::unmarshal_cbor] for CBOR instead. Non-human-readable (de)serializers
//! therefore return an error.

use crate::cmw::CMW;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::Value as JsonValue;

impl Serialize for CMW {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if !serializer.is_human_readable() {
            return Err(serde::ser::Error::custom(
                "CMW serde supports human-readable formats (JSON) only; \
                 use CMW::marshal_cbor for CBOR",
            ));
        }
        self.to_json_value()
            .map_err(serde::ser::Error::custom)?
            .serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for CMW {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if !deserializer.is_human_readable() {
            return Err(serde::de::Error::custom(
                "CMW serde supports human-readable formats (JSON) only; \
                 use CMW::unmarshal_cbor for CBOR",
            ));
        }
        let v = JsonValue::deserialize(deserializer)?;
        Self::from_json_value(&v).map_err(serde::de::Error::custom)
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
    fn serde_cbor_is_rejected() {
        // CBOR must go through marshal_cbor/unmarshal_cbor, not serde.
        let monad = Monad::new_cf(30001, vec![0x01, 0x02, 0x03], None).expect("monad");
        let cmw: CMW = monad.into();
        assert!(minicbor_serde::to_vec(&cmw).is_err());

        let bytes = cmw.marshal_cbor().expect("marshal_cbor");
        assert!(minicbor_serde::from_slice::<CMW>(&bytes).is_err());
    }
}
