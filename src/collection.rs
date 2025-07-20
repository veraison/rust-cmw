// Copyright 2025 Contributors to the Veraison project.
// SPDX-License-Identifier: Apache-2.0

//! Collection CMW: map of keys → nested CMW, with optional "__cmwc_t" type.

use crate::{
    cmw::{Error, Kind},
    CMW,
};
use iri_string::types::UriStr;
use minicbor::{data::Type as CborType, encode::Write, Decoder, Encode, Encoder};
use regex::Regex;
use serde_json::{Map as JsonMap, Value as JsonValue};
use std::{
    collections::BTreeMap,
    fmt::{self, Display},
    str::FromStr,
};

/// Format enum: JSON/CBOR record, collection, or CBOR Tag.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Format {
    Json,
    Cbor,
}

impl fmt::Display for Format {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Format::Json => "JSON collection",
            Format::Cbor => "CBOR collection",
        };
        write!(f, "{s}")
    }
}

/// A collection key: either a String or u64.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Label {
    Str(String),
    Uint(u64),
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Label::Str(s) => write!(f, "{s}"),
            Label::Uint(u) => write!(f, "{u}"),
        }
    }
}

/// Meta entry (key + kind).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Meta {
    pub key: Label,
    pub kind: Kind,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Type(String);

impl Type {
    pub fn new<S: Into<String>>(s: S) -> Result<Self, Error> {
        let s = s.into();
        Self::validate(&s)?;
        Ok(Type(s))
    }

    /// Validate collection type: must be a URI or an absolute OID.
    fn validate(s: &str) -> Result<(), Error> {
        let oid_re = Regex::new(r"^([0-2])(([.]0)|([.][1-9][0-9]*))*$").unwrap();

        if oid_re.is_match(s) {
            return Ok(());
        }
        if UriStr::new(s).is_ok() {
            return Ok(());
        }
        Err(Error::InvalidData(format!(
            "invalid collection type: {:?}. MUST be URI or OID",
            s
        )))
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for Type {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::new(s)
    }
}

/// The Collection structure.
#[derive(Clone, Debug, PartialEq)]
pub struct Collection {
    cmap: BTreeMap<Label, CMW>,
    ctyp: Option<Type>,
    pub(crate) format: Option<Format>,
}

impl Collection {
    /// Create new collection with optional type.
    pub fn new(ctyp: Option<Type>, format: Option<Format>) -> Result<Self, Error> {
        Ok(Collection {
            cmap: BTreeMap::new(),
            ctyp,
            format,
        })
    }

    pub fn get_type(&self) -> Option<&Type> {
        self.ctyp.as_ref()
    }

    pub fn add_item(&mut self, key: Label, node: CMW) -> Result<(), Error> {
        // Validate key (string must not be "__cmwc_t" or empty/whitespace).
        match &key {
            Label::Str(s) => {
                if s.trim().is_empty() {
                    return Err(Error::InvalidData(
                        "bad collection key: empty or whitespace only".into(),
                    ));
                }
                if s == "__cmwc_t" {
                    return Err(Error::InvalidData(
                        "bad collection key: __cmwc_t is reserved".into(),
                    ));
                }
            }
            Label::Uint(_) => {}
        }
        self.cmap.insert(key, node);
        Ok(())
    }

    pub fn get_item(&self, key: &Label) -> Option<&CMW> {
        self.cmap.get(key)
    }

    pub fn validate(&self) -> Result<(), Error> {
        if self.cmap.is_empty() {
            return Err(Error::InvalidData("empty CMW collection".into()));
        }
        for (k, v) in &self.cmap {
            if let Err(e) = v.validate() {
                return Err(Error::InvalidData(format!(
                    "invalid collection at key {:?}: {}",
                    k, e
                )));
            }
        }
        Ok(())
    }

    pub fn get_meta(&self) -> Vec<Meta> {
        let mut metas: Vec<Meta> = self
            .cmap
            .iter()
            .map(|(k, v)| Meta {
                key: k.clone(),
                kind: v.kind(),
            })
            .collect();
        metas.sort_by(|a, b| {
            let ka = match &a.key {
                Label::Str(s) => s.clone(),
                Label::Uint(u) => format!("##{}", u),
            };
            let kb = match &b.key {
                Label::Str(s) => s.clone(),
                Label::Uint(u) => format!("##{}", u),
            };
            ka.cmp(&kb)
        });
        metas
    }

    /// Serialize as JSON: object with string keys.
    pub fn marshal_json(&self) -> Result<Vec<u8>, Error> {
        let mut map: JsonMap<String, JsonValue> = JsonMap::new();
        if let Some(ref c) = self.ctyp {
            map.insert("__cmwc_t".into(), JsonValue::String(c.to_string()));
        }
        for (k, v) in &self.cmap {
            if let Label::Str(kstr) = k {
                let vb = v.marshal_json()?;
                let val = JsonValue::String(String::from_utf8(vb).map_err(|e| {
                    Error::Unexpected(format!("converting nested CMW bytes to string: {}", e))
                })?);
                map.insert(kstr.clone(), val);
            } else {
                return Err(Error::InvalidData(
                    "JSON collection, key error: want string".into(),
                ));
            }
        }
        serde_json::to_vec(&map).map_err(Error::Json)
    }

    /// Deserialize from JSON bytes.
    pub fn unmarshal_json(b: &[u8]) -> Result<Self, Error> {
        let raw: JsonValue = serde_json::from_slice(b).map_err(Error::Json)?;
        let obj = raw
            .as_object()
            .ok_or_else(|| Error::InvalidData("want JSON object start".to_string()))?;
        let mut col = Collection::new(None, None)?;
        for (key, val) in obj {
            if key == "__cmwc_t" {
                if let JsonValue::String(s) = val {
                    col.ctyp = Some(Type::from_str(s)?);
                    continue;
                } else {
                    return Err(Error::InvalidData("invalid JSON collection type".into()));
                }
            }
            if let JsonValue::String(s) = val {
                let cmw = CMW::unmarshal_json(s.as_bytes())?;
                col.cmap.insert(Label::Str(key.clone()), cmw);
                continue;
            }
        }
        col.format = Some(Format::Json);
        Ok(col)
    }

    /// Serialize as CBOR: map with string/u64 keys.
    pub fn marshal_cbor(&self) -> Result<Vec<u8>, Error> {
        let buf = Vec::new();
        let mut encoder = Encoder::new(buf);
        self.marshal_cbor_to_encoder(&mut encoder)?;
        Ok(encoder.into_writer())
    }

    pub(crate) fn marshal_cbor_to_encoder<W: Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> Result<(), Error> {
        let meta = self.get_meta();
        let _ = encoder.map(meta.len() as u64).or(Err(Error::CborEncode(
            "starting CBOR collection map".to_string(),
        )))?;
        if let Some(ref c) = self.ctyp {
            encoder.str("__cmwc_t").or(Err(Error::CborEncode(
                "serializing CBOR collection type".to_string(),
            )))?;
            encoder.str(&c.to_string()).or(Err(Error::CborEncode(
                "serializing CBOR collection type".to_string(),
            )))?;
        }
        for (k, v) in &self.cmap {
            match k {
                Label::Str(s) => encoder.str(s).or(Err(Error::CborEncode(
                    "serializing a collection key (string)".to_string(),
                )))?,
                Label::Uint(u) => encoder.u64(*u).or(Err(Error::CborEncode(
                    "serializing a collection key (int)".to_string(),
                )))?,
            };
            v.encode(encoder, &mut ())
                .or(Err(Error::CborEncode("serializing nested CMW".to_string())))?;
        }
        Ok(())
    }

    /// Deserialize from CBOR bytes.
    pub fn unmarshal_cbor(b: &[u8]) -> Result<Self, Error> {
        let mut decoder = Decoder::new(b);
        Collection::unmarshal_from_decoder(&mut decoder)
    }

    pub(crate) fn unmarshal_from_decoder(decoder: &mut Decoder<'_>) -> Result<Self, Error> {
        let mut col = Collection::new(None, None)?;
        let _ = decoder
            .map()
            .map_err(|e| Error::CborDecode(format!("decoding collection map: {e}")))?;
        loop {
            let key_type: CborType = match decoder.datatype() {
                Ok(t) => t,
                Err(e) if e.is_end_of_input() => break,
                Err(e) => {
                    return Err(Error::InvalidData(format!(
                        "decoding collection key type: {e}"
                    )))
                }
            };
            match key_type {
                CborType::String => {
                    let ks = decoder
                        .str()
                        .map_err(|e| Error::CborDecode(format!("decoding CBOR string key: {e}")))?;

                    if ks == "__cmwc_t" {
                        let collection_type = decoder.str().map_err(|e| {
                            Error::CborDecode(format!("decoding CBOR collection type: {e}"))
                        })?;
                        col.ctyp = Some(Type::from_str(collection_type)?);
                        continue;
                    }

                    let nested = decoder
                        .decode()
                        .map_err(|e| Error::CborDecode(format!("decoding CBOR nested CMW: {e}")))?;
                    col.cmap.insert(Label::Str(ks.to_string()), nested);
                }
                CborType::Int
                | CborType::I16
                | CborType::I32
                | CborType::I64
                | CborType::I8
                | CborType::U16
                | CborType::U32
                | CborType::U64
                | CborType::U8 => {
                    let ui = decoder
                        .u64()
                        .map_err(|e| Error::CborDecode(format!("decoding CBOR uint key: {e}")))?;
                    let nested = decoder
                        .decode()
                        .map_err(|e| Error::CborDecode(format!("decoding CBOR nested CMW: {e}")))?;
                    col.cmap.insert(Label::Uint(ui), nested);
                }
                _ => {
                    return Err(Error::InvalidData(format!(
                        "unknown collection key type: want string or uint: {:?}",
                        key_type
                    )))
                }
            }
        }
        col.format = Some(Format::Cbor);
        Ok(col)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{cmw::CMW, monad::Monad};
    use std::str::FromStr;

    // Helper: create a dummy CMW instance wrapping a Monad.
    fn dummy_cmw() -> CMW {
        // Using new_cf as in your tests, with dummy data.
        Monad::new_cf(30001, vec![0x01, 0x02, 0x03], None)
            .expect("Failed to create dummy Monad")
            .into()
    }

    #[test]
    fn test_validate_collection_type() {
        Type::validate("tag:example.com,2024:composite-attester").unwrap();
        assert!(Type::validate("urn:ietf:rfc:rfc9999").is_ok());
        assert!(Type::validate("http://www.ietf.org/rfc/rfc2396.txt").is_ok());
        assert!(Type::validate("1.2.3.4").is_ok());
        assert!(Type::validate("").is_err());
        assert!(Type::validate("a/b/c").is_err());
        assert!(Type::validate(".2.3.4").is_err());
    }

    #[test]
    fn test_new_collection_no_type() {
        let collection = Collection::new(None, Some(Format::Json))
            .expect("Failed to create collection without type");
        assert!(collection.get_type().is_none());
    }

    #[test]
    fn test_new_collection_with_valid_type() {
        let type_str = "tag:example.com,2024:composite-attester";
        let type_ = Type::from_str(type_str).expect("Failed to parse type");
        let collection = Collection::new(Some(type_.clone()), Some(Format::Cbor))
            .expect("Failed to create collection with type");
        assert_eq!(collection.get_type(), Some(&type_));
    }

    #[test]
    fn test_add_and_get_item() {
        let mut collection =
            Collection::new(None, Some(Format::Json)).expect("Failed to create collection");
        let key = Label::Str("test".to_string());
        let cmw_item = dummy_cmw();
        collection
            .add_item(key.clone(), cmw_item.clone())
            .expect("Failed to add item");
        let retrieved = collection.get_item(&key);
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap(), &cmw_item);
    }

    #[test]
    fn test_validate_empty_collection() {
        let collection =
            Collection::new(None, Some(Format::Json)).expect("Failed to create collection");
        let result = collection.validate();
        assert!(result.is_err());
    }

    #[test]
    fn test_get_meta_sorted() {
        let mut collection =
            Collection::new(None, Some(Format::Cbor)).expect("Failed to create collection");
        // Add items with keys in non-sorted order.
        let key1 = Label::Str("b_key".to_string());
        let key2 = Label::Str("a_key".to_string());
        collection
            .add_item(key1, dummy_cmw())
            .expect("Failed to add item");
        collection
            .add_item(key2, dummy_cmw())
            .expect("Failed to add item");
        let meta = collection.get_meta();
        assert_eq!(meta.len(), 2);
        // Meta should be sorted (by key display).
        let first_key = match &meta[0].key {
            Label::Str(s) => s,
            _ => "",
        };
        assert_eq!(first_key, "a_key");
    }

    #[test]
    fn test_json_roundtrip() {
        let type_str = "tag:example.com,2024:composite-attester";
        let type_ = Type::from_str(type_str).expect("Failed to parse type");
        let mut collection =
            Collection::new(Some(type_), Some(Format::Json)).expect("Failed to create collection");
        collection
            .add_item(Label::Str("item1".to_string()), dummy_cmw())
            .expect("Failed to add item");
        // Marshal the collection to JSON bytes.
        let json_bytes = collection
            .marshal_json()
            .expect("Failed to marshal collection to JSON");
        // Unmarshal the JSON bytes back into a collection.
        let collection2 = Collection::unmarshal_json(&json_bytes)
            .expect("Failed to unmarshal JSON into collection");
        assert_eq!(collection, collection2);
    }

    #[test]
    fn test_cbor_roundtrip() {
        let type_str = "tag:example.com,2024:composite-attester";
        let type_ = Type::from_str(type_str).expect("Failed to parse type");
        let mut collection =
            Collection::new(Some(type_), Some(Format::Cbor)).expect("Failed to create collection");
        collection
            .add_item(Label::Str("item1".to_string()), dummy_cmw())
            .expect("Failed to add item");
        collection
            .add_item(Label::Uint(42), dummy_cmw())
            .expect("Failed to add item");
        // Marshal the collection to CBOR bytes.
        let cbor_bytes = collection
            .marshal_cbor()
            .expect("Failed to marshal collection to CBOR");
        // Unmarshal the CBOR bytes back into a collection.
        let collection2 = Collection::unmarshal_cbor(&cbor_bytes)
            .expect("Failed to unmarshal CBOR into collection");
        assert_eq!(collection, collection2);
    }
}
