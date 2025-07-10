// Copyright 2025 Contributors to the Veraison project.
// SPDX-License-Identifier: Apache-2.0

//! Collection CMW: map of keys → nested CMW, with optional "__cmwc_t" type.

use crate::{cmw::Kind, utils::validate_collection_type, CMW};
use mime::Mime;
use minicbor::{data::Type, encode::Write, Decoder, Encode, Encoder};
use serde_json::{Map as JsonMap, Value as JsonValue};
use std::{collections::BTreeMap, fmt, str::FromStr};

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

/// The Collection structure.
#[derive(Clone, Debug, PartialEq)]
pub struct Collection {
    cmap: BTreeMap<Label, CMW>,
    ctyp: Option<Mime>,
    pub(crate) format: Option<Format>,
}

impl Collection {
    /// Create new collection with optional type.
    pub fn new(ctyp: Option<Mime>, format: Option<Format>) -> Result<Self, String> {
        if let Some(mt) = &ctyp {
            validate_collection_type(mt.as_ref())?;
        }

        Ok(Collection {
            cmap: BTreeMap::new(),
            ctyp,
            format,
        })
    }

    pub fn get_type(&self) -> Option<&Mime> {
        self.ctyp.as_ref()
    }

    pub fn add_item(&mut self, key: Label, node: CMW) -> Result<(), String> {
        // Validate key (string must not be "__cmwc_t" or empty/whitespace).
        match &key {
            Label::Str(s) => {
                if s.trim().is_empty() {
                    return Err("bad collection key: empty or whitespace only".into());
                }
                if s == "__cmwc_t" {
                    return Err("bad collection key: __cmwc_t is reserved".into());
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

    pub fn validate(&self) -> Result<(), String> {
        if self.cmap.is_empty() {
            return Err("empty CMW collection".into());
        }
        for (k, v) in &self.cmap {
            if let Err(e) = v.validate() {
                return Err(format!("invalid collection at key {:?}: {}", k, e));
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
    pub fn marshal_json(&self) -> Result<Vec<u8>, String> {
        let mut map: JsonMap<String, JsonValue> = JsonMap::new();
        if let Some(ref c) = self.ctyp {
            map.insert("__cmwc_t".into(), JsonValue::String(c.to_string()));
        }
        for (k, v) in &self.cmap {
            if let Label::Str(kstr) = k {
                let vb = v
                    .marshal_json()
                    .map_err(|e| format!("serializing nested CMW: {}", e))?;
                map.insert(kstr.clone(), vb.into());
            } else {
                return Err("JSON collection, key error: want string".into());
            }
        }
        serde_json::to_vec(&map).map_err(|e| format!("marshaling JSON collection: {}", e))
    }

    /// Deserialize from JSON bytes.
    pub fn unmarshal_json(b: &[u8]) -> Result<Self, String> {
        let raw: JsonValue =
            serde_json::from_slice(b).map_err(|e| format!("unmarshaling JSON collection: {e}"))?;
        let obj = raw
            .as_object()
            .ok_or_else(|| "want JSON object start".to_string())?;
        let mut col = Collection::new(None, None)?;
        for (key, val) in obj {
            if key == "__cmwc_t" {
                if let JsonValue::String(s) = val {
                    validate_collection_type(s)?;
                    col.ctyp = Some(
                        Mime::from_str(s)
                            .map_err(|e| format!("Could not parse CMW type as media type: {e}"))?,
                    );
                    continue;
                } else {
                    return Err("invalid JSON collection type".into());
                }
            }
            let ser =
                serde_json::to_vec(val).map_err(|e| format!("re-serializing nested JSON: {e}"))?;
            let nested =
                CMW::unmarshal_json(&ser).map_err(|e| format!("unmarshaling nested CMW: {e}"))?;
            col.cmap.insert(Label::Str(key.clone()), nested);
        }
        col.format = Some(Format::Json);
        Ok(col)
    }

    /// Serialize as CBOR: map with string/u64 keys.
    pub fn marshal_cbor(&self) -> Result<Vec<u8>, String> {
        let buf = Vec::new();
        let mut encoder = Encoder::new(buf);
        self.marshal_cbor_to_encoder(&mut encoder)
            .map_err(|e| format!("marshaling CBOR collection: {e}"))?;
        Ok(encoder.into_writer())
    }

    pub(crate) fn marshal_cbor_to_encoder<W: Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> Result<(), String> {
        let meta = self.get_meta();
        let _ = encoder
            .map(meta.len() as u64)
            .or(Err("starting CBOR collection map".to_string()))?;
        if let Some(ref c) = self.ctyp {
            encoder
                .str("__cmwc_t")
                .or(Err("serializing CBOR collection type".to_string()))?;
            encoder
                .str(c.as_ref())
                .or(Err("serializing CBOR collection type".to_string()))?;
        }
        for (k, v) in &self.cmap {
            match k {
                Label::Str(s) => encoder
                    .str(s)
                    .or(Err("serializing a collection key (string)".to_string()))?,
                Label::Uint(u) => encoder
                    .u64(*u)
                    .or(Err("serializing a collection key (int)".to_string()))?,
            };
            v.encode(encoder, &mut ())
                .or(Err("serializing nested CMW".to_string()))?;
        }
        Ok(())
    }

    /// Deserialize from CBOR bytes.
    pub fn unmarshal_cbor(b: &[u8]) -> Result<Self, String> {
        let mut decoder = Decoder::new(b);
        Collection::unmarshal_from_decoder(&mut decoder)
    }

    pub(crate) fn unmarshal_from_decoder(decoder: &mut Decoder<'_>) -> Result<Self, String> {
        let mut col = Collection::new(None, None)?;
        let _ = decoder
            .map()
            .map_err(|e| format!("decoding collection map: {e}"))?;
        loop {
            let key_type: Type = match decoder.datatype() {
                Ok(t) => t,
                Err(e) if e.is_end_of_input() => break,
                Err(e) => return Err(format!("decoding collection key type: {e}")),
            };
            match key_type {
                Type::String => {
                    let ks = decoder
                        .str()
                        .map_err(|e| format!("decoding CBOR string key: {e}"))?;

                    if ks == "__cmwc_t" {
                        let collection_type = decoder
                            .str()
                            .map_err(|e| format!("decoding CBOR collection type: {e}"))?;
                        // Check if the collection type is a valid MIME type.
                        validate_collection_type(collection_type)?;
                        col.ctyp =
                            Some(Mime::from_str(collection_type).map_err(|e| {
                                format!("Could not parse CMW type as media type: {e}")
                            })?);
                        continue;
                    }

                    let nested = decoder
                        .decode()
                        .map_err(|e| format!("decoding CBOR nested CMW: {e}"))?;
                    col.cmap.insert(Label::Str(ks.to_string()), nested);
                }
                Type::Int
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::I8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::U8 => {
                    let ui = decoder
                        .u64()
                        .map_err(|e| format!("decoding CBOR uint key: {e}"))?;
                    let nested = decoder
                        .decode()
                        .map_err(|e| format!("decoding CBOR nested CMW: {e}"))?;
                    col.cmap.insert(Label::Uint(ui), nested);
                }
                _ => {
                    return Err(format!(
                        "unknown collection key type: want string or uint: {:?}",
                        key_type
                    ))
                }
            }
        }
        col.format = Some(Format::Cbor);
        Ok(col)
    }
}
