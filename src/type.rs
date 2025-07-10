// Copyright 2025 Contributors to the Veraison project.
// SPDX-License-Identifier: Apache-2.0

//! Type wrapper (media type string or numeric CF), with JSON/CBOR serialization
//! and TN/CF mapping.

use crate::cfmap::{CF_TO_MT, MT_TO_CF};
use crate::tn::{cf, tn};
use mime::Mime;
use minicbor::data::Tag;
use minicbor::encode::Write;
use minicbor::Encoder;
use serde::{
    de::{Error, Unexpected},
    Deserialize, Deserializer, Serialize, Serializer,
};
use std::fmt;
use std::str::FromStr;

/// Enumeration of the allowed CMW type formats.
///
/// * `Media` represents a MIME media type string
/// * `CfNum` represents a Content-Format value
/// * `TagNum` represents a CBOR Tag (generated from
///   a Content-Format using the TN(.) function)
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Media(Mime),
    CfNum(u16),
    TagNum(u64),
}

impl Type {
    /// Compute CBOR Tag number from stored type.
    pub fn tag_number(&self) -> Result<u64, String> {
        match self {
            Type::Media(s) => {
                if let Some(&cf) = MT_TO_CF.get(s.as_ref()) {
                    tn(cf).map_err(|e| e.to_string())
                } else {
                    Err(format!(
                        "media type {s:?} has no registered CoAP Content-Format"
                    ))
                }
            }
            Type::CfNum(cf) => tn(*cf).map_err(|e| e.to_string()),
            Type::TagNum(tn) => Ok(*tn),
        }
    }

    /// Convert an integer (uint64) from CBOR Tag into internal Type. We attempt CF back‐mapping.
    pub fn from_tag(tn: u64) -> Result<Self, String> {
        let cf = cf(tn).map_err(|e| e.to_string())?;
        if let Some(&mt) = CF_TO_MT.get(&cf) {
            Ok(Type::Media(Mime::from_str(mt).unwrap())) // unwrap is ok since we control CF_TO_MT and all types should be correct
        } else {
            Ok(Type::CfNum(cf))
        }
    }

    pub(crate) fn marshal_cbor<W: Write>(&self, encoder: &mut Encoder<W>) -> Result<(), String> {
        match self {
            Type::Media(t) => {
                encoder
                    .str(t.as_ref())
                    .or(Err("writing CBOR media type".to_string()))?;
            }
            Type::CfNum(t) => {
                encoder
                    .u16(*t)
                    .or(Err("writing CBOR CF number".to_string()))?;
            }
            Type::TagNum(t) => {
                encoder
                    .tag(Tag::new(*t))
                    .or(Err("writing CBOR tag".to_string()))?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Media(s) => write!(f, "{s}"),
            Type::CfNum(cf) => {
                if let Some(&mt) = CF_TO_MT.get(cf) {
                    write!(f, "{mt}")
                } else {
                    write!(f, "{cf}")
                }
            }
            Type::TagNum(tn) => {
                if let Ok(cf) = cf(*tn) {
                    if let Some(&mt) = CF_TO_MT.get(&cf) {
                        return write!(f, "{mt}");
                    }
                    write!(f, "{cf}")
                } else {
                    write!(f, "")
                }
            }
        }
    }
}

// JSON: if string -> media type; if number -> CF number.
impl Serialize for Type {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Type::Media(s) => serializer.serialize_str(s.as_ref()),
            Type::CfNum(cf) => serializer.serialize_u64(*cf as u64),
            Type::TagNum(tn) => serializer.serialize_u64(*tn),
        }
    }
}

impl<'de> Deserialize<'de> for Type {
    fn deserialize<D>(deserializer: D) -> Result<Type, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct TypeVisitor;
        impl<'de> serde::de::Visitor<'de> for TypeVisitor {
            type Value = Type;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "string or integer for Type")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                // Must validate media type syntax via `mime::Mime`.
                match mime::Mime::from_str(v) {
                    Ok(mt) => Ok(Type::Media(mt)),
                    Err(e) => Err(E::custom(format!("bad media type: {e}"))),
                }
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: Error,
            {
                if v <= u16::MAX as u64 {
                    Ok(Type::CfNum(v as u16))
                } else {
                    Ok(Type::TagNum(v))
                }
            }

            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
            where
                E: Error,
            {
                Err(E::invalid_type(Unexpected::Float(v), &self))
            }
        }

        deserializer.deserialize_any(TypeVisitor)
    }
}
